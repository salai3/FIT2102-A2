import {
    debounceTime,
    fromEvent,
    map,
    merge,
    mergeScan,
    Observable,
} from "rxjs";
import { fromFetch } from "rxjs/fetch";
import type { State } from "./types";

const grammarInput = document.getElementById(
    "grammar-input",
) as HTMLTextAreaElement;
const stringInput = document.getElementById(
    "string-input",
) as HTMLTextAreaElement;
const dropDown = document.getElementById("parserSelect") as HTMLSelectElement;
const button = document.getElementById("runParser")!;
const saveButton = document.getElementById("saveButton")!;

type Action = (_: State) => State;

const resetState: Action = s => ({ ...s, run: false });

// Create an Observable for keyboard input events
const input$: Observable<Action> = fromEvent<KeyboardEvent>(
    grammarInput,
    "input",
).pipe(
    debounceTime(1000),
    map(event => (event.target as HTMLInputElement).value),
    map(value => s => ({ ...s, grammar: value, resetParsers: true })),
);

const stringToParse$: Observable<Action> = fromEvent<KeyboardEvent>(
    stringInput,
    "input",
).pipe(
    debounceTime(1000),
    map(event => (event.target as HTMLInputElement).value),
    map(value => s => ({ ...s, string: value, resetParsers: false })),
);

const dropDownStream$: Observable<Action> = fromEvent(dropDown, "change").pipe(
    map(event => (event.target as HTMLSelectElement).value),
    map(value => s => ({
        ...s,
        selectedParser: value,
        resetParsers: false,
    })),
);

const buttonStream$: Observable<Action> = fromEvent(button, "click").pipe(
    map(() => s => ({ ...s, run: true, resetParsers: false })),
);

const saveButtonStream$: Observable<Action> = fromEvent(saveButton, "click").pipe(
    map(() => s => ({ ...s, saveStatus: "Saving..." })),
);

function getHTML(s: State): Observable<State> {
    // Get the HTML as a stream
    const body = new URLSearchParams();
    body.set("grammar", s.grammar);
    body.set("string", s.run ? s.string : "");
    body.set("selectedParser", s.run ? s.selectedParser : "");

    return fromFetch<
        Readonly<
            | { parsers: string; result: string; warnings: string }
            | { error: string }
        >
    >("/api/generate", {
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: body.toString(),
        selector: res => res.json(),
    }).pipe(
        map(res => {
            if ("error" in res) return { ...s, grammarParseError: res.error };
            const parsers = res.parsers.split(",");
            return {
                ...s,
                grammarParseError: "",
                warnings: res.warnings.split("\n"),
                parserOutput: res.result,
                parsers,
                selectedParser: parsers.includes(s.selectedParser)
                    ? s.selectedParser
                    : (parsers[0] ?? ""),
            };
        }),
    );
}

function saveFile(s: State): Observable<State> {
    // If saveStatus is not "Saving...", don't save
    if (s.saveStatus !== "Saving...") {
        return new Observable(observer => {
            observer.next(s);
            observer.complete();
        });
    }

    const body = new URLSearchParams();
    body.set("grammar", s.grammar);

    return fromFetch<
        Readonly<
            | { success: string; message: string }
            | { error: string }
        >
    >("/api/save", {
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: body.toString(),
        selector: res => res.json(),
    }).pipe(
        map(res => {
            if ("error" in res) {
                return { ...s, saveStatus: "Error: " + res.error };
            }
            return { ...s, saveStatus: res.message };
        }),
    );
}

const initialState: State = {
    grammar: "",
    string: "",
    selectedParser: "",
    run: false,
    resetParsers: false,
    grammarParseError: "",
    parsers: [],
    parserOutput: "",
    warnings: [],
    saveStatus: "",
};

function main() {
    const selectElement = document.getElementById("parserSelect")!;
    const grammarParseErrorOutput = document.getElementById(
        "grammar-parse-error-output",
    ) as HTMLOutputElement;
    const parserOutput = document.getElementById(
        "parser-output",
    ) as HTMLOutputElement;
    const validateOutput = document.getElementById(
        "validate-output",
    ) as HTMLOutputElement;
    const saveStatusElement = document.getElementById("save-status")!;

    // Subscribe to the input Observable to listen for changes
    merge(input$, dropDownStream$, stringToParse$, buttonStream$, saveButtonStream$)
        .pipe(
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // First apply saveFile, then getHTML
                // Both return observables of length one
                return saveFile(newState).pipe(
                    mergeScan((s: State) => getHTML(s).pipe(map(resetState)), newState)
                );
            }, initialState),
        )
        .subscribe(state => {
            if (state.resetParsers) {
                selectElement.replaceChildren(
                    ...state.parsers.map(optionText => {
                        const option = document.createElement("option");
                        option.value = optionText;
                        option.text = optionText;
                        return option;
                    }),
                );
                // if the <option> HTML elements are changed, the value of the
                // <select> element will reset to ""
                dropDown.value = state.selectedParser;
            }

            grammarParseErrorOutput.value = state.grammarParseError;
            parserOutput.value = state.parserOutput;
            validateOutput.value = state.warnings.join("\n");
            saveStatusElement.textContent = state.saveStatus;
        });
}
if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}
