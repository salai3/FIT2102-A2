import {
    debounceTime,
    fromEvent,
    map,
    merge,
    mergeScan,
    type Observable,
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

    // Subscribe to the input Observable to listen for changes
    merge(input$, dropDownStream$, stringToParse$, buttonStream$)
        .pipe(
            mergeScan((acc: State, reducer: Action) => {
                const newState = reducer(acc);
                // getHTML returns an observable of length one
                // so we `scan` and merge the result of getHTML in to our stream
                return getHTML(newState).pipe(map(resetState));
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
        });
}
if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}
