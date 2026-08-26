# Sweep: "a capability claim about model A, carried into code whose default is model B"

**Executed:** 2026-08-26. The tripwire filed from the kimi `mandatory` finding names a
grep-shaped predicate; this is the pass that turns one instance plus a hypothesis into a
measurement. Scope: every generation driver.

## The instrument, and its own first failure

The FIRST version scanned **comments only**. It would not have caught the known instance — the
`mandatory` claim lives in a module **docstring**, whose body lines start with neither `#` nor
`"""`. A sweep that cannot find the case it was built from is an untested instrument, so it was
rebuilt on `tokenize` (comments) + `ast.get_docstring` (docstring bodies) before any result was
believed.

## Result

| driver | `DEFAULT_MODEL` | capability claims naming a model | mismatched |
|---|---|---|---|
| `run_no_scope_kimi.py` | `kimi-k2.6` | 6 | see below |
| `run_no_scope_stealth.py` | `stealth/ox-alpha` | 0 | — |
| `run_no_scope_gemini.py` | `gemini-2.5-flash` | 0 | — |
| `run_no_scope_sonnet.py` | `claude-sonnet-5` | 0 | — |
| `generate_kernel_corpus.py` | `claude-haiku-4-5-20251001` | 0 | — |
| `generate_json_haiku.py` | `claude-haiku-4-5-20251001` | 0 | — |
| `story_generator_base.py` | `gemini-2.5-pro` | 0 | — |

**The class is real but its incidence is ONE FILE, and the mechanism is now nameable: a model
RETARGET.** `run_no_scope_kimi.py` was built for K3 and retargeted to k2.6 (KNOWN_STATE
2026-07-19, "twin retargeted k2.6"); its K3-era justifications stayed. No other driver has
changed target model, and none carries a capability claim naming any model at all. So the
sharper tell is **"a capability claim that survived a model retarget"** — more findable than the
general form, and it has exactly one candidate site in this repo today.

## The one remaining kimi row, checked rather than assumed

`:184` — *"No temperature: kimi-k3 is reasoning-only and (like Sonnet-5/Opus-4.7+) rejects a
non-default sampling temperature"* — a claim about K3 guarding a function whose default is k2.6.
Same shape, three lines from the one already fixed. **Checked live** (`kimi_temp_check.py`):

| arm | sent | result |
|---|---|---|
| A | `temperature: 0.2` (genuinely non-default) | **HTTP 400** — `invalid temperature: only 1 is allowed for this model` |
| B | `temperature: 1.0` (== the default; known-good control) | HTTP 200 |
| C | nothing (what the driver sends; control) | HTTP 200 |

**Verdict: MIS-ATTRIBUTED BUT TRUE.** The claim's subject is the wrong model, but its content
holds for k2.6 — only temperature 1 is permitted. **Nothing is foreclosed, so this is not a
consequential instance.** The justification was corrected in place to name k2.6 and carry this
witness.

**An error in this document's own instrument, recorded because it is the same defect class.**
The first draft of `kimi_temp_check.py` labelled arm B *"temperature 1.0"* and folded it into
`a or b` as evidence that a NON-DEFAULT temperature is accepted — but the API's own 400 says
*only 1 is allowed*, so 1.0 IS the default and arm B is a control, not evidence. The label was
not true of what it measured, and the wrong verdict ("k2.6 ACCEPTS a non-default temperature")
printed before it was caught. Corrected and re-run before the result was reported anywhere.

## Disposition

- **Consequential instances found: 1** (the thinking toggle), already fixed.
- **Mis-attributed-but-correct: 1** (temperature), corrected in place.
- **Other six drivers: a TESTED absence** — the instrument fires on the known case and returns
  zero on them, so this is not "we didn't look".
