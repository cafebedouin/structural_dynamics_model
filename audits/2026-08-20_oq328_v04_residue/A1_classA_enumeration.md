# A1/A2 — the eight indices enumerated against Wu's class-A criterion, and the zero classified

**Executed:** 2026-08-20. Discharges the plan's Package A gate: *row A asserts no member — a zero
— and §7.8 requires every cited zero be classified before citation.*

## The criterion, as the paper itself states it

§6.2's table: **A — environment and platform quirks (dev green, prod silent).** The class name is
*environment and platform quirks*; the parenthetical is an exemplar of polarity, not the definition.
The discriminating question per index is therefore: **does this index range over *where the code is
running*?**

**The criterion's own instability, recorded as part of the result** (plan's A1 requirement).
`audits/2026-08-10_oq277_rq2_crosscoding/RECON.md:180-187` finds **Class A VERDICT-INELIGIBLE**:
n=1 under Wu's catalog index (`whatsapp_client_display_folding`, the six quirks logged as
sub-events with no independent case documents), n=4 under his dataset index. A class that is
verdict-ineligible under one index and not the other is the same self-disagreement that retired the
Wu limb, landing on this exact row. Everything below is therefore an enumeration against a
criterion whose *extension* is unstable, even though its *intension* — environment-indexed — is not.

## The enumeration: eight indices, one criterion

| index | what the index ranges over | environment-indexed? |
|---|---|---|
| P1 Produced-but-not-consumed | the **wire** between a producer and a reader | no — a missing consumer is missing in every environment |
| P2 One-canonical-thing-became-two | the **identity** of a thing (no queryable fact of canonicity) | no — both copies exist in every environment |
| *index 3* | vacated 2026-08-11, never reused | n/a |
| P4 Fabricated default | the **value** (absent datum → plausible constant) | no — the fallback fires on input shape, not on host |
| P5 Absence satisfies the gate | the **gate's input** (no datum to check) | no — an empty table is empty everywhere |
| P6 Success-shaped absorption | the **composition boundary** (measured-empty vs didn't-look) | **near-miss, and it is where the institution's environment incidents actually get filed** — but P6 ranges over *which boundary was crossed*, and filing an environment incident there discards the environment as the variable. See the disposition below |
| P7 Bound-probe bypasses clause-order | **dispatch** (a bound selector skipping a cut) | no |
| P8 Recap-as-witness substitution | **reporting** (a claim without its witness) | no |

**Result: zero of eight indexes on environment.** Every member indexes on a *layer of the value's
journey* — production, identity, value, gating, composition, dispatch, reporting — which is exactly
what §5.1 says the taxonomy is (*"one mechanism surfacing at seven layers"*). Wu's A indexes on a
different axis entirely: not where in the journey the collapse happens, but which machine the
journey is running on. **The two taxonomies are not merely missing each other's members; they are
indexed on different variables, and no member of a layer-indexed set can express an
environment-indexed class.** That is a stronger and more checkable statement of the gap than *no
member*, and it is what enumeration buys over recognition.

## The finding the enumeration was not looking for

Row A does not only assert the zero. It asserts a **reason**:

> This institution has no dev/prod split of the relevant kind; its analogue is regime boundaries
> and corpus resets, which the trifurcation types as Type A drift (§2.8).

**That clause is false, and the same enumeration falsifies it.** The institution has at least four
environment splits of exactly the relevant kind, all four documented in its own always-loaded rules,
and at least one with a witnessed silent incident:

| # | the split | the silence | citation |
|---|---|---|---|
| 1 | **suite load path vs pipeline load path** | `metric_drift_events.pl:230` called a predicate qualified to the wrong module. The suite path **threw** and aborted the whole drift scan; the pipeline path **silently resolved** through `json_report.pl`'s user-imports and produced correct-by-accident drift events **for months**. *"The two paths witnessed opposite behaviors for the same line of code."* | OQ-57, resolved 2026-06-04; `docs/technical/swipl_load_path_and_probe_gotchas.md` §1; CLAUDE.md:165 |
| 2 | **fresh worktree vs main checkout** | `outputs/` is gitignored, so a fresh worktree lacks `pipeline_output.json` and *"read pre-computed values from there"* probes *"read empty/stale and look fine."* Named in CLAUDE.md and **already typed Pattern-6 there** | CLAUDE.md:1183 |
| 3 | **`[stack]` REPL vs `run_pipeline`** | a plain `[stack]` load leaves MaxEnt unfitted and its reads **fail soft** — `catch/3` does not intercept, so a probe reading MaxEnt observables under `[stack]` alone *"measures NOTHING"* while a placeholder makes the nothing indistinguishable from a result | OQ-66, 2026-07-25; CLAUDE.md:315 |
| 4 | **Edit/Write tool channel vs Bash tool channel** | the `PreToolUse` matcher is `Edit\|Write` only, so a `sed`/heredoc edit *"NEVER fires the hook and produces the same silence as a clean query"* — indistinguishable at the read site | CLAUDE.md:144; KNOWN_STATE 2026-08-20 |

Instance 1 is Wu's class A almost verbatim, with the polarity inverted: **test red, production
silent**, rather than dev green and prod silent. Instance 2 is the literal polarity — the new
environment reads green and is empty.

**Why this does not resurrect the zero.** Instances 1–4 are class-A *incidents*; none of them has a
class-A *index*. Instance 2 is filed at P6 and instance 1 at no index at all. So the taxonomy gap
survives the enumeration intact — what does not survive is the explanation offered for it. Row A
conflates two claims and licenses the first with the second:

- **(a) the taxonomy has no member expressing Wu's A** — **TRUE**, and now enumerated: eight
  indices, none environment-indexed, because the taxonomy is indexed on layers.
- **(b) the institution has no dev/prod split of the relevant kind** — **FALSE**; four splits, one
  witnessed silent for months.

The honest repair keeps (a), retires (b), and replaces (b) with the sharper reason enumeration
actually found: the gap is **axis-level, not incident-level.**

## The second clause — found on review, and it fails on the same axis error

Row A's second sentence has two clauses joined by a semicolon, and the first pass addressed only
the first. The second reads:

> its analogue is regime boundaries and corpus resets, which the trifurcation types as Type A
> drift (§2.8)

**It was a substitute**, offered because no literal instance was believed to exist. Four literal
instances are now witnessed, so there is nothing left for it to substitute for — but it also fails
on its own terms, and it fails the same way the clause before it does.

| | indexes on | §2.8 gloss |
|---|---|---|
| **Wu's class A** | **location** — same code, two places, different behaviour, one silent | — |
| **Type A (drift)** | **time** — spec-vs-code drift, *"the corpus"* without a date | *"the framing expired"*; repaired by as-of stamps |
| **Type B (structure)** | **contradiction inside one system** | *"two framings contradict inside one system"*; repaired by machine-enforced invariants in the standing gate |

Offering a **time**-indexed analogue for a **location**-indexed class is the identical axis
confusion this enumeration diagnoses in the clause before it — committed one clause earlier, as the
remedy for it.

**The four instances are Type B, and the typing is confirmed by a route nobody chose for it.**
Type B's declared repair is *machine-enforced invariants in the standing gate*. OQ-57's actual
repair, taken **2026-06-04** and long predating this row, was `prolog/check_stack.pl` run as a
standing command against a recorded baseline — *"surfacing undefined-predicate references of exactly
this class as a command instead of forensics"*
(`swipl_load_path_and_probe_gotchas.md:42-44`; CLAUDE.md:325). **The repair chose Type B before
anyone asked what type it was.** That is a naturally-arising confirmation, not a classification
constructed to fit.

**What this does to the gap — it narrows it, and the narrowing still runs against interest.** The
surviving claim is *not* that the institution cannot see these failures: it can, under the
trifurcation, and it repaired one of them years of sessions ago. It is that **this paper's
eight-index failure taxonomy has no index for what the institution's own trifurcation can type.**
Harder to say than *we do not have that shape*, and it is the form in which the row carries RQ2's
falsifier.

**Light note, not a defect:** §2.8 was forward-pointered to the practice paper on 2026-08-20 and is
marked *not maintained*, but the trifurcation table is retained at its number and the citation
resolves, exactly as that subsection's redirect promises.

## A2 — the zero classified, per §7.8

**Tested absence**, scoped to claim (a).

The enumeration is the test: eight indices, one criterion, each index's ranging-over stated and
compared. It is not a search that came back empty — there was nothing to search, because the
question is what each index ranges over, and that is readable off §5.1's own layer column. A reader
can re-run it by reading the table.

**Not** *untested instrument*: the plan's pre-registered worry was that nobody had enumerated, which
was correct, and enumerating is what changed the disposition.
**Not** *unrecheckable*: every input is a live tracked file, cited above.

**The declared limit on the tier.** The tested absence is (a) only. Claim (b), which row A currently
uses to justify (a), was *never* tested and is now falsified; the pre-enumeration row asserted the
conjunction, so **the row as written was not WITNESSED** — the plan's second branch was live for the
row as it stood. It becomes WITNESSED only in the corrected form that drops (b).

## Two-sided control on the vocabulary probe

`A1_classA_vocab_probe.md` — fires on `staging` (60 files), `fail-plausible` (15 outside the paper),
`arXiv:2606.14589` (10), `load-path` (18), `REPL` (2,966); declines on `dev green`, `dev/prod`,
`environment and platform` (0 outside the paper's own versions). Same path, `/usr/bin/grep -F`
pinned, `git ls-files` frame.

**Reported at its true altitude, which is low.** That probe searches for **Wu's words**, not Wu's
class, and its 0 is precisely the *concept→surface* false absence `build_discipline.md` warns about
under *False-absence* sub-rule (c): a perfect control ladder on the wrong predicate still yields a
false absence. The `load-path` row (18 files, all outside the paper) is the tell — the institution
discusses this class constantly in its own vocabulary. **The vocabulary probe is evidence that the
institution does not borrow Wu's words. It is not evidence about the class, and the enumeration
above, not the probe, is what discharges the zero.**

**Prior art:** grepped `build_discipline.md` for `False-absence`, `concept→surface`, `environment`,
`load-path` — hit on *Over-confident moves on the synthesis side* (1) False-absence, sub-rule (c)
(the `coordination_vitality`/`founding_problem_status` instance). This is a **RE-DISCOVERY** of that
sub-rule at a new surface: the wrong predicate here is a *vocabulary* rather than an authored field.
