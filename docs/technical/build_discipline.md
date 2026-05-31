# Build Discipline — Recurring Failure Modes

Implementation note. Scope: a recurring family of defect *patterns* that have appeared in
multiple unrelated subsystems, with diagnostics for catching them. This is not general
architecture; it is the specific shape of mistakes this repo keeps making, recorded so
they stop. Pointer in `CLAUDE.md` → Build Discipline.

The root cause is structural, not careless: the repo was built fast by one person, the
*producing* step of any feature is the interesting part, and the *reconciling* step —
wiring the output to a consumer, collapsing a fork back to one canonical copy — has no
payoff in the moment and is infinitely deferrable. So it gets deferred, and the deferral
is invisible because the producer looks finished. That is the *why* the patterns recur; they
also share a single structural *what* — every one of them is an absence that presents as a
presence — named and tabulated in **The spine** at the end of this note.

---

## Pattern 1 — Produced-but-not-consumed (the dangling wire)

**Shape:** data is correctly generated, written to disk, and never read back into the
thing that needs it. Every check on the *producer* passes; the gap is in the absent
consumer, so nothing fails — the information just sits unused.

**Known instances:**
- Sensitivity sweeps in `python/sweeps/` write `*_sensitivity_results.json`; no consumer
  reads them. The fold-tightness data the engine produces about its own parameters is
  measured and discarded.
- SCOPE writes `outputs/kernel_manifests/<run_tag>/kernel_grouping.json` (the authoritative
  kernel→readings grouping) but the grouping was not stamped into the generated `.pl`
  files. Result: ~83 stories carried `cs_story_uid` and no `cs_kernel_id` — the linkage
  existed in the manifest and in filename convention, but not as a fact the engine could
  query. The cross-reading machinery therefore could not gather a kernel's readings.
- The pipeline manifest convention exists so audits *can* cite provenance, but nothing
  enforces that an audit actually does.

**Rule:** a producer is not done until something consumes its output. When you add a step
that writes data, in the same change either wire the consumer or add a check that fails
loudly when the output is left unconsumed.

**Diagnostic — find orphaned outputs:**
```bash
# JSON written by some script but grepped-for by none
for f in $(find outputs python -name "*.json" 2>/dev/null); do
  base=$(basename "$f")
  consumers=$(grep -rl "$base" python prolog agent --include=*.py --include=*.pl 2>/dev/null \
              | grep -v "$(dirname $f)" | wc -l)
  [ "$consumers" -eq 0 ] && echo "ORPHAN: $f"
done

# stories with identity but no fold-membership (the linkage gap)
for f in prolog/testsets/*.pl; do
  grep -q cs_story_uid "$f" && ! grep -q cs_kernel_id "$f" && basename "$f"
done
```

---

## Pattern 2 — One-canonical-thing-became-two (the silent fork)

**Shape:** a file or record is copied to a scratch/test location, possibly edited, and now
two versions coexist with no queryable fact stating which is canonical. The knowledge
lives only in memory ("I put it there to test it; a model moved the other one"). A
downstream step that targets the wrong copy produces results that look correct and are not.

**Known instances:**
- `generate_kernel_corpus.py` exists in both `commitment_corpus/` (test copy) and `agent/`.
  Targeting the non-canonical copy with a linkage join would stamp facts into a file
  generation does not use — a "fix" that lies.
- Historically: ISSUES.md / AGENDA.md / PRIORITIES.md / TODO.md were all tracking
  surfaces, but the end-of-session update protocol named only some, so the unnamed ones
  silently drifted (TODO.md held a live work item the protocol never reconciled).

**Rule:** one canonical location per thing, and which one is canonical must be a *checked
fact* — a documented path, a CI assertion — not a memory. Resolve a discovered fork by
evidence, not preference:
1. Which path do the documented run-commands actually invoke? (`grep` READMEs, CLAUDE.md,
   AGENTS.md, Makefile, and the module's own usage string.)
2. Which copy's imports resolve from its location?
3. `git log` recency / which is the move-destination.

Record the verdict in CLAUDE.md `Known State` and grep for references to the retired path
before deleting it (a retired copy with live references is Pattern 1 one layer up).

**Diagnostic — find forks:**
```bash
# same basename in 2+ locations (excluding archives)
find . -name "*.py" -not -path "*/archive*" -not -path "*/node_modules/*" \
  | xargs -n1 basename | sort | uniq -d

# duplicate Prolog module declarations (a hard load collision)
grep -rhoE "^:- module\([a-z0-9_]+" prolog --include=*.pl | sort | uniq -d
```

**Sub-case — self-description fork (a comment vs. its own code's behavior).** The derived copy need
not be a separate file. A comment, docstring, or adjacent note that *describes the behavior of the
code it sits next to* is a derived copy too, and forks the same way when the code changes and the
description doesn't. **Instance (2026-05-31):** after B4 stripped the mountain
`accessibility_collapse`/`resistance` thresholds from the schema gate, the emit site in
`generate_constraint_pl.py` still carries `% --- NL Profile Metrics (required for mountain
constraints) ---`. The comment says *required* when the schema no longer gates on it — and a stale
"required" comment is precisely what would mislead the next editor of that emit site into thinking
the gate still exists. This is the silent fork one layer in: the file's self-description forked from
the file's behavior. (Same disease as a doc forking from the code it documents — e.g.
`generator_emission_map.md` vs `generate_constraint_pl.py` — just at comment range instead of file
range.)

**Triage — keep one-liners out of the OQ ledger.** Not every fork is OQ-weight. A trivial,
self-contained, fix-in-place cleanup (a stale comment, a renamed local) does **not** earn a tracked
OQ in `ISSUES.md`: that accumulates ceremony for a one-liner and dilutes the ledger's meaning
("unresolved engine-level question requiring a decision, measurement, or cross-file coordination").
File it instead as a *tiny cleanup with a disposition* — **fix it in the same change that next
touches that file**, where the editor is already in context and the fix is free. The disposition
*is* the filing; there is no tracking row to reconcile later. The stale-comment instance above is
filed exactly this way: fix on the next edit of `generate_constraint_pl.py`, not as a standing item.

---

## Pattern 3 — Bound-probe bypasses clause-order (query-binding-bypasses-cut)

**Shape:** a probe enumerates a class by *binding* the selecting argument —
`findall(C, constraint_signature(C, natural_law), Cs)` — and receives constraints the
engine never actually classifies as that class.

**Mechanism:** `signature_detection:constraint_signature/2` resolves by clause order, with
lock clauses that fire first under a cut when the engine calls with the second argument
*unbound*:

```prolog
% prolog/signature_detection.pl
:70  constraint_signature(C, false_natural_law)    :- false_natural_law(C, _), !.
:77  constraint_signature(C, false_ci_rope)        :- false_ci_rope(C, _), !.
:87  constraint_signature(C, false_summit_mountain) :- false_summit_mountain(C, _), !.
:97  constraint_signature(C, natural_law)           :-
         domain_priors:emerges_naturally(C),
         get_constraint_profile(C, Profile),
         natural_law_signature(Profile), !.
```

When the probe binds the second arg to `natural_law`, the lock clause heads
(`false_natural_law`, `false_ci_rope`, `false_summit_mountain`) fail to unify (wrong atom),
their cuts *never execute*, and Prolog falls through to the `:97` clause. The probe answers
"satisfies the `natural_law` clause body in isolation," not "the engine assigns
`natural_law`" — they differ exactly when a lock would have fired.

**Live demonstration (223-constraint corpus, 2026-05-30):**

```
findall(C, signature_detection:constraint_signature(C, natural_law), BoundCs)
  → [behavioral_competence_reading]   % bound form: 1 result

findall(C, (signature_detection:constraint_signature(C, Sig), Sig == natural_law), UnboundCs)
  → []                                % unbound+post-filter: 0 results
```

`behavioral_competence_reading` satisfies the `:97` clause body but the engine actually
assigns it `false_summit_mountain` (lock at `:87`). The bound probe manufactured a false
witness.

**Fix:** query unbound, take the engine's first solution, post-filter by equality:

```prolog
findall(C, (signature_detection:constraint_signature(C, Sig), Sig == natural_law), Cs).
```

**Diagnostic:** any `findall`/`forall` over a cut-ordered predicate with the *selecting*
argument bound is suspect. Re-run unbound + post-filter; if the count drops, the bound form
over-counted.

**Where it recurs:** a probe that queries signature membership directly to build a witness
set for kernel readings will over-count exactly the constraints the locks were installed to
protect. Welfare-reading / false_natural_law (OQ-30) is the live case: a bound probe there
manufactures false natural-law witnesses on the constraints `false_natural_law` was designed
to intercept.

---

## The shared root: build for the corpus you want, not the one you have

Both patterns are special cases of designing against the present sample instead of the
intended target. The corpus on disk is one generation; naming schemes, linkage rules, and
reports must be correct for the corpus you are *heading toward* — thousands of stories,
regeneration under schema change, found-article ingestion, adversarial input. Checking a
design against today's corpus is confirmation, not perturbation: a naming scheme that
*happens not to* collide in 223 files is not the same as one that *cannot* collide by
construction.

Concrete application — reading names. A reading named from its interpretive label alone
(`hybrid_reading`, `autonomy_reading`) is unique today but not unique across kernels; the
moment two kernels each want a "hybrid" reading, the bare name denotes two stories. The
collision-proof scheme namespaces the name under its kernel (`<kernel>__<reading>`), making
the module name, filename, and predicate base unique by construction and making
"readings of kernel K" a prefix query. Identity stays on `cs_story_uid` (UUID, stable
through regeneration); the compound name is the human- and load-facing handle the UUID
cannot be.

Kernel membership follows SCOPE's judgment, recorded in the manifest, not a generation-time
heuristic: if SCOPE marked the seed `is_contested_kernel: true`, every reading gets a
`cs_kernel_id` even when it is the only reading so far (a one-cut fold the next reading
attaches to); if `false`, the story is a genuine standalone and gets no kernel_id. The
join step transcribes that decision from `kernel_grouping.json` into the `.pl`; it does
not re-derive it.

## Pattern 4: Fabricated default — missing-data fallback that emits a real-looking value

A predicate that lacks its input fabricates a plausible constant rather than failing or
returning `unknown`. Downstream callers receive a real-looking value and treat it as a
measurement. The fabrication fires silently — no error, no warning, no coverage flag —
and is distinguishable from a genuine measurement only by perturbation (tripwiring the
fallback to an obviously wrong value and observing what flips).

**Sibling of produced-but-not-consumed:** P-b-n-c leaves a wire dangling; the fabricated
default connects the wire to a made-up signal so nothing looks broken. The defect is
harder to see because the system appears to work.

**Live instance (OQ-33, 2026-05-30):** `classify_at_time` (`drl_composition.pl:179`)
falls back to `Supp=0.5` when `suppression_requirement` is absent. That measurement is
absent in 190/190 live testsets — the fallback fires 100% of the temporal path.
Tripwire confirmed: 443/519 non-unknown temporal classifications flip to `unknown` when
`Supp` is poisoned at source, proving the fabricated default is **LOAD-BEARING-WRONG**.
Secondary finding: the static path (`get_raw_suppression`, `drl_core.pl:96`) fabricates
the same gap as `Supp=0`, not `0.5` — two surfaces invent different fillers for the same
missing data, producing divergence that is artifact, not observational signal. See OQ-33
for resolution options and blocks.

**Diagnostic:** if a predicate has a catch-all clause that binds a metric variable to a
constant — `(measurement(..., V) -> true ; V = 0.5)` — ask whether the fallback is ever
reached in the actual corpus. If it fires on more than a handful of constraints, it is a
fabrication, not a safety net. Tripwire it: replace the constant with an obviously
out-of-range value and count the flips.

**Census blast radius vs measured blast radius.** The flip count is the *census* blast radius —
the visible wrong *outputs*. It systematically undercounts exposure, because it only sees
constraints whose final type *changed*; every value computed on the bad input but not pushed across
a classification boundary is equally contaminated and invisible to a flip count. So measure two
numbers, not one: the census (outputs that changed) and the *input-exposure* (rows computed on the
fallback at all). The exposure is the real blast radius and **can be much larger than the census**,
because most contaminated inputs may land on the same side of a boundary and never flip. For the
OQ-33 gap the input-exposure was **268 rows, ~99% of the path**, against a census of **279 flips** —
comparable here, but they are not the same quantity and in general diverge by orders of magnitude.
**Carry this into D4 (scalar-vs-temporal divergence):** it may look small by flip-count and be large
by input-exposure — report the exposure denominator, not only the flips, before sizing the gap.

**Where it recurs:** any Surface-3 (temporal) predicate that reads authored measurements
from testsets; authored fields are sparse by construction (authors fill what they
understand), so temporal surfaces are structurally exposed to this pattern.

---

## Pattern 5: Absence satisfies the gate (authored-zero vs absent conflation)

A gate, threshold, or quantifier passes because its input is *missing*, not because a
condition was *checked*. `Count == 0` is true both when the constraint was authored to have
zero beneficiaries and when no beneficiary facts exist at all; `Supp =< 0.05` is true both
when suppression was measured low and when suppression is absent and defaulted; `forall(P, Q)`
is vacuously true when `P`'s table is empty. The engine reads absence as a satisfied
condition and emits a positive finding that means "nobody authored the disqualifier," not
"the disqualifier is absent in the world."

**The discipline, stated generally:** the engine must distinguish *authored to be zero* from
*absent*, everywhere, and never let absence satisfy a gate. Zero-because-measured and
zero-because-missing collapse to the same value at the comparison site; a gate that cannot
tell them apart is testing nothing whenever its source table is empty. A gate over a table
that can be empty must first establish the datum was authored (the table is non-empty for
this constraint), then check the condition — fail-closed on absence, not pass-open.

**Sibling of Pattern 4 (fabricated default):** Pattern 4 invents a *value* and feeds it to a
downstream computation; Pattern 5 lets *absence itself* pass a *condition*. Both conflate
missing with measured. Pattern 4 manufactures a number; Pattern 5 manufactures a satisfied
predicate. Pattern 4's tell is a catch-all clause binding a constant; Pattern 5's tell is a
comparison or quantifier whose driving table is empty in the corpus.

**Worked instance (OQ-43, 2026-05-31, NL beneficiary gate — the gate itself was RESOLVED by Commit
B1; see closing note):** `natural_law_signature`'s
`BeneficiaryCount == 0` (`signature_detection.pl:295`) reads `count_power_beneficiaries`,
which joins `affects_constraint × intent_power_change`. `intent_power_change` is empty
corpus-wide (**0 facts** on testsets_3000), so `BeneficiaryCount == 0` holds for *every*
constraint by absence, not by checking. The gap check confirmed the consequence: of the 404
`natural_law`-signature constraints, **0/404** carry any beneficiary signal from either source
(`constraint_beneficiary/2` *or* `intent_power_change`), and FSM coverage of the NL population
is **0/404 by cascade construction**. The 404 NL certifications currently mean "no beneficiary
**authored**," not "no beneficiary **exists**." Same class: `data_verification`'s
`forall(intent_beneficiary_class, intent_power_change)` is vacuously satisfied corpus-wide, and
`get_metric_average:160` returns the `0.5` default for any metric with no rows.

**RESOLVED 2026-05-31 (Commit B1) — the NL-gate member of this class:** `count_power_beneficiaries`
was repointed to read the authored, populated `constraint_beneficiary` table (1237 facts live)
instead of the empty `intent_power_change` join, so `BeneficiaryCount == 0` is now a checked
condition over a non-empty table (authored-zero), not a pass-by-absence; live NL certifications
dropped 5→2 (3 constraints with authored beneficiaries correctly declined). This is the
"author/repoint to the populated table" resolution below. The `data_verification` `forall` and
`get_metric_average` siblings remain open (OQ-44); the instance is kept here as the worked example.

**Diagnostic:** for any gate of the form `Count == 0`, `=< Threshold` over a `findall`, or
`forall(...)`, check whether the driving table is *non-empty for the corpus*. If the table is
empty (or the per-constraint findall is always `[]`), the gate is vacuously satisfied — it is
testing nothing.
```bash
# count facts behind a gate's source predicate across the active corpus
cd prolog && swipl -q -g "consult(stack), \
  retract(config:param(corpus_path,_)), assertz(config:param(corpus_path,'testsets_3000')), \
  corpus_loader:load_all_testsets, \
  aggregate_all(count, narrative_ontology:intent_power_change(_,_,_), N), \
  format('intent_power_change facts: ~w~n',[N]), halt"
# N == 0  ⇒  any gate reading this predicate passes by absence, not by check
```
A gate whose source count is 0 is not a safety net and not a discriminator; it is a no-op
that reads as a pass. Either author the table (so the gate discriminates) or make the gate
fail-closed when the source is empty (so absence cannot certify).

**Where it recurs:** any gate keyed on the sparse `intent_*` family or on an optional authored
field; any quantifier (`forall`, negation-as-failure) over a table that the current corpus
leaves empty. See OQ-44 for the engine-wide audit.

---

## Estimator-classifier independence

The principle, generalized: the boundary logic should live in exactly one place — the
engine — and the author should never see it. The author's job is to estimate the substrate
(how extractive, how coerced, how performative); the engine's job is to decide what type
that substrate implies. The moment the author can see the engine's decision rule, the two
stop being independent and the diff stops measuring anything. Strip the numerical guidance
not as a one-off fix but because the architecture only works if the estimator and the
classifier can't see each other.

**Concrete instance (2026-05-31, NL circularity audit):** The generation prompt told
authors that mountain constraints require `accessibility_collapse ≥ 0.85`. Result: 84.3%
of all authored AC values across 3380 constraints are exactly 0.92 — a stamp, not a
measurement. The engine certifies these as `natural_law`. The certification looks like
confirmation but is circular: the author was given the threshold and satisfied it, so the
diff between the author's estimate and the engine's verdict carries no information about
whether the constraint is a genuine natural law.

T.1 audited this directly: of the 404 `natural_law`-signature constraints, 404/404 (100%)
would classify as mountain from `ε/supp/emerges_naturally` alone without the NL signature.
The stamp manufactured nothing — but it made the test look like a test when it was a
tautology. The fix (`fix/stripped_prompt.md`, `fix/stripped_schema.json`) removes
`accessibility_collapse ≥ 0.85` and `resistance ≤ 0.15` from the generation instructions
while keeping `extractiveness ≤ 0.25`, `suppression ≤ 0.05`, and `emerges_naturally`. The
engine's threshold (`natural_law_collapse_min = 0.85` in `config.pl`) is unchanged. After
the strip, the author estimates AC without knowing the cutpoint; the engine decides whether
the estimate clears the bar. If future generated mountains cluster at AC ≈ 0.60 instead of
0.92, that is evidence the prior stamp was rule-satisfaction rather than domain measurement.

**Scoping correction — do not cite T.1's "cosmetic" verdict unqualified (2026-05-31, Commit B1).**
T.1's result (removing the NL signature changes 0 mountain classifications) holds for the
*metric-agreeing majority*. But the NL *beneficiary gate* (`count_power_beneficiaries`) was passing
vacuously over the empty `intent_power_change` table (0 facts both corpora), so it certified as
`natural_law` 3 live constraints that carry *authored* asymmetric beneficiaries. Pointing the gate at
the populated `constraint_beneficiary` table (1237 facts) declined those 3 (live NL 5→2). So the
signature was cosmetic for the mountain *type* but **not inert for the natural-law *certification***:
it hid 3 false-naturals the empty-intent gate could not exclude. Cite "cosmetic" scoped to the
metric-agreeing majority; for the false-natural tail it was classification-(certification-)changing.

**Where this recurs:** any generation prompt or schema that exposes a classification
threshold to the author creates the same risk. Whether it is a problem depends on whether
the diff between author estimate and engine verdict is supposed to carry information. For
the NL profile metrics (AC, resistance), the diff was the audit target; exposing the
threshold collapsed it to zero. For `ε ≤ 0.25` and `suppression ≤ 0.05` on mountains,
the thresholds are retained because they bound what counts as mountain *substrate* by
definition — the author needs to know they are authoring a low-extraction scenario.

**Third contamination surface — the worked EXAMPLE, not just instructions and schema (2026-05-31,
regen-path audit).** The estimator sees the decision rule through three surfaces, not two. Stripping
the *instruction* (the prompt's "AC ≥ 0.85" line) and the *schema gate* is incomplete if a few-shot
**example** still carries a gate-satisfying value. `json/antifragility.json` — the exemplar the
`c-orchestrator` generator injects — hard-codes `accessibility_collapse: 0.9, resistance: 0.08`, the
exact mountain pattern that was stripped from the prompt and schema. **A worked example showing the
rule *satisfied* teaches the decision rule more strongly than a stated threshold: demonstration beats
instruction.** The prompt's contamination surface is **{instructions, schema, examples}**; scrubbing
the first two while the third still shows AC=0.9 leaves the leak intact. (Scope: this exemplar is on
the c-orchestrator path only; the kernel regen pipeline `generate_kernel_corpus` injects a clean
exemplar `agent/verification_bottleneck.json`, so for the regen path the scrub is hygiene, not a
precondition — but the principle stands: **enumerate examples as a contamination surface.**)

**The discipline:** when deciding whether to expose a threshold to an author, ask: is this
a *definitional* bound on what the substrate can be (author needs it) or a
*measurement-independent decision rule* the engine applies to an author-estimated value
(exposing it corrupts the signal)? For DR: extractiveness and suppression are definitional
bounds (mountain = low extraction, full stop); accessibility collapse is a
measurement-independent signal the engine checks against its own threshold, so it should
not appear in the authoring instructions.

---

## When reasoning has run out

A corollary, since both patterns above were diagnosed by *running greps*, not by thinking:
design reasoning has a stopping point past which the next real information comes only from
building and testing. Claims like "the UUID survives regeneration" or "this naming scheme
holds at scale" cannot be settled by argument — they are settled by regenerating a small
corpus and watching what breaks. When a design question has been reasoned to the point
where further turns produce elaboration rather than resolution, that is the signal to build
the thinnest real version and test it, not to think harder.

---

## Every diagnostic needs a positive control

A diagnostic is itself a producer, and its null result is the spine one level up: **a clean read
is byte-identical to a read that didn't look.** An empty grep, a `findall` that returns `[]`, a
count of `0`, an "I found it nowhere" — each can mean "nothing is there" *or* "the probe never
dispatched, queried the wrong thing, or was never run." The two are indistinguishable from the
output alone; absence and "looked and found absence" collapse to the same token at the read site,
exactly as in the five patterns.

This conversation supplied four instances, all the same shape:
- the bound Pattern-3 probe reported **432** `natural_law` constraints — a result set produced by a
  query that **silently failed to dispatch** the lock clauses; the engine's real count is **404**;
- a `0 facts` result means "this predicate is empty" only if the query was *aimed right* — otherwise
  it means "didn't look right";
- the G3 dead-code triage (OQ-38) nearly read an **empty caller-set as orphaned code** — absence of a
  found caller taken for absence of a caller;
- and the meta-instance: the claim that this document's spine "is stated nowhere else / written
  exactly once," asserted *without reading the whole document*. Running that control — reading the
  full doc — found the spine already **partially stated** in Pattern 5 ("both conflate missing with
  measured") and the sibling notes, correcting the claim. This section exists because its own
  positive control fired.

**The rule:** every diagnostic — grep, query, *or a reasoning claim of the form "X appears nowhere /
happens never / is unique"* — must be run against a **positive control**: a case you know in advance
it must flag. If it does not fire on the known-positive, its clean result on the real question is
worthless. This applies to reasoning about the code, not only to shell commands: an analyst
asserting "this is stated only once" is running an unfalsified diagnostic on the document, and "I
didn't find it" is not "it is not there" until the finder is shown to find.

**Two catches from the OQ-33 unknown-ruling arc (2026-05-31) — the method validating itself.** The
positive control fired twice in one session, the two catches together showing the rule guards *both*
directions of the absence-as-value sin and that it composes on itself:

1. **The control overturned the ruling that commissioned it.** The standing instruction was "return
   `unknown` for absent suppression" (Pattern 4's fix direction). Tracing it end-to-end against the
   corpus *before shipping* showed the premise was empirically false: **650/656** rows carry an
   authored scalar, so a blanket `unknown` would have discarded real measured data — committing the
   absence-as-value sin **in the other direction** (reading *present* data as absent). The ruling
   was wrong and its own verification caught it pre-ship. A positive control is not only a guard on
   clean nulls; it can falsify the *premise of the action* it was run to support — which is the more
   valuable firing, because it overturns rather than confirms.

2. **A positive control of a positive control — the recursion, run not just documented.** The first
   attempt at the row-26 control — a *guard-falsity count* — was itself caught vacuous by *its own*
   positive control: the guards succeed even for a deliberately bogus constraint, so the count
   discriminated nothing (a clean "0 failures" that meant "didn't test," the spine exactly). It was
   replaced with a sound **999.9 branch-reachability tripwire** that *does* fire on the
   known-positive. A diagnostic checking whether another diagnostic actually discriminates — the
   recursion this section names, executed against substrate rather than asserted. The check checked
   the check.

---

## Separate fallible judgment from action at the tool boundary

The meta-lesson behind several patterns above (Pattern 4 fabricated default, Pattern 5
absence-satisfies-the-gate, and the recap-as-witness defect in `CLAUDE.md`): **when an agent's
judgment and its actions are both fallible in the same direction, separate them at the tool
boundary so the fallible judgment cannot execute itself.** The agent that would mis-decide is the
same agent that would carry out the mis-decision, so a single pass that both decides *and* acts
compounds the error with no checkpoint — and when the error mode is silent (a warning dropped from
context, a gate passed on absence, an edit reported but not made), nothing surfaces it.

A *procedural* separation ("classify first, then act") is not enough, because it relies on the
same fallible agent to honor the boundary it is biased to cross. A *structural* separation removes
the capability: give the deciding pass no tools that can act, so honoring the boundary is not a
choice it can get wrong.

**Instance (2026-05-31, KNOWN_STATE.md split).** Classifying 18 changelog entries as
"standing-warning" (keep in auto-loaded context) vs "history" (move out) is judgment that fails
silently when wrong: misclassify a warning as history and it leaves context with no error, exactly
the seam the whole audit arc was about. The classify pass was run as a **read-only Plan agent**
(no Edit/Write tools) that could only emit a table and hand it back; a separate pass reviewed the
table, overturned five of its six borderline calls against independently-checked evidence (greps
proving anti-duplicate coverage), and only then executed the promotions and the move. The agent
whose classification was wrong six times *physically could not* act on those classifications. The
gate was the tool boundary, not an instruction.

**The discipline:** for any step where (a) the decision is judgment-grade (not mechanical), (b) a
wrong decision fails silently, and (c) the same actor would both decide and execute — split it.
Propose/apply, plan/execute, read-only-reviewer/separate-approver, dry-run-then-commit are all the
same move: deny the deciding pass the ability to enact its own decision, and put an independently-
sourced check in between. The cost is one handoff; the return is that a silent misjudgment cannot
self-execute. Verify the reviewer's check against substrate (run the grep, read the file), not
against the first agent's report — otherwise the second pass just ratifies the first and the
separation buys nothing.

---

## The spine: every defect here is an absence that presents as a presence

The five patterns are one shape seen in five places. In each, something is **missing** — a
consumer, a canonical-fact, a clause dispatch, an authored datum, an authored disqualifier — and
the missing thing is filled by a **success-shaped token** the read site cannot tell from the real
thing. The producer ran; both copies parse; a solution came back; a plausible constant arrived; the
gate passed. Presence is reported where there is absence.

This is the *what* the patterns share. It is distinct from, and complementary to, the two other
generalizations already in this note: the *why* (the intro — the reconciling step is deferrable and
the producer looks finished) and the *where* ("The shared root" — design against the corpus you are
heading toward, not the present sample). Three orthogonal axes, not three rival roots. The spine is
the *what*.

| # | Pattern | The hole (absence) | The success-shaped token that fills it | The read site it fools |
|---|---------|--------------------|----------------------------------------|------------------------|
| 1 | Produced-but-not-consumed | no consumer reads the output | the producer ran and wrote the file → "done" | whoever checks the producer |
| 2 | Silent fork | no fact says which copy is canonical | both copies exist and parse → "it's there" | a step targeting "the" file |
| 3 | Bound-probe bypasses cut | the lock clause never dispatched | a solution came back → "it's in the class" | the `findall` result/count |
| 4 | Fabricated default | the datum was never authored | a plausible constant (`0.5`) → "a measurement" | the downstream computation |
| 5 | Absence satisfies the gate | the disqualifier was never authored | the gate passed → "checked and clear" | the gate's boolean |
| — | (diagnostic layer) | the probe didn't actually look | a clean/empty result → "nothing there" | the analyst reading the result |

Pattern 5 already states this for the P4↔P5 pair ("both conflate missing with measured"); the spine
is that statement widened to all five and to the diagnostic layer below (see *Every diagnostic needs
a positive control*). The bottom row is why diagnostics are not exempt: a null result is the same
shape one level up.

**The fix is one move, too.** Every pattern's rule above is the same act: **carry the provenance bit
with the value, so absence and success stop collapsing to one token at the read site.** A bare value
is a lie of omission the consumer cannot detect — it asserts "this is real" by saying nothing about
whether it is. Make the absence representable and branch the consumer on it:

- **P1** — wire the consumer, or **fail loud** when output is left unconsumed (don't let "written" stand for "used").
- **P2** — make canonicity a **checked fact** (a documented path, a CI assertion), not a copy that merely exists.
- **P3** — let the **engine dispatch** (query unbound, post-filter); don't let a bound probe substitute for the cascade.
- **P4** — return **`unknown`, not `0.5`** — an out-of-band token the caller is forced to handle.
- **P5** — **fail-closed on absence**: the gate may not pass until the datum is authored.
- **(diagnostics)** — pair every probe with a **positive control** before trusting its clean result.

The shared invariant: *a value and "no value" must never be the same token where someone reads them.*
Where they are, that read is unfalsified — and somewhere downstream, absence is being reported as
presence.
