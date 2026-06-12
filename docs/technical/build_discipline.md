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

## The working method: separated passes (the procedure that prevents the patterns)

The patterns below are introduced when deciding and writing happen in one undivided pass. The
foundational counter-procedure is four phases, kept separate. (The deepest member, the
deciding/acting split, is treated in full under *Separate fallible judgment from action at the tool
boundary*; this is the crisp statement of the whole method.)

1. **Read-only deciding passes precede write passes.** A pass that gathers evidence and decides what
   to do does not also mutate files. Decide first from what you read, then write in a separate pass.
   This is the audit discipline (collect, *then* analyze) applied to editing: interleaving lets a
   half-formed conclusion edit the substrate before it has been checked.
2. **Human-ruled adjudication.** A call that is genuinely the human's — an ambiguous requirement, a
   contradiction between sources, a trade-off with no default — is escalated, not self-resolved.
   Decide what the evidence settles; do not decide what only the human can rule.
3. **Paste-or-untag.** Every "done / verified / fixed / passing" claim carries its witness — the
   pasted run, diff, or count — in the same turn. If the witness cannot be produced this turn, drop
   the done-tag and mark the item OPEN with its graduation step. A claim without its witness is
   untagged, not done — this is Pattern 1 (produced-but-not-consumed) one layer up: a claim produced
   without the witness that consumes it.

These three are the same stance run forward through the work: decide on evidence, rule only what is
yours to rule, and never let a summary stand in for a witness. Each defect pattern below is what
happens when one of these phases is skipped.

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
- `python/w1_sheaf_join.py` wrote `outputs/w1_sheaf_join.{json,md}` as a post-process that
  `run_pipeline.py` never re-ran. A later pipeline run refreshed `pipeline_output.json`
  (n=563 → 772) but left the join frozen at the old snapshot — its embedded manifest still
  read `n=563 / b5ccee0` while the corpus had moved on. The artifact had *consumed* its
  inputs once, so it looked done; nothing re-ran it, so it silently went stale and read as
  current. Fixed by wiring it into `run_pipeline.py` after the manifest step.

**Rule:** a producer is not done until something consumes its output. When you add a step
that writes data, in the same change either wire the consumer or add a check that fails
loudly when the output is left unconsumed.

**Sub-pattern — consumed-once ≠ kept-fresh (the staleness chain).** Wiring a consumer *once*
is not enough: a derived artifact is stale the instant any input is regenerated without it.
The bar is not "a consumer exists somewhere" but "the orchestrator (`run_pipeline.py`) re-runs
the consumer whenever upstream changes." Three obligations when you add to the chain:

- **Wire it into `run_pipeline.py` in dependency order.** Place a new step after the steps
  that produce its inputs and before the steps that read its output. A step run out of order
  reads stale inputs and writes a stale-but-error-free result. Canonical chain to respect:
  `pipeline_output.json` (Prolog) → `enrich_pipeline_json.py` → `enriched_pipeline.json` →
  `enhanced_report.py`; anything `enhanced_report.py` newly consumes must have its producer
  wired and ordered upstream of it, not just exist.
- **Certify the whole transitive chain, not just your link.** Second-order staleness: if your
  step reads an artifact that can itself go stale, or writes one others read, staleness
  propagates. Adding a node means re-running and re-certifying everything downstream of the
  insertion point, out to the leaves — not only the node you touched.
- **Make freshness checkable, not assumed.** Stamp the same run manifest into co-produced
  artifacts (the `orbit_data.manifest.json` sidecar) and have consumers assert same-run before
  joining (the `w1_sheaf_join` guard). Then a mismatch fails loudly instead of yielding a
  stale join that reads as current.

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

# stale derived artifacts: embedded manifest older than the latest pipeline run
python3 - <<'PY'
import json, glob, os
ref = json.load(open("outputs/pipeline_output.json"))["manifest"]["pipeline_run_at"]
for f in glob.glob("outputs/*.json"):
    try: m = json.load(open(f)).get("manifest")
    except Exception: continue
    if m and m.get("pipeline_run_at") and m["pipeline_run_at"] != ref:
        print(f"STALE: {os.path.basename(f)} @ {m['pipeline_run_at']} (pipeline @ {ref})")
PY
```

---

## Unwired ≠ worthless — judge a dangling wire by its contribution, not its consumers

Pattern 1 is a **build-time** rule: when you *create* a producer, finish the wire. It does **not**
license the inverse at **audit time** — finding an unwired producer and concluding it is cruft.
"Has a consumer" / "is wired into `run_pipeline.py`" answers *is it currently used*, not *is it
useful*. Those are different questions, and the consumer test is the wrong one for worth — it is
the test every model reaches for first because it is mechanical and cheap, which is exactly why it
misleads. The same trap holds for *fires on the corpus*: a diagnostic that never fires may be dead,
or the corpus may simply not exercise it (test cross-corpus + against reference exemplars before
concluding anything — see *Every diagnostic needs a positive control*).

**Every subsystem here was initiated for a reason.** Boltzmann compliance, the FPN, the signature
taxonomy, the trajectory classifiers — each was built to extract a specific analytical product. An
unwired one is evidence the *build* was left unfinished, not that the *idea* was worthless. The
verdict comes from the value question, not the wiring status:

1. **What analytical product does it yield?** (a classified type, a coupling score, a type-path
   over time, an observer residual …)
2. **Does any *live* subsystem already yield that?** If yes → candidate **duplicate** (cruft).
3. **If not, what would it add once wired?** **Unique** signal, or a **refinement** of an existing
   signal → *unfinished value*. Remedy: **wire it** (or record it as an intended, not-yet-built
   capability in `design_gaps.md`), **never retire it on wiring grounds**.
4. Only **duplicate-of-X** or **yields-nothing-interpretable (vestigial)** is genuine cruft.

So the liveness / firing / consumer passes are **evidence-gathering** (what exists, what is
exercised — across the live corpus *and* the archives) that *feeds* the value adjudication; they are
not the adjudication. Wiring status is at most a **prompt** to ask "what does this bring?", never the
answer.

**The asymmetry that makes the consumer-test dangerous:** retiring valuable-but-unwired silently
destroys a distinct analytical capability (it reads as "removed dead code"); keeping a duplicate
costs a little clutter. The error is not symmetric — when unsure, **preserve and adjudicate**, do
not delete on wiring grounds.

**Instances (including this doc author's own slip):**
- The **8 zero-firing signatures** (`natural_law`, `coordination_scaffold`, `piton`,
  `false_natural_law`, `false_summit_mountain`, `constructed_low`/`constructed_constraint`,
  `ambiguous`) fire on none of the live corpus. Each names a *distinct* constraint type — unique
  signal even at zero current firings; "not exercised here" is a fact about the corpus, not a worth
  verdict.
- The **old trajectory classifier** (`snapshot_type`/`degradation_chain`) sits unwired beside the
  now-live `drift_trajectory`/`temporal_residual`. Reaching for "superseded ⇒ cruft" was the error:
  the old classifier yields a **categorical type-path** (rope→snare→…); the live one yields a
  **quantitative metric series**. Different products — so it is plausibly *unfinished value to wire*,
  not a duplicate. The wiring told you it was unused; only the value question tells you whether it is
  worth keeping.

---

## Over-confident moves on the synthesis side: false-absence, false-unification, and the unguarded axis-swap

These are the auditor/assistant's own failure modes, distinct from the five build defects: they are
errors of *claiming*, not of building. Both were caught repeatedly in one session (2026-06-10) — each
time by the human supplying the positive control the assistant should have generated. "Be careful" does
not fix them; a structural rule does.

**False-absence — owe a positive control before any "absent / can't / unrepresentable / no X."** An
absence or impossibility claim is the highest-confidence-lowest-evidence move available, and it is the
assistant's characteristic error. It must carry its probe — *grep a name you KNOW exists to prove the
search fires; construct the case the thing must flag* — or be tagged **OPEN**, never emitted as a
finding. Instances: claimed "no fixer predicate exists" (missed `agenda_setter` — wrong grep layer);
claimed a constraint type "unrepresentable, needs new design," then over-corrected to "representable
now" (the *headline* oscillated to match the interlocutor while the *body* kept the true caveat). Two
sub-rules: **(a) the headline must carry the body's caveat** — if the body says "X deferred / proxy
only," the headline may not say "solved"; a proxy improving is not the mechanism becoming checkable.
**(b) Control the claim at the altitude it's made** — a probe over predicate `f` licenses "absent in
`f`," not "absent in the system"; to claim the broader thing, extend the control to the other named
sites or narrow the claim to what was probed (the `transition_path` decay-vs-repair case: the grep
licensed "no upgrade head in the predicate"; the system-level claim needed the live-path and
trajectory-reporter checks added before it was earned).

**False-unification — owe a distinction-check before merging things that share concepts.** The
synthesis twin: two components sharing a vocabulary or a dynamics is NOT license to fold them, import
one's machinery into the other, or treat them as one. Check whether the architecture *mandates* their
separation first, and cite where it rules. Instance: proposed "import the repair half from the
committer axis into the observer axis" — exactly the fold `deferential_realism_paper_v7.md` mandatorily
refuses (Theorem 7 Detection Independence: the axes detect disjoint failures; "the cost of the second
axis is the discipline of keeping it separate"). Shared dynamics across distinct objects is **analogy**
(inspiration), not a bridge. A subtler form is **cross-metaphor welding**: "scaffold = {maintain,
splice, replace}" composes the construction metaphor with the rigging metaphor; the type vocabulary is
multi-metaphor *by design* and the source domains do not compose. Rule: when a synthesis wants to
combine two named things, state the separation it might be violating and cite the ruling, before
proposing the merge.

**Axis-introduction owes a PRE-REGISTERED discriminating control — the operational guard for
false-unification, and a peer rule because it reaches a layer the others don't.** Read-before-write
guards against writing a file wrong; escalate-what's-yours guards against deciding a human's call.
Neither catches the move where a synthesis *introduces or relabels an axis* and quietly re-labels a
settled one — no file is written wrong, and it doesn't feel like a ruling. So: when you propose a new
axis (or claim that some axis X separates two types), construct the case where the new axis and the
**nearest prior axis come apart**, **pre-register what each outcome means before the run** (so the
result can't be narrated into agreement), then run it. Witnessed instance (2026-06-10): a proposed
"designed vs undesigned" 2×2 was falsified by the DMV control — designed + *uncaptured* extraction
landed in `snare`, so the real axis is **capture**, not design; and "emergent coordination = piton's
mirror cell" was falsified by the desire-path control (`→FSM`) vs the unmaintained-coordination
control (`→FCR`) being distinct cells (scatter). **The honest framing is the standing risk this names,
not a victory:** in every instance so far the discriminating case and the pre-registration came from
*outside* the loop (the operator), not from the synthesizer. The job the rule sets — generate the
discriminating control *for your own synthesis, before it lands* — is the one still unmet by default;
the rule is the named guard against a gap that is real and, so far, externally caught. Corollary
(under-claim): one discriminating witness earns "the axes are separable / the prior label is wrong on
this point," NOT "orthogonal/independent across the range" — that is the hypothesis it opens.

**The shared root:** all three are the generative/confident faculty outrunning its evidence —
false-absence collapses "I didn't find it" into "it isn't there"; false-unification collapses "these
rhyme" into "these are one"; the axis gap is "I relabeled the axis and never ran the case that would
tell." The same fix shape works on each: name the witness the claim would need (a firing probe; a
ruling on separation; a pre-registered discriminating control) and either produce it or tag the claim
OPEN.

**Hedging-as-rigor — the under-confident dual (held-open owes a falsifier check).** The mirror
image of the three moves above, with the same root (claiming decoupled from the witness apparatus)
and the opposite sign: the synthesis *refuses* a commitment it could make. The default that produces
it treats hedging as rigor — "two rival readings, the data cannot distinguish, adjudication
deferred" reads as careful. The house discipline runs on the opposite division of labor: **the
prose commits, and the uncertainty lives in the falsification apparatus.** "Held open" in the body
is earned only when no falsifier can be specified; **if a kill condition is available, the claim
must be made and the kill condition attached.** Under-claiming a committable verdict is not the
safe direction — it moves the error from the claim to the reader, who now lacks both the verdict
and the test that would break it. Witnessed instance (2026-06-11, Pew political-typology essay
exchange): a "Counter-Reading, Held Open" section was drafted agnostic between two rivals while
the synthesis that adjudicated them was already available in the same material; an external
reviewer's *question* (not an edit) forced the commitment. **Generation-time trigger:** catching
yourself drafting a both-readings-possible passage IS the check — ask whether
commitment-plus-falsifier is available *then*, not at the review round. Two corollaries from the
same exchange: (a) **claims-with-falsifiers per piece is the draft-time metric** for synthesis
output (the review round's measurable effect was one claim with two hedges → three claims with
three clocks); (b) when triaging multi-reviewer feedback, **weight reviewers' questions over their
line edits** — edits propose substitutions inside your frame; questions force synthesis across it.

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

**Standing canonical location — audits.** The audit corpus was itself a Pattern-2 instance at
directory scale: writeups in `docs/`, one in `docs/audits/`, one in `docs/technical/`, findings in
gitignored `outputs/`, self-contained packages at root (`audit/`, `audit_data/`, `audit_proposal/`,
`phase1/`), plus two true forks (the scaffold-piton writeup in `docs/` vs `python/docs/`; the
repo-reorg proposal in `audit_proposal/` vs `audit/agy/`). Consolidated 2026-06-04. **Mandate:
every audit lives in `audits/<YYYY-MM-DD>_<slug>/` — one subdirectory per audit, writeup +
evidence artifacts together.** `outputs/` stays the scripts' regenerable workspace; `audits/` is
the versioned archive; scripts stay in `python/audits/`. A writeup left in `docs/` or findings
left only in `outputs/` re-open this pattern. Conventions: `audits/README.md`.

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

## Pattern 6 — Success-shaped absorption (measured-empty and didn't-look collapse to one output at aggregation/channel boundaries)

Three instances witnessed in ONE DAY (2026-06-10), at three altitudes — which is what
promoted this from notes to a numbered pattern:

1. **Value altitude:** `system_gradient`'s `[] → 0.0` fallback (`coercion_projection.pl`).
   Every gradient computation ever made failed (the `time_point_in_interval` cut bug) and the
   fallback emitted `0.0` — byte-identical to a measured flat gradient — for the construct's
   entire life. The bug was invisible BECAUSE the absorption was downstream of it.
2. **Channel altitude:** `grep -v Warning`. The `domain_registry` dangling-module warning
   printed at every load for four months into a universally filtered channel, until the dead
   reference crashed the validation suite at runtime (OQ-96).
3. **Aggregation altitude:** `system_gradient`'s findall over levels. A constructed 8/32
   one-level grid yielded `G_sys=0.216` presented as a SYSTEM reading with a full
   `increasing_coercion` verdict beside `completeness=0.25` — missing levels contribute
   silence, not absence-marks, and the consumer never consults coverage (OQ-93 stage-2
   battery item 4).

**The class:** an aggregation or channel that cannot distinguish *measured-empty* from
*didn't-look*, emitting success-shaped output either way. It is the spine's defect one
composition up: each COMPONENT may be individually sound (the findall is correct; the filter
is deliberate; the default is documented), and the absorption happens where they compose —
which is why none of the three instances was caught at its own site.

**The rule:** aggregates carry their COVERAGE (what fed them) to the read site; channels carry
ALLOWLISTS (what silence is allowed to mean — `load_warning_gate.py` is the template);
defaults-on-empty get the Pattern-4 treatment (return `unknown`/OPEN, never a plausible
value). Fail-closed per-QUESTION, not per-dataset: sufficiency is a property of the question
(a one-level grid is adequate for a one-level read and worthless for a two-needle verdict —
no global threshold encodes that; consumer-named requirements do).

**Diagnostic:** every `findall`-feeding-aggregate and every default-on-empty branch
(`;  X = 0`, `;  X = 0.0`, `-> ... ; Default`) is a candidate site. Bounded census of live
code for these two syntactic shapes: EXECUTED 2026-06-11
(`audits/2026-06-11_oq97_pattern6_census/`, OQ-97 resolved) — 19 idiom classes over 106 files;
8 confirmed-candidate classes tracked as OQ-112; new sites sort against the class table there.

**Relationship to the other patterns:** Pattern 4 is the value-level case (one fabricated
default), Pattern 5 the gate-level case (absence satisfies a check); Pattern 6 names the
aggregation/channel level where sound parts compose into an output that erases the
distinction both lower patterns protect.

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

**The qualifier needs its own qualifier — name the level.** "Do not cite cosmetic unqualified" is
itself a claim that is incomplete until qualified, and the qualifier is a *level*, not a hedge. The
verdict splits cleanly by output layer: at the **final-type level** (the classification the engine
emits) removing the NL signature is **fully cosmetic** — 0 mountain types change, no caveat. At the
**raw-diagnostic level** (the `natural_law` certification tag the signature stamps before the type is
finalized) it is **non-cosmetic** — it flips 3 false-naturals. The same edit is simultaneously
inert and consequential because it touches two different read sites; "cosmetic" is true at one and
false at the other, and neither is the whole answer. So the discipline is not "always append a
caveat to cosmetic" — it is **state the layer the verdict is scoped to**, because a layer-free
"cosmetic" defaults to whichever layer the reader happens to be standing on (here, the type layer,
which is the *true* reading — making the omission silently self-confirming). This is Pattern 5's
spine one turn further in: a success-shaped token ("cosmetic") that is genuinely true at the layer
the reader checks and false at the layer they don't, so the read site can't tell the scoped claim
from the universal one. Carry the level bit with the verdict.

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

**The oracle must differ from the probe in the exact dimension the probe could be wrong on, or a
set-for-set "match" is vacuous.** A positive-control oracle that shares the probe's matching logic
proves *agreement*, not *discrimination*: the two agree because they make the same mistake, not
because the probe is right. The independent derivation has to vary precisely where the probe is
fallible. Instance (2026-06-02, reading-axis obstruction OQ-54): the `cs_kernel_obstruction` probe
classified a kernel `real_closure` by exact-matching a `forecloses` target against the kernel's
reading names; the "independent" oracle did the *same exact-match*, and they matched set-for-set at
84 — which proved nothing, because the shared flaw was **name resolution** (targets authored short,
`ishmael_covenant_reading`, vs registered full, `abrahamic_covenant__ishmael_covenant_reading`). A
genuinely independent oracle that *normalized* the name form found 10 more must-flag kernels the
probe was silently missing. Only after the name-form dimension was repaired (so `fixable-remaining =
0` — no residue in the flaw dimension) did the set-for-set match (94 = 94) carry information. The
discipline: before trusting a set-for-set match, name the dimension the probe could be wrong on and
confirm the oracle is derived *without* that dimension's logic — and that the residue in it is zero.

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

## A gating count is not a finding without its composition (compute the breakdown in the SAME pass)

A count that is about to gate a decision (a corpus build, a verdict, a "build vs don't") is **not a
weaker version of the result — it is a different and usually wrong result.** The count and its
composition can point in **opposite directions**, not just differ in magnitude. So the
cause/composition breakdown is computed **in the same pass that produces the count**, never as a
follow-on when someone doubts the headline. The count-alone should not be written down as a
candidate gate number even provisionally, because once written it gets reasoned about as if it were
the finding.

Two instances this thread, both the same shape, both caught only because a per-item check ran
*before* the count became a gate:
- **OQ-83 4b:** "renamed-not-escaped, the migration re-imposed the straitjacket" — the headline. A
  one-line consumer grep ("does anything read `in_contention`": zero consumers) showed the count was
  about an annotation predicate that feeds no classifier; the finding was the opposite size and kind.
- **OQ-87 diverge-A:** "74 detection-independence cases" read as "orthogonal detection is real." The
  cause-of-death distribution under the 74 showed **~89% is one drift-authoring convention firing
  uniformly through the observer-coherent slice** (the saturation already declared untrustworthy,
  leaking into the one cell thought clean); the clean content-driven core was ≤8. The count did not
  overstate the effect — it **misidentified** it.

**The rule:** for any count that will gate a decision, its composition (cause distribution / the
per-item breakdown that says *what the count is made of*) is part of the deliverable that produces
the count, in the same pass. "N cases of X" is never the finding; "N cases, of which k are
content-driven and N−k are one convention" is the finding. This is the positive-control discipline
applied to your own headline: the breakdown is the control that catches the count standing in for
the substrate.

---

## Count-as-witness assumes a single writer (under parallel instances, the diff is the witness)

A global count used as a commit's witness — "checker: 94 parsed, 0 malformed" offered as proof
that THIS edit landed correctly — is valid only while one writer holds the ledger. The moment
parallel instances write the same file, the count's delta carries every writer's changes at once:
a 94→95 alongside another instance's new entry no longer isolates this session's edit (observed
2026-06-10: OQ-94 corrections committed while a parallel instance landed OQ-95; the parse-count
delta was confounded in the same hour the practice would have been cited). The checker's PASS/FAIL
stays valid — it certifies the whole file's grammar — what breaks is the COUNT as an edit-witness.

**The rule:** a commit's witness must be scoped to the commit — the diff (`git show --stat`, or
the pasted hunks), or an entry-anchored check (query the specific entry the edit touched) — never
a global count. Global counts remain fine as whole-file gates (the checker's exit code). This is
the single-writer assumption made explicit: counts aggregate; diffs attribute. Same family as the
section above ("a gating count is not a finding without its composition") — under multi-writer,
the composition of a count delta includes other writers' work.

---

## Perturbation is the probe; invariance is the read (a claimed invariant needs a perturbation that moves it)

The engine's whole read is **perturb one axis, hold the rest, sort what stays (invariant) from what
moves (variant)** — observer (`reading_diff`), axiom (`axiom_diff`), time (the drift machinery), and
the apparatus itself (`perturb.py`, the stability band). Theory: `docs/the_perturbation_principle.md`;
code shape: `docs/design/the_perturbation_move.md`. The build-discipline consequence is one sentence:
**an invariance claim is the null result one level up, so it inherits the spine.** "Invariant under
perturbation" is byte-identical to "I never perturbed," "I perturbed the wrong axis," or "my probe
didn't dispatch" — the same absence-as-presence the five patterns share, now wearing the costume of a
*stability* finding instead of a clean grep.

So a claimed invariant is unfalsified until a **perturbation you know in advance must move a seated
verdict** fires on it — the positive control of the section above, specialized. The canonical one is
**self-diff**: a reading diffed against itself must return all-invariant under the strict key
(`reading_diff(X,X,exact,_,[],[])`; `axiom_diff(X,X,exact_name,Ag,[],[])`), and the operator must be
shown to *find* variance on a known-variant case before its "invariant" on the real case counts. An
operator that cannot see "no difference" cannot be trusted to see difference.

Two failure shapes specific to the invariance read, both already in the patterns above:

- **The phantom invariant (Patterns 4–5 in stability clothing).** Zero variation has two causes that
  present identically: a genuinely fixed axis, and a perturbation never run / an absent datum
  defaulted. The `Supp=0.5` fallback (Pattern 4) injects a value that does not move *because it was
  never authored*, indistinguishable at the read site from a value that does not move *because it is
  fixed*. Tripwire the fallback (perturb it to an out-of-range value); if the "invariants" flip, they
  were phantoms. An invariant you *found* and an invariant you *failed to probe for* are the same flat
  result until the control separates them.
- **The baked axis (S2's no-seat pose in code).** The perturbation key is the seat; defaulting it
  silently is a concealed seat. The substrate refuses this on purpose: `reading_diff` **throws**
  rather than fake a `weighted` partition it cannot honestly form, and `axiom_diff`'s `axiom_concept/2`
  is empty by default with the report stating that concept-alignment is therefore all-blind. A
  perturbation operator that picks its own axis without being told is not "convenient" — it is
  reporting a seated verdict while concealing the seat, which is the one inconsistency the framework
  names. **Make the axis a required argument; let the operator fail loud rather than choose for you.**

When you add the *next* diagnostic, recognize whether it is this move (object, perturbed axis, authored
value read, declared key) and give it the same shape and the same self-diff control — do not rebuild it
bespoke and do not let it assert an invariant it never tried to break.

---

## Cross-sibling comparison disambiguates authored-field calls (the corpus as its own control)

When a per-item call about an authored field is ambiguous in one file — is this beneficiary value
an agent or a vindicated proposition? is this omega epistemic or structural? — the corpus usually
already contains the disambiguating perturbation: a sibling reading of the same kernel, or a
sibling kernel of the same topic, that foregrounds the same structure differently. Reading the
siblings side by side is the perturbation move run over **authored text** instead of engine
output: hold the structure, vary the authorial framing, and the variation exposes which features
belong to the referent's kind and which to the file's framing. **Standard practice:** before
escalating an ambiguous authored-field call as undecidable, check the siblings (`cs_kernel_id`
groups, `cs_reading_relation` edges, name-prefix families). The comparison is cheap and often
decisive as a hypothesis-generator.

The footing rule that keeps it honest, load-bearing: **cross-sibling comparison GENERATES the
hypothesis; only an in-file witness RULES it.** Distinct kernels (separate `cs_kernel_id`s) make
the transfer analogical — not a rigorous single-kernel perturbation — so where the in-file
witness is absent and only the analogy carries, mark the call INFERRED, not ruled. (Same-kernel
sibling readings are closer to a true perturbation but are still distinct constraints with their
own ε — sibling readings are distinct probes, never coverage.)

Witnessed instance (2026-06-04, OQ-63/OQ-64): `institutional_continuity_narrative`
(preparedness_commitment__husk_reading) read proposition-shaped in isolation. The sibling kernel
(preparedness_transmission__husk_reading :102) front-loads "Central Government Administration …
captures political credit" — the same institution-capturing-legitimacy structure, foregrounded —
which generated the AGENT hypothesis; the commitment file's own directionality logic (:225, "the
institution itself experiences the constraint as moderately beneficial") then confirmed it:
ruled, not inferred. Both directions of the name/referent orthogonality are now witnessed
(ISSUES.md OQ-64): a proposition referent in the agent-shaped beneficiary field
(maxwell_demon's `entropic_universe_hypothesis`) and an agent referent under a
proposition-shaped name (this case). Consequence: **the value string can never carry the call —
the authored gain/directionality text does.** Suffix heuristics lied twice before this was
accepted; do not reintroduce them.

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

## Commit-as-you-go: a witnessed unit of work is committed when witnessed, not at session end

**Operator ruling (2026-06-09): standing permission to commit without asking.** The repo is CC0,
single-operator, iteration-over-correctness; mistakes are recoverable through git itself. The
agent does not need (and should not wait for) per-session commit authorization.

**Why this is a build-discipline rule, not a convenience.** Uncommitted work has the same failure
mode as an unpasted witness: it exists only in a volatile medium. A session that resolves five
items and plans one end-commit holds all five in-flight for hours — exposed to context compaction
(the agent loses the detail needed to write the commit honestly), harness outages (observed
2026-06-09: an execution-classifier outage froze all commits for a working session whose changes
were complete), and cross-instance interference. The end-of-session batch commit is
recap-as-witness applied to git: "I'll commit it all later" is a done-claim whose witness does not
yet exist.

**The discipline:** when a unit of work is *witnessed* (its paste-or-untag obligation is
discharged), commit it then, as its own commit. Granularity follows witness boundaries, not
session boundaries. The output-changing vs behavior-preserving split (memory:
`feedback_output_changing_commit_discipline`) still applies within this — committing often does
not license mixing the two in one commit. Corollary for multi-instance work: **one instance per
git worktree** (`git worktree add ../wt-<task> <branch>`); two instances sharing a working tree
step on each other's uncommitted state — which commit-as-you-go shrinks but does not eliminate.

---

## A witnessed fact has a shelf life: the citation-time rule and the staleness ladder

The paste-or-untag rule (CLAUDE.md governing stance; the "recap-as-witness" pattern) fires at the
moment of **assertion** — when you report something done, carry its witness that turn. It is silent
at the moment of **reuse**. But a premise cited in a later argument is a fresh assertion of the fact
wearing the clothes of a settled one: "we verified X, so Y" turns the witness into a token, and the
token travels while the artifact stays behind. Two distinct leaks hide under this:

- **Staleness.** The run was real at commit A; you are at commit C; "tests pass" is now a true
  statement about a state that no longer exists. The witness wasn't false — the world moved under it.
- **Compression-laundering.** Even at the same state, "verified X" promotes the witness to "known,"
  and "known" gets cited by its conclusion-label, never re-checked.

**The edge, unsoftened:** this is *not* fully fixable with a better tag. Summarization is
definitionally the discarding of the witness — a summary that carried every witness wouldn't be one.
So "carry the witness everywhere" is self-defeating. The resolution is **triage**, and triage is not
binary (summarize vs re-run) — it is **assigning each load-bearing premise a rung on a four-rung
ladder**, each strictly more staleness-resistant than the last:

| Rung | Form | What it resists | Visible to |
|---|---|---|---|
| 1 | bare claim ("tests pass") | nothing — the token travels alone | nobody; it's laundered |
| 2 | **pointer** ("[§turn-1 run]") | nothing automatically, but it's re-checkable | a reader who bothers to follow |
| 3 | **as-of stamp** ("as of commit A / 00:10Z") | silent promotion — staleness is legible on the page | a reader who notices A ≠ HEAD |
| 4 | **gate** (consumer refuses on a stale premise) | staleness is *enforced*, not merely visible | the machine; it can't proceed |

**The triage criterion is two-factor: mutable-state-ness × cost-of-acting-on-stale.**

- A load-bearing **structural** claim ("the clause reads `agent_beneficiary`, not raw
  `constraint_beneficiary`") needs only **rung 2**: its witness is "read current source" — always
  available, always current, free to re-observe. It cannot silently drift past you, because
  re-witnessing it costs a `grep`.
- A load-bearing **state / event** claim ("the run passed," "the corpus held N") is the dangerous
  kind: the witness was a **past event you cannot re-observe, only re-produce**, so the world moves
  under the token. These need **rung 3 minimum**, and **rung 4** when a costly or irreversible
  decision acts on them.

Every rung is already instantiated in this repo — assigning rungs is the work, not building them:
rung 3 is the **pipeline manifest** (`code_commit`, `pipeline_run_at`, `code_dirty` — the as-of
stamp, already required for audits); rung 4 is the **same-run guard** in `w1_sheaf_join` (refuses to
join orbit data and `pipeline_output` from different corpus states); the **leak** is the same join
*before* that guard existed, frozen at n=563 while the corpus grew to 772 (Pattern 1). The
highest-leverage move on any rung-4 premise is to promote it from discipline-note to mechanical gate:
"remember to re-run the check" is rung 2 wearing a rung-4 costume.

### The triage list (which premises may not travel without a live re-witness)

*Stub — set 2026-06-06; the contents are the operator's lever ("name the few"), edit freely. A
premise here may not be cited as settled without re-witnessing at its rung at point of use.*

| Premise | Kind | Rung | Re-witness at point of use |
|---|---|---|---|
| The live corpus / `pipeline_output` denominator is current | state | **4** | the `w1_sheaf_join` same-run guard (manifest `pipeline_run_at`/`code_commit` vs the data being analyzed); the concurrent-runs race (OQ-77) is this premise failing — never cite a corpus statistic without checking `manifest` is from one coherent run |
| The de-leak holds (no engine band reaches the authoring LLM) | state | **4** | dump `story_generator_base.build_prompt(...)` and grep for band values near type names (AGENTS.md Rule 3b) — currently a note; **candidate to promote to a test** |
| Validation / tests pass (before a push or a decision that acts on them) | event | **3 min** | re-run against current HEAD; cite with commit, never a prior turn's green |
| The ruled structural invariants (perception ≠ claim, OQ-70; agency-filtered d, OQ-63) | structural | **2** | `grep` the clause in current source — pointer suffices; reading the file *is* the witness |

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
