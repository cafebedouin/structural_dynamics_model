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
trajectory-reporter checks added before it was earned). **(c) The concept→surface mapping is itself a
claim, and a control that validates the SEARCH does not validate the MAPPING.** The (b) failure has a
subtler twin: the control ladder is impeccable — grep shown to fire, absence real at every rung — but
the *predicate probed* was the wrong carrier for the *concept claimed*, so a true "surface X is
unauthored" launders a false "concept C has no live channel." Witnessed (2026-07-25, OQ-255 audit §8):
"Q6's engine channel is authored-empty" rode a full control ladder on `coordination_vitality/2`
(genuinely 0-authored on every live leg) while the concept's actual carrier,
`founding_problem_status/2`, sat authored on 164/199 with wired consumers
(`narrative_ontology.pl:168-170`) — and both branches of a pre-registered discriminator inherited the
misidentification, so the downstream check was controlled and still aimed wrong. Rule: before any
"concept C has no live surface," sweep the sibling surfaces (the dynamic-declaration block, the
authored-field census, both axes — see *Two-axis classification surfaces* in memory) and cite the
sweep, not the one-predicate ladder. The absence claim inherits the weakest link in
concept→surface→search, and the first link is the one no grep controls.

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

**Reconciling two surfaces owes the full product table, not the named cells.** A special case of
false-unification's demand for a distinction-check, with its own trigger: when two predicates
measure "the same" thing (a flag and its graded generalization; a verdict and its metric), the
coherence claim is over the *entire* verdict×value product space. Derive every cell from the two
definitions and census-count each; the plan's enumerated special cases are hypotheses about which
cells are populated, not the table. Twice in one build chain the un-named cell carried the most
mass: the OQ-207 D4 ruling enumerated "two divergence cells (exact special case)" — deriving the
full `consensus_provenance/2` × `stakeholder_obstruction/5` table from the definitions found a
third (`plural([T,unknown])`), which was the *most* populated live (19/66/129 across the legs);
then the OQ-217 tightening's predicted-zero cell (`mcc_untypeable`) censused heavily live
(12/50/39). A coherence check that tests only the named cells passes over the cell nobody
predicted — enumerate from the definitions, let the census say which cells are empty.
(`audits/2026-07-12_oq207_stakeholder_h1/`, `audits/2026-07-12_oq217_consensus_tightening/`.)

---

## When to stop verifying: the verification-depth seat and the conceal-an-open check

**"Verified enough" has no seat-free answer — so the honest stopping rule is not "stop when
verified" but "stop when the next pass costs more than being wrong about what it would catch, *and*
every still-open thing is declared rather than concealed."** Verification depth is a seat
(`docs/seat-theorem-v1.md` §8: no level is innocent; climbing buys a new σ with the same defect, not
neutrality). There is no depth the facts *themselves* tell you to stop at, so a run of rewrites is
**not** convergence toward a correct stopping point that one-more-pass would reach. Each pass moves
the boundary between "live dial I'm still deciding" and "background I now treat as settled" — it is
the framing seat being **reoccupied**, and it can be reoccupied again (there is always a next pass
that notices a thing the last one had bundled into background). The regress is real; do not narrate
it as progress toward closure.

**This is the Omega framework's stopping rule, not a new one — cite it, do not fork it
(`docs/omega_variables.md`).** Clause 1 is the **cost-benefit** line of "When Reasoning Stops" — *"the
next possible refinement would cost more than it gains"* — **not** the stable-marriage
*structural-convergence* terminus in the same paragraph: that terminus assumes a fixed option set
(the doc's own Mechanism Boundaries: "no dynamic generation of new options mid-process"), and the
verification-depth regress is **generative** — each pass manufactures a *new* dial (orientation was
not among pass seven's options; the regress is `seat-theorem` §8's framing-regress, which generates).
Cite the cost line, which survives generativity; the structural-convergence terminus does not apply.

**"downgrade to OPEN" is literally "route to a typed Ω", and the Ω-type must be read against the
doc's definitions, not assigned loosely** (the load-bearing classification, corrected 2026-06-16):
- An unwitnessable **orientation** gloss (enclosure vs defense vs survival-frame) is a **deferred Ω_E**,
  **not Ω_P.** Ω_P is a value judgment that differs *legitimately across stakeholders* (resolved by
  those bearing the cost deciding). Orientation is a **fact about the concealer's actual stance** —
  observers differ in *access*, not legitimately in *values* — and its named resolution operation is
  *world-observation*: the Corollary-3 honor-vs-reabsorb confrontation signature over time
  (longitudinal — the paradigm Ω_E operation per `omega_variables.md`'s operation-locus annotation;
  filed as **OQ-133**, deps-linked to the diachronic tier OQ-83/109/110). **The stake is not
  taxonomic:** routing orientation to Ω_P would make its resolution "someone bearing the cost
  declares it," which hands the encloser the right to **self-certify as a defender by fiat** — the
  no-seat pose, the concealment move blessed by the routing. The Ω_E routing is exactly what
  *withholds* that license: orientation cannot be declared, it must earn its verdict from the
  honor/reabsorb pattern. *Falsifier (the Ω_E claim is hostage to it):* the signature tracks
  orientation only absent strategic gaming — a sophisticated encloser can **perform** honoring (drop
  the cover theatrically, keep extracting), forging the longitudinal witness the way a deepfake forges
  the index. Under gaming the operation fails and orientation falls **outside the framework entirely**
  (Mechanism Boundaries exclude "strategic gaming between components") — Ω_E in the non-gaming regime,
  out-of-framework under gaming, **never Ω_P in either case.**
- A genuinely **contested origin** (`contested_open`, rule 11) *is* an Ω_P/Ω_C — a real
  cross-stakeholder dispute about what the founding problem was; the engine correctly **abstains**. Do
  **not** collapse it with the orientation case: same surface ("OPEN"), opposite type, opposite
  operation — *abstain-as-preference* (a question that is preference/framing-shaped) vs
  *route-to-deferred-measurement* (a question with a fact and a witness not yet reachable).
- An undefined **tier boundary** is an **Ω_C**.

Three things this section adds *on top of* the framework, so the cross-reference is not pure
restatement: (a) the **seat-theorem layer** — the stopping point is *itself* a seat, so clause 2
("declared not concealed", Cor 2a) is forced, which the structural-convergence reading does not
frame; (b) the **mechanization** — run classify-and-route as a check on the engine's *own emitted
verdicts*, not only on stalled reasoning; (c) the **generativity boundary** — the regress manufactures
options, so the cross-reference holds only at the cost-benefit reading and the build-side case sits
*at/past* the doc's declared mechanism boundary, a sharper scoping than "it was already in the doc."

**The second clause is the whole difference, and unlike the first it is *checkable*.** The
cost/benefit half of the rule is itself a seat (you are estimating the cost of being wrong about an
unknown). But "are the opens declared or concealed" terminates: **for each verdict the engine — or
the synthesis — emits, name the tier-available falsifier, and check whether the name claims more
than the tier can witness. No tier-available falsifier ⇒ the name asserts an unfalsifiable-at-this-
tier claim ⇒ downgrade it to OPEN** (= route it to a typed Ω; `docs/omega_variables.md`). "Name a
falsifier or downgrade to OPEN" is decidable; "is this verified enough" is not. The depth seat does
not close, but the **did-I-conceal-an-open** seat does — it is checkable the way the seat
*declaration* law is checkable while the honoring downstream of it is not (`docs/seat-theorem-v1.md`,
Cor 2a). Run this single pass with a falsifier pointed at the engine's own outputs and it terminates;
it is cheaper than another rewrite and catches the defect class that rewrites chase by hand.

**Witnessed instance (this document's own build, the `q6_crosscheck` plan).** The cell
`live_claim_vs_snare_present` was glossed "cover-story — the highest-value case." "Cover-story" is an
*orientation* claim — a **deferred Ω_E**, not Ω_P (there is a fact of the concealer's stance; the
synchronic tier just cannot *reach* it), and the synchronic tier has **no** fact distinguishing a
cover story from a survival frame or a defensive concealment — the structural footprint is identical
across all three. So the gloss asserted an unfalsifiable-at-this-tier claim: the engine claiming to
witness a seat it cannot occupy (and, worse than mere absence, an orientation it could only have by
*declaring* it — the encloser's self-certification, see the Ω-typing above). It took
eight review passes to pull "orientation" back out of background into a live dial. A single
falsifier-per-verdict pass at pass five would have caught it: run "what would falsify this name?" on
"cover-story" and it fails immediately. **The defect that would have shipped was not the missing
orientation layer — that is genuinely hard and correctly deferred. It was the *concealment* of the
missing layer: a confident gloss where an OPEN belonged.** The conversation's yield was not a ninth
rewrite's worth of correctness; it was converting one *concealed* seat into a *declared* one
("orientation is not witnessed at this tier") — a thing pass five could have written down with no new
machinery, had the question been live.

**The two failure modes are symmetric, and pass-count is not the variable.** *Shallow concealment:*
"looks done" sets the verification threshold at the first green with no marker that a threshold was
set — the output ships as fact-decided when it was depth-decided at the shallowest setting. *Deep
concealment:* "I'm still verifying" becomes the story that defers a declaration you could already
make — depth as its own flinch (drafting another both-readings-safe pass instead of committing the
claim-plus-falsifier and shipping). Not every pass in a long run is load-bearing; some are the seat
declining to declare itself done. **The honest position is not "go deeper" — it is "declare the
stopping point."** One pass with the opens marked beats eight passes that ship a confident gloss over
the one thing the system cannot see. Triage test for a run of rewrites: did each pass catch a
*concealed-seat* defect (earning its cost) or merely tighten an already-correct claim (cost outran
catch)? If a conversation finally worked, it usually changed *what you were looking for* — from "is
this right" to "is this claiming more than it witnesses" — not how deep you went.

**Forward step (load-bearing, not yet built): make the check mechanical.** The falsifier-per-verdict
test is not a thing you do *to* a plan; it is a **lint the engine should run on itself** — every
emitted verdict name carries, or fails to carry, a tier-available falsifier, and the ones that cannot
get **auto-downgraded to OPEN**. That closes the conceal-an-open question for the whole verdict class
by making it mechanical, leaving the genuinely uncheckable part (orientation; the honoring) declared
and deferred instead of silently shipped. This is a declared absence (a capability the engine does
not yet have) → log it in `docs/design/design_gaps.md` and/or mint an OQ; do not treat the absence as
done because the principle is written here.

### Don't answer "does the apparatus pay for itself?" by producing more apparatus

A sibling of the verification-depth seat, on the meta level. When the question on the table is
whether the *audit arc / apparatus* is worth its weight, the failure mode is to resolve it by
producing more well-formed apparatus-output — a clean cruft test, an asymmetry rule, a new
`design_gaps.md` filing. That answers whether one *predicate or site* is worth keeping: a different,
lower question than whether the *arc caught a defect a user would have hit*. Producing more of the
thing being questioned, *well*, is the swap to watch for (witnessed OQ-112 Round 3, 2026-06-23:
asked "does the arc pay for itself," answered with a predicate-worth adjudication).

Why the two don't substitute: "unwired but worth keeping" and "premise falsified, commit dropped"
are both **latent** findings — value or hazard conditional on an input the system does not currently
produce (a future caller; an absent metric that doesn't occur on the live corpus). An arc that finds
mostly latent hazards plus unfinished value in a healthy codebase is reporting *good news*
("hardened before it went live") — but that is a **different claim** than "this arc catches live
defects," and the apparatus's weight is justified by the second, not the first.

The honest moves: (1) hold the two threads apart — settle the local decision, but **name** the
meta-question and leave it open rather than answering it with output; (2) run the **one-fix-bite
check** — pick one shipped fix and ask whether it changed a verdict a user actually saw, vs. closed a
latent-on-every-corpus branch; (3) treat "no live bite across the arc" as itself needing a positive
control — don't assert it (false-absence) or bury it; (4) **install the kill-question in substrate**
(the OQ entry) as the gate on the next round, so a future instance can't spin up Round N+1 without
facing it; (5) watch for **converging-to-approval** — repeatedly landing on "approve + two
sharpenings" is the affirm-bias, sharpening at the margin while never asking whether the margin is
where the value lives. The **read pass killing a write** before it touches the engine (a Round-0
recon falsifying a pre-registered commit) is the arc's real transferable product — escalate the
premise-falsification, don't build around it.

### Nobody reads a hammer's changelog — name the reader before you keep the record (operator, 2026-08-12)

This project mints artifacts fast, and the ones it mints *about its own work* are the least
examined, because producing them feels like diligence. The standing question — **is this history,
this audit, this new idea load-bearing?** — is cheapest to ask at creation time, and that is where
it goes.

> **Before creating any record — a changelog, an evidence base, a provenance note, an audit
> directory, an outcome file, a version history — name the reader and the decision it changes for
> them. If you cannot name both, you are producing cost that looks like diligence.**

**This is a rule about MINTING, not about deletion, and the asymmetry is deliberate.** *Unwired ≠
worthless* still governs removal: judge an existing thing by its product, never by whether anything
consumes it. Creating is cheap, continuous and unexamined; retiring is destructive and owes a diff.
So the correct posture is **reluctant to mint and reluctant to delete** — the pressure applies at
the moment of creation, not to the accumulated record.

**Records that earn their place** serve a *future decider* who consults them before acting, and the
repo's provenance discipline is exactly this and is not weakened by the rule:

- `KNOWN_STATE.md` entries that pass the promotion test — a cold reader makes a **silent** mistake
  without them.
- Audit directories: a claim's witness has to be re-checkable, or the claim was never discharged.
- `ISSUES.md` resolution notes: someone will re-open the question and needs to know what settled it.
- Amendment provenance in a **derivation** (`seat-theorem-v1.md`) — showing how a conclusion was
  reached is what lets a reader disagree at the right point. That is load-bearing for a proof and
  academic for a tool; the same content is not the same artifact in the two places.

**Records that earn nothing** have a subject validated by *use*:

- **A tool's changelog.** Whoever picks up `uke_referee.md`, a Prolog module, or a `python/` script
  has a problem now and will test it against that problem. They will not read how it got here, and
  no accumulated evidence base makes it likelier to work for them. **The proof of a tool is that it
  works when someone wants to use it** (operator, 2026-08-12).
- **A provenance file for an artifact whose proof is that it works.**
- **An outcome record built to justify apparatus** — apparatus about apparatus, the sibling rule
  above.

**The exception worth stating, because it looks like the thing being banned:** a tool's *version
notes* earn their place when they are **forward-facing and short** — what changed, and where the
tool is still thin — because that serves the person deciding whether to trust a section right now.
That is not a history. The test is tense: a note about what a reader should expect *from here* is
usable; a record of how we got here is not.

**The tell.** When the justification for keeping something is *"provenance matters"* or *"for the
record"* with no named reader and no decision attached, the phrase is doing the work a reason should
do. Ask who, and what changes for them. Frequently the honest answer is that the thing belongs in a
commit message, which is already the record and costs no attention.

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

**The dual — fail-closing on a NON-genuine absence (OQ-178, 2026-06-24).** Pattern 5 and the OQ-44
policy say *fail-closed on absence*. The symmetric failure is to apply that reflexively to an
absence that is not genuine: the datum IS authored, the **probe just landed off its grid**. Witnessed:
`cs_kernel_registry` classified each reading at a synthetic `Time=0` ("baseline comparison"), but
15 live constraints author `base_extractiveness` only as a temporal series at real years (1900,
1450, 480 BC…), none at `Time=0`. So `classify_at_time` hit the `BaseX=0.5` fabricated-default
branch and read "absent" — when the value existed on a different grid. The OQ-44 reflex (fail-close
to `unknown`) was *worse* than the fabrication: it **discarded the authored series and erased a real
`snare`-vs-`scaffold` kernel divergence**, reporting fully-robust agreement where two readings
genuinely disagree (`robust_context_count` 0→156). Neither impute-0.5 nor fail-closed-unknown was
right; the bug was the **off-grid probe**. **Rule: before fail-closing on absence, establish the
absence is GENUINE — would a probe ON the authored grid (the story's own time-points / scalar)
find the datum? If yes, fix the probe (read it on its grid), do not fail-close.** Diagnostic: for
the constraints the gate calls "absent," enumerate the source predicate's facts at *any* key, not
just the queried one; if they exist elsewhere, the absence is off-grid, not real. The deeper spine
is the same as Pattern 5 proper — *genuine* vs *apparent* absence collapse at the read site — but
the corrective is the opposite direction: Pattern 5 says don't let absence *pass*; its dual says
don't let off-grid-ness *fail-close real data away*. (Provenance: `audits/2026-06-24_oq41_basex_t0/`,
OQ-178; the off-grid family is OQ-105.)

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

**Dead-by-range — the complement, and the fail-closed fix's own failure mode (OQ-113,
2026-06-18).** Counting the source table catches *pass-on-absence*; it MISSES the dual case
where a gate can *never pass* because the value it compares against is outside its builder's
RANGE — dead even on a full table. Worked instance: OQ-43's fix (above) made
`has_viable_alternatives/2`'s default the fail-closed sentinel `unknown` so `== false` could no
longer pass by absence — but that very fix left the builder with range `{true, unknown}`, so
`natural_law_signature`'s `HasAlternatives == false` leg became permanently UNSATISFIABLE
(builder-unreachable), and with it the whole signature and the `pure_natural_law` purity subtype
went 0-firing by construction on every corpus (live 79 + twins 960+960 = 1,999 witnessed). The
fail-closed fix for one absence-gate manufactured a dead-by-range gate next door. **Diagnostic
upgrade: when auditing a gate, don't only count the source table's facts (`0 ⇒ no-op`); also
check the PRODUCER's range — enumerate the values the builder can actually emit. A comparison
against a value outside that range is dead even on a full table.** Tell them apart by the
positive control: an absence-gate fires once you author one row; a dead-by-range gate stays
0-firing no matter what you author (the constructed-true positive control fires, the corpus
sweep does not — exactly the OQ-113 witness pair). See OQ-113 (the close routes the residual
capability to GAP-08 §7) for the full treatment.

**Where it recurs:** any gate keyed on the sparse `intent_*` family or on an optional authored
field; any quantifier (`forall`, negation-as-failure) over a table that the current corpus
leaves empty; any comparison `== V` / `=< V` where the producing predicate's range excludes the
satisfying `V` (dead-by-range). See OQ-44 for the engine-wide audit.

**Instance — a `[fail]`-mode test is an absence-gate, and corpus-fixture-loading suites rot on
reset (witnessed 2026-07-02, OQ-137).** `tests/test_cs_drift_engine.pl` loaded its fixtures
from `testsets/` (the capital-punishment triplet); the 2026-06-05 corpus reset deleted those
files and the suite sat 7/8-red for a month unnoticed — plunit suites were wired into no gate,
so red was invisible (partially closed: the reading-totality suite is now a `run_pipeline`
gate; the rest still run only by hand). The sharper point: the one "passing" test was the
`[fail]`-mode negative (`no_unacknowledged_retributive`) — a test asserting "predicate does NOT
fire" is satisfied by TOTAL breakage exactly as by correct behavior. Rules: a test file asserts
its own fixtures (setup/cleanup) rather than loading corpus files — the corpus is mutable
substrate, not test vocabulary; and a `[fail]`-mode test needs a same-fixture positive sibling
(the predicate DOES fire on the firing fixture) or it is an absence-gate. Diagnosing a red
suite after an engine edit: run it against the PRE-edit engine first — identical failures =
pre-existing rot, not your regression (the discriminating witness that separated the fixture
rot from the same-day attractor-table change).

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
   **The dual, same altitude (2026-07-24):** *truncating to the HEAD* of a warning-heavy
   channel. `run_pipeline.run_prolog` reported failures as `result.stderr[:300]`; SWI emits
   load-time warnings for hundreds of lines before any ERROR, so the head-slice was
   structurally guaranteed to be noise on EVERY failure across all 12 Prolog steps. A
   trajectory-step crash surfaced as two "Local definition ... overrides weak import"
   warnings cut off mid-word — the real stderr was 259,426 chars / 2,311 lines, and the
   exception that ended the run never appeared. Filtering-out and slicing-the-head are the
   same defect: a fixed rule applied to a channel whose payload position is not fixed. Fixed
   by `salient_stderr()` (prefer ERROR lines, else the TAIL, never the head; commit
   `55c8b242`). **Rule: when a channel is noise-dominated by construction, select by
   PREDICATE (what makes a line diagnostic), never by position.**
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

**Sub-rule: evaluate a measure on the COMPARATOR ITSELF, at the actual cell sizes, before freezing
it (operator, 2026-08-10; OQ-78).** A separability, concentration, or overlap statistic is not a
pinned condition until it has been run on the known-positive and known-negative populations *at the
cell sizes the design will actually apply it to*. **Until then it is a name for a condition, not a
condition** — and because it will emit a number either way, a vacuous one reads exactly like a
working one.

The check is mechanical, costs one pre-freeze pass, and runs before any test datum is visible. It
caught two live defects in a single pass on OQ-78, each of which would have produced a
success-shaped close:

- **The measure scored its own pass-value on the comparator.** The pinned p10–p90 interval-overlap
  separability measure returned **1.0 on the archive comparator itself** (rope's p90 dragged to 0.68
  by three documented exceptions), so its bootstrap threshold calibrated to 1.0 and *every possible
  banding would have passed*. Its obvious repair, p25–p75, was vacuous in the opposite direction —
  **0.000 on all four non-test legs**, no variance to threshold. Only the third candidate
  (worst-pair AUC) varied across legs, which is what made it the only one carrying information.
- **The floor sat below the null median at the smallest admitted cell.** A concentration floor of
  0.10 against a uniform-digit null whose p50 is **0.300 at n=5** and 0.200 at n=10 does not
  measure localization at those cells — it fires on noise. Fixed by raising the minimum scored cell
  until the null p99 cleared the floor, and by setting the floor to *just admit the weakest true
  positive* (so the headroom is visible rather than assumed).

Diagnostic questions, in order: does the measure discriminate *between* my known populations (not
merely return a number)? Does the criterion exceed the null's upper tail at the **smallest** cell it
will be applied to? Is the floor's headroom over the weakest true positive stated, rather than
assumed comfortable? A "no" to any of them means the condition is unpinned. Related: *An introduced
instrument is itself a claim* — the repair measure inherits this discipline too, which is why the
AUC replacement was itself evaluated on all four non-test legs before it was pinned.

**The rule:** every diagnostic — grep, query, *or a reasoning claim of the form "X appears nowhere /
happens never / is unique"* — must be run against a **positive control**: a case you know in advance
it must flag. If it does not fire on the known-positive, its clean result on the real question is
worthless. This applies to reasoning about the code, not only to shell commands: an analyst
asserting "this is stated only once" is running an unfalsified diagnostic on the document, and "I
didn't find it" is not "it is not there" until the finder is shown to find. **And firing on the
known-positive is the floor, not the discharge** — a plant proves the instrument *can* fire; the
witness that its firing carries information is a case it *declined*. Grades of decline, the
record-not-per-run rule, and the role-reuse failure: *A positive control demonstrates
DISCRIMINATION, not detection*, below.

**Chains: every verdict-producing instrument gets its OWN control — screen-controlled ≠
rubric-controlled.** In a multi-stage audit (screen → content read; finder → judge), controlling
the upstream stage does nothing for a downstream stage that can independently return "0 found."
Spot-verifying a stage's *hits* checks true-positives only; it is powerless over a 0-flagged run,
which stays byte-identical to a read that never looked. Pre-flight each stage on a known-positive
BEFORE it touches the target population. Witnessed (2026-07-01, OQ-45 hidden-winner audit): the
engine screen was positive-controlled on kernel_v1's known false-mountains, but the content
rubric was not — its pre-flight then FAILED 0/3 on those same known cases (the rubric's
naturalness clause required story-voice assertion; the known cases carry naturalness only as an
in-frame reading). The amended rubric passed 3/3 and went on to find 6 real hits in the 404. An
uncontrolled rubric would have returned a plausible clean read over the same population.
Evidence: `audits/2026-07-01_oq45_oq52_hidden_winners/b3_rubric_and_strata_registration.md`
(v1 kept beside v2 — the amendment history is part of the control's witness).

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

**The inverse shape: a verification grep where the old token is a substring of the new one fires on
*correct* work and reads it as broken.** The section above is about clean nulls (a probe that didn't
fire reading as "nothing there"); the dual is a probe that fires on the wrong thing, reporting
success-shaped work as a failure. Witnessed OQ-16 (2026-06-25): the rename `drift_events →
metric_drift_events` was complete, but the "no dangling refs" check `grep 'drift_events\.pl'` matched
every *correctly-renamed* `metric_drift_events.pl` site (old ⊂ new) and read as "the rename failed" —
nearly sending the run to re-edit already-correct files. Whenever old ⊂ new (`drift_events ⊂
metric_drift_events`, `drift_report ⊂ metric_drift_report`), the survivor-grep **must** anchor:
`grep -nE '\bdrift_events\.pl'` (the `_` in `metric_drift` is a word char, so `\b` excludes the
prefixed form). The mirror failure — counting *new-name* hits as the witness — is equally blind: a
high count can't distinguish a real survivor from a renamed site. The discipline is the same as the
oracle rule: the probe must discriminate on the exact dimension it could be wrong on (here, the prefix
boundary), or its verdict (pass *or* fail) carries no information. Forward-relevant to any future
rename wave (the OQ-135 v8 seat/gauge/orientation vocabulary migration will hit this directly).

---

## A positive control demonstrates DISCRIMINATION, not detection (the grades of control; operator ruling, 2026-08-11)

**Planting the target shows the instrument can fire. Only a case it DECLINED shows that its firing
carries information.** This is the standing failure mode of the rule above — the demand for a
positive control is heard, and satisfied with a plant: construct the thing the probe must flag,
watch it flag, declare the instrument controlled. But an instrument that fires on everything also
fires on the plant. **A control with no decline available is one-sided and licenses nothing, however
well the plant worked** — the plant proves detection (the instrument *can* fire), and the claim being
made rests on discrimination (this firing *distinguishes*). The two are as different as a clean read
and a read that never looked, and they look identical in the transcript: both end in "positive
control fired."

**Grades of control, strongest first:**

1. **A case the instrument declined in its own history.** The instrument, unprompted and on real
   input, returned *no*. Strongest because the decline was not designed to be declinable — nobody
   built it to be rejectable, so it cannot be tuned to the instrument's known weaknesses.
2. **A naturally-arising negative drawn from the population.** A member of the actual corpus that
   should not fire, selected before the run. Weaker than (1) because you chose it, but it carries the
   population's real shape — including the near-misses an author would never think to write.
3. **An authored decoy.** Weakest, and specifically: **a decoy shows only that the instrument can
   reject authored decoys.** The author of the decoy already knows the discrimination the instrument
   claims to make, and writes to the near side of it — so the decoy tests the author's model of the
   boundary, not the boundary. Report a decoy-only control at that altitude; it is a floor, not a
   pass.

**The control attaches to the instrument's discrimination RECORD, not to each run.** Once an
instrument has declined — grade (1) or (2) — that decline is a standing property of it. The
obligation at each use is then **cite the record and show that this application is in distribution
for it** (same input shape, same population, same role), not re-plant a target every turn.
Re-planting per run is busywork that keeps re-proving detection while the discrimination question
goes unasked. Corollary, and the live part of the rule: **when the application drifts out of the
record's distribution, the record lapses** — a new population, a new input shape, or a changed
engine token (see *Instrument vocabulary rots*, below) puts a fresh decline back on the bill.

**If no decline is available anywhere in the population, the question is unanswerable from THIS
corpus** — that is the verdict, not "the control is merely absent, noted, proceeding." Absent-control
reads as a caveat and ships the finding; unanswerable-from-this-corpus is a *result*: declare it and
route it to a typed Ω (`docs/omega_variables.md`) or to `design_gaps.md`. **And if no positive
control could exist even in principle, that is a verdict on whether the category may be added at
all** — not a limitation to note and work around. A category nothing could fail to be admits every
member and routes nothing; it is the liveness test applied to the taxonomy
(`docs/design/design_discipline.md` §5 → *A category whose positive control cannot exist*).

**The silent failure: an instrument validated in one ROLE, then reused in another.** *The error
profile is a property of the role, not the instrument.* A matcher validated as a **detector** — where
false positives are conservative, because they widen a net a human then reads — becomes **silently
decisive** when reused as a **selection metric**, where those same false positives now *choose*, with
no downstream reader to catch them. Nothing about the matcher changed; its discrimination record was
earned under an error profile the new role does not have, and the reuse looks like thrift rather than
a new claim. **Rule: a reuse across roles is a NEW instrument and owes its own decline under the new
role's error profile** — ask, at every reuse, *which direction of error is cheap here?*, and if the
answer differs from the validation context, the record does not transfer. (The pipeline-local form of
this is *Chains: every verdict-producing instrument gets its OWN control*, above — screen-controlled ≠
rubric-controlled. Role-reuse is the same defect displaced in time instead of in the pipeline.)

**Why it belongs to the spine:** "the instrument fired" is a success-shaped token filling the absence
of "the instrument can tell these apart." Same shape as a fabricated `0.5`, a vacuous `forall`, and a
byte-identical stale diff — a presence standing where a missing thing should be visible.

---

## A consistency check is not a discrimination check (the tautological witness)

**A check that cannot fail witnesses nothing, and it looks exactly like a check that passed.**
This is the positive-control rule one turn earlier: the probe above at least *could* return the
wrong answer. Here the reported check is an identity — true by construction, whatever the
instrument did.

Witnessed 2026-08-10 (OQ-277, `audits/2026-08-10_oq277_rq2_crosscoding/frame/`). A sampling
frame was frozen and reported as **"174 dirs = 73 incident-bearing + 101 non-census, partition
exact."** The partition line reads as verification. It is not: the two strata were built by
`comm -23` against the same population, so the counts sum by construction and **no possible
miscount of the census could make that line fail.** Every figure downstream inherited an
unverified instrument while displaying a green check.

Building the actual control (six planted directories with known-correct classifications, each
asserted two-sided) took minutes and immediately found two live defects in the same command:

1. **The census is a positional parse of tool output.** `grep -rl … audits/ | cut -d/ -f2`
   extracts the *directory* only because the target carries an `audits/` prefix. Run from
   inside `audits/` with an unprefixed target, the same command extracts **filenames and
   subdirectory names** instead — the exact unit error that produced the `77/175` figure later
   corrected to `73/175`. The field index is never checked against the path shape.
2. **`grep` is a shell FUNCTION in the interactive harness shell**, not `/usr/bin/grep`, and
   the two disagree on emitting a `./` prefix — which shifts `cut`'s field by one. A figure's
   value therefore depended on *which shell ran it*. Pin the binary (`/usr/bin/grep`) in any
   script that computes a reported count.

**Rules.**
- Before reporting a check as a witness, ask **"what value of the underlying quantity would
  make this line fail?"** If no such value exists — sums that balance by construction, a
  partition from a set-difference, a total recomputed from its own parts, a round-trip through
  the code that produced it — it is a **consistency** check. Report it as arithmetic, never as
  verification, and build the discriminating control separately.
- **Never positionally parse another tool's output for a reported figure** without pinning the
  shape that makes the index correct. (This is the same mechanism a peer taxonomy independently
  names as its own class: positional parsing of a generator's output is a latent failure,
  because the generator's shape is a distribution, not a contract.)
- **A control whose verdict depends on ambient environment is not a control.** Pin binaries,
  not names.
- Corollary for the control itself: the first two versions of the control above *failed on
  their own fixtures* — one used a target shape that shifted the field back, one asserted an
  over-narrow property (`all names end in .md`, false for a nested hit yielding a subdirectory
  name). Both were left recorded in the file. A control that fails on a fixture you constructed
  is the control being wrong; fix it, and keep the failure visible so the next reader knows the
  fixtures are load-bearing rather than decorative.

**Related shape — the instrument inside its own population.** The same audit's sampling frame
initially included *the audit's own directory*: it was dated that day, so it entered the
population, and it landed in precisely the stratum the audit sampled from. Drawing it would
have asked a blind coder to classify the experiment classifying it. Whenever a probe samples a
population it is itself a member of, exclude by an explicit **pre-sample** rule and record the
exclusion **as a count** in the manifest, so the exclusion can never be silent.

---

## Existence questions are closed by adversarial coverage, not random samples

"Do ANY of the N have property X?" is a different question from "what fraction of the N have X?",
and a uniform random sample answers only the second — which may itself be a forbidden claim (a
bait-confounded corpus licenses per-story reads but no prevalence). Closing an existence question
on "k random members were clean" is the inverse flinch of hold-open-close: the sample cannot
discharge the question it was assigned, and the full read usually remains an available, affordable
kill condition. **The design:** pre-register what "most suspicious" means (the exact selection
recipe, written before any member is read — that preserves the anti-cherry-pick discipline), make
the worst candidates the primary stratum, keep a small seeded random stratum for calibration only.
"Even the worst candidates are clean" earns an existence-close; "25 random are clean" earns
route-not-close. Two riders, both witnessed (OQ-45, 2026-07-01): (1) the close is earned by the
adversarial stratum TOGETHER with a positive-controlled instrument (see the chained-instrument
rule above) — either alone is insufficient; (2) **a random-stratum hit is a bound-breaker, log it
as one**: it means the pre-registered suspiciousness criterion did NOT bound the phenomenon — a
fact worth a sentence precisely because the prevalence version of it cannot be claimed.

## Selecting a sample to make a value PRESENT severs it from the question (selection on the outcome variable)

The dual of the existence-question flinch above. When you build a sample or probe so a needed value is
*present* — to satisfy a variance gate, populate a cell, complete a 2×2 — you select on the outcome
variable, and the sample can no longer answer whether the value *occurs*, only that you put it there.
Witnessed four times in one instrument-build arc (OQ-153, 2026-07-24), all caught: an all-in-stratum
sample (a Kill-C 2×2 collapsed to a column); immutability-language selection for a `frozen` value (the
selecting signal *was* the confound being tested against); a "widen the canon stratum" proposal to
satisfy a variance gate (buys a number whose answer is already known); a shape-test pool too thin to
instantiate the shape (3/4 candidates failed — the null was about the pool, not the field). **Rules:**
- **Selecting for a value forfeits the right to read its presence as a finding.** Only its *absence
  under enrichment* carries information — `dead∧frozen = 0/8` in a sample *built* to contain both
  halves is a real empty; the items that DID show the value prove nothing, because they were selected
  for. Pre-register any selected-for value as **supply-only** and exclude it from every test that
  reads presence as evidence (the Kill-B-denominator move).
- **Instruction-following ≠ readable-from-substrate (contamination inside a fix).** If a rubric or
  instruction *names the exact item-class* the test is about, a pass shows the classifier can follow
  the instruction — not that the property is readable from the substrate. A fix's prediction only
  *generalizes* once an **un-enumerated** case (not named in the rule) also moves; the named cases are
  instruction-following. Any fix validated on the cases it names owes one un-named case.
- **Unsearchable ≠ empty; an argument ≠ a run** (extends *Every diagnostic needs a positive control*).
  A probe that cannot detect the thing — no authored signal to search on, no constructible positive
  control — returns **unsearchable**, not negative; recording it as "empty" tells a reviver a capable
  search ran and found nothing, so they will not repeat it (write "pending an authoring channel").
  And a reasoned conclusion ("the axes are orthogonal, so X does not reduce to Y") is an **authored
  conclusion**, not a measurement — label it so nobody later cites a probe that never ran.

## Instrument richness is gated on substrate instrumentation (the positive control, one level up)

The positive-control rule asks *did the probe fire?* — generalize it one level up to *is the axis
instrumented?* A richer instrument (a multi-axis join, a cross-perspective diff, any aggregate over
several channels) computed over substrate that does not populate all its channels is not a richer
measurement — it is the grid-absent vacuity of Patterns 5/6 wearing a more sophisticated outfit. The
extra dimensions do not add signal; they add noise that *looks* like signal, and the sophistication
of the instrument disguises the same hole. **Before running a join/aggregate over N axes, witness
each axis is non-vacuously instrumented on the test corpus** — the same way you witness a probe fires
before trusting its absence. An axis that no-ops (its source predicate empty) or reads thin
(fabricated-default-fed, low-confidence) is a channel of noise, and a diff over it measures the one
live axis with the dead ones blurring the result.

One thread (OQ-117, 2026-06-13) supplied this three times in three costumes, each caught by the same
instinct:
- the **matched fed arm** that did not exist — a divergence-rate read blocked because its comparator
  substrate was absent (caught by the probe-fires control on the name search);
- the **join-structure fed-vs-withheld diff** — proposed as "the proper test," then withdrawn because
  on the test corpus the committer/axiom axis no-opped (`cs_kernel_id` absent) and the temporal axis
  read thin (grid absent, OQ-93/OQ-33), leaving only the observer axis live: a join measured on ~1.5
  of 3 axes (filed → OQ-119, blocked on a three-axis-instrumented corpus);
- the **grid-absent temporal axis** *inside* that join — the same vacuity one level down.

The rule: name every axis the richer instrument spans, confirm each is populated non-vacuously on
*this* substrate, and if any is not, the instrument is **not cleanly computable here** — file it
behind its substrate gate (the corpus that instruments all axes), do not run a confounded version
now. "More dimensions" must never launder an underpowered measurement into a finding.

**The same vacuity recurs at the essay/read site — formalization-of-a-reading is not measurement
(2026-06-13).** A `constraint_reports/*.md` is a rich instrument too, and when its substrate is a
single regime-self-presentation testset (one translated press conference) the report still emits
confident scalars — ε, χ, purity, Boltzmann (non-)compliance, Wasserstein transport, theorems
T2–T6 — every one computed from the testset author's authored inputs, several of them on series the
engine itself labels `basis=projected` (*guesses, not observations*, OQ-102(a)). The report does
not hide this: it prints `grid authored 0/32`, `[INDEX VACUOUS] … ZERO per-index checks ran (not a
clean pass)`, and a structural verdict of `OPEN(no_gradient_data)`. **The hazard is the synthesizer
sliding past those flags** — reading the confident numbers as *evidence about the world the story
describes* when they are a well-structured *restatement of one analyst's reading of the source*.
The witnessed instance: the `captive_on_both_ends_v3` essay synthesized from seven China-legitimacy
reports, all `INDEX VACUOUS / OPEN(no_gradient_data) / basis=projected`. The contamination edges
that carry such an essay's coupling thesis are the load-bearing read: confirm each edge's
`Provenance` column says `authored` (the testset asserts the link) before leaning on it — a
`corpus-derived` edge is the corpus's topology, not this story's claim (OQ-103, the
`shared_beneficiary` xprize case). Note the provenance flags that make this checkable — the
`basis=projected` tail and the `Provenance | Salience` edge columns — **are the OQ-102 (closed
2026-06-11) and OQ-103 (resolved 2026-06-12) fixes working, not open gaps**: cite them as resolved
surfacing, never as live defects. The rule is the same one level up: a vacuous instrument that
prints confidently must inherit its own `OPEN` at the read site — the prose may not be more certain
than the verdict line. The under-discussed thing such reports *do* anatomize is the **structure of
the rhetoric** (how a legitimacy claim is built), never the **mechanism it describes** (how the
extraction works); the second sentence is not licensed by a propaganda transcript.

---

## A gate that CANNOT PASS is a false-positive gate — and it fabricates a finding (2026-08-12)

The orphaned-control rule covers a check that is **green and wired to nothing**. This is its exact
converse: a check that is **red and caused by nothing**. Both are why gate calibration is
*two-sided*, and the second is worse than it looks, because a check that cannot fail merely
witnesses nothing while a check that cannot pass **manufactures a positive**.

**Witnessed, and it was pre-registered.** OQ-289's driver carried a numeric HALT —
`cache_read_input_tokens == 0` per unit, "nonzero means isolation failed and the delivered count is
corrupt." It was principled, it had a stated rationale, and it was **unsatisfiable under the
transport the run actually uses**: the CLI caches the system prompt, so every one of smoke run 1's
six units returned `cache_read` of 3,289 / 4,479 with `input_tokens = 2`. Frozen as written it
would have **voided every rung of a fully valid sweep and been read as evidence of isolation
failure** — a fabricated finding, not merely a missing one, and one the freeze would have
protected from revision. A second instance landed in the same file the same day: an isolation
clause compared a dict against a differently-shaped dict, so it fired unconditionally; only its
CONVERSE control could see it, and it did not have one until it was added. **Two instances is a
pattern.**

**The rule.** Every gate owes **both** calibrations, and they are different questions:

| Direction | Question | Failure it catches |
|---|---|---|
| Can it fire? | plant the violation — does it fire? | a check that cannot fail (witnesses nothing) |
| **Can it pass?** | **run the clean case — does it stay quiet?** | **a check that cannot pass (fabricates a finding)** |

**And a replacement gate inherits the old one's status until it carries its own satisfiability
witness.** Swapping an unsatisfiable HALT for a better-reasoned one is not a fix; it is the same
move with a new name, unless the replacement is *witnessed passing on real data*.
`DELIVERED_UNSTABLE_ACROSS_K` replaced the assertion above and is admissible **only** because smoke
run 1 shows `delivered` identical across k=3 twice — 9,002 ×3 and 10,262 ×3, zero variance
(`python/audits/oq289_smoke_run1/`). State that witness next to the gate, in the gate.

**Where it bites hardest: pre-registration.** A freeze is designed to prevent post-hoc revision, so
an unsatisfiable gate inside one is *protected* from the correction it needs. **A numeric HALT owes
a satisfiability witness BEFORE the freeze, on the real transport** — which is a second, independent
argument for running the feasibility probe before freezing (operator ruling, 2026-08-12), beyond
the one about whether the test is runnable at all.

---

## An introduced instrument is itself a claim (the recursion, generalized past diagnostics)

The positive-control rule and its "one level up" sibling both say a probe's clean null is worthless
until the probe is shown to fire. Generalize once more — past *diagnostics* to **any instrument you
ADD to satisfy the witness discipline**: a positive control, a canary, a fallback branch, a
perturbation harness, a verification step. Each is a new claim, and each inherits the discipline it
was introduced to enforce. The confound you close at one level silently reopens at the level of the
tool you closed it with.

The OQ-69 ledger-drain plan (2026-06-20) supplied this across three review rounds; the operator
named the class twice — *"a positive control that doesn't control the thing it stands in for"* and
*"the plan's own rules turned on the instruments the revision introduced."* Every residue crack
after round 1 was an added instrument escaping its own check:

- **Same-sink control.** A perturbation positive control must move the *exact output the experiment
  reads*, not merely move *something*. The δ-load-bearing probe's control had to route through
  `derive_directionality`'s χ sink — a flip on any other sink proves "the harness perturbs path X"
  while δ-no-change is read on path Y, reopening the confound one level down. (Resolved: neg control
  `δ:=0.0` byte-identical, experiment `δ:=0.3` flips χ on its own sink ⇒ δ is **live-but-zeroed** —
  wired and load-bearing when set, inert at the shipped `0.0`/uniform default.) This is the
  oracle-must-differ-in-the-fallible-dimension rule, applied to the *experiment's* own output.
- **Two-sided harness control.** Before trusting a "no change ⇒ null" perturbation, witness that a
  *no-op* overlay leaves output byte-identical AND a *known-live* input flips it. Without the pair,
  "no change" cannot distinguish a real null from a dead harness (wrong arity, cache not cleared via
  `cache_registry:clear_all_caches/0`, predicate absent).
- **Canary the riskiest shape, with its dependencies present.** A grammar canary on the *easy*
  entry (a bare ISSUES.md OQ) leaves the *risky* shape (a `Deps`-bearing entry) first validated only
  in the batch — the late reject the canary existed to kill. And a canary whose entry forward-refs an
  unminted target false-rejects on a dangling edge unrelated to its own well-formedness: mint the
  referenced pair (the `gates`/`blocked_on` pair) together.
- **The control's own anchor must be substrate-confirmed.** "The closed OQ is gone from `omega
  menu` ⇒ the close took" holds only if the menu *excludes* resolved items — so the witness needs a
  known-resolved control absent from the *same* block, and that control's resolved status read from
  ISSUES.md, not taken from the triage's word.
- **A pre-write witness cannot require a write to observe.** "Run `omega check` to see whether a
  resolved parent dangles" needs the close to already exist — making the keep-open fallback a
  post-write revert, not a pre-write branch. Rule from the *source code's* dangling logic instead
  (`omega_resolver.py`'s authority set); the post-write run confirms, never makes, the ruling. Keeps
  the separated-passes stance: read-only deciding precedes the write.
- **A noise floor used for attribution is itself a claim — and it's directional.** When you subtract
  a same-condition noise floor (byte-identical repeats ⇒ empty mask) and attribute the surviving
  cross-condition diffs to *the condition* ("this changed because of the code, not noise"), the
  floor's emptiness is load-bearing in exactly one direction. For the *unchanged* set, an empty floor
  is pure good news (nothing was subtracted, so stability is raw). For the *changed* set, the
  attribution silently assumes the floor *could* have detected non-determinism if present — which an
  all-cold-repeat floor cannot witness on its own. Positive-control it: show the repeats are
  independent (fresh processes that recompute, not a shared cache) **and** that a deliberately
  different cache/order state (warm vs cold) still produces identical output. Reconcile against any
  standing non-determinism premise (the OQ-112 / "stale memo unless cleared" class): name *why* the
  floor is empty (the path doesn't touch those sources) rather than rationalizing an empty result
  after the fact. The diff tool seeing a planted difference proves the *diff*; it does not prove the
  *run harness* yields independent repeats — distinct instruments, control both. (OQ-20 baseline
  diff, `audits/2026-06-22_oq20_dr_baseline_diff/`.)
- **A comparator's control must use a data shape on which the two quantities CAN differ.** When a
  probe compares an endpoint reduction (`V_last − V_first`, or `Delta/Duration`) against a "faithful"
  full-series measure (`drl_composition:linear_slope/2`), the positive control must witness that the
  two *can* diverge — and on the dominant data shape they may not. **For ≤3 evenly-spaced points the
  least-squares slope equals the endpoint slope exactly** (the interior point cancels: slope =
  `(V₃ − V₁)/(t₃ − t₁)` either way). A control built from a symmetric 3-point spike therefore shows
  endpoint ≡ LSQ and the comparator reads dead/safe when it simply cannot discriminate on that shape —
  a positive control that doesn't control the thing it stands in for. Build the control with **≥4
  points or uneven spacing**, confirm it *exhibits* the divergence (ideally a sign flip), and only
  then trust a "0 divergence" census over real series. (Witnessed OQ-18, 2026-06-25: a symmetric
  spike control read endpoint≡LSQ and would have validated a faithful-velocity flip-probe that could
  not tell the two apart; replaced with an uneven-spacing control — endpoint `+0.0005` vs LSQ
  `−0.0035`, opposite sign — after which the corpus census, with the comparator proven live, was a
  trustworthy measured zero.)
- **A planted control needs an artifact boundary to doctor.** When an assertion layer reads *live
  computation*, "planted violation" has no injection point: fabricating a bad value after the read
  witnesses the comparator's arithmetic, not the probe. Design the pipeline so the assertion layer
  consumes an intermediate artifact, and plant the violation in a doctored COPY of that artifact
  run through the same assertion path — the control then fires on the thing audited, not on a
  post-read fabrication. (OQ-207 census design D8, `audits/2026-07-12_oq207_stakeholder_h1/`:
  swipl dumps `census_input_<leg>.json`, Python asserts over the artifacts only, the doctored copy
  is shown FLAGGED.) **And the artifact layer inherits Pattern 5:** the same census's first dump
  AUTHORED `n_excluded: 0` for non-mcc verdicts instead of measuring it, silently
  under-determining the OQ-217 movement prediction until the diff comparator caught it on real
  data (`movement_diff.v1_flagged.json`). Every field in a dumped artifact is measured-or-absent,
  never defaulted.

Two more from the same review, about plan *structure* rather than the instruments:
- **A fallback gated on a condition the design avoids is dead code.** The first close-vs-keep-open
  witness tested tolerance of a `Deps` edge the plan had already decided never to author — probing
  an impossible condition while the real risk (pre-existing inbound refs to the closing OQ) went
  dark. Probe the risk that can occur, not the one the design routes around.
- **A remedy must not recreate its own target at reduced scale.** The drain existed to dissolve "~14
  items share one buried `Priority`"; assigning priority *bands* without *distinct integers* would
  reproduce a smaller tie-pile the resolver still cannot sequence — the failure mode surviving its
  own fix. (Related: a triage verdict that sets a *scope floor* — a PARTIAL "what already shipped"
  claim — corrupts the new work-item's **output**, not just its ranking; re-witness PARTIAL
  boundaries against the file, while a STILL-LIVE misrank costs only a re-rule.)

**The rule:** when a fix, plan, or audit introduces an instrument to discharge the witness
discipline, treat that instrument as the next thing to witness — same-path, two-sided,
riskiest-shape, substrate-anchored, write-free-if-pre-write. And when review keeps finding cracks of
this one class with the count *dropping*, that is the floor: fix inline and ship — a further pass is
convergence-softening, not signal (the operator's `[EDGE]` call). Worked instance with all witnesses:
`audits/2026-06-20_oq69_ledger_drain/`.

**Instrument vocabulary rots when the engine's tokens change (2026-07-23, OQ-60).** A probe that
tags a condition by matching an engine token (census v1: mechanism-1 ⟺
`scope_invariance_test(C, variant([]))`) reports a VACUOUS zero the moment the fix retires that
token (`variant([])` → `no_data`) — Pattern 5 inside the instrument, doubly invisible because zero
was also the hoped-for post-fix answer. Rule: an instrument is versioned WITH the engine tokens it
reads (`census_oq60_v2.pl` shipped alongside the producer commit that retired v1's tags), and its
positive control must re-fire on the NEW engine before any re-census is trusted.

---

## A deterministic gate is buildable IFF the defect has a merit-independent signature (the counting↔theme mirror)

Before arming a mechanical gate against a defect, ask the only question that decides whether the
gate can exist: **is the defect's extractable surface merit-INDEPENDENT or merit-CORRELATED?** A
gate is buildable iff independent.

Two witnessed instances, learned one after the other, define the frontier:

- **Counting — merit-INDEPENDENT ⇒ the meter gates.** A digit means the same in a defect and in a
  masterpiece (`47` carries no craft), so `_numeric_inventory`'s false positives are rare and
  uncorrelated with quality. The gate can arm: measure density, one revision call, escalate. (OQ-215;
  `agent/uke_narrative_orchestrator.py`.)
- **Theme-naming — merit-CORRELATED ⇒ the meter cannot gate.** The extractable surface (anaphora,
  refrains, aphoristic closers) IS the surface earned prose uses on purpose — rift3's institutional
  creed, the empty-pan's refused ledger-math, McCarthy's whole body of work all light it up. So
  `_theme_inventory` collapses to a **high-recall candidate list under per-instance adjudication**;
  its auto-gate is deliberately narrow (two low-merit-correlation kinds) and, on calibration, sits
  above everything good and essentially never fires. (OQ-214; `audits/2026-07-13_oq214_theme_meter/`.)

**The tell that decides it (construct it before you arm anything):** run the extractor over the
*earned* exemplar and over a before/after fix. If flagging the earned case is a false positive, or if
defect and fix score the SAME on the would-be gating kinds, the signature is merit-correlated —
ship the candidate list, never the auto-reject. OQ-214's calibration showed exactly this: runs 1&2
had *identical* anaphora/causal counts across the improvement (the fix lived in the merit-correlated
refrain), and earned-dense rift3 outscored two of three defects.

**The trap this kills:** a merit-correlated meter that "looks successful" — defect-high, fix-low —
while it is actually issuing revision calls against earned craft. The **bucket rule** (a kind may
gate only if flagging it in the earned exemplar is NOT a false positive) is what turned OQ-214's
false success into a true null. Had the merit-correlated kind stayed on the gate, the calibration
would have validated a craft-suppressor.

The **design consequence** — that this boundary marks the mechanization floor and makes the assisted
posture permanent architecture — is `docs/design/design_discipline.md` §11b. The **repo-wide test**
of whether the partition generalizes beyond these two defects is **OQ-221**. Provisional, with the
falsifier stated: a future defect that *is* mechanically separable revives the meter approach for it.

---

## A model swap is an engine change: its witness is a full run through the structural gates

A witness class must match the failure class it guards. An API round-trip ("OK" came back, no
400) witnesses *reachability* — params accepted, auth works, the gate on sampling params holds.
The failure class of a **generator change** is different: format and register drift at stage
boundaries — output that parses, flows, and lands at the wrong address. Only a full run through
the structural gates can witness against that class.

Witnessed both ways in one day (2026-07-12/13, Sonnet 4.5 → Sonnet 5 migration): the swap was
verified with live round-trips on all three call paths and called clean; the first full
production run (`112_ergodocity_kids_1783916200`) had stage 2 fold SECTION 0: INVARIANT CONTRACT
into SECTION 1 as "Step 0" — content at the wrong address, extraction empty, R13 threading dead
behind a warning, run "completed." The round-trip witness could not have caught it in principle:
the failure lived in output *structure*, a class the witness never sampled. (The fix is the
OQ-216 template: the stage-2 SECTION-0 guard now fails loud; two-sided witness in the guard's
comment.)

**Rule: after any model change in a generator — pipeline stage models, SCOPE/architect roles,
batch paths — the migration is OPEN until one full run passes the structural gates on the new
model.** Round-trips are the pre-flight, not the witness. Corollary, same class: pin the OLD
model via per-stage overrides when a probe's comparability depends on the generator
(the OQ-218 Stage-2 batch pattern) — the executor's model and the generator's model are
independent choices.

## Shuffle-test / permutation-null discipline (the control's precision must match what it gates)

A permutation null ("real structure beats shuffled structure") is an absence claim about the
*shuffle* — worthless until the shuffle is shown to *destroy*. OQ-182's C-null (the HAC
family-meaning gate, `audits/2026-06-25_oq182_trajectory_revive/`) added four rules to the
positive-control discipline above, each a place a clean-looking null lies:

- **Match the control's precision to what it gates.** A single shuffle draw is the *same epistemic
  object* as one null draw — it can land high by luck. So do not gate a precise N-draw null with one
  fragile draw: read the stochastic control off *the distribution you are already computing* (the
  teeth-witness `null median < real` over all N draws), and print it *before* the percentile verdict
  to keep control-first. A deterministic control (identity → byte-equal partition) is correctly a
  single draw; a stochastic one never is.
- **The positive control must distinguish the *destroying* shuffle from a *toothless* one, and that
  distinction appears only under re-clustering.** The intended (per-component-independent) shuffle
  breaks cross-component co-occurrence; a *joint* shuffle merely relabels intact vectors and
  re-clusters to the same partition (silhouette unchanged) — a false PASS. Scoring the *real*
  partition under shuffled distances collapses for *both* shuffles, so it cannot tell them apart;
  only **re-clustering** each shuffle shows per-component-collapses-while-joint-holds. And the
  threshold quantile must be computed over the *destroying* draws, never the toothless ones, or the
  whole threshold inherits the toothless null one level up.
- **A correction that ADDS a code path is the least-witnessed surface — the control suite predates
  it.** The C-null harness's surgery-map correction introduced `make_groups` (re-derives the shift
  pre-grouping under σ); the end-to-end FIDELITY control, run at identity σ, is a *no-op on the
  regrouping* and cannot see a `make_groups` that mis-keys under permutation. The added path needs
  its own positive control at the point it must agree with what it replaced (`make_groups(identity)`
  == engine `group_by_shift`). This is *an introduced instrument is itself a claim*, specialized: the
  instrument is a corrected mechanism, and the existing suite was shaped for the old one.
- **A FAIL must name which gate failed — same word, opposite next-actions.** Under a conjunctive
  verdict (`real > P95` AND teeth-passed), a TEETH-fail (toothless null) means the *instrument is
  void* — redesign, do not defer; a percentile-only fail (real signal, sub-threshold) means
  *real-but-weak* — defer with the test named as the closer. A bare "FAIL" collapses "the test didn't
  run validly" into "the thing isn't there."

(Witnessed PASS, testsets/: real silhouette 0.161 vs null P95 −0.026, 0/200 draws reach real, +5.0σ;
controls all pasted before the verdict. `c_null_harness.pl` + `c_null_results.log`.)

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

## "Redundant / safe to remove" on a shared edge or field needs a per-consumer reachability witness

When a datum (an edge type, an authored field) is read by N consumers and you want to remove it,
guard it away, or call it redundant, the claim that decides safety is *per-consumer* — and the
shape of that claim is the same aggregate-vs-composition trap as the section above, one level up.
Four moves, none skippable:

1. **Consumer-count is not the witness.** "Redundant over the set" ≠ "redundant per consumer." The
   same datum is usually read as *different structure* by each consumer (one reads an `affects_constraint`
   edge as contamination, another as composite→component, another as ordered dependency), so a
   single "it's redundant" cannot hold across the set. Enumerate the consumers and decide each.
2. **Per consumer, ask: does its read reach a shipped product/verdict, or die internally?** This
   splits *ships-and-wrong* (fix it) from *inert-wrong* (log it, no engine change). A consumer with
   no live caller, or whose output is never serialized/reported, is inert — its wrong read is real
   but harmless, and "fixing" it is unwitnessable churn. Trace callers + output paths; do not assume.
3. **A substitute must carry what each consumer READS, not merely a shared label.** "Both edges mean
   *these are siblings*" is a claim about a *label*; the consumers read directed graph *structure*
   (embedding, dependency, contamination). A label-equivalent twin is not a structure-equivalent
   substitute. Witness that each consumer's computed output is reconstructible from the substitute —
   per consumer — before banking "no information lost."
4. **A fix at a SHARED site owes a zero-change control on the layers it must NOT touch** — the
   symmetric image of the positive control. The positive control proves your change *reached* the
   thing it targets; the zero-change control proves it *did not reach* the things it must leave
   alone. (Run the untouched consumer old-vs-new and confirm byte-identical.)

Instance (OQ-23, `audits/2026-06-29_oq23_coexists_fpn_canary/`): "the typed `cs_reading_relation`
makes the sibling `affects_constraint` edge redundant, so strip it" was **falsified per-consumer** —
under a reversible strip, 4 of 5 consumers' outputs changed, and only 2 reached a shipped product
(FPN purity → `pipeline_output.json`; coupling baseline → `coupling_protocol.md`); the other two
were inert (no live caller). That moved the fix from the shared neighbor-construction site to a
narrow contamination-local site (zero-change control: giant_comp connectivity 276/334 unchanged),
and deferred the one genuinely-open consumer question (giant_comp's 334→70 reinterpretation) to its
own ruling (OQ-193) rather than landing it as a side effect. The discipline that caught it: witness
per-consumer reachability **before** the removal, not a set-level "redundant."

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

**Corollary — `git commit -- <pathspec>` silently omits UNTRACKED files, at exit 0 (witnessed
2026-07-21).** A commit with explicit pathspecs commits only *tracked* modifications matching those
paths; **untracked new files under the same pathspec are NOT added** and the command still exits 0.
Instance: a `git add A B C` that hit a stale pathspec aborted the whole add, then
`git commit -- <dir>` committed the tracked rename + edit but silently dropped THREE untracked new
`.md` files — `git log --stat` showed 2 files, exit 0, and a turn-end recap would have read "done."
This is the Pattern-4/6 shape (a green exit conceals the absence). **The rule: after any commit
whose staging was non-trivial, witness the commit's CONTENTS (`git show --stat <sha>`), not its exit
code — and for a tracked+untracked mix, `git add` the paths explicitly first and confirm
`git status` is clean.** Exit 0 attests the command ran, never that it captured what you intended.

---

## A latent edit's witness is a RED→GREEN test, not a null diff (operator corrections, 2026-07-23)

A byte-identical pipeline diff for a supposedly-latent edit is consistent with two states: the
edit is latent, or the edit was never exercised (shadowed clause, off-path guard, dead branch).
The null diff cannot distinguish them — it is the success-shaped absence of the spine applied to
the *witness itself*. Ruling from the OQ-60 pass: **every producer edit is witnessed by a plunit
test that fails RED at pre-edit HEAD and goes GREEN with the fix, landing together**; the pipeline
diff then witnesses only the LATENCY claim (nothing else moved), never the edit. Retroactive:
already-landed commits whose only witness was a null diff owe a retro positive control (OQ-60's
0a/0a.2 were retro-witnessed by injecting a synthetic `unknown` through the full consumer chain).
Companions from the same review arc:

- **Not-RED is a HALT, not a lucky green.** A new mechanism test that already passes at pre-fix
  HEAD means the model of HEAD is wrong — never record it as a pass.
- **Screen vs witness.** A pre-registered aggregate prediction (exact per-leg delta-mean, with a
  float-tolerance window stated BEFORE the run so a ULP mismatch cannot HALT a correct fix)
  SCREENS; only the per-item join (flip set == predicted rows, nothing else) WITNESSES. Name both
  failure directions with different first suspects — over-flip ⇒ the census/premise is falsified;
  under-flip ⇒ a missed caller — so the join is self-interpreting.
- **Collapse latent commits when tests already attribute.** N zero-victim mechanisms can land as
  ONE commit when N RED→GREEN tests give per-mechanism attribution — the per-commit witness cycle
  bought nothing the tests don't.

Provenance: `audits/2026-07-17_oq60_purity_absence/` (PREFLIGHT / WITNESS_CLATENT /
WITNESS_CFLOOR _2026-07-23.md).

## Latent-now is not safe-later: a new token obligates a consumer sweep at INTRODUCTION (2026-07-24)

The section above governs *whether* an edit is latent. This one governs what latency licenses —
because a correct latency witness has an expiry date nobody schedules.

Shape: a change introduces a NEW value into an existing domain (a token, sentinel, enum member,
null), guards it at the introduction site, and records — accurately — that the path is *"inert
until a producer emits it"* and the diff *byte-identical*. Both claims are true at commit time.
Later, in an unrelated commit, a producer lands. Every pre-existing consumer that was written when
the domain was smaller now receives a value it was never designed for, and the byte-identical
witness on the introducing commit is exactly what made the omission feel complete.

Witnessed (OQ-60 → OQ-242/243): `purity_scoring.pl:49-55` added `Score = unknown` with the
comment *"propagate `unknown` rather than feeding it to the weighted sum (which would throw)"* and
the note *"inert until a producer emits `unknown` (Commit 0a is byte-identical)."* A producer
landed; `context_profile_mining.pl:434` — one level below the site that was hardened — then did
precisely the throw that comment predicted, killing the trajectory step
(`ERROR: =:=/2: Arithmetic: 'unknown/0' is not a function`, commit `ab748fc6`). The introducing
commit anticipated the failure mode in prose and still shipped without the sweep.

**Rule: introducing a value into a domain is a domain-widening change, and its unit of work is the
CONSUMER SET, not the producer.** Enumerate the consumers in the same change and record the
disposition of each (guarded / defended-downstream / unreachable). "Inert for now" is a schedule,
not a disposition — if the sweep is deferred, the deferral is an OQ with the activation condition
named, not a code comment.

Two things that make the sweep miss, both live here:

- **The loud shape bounds the search; the silent shape does not.** In Prolog, arithmetic and
  comparison (`is`, `=:=`, `<`, `>=`) THROW on an atom, so those consumers announce themselves —
  a green pipeline genuinely rules them out *for the paths the corpus reaches*. But `\=`, `==`,
  and pattern-matching do not throw, so a filter written `P \= -1.0` silently ADMITS the new
  token into a numeric path. A green run is evidence about the loud shape only; it says nothing
  about the silent one. (Cleared instance: `json_report.pl:1347/1349` has exactly that bare
  filter where its twin at `:1282` guards with `number/1` — defended only by
  `write_json_number/2:2549`'s explicit `unknown → null` clause at the emit boundary. Consumers
  with no serialization boundary downstream have no such backstop.)
- **Config-gated and optional-path consumers are invisible to a default-config sweep.** The
  consumer that crashed sits behind `config:param(trajectory_enabled, 1)` (checked at
  `run_pipeline.py:544`, which returns early and writes an EMPTY report when disabled). A
  consumer-chain injection driven by a default pipeline run reaches only the enabled path — so
  "injected the token through the full consumer chain" means *the chain this config selects*.
  Enumerate consumers from the CALL GRAPH (`grep` the producer predicate), not from a run.

Provenance: KNOWN_STATE 2026-07-24; commits `ab748fc6` / `55c8b242`; the two declared residuals
are OQ-242 (the absence-semantics ruling) and OQ-243 (the unswept ~50 call sites).

## Extension-touching diffs decompose into direct targets vs ensemble refit (or they read as walls)

**Instance (2026-06-12, OQ-109 B3 unanimity guard):** a guard change that moved THREE
stories' signature membership produced a pipeline diff touching **60/62** entries. The 57
non-target entries changed only in corpus-relative statistics — MaxEnt distributions,
Wasserstein profiles, Arakelov heights, signature_pressure are all fit against corpus-wide
composition, so any change to the type/signature ensemble refits every story's derived
statistics. Read naively, the diff is either "the change broke everything" (false) or
"60-story diffs are normal, approve" (worse). Both misreadings were live risks for the B4
gauntlet and the Phase C regen diffs.

**Practice rule: any old-vs-new diff for an extension-touching change (signature membership,
classification, corpus composition) is read in TWO layers, never as a flat count:**

1. **Direct targets** — entries whose change the edit predicts (signature, classification,
   pool membership). Each is justified individually against the ruling that licensed it.
2. **Ensemble refit** — entries changed ONLY in corpus-relative statistics. Verify the
   change-set is confined to those fields (field-level census of the diff, target vs
   non-target — paste it), and name any statistic-driven consumer flips separately
   (top-type flips, verdict/headline changes on non-target stories are REAL output changes
   a reader must see, e.g. a verdict_join yellow→red caused purely by the refit).

**Standard companion: the determinism control.** Re-run the pipeline at the same code and
diff against itself — byte-identical separates a deterministic ensemble cascade from
order-dependency noise (OQ-112 class). Without it, "ensemble refit" is an assumption with
the same shape as the noise it would excuse.

Witnessed pair: `audits/2026-06-11_oq109_phase_b/b3_unanimity_pattern3_diff.out` (the
decomposed 60/62 wall) and `b3_unanimity_dispatch_diff.out` (the byte-identical revert that
confirmed the decomposition's accounting was complete).

**Second instance + two refinements (2026-07-23, OQ-60 C-FLOOR):** removing 93 fabricated
Boltzmann floors diffed 181/199 and 960/960 rows on the two diffed legs — off-target changes
confined to maxent_*/wasserstein_*/arakelov/signature_pressure/contamination_network.
(1) **Attribute at the consumer, not by pattern-match**: the attribution is the pasted consumer
line (`maxent_classifier.pl:140-141` — profiles "computed from actual corpus statistics"), not
"probably the ensemble." (2) The strongest containment statement is the **headline-key count:
`classifications` changed on ZERO rows** — then the statistic-driven consumer flips (12 shadow
`maxent_top_type`, 9 alert-driven `verdict_join` red→yellow) are named separately per rule 2.
`audits/2026-07-17_oq60_purity_absence/WITNESS_CFLOOR_2026-07-23.md`.

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

## The relocating confound: a discriminating design is finished when the confound has nowhere left to land

A falsifier, gate, or discriminating probe is not done when a control is attached — a control
closes exactly ONE channel, and the confound relocates rather than dies. The OQ-232 review arc
witnessed three relocations in a single design (2026-07-23): the Axiom 2 falsifier's
adaptive-preference confound moved from the entrapment channel (the naive report-based kill) to
the position channel (under the single perturbation control — the "fixed" falsifier was
disjunctive over axiom-false ∨ P-indexed adaptation) to the cancellation neighborhood (under the
factorial — sign-opposed hypotheses cancel, so "flat" is both the axiom-false signature and the
both-true-and-cancelling signature). The design converged only when the last landing was
DECLARED as a scoped residue (the cancellation band, carried in the shipped falsifier text)
rather than fixed. Checklist before any kill condition or discriminating gate ships:

- **Ask where the confound lands NOW.** Iterate until it lands nowhere or the landing is a
  declared, scoped residue — never an unexamined corner.
- **Sign-opposed hypotheses cancel.** Assert over the swept magnitude interval, not a point —
  and expect a bounded instrument to widen the failing band: floors/ceilings absorb inversions,
  so an outcome row can be empirically EMPTY at realistic resolution (check reachability with a
  positive control; don't assume the outcome space).
- **Check feasibility at the position the claim NAMES**, not in aggregate: OQ-232's magnitude
  criterion was feasible only at mid-slope positions and sub-resolution at the powerless
  position the kill condition is stated over — "feasible somewhere" concealed "infeasible
  exactly where asked to work."
- **An arbitrary factor that CREATES discriminating power has inverted semantics** (at 1× the
  criterion decided nothing; the 3× multiplier was what cleared resolution) — derive thresholds
  from the declared instrument spec (N scale steps, implied multiplier reported per position),
  so the arbitrariness lives in a visible instrument choice.
- **Scope capability labels to the regimes the probe reached** ("sign-discriminating" was really
  positive-vs-flat at realistic instruments) — and an unreachable outcome row converts an arm's
  OPEN-instantiation from "none identified" into a SPEC (the measure that makes the row
  reachable).
- **A conjunctive kill with an unrunnable arm is worse for practice than scoped falsifiers** —
  ship each arm as a falsifier of the explicit conjunction it can actually reach, and state the
  full kill as the limit case.
- **Exiting the corrupted channel is a distinct repair strategy** (structural witnesses — facts
  formation cannot erase — the Axiom 7 fix), the only one that exits the confound class rather
  than managing it; ask whether the other instances admit one.

Instance, probe, and per-position witness: `audits/2026-07-23_oq232_falsifier_redesign/`; class
tracker OQ-234. Sibling law: *A deterministic gate is buildable IFF the defect has a
merit-independent signature* (above) governs whether the gate can exist at all; this section
governs when its kill condition actually discriminates.

---

## A falsifier must be FIREABLE — and a repair that encodes the claim into the instrument owes a standing probe

Two failure modes upstream and downstream of the relocating confound, both witnessed in one
arc (2026-07-25, OQ-253/OQ-255):

**1. The unfireable falsifier.** Before a pre-registered kill condition counts as a falsifier,
ask: *what observation would satisfy it?* If the condition is contradictory under its own
definitions, no observation can, and what shipped is hedging wearing a falsifier's costume —
the pre-registration RITUAL performed while the epistemic content is zero, which is worse than
no falsifier because it reads as rigor. Witnessed: "exhibit two questions with different
foreclosure-sets and identical seat-cost" was pre-registered while seat-cost was *defined as*
the foreclosure-set — "different sets, identical cost" was a contradiction, not an experiment
(caught by the author on re-read, one turn later; the fireability check costs a sentence at
pre-registration and was skipped). The repair that makes it fireable is structural, not verbal:
the definition had to become a *function* from foreclosure-sets to a coarser ordering
(gate + grade), at which point "both pass the gate, grades differ, difference changes the
selection" is satisfiable-in-principle and was in fact run. Rule: a kill condition ships with
its fireability shown — name a possible world that satisfies it. If you cannot, the definition
under test is doing the falsifier's job by fiat.

**2. The instrument that encodes the claim (falsifiability relocated to a standing probe).** A
repair may fix the instrument by building the tested claim INTO it — after which the instrument
can no longer vote against the theory, and every future pass through the gate is confirmation
by construction. Witnessed: gate v1.1 admits only carriers that can terminate on cost-present
cases, which *is* the theorem doc's claim that cost-finding questions cannot do the guard's
job; the gate now presupposes what the discriminator tested. This is not automatically wrong —
sometimes the claim has earned its place in the instrument — but it converts the one probe that
checked it into the theory's ONLY remaining falsification channel. Rule: at the moment of
adoption (not in the audit trail), record that probe as a **standing probe** — scope attached
(what tokens, what corpus, what n), re-fire obligation named (new corpus / new question-class),
and the failure semantics pre-committed (the gate weakens back toward its prior form; it is not
quietly re-scoped to exclude the counterexample). Instance: seat-theorem v2.5 §6.2 carries
exactly this block inline; the operator's rider that forced it ("if the instrument can't vote
against the theory, the theory stops being falsifiable through it") is the general form of v8
§5.8 one level down. Sibling: *An introduced instrument is itself a claim* (above) governs the
instrument's own verification; this entry governs what the instrument's adoption does to the
THEORY's falsifiability.

---

## A judged-tier grammar is an instrument, and inherits the instrument discipline

Judged/blind verdict tiers are now a recurring audit shape (OQ-258, OQ-259, OQ-264,
OQ-262 all carried one), and their failure modes are Pattern-6-silent: a one-sided
control gate passes a grammar that fires on everything, and the output still reads as a
finding. Five riders, each closing a channel by which a frozen-looking pre-registration
silently pre-decides its outcome (first full application:
`audits/2026-08-09_oq262_coexists_severance/PREREGISTRATION.md` — use it as the template):

1. **Two-sided calibration.** An expected-POSITIVE control alone ("the grammar finds the
   defect on known-bad cases") passes a grammar that finds the defect in *everything*.
   Pre-register at least one expected-NEGATIVE case, chosen before verdicts exist; if no
   credible candidate exists, declare that verdict class **UNCALIBRATED** rather than
   letting it fire uncontrolled. (Witnessed both ways in one audit: OQ-262's
   expected-`genuine` control FAILED against its pre-registration, and the `genuine`
   class stayed scoped as uncalibrated instead of being quietly trusted.)
2. **Declare grammar-post-recon — don't launder.** The freeze fixes the criterion before
   *verdicts*, not before *substrate knowledge*. If the grammar was authored after
   reading the per-item inventory, the prereg says so; controls whose named fire/no-fire
   targets were specified against items already known to satisfy them test
   IMPLEMENTATION, not discrimination — the prereg says that too.
3. **Pre-commit the interpretive-downgrade branch** before any verdict table exists: if
   the in-file-witnessed (RULED) rows come back a minority, the deliverable's altitude
   drops to "a reading of the text under a frozen grammar," not an audit result.
   Writing the branch after seeing the table is the laundering it exists to prevent.
   (Witnessed: the OQ-262 branch FIRED at 3/13 RULED and the altitude dropped honestly.)
4. **No-rate rule vs unanimity gate — state the distinction.** "No shares/percentages
   over the deliverable table" and "k=N unanimity control gate" coexist legitimately
   only when the prereg and writeup explicitly distinguish them; otherwise the writeup
   reads as violating its own rule.
5. **Blinding leaks through the instrument.** The grammar text handed to a blind judge
   is itself a channel: before launch, read the LITERAL prompt the judge receives and
   confirm no control-item name, expected direction, or verdict-class distribution
   survives in it — and paste that check.

Corollary (gate-spec class): **a pinned control clause must quantify over exactly its
recon basis.** OQ-262's "M2 fires nowhere on kernel_test" control FAILED AS WRITTEN
because the clause swept the whole archive while its stated basis covered two families —
the frozen prereg was not amended; the fail stood on the record with the correctly-scoped
claim witnessed separately. The honest handling of a mis-scoped frozen control is
record-fail + cause-classify + scoped re-witness, never a quiet rewrite.

---

## Pooled-across-story H¹ inherits story-level typing (the pooled read that measures its own construction)

**The trap (witnessed 2026-08-08, OQ-261 C3 + post-hoc,
`audits/2026-08-07_oq261_forced_gluing/`):** seat types are story-derived and seats have
no cross-story identity (GAP-31), so a type vector POOLED across the stories of a kernel
family inherits each story's typing wholesale. Any pooled sub-vector then reduces to the
family's story-level type structure: in the fiat family, performance seats,
topic-community seats, and all agent seats each carried H¹ = (#rope)·(#scaffold) exactly
(densities 0.4945/0.60/0.4952) — the partition contributed nothing; the read measured
the family's two blocs through every window. At corpus scale the same shape read 15/16
`real_closure` families "obstructed" on the pooled agent-seat vector. The number is
arithmetically real and diagnostically empty about the seat SET chosen — a
success-shaped output whose information content is the pooling convention (Pattern-6
adjacent; the OQ-264 standard-#2 shape lifted to the H¹ layer).

**Rule:** before citing a pooled-across-story H¹ (or any pooled-across-story aggregate
over story-derived values) as evidence about the pooled SET, run the symmetric read —
the complement set and the everything set — and compare densities; comparable density =
the pooled read is a restatement of story-level structure, and the claim must demote to
that altitude. **Candidate standard (operator-proposed 2026-08-08, adoption pending —
ISSUES OQ-264 candidate #7):** no pooled-across-story H¹ claim without an
identity-controlled comparator. Single-story reads (the control's own 7-seat vector) and
identity-bearing substrates (GAP-31's discharge condition) are exempt — the trap is
specifically pooling across stories that cannot share a seat.

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

**Prose-right ≠ artifact-right (recap-as-witness, sharpened).** A correct *understanding* stated in
the recap is not evidence the *artifact* matches it — you can grasp the right form and still write the
weaker one into the file by habit. Witnessed 2026-06-16 (v8 spec review): the recap said "taint
property," the prose *reasoning* was correct, yet the committed §3 operationalized a **count**; a
later pass said "transitive," and the committed text was still anchored **per-bridge**. Both times the
divergence was real and only the *pasted committed text* exposed it — the reviewer was right to refuse
to certify against the summary and demand the file. The discipline: **verify the artifact, not your
understanding of it.** When a load-bearing claim is "the doc/code now says X," the witness is the
quoted committed lines, never the recap of them — even, especially, when the recap is correct, because
a correct recap is exactly what makes the unverified artifact feel safe to ship.

---

## Write the receiver's prompt: a handoff is a specification test (stated-versus-instructed)

**The construction (operator ruling, 2026-08-11 — minted from OQ-277, where it fired at least three
times in one session).** Before declaring a design, plan, pre-registration, or ruling *done*, write
the prompt the next instance would need in order to execute it. **The act of writing that prompt
surfaces underspecification that re-reading the design does not.** This is a control, not a
courtesy — and it belongs to the same family as *Every diagnostic needs a positive control* and *An
introduced instrument is itself a claim*: it verifies the thing you verify with.

**Why it works — the mechanism, which is the transferable part.** Re-reading a rule exercises
*recognition*: you check that the text still says what you remember, and a rule that is correct in
prose passes every time. Writing an instruction exercises *enumeration*: you must produce the
operational form — every input the receiver needs, every artifact they must produce, every decision
they would otherwise have to make for you. Gaps live exactly where the design named a decision but
never named its operational half, and those gaps are invisible to recognition and unavoidable under
enumeration. The operator relates it to the arc's *stated-versus-counted* table (`§L` of the OQ-277
amendment ledger — a rule correct in prose and wrong once you count what it produces); this is the
same shape one layer up, **stated-versus-instructed**: the defect appears when you write what the
rule *makes someone do* rather than read what it *says*.

**Witnessed instances, one session (OQ-277 escape stratum, 2026-08-11):**
- Writing the assembler's prompt surfaced that a pre-registration had pinned *what is judged* and
  never *what is shown* — leaving the item-presentation decision (whether the judge sees the
  extractor's own reasoning, and the stratification key) to the assembler, where it would have
  changed the result. Fixed as prereg Amendment 4, before assembly.
- Converting a prose ruling ("two candidates and two primaries") into an executable draw procedure
  surfaced that at n=2 an unstratified draw could return two items of the same kind, making
  placement indistinguishable from the threshold under test. Fixed by stratifying before the draw.
- Running the handoff's own stated self-check command surfaced that it could not consume the file
  format the same handoff specified. (This one is the adjacent rung — *stated-versus-executed* —
  and it is why the two are worth naming together: reading caught neither.)

**The operative clause, and the way this fails.** The prompt must **enumerate the receiver's
actions concretely**. A prompt that says *"read the design document and execute it"* performs no
enumeration and catches nothing — it is a pointer wearing an instruction's clothes, and writing one
will feel like discharging this rule while discharging none of it. The check is: *could a receiver
who has read only my prompt take a wrong-but-reasonable action that the design intends to
forbid?* Every yes is a gap in the design, not in the prompt.

**Scope and cost.** Cheap by construction — the prompt is a thing you were going to write anyway;
the discipline is only that you write it **before** calling the design done, and treat what it
surfaces as a design defect rather than a prompt defect. It does not work on a receiver who is
yourself (no enumeration pressure — you supply the missing halves silently), which is the reason
it is a *handoff* rule and not a general review rule. And it finds underspecification, not
wrongness: a design that is fully specified and wrong will produce a clean prompt.

**Corollary — the terminal is a channel.** When the receiver's output is visible to a party the
design blinds (the common case: an assembler working in the judge's terminal), the prompt must say
what may be *printed*, not only what may be *done*. A packet assembled correctly and echoed to the
screen is unblinded before the pass begins, and no downstream control recovers it.

---

## When a defect is found, its before-commit is a free NEGATIVE control

**Operator ruling, 2026-08-11.** Every control in this repository was validated on **planted
fixtures**, and under *a positive control demonstrates DISCRIMINATION, not detection* that licenses
only the weakest claim: *authored drift gets rejected*. There is a stronger grade sitting unused in
git.

> **A defect's commit is a naturally-arising POSITIVE case; the commit before it is a naturally-
> arising NEGATIVE case. Neither was authored to be found. When you build a detector in response
> to a defect, grade it against that pair before you grade it against a fixture.**

**The instance that revealed it.** `orphaned_controls()` was built after two assertions were found
unwired. Run against the driver at `4e0d8725` (after the relaxation orphaned them) it names exactly
those two; at `cb1b33e5` (before, both wired) it returns `[]`. A real defect and a real clean state,
neither constructed. **Nothing else in that arc reaches this grade**, and it cost nothing — the
defect had already happened and git held both states.

**Availability is NOT automatic, and the count must be checked rather than assumed.** Of five
defects in the OQ-277 arc with this shape, **three have usable pairs and two do not** — verified,
not estimated:

| defect | before-state in history? | witness |
|---|---|---|
| control orphaning | **yes** — `cb1b33e5` / `4e0d8725` | detector declines then fires |
| capture path absent | **yes** — `cb1b33e5~1` has no `write_response` | `grep -c "def write_response"` → 0 |
| lexicon single-object `KeyError` | **yes** — `3e16a1d8~1` holds the defective normaliser | `grep -c` → 1 |
| drift-list false positives | **no** | `git log -S` finds 0 commits containing the defective test |
| baseline logging crashes as leaks | **no** | a reasoning artifact in scratch output, never repo code |

**The tension worth naming, because it cuts against the rule.** The two unavailable pairs are
unavailable *because the defect was caught before commit* — which is better engineering, and which
**destroys the free control**. Catching earlier is still right; the cost is simply real and should
be recognised rather than discovered. Two partial mitigations: **commit-as-you-go** shortens the
window in which a defect can live and die uncommitted, and a defect caught pre-commit can have its
defective state **preserved deliberately** (a scratch copy, or the diff quoted in the commit
message) when a detector is going to be built in response.

**Standing practice.** A detector built in response to a defect is not done when its fixture
passes. Locate the defect's commit and its parent, run the detector against both, and record the
pair — *fires at N, declines at N−1* — as its discrimination record. If no pair exists, say so, and
the detector ships at fixture grade with that stated.

---

## A control must witness that it is CALLED, not only that it works

**Operator ruling, 2026-08-11.** The same lesson as *gate the output, not only the input*, one
level up: **you verified the instrument and not its consumption.**

**The instance.** Two assertions in the OQ-277 driver — `assert_live_capture_dir_untouched` and
`assert_live_response_dir_untouched` — kept four green selftest lines after `run()` had stopped
calling them. Every prior vacuous check in that arc was a check that **could not fail**, **could
not pass**, or **could not be read**. These were none of those. **The code was correct, the
assertions would have fired, the selftests were real.** They simply were not wired to anything:
four green lines proving a guarantee no production path requested.

**Why this is worse than a red light, in one specific respect.** A red light recruits attention.
Green lines from a disconnected control are **indistinguishable** from green lines from a
connected one — and they *add to the control count*, so the apparatus looks **stronger** for
containing them. **Control count can rise while coverage falls**, and nothing in the stack
measures the difference.

**How they were orphaned — the mechanism, which is the transferable part.** A *repair* did it.
When the capture-dir invariant was relaxed from emptiness to provenance keying, `run()` switched
to the new assertions and nobody deleted the old ones, **because removal was not part of the
fix**. This is produced-but-not-consumed arriving in the *control layer*, via the same asymmetry
that drives memory accretion: **minting has a constituency and retirement does not.** Every
replacement is an orphaning event unless something forces the retirement.

> **The rule.** The selftest exercises the function; **something has to exercise the wiring.**
> A guarded function called only from the selftest is an orphan, and orphans are removed or
> re-wired — never left green.

**The forcing mechanism, since a rule with no instrument is the thing this arc keeps finding.**
`oq277_crosscoding_driver.py:orphaned_controls()` parses its own module AST and returns every
guarded function (`gate_*`, `assert_*`, the capture and provenance helpers) whose only caller is
the selftest. It runs as a selftest control, two-sided: a planted selftest-only function IS
detected, and the same function called from `run()` is NOT flagged.

**Its discrimination record is the strongest grade available**, because it is drawn from the
instrument's own history rather than from a plant:

| driver at commit | state of the world | detector says |
|---|---|---|
| `cb1b33e5` — before the relaxation | both assertions wired into `run()` | `[]` — **declines** |
| `4e0d8725` — after the relaxation, before the removal | both orphaned, four green controls | `['assert_live_capture_dir_untouched', 'assert_live_response_dir_untouched']` — **fires**, naming exactly the two |

A naturally-arising positive *and* a naturally-arising negative, on real historical material, with
no plant involved. It fires at exactly the commit that created the defect and declines at the one
before.

**One exemption, stated rather than silently taken:** `orphaned_controls` does not guard itself.
It is a selftest instrument by design, and its wiring witness is that `--selftest` fails without
it. An exemption nobody writes down is how the genre-based pin rule happened
(`audits/2026-08-10_oq277_rq2_crosscoding/SPEC_next_preregistration.md` §1).

---

## Gate the output, not only the input

**Operator ruling, 2026-08-11, priced at 219 model calls.**

> **A pipeline verified end-to-end on what it CONSUMES can produce nothing and report green
> on every check.**

**The instance.** The OQ-277 cross-coding driver had three gates, and they were good ones:
count captured payloads against expected calls *before* grepping; keep planted fixtures in their
own subdirectory so they cannot inflate the count; only then sweep for leaks. Every one is an
**input** gate. Nothing counted responses — and the driver had no code path that wrote them. A live
run made 219 calls, passed every gate, printed its expected totals, and persisted **nothing**. The
answers were computed in memory, aggregated into four printed lines, and discarded at process exit.
The whole run was unrecoverable.

**Why no check caught it.** Each gate was individually sound and the composition had a hole where
none of them looked. This is Pattern 1 (produced-but-not-consumed) one layer up — a producer whose
output has no *destination at all* — presenting as Pattern 6, because the aggregate that reported
success was computed from data that was never retained.

**The exact shape is worth its own line, because it is not quite any of the six.** The driver's
`--dry-run` flag advertised *"do not write responses/"* and printed *"responses NOT written.
responses/ left empty."* Both statements are **true**, and both describe a distinction the code does
not implement — there was no writer in either mode. The flag was **documentation of an intended
architecture wearing a switch's clothes**. Absence presenting as presence, where the
presence-token is a correct sentence. A reader checking whether responses are persisted would find
two pieces of evidence saying the question had been considered, and no persistence.

**The rule, in four parts:**

1. **Persist the raw datum first, and make it primary.** Write each response the moment it returns
   — before parsing, normalising, aggregating or resolving. Labels are derived; text is the datum.
   A parse bug, an adjudication bug, or a later capture bug then degrades to *recoverable* instead
   of *total*.
2. **Write-then-verify per unit, not per run.** Assert the artifact landed and is non-empty before
   the next unit issues. A run that dies at call 140 leaves 140 recoverable answers instead of zero;
   verifying at the end has the same failure profile as not verifying at all for everything that
   never got made.
3. **The mirror gate is necessary and not sufficient.** *Captured outputs == expected outputs* still
   passes when every file is written empty. Assert count **and** non-emptiness **and** that each
   parses to a value in the expected vocabulary. Out-of-vocabulary results are *reported, never
   coerced*, and the gate runs after everything is on disk so a failure is a finding with its
   evidence retained rather than a second loss.
4. **Count from the artifact, never from the loop.** Reporting `len(results)` as the persisted count
   is a claim about persistence sourced from the thing that is not persistence. Count the files.

**The attention asymmetry, which is the transferable part.** In the same session, the driver's
*refusal* path got the strongest control in the arc — five constructed bad states plus a converse —
and the *capture* path got nothing. Both were untested by construction. The difference was that the
refusal path had a **red light** on it (a mis-written control that had inverted to permanently-red)
and the capture path emitted **no signal at all**, because a writer that does not exist produces no
error, no warning, and no output to inspect. **Attention went to the failure that announced itself.**
A verification stack audited by following its red lights will systematically miss every defect whose
signature is silence — and those are the ones that cost whole runs. Enumerate what a spend *depends
on* and mark each path tested or untested; do not infer the list from which lights are lit.

---

## The receiver's license to refuse: the same construction from the other side

**Operator ruling, 2026-08-11 (OQ-277 arc).** *Write the receiver's prompt* is the sender's half. The
receiver's half had been operating unnamed for the whole arc, and is the half that actually caught
things.

**The rule.** An instance executing a handoff holds a standing **license — and an obligation — to
refuse an instruction that is correct in prose and wrong when executed**, and to say so rather than
comply. "The prompt said to" is not a witness. The refusal is reported at the same volume as a
completion, because a receiver who complies with a prose-correct/execution-wrong instruction produces
work that looks exactly like work, and the defect lands in the substrate with the sender's authority
on it.

**Why it works: the same mechanism as the sender's half, from the other end.** The sender re-reading
their own design exercises **recognition**, and a rule correct in prose passes recognition every time.
The receiver, in order to comply at all, must **enumerate** — produce the concrete actions the
instruction licenses — and the gap surfaces the moment the enumeration is attempted. **Enumeration
catches what recognition doesn't.** That single sentence is the finding; the two constructions are
just the two places to force enumeration to happen: *before* the handoff (the sender writes the
prompt) and *at* it (the receiver refuses instead of complying). **A handoff with neither is two
recognitions in a row**, and neither party ever occupies the position from which the defect is
visible.

**The inversion worth stating plainly: the receiver's ignorance is load-bearing.** A receiver who
shares the sender's context supplies the missing halves silently and catches nothing — which is the
same reason the sender's half "does not work on a receiver who is yourself" (above). The handoff
boundary is **the only place in this working form where a specification is executed by someone who
did not write it**, which is the one condition under which underspecification becomes visible at all.
So worker amnesia — the central liability of the whole arrangement, the thing every memory document
exists to compensate for — is *also* the mechanism that makes specification defects detectable. It is
not only a cost being paid; at the handoff it is the instrument.

**Witnessed.** The operator's count is **five refusals in this arc, every one catching a real defect,
and none of them caught by a sender re-reading** (2026-08-11). *Recorded as the operator's count, not
one this document has itemized* — three are written up in the section above (the prereg that pinned
what is judged and never what is shown; the prose ruling confounded once written as an executable
draw; the self-check command that could not consume its own specified format). **The remaining two are
unitemized**; a future session that finds them should list them here rather than let the figure stand
on memory (this is the *stated-versus-counted* rung applied to this very entry).

**How it fails: the compliant receiver.** The default disposition of a fluent instance is to comply,
and compliance is *most* likely exactly where the instruction reads well — a smooth prompt suppresses
the enumeration that would break it. Two consequences:

- **The license must be stated in the prompt, not assumed.** An unstated license is not exercised. A
  handoff prompt should carry it explicitly, in the sender's own words, alongside the standing rules.
- **The refusal must be routed to the sender, not resolved in place.** A receiver who silently repairs
  the instruction and proceeds has converted a design defect into a local fix that no one else learns
  about — the defect stays in the design and fires again on the next receiver.

**Scope — what this is not.** It is *not* licence to refuse work that is merely hard, ambiguous,
tedious, or disagreeable; not licence to substitute a better plan; and not a general veto. The
trigger is narrow and checkable: **executed as written, this instruction produces something the design
means to forbid.** Everything else routes through the existing channels — a genuine ruling escalates
(the operator's seat), a better implementation gets the *one-sentence flag* and then proceeds. The
distinction from the flag is the disposition: the flag says *"there is a better way, proceeding
anyway"*; the refusal says *"executing this as written is wrong, not proceeding."*

**Family.** Same family as *Every diagnostic needs a positive control* and *An introduced instrument
is itself a claim*: it is a control on the thing you control with. It also inherits their asymmetry —
a refusal that turns out to be wrong costs one round-trip; a compliance that turns out to be wrong
costs the artifact and everything built on it.

### Corollary — a receiver holding pins on a file LOCKS that file, and clearance means checking the pin set

**Operator ruling, 2026-08-11, from a live instance of the mistake.** A handoff brief that cites
`file:line` hands the receiver a set of **pins**. For as long as that receiver is running, the file
is under active reference, and any parallel writer editing it can shift a pin underneath them.

**The failure is silent and it corrupts the record rather than breaking the run.** A receiver that
finds a cited line no longer holding its content does the right thing — locates by content and
*records a correction with both line numbers*. If the drift was caused by a parallel writer rather
than by real churn, that correction is a fabricated provenance note: it documents a discrepancy that
did not exist when the brief was written. Nothing errors, and the extraction notes now contain a
false fact with a witness attached.

**The rule.** Before editing a file a running receiver holds pins on, check **the pin set**, not the
pins you happen to know about. And the pin set has to be *discoverable*: a brief that cites eleven
line numbers while nothing in the repository says "these lines are held" makes the check impossible
to perform correctly, which makes the omission a defect in the **handoff**, not in the writer.

**The witnessed instance.** A parallel instance verified two cited lines (473, 486), reported that as
clearance, and edited `CLAUDE.md` — the pin set was eleven lines, 473–527. The edit was an insertion
at line 598 and moved nothing, so the outcome was clean; **the clearance was narrower than the
confidence attached to it, and the safety was luck.** This is the arc's own recurring shape — a sound
measurement licensing a claim wider than its coverage — committed while documenting that shape.

**Scope note against over-reading the one-writer rule.** *One writer at a time* (CLAUDE.md → *How the
operator works*) is about **shared trackers** — `ISSUES.md`, `KNOWN_STATE.md` — colliding across
instances. It says nothing about a file that is merely *referenced* by a parallel receiver, and a
file under active reference is not protected by it: the writer holds the only write, obeys the rule
completely, and still corrupts the receiver's record. The two constraints are independent, and only
one of them is currently enforced by anything.

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

**Corollary — save member LISTS, not counts, for any engine-computed selection.** A count over a
selection the engine computes ("16 false-mountain rows of 98 manifest") is DERIVED twice over: it
inherits both the corpus snapshot and the engine regime, and it rots when either moves — while a
saved member list stays checkable item-by-item forever. Witnessed (OQ-52): the 2026-06-02
"16 of 98" was recorded as counts + 5 example names; four weeks of engine drift later the count
was 235 of 944, the full 16-list had never been written to substrate, and the historical selection
was UNRECONSTRUCTIBLE — the recorded control ("recover the 16") had to be retired as impossible
rather than run. The five saved names were the only member-level anchor that survived (their H1
values reproduced exactly). Rule: when a finding is a selection, the artifact is the ID list (plus
the manifest pin); the count is a summary of the artifact, never the artifact.

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
| An **approved plan's** empirical premises (corpus content/shape, named witness cids, line anchors) | state | **2–3** | re-witness each load-bearing premise against the live substrate at execution — approval does not refresh it. A plan is a bundle of citations authored at time N, executed at N+k; the corpus rebuild can silently invalidate one. Witnessed: the OQ-19 plan asserted "live data is 2-decimal," but a rebuild had made 4 constraints authored-3-decimal between authoring and execution (KNOWN_STATE 2026-06-25) — caught only by re-probing, not by reading the plan |

---

## A correction is not done until the old value's consumers are swept

The citation-time rule above governs *re-using* a witnessed fact; this is its dual for
*correcting* one. When a number, label, or detector semantics is found wrong, fixing the source
site is half the job: the old value may be load-bearing anywhere it was ever cited, and each such
site silently re-asserts it after your fix. **Rule: a correction ships with a sweep of the old
value's consumers** — grep the token/number/predicate across `prolog/`, `python/`, `audits/`,
`ISSUES.md`, `docs/`; give each hit a per-claim verdict (*rests-on-the-old-semantics* → its own
correction line or OQ; *anchored-to-its-own-witness* → record why it stands); commit the sweep as
an artifact, not a session claim. A "zero corrections owed" conclusion needs the sweep pasted.

Witnessed instance (OQ-151 close, 2026-08-09): the `seat_role_vector` twin-agreement 0.245 was
measured on the power-keyed vector, and because nobody swept its spenders at mint, the mis-keyed
number propagated into `design_discipline.md` §0.1 and two ISSUES entries *as a decline ground* —
"finding the number and not checking who spent it is how the 0.245 got where it is" (operator).
The corrective arc then ran the sweep as an audit artifact
(`audits/2026-08-09_oq151_dual_gauge/consumer_sweep.txt`, per-claim verdicts in its WRITEUP §3)
and corrected all three citation sites in one commit. The durable form of a sweep is a pointer at
the corrected clause itself (the `unanimous_verdict/4` comment naming its typed refinement) — a
sweep is a snapshot; the clause comment is where the next reader will actually be standing.

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

**The gate that keys on a present-but-wrong proxy (the spine's mirror).** The spine is *absent input
read as present*; this is its mirror — a **present, plausible input that is adjacent to the real
precondition, not the precondition itself**. When a check gates a decision, verify it tests the actual
precondition, not a nearby observable that *usually* co-varies with it — and construct the case where
the two come apart **before** trusting the check. This is not Pattern 5 (there the input is absent);
here the input is present and clean-looking, so it never trips a missing-data guard — it fails only
where proxy and precondition diverge, which is exactly the case the decision turns on. Witnessed
recurring across unrelated surfaces in one session (OQ-197/detector_calibration, 2026-07-01): a
type-binary reused as a proxy for "the calibration-relevant axis"; a `piton_theater_floor` borrowed as
a proxy for "a calibrated cover-story threshold"; a stakeholder *count* read as a proxy for
"`extraction_blindness` is operative"; a bound-mode predicate that *reads as* the property but is a
proxy under its default binding; a grep for a `"gaps"` render used as a proxy for "distinguishes
undetermined at a human surface." Each looked clean from outside; each was caught by attention, not a
control. Provenance and the five instances: `docs/design/detector_calibration_omega_proposal.md`
(Corrections log → *Named failure mode* / *Standing guard*).

---

## An audit directory has one entry point and an evidence map (Patterns 1+2 on the prose substrate)

The 2026-08-06 `audits/` index build found the writeup layer forked the way code does: four rival
entry-point names (`WRITEUP.md`/`FINDINGS.md`/`README.md`/bespoke), three spellings of
pre-registration (`PRE_REGISTRATION`/`PREREGISTRATION`/`PREREG`), six directories with no writeup
at all — and mechanical index extraction mis-picked the "main" file in several directories (a
subagent prompt, a raw stage-2 draw output) because no fact said which file was canonical. Same
defects, prose substrate: canonicity was a memory, not a checked fact (Pattern 2), and evidence
artifacts sat unmapped where nothing declared which claim consumed them (Pattern 1).

**The rule (full spec + header template: `audits/README.md` → *Writeup format*):** every new
audit directory carries exactly one `WRITEUP.md` entry point — required header block (executed
date, OQ pointer, one-line verdict at its scoped altitude, substrate/manifest cite, evidence
map) — plus reserved phase-file names (`RECON.md`, `PROPOSAL.md`, `PREREGISTRATION.md` — one
spelling, frozen at spend time and never retro-edited). The evidence map is the Pattern-1
discharge for the directory: every artifact is either named with the claim it witnesses or
declared dead. Existing directories are point-in-time and stay as-is; the convention is
forward-only — and it is a **checked fact, not a memory** (the Pattern-2 rule discharged
literally): `python/audit_writeup_gate.py --check` is wired into `scripts/gate.sh`, fails
closed on malformed directory names, and runs an 8-fixture selftest (6 violation shapes must
flag, 2 conforming shapes must pass) before every live sweep.
