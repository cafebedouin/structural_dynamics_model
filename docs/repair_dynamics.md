# Repair Dynamics: coordination is maintained, and the engine encodes its decay but not its repair

*Draft synthesis, 2026-06-10. Captures a line of theory that was scattered across conversation,
`prolog/transition_paths.pl`, `docs/logic.md` (Theorem 3), the sixth question in
`docs/the-few-seats-worth-choosing-v2.md`, and the commitment-systems sketches. It is a first cut
in need of the author's voice and judgment — especially §3–§5, which are conceptual. Claims are
tagged **[verified]** (checked against code/docs this session), **[analogy]** (structural
resemblance, not identity), or **[OPEN]** (not settled). Three separations are held as constraints
throughout — see §7.*

---

## 1. Coordination is maintained, not achieved

A rope — clean coordination — is not a static achieved state. It is a *maintained* one. Left
alone, it degrades. This is the same content as the commitment-systems principle **[verified:
`commitment_systems_sketch_v3` "drift is intrinsic … systems that deny drift accumulate gap until
breakdown; systems that acknowledge it can maintain stability indefinitely"]**: drift is the
default, and persistence of coordination is a standing achievement under upkeep, not a property a
constraint has once and keeps.

So the type space is not a static taxonomy. It is the state space of a dynamics: things drift down
a coordination-quality ordering unless something acts to hold or lift them.

## 2. Degradation — the downward dynamics (encoded)

`transition_paths.pl:transition_path/4` encodes **eight transitions, every one of them a descent
or a slide sideways-into-worse — none upward** **[verified this session: full enumeration of the
predicate's heads]**: rope→tangled_rope, tangled_rope→snare, rope→piton, scaffold→{piton, snare,
tangled_rope}, snare→{piton, false_mountain}. There is no `X→rope`, no `→scaffold`-as-improvement:
the transition *detector* is decay-only. It is consumed by `drift_report.pl` (a lifecycle/diagnostic
consumer) and **zero times by `run_pipeline.py`** **[verified]** — so the live classification path
carries no transition concept at all; it classifies each constraint statically. The degradation
dynamics is thus encoded but off the live path, not dead.

## 3. Repair — two metaphors, held apart

Repair runs in two distinct source domains, and they must not be composed ("splice the scaffold"
is a category error):

- **Rope (rigging) metaphor — operations on the line:** **maintain** (continuous upkeep),
  **splice** (local in-place mend), **replace** (swap for a better line). Cost-graded; which one
  applies is set by how far the reason/circumstances have drifted (§4).
- **Construction metaphor — the scaffold:** *not* a repair operation. It is the temporary
  alternative load-path you erect so a **load-bearing** constraint can be taken offline for repair
  without cascade collapse — because **you cannot work on the thing while you are fully relying on
  it** **[verified: `logic.md` Theorem 3, "load-bearing constraints (Supp ≥ 0.70) require Scaffold
  construction before removal; direct cutting triggers cascade failure"]**. The scaffold is struck
  (sunset) once the repair holds; if it is not struck it ossifies into a piton (the framework
  already says this).

The type vocabulary is **multi-metaphor by design** — mountain (terrain), rope (rigging), scaffold
(construction), snare (trapping), piton (climbing anchor), tangled_rope (fouled rigging). Each name
is chosen for its own source domain's aptness; they are **not** meant to compose into one extended
metaphor. The two repair metaphors touch at exactly one point — the load-bearing replace, where a
scaffold bears the load while the rope is swapped — and stay distinct images even there.

## 4. The economics of repair — the sixth question, and two persistence mechanisms

The sixth question **[verified: `the-few-seats-worth-choosing-v2.md` "Six: why was this built, and
is the reason still live?"; now also `docs/six_questions.md`]** sizes the repair and sorts the
arrangement. "Why was it built" has three answers: built for extraction (a snare — someone
benefits); built for a problem that no longer exists but persisting (drift); built for a problem
still live, where upkeep is the standing price of a still-needed good (the load-bearing good the
five cost-finding questions structurally cannot surface). Dead reason → fixing is cheap
(reorganize); live reason under changed circumstances → fixing is dear (build a better replacement).

Two **distinct** persistence mechanisms fall out, and they must be kept apart **[separation 3]**:

- **Load-bearing repair cost (scaffold-cost):** high suppression (Supp ≥ 0.70, Theorem 3) → a
  scaffold must be erected before the constraint can be touched → expensive. This is the
  **snare/rope** register.
- **Piton persistence (diffuse benefit / rational inaction):** the piton gate is *low* suppression
  (Supp ≤ 0.2) **[verified]** → cheap to cut, but unfixed because no concentrated party is hurt
  enough, or benefits enough, to act (per-fixer benefit < even a cheap fix). This is **not**
  scaffold-cost. It is OQ-90's open `fixing_cost` question and must not be collapsed into
  scaffold-cost. **[OPEN]**

## 5. The same dynamics as the committer/CS axis — and why they stay decoupled

The repair dynamics **rhymes** with the committer/commitment-systems axis **[analogy]**:
degradation ≈ drift, repair ≈ acknowledgment / marked-revision / bandwidth-extension, an unstruck
scaffold ≈ atrophy through unprocessed drift, the sixth question ≈ "is the configuration functional
in *this* environment?", and "cover story" is **literally the same term** in both.

But this is analogy across distinct objects (interventions vs. kernels), **not** a bridge, and the
distinction is mandatory, not stylistic. `deferential_realism_paper_v7.md` makes non-unification
its central commitment **[verified: v7 Theorem 7 Detection Independence — the axes detect *disjoint*
failures (an observer-coherent reading can be committer-foreclosed); line 27 "v7 refuses this fold
… incompatible mathematical characters … failures that do not coincide"; line 165 "the cost of the
second axis is the discipline of keeping it separate"]**. Therefore **[separation 2]**: the
observer-axis repair theory must be built in observer-axis terms; do **not** import committer-axis
machinery. The committer axis's acknowledgment/renewal theory is its own repair register and serves
here as *inspiration only*. The single sanctioned cross-axis link is v7 §4.5's bridge, and even
cross-axis *citation* requires one fixed-ε load (the `hanbali_reading` cautionary case, where an
H¹=0 and a foreclosure routing came from two different ε states).

## 6. The asymmetry, and what is missing

The engine encodes **decay** but **not repair**, and the claim is controlled at two scopes:

- *In the transition detector:* all eight `transition_path/4` heads are downward / lateral-into-worse;
  **none is upward** **[verified — full enumeration; the positive control is that the eight
  degradation paths are present, so an upward head would have shown up beside them]**.
- *On the live path:* `transition_path` is consumed **zero times in `run_pipeline.py`** **[verified]**
  — the live classification is static per-constraint, with no transition (up or down) concept at all.

So the observer axis can describe how a constraint falls, not how a scaffold lifts it.

This mirrors **[analogy]**, structurally, the five-questions-without-the-sixth bias: a decay /
cost-finding apparatus that by construction cannot see what is being fixed. The sixth question is the
guard for the question battery; the upgrade transition is the guard for the engine.

**A home exists, with a caveat that is the operator's to rule** **[verified structure]**: the
type-trajectory *reporter* `degradation_chain`/`snapshot_type` is **direction-neutral** — it reads
`snapshot_type` over the measurement series and reports the *sequence*, so it would surface an
*upward* run if one occurred; it is dormant (off the live path), not decay-shaped. The transition
*detector* `transition_path/4`, by contrast, is decay-only and would need upward heads added. So
whether to extend a decay-*named* detector or build a dedicated upward-transition structure is a
design decision, not a settled "home." Either way the machinery is unfinished, not cruft (`CLAUDE.md`
"Unwired ≠ worthless").

## 7. The three separations (held as constraints, not preferences)

1. **Metaphors unwelded** — rope-repair ops (maintain/splice/replace) ≠ scaffold (construction);
   the type vocabulary is multi-metaphor and does not compose.
2. **Axes decoupled** — observer vs committer; v7 Theorem 7; the cross-axis rhyme is analogy, the
   only bridge is v7 §4.5, citation only at fixed-ε.
3. **Persistence mechanisms distinct** — scaffold-cost (load-bearing, high-Supp, snare/rope) ≠
   piton persistence (diffuse benefit, low-Supp). Do not collapse them.

## OPEN / next

- Build the **upgrade transitions** (observer-axis terms) so the engine can register repair, not
  only decay — see the OQ.
- Represent piton `fixing_cost` (diffuse benefit vs fixing cost), distinct from scaffold-cost
  (OQ-90).
- Decide whether **maintain / splice / replace** warrant explicit named operations in the engine,
  or remain descriptive.
- ~~Whether to retire `five_questions.md`~~ — done 2026-06-10: retired to a redirect tombstone;
  Q1–5 preserved verbatim in `six_questions.md`.
