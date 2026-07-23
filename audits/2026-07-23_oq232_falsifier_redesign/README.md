# OQ-232: Axiom 2's falsifier redesigned as two scoped discriminating perturbations

**Date:** 2026-07-23. **Plan:** rev 4 (post-review-3), executed from
`~/.claude/plans/pick-the-one-that-ancient-wren.md`. **Status at close:** amendments landed
in v8 §9.5 item 3 and v6.13.1:88; probe green; Axiom 7 sweep verdict PRESENT; class OQ
minted (see ISSUES.md).

## Recon (read-only, verified against source this session)

- The published falsifier (v8 §9.5 item 3 pre-amendment; v6.13.1:88 pre-amendment) controls
  for information access and position but not **preference adaptation** — a mechanism the
  framework itself models (`suppression`, `accessibility_collapse`). The kill could fire
  while the axiom is true.
- P and E are separable site indices: d = BaseD(P) + ExitMod(E), clamped
  (`constraint_indexing.pl:470–499`); trapped contributes +0.05. "Powerless = high exit
  costs" is a population correlation, not a definition.
- Site index set is exactly four: `context(agent_power, time_horizon, exit_options,
  spatial_scope)` (`constraint_indexing.pl:139`) — feeds the class OQ's
  channel-enumeration sub-item.
- Review arc (three rounds): the confound relocated each round — E-channel (rev 1),
  P-channel (rev 2), cancellation neighborhood (rev 3). Rev 4 asserts intervals, not
  points, and ships two scoped falsifiers rather than one joint kill.

## Proposal (pre-registered before execution)

- **P-arm (primary, sign-discriminating):** hold E, vary P. Sign table: positive = axiom's
  prediction; flat = refutes (Axiom 2 ∧ no comparable-magnitude cancellation); negative =
  P-adaptation dominant, axiom indeterminate. Flat is the cancellation signature, never a
  clean kill.
- **E-arm (secondary, magnitude-discriminating):** hold P, vary E. 2×2; Cell 4 (both
  non-extractive) refutes (Axiom 2 ∧ no P-channel adaptation). Decision statistic:
  |Δχ| ≤ ε·σ(S)·|f(d)−f(d−0.05)|; **3× safety factor registered as arbitrary; 1×–3× band
  declared indeterminate**; feasibility compared to a declared 7-point resolution (1/6).
- **Probe assertions (i)–(v)** per plan A.5: old observable non-discriminating; E-arm
  conjunct necessary; P-arm sign separation + D flagged in-band, asserted over the swept
  s-interval, not a point; criterion printouts with threshold-link breaker; s=0 control.
  Escape hatch: interval-empty ⇒ surface to operator before amending.
- **Part B criterion (pre-registered):** Axiom 7 confound class PRESENT iff (i) the
  falsifier observable is producible at report level by naturalized foreclosure while the
  axiom is true, AND (ii) the kill is unconditioned on structural witnesses in the
  falsifier text or via explicit cross-reference from it (conditioning living only in
  §4.3 / v8:689–700, unreferenced from §9.5, does not count).

## Execution

- `discrimination_probe.py` → `discrimination_probe.log` (all assertions PASSED; worlds
  A–D × {linear, threshold} links × s ∈ [0, 2.5] step 0.005; s=0 control; negative-branch
  positive control at res=0.01 + concentrated adaptation shape).
- Part B applied to the §9.5 sentence against v8 §4.3, v8:689–706, v7 §2.
- `falsifier_sweep_grep.txt`: token positive controls (both known sites hit) + concept
  sweep + eyeball verdict.

## Writeup — findings (all cite probe output or code inspection)

1. **Old observable non-discriminating (probe i).** "A trapped worker rates
   non-extractive" fires in all four worlds — axiom-true-with-E-adaptation (s ≥ 0.695
   linear), axiom-false (all s), axiom-true-with-P-adaptation (s ≥ 0.82), and the
   cancellation world.
2. **E-arm conjunct demonstrated necessary (probe ii).** Cell 4 fires in worlds C and D —
   where Axiom 2 is TRUE — so it refutes only (Axiom 2 ∧ no P-channel adaptation), never
   Axiom 2 alone. It never fires in world A (Cell 2 obtains there).
3. **Cancellation band: lower edge as expected, upper edge UNBOUNDED (probe iii —
   deviation from the expected ±X form).** s_c = 1.329 (adaptation magnitude equal to the
   axiom's own end-to-end P-slope, 0.930 χ-units / 0.70 per unit s). Linear 7-point link:
   world C collapses onto world B's signature (E-arm fired + P-arm flat) for
   s ∈ [1.015, +∞) — lower edge at −23.6% of s_c, but **unbounded above**: the bounded
   scale floors both ends of the P range, so the sign table's NEGATIVE row is unreachable
   under proportional adaptation at 7-point resolution. Flat's disjunction gains a third
   disjunct: (axiom false) ∨ (comparable-magnitude cancellation) ∨ (dominant adaptation,
   floored). Threshold link: collapse from s = 0.495 (−62.8% of s_c) — saturation widens
   the band far below s_c. **Positive control for the absence claim:** at res = 0.01 with
   concentrated adaptation the negative verdict IS reachable (first s = 0.945) — the
   verdict machinery works; its absence in the primary sweep is a resolution+shape
   feasibility fact, not a dead branch.
4. **Coverage did NOT collapse** (no operator-halt condition): the separation interval
   [0, 1.015) under the linear link is non-empty and covers all sub-cancellation
   adaptation magnitudes; world A (positive at every s) never collapses onto B.
5. **Magnitude criterion feasibility (probe iv).** The 1× χ-bound translates below one
   7-point scale step at EVERY held position (max 0.074 vs 0.167) — at 1× the criterion
   decides nothing at a realistic instrument. The 3×-inflated bound CROSSES one scale
   step at mid-slope positions (powerful, organized: 0.223) — the feasibility verdict
   itself flips inside the 1×–3× band, so the band's declared indeterminacy is live, not
   academic. The threshold link amplifies a sub-bound Δχ (0.002) to a full-scale rating
   gap: the criterion is licensed only under an approximately linear reporting channel.
6. **Part B verdict: confound class PRESENT for Axiom 7.** (i) holds — v8:700–706 itself
   states thoroughly foreclosed positions are least likely to author, and the
   acknowledged bit is authored, not detected: report-level inability-to-author is
   disjunctive over (axiom false ∨ formation erased the capacity). (ii) holds — the §9.5
   sentence carried no conditioning and no cross-reference; the structural witnesses
   (consistent negation, present beneficiary) live only at v8:693–695/§4.3, unreferenced.
   Second instance confirms the class.
7. **Class-OQ recon.** The site's index set is exactly {P, T, E, S}
   (`constraint_indexing.pl:139`). P and E are adaptation-indexable (adaptation attaches
   to what the rater cannot costlessly vary — position and exit). T and S are adoptable
   analytic frames, not facts of the rater's predicament — not plausibly
   adaptation-indexable. The framework's own exposure closes at {P, E}; the sharpened
   open question is whether the four-index set is complete.
8. **Sweep.** No live surface outside the two amended sites restates the old falsifier;
   archived versions and `docs/v8/foundations/` snapshots contained by pointer (see
   `falsifier_sweep_grep.txt` eyeball verdict). v7 needs no amendment.

**Declared limits (probe overclaim guard):** the probe is a logical demonstration under
declared assumptions — the χ→rating link is invented (two links swept), adaptation shapes
declared (proportional primary; concentrated only as control), world B's constant an
existence instantiation. No empirical claim about actual raters is made.

**Arm statuses at close:** E-arm instantiable (FTC non-compete window, H-1B portability,
plant closures with pre-period surveys) but refutes only its stated conjunction. P-arm
specified, sign-discriminating, **OPEN-instantiation** (candidates: workplace promotion
studies, union-officer elections, tenure cases; none design-checked end-to-end this
session). Consequence, stated plainly: **Axiom 2's empirical anchor currently has no
runnable falsifier** — the runnable arm kills only a conjunction; the arm that kills more
is uninstantiated.

## Artifacts

- `discrimination_probe.py` — the probe (standalone, no repo imports)
- `discrimination_probe.log` — pasted run, all assertions green
- `falsifier_sweep_grep.txt` — C.3 sweep with positive controls + eyeball verdict

## Post-close adjudication (2026-07-23, Claude-web round-4 review — six checks, all run against the artifacts)

1. **Feasibility lands where the falsifier doesn't apply — CONFIRMED from the log's
   per-position table.** At powerless (the position the kill names): 3× bound = 0.0778
   rating units vs 0.1667 resolution — sub-resolution; the 3× bound crosses resolution only
   at `organized`/`powerful` (0.2233). Point-of-use qualifiers added: v8 item 3 ("instantiable
   now on its categorical read"), v6.13.1, OQ-232 close, KNOWN_STATE headline.
2. **The 3× factor manufactures the discriminating power rather than being conservative —
   CONFIRMED** (1× decides nothing anywhere; 3× clears only mid-slope). v6.13.1 reworded:
   threshold derived from a declared number of scale steps, implied multiplier reported per
   position (one step at powerless implies k≈6.4); bare-multiplier framing removed.
3. **"Sign-discriminating" overclaimed — CONFIRMED.** Under proportional adaptation at
   7-point the negative row is unreachable; the arm discriminates positive-vs-flat.
   All four surfaces requalified; the P-arm's OPEN-instantiation is now stated as a spec
   (floor-free or fine-resolution measure), not merely "none identified."
4. **Structural witnesses are a third repair strategy (exits the class), not a scope
   patch** — added to OQ-234 with the leading sub-item: is there a structural observable
   for Axiom 2 (the behavioral supplement's demotion was exit-specific, so the question is
   open, not settled negative)?
5. **The {P,E} closure claim is a declared judgment, not an engine read** (the engine does
   not index adaptation at all — suppression is constraint-level) — labeled as such in
   OQ-234's channel-enumeration sub-item.
6. **§5.9 citation resolves** — §5.9 heads at v8:655; the structural-witness passage
   (consistent negation, present beneficiary) sits at v8:677/694, inside it. No fix needed.
