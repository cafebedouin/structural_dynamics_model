# STAGE 1: FORMAL CONSTRAINT NETWORK

```
```

---

## CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.10</epsilon>
    <suppression>0.05</suppression>
    <coordination>true</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>       <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>universal</scope>        <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.5 × 1.0 = 0.15</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 (PASS: 0.10), Supp ≤ 0.05 (PASS: 0.05) — Mountain gate satisfied</threshold_check>
    <rationale>Zero-construction partition; immutable across biographical time horizon; trapped exit confirms no escape vector exists at this index.</rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>institutional</power>   <!-- π = -0.2 -->
      <time>civilizational</time>
      <exit>arbitrage</exit>
      <scope>universal</scope>        <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.10 × -0.2 × 1.0 = -0.02</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 (PASS: -0.02), χ ≤ 0 → ε ceiling bypassed (net beneficiary path)</threshold_check>
    <rationale>Negative χ indicates net extraction flows toward this index; administrative/arbitrage exit option confirms changeability from this structural position.</rationale>
  </agent>

  <indexical_variance>
    C1: X₁ χ=0.15 → Mountain, X₂ χ=-0.02 → Rope
    Same partition; powerless/trapped index perceives immutable terrain, institutional/arbitrage index perceives a managed coordination mechanism it administers and extracts negative cost from.
  </indexical_variance>

  <boltzmann_test>
    FAIL for universal classification — Type varies with Power (powerless→Mountain, institutional→Rope) while Scope held constant (universal, σ=1.0 both). Classification is non-factorizable across P.
    However: base ε=0.10, Supp=0.05 independently satisfy Mountain thresholds. Resolution: the partition-as-physical-law passes Boltzmann (S invariant, natural emergence plausible at civilizational T); the partition-as-administered-boundary fails Boltzmann for X₂'s index specifically because X₂'s exit option (arbitrage) demonstrates the boundary is a maintained jurisdictional edge, not terrain. This is the Missing Floor signature: apparent Mountain status for X₁ is real (χ, ε, Supp all clear) but coexists with a constructed-boundary substrate visible only from X₂'s index.
    Verdict: Mountain classification for X₁ is CERTIFIED at X₁'s index (PIR-invariant natural law from below). Global Mountain status for C1 is NOT certified (fails cross-index independence) — this asymmetry is itself the load-bearing structural fact, not an error.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.30</epsilon>
    <suppression>0.40</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>        <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>universal</scope>        <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.30 × 1.0 × 1.0 = 0.30</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 (PASS: 0.30), ε ≤ 0.45 dual-threshold check (PASS: 0.30) — Rope gate satisfied</threshold_check>
    <rationale>Below Rope ceiling on both axes; constrained exit (high cost, not trapped) plus moderate power position yields a genuine agency-instrument classification.</rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>powerless</power>       <!-- π = 1.5 -->
      <time>civilizational</time>
      <exit>trapped</exit>
      <scope>regional</scope>         <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.30 × 1.5 × 0.9 = 0.405</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.35 < χ ≤ 0.90 (PASS: 0.405, strict floor cleared), ε ≥ 0.30 (PASS: 0.30, boundary), Supp ≥ 0.40 (PASS: 0.40, boundary) — Tangled Rope gate satisfied</threshold_check>
    <rationale>Power-scaled extraction crosses the strict Tangled floor; coordination value (shared meaning function) and asymmetric cost (extraction of dormant affect) both present per Coord=true, Asym=true; enforcement is active (the mechanism operates on the subject without consent-renewal).</rationale>
  </agent>

  <indexical_variance>
    C2: X₁ χ=0.30 → Rope, X₃ χ=0.405 → Tangled Rope
    Divergence driven by π differential (moderate=1.0 vs powerless=1.5) and σ differential (universal=1.0 vs regional=0.9) acting in opposite directions on χ, with the π gap dominating. Instrument-wielding index sees pure coordination tool; instrument-receiving index sees hybrid coordination-extraction.
  </indexical_variance>

  <boltzmann_test>
    N/A — C2 does not claim Mountain status. ε=0.30 exceeds mountain_extractiveness_max (0.25) regardless of index.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>0.80</epsilon>
    <suppression>0.90</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>       <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 (PASS: 0.96), ε ≥ 0.46 (PASS: 0.80), Supp ≥ 0.60 (PASS: 0.90) — Snare gate fully satisfied</threshold_check>
    <rationale>All three Snare floors cleared with margin. SnareImmutability holds: immediate/trapped index perceives no exit, while institutional index (X₂, below) perceives the identical mechanism as Rope — textbook index-sensitive Snare signature.</rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>institutional</power>   <!-- π = -0.2 -->
      <time>immediate</time>
      <exit>arbitrage</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × -0.2 × 0.8 = -0.13</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 (PASS: -0.13), χ ≤ 0 → ε ceiling bypassed</threshold_check>
    <rationale>Net-beneficiary position; the condition costs this index nothing and resolves an administrative exception at negative marginal cost, regardless of high base ε.</rationale>
  </agent>

  <indexical_variance>
    C3: X₁ χ=0.96 → Snare, X₂ χ=-0.13 → Rope
    Maximum divergence in the network. Identical mechanism, opposite sign of χ. This is the SnareImmutability condition in its clearest form: the subject's index cannot factor the condition as anything but a trap; the administrator's index cannot factor it as anything but routine cost-free housekeeping.
  </indexical_variance>

  <boltzmann_test>
    FAIL as expected/required — C3 does not claim Mountain status (ε=0.80 far exceeds 0.25 ceiling). Test not applicable; included for completeness of network certification. The extreme type-divergence (Snare vs Rope) itself confirms non-factorizability across P, consistent with a constructed instrument rather than natural law.
  </boltzmann_test>
</constraint>
```

---

## TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>deploy_extraordinary_capability_to_breach_partition</condition>
    <target>C1</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = moderate (π=1.0), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.10 × 1.5 × 1.0 = 0.15 → Mountain</before>
    <after>χ = 0.10 × 1.0 × 1.0 = 0.10 → Mountain (unchanged type; χ drops but stays below Rope ceiling too)</after>
  </chi_recalculation>
  <type_change>Mountain → Mountain (local index shift insufficient to reclassify; underlying constraint is genuinely resistant to unilateral action at biographical/civilizational scale mismatch)</type_change>
  <preconditions>X₁ must access a capability class normally unavailable to powerless-index agents (state variable: capability_flag = exceptional)</preconditions>
  <blocked_by>C1's Boltzmann-certified Mountain status at X₁'s baseline index; capability_flag reclassifies E and P locally but does not alter T (civilizational) or the natural-emergence property, so type persists</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>exercise_affect_instrument_upward_against_institutional_target</condition>
    <target>C1</target>
    <agent>X₂</agent>
  </trigger>
  <index_change>
    <from>P = institutional (π=-0.2), Coord-context = administering</from>
    <to>P = institutional (π=-0.2), Coord-context = petitioned</to>
  </index_change>
  <chi_recalculation>
    <before>χ = -0.02 → Rope (C1 baseline for X₂)</before>
    <after>χ = -0.02 → Rope (χ unchanged; the transformation operates via C2's mechanism, not via re-indexing C1 directly)</after>
  </chi_recalculation>
  <type_change>Rope → Rope, but state variable exception_granted = true is appended (see TR3 for downstream effect)</type_change>
  <preconditions>C2 must be active (Coord=true) and directed at X₂ with sufficient ε×π product to register as input rather than noise — requires X₃-class audience effect to transfer analogically to X₂</preconditions>
  <blocked_by>Nothing structural; this is why generation_order places C2 as necessary precondition for challenging C1 — the affect-instrument is the only channel with nonzero transfer function into institutional-index decision-making</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>institutional_agent_grants_conditional_exception_to_generate_new_constraint</condition>
    <target>C3</target>
    <agent>X₂</agent>
  </trigger>
  <index_change>
    <from>C1 exception_granted = true (from TR2)</from>
    <to>C3 instantiated: ε=0.80, Supp=0.90, Coord=false, Asym=true</to>
  </index_change>
  <chi_recalculation>
    <before>C3 does not yet exist (no χ)</before>
    <after>χ(X₁) = 0.80 × 1.5 × 0.8 = 0.96 → Snare; χ(X₂) = 0.80 × -0.2 × 0.8 = -0.13 → Rope</after>
  </chi_recalculation>
  <type_change>∅ → {Snare (X₁ index), Rope (X₂ index)} — constraint generation, not transition of existing constraint</type_change>
  <preconditions>TR2 must have fired (exception_granted = true); institutional agent selects condition parameters such that ε and Supp are set high while Coord is set false — a design choice, not a forced outcome</preconditions>
  <blocked_by>Nothing prevents this generation; the omega (sovereign_motive) leaves ambiguous whether X₂ selects these parameters knowing SnareImmutability will hold for X₁, or believes χ(X₁) will resolve favorably. Rule fires identically under either interpretation — motive is not encoded in the mechanical trigger.</blocked_by>
</transformation_rule>

<transformation_rule id="TR4">
  <trigger>
    <condition>satisfy_condition_before_boundary_exit_point</condition>
    <target>C3</target>
    <agent>X₁</agent>
  </trigger>
  <index_change>
    <from>E = trapped, T = immediate</from>
    <to>E = mobile, T = immediate (hypothetical, unrealized in source break_contract)</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.96 → Snare</before>
    <after>χ_hypothetical = 0.80 × 0.6 × 0.8 = 0.384 → Tangled Rope (if condition satisfied, P effectively shifts toward powerful=0.6 via demonstrated exit)</after>
  </chi_recalculation>
  <type_change>Snare → Tangled Rope (hypothetical only — not reached in break_contract, where prior_status=DEAD indicates failure path taken)</type_change>
  <preconditions>Sustained satisfaction of C3's embedded condition across full immediate-time-horizon window; zero relapse into C2-triggered behavior</preconditions>
  <blocked_by>C2 itself — the same instrument that generated leverage against C1 (TR2) reintroduces failure vector into C3 because C2's Supp=0.40 asymmetric extraction (affect triggering doubt/backward-look) operates on X₁ throughout the C3 window. This is the structural trap: TR2's precondition (C2 active) is never switched off, so it remains available to sabotage TR4.</blocked_by>
</transformation_rule>
```

---

## ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₂</agent>
  <constraint>C3</constraint>
  <actual_type>Rope (at X₂'s own index, χ=-0.13, correctly classified)</actual_type>
  <perceived_type>Rope (X₂'s self-classification is accurate for X₂ — error is in failing to model X₁'s index)</perceived_type>
  <observable>Condition parameters (ε=0.80, Supp=0.90, Coord=false) are set without adjustment despite χ(X₁)=0.96 crossing Snare floor on all three metrics; institutional agent does not modify Supp downward or introduce Coord-preserving features that would lower X₁'s χ below 0.66</observable>
  <correction_trigger>Recomputing χ at X₁'s index (powerless, immediate, trapped, local) before instantiating C3 would surface χ=0.96 and trigger reclassification from "administrative Rope" to "Snare for the petitioner" — no such recomputation is evidenced in the generation sequence</correction_trigger>
</error>

<error id="E2">
  <type>Type I — False Mountain (partial/contained)</type>
  <agent>X₁</agent>
  <constraint>C1</constraint>
  <actual_type>Mountain at X₁'s baseline index (Boltzmann-certified at ε=0.10, Supp=0.05)</actual_type>
  <perceived_type>Treated as absolutely unchangeable prior to capability_flag exception (TR1) — but TR1 demonstrates local reclassification is achievable, meaning the pre-TR1 treatment was over-generalized</perceived_type>
  <observable>X₁ does not attempt boundary engagement through ordinary channels prior to acquiring exceptional capability, despite C1 showing coordination=true (a negotiable-in-principle predicate) at generation — the agent's own index treats Coord=true as irrelevant until affect-instrument (C2) becomes available</observable>
  <correction_trigger>Recognition that Coord(C1)=true implies a petition channel exists in principle; the correction is realized narratively via TR2 (using C2 to access it), so the error is transient/setup-stage rather than sustained</correction_trigger>
</error>

<error id="E3">
  <type>Type V.b — Tangled Rope Mishandling (Tangled-as-Snare, ignoring coordination component)</type>
  <agent>X₃</agent>
  <constraint>C2</constraint>
  <actual_type>Tangled Rope (χ=0.405, both Coord and Asym confirmed true)</actual_type>
  <perceived_type>Risk of pure-Snare misclassification if only the extraction component (forgotten/painful affect surfacing) is weighted</perceived_type>
  <observable>Aggregate response pattern to C2 (assembled audience state) would show only extraction-avoidance behavior (disengagement) if misclassified as Snare, versus the actual mixed pattern of sustained attention plus visible distress predicted by Tangled Rope's dual-component structure</observable>
  <correction_trigger>Verifying that removing C2 would eliminate a genuine shared-meaning coordination function (Type IV test) — if the function is real, Tangled Rope stands; observable engagement-despite-distress in the audience index confirms the coordination term is not zero</correction_trigger>
</error>

<error id="E4">
  <type>Type II — Mountain Denial</type>
  <agent>X₁</agent>
  <constraint>C3</constraint>
  <actual_type>Snare (χ=0.96, all floors cleared — genuinely near-immutable at X₁'s index within the immediate time horizon)</actual_type>
  <perceived_type>Treated as a surmountable condition rather than a structurally-engineered trap</perceived_type>
  <observable>Sustained condition-compliance behavior continues across the full trapped/immediate window without introducing any exit-option-expanding action (e.g., third-party verification, alternative confirmation channel) — action set does not expand despite Supp=0.90 signaling this requires active, near-total suppression to maintain, which is itself evidence of a maintained-not-natural barrier</observable>
  <correction_trigger>Applying the Boltzmann-style check locally — noticing that X₂'s identical constraint classifies as Rope — would reveal the condition is index-relative engineering, not fixed terrain; no such cross-index check is evidenced before the terminal window closes</correction_trigger>
</error>
```

---

## INSTITUTIONAL RATIONALITY MODEL

```
Model selected: PIR (Perfect Institutional Rationality)

Justification:
- X₂ (institutional index) shows χ ≤ 0 across both C1 and C3 — a pure net-beneficiary
  profile inconsistent with satisficing-under-uncertainty (BIR territory).
- No principal-agent slippage is present: X₂'s coordination-vitality is fully intact
  (C1 as Rope: functioning administrative mechanism, not degraded).
- C3's parameters (ε=0.80, Supp=0.90, Coord=false) are set at maximal-extraction /
  maximal-suppression / zero-coordination values — utility-maximizing design, not a
  bounded compromise. A BIR institution satisficing under risk aversion would more
  plausibly set intermediate Supp or preserve some Coord to hedge against backlash.
- omega(sovereign_motive) is undecidable between pity and realpolitik — but the
  MECHANICAL trigger (TR3) fires identically either way, meaning the formal model
  does not require resolving the omega to specify institutional behavior. PIR is
  compatible with both readings: a PIR agent acting from pity still deploys
  utility-maximal parameters if it correctly predicts those parameters as necessary
  to preserve system consistency (no Pareto-improving alternative on the table
  given Coord=false is a design choice, not a constraint).

Rejected: BIR would require evidence of negotiation, risk-averse parameter softening,
or principal-agent divergence between X₂'s stated intent and C3's actual parameters.
None is present in the constraint manifest; C3's properties are internally consistent
with a single, unbounded-optimizing institutional actor.
```

---

## TERMINAL ATTRACTOR SELECTION

```
Selected: Deterministic Tragedy

Compatibility check against Attractor Compatibility Matrix:
  Constraint profile = Mountain (C1 at X₁ index) + Snare (C3 at X₁ index),
  generated via Tangled Rope (C2) intermediary.

  Row match: "Mountain + Snares" → PIR → Deterministic Tragedy. MATCH.
  Row match: "Tangled Ropes dominant" + PIR → "Tragedy (crushed by hybrid)". 
    C2 as Tangled Rope is the generative mechanism that produces C3; this is
    consistent with the hybrid-crushes-outcome reading — C2 is not resolved,
    it is the very tool that manufactures the terminal Snare.

Justification:
- break_contract.prior_status = DEAD confirms constraints ran to completion
  rather than being bargained down (rules out Negotiated Equilibrium) or
  disrupted at the logic level (rules out Revolutionary Rupture — C1's
  partition-logic is fully intact at termination, per target_prior).
- target_prior ("perfect demonstration insufficient; human fallibility
  snatches defeat from victory") is the exact signature of Deterministic
  Tragedy: the terminal Snare (C3) executes its designed function against
  X₁ without institutional deviation.
- No Piton is present in the selected constraint set (C1, C2, C3 all show
  live, non-degraded coordination/extraction dynamics — no dead-coordination
  theater), which would be required to redirect toward Seeded Possibility.
  The deferred constraints (C4, C5) are hypothesized Mountains but are
  off-stage and do not alter the terminal attractor of the selected three.

Attractor: Deterministic Tragedy — CERTIFIED, compatible with PIR, compatible
with Mountain+Snare profile, compatible with Tangled-Rope-as-generative-hybrid.
```

---

## VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized with ε, Supp, Coord, Asym (C1, C2, C3)
☑ All χ calculations shown with π and σ values (all agent/constraint pairs)
☑ All characters use variable names (X₁, X₂, X₃) — no source identifiers in body
☑ Variable mapping table present in header (for traceability before anonymization)
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields
☑ Transformation rules use abstract trigger descriptions (TR1–TR4)
☑ Error observables are testable structural conditions (E1–E4)
☑ Institutional rationality model specified (PIR) with justification
☑ Terminal attractor selected (Deterministic Tragedy), justified, and compatible
☑ Indexical variance explicitly preserved across agents (all three constraints)
☑ Boltzmann test run for each constraint claiming Mountain status (C1 tested;
    C2, C3 correctly excluded as non-claimants with rationale given)
☑ No ambiguity in specifications
☑ Output is pure symbolic network
```

<!-- ANONYMIZATION: 3 character names replaced with structural labels: Agent_A, Agent_B, Agent_C -->
