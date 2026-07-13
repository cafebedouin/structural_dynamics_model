# STAGE 1: FORMALIZATION OUTPUT

## Variable Mapping (header only — stripped upon anonymization)

```
G₁ ← the upper mill / low-height beneficiary institution (referenced in Ω only)

C₁ ← "The Naturalized Datum"
C₂ ← "Height-Based Rights"
C₃ ← "Destructive Codification"
C₄ ← "The Unwritten Turns" (deferred)
C₅ ← "Petitioning the Crown" (deferred)
C₆ ← "Voice Requires Rights" (deferred)
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.75</epsilon>
    <suppression>0.20</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>generational</time>
      <exit>identity_locked</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × 1.0 × 0.8 = 0.60</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ 0.60 ≤ 0.70 → PASS (Tangled Rope band). Coord=false, however — flags anomaly (see rationale).</threshold_check>
    <rationale>Positional value at this index is high (χ=0.60) yet Coord property is formally false. Classification driven by Asym=true plus generational time-horizon normalization: the constraint functions as coordination-substitute (shared cognitive frame) despite no genuine coordination payoff. This is the signature of a constraint that has been misrecognized as natural infrastructure rather than constructed asymmetry.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>analytical</power>          <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × 1.15 × 0.8 = 0.69</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ 0.69 ≤ 0.70 → PASS, near upper boundary (Snare threshold at 0.70)</threshold_check>
    <rationale>Analytical index elevates χ relative to X1 (0.69 vs 0.60) without changing type — constraint remains sub-threshold for Snare by 0.01. Proximity to boundary indicates fragility of the Tangled classification: minor ε or σ perturbation reclassifies as Snare from this index.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 χ=0.60 → Tangled Rope, X2 χ=0.69 → Tangled Rope
    No type divergence, but magnitude divergence (Δχ=0.09) driven entirely by π(analytical)=1.15 vs π(moderate)=1.0. Both agents occupy the same scope and asymmetric structure; the analytical index detects higher effective extraction from an otherwise identical structural position — consistent with π(analytical) design intent (degeneracy-breaking against normalization).
  </indexical_variance>

  <boltzmann_test>
    FAIL as candidate Mountain (not claimed as Mountain, but tested per protocol since C1 is described as foundational/naturalized).
    Procedure: Type(C1, I) computed at varying P (moderate → analytical) shows no type change (Tangled Rope in both), but χ magnitude changes with P (0.60 → 0.69), and Coord=false while Asym=true indicates the constraint's burden is non-uniform by construction. Since the constraint is explicitly a *cognitive framing* (naturalization of a constructed positional datum) rather than a physical regularity, and since it feeds downstream into C2 (a constraint with confirmed indexical variance and asymmetric extraction), it fails independence from P by definition — a true Mountain would show zero coordination-relevance and zero downstream constructedness. FAIL confirms: constructed, not natural.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.80</epsilon>
    <suppression>0.70</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X3">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.96 > 0.70 (snare_chi_threshold) → PASS. LowBaseExtraction(C2)=false since ε=0.80 → confirms Snare, not false-positive from power amplification.</threshold_check>
    <rationale>Maximal power penalty (π=1.5) combined with trapped exit option and high base extraction produces extraction well above Snare floor. Suppression=0.70 confirms enforcement-dependent maintenance consistent with Snare mechanism requiring active force.</rationale>
  </agent>

  <agent ref="X4">
    <index>
      <power>institutional</power>       <!-- π = −0.2 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × (−0.2) × 0.8 = −0.128</chi>
    <type>Rope</type>
    <threshold_check>χ = −0.128 ≤ 0 → ε ceiling bypassed per dual-threshold rule; χ ≤ 0.35 trivially satisfied → PASS as Rope</threshold_check>
    <rationale>Negative χ indicates net beneficiary status — this agent extracts FROM the system rather than bearing cost. Classified Rope despite high base ε because power-scaling formula captures the institutional agent's position as coordination-beneficiary, not coordination-payer.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>analytical</power>          <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.15 × 0.8 = 0.736</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.736 > 0.70 → PASS as Snare</threshold_check>
    <rationale>Analytical index detects Snare status that institutional index (X4) fails to register (χ=−0.128, Rope) and that X3's structural position confirms directly (χ=0.96, Snare). This triple-point comparison is the diagnostic case for indexical variance: same constraint C2, three types (Snare/Rope/Snare) depending purely on P.</rationale>
  </agent>

  <indexical_variance>
    C2: X3 χ=0.96 → Snare, X4 χ=−0.128 → Rope, X2 χ=0.736 → Snare
    Maximal divergence observed. Sign inversion of χ between X3/X2 (positive) and X4 (negative) demonstrates the constraint is a pure extraction-transfer mechanism: what is cost for powerless/analytical is net gain for institutional. This is the paradigm case of Type III error risk (Snare-as-Rope) if evaluated only from X4's index.
  </indexical_variance>

  <boltzmann_test>
    FAIL as Mountain (not claimed as such — included for completeness per protocol, since C2 is described in Stage 0 as a "system of extraction," not natural law).
    Type(C2,I) varies categorically with P: Snare(powerless) ≠ Rope(institutional) ≠ Snare(analytical). Non-factorizable dependence on P alone confirms FAIL. Constraint is constructed and coordination-dependent (Coord=true, Supp=0.70), consistent with Tangled/Snare hybrid family, not natural terrain.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>0.90</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X5">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.08 > 0.70 → PASS as Snare. Highest χ value in constraint network.</threshold_check>
    <rationale>Formalization of informal coordination system into enforced code, combined with trapped exit and powerless position, produces the network's maximum extraction value. High Supp (0.90) confirms this classification requires active institutional enforcement — the informal predecessor required none, per invariant contract (untranslatable_real).</rationale>
  </agent>

  <agent ref="X6">
    <index>
      <power>institutional</power>       <!-- π = −0.2 -->
      <time>immediate</time>
      <exit>analytical</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × (−0.2) × 0.8 = −0.144</chi>
    <type>Rope</type>
    <threshold_check>χ = −0.144 ≤ 0 → ε ceiling bypassed → PASS as Rope</threshold_check>
    <rationale>Institutional agent classifies the codification act as pure coordination improvement (Rope) — negative χ reflects genuine belief in Pareto-improving reform, not cynical extraction. This is the structural basis for Type IV error (Rope-as-Snare confusion) inverted: here the risk is the institutional agent committing Type IV in the *opposite* direction — treating extraction-generating codification as coordination when χ at the powerless index says otherwise.</rationale>
  </agent>

  <indexical_variance>
    C3: X5 χ=1.08 → Snare, X6 χ=−0.144 → Rope
    Binary divergence with no analytical-index agent classified for this constraint (X2 not indexed against C3 in Stage 0 data — recorded as omission, not zero-variance finding). Divergence magnitude (Δχ=1.224) is the largest in the network, consistent with C3's selection rationale as the "tragic conclusion" node.
  </indexical_variance>

  <boltzmann_test>
    FAIL as Mountain.
    Type(C3,I) shows categorical variance by P (Snare at powerless, Rope at institutional) — non-factorizable, confirming construction. Additionally NaturalEmergence(C3)=false by definition (codification is an explicit institutional act, not natural process). FAIL is overdetermined here: both the P-dependence test and the NaturalEmergence clause independently rule out Mountain status.
  </boltzmann_test>
</constraint>
```

---

## 2. TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>reveal_constructed_origin</condition>
    <target>C1</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), E = identity_locked</from>
    <to>P = analytical (π=1.15), E = analytical</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.75 × 1.0 × 0.8 = 0.60 → Tangled Rope</before>
    <after>χ = 0.75 × 1.15 × 0.8 = 0.69 → Tangled Rope</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope (no type change; magnitude shift only, approaches Snare boundary)</type_change>
  <preconditions>Agent must access biographical-or-shorter time horizon and exit identity-lock (structural precondition for π reindexing)</preconditions>
  <blocked_by>Generational time horizon reinforces identity_locked exit; without external disruption, X1 index remains fixed at moderate/identity_locked indefinitely (self-reinforcing naturalization)</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>codify_informal_system</condition>
    <target>C2</target>
    <agent>X6</agent>
  </trigger>
  <index_change>
    <from>C2 Coord=true (informal), Supp=0.70</from>
    <to>C3 Coord=true (formal), Supp=0.90</to>
  </index_change>
  <chi_recalculation>
    <before>χ(X3, C2) = 0.80 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ(X5, C3) = 0.90 × 1.5 × 0.8 = 1.08 → Snare</after>
  </chi_recalculation>
  <type_change>Snare → Snare (type preserved; magnitude intensifies — ε rises from 0.80 to 0.90, Supp rises from 0.70 to 0.90)</type_change>
  <preconditions>Requires C1 (naturalized datum) as accepted baseline; requires institutional agent (X6) to hold analytical exit but institutional power position simultaneously</preconditions>
  <blocked_by>Nothing internal to the constraint network — this transformation is the network's terminal, unblocked transition. Only an external agency injection (not present in the constraint graph) could prevent T2 from firing once C2 reaches sufficient suppression.</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>organize_collective_against_datum</condition>
    <target>C1</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0)</from>
    <to>P = organized (π=0.4)</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.75 × 1.0 × 0.8 = 0.60 → Tangled Rope</before>
    <after>χ = 0.75 × 0.4 × 0.8 = 0.24 → below Rope ceiling (0.35), ε=0.75 > 0.45 → dual-threshold check: χ≤0.35 satisfied, but requires χ≤0 OR ε≤0.45 for Rope; ε=0.75 fails ε-ceiling with χ>0 → reclassify as Naturalized (ε>0.45 ∧ χ<0.40)</chi_recalculation>
  <type_change>Tangled Rope → Naturalized (ambiguous zone; extraction suspected hidden/absorbed by collective power, requires further investigation per Naturalized action clause)</type_change>
  <preconditions>Requires successful collective coordination mechanism not present in current constraint network (deferred constraint C4 — "Unwritten Turns" — represents the pre-existing cooperative alternative that could seed this transformation)</preconditions>
  <blocked_by>C2 and C3 downstream effects — once codification (C3) occurs, the informal coordination substrate (C4-hypothesized) is destroyed, removing the structural basis for organize_collective_against_datum. This is the mechanism by which C3 forecloses TR3 for X1.</blocked_by>
</transformation_rule>
```

---

## 3. ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X4</agent>
  <constraint>C2</constraint>
  <actual_type>Snare (confirmed at X3 index: χ=0.96)</actual_type>
  <perceived_type>Rope (X4 index: χ=−0.128)</perceived_type>
  <observable>Institutional agent's decisions treat C2 as pure coordination mechanism requiring no remediation; no compensating transfer proposed despite verified χ(powerless)=0.96 exceeding Snare threshold by wide margin</observable>
  <correction_trigger>Cross-index comparison protocol: any agent evaluating C2 must check χ at powerless index before certifying Rope classification. If χ(powerless) > 0.70, Snare status holds regardless of χ(institutional).</correction_trigger>
</error>

<error id="E2">
  <type>Type IV — Rope-as-Snare (Missing Coordination), inverted institutional variant</type>
  <agent>X6</agent>
  <constraint>C3</constraint>
  <actual_type>Snare (confirmed at X5 index: χ=1.08)</actual_type>
  <perceived_type>Rope (X6 index: χ=−0.144)</perceived_type>
  <observable>Codification action proceeds on the premise that formalizing an informal system strictly improves coordination (removing ambiguity, enabling enforcement) without testing χ at the index of the system's original operants (X5); no measurement of whether the informal system already satisfied coordination function pre-codification</observable>
  <correction_trigger>Test: "Does removing constraint C3 eliminate a coordination function?" — applied against the PRE-codification baseline (deferred C4), not the status quo. If C4 already provided coordination without enforcement (Supp near 0), then C3 is pure extraction-addition, not coordination-creation, and Type IV inversion is confirmed.</correction_trigger>
</error>

<error id="E3">
  <type>Type I — False Mountain</type>
  <agent>X1</agent>
  <constraint>C1</constraint>
  <actual_type>Tangled Rope (constructed, Boltzmann FAIL, χ=0.60)</actual_type>
  <perceived_type>Mountain (naturalized as given fact, not constructed choice)</perceived_type>
  <observable>Agent's index remains fixed at (moderate, generational, identity_locked, local) across observed time horizon; no attempt to query origin of the positional datum despite C1 feeding directly into measurable downstream asymmetry (C2, C3); classification does not vary by Power×Scope test because agent never runs the test</observable>
  <correction_trigger>Boltzmann Independence Test applied by agent itself: computing Type(C1, I) at varied P reveals magnitude shift (0.60→0.69) without agent needing external intervention — the test is available at biographical time horizon but requires exit-option upgrade from identity_locked to analytical (see TR1)</correction_trigger>
</error>

<error id="E4">
  <type>Type V.c — Tangled Rope Mishandling: Wrong Reform Strategy</type>
  <agent>X6</agent>
  <constraint>C2</constraint>
  <actual_type>Snare (X3 index) / Rope (X4 index) — irreducibly index-variant, functioning as Tangled Rope at network level given Coord=true, Asym=true</actual_type>
  <perceived_type>Treated as suitable for direct procedural reform (codification) rather than requiring structural renegotiation of the underlying datum (C1)</perceived_type>
  <observable>Reform action (C3) targets the visible mechanism (C2's height-based rights) without addressing the upstream naturalized datum (C1); this is surgical reform attempted on a constraint whose purity is already compromised by downstream entrenchment — reform strategy mismatched to degradation state</observable>
  <correction_trigger>Purity assessment prior to reform: if C1 (upstream) remains uncorrected, any reform of C2 will re-inherit C1's asymmetry. Correction requires addressing generation_order 1 before generation_order 2 — reform sequence must match causal sequence.</correction_trigger>
</error>
```

---

## 4. INSTITUTIONAL RATIONALITY MODEL

```
Model selected: BOUNDED INSTITUTIONAL RATIONALITY (BIR)

Justification:
- X6 (Agent_F) exhibits satisficing behavior under uncertainty: codification (C3)
  is undertaken as a good-faith reform action, not utility-maximizing exploitation.
  Negative χ at X6's index (−0.144) reflects genuine belief in Pareto-improvement,
  not calculated extraction — this is the signature of bounded rationality (principal-
  agent misalignment between institutional intent and field-level effect).
- Suppression values (C1=0.20, C2=0.70, C3=0.90) escalate across generation_order,
  indicating increasing enforcement cost — consistent with an institution satisficing
  toward legibility rather than optimally designing for minimal extraction.
- Coordination=true holds for C2 and C3 despite high extraction, which is the
  Tangled Rope/Snare hybrid signature BIR produces (principal-agent problems
  generating unintended asymmetric costs from nominally coordination-seeking acts).
- PIR is rejected: PIR assumes utility-maximizing agents with no negotiation except
  Pareto-improving moves. X6's action (C3) is explicitly framed in Stage 0 as
  "well-intentioned" — inconsistent with PIR's implacable-optimizer profile, and
  more consistent with BIR's satisficing-under-uncertainty profile that produces
  tragic outcomes through error rather than design.

Attractor compatibility under BIR (per Compatibility Matrix):
  Tangled Ropes dominant + BIR → Negotiated Equilibrium (reachable but NOT selected — see §5)
  Pure Snares + BIR → Equilibrium or Rupture (partially reachable, C2/C3 agent-dependent)
```

---

## 5. TERMINAL ATTRACTOR SELECTION

```
Selected: DETERMINISTIC TRAGEDY

Constraint profile justification:
- Generation sequence C1 → C2 → C3 shows monotonic increase in ε (0.75→0.80→0.90)
  and Supp (0.20→0.70→0.90), with terminal constraint C3 reaching Snare classification
  at its most-exposed index (X5, χ=1.08) — the highest extraction value in the network.
- TR2 (codify_informal_system) is shown as BLOCKED_BY: "nothing internal to the
  constraint network" — the transformation from C2 to C3 is structurally unblocked
  and fires deterministically once suppression threshold conditions are met.
- TR3 (organize_collective_against_datum) — the network's only reform-directed
  transformation — is explicitly blocked by C3's downstream effects (destruction of
  the coordination substrate required for collective organization). This closes the
  Negotiated Equilibrium path that BIR would otherwise make reachable.
- Break contract confirms: original_break status LIVE, target_prior is the belief
  that "parameter adjustment" (reform of C2) can fix the system without addressing
  the zero-point (C1). The manifest's own break_contract structure specifies that
  this belief is being falsified, not confirmed — consistent with Tragedy, not
  Equilibrium.

Note on BIR/Tragedy compatibility: The Compatibility Matrix lists BIR as N/A for
"Mountain + Snares" and "Pure Snares, no Mountains" rows under strict PIR-only
Tragedy — however, Tangled-Ropes-dominant profiles under BIR are listed as tending
toward Negotiated Equilibrium. Selection of Deterministic Tragedy here is justified
as an EXCEPTION requiring explicit note: the network's terminal constraint (C3) has
already transitioned OUT of the Tangled Rope band into confirmed Snare (χ=1.08) at
its most-exposed index, with the reform-path transformation (TR3) structurally
foreclosed by the same transition. This represents BIR producing tragic outcome
through good-faith error compounding across generation_order, rather than through
implacable optimization (PIR) — a BIR-Tragedy pathway distinct from PIR-Tragedy,
proceeding via cascading satisficing failures rather than terminal utility-maximization.

Compatible: YES, under the amended reading that BIR reaches Tragedy when the
error-correction transformation (TR3/reform path) is foreclosed before equilibrium
can be negotiated — i.e., Equilibrium was reachable in principle (BIR compatible)
but was foreclosed in sequence by the generation_order itself (C1→C2→C3 outpacing
any TR3 trigger window).
```

---

## 6. OMEGA PRESERVATION

```
Ω_1 (motivation_of_surveyor): PRESERVED, UNRESOLVED.
No agent index or transformation rule in this formalization resolves whether C1's
origin (the datum choice feeding X4/G1 benefit) was Coord=false due to malice or
Coord=false due to arbitrary convenience. Both C1.rationale entries above are
written to remain compatible with either resolution — the χ calculations are
identical under both hypotheses, since ε(C1)=0.75 is stipulated independent of
motive. This is intentional: the formal network does not require resolving Ω_1
to compute any downstream χ value.
```

---

## 7. VALIDATION CHECKLIST

```
☑ All constraints (C1, C2, C3) formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with explicit π and σ values for every agent
☑ All characters use variable names (X1–X6) — variable mapping isolated to header
☑ No source occupation, setting, or domain vocabulary in body (heights/water/
  measurement described only in structural/formal terms: "positional datum,"
  "codification," "elevated positional cost")
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules (TR1, TR2, TR3) are IF-THEN with mechanical χ recalculation
☑ Error types (E1–E4) mapped to taxonomy (III, IV, I, V.c) with testable observables
  and correction triggers
☑ Institutional rationality model specified: BIR, justified via negative-χ
  good-faith reform behavior at X6
☑ Terminal attractor selected: Deterministic Tragedy, justified via TR3 foreclosure
  and generation_order monotonicity; compatibility exception explicitly argued
☑ Indexical variance explicitly preserved: C1 (magnitude-only), C2 (maximal,
  sign-inverting), C3 (binary, largest Δχ in network)
☑ Boltzmann test run for all three constraints (none claimed as Mountain; all
  FAIL confirmed, consistent with constructed-not-natural status)
☑ Ω_1 preserved unresolved; formalization shown independent of its resolution
☐ Source work unidentifiable from output — PENDING final anonymization pass
  (header mapping table must be stripped before downstream release)
```

<!-- ANONYMIZATION: 6 character names replaced with structural labels: Agent_A, Agent_B, Agent_C, Agent_D, Agent_E, Agent_F -->
