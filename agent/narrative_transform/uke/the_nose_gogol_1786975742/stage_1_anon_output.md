# STAGE 1: FORMAL CONSTRAINT NETWORK

## Variable Mapping (header only — stripped after this point)

```
C₁ ← Baseline_Physical_Form
C₂ ← Rank-as-Identity
C₃ ← Inadmissible_Phenomena
C₄ ← Low-Status_Criminality_Default [deferred]
```

---

## 1. Constraint Formalizations

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.10</epsilon>
    <suppression>0.00</suppression>
    <coordination>true</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>moderate</power>          <!-- π = 1.0 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.0 × 0.9 = 0.09</chi>
    <type>Mountain</type>
    <threshold_check>ε ≤ 0.25 (0.10 ✓), Supp ≤ 0.05 (0.00 ✓) → PASS</threshold_check>
    <rationale>Zero suppression, minimal extraction, naturally emergent property with no constructed enforcement mechanism. Immutable at immediate time horizon with trapped exit.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 χ=0.09 → Mountain.
    No divergence tested at other indices — classification is declared invariant by construction (biological baseline for all agents in the network).
  </indexical_variance>

  <boltzmann_test>
    PASS. Reasoning: ε and Supp values do not vary by P or S — the constraint is a substrate condition, not agent-differentiated. Test procedure requires computing Type(C1, I) across varying P × S; because C1 functions as background condition rather than agent-specific burden, it factors independently of both dimensions. Candidate Mountain confirmed.
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

  <agent ref="X1">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>immediate</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.0 × 0.9 = 0.72</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.35 &lt; χ ≤ 0.90 (0.72 ✓), ε ≥ 0.30 (0.80 ✓), Supp ≥ 0.40 (0.70 ✓) → PASS</threshold_check>
    <rationale>Moderate power position with identity-locked exit produces hybrid classification: constraint provides genuine coordination (status legibility function) while extracting asymmetric cost. Active enforcement required (Coord=true, Asym=true satisfied).</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerful</power>           <!-- π = 0.6 -->
      <time>immediate</time>
      <exit>mobile</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 0.6 × 0.9 = 0.432</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.35 &lt; χ ≤ 0.90 (0.432 ✓), ε ≥ 0.30 (0.80 ✓), Supp ≥ 0.40 (0.70 ✓) → PASS</threshold_check>
    <rationale>Power dampening (π=0.6) reduces effective extraction relative to X1, but does not clear the Rope ceiling (χ ≤ 0.35). Mobile exit does not equate to net-beneficiary status — still bound by the same enforcement mechanism, just at reduced cost.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 (0.96 ✓), ε ≥ 0.46 (0.80 ✓), Supp ≥ 0.60 (0.70 ✓) → PASS</threshold_check>
    <rationale>Powerless position amplifies extraction beyond Tangled Rope ceiling. Trapped exit confirms SnareImmutability — constraint is immutable to this index but perceived as Rope/Tangled Rope from higher-power indices (X2, institutional actors), satisfying the index-sensitivity criterion for Snare classification.</rationale>
  </agent>

  <indexical_variance>
    C2: X1 χ=0.72 → Tangled Rope, X2 χ=0.432 → Tangled Rope, X3 χ=0.96 → Snare.
    Divergence: identical base constraint (ε=0.80, Supp=0.70) produces three distinct effective classifications purely as a function of π scaling. Power position is the dominant differentiator; scope is held constant/near-constant (0.9 vs 0.8) and contributes secondarily. This is the canonical high-index-sensitivity profile — same coordination-enforcement mechanism functions as extraction trap for the powerless index and hybrid coordination-cost structure for moderate/powerful indices.
  </indexical_variance>

  <boltzmann_test>
    FAIL (as expected — not claiming Mountain status). Type varies non-factorizably with P (moderate→Tangled Rope, powerless→Snare, powerful→Tangled Rope at lower χ). Confirms C2 is constructed, not natural — consistent with high ε/Supp values already indicating human-built enforcement architecture.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>0.60</epsilon>
    <suppression>0.60</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>regional</scope>            <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.60 × 1.5 × 0.9 = 0.81</chi>
    <type>Snare</type>
    <threshold_check>χ ≥ 0.66 (0.81 ✓), ε ≥ 0.46 (0.60 ✓), Supp ≥ 0.60 (0.60 ✓) → PASS</threshold_check>
    <rationale>Downstream constraint inherits powerless positioning from C2 interaction. Agent experiencing a C1-violation event that C3's admissibility filter cannot process is structurally re-scored as powerless with no scope for appeal — constraint enforces silence/non-recognition of the anomaly with high suppression.</rationale>
  </agent>

  <agent ref="X4">
    <index>
      <power>institutional</power>       <!-- π = −0.2 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.60 × (−0.2) × 0.9 = −0.108</chi>
    <type>Rope</type>
    <threshold_check>χ ≤ 0.35 (−0.108 ✓), χ ≤ 0 → ε ceiling bypassed → PASS</threshold_check>
    <rationale>Institutional power position inverts extraction sign — agent is net beneficiary of the admissibility filter, which protects institutional processing function from anomalous inputs. Negative χ bypasses ε ceiling per Rope dual-threshold rule.</rationale>
  </agent>

  <indexical_variance>
    C3: X1 χ=0.81 → Snare, X4 χ=−0.108 → Rope.
    Divergence: institutional power position (π=−0.2) does not merely dampen extraction but inverts it, producing negative χ. The identical admissibility rule is experienced as a trap by the agent whose reality-claim is being filtered out, and as protective coordination infrastructure by the agent whose institutional function the filter defends. This is a sign-flip divergence, structurally distinct from C2's magnitude-only divergence.
  </indexical_variance>

  <boltzmann_test>
    FAIL (not claiming Mountain status). Type flips entirely with P (powerless→Snare, institutional→Rope) at near-constant S. Confirms constructed status — the filter is an institutional artifact, downstream of C1 (natural) and C2 (constructed hierarchy), not an independent natural law.
  </boltzmann_test>
</constraint>
```

---

## 2. Transformation Rules

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>agent_attempts_alternative_verification_channel</condition>
    <target>C3</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = powerless (π=1.5), E = constrained</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.60 × 1.5 × 0.9 = 0.81 → Snare</before>
    <after>χ = 0.60 × 1.5 × 0.9 = 0.81 → Snare (unchanged; E shift alone does not alter χ)</after>
  </chi_recalculation>
  <type_change>Snare → Snare (no change)</type_change>
  <preconditions>Agent must locate an institutional actor whose π is closer to neutral than X4's institutional-negative value; simple exit-option upgrade from trapped to constrained is insufficient without a P-position change.</preconditions>
  <blocked_by>C2 (identity-lock prevents the agent from being processed as a legible claimant independent of rank); C3's own high suppression (0.60) actively resists alternative-channel attempts.</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>rank_marker_reacquired_independent_of_original_bearer</condition>
    <target>C2</target>
    <agent>X1</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), E = identity_locked</from>
    <to>P = moderate (π=1.0), E = mobile</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.80 × 1.0 × 0.9 = 0.72 → Tangled Rope</before>
    <after>χ = 0.80 × 1.0 × 0.9 = 0.72 → Tangled Rope (χ unchanged; type persists, only E shifts)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope (no type change; precondition for eventual T1/T2 lifecycle shift not yet met)</type_change>
  <preconditions>Reacquisition event must be independently verifiable through C3's admissibility filter — creating direct dependency on TR1 succeeding first.</preconditions>
  <blocked_by>C3 (Snare-classified from X1's index — filter blocks the verification event required to complete reacquisition).</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>institutional_actor_reclassifies_anomalous_claim_as_admissible</condition>
    <target>C3</target>
    <agent>X4</agent>
  </trigger>
  <index_change>
    <from>P = institutional (π=−0.2), E = mobile</from>
    <to>P = institutional (π=−0.2), E = arbitrage</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.60 × (−0.2) × 0.9 = −0.108 → Rope</before>
    <after>χ = 0.60 × (−0.2) × 0.9 = −0.108 → Rope (X4's own χ unaffected by the reclassification action — this is an other-agent-affecting move)</after>
  </chi_recalculation>
  <type_change>No change to X4's type; downstream effect targets X1's classification of C3.</type_change>
  <preconditions>Requires X4 to accept processing cost against institutional self-interest (violates PIR; requires BIR satisficing behavior — see §3).</preconditions>
  <blocked_by>C3's Coord=true property itself — the admissibility filter's entire function is to prevent exactly this kind of exception, creating structural resistance from within X4's own institutional role.</blocked_by>
</transformation_rule>
```

---

## 3. Error Manifestations

```xml
<error id="E1">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X2</agent>
  <constraint>C2</constraint>
  <actual_type>Tangled Rope (χ=0.432, from X2's own index)</actual_type>
  <perceived_type>Rope (extraction not felt at powerful π=0.6)</perceived_type>
  <observable>Agent at powerful index does not register enforcement cost imposed on powerless-index agents subject to the same nominal rule; treats system purely as functional coordination without accounting for asymmetric downstream burden.</observable>
  <correction_trigger>Compute χ at powerless index (X3: χ=0.96 ≥ 0.66) — this reveals Snare exists regardless of χ(powerful), per Type III test procedure. Correction requires cross-index comparison, not introspection at single index.</correction_trigger>
</error>

<error id="E2">
  <type>Type I: False Mountain</type>
  <agent>X1</agent>
  <constraint>C2</constraint>
  <actual_type>Tangled Rope (χ=0.72, changeable via TR2 given precondition satisfaction)</actual_type>
  <perceived_type>Mountain (treated as unchangeable fact of reality rather than constructed hierarchy)</perceived_type>
  <observable>Agent does not attempt reacquisition or alternative-legibility strategies despite χ indicating a Tangled Rope (changeable-in-principle) rather than Mountain classification; behavior consistent with identity_locked exit rather than mere trapped exit.</observable>
  <correction_trigger>Test classification variance across P × S. Since C2 demonstrably varies by P (X1: Tangled Rope, X2: Tangled Rope-lower-χ, X3: Snare), it fails Boltzmann and cannot be Mountain — correction requires agent to recognize identity_locked exit as a cognitive/structural fusion, not physical impossibility.</correction_trigger>
</error>

<error id="E3">
  <type>Type V.b: Tangled-as-Snare (ignoring coordination component)</type>
  <agent>X1</agent>
  <constraint>C2</constraint>
  <actual_type>Tangled Rope (Coord=true, genuine legibility function exists alongside extraction)</actual_type>
  <perceived_type>Pure Snare (coordination function invisible to agent under acute crisis conditions)</perceived_type>
  <observable>Agent pursues wholesale rejection/destruction-oriented responses rather than reform-oriented responses (e.g., does not attempt to negotiate within the system or exploit its legibility function) — consistent with treating χ=0.72 constraint as if it had zero coordination value.</observable>
  <correction_trigger>Verify Coord=true property directly: removing C2 entirely would eliminate a functioning (if extractive) status-legibility mechanism relied on by multiple agents (X1, X2) for coordination purposes — confirms Tangled, not pure Snare, and implies surgical-reform strategy over rupture strategy per Type V.c guidance.</correction_trigger>
</error>

<error id="E4">
  <type>Type IV: Rope-as-Snare (Missing Coordination)</type>
  <agent>X4</agent>
  <constraint>C3</constraint>
  <actual_type>Rope (χ=−0.108, from X4's institutional index)</actual_type>
  <perceived_type>N/A — flagged as absence: X4 does not misclassify but the system's design invites external agents (e.g., X1) to misread C3's function</perceived_type>
  <observable>External-facing test: does removing C3 eliminate a coordination function? Yes — an unfiltered admissibility system would flood the institutional processing capacity referenced by X4, indicating genuine (if asymmetrically distributed) coordination value.</observable>
  <correction_trigger>This error is recorded as a structural risk rather than an observed agent failure — included to confirm C3 is correctly Rope-classified from X4's index and not vulnerable to Type IV misreading from within the network.</correction_trigger>
</error>
```

---

## 4. Institutional Rationality Model

```
Selected: Bounded Institutional Rationality (BIR)

Justification:
X4's index (institutional, biographical, mobile, regional) with χ=−0.108 indicates
a satisficing actor operating a risk-averse admissibility filter — not a utility-maximizing
implacable system. C3's Supp=0.60 reflects active, effortful enforcement (principal-agent
dynamics: X4 protects institutional function from reputational/processing risk) rather than
zero-cost mechanical exclusion. TR3's precondition (institutional actor accepting processing
cost against self-interest) is only reachable under BIR — PIR would treat any Pareto-negative
move for the institutional agent as strictly prohibited, foreclosing TR3 entirely.

C2's dual-classification profile (Tangled Rope for X1/X2, Snare for X3) is also
BIR-compatible: a PIR system would not tolerate the coordination inefficiencies embedded
in a hierarchy that produces such divergent effective extraction — BIR's satisficing
tolerance explains why the system persists in hybrid form rather than optimizing to
pure extraction or pure coordination.
```

---

## 5. Terminal Attractor Selection

```
Selected: Seeded Possibility

Constraint profile: Mountain (C1) + Tangled Ropes/Snare hybrid (C2, C3) — mixed profile
with no pure-Snare-dominant or pure-Mountain-dominant reading.

Compatibility check against Attractor Compatibility Matrix (§VII):
- Mountain-dominated → Tragedy, Seeded Possibility ✓ (C1 present as upstream Mountain)
- Tangled Ropes dominant + BIR → Negotiated Equilibrium (available but not selected —
  no agent in the network reaches full negotiated resolution; X1 remains identity_locked
  at network closure)
- Piton present → Seeded Possibility (C4, deferred, hypothesized Snare with offstage
  Piton-adjacent function, supports this reading structurally even off-stage)

Justification: The network shows surface-level non-resolution (C2 remains Tangled Rope/
Snare across all tested agents at closure; TR1/TR2 chain remains blocked by C3) while
the underlying transformation-rule structure (TR1→TR2 dependency chain) demonstrates
that reclassification is mechanically available given precondition satisfaction — the
possibility is seeded in the formal structure even though no agent in the observed
network completes the chain. This matches Seeded Possibility rather than Deterministic
Tragedy specifically because C3's classification is BIR-satisficing (X4, Rope) rather
than PIR-implacable, leaving the door mechanically open rather than formally sealed.
```

---

## 6. Validation Checklist

```
☑ All Stage 0 constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X1–X4) — no source identifiers in body
☑ Variable mapping table present in header only
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields
☑ Transformation rules use abstract trigger descriptions
☑ Error observables are testable structural conditions
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Seeded Possibility), justified, compatible
☑ Indexical variance explicitly preserved across agents (C2: magnitude divergence;
   C3: sign-flip divergence)
☑ Boltzmann test run for each constraint claiming Mountain status (C1: PASS;
   C2, C3: FAIL as expected, not claimed)
☑ No ambiguity in specifications
☑ Output is pure symbolic network
```

<!-- ANONYMIZATION: 4 character names replaced with structural labels: Agent_A, Agent_B, Agent_C, Agent_D -->
