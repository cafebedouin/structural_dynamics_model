# STAGE 1: FORMALIZATION OUTPUT

## Variable Mapping (Header Only — Stripped Post-Anonymization)

```
X₁ ← Anna
X₂ ← Rina
X₃ ← Kwan
C₁ ← Biological_Imperative
C₂ ← Dyadic_Coordination
C₃ ← Gamified_Compliance
C₄ ← Constant_Monitoring [deferred]
C₅ ← Physical_Confinement [deferred]
```

---

## 1. CONSTRAINT FORMALIZATIONS

```xml
<constraint id="C1">
  <properties>
    <epsilon>0.10</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerless</power>       <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>N/A — Mountain classification bypasses χ gate</chi>
    <type>Mountain</type>
    <threshold_check>ε=0.10 ≤ 0.25 PASS; Supp=0.00 ≤ 0.05 PASS</threshold_check>
    <rationale>Zero-suppression, minimal-extraction constraint with no constructed enforcement mechanism. Naturally emergent condition of the agent's biological substrate.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerless</power>
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>
    </index>
    <chi>N/A</chi>
    <type>Mountain</type>
    <threshold_check>ε=0.10 ≤ 0.25 PASS; Supp=0.00 ≤ 0.05 PASS</threshold_check>
    <rationale>Identical structural exposure to X1. No power-differential effect on classification.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>moderate</power>
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>
    </index>
    <chi>N/A</chi>
    <type>Mountain</type>
    <threshold_check>ε=0.10 ≤ 0.25 PASS; Supp=0.00 ≤ 0.05 PASS</threshold_check>
    <rationale>Despite elevated power position relative to X1/X2, base properties remain invariant — confirms non-factorizable dependency on P is absent.</rationale>
  </agent>

  <indexical_variance>
    C1: X1 → Mountain, X2 → Mountain, X3 → Mountain
    No divergence. Classification is invariant across power positions, consistent with natural-law status.
  </indexical_variance>

  <boltzmann_test>
    PASS. Type(C1, I) = Mountain for P ∈ {powerless, moderate} and S = local (only tested scope in corpus). No burden variance detected across power positions. Classification does not factorize with P or S — confirms candidate Mountain status structurally, not merely by low ε/Supp.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C2">
  <properties>
    <epsilon>0.05</epsilon>
    <suppression>0.00</suppression>
    <coordination>true</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X2">
    <index>
      <power>organized</power>       <!-- π = 0.4 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.05 × 0.4 × 0.8 = 0.016 ≈ 0.02</chi>
    <type>Rope</type>
    <threshold_check>χ=0.02 ≤ 0.35 PASS (dual threshold ε=0.05 ≤ 0.45 also PASS)</threshold_check>
    <rationale>Minimal power-scaled extraction; coordination function dominant. Exit is structurally available but foreclosed by identity fusion — does not affect χ, only affects agent's willingness to invoke exit.</rationale>
  </agent>

  <agent ref="X1">
    <index>
      <power>organized</power>
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>
    </index>
    <chi>ε × π × σ = 0.05 × 0.4 × 0.8 = 0.016 ≈ 0.02</chi>
    <type>Rope</type>
    <threshold_check>χ=0.02 ≤ 0.35 PASS; ε=0.05 ≤ 0.45 PASS</threshold_check>
    <rationale>Symmetric dyadic position — identical index to X2 by construction of the coordination structure.</rationale>
  </agent>

  <indexical_variance>
    C2: X1 χ=0.02 → Rope, X2 χ=0.02 → Rope
    No divergence between the two internal participants. This constraint's index is undefined for any agent outside the dyad (X3 has no index for C2 — structurally inaccessible, not merely unmeasured).
  </indexical_variance>

  <boltzmann_test>
    NOT APPLICABLE — Coord(C2)=true and constraint does not claim Mountain status. Test reserved for Mountain candidates only.
  </boltzmann_test>
</constraint>
```

```xml
<constraint id="C3">
  <properties>
    <epsilon>0.80</epsilon>
    <suppression>0.80</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X1">
    <index>
      <power>powerless</power>       <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>χ=0.96 > 0.70 PASS (Snare gate); LowBaseExtraction=false (ε=0.80 high) confirms not false-positive from power amplification</threshold_check>
    <rationale>Maximal power-scaled extraction under high suppression and asymmetric cost distribution. No coordination benefit accrues to this index despite Coord(C3)=true at system level.</rationale>
  </agent>

  <agent ref="X2">
    <index>
      <power>powerless</power>
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>χ=0.96 > 0.70 PASS</threshold_check>
    <rationale>Structurally identical exposure to X1 under the baseline index.</rationale>
  </agent>

  <agent ref="X2_prime">
    <index>
      <power>analytical</power>      <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.15 × 0.8 = 0.736 ≈ 0.74</chi>
    <type>Snare</type>
    <threshold_check>χ=0.74 > 0.70 PASS (marginal — closest agent to Tangled Rope boundary)</threshold_check>
    <rationale>Post-transition index reflecting acquired analytical capacity following an exit event within C2 (see TR2). Power position shifts from powerless to analytical; exit shifts from trapped to identity_locked. χ decreases from 0.96 to 0.74 but remains above the Snare threshold — degeneracy-breaking modifier (π=1.15) confirms continued extraction-detection rather than false normalization to Tangled Rope.</rationale>
  </agent>

  <agent ref="X3">
    <index>
      <power>moderate</power>        <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>            <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.0 × 0.8 = 0.64</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ χ=0.64 ≤ 0.70 PASS; Coord(C3)=true PASS; Asym(C3)=true PASS</threshold_check>
    <rationale>Moderate power-scaled extraction combined with confirmed coordination value and asymmetric cost distribution. Hybrid classification — administrative position absorbs less extraction than subject positions but is not a net beneficiary (π ≠ institutional/negative).</rationale>
  </agent>

  <indexical_variance>
    C3: X1 χ=0.96 → Snare, X2 χ=0.96 → Snare, X2_prime χ=0.74 → Snare, X3 χ=0.64 → Tangled Rope
    Strong divergence confirmed. Identical structural mechanism (same ε, Supp, Coord, Asym base values) produces Snare classification for powerless/trapped indices and Tangled Rope for moderate/constrained index. This is the canonical index-sensitivity signature per §IV: "same constraint: Snare (powerless), Tangled (moderate)."
  </indexical_variance>

  <boltzmann_test>
    FAIL (as expected — C3 does not claim Mountain status). Classification varies non-factorizably with P (Snare at powerless/analytical, Tangled Rope at moderate) while S is held constant at local. This confirms C3 is constructed, not natural — consistent with high Supp=0.80 and Coord=true properties.
  </boltzmann_test>
</constraint>
```

### Deferred Constraints (Formalized as Hypotheses Only)

```xml
<constraint id="C4" status="deferred">
  <properties>
    <epsilon>unknown</epsilon>
    <suppression>hypothesized_high</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>
  <hypothesis>Tangled Rope</hypothesis>
  <function_note>Infrastructural enabler of C3 — increases effective Supp(C3) by eliminating concealment options. Not independently classified pending Stage 2 activation.</function_note>
</constraint>

<constraint id="C5" status="deferred">
  <properties>
    <epsilon>low_hypothesized</epsilon>
    <suppression>low_hypothesized</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>
  <hypothesis>Mountain</hypothesis>
  <function_note>Background pressure fixing E=trapped/constrained for all agents across C1–C3. Not independently classified pending Stage 2 activation.</function_note>
</constraint>
```

---

## 2. TRANSFORMATION RULES

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>establish_dyadic_coordination</condition>
    <target>C2</target>
    <agent>X1, X2</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = identity_locked</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.05 × 1.5 × 0.8 = 0.06 → Rope (hypothetical pre-formation index)</before>
    <after>χ = 0.05 × 0.4 × 0.8 = 0.02 → Rope</after>
  </chi_recalculation>
  <type_change>Rope → Rope (no type change; magnitude of χ decreases within same classification band)</type_change>
  <preconditions>Mutual recognition event between X1 and X2; C1 must be active (generation_order dependency)</preconditions>
  <blocked_by>None — C2 has no downstream dependency on C3 activation</blocked_by>
</transformation_rule>

<transformation_rule id="TR2">
  <trigger>
    <condition>exit_constraint (unilateral withdrawal from dyadic coordination)</condition>
    <target>C2</target>
    <agent>X2</agent>
  </trigger>
  <index_change>
    <from>P = organized (π=0.4), E = identity_locked [within C2]</from>
    <to>P = analytical (π=1.15), E = identity_locked [residual, now indexed to C3]</to>
  </index_change>
  <chi_recalculation>
    <before>χ(C3) = 0.80 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ(C3) = 0.80 × 1.15 × 0.8 = 0.74 → Snare</after>
  </chi_recalculation>
  <type_change>Snare → Snare (magnitude reduction only; degeneracy-breaking modifier prevents misclassification as Tangled Rope despite reduced χ)</type_change>
  <preconditions>C2 must be active with χ ≤ 0.35 (Rope-status) prior to withdrawal; withdrawal removes agent from C2's index space entirely</preconditions>
  <blocked_by>C1 (Mountain status of C1 constrains available exit options — withdrawal from C2 does not grant exit from C3)</blocked_by>
</transformation_rule>

<transformation_rule id="TR3">
  <trigger>
    <condition>increase_enforcement_infrastructure</condition>
    <target>C3</target>
    <agent>X3</agent>
  </trigger>
  <index_change>
    <from>P = moderate (π=1.0), E = constrained</from>
    <to>P = moderate (π=1.0), E = constrained [Supp(C3) increases via C4 activation]</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.80 × 1.0 × 0.8 = 0.64 → Tangled Rope</before>
    <after>χ = 0.80 × 1.0 × 0.8 = 0.64 → Tangled Rope (χ formula unaffected by Supp directly; classification stability confirmed unless ε shifts)</after>
  </chi_recalculation>
  <type_change>Tangled Rope → Tangled Rope (no change; note that C4 activation would need to raise ε(C3), not just Supp(C3), to shift χ and cross into Snare band for X3)</type_change>
  <preconditions>C4 transitions from deferred to active status</preconditions>
  <blocked_by>Nothing structurally — flagged for Stage 2 narrative decision on whether C4 activation is depicted</blocked_by>
</transformation_rule>

<transformation_rule id="TR4">
  <trigger>
    <condition>organize_collective (hypothetical, tested for feasibility)</condition>
    <target>C3</target>
    <agent>X1, X2</agent>
  </trigger>
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = constrained [hypothetical]</to>
  </index_change>
  <chi_recalculation>
    <before>χ = 0.80 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.80 × 0.4 × 0.8 = 0.256 → Rope (below 0.35 ceiling; but ε=0.80 > 0.45 dual-threshold FAILS)</after>
  </chi_recalculation>
  <type_change>Snare → Naturalized (ε=0.80 > 0.45 ∧ χ=0.256 < 0.40 — high base extraction absorbed by hypothetical power shift, flagged for investigation rather than clean Rope reclassification)</type_change>
  <preconditions>C5 (Mountain, deferred) must permit reduced E-value; untested whether physical confinement structurally forecloses this transformation entirely</preconditions>
  <blocked_by>C5 (hypothesized Mountain) — if C5's immutability extends to blocking collective reorganization, this transformation is unreachable regardless of χ mathematics</blocked_by>
</transformation_rule>
```

---

## 3. ERROR MANIFESTATIONS

```xml
<error id="E1">
  <type>Type V.b — Tangled Rope Mishandling (Tangled-as-Snare)</type>
  <agent>X3</agent>
  <constraint>C3</constraint>
  <actual_type>Tangled Rope (χ=0.64)</actual_type>
  <perceived_type>Snare (if X3 or an external observer collapses the hybrid into pure extraction)</perceived_type>
  <observable>Agent takes actions consistent with total-extraction assumption (e.g., ceases all administrative discretion) despite χ indicating a genuine coordination component remains active at this index.</observable>
  <correction_trigger>Verification that removing C3 entirely would eliminate a coordination function currently benefiting X3's operational position — if confirmed, Tangled Rope classification holds and pure-Snare treatment is an error.</correction_trigger>
</error>

<error id="E2">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X2_prime</agent>
  <constraint>C3</constraint>
  <actual_type>Snare (χ=0.74 at analytical index)</actual_type>
  <perceived_type>Tangled Rope or Rope (if analytical distance is misread as reduced extraction rather than clearer perception of it)</perceived_type>
  <observable>Agent's post-transition analytical capacity produces recognition of extraction magnitude rather than normalization; observable as continued classification of C3 as high-extraction despite reduced χ relative to pre-transition baseline (0.96 → 0.74).</observable>
  <correction_trigger>Check χ at powerless index (X1, X2 baseline = 0.96) — confirms Snare exists structurally regardless of the analytical agent's reduced-but-still-high χ. Degeneracy-breaking modifier (π=1.15) is precisely the mechanism preventing this error.</correction_trigger>
</error>

<error id="E3">
  <type>Type II — Mountain Denial</type>
  <agent>X1, X2</agent>
  <constraint>C1</constraint>
  <actual_type>Mountain (Boltzmann PASS)</actual_type>
  <perceived_type>Changeable constraint (if agent expends resources attempting to alter C1 directly)</perceived_type>
  <observable>Does not attempt direct modification of C1 despite proximate pressure from C3 — tests whether agent correctly routes energy toward C2/C3-level action rather than futile engagement with the upstream Mountain.</observable>
  <correction_trigger>None required for correct behavior — absence of this observable in the base case confirms agents correctly treat C1 as immutable and direct all agentic effort toward C2 formation instead.</correction_trigger>
</error>

<error id="E4">
  <type>Type I — False Mountain</type>
  <agent>X1, X2 (hypothetical misclassification)</agent>
  <constraint>C3</constraint>
  <actual_type>Snare (χ=0.96, constructed, Boltzmann FAIL)</actual_type>
  <perceived_type>Mountain (if agent treats C3 as natural/unchangeable rather than constructed)</perceived_type>
  <observable>Does not attempt collective organization (TR4) despite χ mathematics indicating theoretical feasibility of Rope-band reduction through power-position shift — observable as total absence of coordination-seeking behavior toward C3 specifically (contrasted with successful coordination-seeking observed toward C2).</observable>
  <correction_trigger>Demonstration that χ(C3) varies with P (0.96 at powerless vs. 0.64 at moderate) — non-factorizable P-dependence confirms constructed status, which should trigger reclassification away from Mountain.</correction_trigger>
</error>
```

---

## 4. INSTITUTIONAL RATIONALITY MODEL

```
Selected: BOUNDED INSTITUTIONAL RATIONALITY (BIR)

Justification:
- C3 exhibits Coord=true ∧ Asym=true with agent-level classifications spanning
  Snare (X1, X2, X2_prime) and Tangled Rope (X3) — a hybrid profile inconsistent
  with pure utility-maximization (PIR would predict uniform extraction absent
  any satisficing/bounded behavior at the administrative index).
- X3's Tangled Rope classification (χ=0.64, moderate power) indicates the
  administrative layer operates under principal-agent constraints rather than
  perfect optimization — consistent with BIR's "satisfice under uncertainty."
- The presence of C4 (Constant_Monitoring, hypothesized Tangled Rope) as
  enabling infrastructure rather than direct extraction mechanism suggests
  bounded/delegated enforcement rather than centralized perfect rationality.
- C2's existence and persistence (Rope, χ=0.02, uncontested by C3's
  suppression apparatus for its duration) indicates the system does not
  optimize against all possible resistance vectors — a PIR system would
  detect and extract from C2 given Supp(C3)=0.80. Its survival implies
  bounded monitoring capacity, not omniscient control.

Attractor reachability under BIR: Negotiated Equilibrium, Seeded Possibility
(per Compatibility Matrix — Tangled Ropes dominant + BIR → Negotiated
Equilibrium; Piton-adjacent dynamics via C4/C5 → Seeded Possibility also
reachable).
```

---

## 5. TERMINAL ATTRACTOR SELECTION

```
Selected: ☒ SEEDED POSSIBILITY

Justification:
- Dominant constraint type for the two lowest-power agents (X1, X2) is Snare
  (χ=0.96) under C3, with C1 as an underlying Mountain and C5 (deferred) as
  a reinforcing Mountain — this satisfies "Mountain + Snares" profile.
- However, C2 (Rope, χ=0.02) persists as an uncontested, low-extraction,
  high-coordination-value structure entirely internal to X1/X2, invisible to
  and unreachable by C3's measurement apparatus (per invariant_contract:
  untranslatable_real, marked primary="yes").
- This matches Seeded Possibility precisely: surface-level constraint
  resolution trends toward Deterministic-Tragedy-like outcomes for X1/X2
  under C3's Snare classification, while C2 constitutes an "underground
  transformation" — a value structure the dominant system's
  inherent_instrument (certified scores, public displays) cannot access,
  measure, or extract from.
- The break_contract's target_prior explicitly encodes this: victory is
  redefined as preservation of an internal state (C2) rather than
  reconfiguration of the external constraint (C3) — the defining signature
  of Seeded Possibility over Deterministic Tragedy.
- Compatible with BIR: Piton-adjacent presence (C4 as enabling infrastructure
  that could degrade to pure theater if C3's function ever hollows out) is
  compatible with either rationality model per the Compatibility Matrix,
  removing any conflict with the BIR selection above.

Rejected alternatives:
- Deterministic Tragedy: would require C2 to have no independent persistence
  or thematic centrality — contradicted by selection_reason marking C2 as
  "primary axis of thematic tension."
- Negotiated Equilibrium: would require bargaining between X1/X2 and the
  C3 structure itself — no such bargaining channel exists (Supp=0.80,
  trapped exit); C2 is parallel/orthogonal to C3, not a negotiation with it.
- Revolutionary Rupture: would require disruption of C3's constraint logic
  itself — no transformation rule (TR1–TR4) achieves this; TR4 is explicitly
  gated by unresolved C5 Mountain status and produces Naturalized, not
  Rupture, even under best-case χ mathematics.
```

---

## 6. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X1, X2, X3, X2_prime) — no source identifiers in body
☑ Variable mapping table present in header (for traceability before anonymization)
☑ No source occupation, setting, or domain vocabulary anywhere in output
☑ No <experience>, <dialogue_markers>, or <narrative> fields present
☑ Transformation rules use abstract trigger descriptions (establish_dyadic_coordination, exit_constraint, increase_enforcement_infrastructure, organize_collective)
☑ Error observables are testable structural conditions
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Seeded Possibility), justified, and compatible
☑ Indexical variance explicitly preserved across agents (C3 shows Snare/Tangled Rope split; C1 shows invariant Mountain)
☑ Boltzmann test run for each constraint claiming Mountain status (C1: PASS; C3: FAIL, as expected for non-Mountain claim)
☑ No ambiguity in specifications
☑ Output is pure symbolic network — source work unidentifiable from body text
```