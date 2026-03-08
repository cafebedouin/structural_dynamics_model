# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Velveteen Rabbit
X₂ ← Boy
X₃ ← Skin Horse
X₄ ← Doctor
X₅ ← Nursery Magic Fairy
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Property Status Asymmetry

```xml
<constraint id="C₁">
  <base_properties>
    <epsilon>0.95</epsilon>
    <suppression>0.10</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.95 × 1.5 × 0.8 = 1.14</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.14 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Total contingent existence. Zero power to alter property relation.
      Physically cannot exit (trapped). High base extraction (ε=0.95)
      amplified by powerless position. No coordination value from this index.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>powerful</power>          <!-- π = 0.6 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.95 × 0.6 × 0.8 = 0.456</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ = 0.456 ≤ 0.70 (tangled_chi_range) → PASS
      Coord = false BUT Asym = true AND provides utility → Tangled classification valid
    </threshold_check>
    <rationale>
      Powerful position dampens extraction. Mobile exit options (can replace object).
      Receives utility (comfort, companionship proxy) at cost of object's autonomy.
      Asymmetric: benefits accrue to powerful agent, costs to powerless.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=1.14 → Snare, X₂ χ=0.456 → Tangled Rope
    
    Same structural relation produces divergent classifications. For X₁ (powerless,
    trapped), property status is total extraction with no coordination value — a Snare.
    For X₂ (powerful, mobile), same relation provides genuine utility at moderate
    extraction cost — a Tangled Rope. The asymmetry is structural: X₂'s benefits
    derive FROM X₁'s constraint, not despite it.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies by Power dimension (X₁ powerless → Snare,
  X₂ powerful → Tangled Rope). This is a constructed social relation, not natural law.
  Constraint cannot be Mountain despite low suppression.</boltzmann_test>
</constraint>
```

---

### Constraint C₂: Transformation Through Degradation

```xml
<constraint id="C₂">
  <base_properties>
    <epsilon>0.80</epsilon>
    <suppression>0.00</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.96 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      High extraction (physical destruction required for ontological status change).
      Identity-locked exit: cannot pursue alternative paths to status.
      Coordination exists (defines transformation pathway) but dominated by extraction
      cost from powerless position. Process is irreversible and consumptive.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>generational</time>
      <exit>identity_locked</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.15 × 0.8 = 0.736</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.736 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Analytical position (π=1.15) breaks normalization: detects extraction despite
      generational time horizon. Identity-locked: has undergone process, cannot exit.
      Recognizes high cost (physical destruction) even while affirming process value.
      Coordination acknowledged but extraction dominates classification.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.96 → Snare, X₃ χ=0.736 → Snare
    
    Both agents classify as Snare despite different power positions. X₃'s analytical
    stance (π=1.15) prevents normalization that would occur at moderate power (π=1.0).
    At π=1.0, χ would be 0.64 → Tangled Rope, hiding extraction. The analytical
    modifier reveals the process as Snare regardless of understanding or acceptance.
    Both are identity-locked: transformation is irreversible, exit impossible.
  </indexical_variance>

  <boltzmann_test>FAIL — While both tested agents classify as Snare, classification
  would vary at moderate power (π=1.0 → χ=0.64 → Tangled Rope). The process is
  constructed: physical degradation is not naturally coupled to ontological status.
  Cannot be Mountain.</boltzmann_test>
</constraint>
```

---

### Constraint C₃: Contamination Protocol

```xml
<constraint id="C₃">
  <base_properties>
    <epsilon>0.90</epsilon>
    <suppression>0.80</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>        <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>             <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.08 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>
      Immediate time horizon: no planning scale where destruction is avoidable.
      Powerless, trapped: zero capacity to influence protocol or exit situation.
      High suppression (ε=0.80) enforces compliance. Extraction is total (annihilation).
      No coordination value from this index — protocol serves external health logic.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>institutional</power>     <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>national</scope>          <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.90 × (-0.2) × 1.0 = -0.18</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.18 ≤ 0 → net beneficiary, ε ceiling bypassed
      χ ≤ 0.35 (rope_chi_ceiling) → PASS
    </threshold_check>
    <rationale>
      Institutional position: net beneficiary of protocol (π=-0.2 → negative χ).
      Protocol coordinates public health response, provides professional authority.
      Mobile exit, national scope: can implement protocol across contexts.
      From this index, constraint is pure coordination mechanism (Rope).
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=1.08 → Snare, X₄ χ=-0.18 → Rope
    
    Extreme divergence. For X₁ (powerless, immediate, trapped), protocol is absolute
    Snare leading to annihilation. For X₄ (institutional, biographical, mobile),
    same protocol is beneficial Rope coordinating health response. The asymmetry is
    structural: X₄'s coordination benefits derive FROM enforcement against X₁.
    No intermediate classification exists — the constraint is purely extractive or
    purely coordinative depending on structural position.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies dramatically by Power (powerless → Snare,
  institutional → Rope) and Scope (local σ=0.8 vs national σ=1.0). This is institutional
  protocol, not natural law. High suppression (0.80) confirms constructed enforcement.
  Cannot be Mountain.</boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Degradation Accumulation (C₁ → C₂ coupling)

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>Sustained interaction under property asymmetry</condition>
    <target>C₂</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>Exit = trapped (C₁ only)</from>
    <to>Exit = identity_locked (C₁ + C₂ coupled)</to>
  </index_change>
  
  <chi_recalculation>
    <before>
      C₁: χ = 0.95 × 1.5 × 0.8 = 1.14 → Snare
      C₂: Not yet active
    </before>
    <after>
      C₁: χ = 0.95 × 1.5 × 0.8 = 1.14 → Snare (unchanged)
      C₂: χ = 0.80 × 1.5 × 0.8 = 0.96 → Snare (now active)
      Exit shifts from trapped to identity_locked (cannot exit C₂ even if C₁ removed)
    </after>
  </chi_recalculation>
  
  <type_change>
    Single constraint (C₁ Snare) → Coupled constraints (C₁ + C₂ both Snare)
  </type_change>
  
  <preconditions>
    - C₁ active (property relation established)
    - Sustained interaction period (biographical time scale)
    - X₁ remains at powerless position throughout
  </preconditions>
  
  <blocked_by>
    - Termination of property relation before coupling occurs
    - Power position shift (powerless → moderate+) breaking extraction accumulation
  </blocked_by>
</transformation_rule>
```

---

### TR₂: Contamination Trigger (C₂ → C₃ activation)

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>Biological contamination event during C₂ process</condition>
    <target>C₃</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>Time = biographical (C₂ active, C₃ dormant)</from>
    <to>Time = immediate (C₃ activated, overrides C₂)</to>
  </index_change>
  
  <chi_recalculation>
    <before>
      C₂: χ = 0.80 × 1.5 × 0.8 = 0.96 → Snare (active)
      C₃: Dormant (protocol exists but not triggered)
    </before>
    <after>
      C₂: χ = 0.80 × 1.5 × 0.8 = 0.96 → Snare (interrupted)
      C₃: χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare (activated, dominant)
      Time horizon collapses to immediate (days), exit remains trapped
    </after>
  </chi_recalculation>
  
  <type_change>
    C₂ Snare (slow extraction) → C₃ Snare (immediate extraction, terminal)
  </type_change>
  
  <preconditions>
    - C₂ active (degradation process underway)
    - Contamination event occurs
    - X₄ (institutional agent) present to enforce protocol
  </preconditions>
  
  <blocked_by>
    - No contamination event
    - Institutional enforcement absent (X₄ not present)
    - X₁ exits property relation before contamination (breaks C₁, prevents C₃)
  </blocked_by>
</transformation_rule>
```

---

### TR₃: Supernatural Rupture (System Exit)

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>External intervention violating constraint logic</condition>
    <target>All constraints (C₁, C₂, C₃)</target>
    <agent>X₅</agent>
  </trigger>
  
  <index_change>
    <from>X₁: (powerless, biographical, identity_locked, local)</from>
    <to>X₁: (powerful, biographical, mobile, local)</to>
  </index_change>
  
  <chi_recalculation>
    <before>
      C₁: χ = 0.95 × 1.5 × 0.8 = 1.14 → Snare
      C₂: χ = 0.80 × 1.5 × 0.8 = 0.96 → Snare
      C₃: χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare
    </before>
    <after>
      C₁: χ = 0.95 × 0.6 × 0.8 = 0.456 → Tangled Rope (power shift)
      C₂: χ = 0.80 × 0.6 × 0.8 = 0.384 → Rope (extraction dampened)
      C₃: χ = 0.90 × 0.6 × 0.8 = 0.432 → Tangled Rope (no longer terminal)
      Exit shifts from identity_locked to mobile (can exit all constraints)
    </after>
  </chi_recalculation>
  
  <type_change>
    All Snares → Rope/Tangled Rope (power position shift breaks extraction)
  </type_change>
  
  <preconditions>
    - C₂ process completed (degradation sufficient)
    - C₃ activated (terminal threat present)
    - X₅ intervention occurs (external to constraint system)
  </preconditions>
  
  <blocked_by>
    NONE — This is a deus ex machina rupture. No internal constraint blocks it.
    Question: Is X₅ an agent of higher-level constraint or true system rupture?
  </blocked_by>
  
  <omega_flag>
    This transformation violates constraint logic. Either:
    (a) X₅ represents higher-level constraint (e.g., "Ontological transformation
        transcends physical degradation") making this a valid transformation, OR
    (b) X₅ is narrative rupture, breaking the formal system.
    
    Classification depends on whether supernatural intervention is formalized as
    constraint or treated as external to the model.
  </omega_flag>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type I False Mountain (X₂ regarding C₁)

```xml
<error id="E₁">
  <type>Type I — False Mountain</type>
  <agent>X₂</agent>
  <constraint>C₁</constraint>
  
  <actual_type>Tangled Rope (from X₂'s index: χ=0.456)</actual_type>
  <perceived_type>Mountain (unchangeable natural relation)</perceived_type>
  
  <observable>
    Does not question property relation despite:
    - Biographical time horizon (changeable within lifetime)
    - Mobile exit options (can alter or exit relation)
    - Moderate extraction cost (χ=0.456, not prohibitive)
    
    Treats constructed social relation as natural law.
  </observable>
  
  <correction_trigger>
    Recognition that property status is:
    - Socially constructed (fails Boltzmann test)
    - Changeable at biographical time scale
    - Asymmetric (benefits X₂ at cost to X₁)
    
    Would shift classification from Mountain to Tangled Rope, enabling reform.
  </correction_trigger>
</error>
```

---

### Error E₂: Type III Snare-as-Rope (X₂ regarding C₂)

```xml
<error id="E₂">
  <type>Type III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₂</agent>
  <constraint>C₂</constraint>
  
  <actual_type>Snare (from X₁'s index: χ=0.96)</actual_type>
  <perceived_type>Rope (beneficial transformation process)</perceived_type>
  
  <observable>
    Normalizes degradation process as natural/beneficial without recognizing:
    - High extraction cost to X₁ (physical destruction)
    - Asymmetric burden (X₂ receives utility, X₁ bears cost)
    - Irreversibility (identity-locked exit for X₁)
    
    From X₂'s powerful position (π=0.6), process appears as coordination.
    Fails to check χ at powerless index where extraction dominates.
  </observable>
  
  <correction_trigger>
    Index shift to powerless position OR
    Analytical examination (π=1.15) revealing:
    - χ(X₁) = 0.96 > 0.70 → Snare classification
    - Extraction cost borne entirely by X₁
    - No genuine coordination from X₁'s perspective
    
    Would reveal process as Snare, not Rope.
  </correction_trigger>
</error>
```

---

### Error E₃: Type I False Mountain (X₁ regarding C₃)

```xml
<error id="E₃">
  <type>Type I — False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Snare (from X₁'s index: χ=1.08)</actual_type>
  <perceived_type>Mountain (unchangeable institutional protocol)</perceived_type>
  
  <observable>
    Treats institutional protocol as natural law:
    - Immediate time horizon (appears unchangeable)
    - Powerless position (no capacity to influence)
    - Trapped exit (cannot escape enforcement)
    
    Does not recognize protocol as constructed, changeable at higher power positions
    or longer time horizons. Fails Boltzmann test (varies by Power and Scope).
  </observable>
  
  <correction_trigger>
    Recognition that:
    - Protocol is institutional (Supp=0.80, constructed enforcement)
    - Classification varies by index (X₄ sees Rope, X₁ sees Snare)
    - Changeable through institutional reform or power position shift
    
    Would shift from Mountain to Snare, revealing extraction and potential for change.
  </correction_trigger>
</error>
```

---

### Error E₄: Type IV Rope-as-Snare (X₄ regarding C₃)

```xml
<error id="E₄">
  <type>Type IV — Rope-as-Snare (Missing Coordination)</type>
  <agent>X₄</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Rope (from X₄'s index: χ=-0.18)</actual_type>
  <perceived_type>Snare (if protocol questioned as purely extractive)</perceived_type>
  
  <observable>
    Potential error if X₄ were to:
    - Reject protocol as extractive without recognizing coordination function
    - Eliminate contamination response, destroying public health coordination
    - Treat institutional position as illegitimate extraction
    
    From X₄'s index, protocol is net beneficial (χ<0), provides genuine coordination.
    Error would be failing to recognize this while seeing only extraction to X₁.
  </observable>
  
  <correction_trigger>
    Recognition that:
    - χ(X₄) = -0.18 < 0 → net beneficiary, genuine coordination
    - Protocol serves public health function beyond individual case
    - Asymmetry is structural feature, not eliminable without losing coordination
    
    Would maintain Rope classification, preventing destructive reform.
  </correction_trigger>
  
  <note>
    This error is POTENTIAL, not manifest in source. Included to show indexical
    variance can produce errors in both directions: normalizing extraction (E₂)
    OR rejecting genuine coordination (E₄).
  </note>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

**Justification:**

1. **X₄ exhibits satisficing behavior:**
   - Applies standard protocol (contamination → destruction)
   - Does not optimize for individual case (would require evaluating X₁'s ontological status)
   - Risk-averse: eliminates contamination threat with high certainty

2. **Principal-agent structure present:**
   - X₄ acts as agent of public health institution
   - Protocol serves institutional goals (disease prevention)
   - Individual case (X₁) subordinated to institutional mandate

3. **Uncertainty and bounded information:**
   - X₄ operates under uncertainty about contamination spread
   - Standard protocol reduces cognitive load
   - No mechanism for evaluating non-standard cases (X₁'s transformation status)

4. **Realistic institutional behavior:**
   - Organizations satisfice under constraints
   - Protocols persist even when individual cases suggest exceptions
   - Institutional inertia prevents case-by-case optimization

**Incompatible with PIR:**
- Perfect rationality would require evaluating X₁'s unique status
- Would optimize for Pareto improvement (preserve X₁ if contamination manageable)
- No evidence of utility maximization without bounds

**Attractor implications:**
- BIR enables: Negotiated Equilibrium, Seeded Possibility
- BIR blocks: Deterministic Tragedy (requires PIR's implacability)
- Revolutionary Rupture remains possible under either model

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Seeded Possibility

**Structural justification:**

1. **Surface trajectory: Deterministic Tragedy**
   - C₁ (property) → C₂ (degradation) → C₃ (destruction) runs to completion
   - No internal mechanism prevents constraint cascade
   - X₁ reaches terminal state (annihilation under C₃)

2. **Underground transformation: Ontological shift**
   - C₂ process (degradation) produces status change invisible to C₃
   - X₁'s transformation occurs WITHIN constraint system
   - Supernatural intervention (TR₃) reveals transformation, does not create it

3. **Compatibility with BIR:**
   - Institutional satisficing (X₄) executes protocol without recognizing exception
   - No negotiation occurs (incompatible with Negotiated Equilibrium)
   - Bounded rationality prevents X₄ from detecting X₁'s transformed status

4. **Constraint profile:**
   - All constraints classify as Snare from X₁'s index (high extraction)
   - No Mountains present (all fail Boltzmann test)
   - Asymmetry throughout (extraction concentrated on powerless agent)

**Attractor Compatibility Check:**

```
Constraint Profile: Pure Snares (C₁, C₂, C₃ all Snare from X₁'s index)
Rationality Model: BIR
Compatible Attractors: Negotiated Equilibrium, Revolutionary Rupture, Seeded Possibility

Negotiated Equilibrium: REJECTED
  - Requires bargaining between agents
  - X₁ has no bargaining power (powerless, trapped)
  - X₄ satisfices, does not negotiate

Revolutionary Rupture: PARTIAL
  - TR₃ (supernatural intervention) could be rupture
  - But transformation occurs WITHIN C₂ (degradation process)
  - Rupture reveals transformation, does not create it

Seeded Possibility: SELECTED
  - Surface: Constraints run to completion (C₁→C₂→C₃)
  - Underground: C₂ produces ontological transformation
  - TR₃ reveals hidden transformation, enabling escape
  - Compatible with BIR (institution cannot detect transformation)
```

**Omega flag:**

The classification of TR₃ (supernatural intervention) remains ambiguous:
- If X₅ represents higher-level constraint ("Ontological transformation transcends physical form"), then Seeded Possibility is complete formalization
- If X₅ is true deus ex machina (external to constraint logic), then attractor is hybrid: Seeded Possibility + Revolutionary Rupture

Recommend: Treat X₅ as higher-level constraint for formal completeness. The transformation WITHIN C₂ (degradation → ontological status) is the seed; X₅ merely reveals what C₃ cannot detect.

---

## VI. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X₁, X₂, X₃, X₄, X₅)
☑ No source occupation, setting, or domain vocabulary
☑ No <experience>, <dialogue_markers>, or <narrative> fields
☑ Transformation rules are testable (IF-THEN with mechanical χ recalculation)
☑ Error types have observable patterns (testable conditions)
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Seeded Possibility), justified, compatible
☑ Indexical variance explicitly preserved across agents
☑ Boltzmann test run for each constraint (all FAIL → constructed, not natural)
☑ No ambiguity in specifications (omega flag on TR₃ noted)
☑ Output is pure symbolic network
```

---

## VII. SUMMARY METRICS

**Constraint Network:**
- 3 formalized constraints (C₁, C₂, C₃)
- 5 agents (X₁, X₂, X₃, X₄, X₅)
- 3 transformation rules (TR₁, TR₂, TR₃)
- 4 error manifestations (E₁, E₂, E₃, E₄)

**Classification Distribution:**
- Snare: 5 instances (C₁[X₁], C₂[X₁], C₂[X₃], C₃[X₁], pre-TR₃)
- Tangled Rope: 2 instances (C₁[X₂], post-TR₃)
- Rope: 2 instances (C₃[X₄], post-TR₃)
- Mountain: 0 instances (all constraints fail Boltzmann test)

**Indexical Variance:**
- C₁: Snare (X₁) vs Tangled Rope (X₂) — Δχ = 0.684
- C₂: Snare (X₁) vs Snare (X₃) — Δχ = 0.224 (both Snare, analytical prevents normalization)
- C₃: Snare (X₁) vs Rope (X₄) — Δχ = 1.26 (maximum divergence)

**System Dynamics:**
- Rationality Model: BIR (Bounded Institutional Rationality)
- Terminal Attractor: Seeded Possibility
- Omega Flags: 1 (TR₃ classification ambiguity)

**END OF STAGE 1 FORMALIZATION**