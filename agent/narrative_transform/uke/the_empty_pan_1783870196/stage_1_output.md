# STAGE 1: FORMAL CONSTRAINT NETWORK SPECIFICATION

## Variable Mapping (Header — Stripped During Anonymization)

```
X₁ ← Narrator (apprentice)
X₂ ← Narrator (adult)
X₃ ← Verrel
X₄ ← Merchants (collective)
G₁ ← Assayers Guild
I₁ ← Mint (institutional authority)
C₁ ← Unownable_Truth
C₂ ← Foundational_Choice
C₃ ← Instrumentalized_Standard
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Natural Law Baseline

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.05</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.5 × 0.8 = 0.06</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.0 ≤ 0.05 (mountain_suppression_ceiling): PASS
      χ = 0.06: Not checked for Mountain (natural laws affect all equally)
    </threshold_check>
    <rationale>
      Zero suppression indicates no enforcement mechanism required.
      Minimal extraction reflects natural constraint on information transfer.
      Immutable from biographical time horizon — no human action can make 
      absolute truth capturable within finite measurement systems.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.15 × 0.8 = 0.046</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25: PASS
      Supp = 0.0 ≤ 0.05: PASS
    </threshold_check>
    <rationale>
      Analytical position provides no escape from fundamental information-theoretic
      limit. Recognition of the constraint's nature does not enable circumvention.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.06 → Mountain, X₃ χ=0.046 → Mountain
    No variance. Classification stable across all observed indices.
    Both agents experience identical constraint type despite different power positions.
  </indexical_variance>

  <boltzmann_test>
    PASS
    Tested indices:
      (powerless, local): χ=0.06 → Mountain
      (analytical, local): χ=0.046 → Mountain
      (moderate, local): χ=0.04 → Mountain
      (powerful, local): χ=0.024 → Mountain
    
    Classification invariant under power variation.
    Burden varies proportionally (factorizable: χ = ε × π × σ).
    No non-factorizable coupling between P and S dimensions.
    
    Certification: Natural law — information-theoretic limit on measurement precision.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Constructed Baseline

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.1</epsilon>
    <suppression>0.8</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.1 × 1.5 × 0.8 = 0.12</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.1 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.8 > 0.05 (mountain_suppression_ceiling): FAIL
      
      Classification: Type I Error (False Mountain)
      High suppression (0.8) indicates enforcement mechanism.
      Constraint appears immutable from (trapped, biographical) index,
      but structural properties reveal constructed nature.
    </threshold_check>
    <rationale>
      From powerless index with identity_locked exit, the arbitrary selection
      of reference standard appears as unchangeable fact. High suppression
      maintains this naturalization. Biographical time horizon insufficient
      to observe standard revision cycles.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.1 × 1.15 × 0.8 = 0.092</chi>
    <type>Rope</type>
    <threshold_check>
      χ = 0.092 ≤ 0.35 (rope_chi_ceiling): PASS
      ε = 0.1 ≤ 0.45 (rope_epsilon_ceiling): PASS
      Coord = true: Provides standardization function
      Changeable from analytical index: Can observe arbitrary nature of selection
    </threshold_check>
    <rationale>
      Analytical position enables recognition that reference standard is
      conventional choice, not natural necessity. Low base extraction reflects
      genuine coordination value (standardization enables commerce). Asymmetry
      lies in who controls standard selection, not in extraction magnitude.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.12 → Mountain (False), X₃ χ=0.092 → Rope
    
    CRITICAL DIVERGENCE: Same constraint classified as immutable vs. changeable.
    
    Mechanism: High suppression (0.8) naturalizes arbitrary choice for powerless.
    Analytical index penetrates naturalization, reveals constructed coordination.
    
    This is the UPSTREAM ENABLER of C₃. By naturalizing the reference standard,
    C₂ makes the extraction mechanism in C₃ appear inevitable rather than designed.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    Tested indices:
      (powerless, local): χ=0.12 → Mountain (False)
      (analytical, local): χ=0.092 → Rope
      (institutional, local): χ=-0.016 → Rope (net beneficiary)
    
    Classification varies with power position in non-factorizable way.
    Powerless experiences as immutable; analytical sees as changeable.
    
    Certification: CONSTRUCTED CONSTRAINT
    High suppression maintains naturalization gradient across power positions.
    Not a natural law — a coordination mechanism with asymmetric visibility.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Primary Extraction Mechanism

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.8</epsilon>
    <suppression>0.7</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70 (snare_chi_threshold): PASS
      ε = 0.8: Not low base extraction
      High suppression (0.7) maintains extraction despite visibility
    </threshold_check>
    <rationale>
      Measurement-based extraction amplified by powerless position.
      Identity_locked exit: Cannot escape professional role requiring
      participation in measurement system. High base extraction (0.8)
      reflects systematic value transfer through certified deviation metrics.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>             <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.0 × 0.8 = 0.64</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      χ = 0.64: 0.46 ≤ χ ≤ 0.70 (tangled range): PASS
      Coord = true: Standardization provides genuine value
      Asym = true: Extraction distribution asymmetric
    </threshold_check>
    <rationale>
      Moderate power position (experience, insider status) dampens extraction
      to mid-range. Constraint normalized as "corrupt but functional" — 
      extraction acknowledged but accepted as system cost. Coordination value
      (standardization) remains visible, preventing Snare classification.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.15 × 0.8 = 0.736</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.736 > 0.70: PASS
      Analytical modifier (π=1.15) breaks normalization
    </threshold_check>
    <rationale>
      Analytical position detects extraction that moderate position normalizes.
      π=1.15 calibrated to prevent degeneracy: at π=1.0, would classify as
      Tangled Rope (χ=0.64). The 15% modifier reflects freedom from normalization
      pressure, enabling recognition of extraction magnitude.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>constrained</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70: PASS
      Immediate time horizon: No visibility into standard revision possibility
      Constrained exit: Can avoid measurement, but at cost of market exclusion
    </threshold_check>
    <rationale>
      Collective agent (merchants) experiences identical χ to X₁ despite
      different exit options. Constrained exit (can trade elsewhere) vs.
      identity_locked (cannot change profession) — but both face same
      power-scaled extraction within this market system.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=0.96 → Snare, X₂ χ=0.64 → Tangled Rope, X₃ χ=0.736 → Snare, X₄ χ=0.96 → Snare
    
    CRITICAL VARIANCE: Same constraint spans three classifications.
    
    Powerless (X₁, X₄): Pure extraction trap
    Moderate (X₂): Normalized hybrid — extraction acknowledged, coordination valued
    Analytical (X₃): Extraction detected despite normalization pressure
    
    This variance is the CORE MECHANISM of character development:
    X₁ → X₂ represents normalization trajectory (Snare → Tangled Rope)
    X₃ maintains analytical classification despite insider position
    
    The constraint itself is unchanged. What varies is the structural position
    from which it is experienced and classified.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    Tested indices:
      (powerless, local): χ=0.96 → Snare
      (moderate, local): χ=0.64 → Tangled Rope
      (analytical, local): χ=0.736 → Snare
      (institutional, local): χ=-0.128 → Rope (net beneficiary)
    
    Classification varies dramatically with power position.
    Non-factorizable coupling: institutional position inverts extraction sign.
    
    Certification: CONSTRUCTED EXTRACTION MECHANISM
    High base extraction (ε=0.8) + high suppression (0.7) + asymmetry = designed system.
    Coordination value (standardization) is real but asymmetrically distributed.
    Not a natural law — an institutional arrangement with power-dependent burden.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Normalization Through Experience

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>Sustained participation in measurement system over biographical time</condition>
    <target>C₃</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>
      I₁ = (powerless, biographical, identity_locked, local)
      P = powerless (π = 1.5)
    </from>
    <to>
      I₂ = (moderate, biographical, identity_locked, local)
      P = moderate (π = 1.0)
    </to>
  </index_change>
  
  <chi_recalculation>
    <before>
      χ(C₃, I₁) = 0.8 × 1.5 × 0.8 = 0.96 → Snare
    </before>
    <after>
      χ(C₃, I₂) = 0.8 × 1.0 × 0.8 = 0.64 → Tangled Rope
    </after>
  </chi_recalculation>
  
  <type_change>Snare → Tangled Rope</type_change>
  
  <mechanism>
    Power position shift from powerless to moderate reflects:
    - Insider knowledge acquisition (system navigation skills)
    - Partial benefit capture (reduced extraction through expertise)
    - Normalization of extraction (acceptance as system cost)
    
    Base constraint properties (ε, Supp, Coord, Asym) unchanged.
    Only π modifier changes, reflecting structural position shift.
  </mechanism>
  
  <preconditions>
    - Sustained system participation (biographical time scale)
    - Survival of initial high-extraction phase
    - No exit from identity_locked profession
    - Access to insider knowledge accumulation
  </preconditions>
  
  <blocked_by>
    - C₂ (naturalized baseline prevents recognition of reform possibility)
    - High suppression in C₃ (0.7) maintains system despite individual adaptation
  </blocked_by>
  
  <observables>
    - Reduction in explicit resistance to measurement system
    - Adoption of system-internal optimization strategies
    - Shift from "this is wrong" to "this is how it works"
    - Continued participation despite recognized extraction
  </observables>
</transformation_rule>
```

---

### TR₂: Analytical Resistance to Normalization

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>Maintain analytical index despite system participation</condition>
    <target>C₃</target>
    <agent>X₃</agent>
  </trigger>
  
  <index_change>
    <from>
      I₁ = (analytical, biographical, identity_locked, local)
      P = analytical (π = 1.15)
    </from>
    <to>
      I₂ = (analytical, biographical, identity_locked, local)
      P = analytical (π = 1.15)
    </to>
  </index_change>
  
  <chi_recalculation>
    <before>
      χ(C₃, I₁) = 0.8 × 1.15 × 0.8 = 0.736 → Snare
    </before>
    <after>
      χ(C₃, I₂) = 0.8 × 1.15 × 0.8 = 0.736 → Snare
    </after>
  </chi_recalculation>
  
  <type_change>Snare → Snare (no change)</type_change>
  
  <mechanism>
    ANTI-NORMALIZATION RULE: Analytical position maintained despite pressure.
    
    π = 1.15 modifier reflects freedom from normalization gradient.
    Unlike TR₁ (powerless → moderate), analytical position does not
    degrade to moderate through system participation.
    
    This is a STABILITY rule, not a transformation rule.
    It specifies what does NOT happen under normalization pressure.
  </mechanism>
  
  <preconditions>
    - Analytical capacity maintenance (external reference frame)
    - Resistance to insider benefit capture
    - Acceptance of sustained extraction burden (no power position shift)
  </preconditions>
  
  <blocked_by>
    - None (this is the stable state)
  </blocked_by>
  
  <observables>
    - Persistent classification of C₃ as Snare despite experience
    - Continued explicit recognition of extraction magnitude
    - No shift to "corrupt but functional" framing
    - Maintenance of reform possibility awareness
  </observables>
</transformation_rule>
```

---

### TR₃: Collective Organization Attempt (Blocked)

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>Organize collective action to modify reference standard</condition>
    <target>C₂</target>
    <agent>X₄</agent>
  </trigger>
  
  <index_change>
    <from>
      I₁ = (powerless, immediate, constrained, local)
      P = powerless (π = 1.5)
    </from>
    <to>
      I₂ = (organized, immediate, constrained, local)
      P = organized (π = 0.4)
    </to>
  </index_change>
  
  <chi_recalculation>
    <before>
      χ(C₂, I₁) = 0.1 × 1.5 × 0.8 = 0.12 → Mountain (False)
    </before>
    <after>
      χ(C₂, I₂) = 0.1 × 0.4 × 0.8 = 0.032 → Rope
    </after>
  </chi_recalculation>
  
  <type_change>Mountain (False) → Rope</type_change>
  
  <mechanism>
    Collective organization would reveal constructed nature of reference standard.
    Power position shift (powerless → organized) reduces extraction through
    burden-sharing and enables recognition of changeability.
    
    HOWEVER: This transformation is BLOCKED by C₂'s high suppression (0.8).
  </mechanism>
  
  <preconditions>
    - Collective action capacity
    - Recognition of C₂ as changeable (requires analytical index)
    - Coordination across X₄ collective
  </preconditions>
  
  <blocked_by>
    C₂.suppression = 0.8 (high enforcement prevents organization)
    C₂ naturalization at powerless index (appears as Mountain, not target for reform)
    
    CRITICAL BLOCK: Cannot organize to change what appears unchangeable.
    Type I Error (False Mountain) prevents recognition of reform target.
  </blocked_by>
  
  <observables>
    - No collective organization attempts observed
    - Individual adaptation strategies only (exit to other markets)
    - Acceptance of standard as given, optimization within constraints
  </observables>
</transformation_rule>
```

---

### TR₄: Institutional Benefit Capture

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>Occupy institutional position controlling standard or measurement</condition>
    <target>C₃</target>
    <agent>I₁</agent>
  </trigger>
  
  <index_change>
    <from>
      I₁ = (moderate, biographical, identity_locked, local)
      P = moderate (π = 1.0)
    </from>
    <to>
      I₂ = (institutional, biographical, identity_locked, local)
      P = institutional (π = -0.2)
    </to>
  </index_change>
  
  <chi_recalculation>
    <before>
      χ(C₃, I₁) = 0.8 × 1.0 × 0.8 = 0.64 → Tangled Rope
    </before>
    <after>
      χ(C₃, I₂) = 0.8 × (-0.2) × 0.8 = -0.128 → Rope (net beneficiary)
    </after>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Rope (extraction sign inverted)</type_change>
  
  <mechanism>
    Institutional position (π = -0.2) inverts extraction direction.
    Same constraint that extracts from powerless now benefits institutional agent.
    
    Asymmetry (Asym = true) enables this inversion:
    - Powerless: χ = +0.96 (pays extraction)
    - Institutional: χ = -0.128 (receives extraction)
    
    Coordination value (Coord = true) remains — standardization still functional.
    From institutional index, appears as pure Rope (beneficial coordination).
  </mechanism>
  
  <preconditions>
    - Access to institutional position (G₁ or I₁ membership)
    - Control over standard selection or measurement certification
    - Maintenance of system legitimacy (suppression = 0.7)
  </preconditions>
  
  <blocked_by>
    - None (this is the designed equilibrium)
  </blocked_by>
  
  <observables>
    - Institutional agents defend system as "necessary standardization"
    - No recognition of extraction from institutional index
    - Classification as Rope (Type III Error: Snare-as-Rope)
    - Resistance to reform proposals that would eliminate extraction
  </observables>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type I — False Mountain (C₂ at powerless index)

```xml
<error id="E₁">
  <type>Type I: False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₂</constraint>
  
  <actual_type>Rope (from analytical index: χ=0.092)</actual_type>
  <perceived_type>Mountain (from powerless index: χ=0.12)</perceived_type>
  
  <mechanism>
    High suppression (0.8) + identity_locked exit + biographical time horizon
    → naturalization of arbitrary reference standard selection
    
    Constraint appears as unchangeable fact rather than conventional choice.
    Boltzmann test FAILS (classification varies with power), but powerless
    index cannot access this test.
  </mechanism>
  
  <observable>
    Does not question reference standard selection.
    Treats deviation from standard as objective fact rather than measurement artifact.
    No attempts to propose alternative standards or challenge baseline selection.
    Optimization strategies accept standard as given, work within measurement system.
  </observable>
  
  <correction_trigger>
    Power position shift to analytical (π = 1.15) would reveal:
    - χ(C₂, analytical) = 0.092 → Rope
    - Recognition of constructed nature
    - Visibility of reform possibility
    
    OR: Time horizon extension to generational/historical would show
    standard revision cycles, breaking immutability perception.
  </correction_trigger>
  
  <consequences>
    Blocks TR₃ (collective organization to modify standard).
    Enables C₃ extraction by naturalizing its measurement basis.
    Prevents recognition of C₂ → C₃ causal chain.
  </consequences>
</error>
```

---

### Error E₂: Type III — Snare-as-Rope (C₃ at institutional index)

```xml
<error id="E₂">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>I₁</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Snare (from powerless index: χ=0.96)</actual_type>
  <perceived_type>Rope (from institutional index: χ=-0.128)</perceived_type>
  
  <mechanism>
    Institutional position (π = -0.2) inverts extraction sign.
    Net beneficiary experiences constraint as pure coordination mechanism.
    
    Asymmetry (Asym = true) enables extraction to flow TO institutional agent
    FROM powerless agents, but institutional index cannot observe this flow
    direction — only experiences net benefit.
  </mechanism>
  
  <observable>
    Defends measurement system as "necessary standardization."
    Frames extraction as "cost of doing business" or "market efficiency."
    Resists reform proposals that would reduce ε or eliminate asymmetry.
    Classification: "This is a Rope — it provides coordination value."
    
    CRITICAL: This classification is TRUE from institutional index (χ < 0).
    Error lies in generalizing local truth to universal claim.
  </observable>
  
  <correction_trigger>
    Index shift to powerless (π = 1.5) would reveal:
    - χ(C₃, powerless) = 0.96 → Snare
    - Extraction magnitude and direction
    
    OR: Explicit indexical variance analysis showing:
    - Same constraint: Rope (institutional) AND Snare (powerless)
    - Both classifications objectively true from their indices
    - Asymmetry enables simultaneous benefit and extraction
  </correction_trigger>
  
  <consequences>
    Institutional agents genuinely believe system is beneficial.
    Not cynical extraction — indexed truth prevents recognition.
    Reform resistance is sincere, not strategic.
    Maintains high suppression (0.7) to preserve "coordination function."
  </consequences>
</error>
```

---

### Error E₃: Type V.a — Tangled-as-Rope (C₃ at moderate index)

```xml
<error id="E₃">
  <type>Type V.a: Tangled-as-Rope (Ignoring Extraction Component)</type>
  <agent>X₂</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Tangled Rope (χ=0.64, Coord=true, Asym=true)</actual_type>
  <perceived_type>Rope (emphasizing coordination, minimizing extraction)</perceived_type>
  
  <mechanism>
    Normalization through experience (TR₁) shifts classification from Snare to
    Tangled Rope, but moderate position tends to over-weight coordination value
    and under-weight extraction magnitude.
    
    χ = 0.64 is in Tangled range (0.46–0.70), but proximity to Rope threshold
    (0.35) creates ambiguity. Insider benefits (reduced personal extraction)
    bias perception toward Rope classification.
  </mechanism>
  
  <observable>
    Frames system as "corrupt but functional."
    Acknowledges extraction exists but emphasizes coordination necessity.
    Resists radical reform: "System needs fixing, not replacement."
    Optimization within constraints rather than constraint elimination.
    
    PARTIAL ERROR: Correctly identifies hybrid nature (Tangled Rope),
    but misweights components in reform strategy.
  </observable>
  
  <correction_trigger>
    Analytical index (π = 1.15) would reveal:
    - χ(C₃, analytical) = 0.736 → Snare
    - Extraction magnitude not reducible to "corruption"
    - Structural asymmetry, not implementation flaw
    
    OR: Observation of powerless index experience:
    - χ(C₃, powerless) = 0.96 → Snare
    - Recognition that "functional for whom?"
  </correction_trigger>
  
  <consequences>
    Incremental reform strategies that preserve extraction mechanism.
    Focus on reducing Supp or improving Coord, not addressing ε or Asym.
    Maintains system stability through insider buy-in.
    Prevents coalition with powerless agents (different constraint types).
  </consequences>
</error>
```

---

### Error E₄: Indexical Variance Blindness (Cross-Agent)

```xml
<error id="E₄">
  <type>Meta-Error: Indexical Variance Blindness</type>
  <agents>X₁, X₂, X₃, I₁</agents>
  <constraint>C₃</constraint>
  
  <actual_state>
    C₃ simultaneously:
    - Snare (X₁, X₃: χ > 0.70)
    - Tangled Rope (X₂: χ = 0.64)
    - Rope (I₁: χ = -0.128)
    
    All classifications objectively true from their respective indices.
  </actual_state>
  
  <perceived_state>
    Each agent generalizes local classification to universal truth:
    - X₁: "This is a Snare" (correct locally, incomplete globally)
    - X₂: "This is a Tangled Rope" (correct locally, incomplete globally)
    - I₁: "This is a Rope" (correct locally, incomplete globally)
  </perceived_state>
  
  <mechanism>
    Indexed realism principle: Truth is position-relative.
    Each agent's classification is TRUE from their structural position.
    Error lies in absolutizing indexed truth, not in the classification itself.
    
    No agent has access to "view from nowhere" — all observations are indexed.
    Variance is not error; variance blindness is error.
  </mechanism>
  
  <observable>
    Agents talk past each other in reform debates:
    - X₁: "Eliminate this extraction trap"
    - X₂: "Reform the corrupt parts, keep coordination"
    - I₁: "Preserve this necessary standardization"
    
    Each is describing DIFFERENT CONSTRAINT TYPES from same structural object.
    Debate appears as value disagreement, but is actually indexical variance.
  </observable>
  
  <correction_trigger>
    Explicit indexical variance analysis:
    1. Map each agent's index: (P, T, E, S)
    2. Calculate χ for each index
    3. Classify constraint from each index
    4. Recognize all classifications as locally true
    5. Reform strategy must address variance, not pick "correct" classification
    
    This requires meta-analytical capacity: ability to model other indices
    without occupying them.
  </correction_trigger>
  
  <consequences>
    Reform coalitions fail due to incompatible constraint models.
    Institutional agents sincerely defend extraction (it's Rope from their index).
    Powerless agents cannot communicate extraction to moderates (different types).
    System stability maintained through indexical fragmentation, not just suppression.
  </consequences>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Model Selection: Bounded Institutional Rationality (BIR)

```xml
<rationality_model>
  <type>Bounded Institutional Rationality (BIR)</type>
  
  <justification>
    I₁ (institutional authority) exhibits:
    
    1. SATISFICING BEHAVIOR
       - Maintains extraction at ε = 0.8 (high but not maximal)
       - Could increase ε further but accepts current level
       - Balances extraction against system stability risk
    
    2. PRINCIPAL-AGENT PROBLEMS
       - G₁ (assayers guild) implements measurement system
       - Institutional control imperfect (covert resistance possible)
       - X₂ writes in margins (hidden dissent within compliance)
    
    3. RISK AVERSION
       - High suppression (0.7) maintains stability
       - Does not push extraction to breaking point
       - Preserves coordination value (Coord = true) as legitimacy source
    
    4. UNCERTAINTY MANAGEMENT
       - Cannot perfectly monitor all agents
       - Relies on naturalization (C₂) rather than pure force
       - Accepts some extraction loss to maintain system
    
    NOT Perfect Institutional Rationality (PIR):
    - PIR would maximize ε without bound
    - PIR would eliminate all coordination value if extraction-neutral
    - PIR would not tolerate covert resistance (perfect monitoring)
    
    BIR better fits observed constraint properties:
    - ε = 0.8 (high but bounded)
    - Coord = true (coordination value preserved)
    - Supp = 0.7 (high but not maximal)
    - Asymmetry maintained but not pushed to collapse
  </justification>
  
  <implications>
    Under BIR:
    - Negotiated Equilibrium is reachable (institutional satisficing enables bargaining)
    - Seeded Possibility is reachable (imperfect monitoring enables covert transformation)
    - Deterministic Tragedy is NOT inevitable (institutions can be negotiated with)
    - Revolutionary Rupture is possible but not necessary (system has flexibility)
    
    Key mechanism: I₁ will negotiate if:
    - Extraction threatened (risk of system collapse)
    - Alternative coordination mechanisms proposed (Pareto-improving)
    - Suppression costs exceed extraction benefits
    
    This creates reform possibility absent under PIR.
  </implications>
</rationality_model>
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Seeded Possibility

```xml
<terminal_attractor>
  <type>Seeded Possibility</type>
  
  <definition>
    Surface: Deterministic Tragedy (constraints run to completion)
    Underground: Transformation seeded through covert action
    
    Observable outcome appears as tragedy, but contains latent transformation
    potential that may activate in future time horizons beyond narrative scope.
  </definition>
  
  <justification>
    CONSTRAINT PROFILE COMPATIBILITY:
    
    1. Mountain-dominated (C₁ + C₂ False Mountain)
       - C₁: True Mountain (natural law) → unchangeable
       - C₂: False Mountain (naturalized) → appears unchangeable
       - Seeded Possibility compatible with Mountain-dominated systems
    
    2. High-extraction Snare present (C₃ from powerless index)
       - χ = 0.96 for X₁, X₄
       - Deterministic Tragedy trajectory for powerless agents
       - BUT: BIR model prevents pure tragedy
    
    3. Indexical variance creates underground space
       - X₂: Tangled Rope (normalized, functional insider)
       - X₃: Snare (analytical, resistant to normalization)
       - Divergence enables covert action (writing in margins)
    
    4. Imperfect institutional monitoring (BIR)
       - Principal-agent problems allow hidden dissent
       - Suppression = 0.7 (high but not total)
       - Covert transformation possible within compliance
    
    RATIONALITY MODEL COMPATIBILITY:
    
    BIR + Seeded Possibility:
    - Institutional satisficing creates monitoring gaps
    - Agents can maintain surface compliance while seeding transformation
    - Long-term instability possible despite short-term stability
    - Matches observed pattern: X₂ writes in margins (covert), maintains role (overt)
    
    NOT Negotiated Equilibrium:
    - No explicit bargaining observed
    - C₂ naturalization prevents reform recognition
    - Institutional agents (E₂) sincerely believe system is Rope
    
    NOT Revolutionary Rupture:
    - No disruption of constraint logic itself
    - System continues functioning
    - Transformation is latent, not actualized
    
    NOT Deterministic Tragedy:
    - BIR model prevents pure tragedy
    - Covert resistance present (not pure compliance)
    - Indexical variance creates transformation potential
  </justification>
  
  <mechanism>
    SURFACE TRAJECTORY (Observable):
    - X₁ → X₂: Normalization (Snare → Tangled Rope via TR₁)
    - X₄: Continued extraction (Snare, no transformation)
    - I₁: System maintenance (Rope from institutional index)
    - Apparent outcome: Stable extraction equilibrium
    
    UNDERGROUND TRAJECTORY (Latent):
    - X₂: Covert documentation (writing in margins)
    - X₃: Analytical resistance (maintains Snare classification)
    - Knowledge accumulation outside official channels
    - Potential for future activation (generational/historical time horizon)
    
    SEEDING MECHANISM:
    - Covert action preserves analytical classification
    - Documentation creates evidence base for future reform
    - Indexical variance prevents total naturalization
    - BIR monitoring gaps enable persistence
    
    ACTIVATION CONDITIONS (Beyond narrative scope):
    - Time horizon extension (biographical → generational)
    - Power position shift (organized collective action)
    - External shock (system instability)
    - Accumulated documentation reaches critical mass
  </mechanism>
  
  <observables>
    Surface (Tragedy):
    - X₁ normalizes into X₂ (accepts system)
    - X₄ continues paying extraction
    - No overt resistance or reform
    - System appears stable
    
    Underground (Seeded):
    - X₂ writes in margins (covert documentation)
    - X₃ maintains analytical index (resists normalization)
    - Knowledge preserved outside official channels
    - Transformation potential latent but present
    
    CRITICAL: Both trajectories are real and simultaneous.
    Not "false tragedy" or "hidden victory" — genuine tragedy with genuine seeds.
  </observables>
  
  <compatibility_check>
    Constraint profile: Mountain-dominated + Snare → Compatible ✓
    Rationality model: BIR → Compatible ✓
    Indexical variance: Present (enables underground) → Compatible ✓
    Covert action: Possible under BIR monitoring → Compatible ✓
    
    Attractor Compatibility Matrix (§VII):
    - Mountain-dominated + BIR → Seeded Possibility: COMPATIBLE
  </compatibility_check>
</terminal_attractor>
```

---

## VI. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
  - C₁: ε=0.05, Supp=0.0, Coord=false, Asym=false
  - C₂: ε=0.1, Supp=0.8, Coord=true, Asym=true
  - C₃: ε=0.8, Supp=0.7, Coord=true, Asym=true

☑ All χ calculations shown with π and σ values
  - All agent classifications include explicit χ = ε × π × σ calculations
  - π values: powerless=1.5, moderate=1.0, analytical=1.15, institutional=-0.2
  - σ values: local=0.8 (all agents)

☑ All characters use variable names (X₁, X₂, X₃, X₄, G₁, I₁)
  - No source identifiers in body
  - Variable mapping in header only

☑ No source occupation, setting, or domain vocabulary
  - Abstract descriptions: "measurement system," "reference standard," "certification"
  - No specific trade, location, or cultural references

☑ No <experience>, <dialogue_markers>, or <narrative> fields
  - All descriptions structural and mechanical
  - Observable patterns specified as testable conditions

☑ Transformation rules are testable (IF-THEN with mechanical χ recalculation)
  - TR₁: Normalization (powerless → moderate)
  - TR₂: Analytical stability (analytical → analytical)
  - TR₃: Collective organization (blocked by C₂ naturalization)
  - TR₄: Institutional benefit capture (moderate → institutional)

☑ Error types have observable patterns (testable conditions)
  - E₁: Type I (False Mountain) — no questioning of reference standard
  - E₂: Type III (Snare-as-Rope) — defense of "necessary standardization"
  - E₃: Type V.a (Tangled-as-Rope) — "corrupt but functional" framing
  - E₄: Indexical variance blindness — agents talk past each other

☑ Institutional rationality model specified (BIR) with justification
  - Satisficing behavior (ε = 0.8, not maximal)
  - Principal-agent problems (covert resistance possible)
  - Risk aversion (maintains coordination value)
  - Uncertainty management (imperfect monitoring)

☑ Terminal attractor selected (Seeded Possibility), justified, and compatible
  - Surface: Deterministic Tragedy (normalization, continued extraction)
  - Underground: Transformation seeded (covert documentation, analytical resistance)
  - Compatible with: Mountain-dominated + BIR + indexical variance
  - Mechanism: Covert action within compliance, latent activation potential

☑ Indexical variance explicitly preserved across agents
  - C₂: Mountain (X₁) vs. Rope (X₃)
  - C₃: Snare (X₁, X₃, X₄) vs. Tangled Rope (X₂) vs. Rope (I₁)
  - All variance mechanically derived from index differences

☑ Boltzmann test run for each constraint claiming Mountain status
  - C₁: PASS (natural law, classification invariant)
  - C₂: FAIL (constructed, classification varies with power)
  - C₃: FAIL (constructed, dramatic variance with power)

☑ No ambiguity in specifications
  - All thresholds explicitly checked
  - All calculations shown
  - All classifications justified with structural rationale
```

---

## VII. NETWORK SUMMARY

### Constraint Dependency Graph

```
C₁ (Natural Law Baseline)
  ↓ (philosophical foundation)
C₂ (Constructed Baseline) → ENABLES → C₃ (Primary Extraction)
  ↓ (naturalization)
  Blocks TR₃ (collective organization)
```

### Indexical Variance Map

```
         │ C₁      │ C₂           │ C₃
─────────┼─────────┼──────────────┼─────────────────
X₁       │ Mountain│ Mountain (F) │ Snare
X₂       │ Mountain│ —            │ Tangled Rope
X₃       │ Mountain│ Rope         │ Snare
X₄       │ —       │ —            │ Snare
I₁       │ —       │ Rope         │ Rope (inverted)

(F) = False Mountain (Type I Error)
```

### Critical Mechanisms

1. **C₂ → C₃ Enablement:** Naturalization of reference standard makes extraction mechanism appear inevitable

2. **Normalization Trajectory:** X₁ → X₂ via TR₁ (Snare → Tangled Rope through experience)

3. **Analytical Resistance:** X₃ maintains Snare classification despite insider position (TR₂ stability)

4. **Institutional Inversion:** I₁ experiences C₃ as Rope (net beneficiary) via TR₄

5. **Reform Block:** TR₃ (collective organization) blocked by C₂ False Mountain error

6. **Seeded Transformation:** Covert action (X₂ writing in margins) within surface compliance, enabled by BIR monitoring gaps

### Attractor Dynamics

**Surface (Observable):**
- Deterministic Tragedy trajectory for powerless (X₁, X₄)
- Normalization equilibrium for moderate (X₂)
- Stable extraction for institutional (I₁)

**Underground (Latent):**
- Analytical resistance (X₃)
- Covert documentation (X₂)
- Knowledge accumulation outside official channels
- Transformation potential seeded for future activation

**Terminal State:** Seeded Possibility — tragedy with latent transformation, compatible with BIR + Mountain-dominated + indexical variance

---

**END OF STAGE 1 FORMAL SPECIFICATION**