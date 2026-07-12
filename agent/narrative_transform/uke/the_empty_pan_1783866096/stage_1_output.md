# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Narrator (apprentice phase)
X₂ ← Narrator (post-revelation phase)
X₃ ← Verrel
X₄ ← King/Guild (institutional actor)
X₅ ← Merchants (collective)

C₁ ← TheUnownableTrueGrain
C₂ ← TheHiddenFoundingChoice
C₃ ← TheWeaponizedStandard
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁

```xml
<constraint id="C₁">
  <base_properties>
    <epsilon>0.05</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.5 × 1.0 = 0.075</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.0 ≤ 0.05 (mountain_suppression_ceiling): PASS
      Boltzmann test required
    </threshold_check>
    <rationale>
      Zero suppression indicates natural emergence. Minimal extraction 
      suggests physical law rather than constructed system. Classification 
      contingent on Boltzmann independence verification.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.15 × 1.0 = 0.0575</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25: PASS
      Supp = 0.0 ≤ 0.05: PASS
      Boltzmann test required
    </threshold_check>
    <rationale>
      Analytical position does not alter classification. Natural law 
      affects all observers equally regardless of analytical capacity.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>             <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.05 × 1.0 × 1.0 = 0.05</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.05 ≤ 0.25: PASS
      Supp = 0.0 ≤ 0.05: PASS
      Boltzmann test required
    </threshold_check>
    <rationale>
      Moderate power position yields baseline χ. Classification 
      invariant across power positions supports Mountain hypothesis.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.075 → Mountain, X₂ χ=0.0575 → Mountain, X₃ χ=0.05 → Mountain
    
    Variance is minimal and factorizable (pure power scaling of low base).
    Classification invariant across all tested indices. Supports Mountain 
    classification pending Boltzmann verification.
  </indexical_variance>

  <boltzmann_test>
    PASS
    
    Test procedure:
    1. Varied P: {powerless, analytical, moderate} → Type invariant
    2. Varied S: universal only (constraint property) → N/A
    3. Varied T: {biographical} → immutability claim consistent with horizon
    4. Varied E: {identity_locked, analytical} → Type invariant
    
    Classification does not vary with power position. Burden distribution 
    is uniform (all agents experience same ε, differences are pure π scaling).
    No non-factorizable Power × Scope interaction detected.
    
    Conclusion: Constraint exhibits properties of natural law within 
    narrative physics. Mountain classification certified.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂

```xml
<constraint id="C₂">
  <base_properties>
    <epsilon>0.2</epsilon>
    <suppression>0.05</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₂">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>historical</time>
      <exit>analytical</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.2 × 1.15 × 1.0 = 0.23</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.2 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.05 ≤ 0.05 (mountain_suppression_ceiling): PASS (boundary)
      Boltzmann test required
    </threshold_check>
    <rationale>
      Historical time horizon makes constraint appear immutable (founding 
      choice already made, cannot be unmade from current position). 
      Analytical exit allows recognition of constructed nature but not 
      alteration. Minimal suppression at boundary threshold.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>historical</time>
      <exit>analytical</exit>
      <scope>universal</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.2 × 1.15 × 1.0 = 0.23</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.2 ≤ 0.25: PASS
      Supp = 0.05 ≤ 0.05: PASS (boundary)
      Boltzmann test required
    </threshold_check>
    <rationale>
      Identical index to X₂. Both agents occupy analytical position 
      relative to historical founding event. Classification identical.
    </rationale>
  </agent>

  <agent ref="X₅">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.2 × 1.5 × 0.8 = 0.24</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.2 ≤ 0.25: PASS
      Supp = 0.05 ≤ 0.05: PASS (boundary)
      Boltzmann test required
    </threshold_check>
    <rationale>
      Immediate time horizon makes historical constraint appear completely 
      immutable. Trapped exit reinforces Mountain perception. Local scope 
      slightly dampens extraction (easier to verify founding choice exists, 
      harder to verify its arbitrariness).
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₂ χ=0.23 → Mountain, X₃ χ=0.23 → Mountain, X₅ χ=0.24 → Mountain
    
    Low variance. All classifications converge on Mountain despite different 
    power positions and time horizons. Suggests constraint functions as 
    immutable background regardless of structural position.
    
    Critical property: Asymmetry flag indicates constructed nature, but 
    historical time horizon makes construction appear as natural law to 
    all current agents.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Constructed, Not Natural
    
    Test procedure:
    1. Varied P: {analytical, powerless} → Type invariant BUT
    2. Asymmetry flag = true → burden distribution non-uniform by design
    3. Coordination flag = true → serves constructed coordination function
    4. Suppression > 0 → requires enforcement (minimal but present)
    
    Critical finding: Constraint exhibits Mountain-like immutability from 
    all current indices, but asymmetric burden distribution reveals 
    constructed origin. A natural law cannot have asymmetric costs.
    
    Resolution: This is a WELL-DESIGNED INSTITUTION that appears as 
    natural law because:
    - Founding choice is historical (outside biographical time horizon)
    - Suppression is minimal (normalized, not actively enforced)
    - Coordination value is real (not pure extraction)
    
    Classification: Mountain (from all current indices)
    Structural certification: Constructed institution, not natural law
    
    Implication: Agents correctly classify as Mountain given their indices,
    but this is Type I error (False Mountain) from civilizational index.
    The constraint IS changeable at founding-choice timescale, but no 
    current agent has access to that timescale.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃

```xml
<constraint id="C₃">
  <base_properties>
    <epsilon>0.8</epsilon>
    <suppression>0.8</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70 (snare_chi_threshold): PASS
      ε = 0.8 > 0.45 (prevents low-base false positive): PASS
      High suppression (0.8) confirms enforcement requirement
    </threshold_check>
    <rationale>
      Powerless position amplifies base extraction. Trapped exit eliminates 
      alternatives. Local scope slightly dampens (easier to verify standard's 
      inaccuracy locally), but extraction remains dominant. High suppression 
      indicates constraint requires force to maintain. Coordination value 
      exists but is overwhelmed by extraction from this index.
    </rationale>
  </agent>

  <agent ref="X₃">
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
      Coord = true: PASS
      Asym = true: PASS
      All conditions for Tangled Rope satisfied
    </threshold_check>
    <rationale>
      Moderate power dampens extraction to mid-range. Identity-locked exit 
      indicates cognitive fusion with constraint (cannot exit without 
      abandoning professional identity). Coordination value is genuine 
      (standard enables measurement system). Asymmetric cost distribution 
      is visible but not dominant from this position. Irreducible hybrid: 
      real coordination AND real extraction.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>institutional</power>        <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.8 × (-0.2) × 0.9 = -0.144</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.144 ≤ 0 (net beneficiary): PASS
      χ ≤ 0 → ε ceiling bypassed (dual threshold rule)
      Changeable from generational horizon: PASS
    </threshold_check>
    <rationale>
      Institutional position inverts extraction (net beneficiary). Negative 
      χ indicates constraint extracts FROM system TO this agent. Arbitrage 
      exit allows playing alternatives against each other. Generational 
      time horizon makes constraint appear modifiable. Regional scope 
      provides verification difficulty that amplifies extraction from 
      others while this agent captures value.
    </rationale>
  </agent>

  <agent ref="X₅">
    <index>
      <power>powerless</power>            <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 0.96 > 0.70: PASS
      ε = 0.8 > 0.45: PASS
      Identical to X₁ classification
    </threshold_check>
    <rationale>
      Collective of powerless agents experiences identical extraction 
      amplification as individual powerless agent. Immediate time horizon 
      makes constraint appear completely immutable. Trapped exit (economic 
      necessity) eliminates alternatives.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=0.96 → Snare, X₃ χ=0.64 → Tangled Rope, X₄ χ=-0.144 → Rope, X₅ χ=0.96 → Snare
    
    HIGH VARIANCE — CRITICAL FINDING
    
    Same constraint produces four distinct classifications:
    - Snare (powerless agents): Extraction trap, no visible coordination
    - Tangled Rope (moderate agent): Hybrid coordination-extraction
    - Rope (institutional agent): Net beneficiary, coordination mechanism
    
    This is NOT measurement error or ambiguity. Each classification is 
    objectively correct from its index. The constraint has different 
    structural properties at different power positions.
    
    Mechanism: Asymmetric cost distribution + high base extraction + 
    power-dependent burden allocation creates indexical divergence.
    
    Implication: Agents at different indices experience fundamentally 
    different constraints. X₄ experiences genuine coordination value 
    (negative χ = receives value). X₁ experiences pure extraction 
    (χ near maximum). X₃ experiences irreducible hybrid.
    
    This variance is the ENGINE of the constraint network's dynamics.
  </indexical_variance>

  <boltzmann_test>
    N/A — Not Claiming Mountain Status
    
    Constraint does not claim to be natural law. High extraction, high 
    suppression, and indexical variance all indicate constructed system.
    Boltzmann test not required.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Attempt

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>Powerless agents attempt collective organization to establish alternative measurement system</condition>
    <target>C₃</target>
    <agents>X₁, X₅</agents>
  </trigger>

  <index_change>
    <agent>X₁</agent>
    <from>
      P = powerless (π=1.5)
      E = trapped
    </from>
    <to>
      P = organized (π=0.4)
      E = constrained
    </to>
  </index_change>

  <chi_recalculation>
    <agent>X₁</agent>
    <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.8 × 0.4 × 0.8 = 0.256 → Rope</after>
  </chi_recalculation>

  <type_change>Snare → Rope</type_change>

  <preconditions>
    <condition>Sufficient agents recognize extraction (not normalized)</condition>
    <condition>Coordination costs lower than continued extraction costs</condition>
    <condition>Alternative measurement system technically feasible</condition>
  </preconditions>

  <blocked_by>
    <constraint>C₂</constraint>
    <mechanism>
      Founding choice constraint makes alternative system appear to require 
      establishing new arbitrary zero-point, which reproduces the original 
      problem. Agents perceive reform as futile (Type II error: treating 
      changeable C₃ as if it inherits immutability from C₂).
    </mechanism>
  </blocked_by>

  <blocked_by>
    <agent>X₄</agent>
    <mechanism>
      Institutional agent has negative χ (net beneficiary). Rational 
      institutional actor will suppress collective organization to preserve 
      extraction flow. Suppression capacity = 0.8 (high).
    </mechanism>
  </blocked_by>

  <outcome>
    Transformation blocked. Agents remain at powerless index despite 
    technical feasibility of organization. C₃ persists as Snare for X₁, X₅.
  </outcome>
</transformation_rule>
```

---

### TR₂: Individual Exit Attempt

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>Individual agent attempts exit from constraint</condition>
    <target>C₃</target>
    <agent>X₁</agent>
  </trigger>

  <index_change>
    <agent>X₁</agent>
    <from>
      E = trapped
    </from>
    <to>
      E = constrained (exit at high cost)
    </to>
  </index_change>

  <chi_recalculation>
    <agent>X₁</agent>
    <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare (unchanged)</after>
    <note>Exit option change does not affect χ calculation directly, but enables escape</note>
  </chi_recalculation>

  <type_change>Snare → (escaped, no longer subject to constraint)</type_change>

  <preconditions>
    <condition>Alternative economic activity available</condition>
    <condition>Agent willing to pay exit cost (abandon profession, relocate, etc.)</condition>
  </preconditions>

  <blocked_by>
    <constraint>C₃ (self-blocking)</constraint>
    <mechanism>
      High extraction (ε=0.8) depletes resources needed for exit. 
      Trapped exit option indicates economic necessity prevents departure.
      Circular trap: need resources to exit, but constraint extracts resources.
    </mechanism>
  </blocked_by>

  <outcome>
    Individual exit typically blocked by resource depletion. Rare successful 
    exits do not change constraint structure for remaining agents.
  </outcome>
</transformation_rule>
```

---

### TR₃: Analytical Recognition (Index Shift)

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>Agent gains analytical perspective on constraint structure</condition>
    <target>C₂, C₃</target>
    <agent>X₁ → X₂</agent>
  </trigger>

  <index_change>
    <agent>X₁</agent>
    <from>
      P = powerless (π=1.5)
      E = trapped
    </from>
    <to>
      P = analytical (π=1.15)
      E = analytical
    </to>
  </index_change>

  <chi_recalculation>
    <constraint>C₂</constraint>
    <before>χ = 0.2 × 1.5 × 0.8 = 0.24 → Mountain</before>
    <after>χ = 0.2 × 1.15 × 1.0 = 0.23 → Mountain</after>
    <note>Classification unchanged, but agent now recognizes constructed nature</note>
  </chi_recalculation>

  <chi_recalculation>
    <constraint>C₃</constraint>
    <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.8 × 1.15 × 0.8 = 0.736 → Snare (still above threshold)</after>
    <note>Extraction dampened but remains Snare. Analytical position breaks normalization.</note>
  </chi_recalculation>

  <type_change>
    C₂: Mountain → Mountain (classification stable, understanding changed)
    C₃: Snare → Snare (χ reduced but still >0.70)
  </type_change>

  <preconditions>
    <condition>Access to information about constraint structure</condition>
    <condition>Cognitive capacity to analyze from outside position</condition>
    <condition>Willingness to denormalize (accept that "normal" is extractive)</condition>
  </preconditions>

  <outcome>
    Agent recognizes extraction but remains subject to it. Analytical 
    position provides understanding without power to transform. Creates 
    potential for Error Type III detection (recognizing that moderate 
    agents misclassify C₃ as Tangled Rope when it is Snare from powerless 
    index).
  </outcome>
</transformation_rule>
```

---

### TR₄: Institutional Suppression Response

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>Institutional agent detects threat to extraction flow</condition>
    <target>C₃</target>
    <agent>X₄</agent>
    <response_to>TR₁ (collective organization attempt)</response_to>
  </trigger>

  <index_change>
    <note>No index change for X₄ (already institutional)</note>
    <note>Suppression parameter increase for C₃</note>
  </index_change>

  <suppression_change>
    <from>Supp(C₃) = 0.8</from>
    <to>Supp(C₃) = 0.9</to>
    <mechanism>Increased enforcement, sanctions against alternative systems</mechanism>
  </suppression_change>

  <chi_recalculation>
    <agent>X₁</agent>
    <note>Suppression does not directly affect χ, but increases cost of transformation</note>
    <before>χ = 0.96 → Snare</before>
    <after>χ = 0.96 → Snare (unchanged)</after>
  </chi_recalculation>

  <preconditions>
    <condition>X₄ has negative χ (rational to defend extraction)</condition>
    <condition>X₄ has institutional power (capacity to suppress)</condition>
    <condition>Suppression cost < value of preserved extraction</condition>
  </preconditions>

  <outcome>
    Increased suppression raises barrier to TR₁ (collective organization).
    Constraint persists. X₄ maintains negative χ (net beneficiary status).
  </outcome>
</transformation_rule>
```

---

### TR₅: Constraint Degradation (Tangled → Snare)

```xml
<transformation_rule id="TR₅">
  <trigger>
    <condition>Coordination value erodes while extraction persists</condition>
    <target>C₃</target>
    <agent>X₃</agent>
  </trigger>

  <index_change>
    <note>No power position change</note>
    <note>Constraint property change: Coord flag degradation</note>
  </index_change>

  <constraint_change>
    <from>
      ε = 0.8
      Coord = true
      Asym = true
    </from>
    <to>
      ε = 0.85 (extraction increases as coordination decays)
      Coord = false (coordination value lost)
      Asym = true
    </to>
  </constraint_change>

  <chi_recalculation>
    <agent>X₃</agent>
    <before>χ = 0.8 × 1.0 × 0.8 = 0.64 → Tangled Rope</before>
    <after>χ = 0.85 × 1.0 × 0.8 = 0.68 → Tangled Rope (near boundary)</after>
    <note>
      Classification remains Tangled Rope but approaches Snare threshold.
      Loss of Coord flag means constraint no longer qualifies as Tangled
      (requires Coord=true). With Coord=false and χ=0.68, this becomes
      high-extraction constraint without coordination value.
    </note>
    <corrected_type>Snare (Coord flag failure disqualifies Tangled classification)</corrected_type>
  </chi_recalculation>

  <type_change>Tangled Rope → Snare (T2: coordination loss)</type_change>

  <preconditions>
    <condition>Measurement system accuracy degrades over time</condition>
    <condition>No maintenance/recalibration mechanism</condition>
    <condition>Extraction continues regardless of coordination value</condition>
  </preconditions>

  <outcome>
    X₃ now experiences C₃ as Snare (χ=0.68, Coord=false). Loses 
    justification for complicity (coordination value no longer exists).
    Constraint has degraded from hybrid to pure extraction.
  </outcome>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type III (Snare-as-Rope) — Missing Extraction

```xml
<error id="E₁">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X₃</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Tangled Rope (from X₃ index: χ=0.64)</actual_type>
  <perceived_type>Rope (misclassification)</perceived_type>
  
  <mechanism>
    Agent at moderate power position (π=1.0) experiences dampened extraction.
    Coordination value is genuine and visible. Asymmetric cost distribution
    exists but is not dominant from this index. Agent normalizes extraction
    component, focusing only on coordination value.
  </mechanism>
  
  <observable>
    Agent defends constraint as "necessary coordination mechanism" without
    acknowledging extraction. Does not investigate χ at powerless index.
    Resists proposals to reduce ε even when coordination value could be
    preserved at lower extraction.
  </observable>
  
  <structural_test>
    Check χ(C₃, powerless): χ = 0.96 → Snare
    Check χ(C₃, moderate): χ = 0.64 → Tangled Rope
    
    Divergence confirms: Same constraint is Snare for some agents.
    Agent claiming "pure Rope" is missing extraction visible from other indices.
  </structural_test>
  
  <correction_trigger>
    Exposure to powerless-index perspective. Recognition that χ=0.96 exists
    for other agents. Acknowledgment that coordination value does not
    eliminate extraction, only makes it hybrid.
  </correction_trigger>
  
  <consequence>
    Error perpetuates extraction by misclassifying hybrid as pure coordination.
    Blocks reform efforts (why fix what isn't broken?). Maintains complicity
    through normalization.
  </consequence>
</error>
```

---

### Error E₂: Type I (False Mountain) — C₂ Treated as Natural Law

```xml
<error id="E₂">
  <type>Type I: False Mountain</type>
  <agent>X₁, X₂, X₃, X₅</agent>
  <constraint>C₂</constraint>
  
  <actual_type>Mountain (from biographical/historical indices)</actual_type>
  <perceived_type>Mountain (correct from current index, error from civilizational index)</perceived_type>
  
  <mechanism>
    Founding choice is historical (outside biographical time horizon).
    All current agents correctly classify as Mountain given their indices.
    ERROR: Treating index-relative Mountain as absolute natural law.
    
    From civilizational index (T=civilizational, E=analytical):
    - Founding choice is changeable (can establish new zero-point)
    - Constraint is constructed (Asym=true, Coord=true)
    - Boltzmann test FAILS (asymmetric burden distribution)
  </mechanism>
  
  <observable>
    Agents do not attempt to establish alternative founding choice.
    Treat arbitrary zero-point as if it were physical necessity.
    Perceive C₃ reform as impossible because "any standard requires
    arbitrary foundation" (correct) therefore "all standards are equally
    flawed" (incorrect — some foundations are less extractive).
  </observable>
  
  <structural_test>
    Boltzmann test on C₂: FAIL (asymmetric costs)
    Time horizon test: Immutable at biographical, changeable at civilizational
    
    Constraint is Mountain from all current indices but NOT natural law.
    This is well-designed institution that appears as terrain.
  </structural_test>
  
  <correction_trigger>
    Access to civilizational time horizon (historical analysis).
    Recognition that founding choices can be remade.
    Understanding that "constructed" ≠ "easily changed" but also ≠ "unchangeable"
  </correction_trigger>
  
  <consequence>
    Error blocks reform of C₃ by making it appear to inherit immutability
    from C₂. Agents perceive extraction as inevitable consequence of
    measurement itself, rather than consequence of specific founding choice.
  </consequence>
</error>
```

---

### Error E₃: Type III (Snare-as-Rope) — Institutional Perspective

```xml
<error id="E₃">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X₄</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Rope (from X₄ index: χ=-0.144, net beneficiary)</actual_type>
  <perceived_type>Rope (correct from own index, error in universal claim)</perceived_type>
  
  <mechanism>
    Institutional agent has negative χ (extracts value FROM constraint).
    From this index, C₃ genuinely is coordination mechanism with no
    extraction burden. Agent experiences only benefits.
    
    ERROR: Generalizing own index to universal claim. Asserting that
    because χ(institutional) < 0, constraint is Rope for all agents.
  </mechanism>
  
  <observable>
    Agent claims constraint is "fair coordination mechanism" while
    simultaneously defending high suppression (Supp=0.8). Logical
    contradiction: Why does fair coordination require force?
    
    Agent resists transparency measures that would reveal χ(powerless)=0.96.
    Defends asymmetric cost distribution as "natural" or "efficient."
  </observable>
  
  <structural_test>
    Check χ across indices:
    - χ(institutional) = -0.144 → Rope (correct)
    - χ(powerless) = 0.96 → Snare (also correct)
    
    Both classifications are true. Error is claiming universal Rope status
    when indexical variance exists.
  </structural_test>
  
  <correction_trigger>
    Forced exposure to powerless-index experience.
    Recognition that negative χ for self implies positive χ for others
    (extraction must come from somewhere).
    Acknowledgment of indexical variance.
  </correction_trigger>
  
  <consequence>
    Error enables extraction by denying its existence. Institutional
    rationality (maximize utility) becomes justification for maintaining
    Snare for others while experiencing Rope for self.
  </consequence>
</error>
```

---

### Error E₄: Type II (Mountain Denial) — Attempting to Reform C₁

```xml
<error id="E₄">
  <type>Type II: Mountain Denial</type>
  <agent>Hypothetical reformer (any agent)</agent>
  <constraint>C₁</constraint>
  
  <actual_type>Mountain (certified by Boltzmann test)</actual_type>
  <perceived_type>Changeable constraint</perceived_type>
  
  <mechanism>
    Agent believes that with sufficient effort, measurement can capture
    true equilibrium state without disturbing it. Treats natural law
    (observer effect, uncertainty principle analog) as if it were
    constructed constraint that can be reformed.
  </mechanism>
  
  <observable>
    Agent attempts to develop "perfect measurement system" that eliminates
    observer effect. Invests resources in impossible goal. Experiences
    repeated failure but attributes to insufficient effort rather than
    fundamental impossibility.
  </observable>
  
  <structural_test>
    C₁ Boltzmann test: PASS (natural law)
    ε = 0.05 (minimal extraction from measurement act itself)
    Supp = 0.0 (no enforcement needed — physical necessity)
    
    Constraint is genuinely unchangeable. Reform attempts are energy
    depletion fighting terrain.
  </structural_test>
  
  <correction_trigger>
    Recognition of Boltzmann independence (classification invariant across
    all indices). Acceptance that some constraints are natural laws within
    narrative physics. Redirection of reform energy toward changeable
    constraints (C₃).
  </correction_trigger>
  
  <consequence>
    Error depletes reform energy on impossible goal. Distracts from
    changeable constraints. May lead to despair ("if even this cannot
    be changed, nothing can").
  </consequence>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

```xml
<rationality_model>
  <type>Bounded Institutional Rationality (BIR)</type>
  
  <justification>
    Constraint network exhibits:
    
    1. Principal-agent problems:
       - X₄ (institutional actor) is not monolithic
       - Internal agents may have different incentives than institution
       - Enforcement capacity (Supp=0.8) suggests imperfect control
    
    2. Uncertainty and risk aversion:
       - C₂ (founding choice) is hidden, suggesting information asymmetry
       - Agents operate under incomplete information about constraint structure
       - Reform attempts face uncertain outcomes
    
    3. Satisficing behavior:
       - X₃ accepts Tangled Rope (hybrid) rather than demanding pure Rope
       - Agents work within flawed system rather than revolutionary rupture
       - Negotiated ethics rather than optimal ethics
    
    4. Realistic institutional constraints:
       - Suppression is costly (Supp=0.8 requires resources)
       - Coordination has genuine value (not pure extraction)
       - System must "keep port fed" (background constraint C₄)
    
    Perfect Institutional Rationality (PIR) would imply:
    - X₄ maximizes extraction without bound → Deterministic Tragedy
    - No negotiation except Pareto-improving
    - Implacable system, no satisficing
    
    But network shows:
    - Extraction is high (ε=0.8) but not maximal
    - Coordination value persists (Coord=true)
    - System is exploitative but not optimally extractive
    
    This is realistic human institution, not algorithmic optimization.
  </justification>
  
  <implications>
    BIR enables attractors: Negotiated Equilibrium, Seeded Possibility
    BIR blocks attractor: Deterministic Tragedy (requires PIR)
    
    Institutional actor (X₄) will:
    - Defend extraction flow (rational given negative χ)
    - But accept negotiated reduction if cost of suppression exceeds value
    - Satisfice rather than maximize
    - Respond to principal-agent pressures, public legitimacy concerns
    
    This creates space for reform that PIR would eliminate.
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
    Surface: Constraint network persists, extraction continues
    Underground: Transformation seeds planted, alternative logic developing
    
    Visible outcome appears as tragedy or stasis.
    Invisible outcome is structural preparation for future transformation.
  </definition>
  
  <justification>
    Network structure supports Seeded Possibility:
    
    1. Mountain-dominated foundation (C₁, C₂):
       - C₁ is genuine Mountain (Boltzmann certified)
       - C₂ is False Mountain (appears unchangeable from current indices)
       - Mountains block Revolutionary Rupture (cannot disrupt natural law)
       - But C₂'s constructed nature creates underground possibility
    
    2. High-variance constraint (C₃):
       - Snare for powerless (χ=0.96)
       - Tangled Rope for moderate (χ=0.64)
       - Rope for institutional (χ=-0.144)
       - Indexical variance creates different lived realities
       - Surface: Extraction persists
       - Underground: Analytical recognition (TR₃) plants seeds
    
    3. Bounded Institutional Rationality:
       - X₄ is not implacable optimizer
       - Satisficing creates negotiation space
       - But current power balance favors extraction
       - Surface: Negotiation fails (insufficient power)
       - Underground: Recognition accumulates
    
    4. Transformation rules show blocked paths:
       - TR₁ (collective organization) blocked by C₂ and X₄
       - TR₂ (individual exit) blocked by resource depletion
       - TR₃ (analytical recognition) succeeds but lacks power
       - Surface: All transformations blocked
       - Underground: TR₃ creates analytical agents (X₂)
    
    5. Error manifestations reveal structure:
       - E₁, E₃: Extraction normalized by moderate/institutional agents
       - E₂: False Mountain blocks reform
       - E₄: Energy wasted on unchangeable
       - Surface: Errors perpetuate system
       - Underground: Error recognition is first step to correction
  </justification>
  
  <compatibility_check>
    Attractor Compatibility Matrix:
    - Constraint profile: Mountain + Snares + Tangled Ropes
    - Rationality model: BIR
    - Compatible attractors: Negotiated Equilibrium, Seeded Possibility
    
    Why not Negotiated Equilibrium?
    - Requires sufficient power balance for bargaining
    - X₁, X₅ (powerless) lack bargaining power
    - X₄ (institutional) has negative χ (rational to resist negotiation)
    - C₂ (False Mountain) blocks perception of negotiation space
    - Current power structure prevents equilibrium
    
    Why not Revolutionary Rupture?
    - C₁ (genuine Mountain) cannot be disrupted
    - C₂ (False Mountain) appears as terrain, blocks rupture perception
    - Mountain-dominated networks resist rupture
    
    Why not Deterministic Tragedy?
    - BIR (not PIR) creates satisficing space
    - Coordination value persists (not pure extraction)
    - Analytical recognition (TR₃) creates underground transformation
    
    Seeded Possibility is compatible and most structurally supported.
  </compatibility_check>
  
  <surface_outcome>
    C₃ persists as weaponized standard.
    X₁, X₅ remain trapped in Snare (χ=0.96).
    X₃ continues complicity in Tangled Rope (χ=0.64).
    X₄ maintains extraction flow (χ=-0.144).
    
    Visible narrative: Tragedy. System continues. Extraction persists.
  </surface_outcome>
  
  <underground_outcome>
    X₂ (analytical agents) accumulate.
    Recognition of C₂ as False Mountain spreads.
    Understanding of indexical variance develops.
    Error detection capacity increases.
    
    Seeds planted:
    - Knowledge that C₂ is constructed (changeable at civilizational scale)
    - Recognition of χ divergence (same constraint, different realities)
    - Analytical tools for detecting normalized extraction
    - Conceptual framework for alternative founding choices
    
    Future transformation potential:
    - When sufficient analytical agents exist
    - When generational time horizon becomes accessible
    - When collective organization overcomes current blocks
    - Alternative logic is ready to deploy
    
    Invisible narrative: Preparation. Structure for future change.
  </underground_outcome>
  
  <attractor_mechanics>
    Surface trajectory:
    t₀: C₃ operational, high extraction
    t₁: TR₁ attempted, blocked by C₂ + X₄
    t₂: TR₂ attempted, blocked by resource depletion
    t₃: System persists, appears stable
    
    Underground trajectory:
    t₀: X₁ experiences Snare, normalizes
    t₁: TR₃ triggers, X₁ → X₂ (analytical recognition)
    t₂: X₂ detects E₁, E₂, E₃ (error recognition)
    t₃: X₂ understands indexical variance, C₂ constructed nature
    t₄: Knowledge accumulates, waits for power shift
    
    Attractor basin:
    - Surface: Stable extraction equilibrium (appears as tragedy)
    - Underground: Unstable preparation state (seeds germinating)
    - Bifurcation point: When analytical agents reach critical mass
    - Future attractors: Negotiated Equilibrium or Revolutionary Rupture
      (but from transformed initial conditions)
  </attractor_mechanics>
</terminal_attractor>
```

---

## VI. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
  - C₁: ε=0.05, Supp=0.0, Coord=false, Asym=false
  - C₂: ε=0.2, Supp=0.05, Coord=true, Asym=true
  - C₃: ε=0.8, Supp=0.8, Coord=true, Asym=true

☑ All χ calculations shown with π and σ values
  - C₁: X₁ χ=0.075, X₂ χ=0.0575, X₃ χ=0.05
  - C₂: X₂ χ=0.23, X₃ χ=0.23, X₅ χ=0.24
  - C₃: X₁ χ=0.96, X₃ χ=0.64, X₄ χ=-0.144, X₅ χ=0.96

☑ All characters use variable names (X₁, X₂, X₃, X₄, X₅)
  - No source identifiers in body
  - Mapping table in header only

☑ No source occupation, setting, or domain vocabulary
  - No references to specific measurement systems
  - No references to specific commodities or trades
  - Abstract structural descriptions only

☑ No <experience>, <dialogue_markers>, or <narrative> fields
  - All descriptions in structural/mechanical terms
  - No phenomenological content

☑ Transformation rules are testable (IF-THEN with mechanical χ recalculation)
  - TR₁: Collective organization → π change → χ recalculation
  - TR₂: Individual exit → E change → outcome
  - TR₃: Analytical recognition → P change → χ recalculation
  - TR₄: Institutional suppression → Supp change → barrier increase
  - TR₅: Coordination loss → Coord flag change → type change

☑ Error types have observable patterns (testable conditions)
  - E₁: Defends constraint without acknowledging extraction
  - E₂: Does not attempt alternative founding choice
  - E₃: Claims universal Rope while defending high suppression
  - E₄: Attempts to reform natural law

☑ Institutional rationality model specified (BIR) with justification
  - Principal-agent problems identified
  - Satisficing behavior documented
  - Realistic institutional constraints noted

☑ Terminal attractor selected (Seeded Possibility), justified, compatible
  - Compatibility matrix checked
  - Surface/underground trajectories specified
  - Bifurcation mechanics described

☑ Indexical variance explicitly preserved across agents
  - C₃ shows four distinct classifications
  - Variance mechanism explained
  - Structural divergence documented

☑ Boltzmann test run for constraints claiming Mountain status
  - C₁: PASS (natural law certified)
  - C₂: FAIL (constructed, appears as Mountain from current indices)

☑ No ambiguity in specifications
  - All thresholds checked
  - All calculations shown
  - All classifications justified
```

---

## VII. NETWORK SUMMARY

```
Constraint Network: Three-constraint system with indexical variance engine

Foundation Layer:
  C₁ (Mountain, certified): Natural law, ε=0.05, Boltzmann PASS
  C₂ (False Mountain): Constructed, appears unchangeable, ε=0.2, Boltzmann FAIL

Operational Layer:
  C₃ (Indexically variant): ε=0.8, high suppression
    - Snare (powerless): χ=0.96
    - Tangled Rope (moderate): χ=0.64
    - Rope (institutional): χ=-0.144

Transformation Dynamics:
  - Collective organization (TR₁): Blocked by C₂ + X₄
  - Individual exit (TR₂): Blocked by resource depletion
  - Analytical recognition (TR₃): Succeeds, creates X₂
  - Institutional suppression (TR₄): Maintains extraction
  - Degradation (TR₅): Tangled → Snare trajectory

Error Landscape:
  - E₁: Moderate agent normalizes extraction
  - E₂: All agents treat C₂ as natural law
  - E₃: Institutional agent denies extraction exists for others
  - E₄: Hypothetical energy waste on unchangeable

Rationality Model: BIR (satisficing, principal-agent, uncertainty)

Terminal Attractor: Seeded Possibility
  - Surface: Extraction persists, appears as tragedy
  - Underground: Analytical recognition accumulates, prepares transformation
  - Bifurcation: When analytical agents reach critical mass

Critical Property: Indexical variance in C₃ creates different lived realities,
enabling simultaneous tragedy (powerless) and coordination (institutional).
This variance is the engine of the network's dynamics and the source of its
ethical complexity.
```

---

**END OF STAGE 1 FORMALIZATION**