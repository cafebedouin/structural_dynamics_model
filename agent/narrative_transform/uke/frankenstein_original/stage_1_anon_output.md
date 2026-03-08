# STAGE 1: FORMAL CONSTRAINT NETWORK

## Variable Mapping (Header — Traceability Only)

```
G₁ ← Scientific community
I₁ ← Social institutions (marriage, family, community)
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Obsessive Pursuit of Glory

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.70</epsilon>
    <suppression>0.20</suppression>
    <coordination>false</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.70 × 1.0 × 0.8 = 0.56</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ(0.56) ≤ 0.70 [PASS]
      Coord = false, but personal drive provides subjective coordination value
      Asym = true (extraction falls on agent, glory accrues to self)
    </threshold_check>
    <rationale>
      Mid-range extraction. Agent has moderate power to pursue or abandon 
      the drive, but exit is identity_locked (abandonment = ego death). 
      Local scope reduces verification difficulty. The drive extracts health, 
      social connection, and safety while providing subjective sense of purpose 
      and potential status elevation. Asymmetric because costs are borne 
      individually while benefits (if achieved) accrue to self.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>regional</scope>            <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.70 × 1.0 × 0.9 = 0.63</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ(0.63) ≤ 0.70 [PASS]
      Same structural position as X₁ with higher scope
    </threshold_check>
    <rationale>
      Higher χ than X₁ due to regional scope (larger-scale expedition, 
      harder verification of claims). Exit is constrained rather than 
      identity_locked (can abandon expedition at high reputational cost). 
      Same Tangled Rope classification: personal ambition extracts while 
      providing subjective coordination around life purpose.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.56 → Tangled Rope, X₂ χ=0.63 → Tangled Rope
    Minimal variance. Both agents experience mid-range extraction from 
    identity-fused ambition. Scope difference (local vs regional) shifts 
    χ but not type. At powerless index with trapped exit, would become 
    Snare (χ > 0.70). At institutional index, would become Rope (χ ≤ 0.35).
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by power position:
    - At (powerless, trapped): χ = 0.70 × 1.5 × 0.8 = 0.84 → Snare
    - At (moderate, identity_locked): χ = 0.56 → Tangled Rope
    - At (institutional, analytical): χ = 0.70 × (-0.2) × 0.8 = -0.112 → Rope
    
    Burden varies by structural position → constructed constraint, not natural law.
    Personal ambition is socially constructed drive, not physical necessity.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: The Creator's Duty

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.80</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.0 × 0.8 = 0.64</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ(0.64) ≤ 0.70 [PASS]
      Coord = true (genuine moral obligation exists)
      Asym = true (duty falls on creator, not distributed)
      High suppression (internal guilt + social enforcement)
    </threshold_check>
    <rationale>
      High base extraction (ε=0.80) moderated by local scope. Genuine 
      coordination value: moral framework for responsibility toward 
      created entities. Asymmetric because obligation is non-transferable. 
      High suppression reflects intense internal enforcement (guilt, horror) 
      plus social judgment. Identity_locked exit: fulfilling duty requires 
      confronting creation, which threatens ego integrity.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ(0.96) > 0.70 [PASS for Snare]
      ε(0.80) > 0.45 [confirms not low base extraction]
    </threshold_check>
    <rationale>
      Same constraint, radically different experience. X₃ is powerless 
      (cannot compel creator to act), trapped (no alternative source of 
      care/recognition), local scope. High extraction: needs care, receives 
      abandonment. Coordination value exists in principle but is inaccessible 
      from this index. Classic Snare: structural position amplifies extraction 
      beyond threshold.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.64 → Tangled Rope, X₃ χ=0.96 → Snare
    
    CRITICAL DIVERGENCE. Same constraint classified as two different types 
    based purely on structural position. X₁ experiences moral obligation 
    with coordination value (Tangled Rope). X₃ experiences abandonment 
    trap (Snare). This is not perception difference — it's structural 
    reality. The duty extracts differently depending on whether you're 
    the obligated party (moderate power, can choose to flee) or the 
    dependent party (powerless, cannot escape need).
    
    Demonstrates core indexed relativity: both classifications are 
    objectively true from their respective indices.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by power position:
    - At (powerless, trapped): χ = 0.96 → Snare
    - At (moderate, identity_locked): χ = 0.64 → Tangled Rope
    - At (powerful, mobile): χ = 0.80 × 0.6 × 0.8 = 0.384 → Rope
    
    Moral obligations are socially constructed, not natural laws. 
    Burden varies by structural position → constructed constraint.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Social Exclusion of the Aberrant

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.85</epsilon>
    <suppression>0.60</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₃">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>regional</scope>            <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.85 × 1.5 × 0.9 = 1.1475</chi>
    <type>Snare</type>
    <threshold_check>
      χ(1.15) > 0.70 [PASS for Snare]
      ε(0.85) > 0.45 [confirms high base extraction]
    </threshold_check>
    <rationale>
      Extreme extraction. X₃ is powerless (cannot force acceptance), 
      trapped (physical appearance prevents exit to alternative communities), 
      regional scope (exclusion operates across geographic area). High 
      base extraction: denial of social recognition, safety, economic 
      participation. Coordination value exists for in-group (boundary 
      maintenance) but is pure extraction for excluded party. Moderate 
      suppression: enforced through social mechanisms, not institutional 
      violence.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>organized</power>          <!-- π = 0.4 -->
      <time>generational</time>
      <exit>mobile</exit>
      <scope>regional</scope>            <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.85 × 0.4 × 0.9 = 0.306</chi>
    <type>Rope</type>
    <threshold_check>
      χ(0.306) ≤ 0.35 [PASS for Rope]
      ε(0.85) > 0.45 but χ ≤ 0 bypasses ε ceiling [N/A — χ > 0]
      ALERT: ε(0.85) > 0.45 with χ(0.306) ≤ 0.35
      → investigate_naturalization
    </threshold_check>
    <rationale>
      Same exclusion mechanism, opposite experience. X₄ (organized community) 
      has collective power to set boundaries, mobile exit (can leave if 
      exclusion becomes burdensome), generational time horizon (norms 
      appear changeable). Low χ despite high ε suggests extraction is 
      being absorbed/hidden by power position. Coordination value: group 
      cohesion, norm enforcement, boundary maintenance. Asymmetric because 
      costs fall on excluded, benefits accrue to in-group.
      
      NATURALIZATION DETECTED: High base extraction (0.85) experienced 
      as low effective extraction (0.306) by powerful agents. Classic 
      pattern of normalized oppression.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₃ χ=1.15 → Snare, X₄ χ=0.306 → Rope
    
    MAXIMUM DIVERGENCE. Same constraint: Snare for excluded, Rope for 
    excluders. This is the structural signature of oppression — what 
    appears as beneficial coordination to the powerful is experienced 
    as inescapable extraction by the powerless. The χ calculation makes 
    this visible: 1.15 vs 0.306 from the same ε(0.85).
    
    X₄'s classification as Rope is technically correct from their index 
    but triggers naturalization investigation. The low χ is real (they 
    genuinely experience low burden) but the high ε reveals the extraction 
    is being deflected onto X₃.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by power position:
    - At (powerless, trapped): χ = 1.15 → Snare
    - At (organized, mobile): χ = 0.306 → Rope
    - At (analytical, analytical): χ = 0.85 × 1.15 × 0.9 = 0.879 → Snare
    
    Social exclusion is constructed, not natural law. Analyst detects 
    Snare where community normalizes as Rope. Burden varies by structural 
    position → constructed constraint.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₄: Isolation as Self-Protection

```xml
<constraint id="C₄">
  <properties>
    <epsilon>0.55</epsilon>
    <suppression>0.15</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.55 × 1.0 × 0.8 = 0.44</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ(0.44) ≤ 0.70 [FAIL — χ below floor]
      Reclassify: χ(0.44) > 0.35 → not Rope
      χ(0.44) < 0.46 → not Tangled Rope by threshold
      EDGE CASE: χ = 0.44 in gap between Rope ceiling (0.35) and 
      Tangled floor (0.46)
      
      Resolution: Coord = false disqualifies Rope. Asym = false suggests 
      self-imposed constraint. Classify as Tangled Rope (lower bound) 
      due to moderate extraction with subjective coordination value 
      (protection from judgment).
    </threshold_check>
    <rationale>
      Self-imposed isolation to avoid social judgment. Moderate extraction: 
      loses social connection, support, intimacy. Low suppression: 
      self-enforced, minimal external pressure. No formal coordination 
      but provides subjective protection value. Exit is constrained 
      (can reconnect but at cost of vulnerability). Non-asymmetric 
      because agent chooses isolation and bears its costs.
      
      EDGE CASE NOTE: χ = 0.44 falls in threshold gap. Classification 
      as Tangled Rope (lower bound) reflects that isolation provides 
      protection value while extracting connection — hybrid structure 
      even though Coord = false formally.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>regional</scope>            <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.55 × 1.5 × 0.9 = 0.7425</chi>
    <type>Snare</type>
    <threshold_check>
      χ(0.74) > 0.70 [PASS for Snare]
      ε(0.55) > 0.45 [confirms not low base extraction]
    </threshold_check>
    <rationale>
      Same isolation mechanism, different structural position. X₃ is 
      powerless (cannot force social acceptance), trapped (exclusion 
      by C₃ makes isolation involuntary), regional scope. What appears 
      as self-protection for X₁ becomes forced exile for X₃. High 
      extraction: complete social deprivation. Crosses Snare threshold 
      due to power amplification.
    </rationale>
  </agent>

  <indexical_variance>
    C₄: X₁ χ=0.44 → Tangled Rope (edge), X₃ χ=0.74 → Snare
    
    Moderate divergence. X₁ experiences isolation as protective choice 
    with moderate cost. X₃ experiences isolation as forced condition 
    with high extraction. The constraint itself (withdrawal from social 
    contact) is identical, but structural position determines whether 
    it's chosen protection or imposed deprivation.
    
    Demonstrates how agency transforms constraint type: same behavior, 
    different χ based on power and exit options.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by power position:
    - At (moderate, constrained): χ = 0.44 → Tangled Rope
    - At (powerless, trapped): χ = 0.74 → Snare
    
    Social isolation is behavioral pattern, not natural law. Burden 
    varies by whether isolation is chosen or imposed → constructed 
    constraint.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₅: Secrecy Around Transgressive Knowledge

```xml
<constraint id="C₅">
  <properties>
    <epsilon>0.75</epsilon>
    <suppression>0.85</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × 1.0 × 0.8 = 0.60</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ(0.60) ≤ 0.70 [PASS]
      Coord = true (prevents dangerous knowledge spread)
      Asym = true (burden on knowledge-holder)
      High suppression (guilt + fear of consequences)
    </threshold_check>
    <rationale>
      Obligation to conceal transgressive knowledge. High base extraction: 
      isolation, inability to seek help, psychological burden of secret. 
      Genuine coordination value: prevents replication of dangerous 
      experiment. High suppression: internal guilt plus fear of social 
      judgment. Identity_locked exit: revealing secret = admitting 
      transgression. Asymmetric because burden falls on knowledge-holder, 
      benefit (safety) accrues to community.
    </rationale>
  </agent>

  <agent ref="G₁">
    <index>
      <power>organized</power>          <!-- π = 0.4 -->
      <time>generational</time>
      <exit>mobile</exit>
      <scope>national</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 0.75 × 0.4 × 1.0 = 0.30</chi>
    <type>Rope</type>
    <threshold_check>
      χ(0.30) ≤ 0.35 [PASS for Rope]
      ε(0.75) > 0.45 but χ ≤ 0.35 requires ε ≤ 0.45 [FAIL]
      ALERT: ε(0.75) > 0.45 with χ(0.30) ≤ 0.35
      → investigate_naturalization
    </threshold_check>
    <rationale>
      Same secrecy norm, experienced by organized scientific community. 
      Collective power to set research boundaries, mobile exit (can 
      leave field), generational time horizon (norms appear changeable), 
      national scope. Low χ despite high ε: extraction absorbed by 
      power position. Coordination value: research safety, ethical 
      boundaries, professional standards.
      
      NATURALIZATION DETECTED: High base extraction (0.75) experienced 
      as low effective extraction (0.30) by organized community. The 
      burden of secrecy falls on individual transgressors while community 
      experiences it as beneficial norm.
    </rationale>
  </agent>

  <indexical_variance>
    C₅: X₁ χ=0.60 → Tangled Rope, G₁ χ=0.30 → Rope
    
    Significant divergence. Individual knowledge-holder experiences 
    high extraction (isolation, guilt, fear). Organized community 
    experiences low extraction (beneficial professional norm). Same 
    constraint: Tangled Rope for transgressor, Rope for institution.
    
    Demonstrates how collective organization deflects extraction onto 
    individuals. G₁'s Rope classification triggers naturalization 
    investigation — the low χ is real but the high ε reveals asymmetric 
    burden distribution.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by power position:
    - At (moderate, identity_locked): χ = 0.60 → Tangled Rope
    - At (organized, mobile): χ = 0.30 → Rope
    - At (analytical, analytical): χ = 0.75 × 1.15 × 0.8 = 0.69 → Tangled Rope
    
    Secrecy norms are socially constructed, not natural laws. Burden 
    varies by structural position → constructed constraint.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Against Exclusion

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective_resistance</condition>
    <target>C₃</target>
    <agent>X₃</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.85 × 1.5 × 0.9 = 1.15 → Snare</before>
    <after>χ = 0.85 × 0.4 × 0.9 = 0.306 → Rope</after>
  </chi_recalculation>
  
  <type_change>Snare → Rope</type_change>
  
  <preconditions>
    - Multiple agents experiencing C₃ as Snare
    - Communication channels available
    - Sufficient resources for collective action
    - Time horizon ≥ generational (immediate horizon sees exclusion as Mountain)
  </preconditions>
  
  <blocked_by>
    - C₄ (isolation prevents coordination)
    - C₃ itself (exclusion prevents assembly)
    - Insufficient agent count (single agent cannot organize collective)
  </blocked_by>
  
  <notes>
    Classic transformation: powerless → organized changes π from 1.5 to 0.4.
    Same constraint (social exclusion) experienced as Rope instead of Snare.
    Does NOT eliminate exclusion — transforms structural position within it.
    Requires breaking C₄ (isolation) first to enable coordination.
  </notes>
</transformation_rule>
```

---

### TR₂: Revelation of Transgressive Knowledge

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>disclose_concealed_knowledge</condition>
    <target>C₅</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>E = identity_locked, Supp(C₅) = 0.85</from>
    <to>E = constrained, Supp(C₅) = 0.40</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.75 × 1.0 × 0.8 = 0.60 → Tangled Rope</before>
    <after>χ = 0.75 × 1.0 × 0.8 = 0.60 → Tangled Rope</after>
    <note>χ unchanged (same ε, π, σ) but suppression drops</note>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Tangled Rope (suppression reduced)</type_change>
  
  <preconditions>
    - Agent willing to accept social judgment
    - Audience available to receive disclosure
    - Time horizon ≥ biographical (immediate horizon sees judgment as unbearable)
  </preconditions>
  
  <blocked_by>
    - C₁ (glory-seeking prevents admission of failure)
    - C₄ (isolation prevents disclosure opportunity)
    - Fear of C₂ enforcement (judgment for abandoning duty)
  </blocked_by>
  
  <notes>
    Revelation reduces suppression (guilt → judgment) but doesn't change χ.
    Exit shifts from identity_locked to constrained (can now leave situation
    but at cost of reputation). Type remains Tangled Rope because ε, π, σ
    unchanged — only enforcement mechanism shifts from internal to external.
    
    This is NOT a type transformation — it's a suppression mechanism shift
    within the same type. Included because it's a critical decision point
    that affects other constraints (enables C₂ enforcement, blocks C₁ pursuit).
  </notes>
</transformation_rule>
```

---

### TR₃: Abandonment of Glory Pursuit

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>renounce_ambition</condition>
    <target>C₁</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>E = identity_locked, T = biographical</from>
    <to>E = mobile, T = immediate</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.70 × 1.0 × 0.8 = 0.56 → Tangled Rope</before>
    <after>χ = 0.70 × 1.0 × 0.8 = 0.56 → Tangled Rope</after>
    <note>χ unchanged but constraint becomes inactive (no longer pursued)</note>
  </chi_recalculation>
  
  <type_change>Tangled Rope → [inactive] (constraint no longer binding)</type_change>
  
  <preconditions>
    - Alternative identity available (exit from identity_lock requires replacement)
    - Time horizon shift to immediate (biographical goals become irrelevant)
    - Acceptance of status loss
  </preconditions>
  
  <blocked_by>
    - C₁ itself (identity_locked exit prevents abandonment)
    - C₅ (secrecy prevents alternative identity formation)
    - Sunk cost (biographical investment makes abandonment costly)
  </blocked_by>
  
  <notes>
    This transformation doesn't change χ — it exits the constraint entirely.
    Identity_locked → mobile requires ego death (abandoning core identity).
    Time horizon shift to immediate makes biographical goals irrelevant.
    
    CRITICAL: This is the transformation X₁ cannot execute. The constraint
    blocks its own exit. C₁ is self-reinforcing through identity fusion.
  </notes>
</transformation_rule>
```

---

### TR₄: Fulfillment of Creator's Duty

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>provide_care_to_created_entity</condition>
    <target>C₂</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>Supp(C₂) = 0.90, ε(C₂) = 0.80</from>
    <to>Supp(C₂) = 0.30, ε(C₂) = 0.50</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.80 × 1.0 × 0.8 = 0.64 → Tangled Rope</before>
    <after>χ = 0.50 × 1.0 × 0.8 = 0.40 → Rope</after>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Rope</type_change>
  
  <preconditions>
    - Agent accepts moral obligation
    - Created entity accessible
    - Resources available for care provision
  </preconditions>
  
  <blocked_by>
    - C₁ (glory pursuit prevents duty fulfillment)
    - C₄ (isolation prevents contact)
    - C₅ (secrecy prevents acknowledgment of creation)
    - C₃ (social exclusion of created entity makes care socially costly)
  </blocked_by>
  
  <notes>
    Fulfilling duty reduces both ε (less extraction from guilt) and 
    suppression (less enforcement needed). χ drops from 0.64 to 0.40,
    crossing threshold from Tangled Rope to Rope.
    
    For X₃: Same transformation would shift from Snare (χ=0.96) to 
    Tangled Rope (χ=0.60) — still extractive but coordination value 
    becomes accessible.
    
    CRITICAL: This transformation is blocked by four other constraints.
    The network structure prevents duty fulfillment. This is the core
    tragedy mechanism.
  </notes>
</transformation_rule>
```

---

### TR₅: Constraint Coupling (C₁ + C₅ → Recursive Trap)

```xml
<transformation_rule id="TR₅">
  <trigger>
    <condition>pursue_glory_through_transgressive_means</condition>
    <target>C₁, C₅</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>C₁ and C₅ independent</from>
    <to>C₁ and C₅ coupled (C₁ requires C₅, C₅ blocks C₁ completion)</to>
  </index_change>
  
  <chi_recalculation>
    <before>
      C₁: χ = 0.56 → Tangled Rope
      C₅: χ = 0.60 → Tangled Rope
      Combined: χ_eff = 0.56 (constraints independent)
    </before>
    <after>
      C₁: χ = 0.56 → Tangled Rope
      C₅: χ = 0.60 → Tangled Rope
      Combined: χ_eff = 0.56 + (0.60 × 0.7) = 0.98 → Snare
      (coupling coefficient 0.7 reflects partial overlap)
    </after>
  </chi_recalculation>
  
  <type_change>
    Individual constraints: Tangled Rope + Tangled Rope
    Coupled system: Snare (χ_eff > 0.70)
  </type_change>
  

<!-- ANONYMIZATION: 2 character names replaced: Victor Frankenstein -> Agent_A, Robert Walton -> Agent_B -->
