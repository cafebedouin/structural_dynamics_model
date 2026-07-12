# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Observer-Dependent Truth

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.10</epsilon>
    <suppression>0.00</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.5 × 0.9 = 0.135</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.10 ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp = 0.00 ≤ 0.05 (mountain_suppression_ceiling): PASS
      Immutable from biographical horizon: PASS
    </threshold_check>
    <rationale>
      Epistemological constraint on measurement systems. Zero suppression 
      indicates natural emergence. Low extraction reflects inherent 
      uncertainty cost rather than constructed burden. Immutable from 
      biographical time horizon — no individual can alter the fundamental 
      observer-dependence of truth claims.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.15 × 0.9 = 0.104</chi>
    <type>Mountain</type>
    <threshold_check>
      ε = 0.10 ≤ 0.25: PASS
      Supp = 0.00 ≤ 0.05: PASS
      Immutable from biographical horizon: PASS
    </threshold_check>
    <rationale>
      Analytical position provides no escape from fundamental epistemological 
      constraint. Can analyze the constraint's structure but cannot alter 
      observer-dependence itself. Classification stable across power positions.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.135 → Mountain, X₃ χ=0.104 → Mountain
    No variance. Classification stable across all tested indices. Natural law 
    of measurement systems — affects all observers equally regardless of power 
    position or scope.
  </indexical_variance>

  <boltzmann_test>
    PASS
    
    Test matrix:
    - (powerless, regional): χ = 0.135 → Mountain
    - (analytical, regional): χ = 0.104 → Mountain
    - (moderate, regional): χ = 0.090 → Mountain
    - (institutional, regional): χ = -0.018 → Mountain (net benefit irrelevant)
    
    Classification invariant across Power dimension. Burden does not vary by 
    structural position — all agents face same epistemological constraint. 
    Scope variation would not change classification (natural laws are 
    scope-invariant). Constraint is genuinely natural, not constructed.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Institutionalized Falsehood

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
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.9 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 1.08 > 0.70 (snare_chi_threshold): PASS
      ε = 0.80 > 0.45 (not low base extraction): PASS
      High suppression (0.90) confirms enforcement requirement
    </threshold_check>
    <rationale>
      Standardized measurement system with embedded falsehood. High extraction 
      (ε=0.80) amplified by powerless position. Identity-locked exit means 
      professional identity fused to system — cannot leave without abandoning 
      occupation. High suppression indicates active enforcement maintaining 
      false standard. Coordination exists but dominated by extraction.
    </rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.9 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>
      χ = 1.08 > 0.70: PASS
      ε = 0.80 > 0.45: PASS
      Trapped exit (economic dependence) reinforces Snare classification
    </threshold_check>
    <rationale>
      Collective agent subject to same measurement system. Economic dependence 
      (trapped exit) prevents escape. System extracts value through false 
      standard while providing minimal coordination benefit. Cannot organize 
      alternative due to C₅ (offstage atomization constraint).
    </rationale>
  </agent>

  <agent ref="X₅">
    <index>
      <power>institutional</power>        <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × (-0.2) × 0.9 = -0.144</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.144 ≤ 0 (net beneficiary): PASS
      ε ceiling bypassed (χ ≤ 0 condition)
      Coordination = true: system provides standardization function
    </threshold_check>
    <rationale>
      Institutional position extracts FROM system rather than being extracted 
      from. Negative χ indicates net benefit. Generational time horizon and 
      arbitrage exit options provide strategic flexibility. System functions 
      as coordination mechanism from this index — standardization enables 
      taxation and control. Asymmetry invisible from beneficiary position.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=1.08 → Snare, X₄ χ=1.08 → Snare, X₅ χ=-0.144 → Rope
    
    High variance. Same constraint experienced as extractive trap by subjects 
    (powerless agents with identity-locked or trapped exits) and as beneficial 
    coordination tool by institutional beneficiary. Power position determines 
    whether agent bears extraction cost or captures extraction benefit. 
    
    Critical structural feature: Asymmetry (true) + high suppression (0.90) 
    indicates active enforcement maintaining differential burden distribution.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    
    Classification varies by Power dimension:
    - powerless: χ = 1.08 → Snare
    - institutional: χ = -0.144 → Rope
    
    Burden distribution depends on structural position. This is a constructed 
    system, not natural law. The false standard is maintained through 
    institutional enforcement (high suppression), not natural emergence.
    
    Implication: Despite low ε relative to pure extraction systems, this 
    constraint is definitively constructed. The Boltzmann failure certifies 
    it as institutional rather than natural.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Pragmatic Complicity

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.52</epsilon>
    <suppression>0.60</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₂">
    <index>
      <power>moderate</power>             <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.52 × 1.0 × 0.9 = 0.468</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      χ = 0.468: 0.46 ≤ χ ≤ 0.70 (tangled range): PASS
      Coord = true: PASS
      Asym = true: PASS
    </threshold_check>
    <rationale>
      Social pressure to uphold flawed system for stability. Moderate power 
      position provides some agency but identity-locked exit prevents escape 
      without professional cost. χ in mid-range indicates genuine hybrid: 
      coordination value (system stability, social function) mixed with 
      extraction cost (moral compromise, complicity burden). Not confused 
      Rope or disguised Snare — irreducible mixture.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>             <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.52 × 1.0 × 0.9 = 0.468</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      χ = 0.468: 0.46 ≤ χ ≤ 0.70: PASS
      Coord = true: PASS
      Asym = true: PASS
    </threshold_check>
    <rationale>
      Analytical capacity does not escape complicity pressure. Can analyze 
      the system's flaws but still subject to social enforcement maintaining 
      false standard. Identity-locked to professional role. Coordination 
      benefit (maintaining social function) real but extraction cost 
      (upholding known falsehood) also real.
    </rationale>
  </agent>

  <agent ref="X₅">
    <index>
      <power>institutional</power>        <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>             <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.52 × (-0.2) × 0.9 = -0.094</chi>
    <type>Rope</type>
    <threshold_check>
      χ = -0.094 ≤ 0 (net beneficiary): PASS
      ε ceiling bypassed (χ ≤ 0 condition)
      Coord = true: system ensures compliance
    </threshold_check>
    <rationale>
      Institutional position benefits from complicity pressure. Negative χ 
      indicates net benefit — social enforcement maintains system stability 
      without institutional cost. Generational horizon and arbitrage exit 
      provide strategic flexibility. Complicity pressure functions as 
      coordination mechanism from this index, ensuring subjects uphold 
      false standard without requiring direct enforcement.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₂ χ=0.468 → Tangled Rope, X₃ χ=0.468 → Tangled Rope, 
        X₅ χ=-0.094 → Rope
    
    Moderate variance. Administrators experience genuine hybrid (coordination 
    + extraction). Institutional beneficiary experiences pure coordination 
    (complicity pressure as enforcement mechanism). 
    
    Key structural feature: Downstream of C₂. Complicity pressure emerges 
    as response to institutionalized falsehood. Cannot exist independently — 
    requires upstream false standard to generate moral cost.
  </indexical_variance>

  <boltzmann_test>
    FAIL
    
    Classification varies by Power dimension:
    - moderate: χ = 0.468 → Tangled Rope
    - institutional: χ = -0.094 → Rope
    
    Burden distribution depends on structural position. Administrators bear 
    moral cost of complicity; institutional beneficiary captures stability 
    benefit without cost. This is a constructed social enforcement mechanism, 
    not natural law.
    
    Implication: Social pressure maintaining false standard is institutional 
    construct. The complicity burden is asymmetrically distributed by design.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Collective Organization Attempt

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective_alternative</condition>
    <target>C₂</target>
    <agent>X₄</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.80 × 1.5 × 0.9 = 1.08 → Snare</before>
    <after>χ = 0.80 × 0.4 × 0.9 = 0.288 → Rope</after>
  </chi_recalculation>
  
  <type_change>Snare → Rope</type_change>
  
  <preconditions>
    - Sufficient collective coordination capacity
    - Alternative measurement system viable
    - Collective can absorb transition costs
  </preconditions>
  
  <blocked_by>
    C₅ (offstage): Atomized Resistance
    
    Offstage constraint prevents collective organization. Individual agents 
    cannot coordinate to form organized collective. Transformation rule 
    exists formally but is structurally unreachable given constraint network.
    
    If C₅ were removed: χ recalculation shows Snare → Rope transformation 
    mechanically follows from power position change. Collective burden-sharing 
    (π=0.4) reduces effective extraction below rope_chi_ceiling (0.35).
  </blocked_by>
</transformation_rule>
```

---

### TR₂: Exit Attempt (Individual)

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>abandon_professional_identity</condition>
    <target>C₂</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>E = identity_locked</from>
    <to>E = mobile (if successful) OR E = identity_locked (if failed)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.80 × 1.5 × 0.9 = 1.08 → Snare</before>
    <after>
      If exit successful: Agent no longer subject to C₂
      If exit failed: χ unchanged, identity-lock reinforced
    </after>
  </chi_recalculation>
  
  <type_change>
    Snare → (not subject) if successful
    Snare → Snare if failed
  </type_change>
  
  <preconditions>
    - Alternative livelihood available
    - Willing to abandon professional identity
    - Can absorb economic transition cost
  </preconditions>
  
  <blocked_by>
    Economic dependence (for X₄: trapped exit)
    Identity fusion (for X₁: identity_locked exit)
    
    Individual exit possible in principle but high-cost. Does not transform 
    constraint itself — only removes individual agent from constraint's scope. 
    C₂ persists for remaining agents.
  </blocked_by>
</transformation_rule>
```

---

### TR₃: Institutional Reform Attempt

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>reform_measurement_standard</condition>
    <target>C₂</target>
    <agent>X₅</agent>
  </trigger>
  
  <index_change>
    <from>ε = 0.80, Supp = 0.90</from>
    <to>ε = 0.30, Supp = 0.40 (if successful)</to>
  </index_change>
  
  <chi_recalculation>
    <before>
      X₁: χ = 0.80 × 1.5 × 0.9 = 1.08 → Snare
      X₅: χ = 0.80 × (-0.2) × 0.9 = -0.144 → Rope
    </before>
    <after>
      X₁: χ = 0.30 × 1.5 × 0.9 = 0.405 → Tangled Rope
      X₅: χ = 0.30 × (-0.2) × 0.9 = -0.054 → Rope
    </after>
  </chi_recalculation>
  
  <type_change>
    X₁: Snare → Tangled Rope
    X₅: Rope → Rope (still net beneficiary)
  </type_change>
  
  <preconditions>
    - Institutional agent recognizes extraction cost
    - Alternative standard available
    - Transition cost acceptable to institutional agent
    - No dependency on extraction revenue
  </preconditions>
  
  <blocked_by>
    C₄ (offstage): The Ungrounded Choice
    
    Offstage constraint establishes that all measurement systems rest on 
    arbitrary initial choice. Reforming C₂ would require selecting new 
    arbitrary standard — problem is universal, not local. Institutional 
    agent has no incentive to reform (χ < 0, net beneficiary). Reform 
    structurally blocked by combination of:
    1. Beneficiary position (no incentive)
    2. Universal arbitrariness (no "correct" alternative)
    3. Transition cost (disruption to existing coordination)
  </blocked_by>
</transformation_rule>
```

---

### TR₄: Complicity Escalation

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>normalize_false_standard</condition>
    <target>C₃</target>
    <agent>X₂</agent>
  </trigger>
  
  <index_change>
    <from>Supp(C₃) = 0.60</from>
    <to>Supp(C₃) = 0.80</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.52 × 1.0 × 0.9 = 0.468 → Tangled Rope</before>
    <after>χ unchanged (suppression affects enforcement, not χ directly)</after>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Tangled Rope (type stable, enforcement increases)</type_change>
  
  <preconditions>
    - Agent accepts complicity as necessary
    - Social pressure internalized
    - Alternative paths foreclosed
  </preconditions>
  
  <blocked_by>None — this is the natural drift direction</blocked_by>
  
  <note>
    This transformation represents T1 (Rope degradation) applied to already-
    Tangled constraint. Increased suppression indicates stronger social 
    enforcement of complicity norm. Does not change χ but increases cost 
    of deviation. Terminal state: agent fully normalized to false standard.
  </note>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type III (Snare-as-Rope) — Missing Extraction

```xml
<error id="E₁">
  <type>III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₅</agent>
  <constraint>C₂</constraint>
  <actual_type>Snare (from X₁, X₄ indices: χ=1.08)</actual_type>
  <perceived_type>Rope (from X₅ index: χ=-0.144)</perceived_type>
  <observable>
    Agent X₅ treats measurement system as pure coordination mechanism. 
    Does not recognize extraction burden on X₁, X₄. Resists reform proposals 
    citing "disruption to standardization." Frames subject complaints as 
    individual incompetence rather than systemic extraction.
  </observable>
  <correction_trigger>
    Index shift to powerless position OR
    Forced recognition of χ(powerless) > 0.70 through:
      - Direct testimony from X₁, X₄
      - Measurement of extraction costs
      - Comparison with alternative systems showing lower ε
  </correction_trigger>
  <structural_note>
    Classic beneficiary blindness. Negative χ from institutional position 
    makes extraction invisible. Agent experiences only coordination benefit. 
    Error is structurally determined by index, not cognitive failure.
  </structural_note>
</error>
```

---

### Error E₂: Type V.a (Tangled-as-Rope) — Ignoring Extraction Component

```xml
<error id="E₂">
  <type>V.a — Tangled-as-Rope (Ignoring Extraction Component)</type>
  <agent>X₂</agent>
  <constraint>C₃</constraint>
  <actual_type>Tangled Rope (χ=0.468, genuine hybrid)</actual_type>
  <perceived_type>Rope (treating as pure coordination)</perceived_type>
  <observable>
    Agent X₂ frames complicity pressure as "professional responsibility" 
    without acknowledging moral cost. Treats upholding false standard as 
    neutral coordination rather than asymmetric burden. Does not recognize 
    extraction component (ε=0.52) in complicity norm.
  </observable>
  <correction_trigger>
    Recognition of moral cost through:
      - Explicit comparison with agents not subject to C₃
      - Measurement of complicity burden (time, energy, psychological cost)
      - Acknowledgment that coordination could exist without false standard
  </correction_trigger>
  <structural_note>
    Common error for moderate-power agents in Tangled Ropes. Coordination 
    benefit is salient; extraction cost is normalized. Agent rationalizes 
    hybrid as pure Rope to reduce cognitive dissonance. Error enables 
    continued participation in asymmetric system.
  </structural_note>
</error>
```

---

### Error E₃: Type II (Mountain Denial) — Treating Natural Law as Changeable

```xml
<error id="E₃">
  <type>II — Mountain Denial</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  <actual_type>Mountain (χ=0.135, Boltzmann-certified natural law)</actual_type>
  <perceived_type>Rope (treating as reformable coordination mechanism)</perceived_type>
  <observable>
    Agent X₁ attempts to establish "objective" measurement system that 
    eliminates observer-dependence. Invests energy in seeking universal 
    standard that transcends perspective. Treats epistemological constraint 
    as institutional failure rather than natural law.
  </observable>
  <correction_trigger>
    Recognition of immutability through:
      - Repeated failure of objectivity attempts
      - Formal proof of observer-dependence in measurement
      - Acknowledgment that C₁ passes Boltzmann test (natural, not constructed)
  </correction_trigger>
  <structural_note>
    Energy depletion error. Agent fights unchangeable constraint. Confusion 
    arises because C₁ (natural law) and C₂ (constructed falsehood) both 
    involve measurement systems. Agent misattributes C₂'s extractiveness 
    to C₁, attempting to "fix" natural law rather than reform institution.
  </structural_note>
</error>
```

---

### Error E₄: Type I (False Mountain) — Treating Constructed System as Natural

```xml
<error id="E₄">
  <type>I — False Mountain</type>
  <agent>X₄</agent>
  <constraint>C₂</constraint>
  <actual_type>Snare (χ=1.08, Boltzmann-failed constructed system)</actual_type>
  <perceived_type>Mountain (treating as unchangeable natural law)</perceived_type>
  <observable>
    Agent X₄ treats false measurement standard as inevitable feature of 
    commerce. Does not attempt collective organization (TR₁) despite 
    mechanical feasibility. Frames extraction as "cost of doing business" 
    rather than institutional choice. Accepts trapped exit as natural 
    constraint rather than constructed barrier.
  </observable>
  <correction_trigger>
    Recognition of constructedness through:
      - Exposure to alternative measurement systems
      - Historical evidence of standard's institutional origin
      - Boltzmann test showing classification varies by power position
      - Demonstration that organized collective (π=0.4) would transform Snare → Rope
  </correction_trigger>
  <structural_note>
    Naturalization error. Agent at (powerless, trapped) index experiences 
    constraint as immutable. Immediate time horizon + trapped exit make 
    changeability invisible. Error is index-determined: from this position, 
    C₂ genuinely appears as Mountain. Correction requires index shift or 
    external evidence of constructedness.
  </structural_note>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

**Justification:**

1. **Principal-agent structure present:** X₅ (institutional agent) operates through intermediaries (X₂, X₃) who have partial autonomy and information asymmetry.

2. **Satisficing behavior observed:** X₅ maintains false standard (C₂) not because it maximizes utility but because it satisfices — provides "good enough" coordination while generating extraction revenue. Reform (TR₃) would be Pareto-improving for subjects but involves transition cost and uncertainty.

3. **Risk aversion:** X₅ prefers stable extraction from known false standard over uncertain benefits of reform. Generational time horizon suggests institutional conservatism.

4. **Bounded rationality of subjects:** X₁, X₄ exhibit naturalization error (E₄), treating constructed system as unchangeable. This is not perfect rationality — it's bounded by index position and information constraints.

5. **Negotiation space exists:** Unlike PIR (which tends toward Deterministic Tragedy), BIR allows for:
   - Partial reforms (ε reduction without full elimination)
   - Bargaining over suppression levels
   - Gradual drift rather than catastrophic collapse

**Incompatible with PIR because:**
- Perfect rationality would require X₅ to recognize that high suppression (0.90) is costly enforcement
- Perfect rationality would require X₁, X₄ to immediately recognize C₂ as constructed (no naturalization error)
- Perfect rationality would drive system to either perfect extraction (Deterministic Tragedy) or perfect reform (Revolutionary Rupture) — no stable middle ground

**BIR implications:**
- Errors (E₁-E₄) are structurally realistic under bounded rationality
- Transformation rules (TR₁-TR₄) face realistic barriers (information, coordination, risk)
- Terminal attractor will be Negotiated Equilibrium or Seeded Possibility, not Deterministic Tragedy

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: **Seeded Possibility**

**Definition:** Surface tragedy (constraints run to apparent completion) with underground transformation (alternative logic persists in hidden form).

**Justification:**

1. **Constraint profile compatibility:**
   - Mountain present (C₁): Establishes unchangeable epistemological terrain
   - Snare dominant (C₂): High extraction drives toward tragic outcome
   - Tangled Rope terminal (C₃): Final state is compromised complicity
   - No pure Ropes: No unambiguous coordination mechanisms
   
   Per Attractor Compatibility Matrix: Mountain + Snares → Tragedy or Seeded Possibility

2. **BIR compatibility:**
   - Bounded rationality allows for hidden resistance (agents don't perfectly enforce)
   - Satisficing behavior creates gaps (X₅ doesn't maximize suppression)
   - Information asymmetry enables underground activity
   
   BIR + Mountain-dominated → Seeded Possibility is compatible path

3. **Structural evidence from constraint network:**
   
   **Surface trajectory (apparent):**
   - X₁ subject to C₂ (Snare, χ=1.08) — high extraction
   - TR₁ blocked by C₅ (offstage atomization) — collective action impossible
   - TR₂ (individual exit) high-cost, identity-locked — exit foreclosed
   - TR₃ (institutional reform) blocked by beneficiary position + C₄ (universal arbitrariness)
   - X₁ drifts into C₃ (Tangled Rope, χ=0.468) — pragmatic complicity
   - TR₄ (normalization) proceeds — complicity becomes terminal state
   - **Surface outcome:** Agent accepts false standard, upholds system, appears fully captured
   
   **Underground transformation (hidden):**
   - C₁ (Mountain) provides epistemological foundation: observer-dependence is natural law
   - Recognition that C₂ is constructed (Boltzmann failure) persists despite normalization
   - Complicity (C₃) is Tangled Rope, not pure Rope — extraction component remains salient
   - Error E₂ (Tangled-as-Rope) may be strategic rather than genuine — agent frames complicity as coordination to reduce enforcement while maintaining private recognition of extraction
   - Underground activity: marginal notation, private documentation, teaching apprentices to recognize constructedness
   - Alternative logic preserved in hidden form, awaiting structural conditions for emergence

4. **Why not other attractors:**

   **Deterministic Tragedy:** Would require PIR (perfect institutional rationality). Under BIR, institutional agent satisfices rather than maximizes — leaves gaps. Also requires no underground resistance — but C₁ (Mountain) provides permanent epistemological foundation for recognizing C₂ as constructed.

   **Negotiated Equilibrium:** Would require bargaining space between X₁/X₄ and X₅. But TR₁ blocked (no collective organization), TR₃ blocked (beneficiary has no incentive). No negotiation mechanism exists in constraint network.

   **Revolutionary Rupture:** Would require disruption of constraint logic itself. But C₁ (Mountain) is unchangeable, C₄ (offstage) universalizes the problem, C₅ (offstage) prevents collective action. No mechanism for rupture exists.

   **Seeded Possibility:** Compatible with all structural features. Surface tragedy (normalization proceeds) + underground transformation (recognition persists in hidden form). BIR allows gaps for hidden activity. Mountain (C₁) provides permanent foundation for alternative logic.

**Observable markers of Seeded Possibility:**

-

<!-- ANONYMIZATION: 5 character names replaced: narrator_apprentice -> Agent_A, verrel -> Agent_B, merchants -> Agent_C, crown -> Agent_D, narrator_master -> Agent_E -->
