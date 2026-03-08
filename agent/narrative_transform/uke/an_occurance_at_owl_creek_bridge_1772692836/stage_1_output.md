# STAGE 1: FORMAL CONSTRAINT NETWORK SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Peyton Farquhar (primary agent)
X₂ ← Federal Scout (deception operator)
X₃ ← Captain (institutional executor)
C₁ ← Code of Social Honor
C₂ ← Deceptive Military Entrapment
C₃ ← Summary Military Justice
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Social Honor Code

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.70</epsilon>
    <suppression>0.40</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>            <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.70 × 1.0 × 0.9 = 0.63</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ(0.63) ≤ 0.70: PASS tangled_chi_range
      Coord = true, Asym = true: PASS hybrid_requirements
    </threshold_check>
    <rationale>
      Moderate power position with biographical time horizon experiences 
      genuine coordination value (social belonging, status recognition) 
      alongside extraction (compulsory risk-taking, identity constraint). 
      Identity-locked exit option prevents departure despite extraction 
      awareness. Regional scope enables peer verification of compliance.
      χ = 0.63 places firmly in Tangled Rope range.
    </rationale>
  </agent>

  <indexical_variance>
    Low variance within affected population. Constraint primarily binds 
    agents who have internalized the social code through biographical 
    socialization. Those outside the identity-lock (exit = mobile) would 
    classify as Rope (χ ≈ 0.42 with π = 0.6), but such agents are not 
    structurally positioned within this constraint's domain.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by power position:
    - At P = powerless (π = 1.5): χ = 0.945 → Snare
    - At P = moderate (π = 1.0): χ = 0.63 → Tangled Rope
    - At P = powerful (π = 0.6): χ = 0.378 → Rope
    
    Non-factorizable dependence on power position indicates constructed 
    social mechanism, not natural law. Burden distribution varies by 
    structural position. Correctly classified as Tangled Rope for X₁'s 
    index, not Mountain.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Deceptive Entrapment System

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.90</epsilon>
    <suppression>0.70</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.90 × 1.5 × 0.8 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>
      χ(1.08) > 0.70: PASS snare_chi_threshold
      ε(0.90) > 0.45: PASS not_low_base_extraction
    </threshold_check>
    <rationale>
      Powerless position (no capacity to resist or verify deception) 
      amplifies already-high base extraction. Immediate time horizon 
      prevents strategic response. Trapped exit option eliminates 
      alternatives. Local scope concentrates information asymmetry.
      χ = 1.08 exceeds Snare threshold decisively. High suppression 
      (0.70) reflects enforcement backing the deception.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>powerful</power>            <!-- π = 0.6 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>            <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.90 × 0.6 × 0.9 = 0.486</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      0.46 ≤ χ(0.486) ≤ 0.70: PASS tangled_chi_range
      Coord = true, Asym = true: PASS hybrid_requirements
    </threshold_check>
    <rationale>
      Powerful position deflects extraction onto target. Biographical 
      time horizon allows strategic deployment. Arbitrage exit option 
      (can choose targets, refuse assignments) provides agency. Regional 
      scope distributes operational risk. Experiences genuine coordination 
      value (military intelligence function) alongside extraction 
      (complicity in deceptive practice). χ = 0.486 places in Tangled 
      Rope range.
    </rationale>
  </agent>

  <indexical_variance>
    HIGH. Same deceptive system:
    - X₁ (target): χ = 1.08 → Snare (fatal trap)
    - X₂ (operator): χ = 0.486 → Tangled Rope (functional tool with costs)
    
    Δχ = 0.594 (55% variance). Demonstrates extreme index-sensitivity.
    Both classifications are objectively true from their respective 
    structural positions.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by both power and scope:
    - Power variation: χ(powerless) = 1.08 vs χ(powerful) = 0.486
    - Scope variation: At powerless, local(σ=0.8) vs regional(σ=0.9) 
      changes χ from 1.08 to 1.215
    
    Non-factorizable dependence confirms constructed mechanism. 
    Deception systems are inherently power-asymmetric and scope-dependent.
    Not a natural law.
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Summary Execution Protocol

```xml
<constraint id="C₃">
  <properties>
    <epsilon>1.00</epsilon>
    <suppression>1.00</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>powerless</power>           <!-- π = 1.5 -->
      <time>immediate</time>
      <exit>trapped</exit>
      <scope>local</scope>               <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 1.00 × 1.5 × 0.8 = 1.20</chi>
    <type>Snare</type>
    <threshold_check>
      χ(1.20) > 0.70: PASS snare_chi_threshold
      ε(1.00) > 0.45: PASS not_low_base_extraction
    </threshold_check>
    <rationale>
      Maximum base extraction (ε = 1.00: total extraction of life).
      Powerless position amplifies to χ = 1.20. Immediate time horizon 
      (minutes to execution). Trapped exit option (physical restraint).
      Local scope (isolated execution site). Maximum suppression (1.00: 
      armed enforcement). Zero coordination value from this index.
      Definitive Snare classification.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>       <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>national</scope>            <!-- σ = 1.0 -->
    </index>
    <chi>ε × π × σ = 1.00 × (-0.2) × 1.0 = -0.20</chi>
    <type>Rope</type>
    <threshold_check>
      χ(-0.20) ≤ 0: PASS net_beneficiary_bypass
      χ(-0.20) ≤ 0.35: PASS rope_chi_ceiling
      Changeable = true (institutional protocols can be modified): PASS
    </threshold_check>
    <rationale>
      Institutional position extracts FROM the system (negative χ: 
      career advancement, authority consolidation). Biographical time 
      horizon enables strategic deployment. Mobile exit option (can 
      transfer, resign commission). National scope distributes 
      responsibility. Experiences protocol as coordination mechanism 
      (maintains military discipline, processes threats). Net beneficiary 
      status (χ < 0) bypasses ε ceiling. Rope classification valid.
    </rationale>
  </agent>

  <indexical_variance>
    EXTREME. Same execution protocol:
    - X₁ (subject): χ = 1.20 → Snare (total extraction)
    - X₃ (administrator): χ = -0.20 → Rope (net benefit)
    
    Δχ = 1.40 (700% variance from X₃'s baseline). Maximum possible 
    divergence in constraint experience. Both classifications objectively 
    true from their structural positions.
  </indexical_variance>

  <boltzmann_test>
    FAIL — Classification varies by power position:
    - At P = powerless (π = 1.5): χ = 1.20 → Snare
    - At P = institutional (π = -0.2): χ = -0.20 → Rope
    
    Sign reversal across power positions (extraction → benefit) indicates 
    constructed institutional mechanism with extreme asymmetry. Not a 
    natural law. Correctly classified per index.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Identity-Lock Exploitation

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>Deploy_deceptive_appeal_to_identity_constraint</condition>
    <target>C₁</target>
    <agent>X₂ → X₁</agent>
  </trigger>
  
  <mechanism>
    Operator (X₂) leverages target's (X₁) identity-locked exit option 
    in C₁ to activate C₂. Target cannot refuse appeal to honor code 
    without violating core identity. Deception exploits the fact that 
    identity-lock prevents strategic evaluation of the offer.
  </mechanism>

  <index_change>
    <agent>X₁</agent>
    <from>
      C₁: I = (moderate, biographical, identity_locked, regional)
    </from>
    <to>
      C₂: I = (powerless, immediate, trapped, local)
    </to>
  </index_change>

  <chi_recalculation>
    <agent>X₁</agent>
    <before>
      C₁: χ = 0.70 × 1.0 × 0.9 = 0.63 → Tangled Rope
    </before>
    <after>
      C₂: χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare
    </after>
    <delta>Δχ = +0.45 (71% increase in effective extraction)</delta>
  </chi_recalculation>

  <type_change>Tangled Rope → Snare</type_change>

  <preconditions>
    - X₁ must have identity_locked exit option in C₁
    - X₂ must have knowledge of X₁'s identity-lock
    - C₁ must be active (X₁ currently bound by honor code)
    - Information asymmetry: X₁ cannot verify X₂'s true affiliation
  </preconditions>

  <blocked_by>
    - If X₁ had exit = mobile in C₁ (could strategically evaluate)
    - If X₁ had P = analytical (could detect deception pattern)
    - If scope were national/global (verification networks available)
  </blocked_by>

  <structural_note>
    This transformation demonstrates how Tangled Rope constraints 
    (C₁) can serve as attack surfaces for Snare activation (C₂). 
    The identity-lock that makes C₁ tolerable becomes the mechanism 
    of entrapment in C₂.
  </structural_note>
</transformation_rule>
```

---

### TR₂: Entrapment-to-Execution Cascade

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>Capture_under_deceptive_entrapment</condition>
    <target>C₂ → C₃</target>
    <agent>X₁</agent>
  </trigger>

  <mechanism>
    Successful entrapment (C₂) triggers institutional protocol (C₃).
    No intermediate negotiation or evaluation. Automatic cascade from 
    capture to execution. C₂ serves as sufficient condition for C₃
    activation.
  </mechanism>

  <index_change>
    <agent>X₁</agent>
    <from>
      C₂: I = (powerless, immediate, trapped, local)
    </from>
    <to>
      C₃: I = (powerless, immediate, trapped, local)
    </to>
    <note>Index unchanged — already at minimum agency</note>
  </index_change>

  <chi_recalculation>
    <agent>X₁</agent>
    <before>
      C₂: χ = 0.90 × 1.5 × 0.8 = 1.08 → Snare
    </before>
    <after>
      C₃: χ = 1.00 × 1.5 × 0.8 = 1.20 → Snare
    </after>
    <delta>Δχ = +0.12 (11% increase to maximum extraction)</delta>
  </chi_recalculation>

  <type_change>Snare → Snare (intensification)</type_change>

  <preconditions>
    - C₂ must have reached completion (capture achieved)
    - X₁ must be classified as threat under institutional criteria
    - C₃ protocol must be active (summary justice authorized)
    - No intervening constraints that would require due process
  </preconditions>

  <blocked_by>
    - If institutional rationality were BIR (might negotiate)
    - If X₁ had institutional connections (P ≠ powerless)
    - If scope were global (international law constraints)
  </blocked_by>

  <structural_note>
    This transformation shows Snare-to-Snare cascade with extraction 
    intensification. C₂ → C₃ is deterministic under PIR. The 11% χ 
    increase represents shift from entrapment (reversible) to 
    execution (irreversible).
  </structural_note>
</transformation_rule>
```

---

### TR₃: Hypothetical Collective Organization (Blocked)

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>Organize_collective_resistance_to_honor_code</condition>
    <target>C₁</target>
    <agent>X₁ + collective</agent>
  </trigger>

  <mechanism>
    If agents bound by C₁ collectively organized, power position would 
    shift from moderate to organized, reducing extraction through 
    burden-sharing and creating exit options through mutual support.
  </mechanism>

  <index_change>
    <agent>X₁</agent>
    <from>
      C₁: I = (moderate, biographical, identity_locked, regional)
    </from>
    <to>
      C₁: I = (organized, biographical, mobile, regional)
    </to>
  </index_change>

  <chi_recalculation>
    <agent>X₁</agent>
    <before>
      C₁: χ = 0.70 × 1.0 × 0.9 = 0.63 → Tangled Rope
    </before>
    <after>
      C₁: χ = 0.70 × 0.4 × 0.9 = 0.252 → Rope
    </after>
    <delta>Δχ = -0.378 (60% reduction in extraction)</delta>
  </chi_recalculation>

  <type_change>Tangled Rope → Rope</type_change>

  <preconditions>
    - Multiple agents bound by C₁ must recognize shared extraction
    - Communication channels must exist
    - Collective action must be feasible within time horizon
  </preconditions>

  <blocked_by>
    <primary>Identity-lock in C₁ prevents recognition of extraction</primary>
    <secondary>C₂ exploits isolation (local scope prevents coordination)</secondary>
    <tertiary>C₃ eliminates agents before collective formation possible</tertiary>
  </blocked_by>

  <structural_note>
    This transformation is BLOCKED in the actual constraint network.
    Included to demonstrate that the χ formula predicts feasible 
    reform path (organized → π = 0.4 → Rope), but structural barriers 
    prevent its activation. The identity-lock that makes C₁ tolerable 
    also prevents its reform.
  </structural_note>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type III — Snare-as-Rope (Missing Extraction in C₁)

```xml
<error id="E₁">
  <type>Type III: Snare-as-Rope (Missing Extraction)</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  
  <actual_type>Tangled Rope (χ = 0.63 from X₁'s index)</actual_type>
  <perceived_type>Rope (extraction component normalized)</perceived_type>
  
  <mechanism>
    Identity-locked exit option causes X₁ to experience extraction 
    (compulsory risk-taking, status competition) as intrinsic to 
    coordination value (social belonging). The biographical time 
    horizon and moderate power position make extraction feel 
    manageable, preventing recognition of asymmetry.
  </mechanism>

  <observable>
    X₁ does not attempt to negotiate reduced risk-taking or question 
    asymmetric burden distribution despite having moderate power 
    position that would enable such negotiation. Treats compulsory 
    risk as natural feature of social membership rather than 
    extractive component of hybrid constraint.
  </observable>

  <index_conditions>
    Common at I = (moderate, biographical, identity_locked, regional).
    The combination of:
    - Moderate power (feels agency exists)
    - Biographical horizon (extraction spread over lifetime)
    - Identity-lock (cannot imagine exit)
    - Regional scope (peer verification normalizes burden)
    ...creates normalization of extraction as coordination.
  </index_conditions>

  <correction_trigger>
    Would require either:
    1. Shift to analytical position (π = 1.15) to detect normalized 
       extraction, OR
    2. Exposure to agents with mobile exit options who reveal 
       asymmetry, OR
    3. Collective organization (TR₃) that makes extraction visible 
       through comparison
  </correction_trigger>

  <consequence>
    Normalization enables C₁ to serve as attack surface for C₂. 
    If X₁ recognized C₁ as Tangled Rope (extraction + coordination), 
    might develop strategic caution. Perceiving it as pure Rope 
    (coordination only) leaves vulnerability to identity-based 
    exploitation.
  </consequence>
</error>
```

---

### Error E₂: Type I — False Mountain (C₃ as Unchangeable)

```xml
<error id="E₂">
  <type>Type I: False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Snare (χ = 1.20 from X₁'s index)</actual_type>
  <perceived_type>Mountain (unchangeable natural law)</perceived_type>
  
  <mechanism>
    Immediate time horizon + trapped exit option + powerless position 
    cause C₃ to appear as immutable physical reality rather than 
    constructed institutional protocol. The combination of maximum 
    suppression (1.00) and local scope (isolated execution site) 
    eliminates perception of alternatives.
  </mechanism>

  <observable>
    X₁ does not attempt to invoke institutional review, appeal to 
    higher authority, or question legitimacy of summary protocol. 
    Treats execution as inevitable physical process rather than 
    institutional decision subject to modification.
  </observable>

  <boltzmann_evidence>
    C₃ FAILS Boltzmann test — classification varies by power position:
    - X₁ (powerless): χ = 1.20 → Snare
    - X₃ (institutional): χ = -0.20 → Rope
    
    This variance proves C₃ is constructed, not natural. A true 
    Mountain would classify identically across all indices.
  </boltzmann_evidence>

  <correction_trigger>
    Would require:
    1. Shift to biographical time horizon (see institutional 
       modification as possible), OR
    2. Access to analytical position (recognize protocol as 
       constructed), OR
    3. Exposure to cases where C₃ was successfully challenged
    
    All blocked by immediate time horizon and trapped exit option.
  </correction_trigger>

  <consequence>
    False Mountain classification eliminates resistance. If X₁ 
    recognized C₃ as Snare (constructed, changeable in principle), 
    might attempt institutional appeal. Perceiving it as Mountain 
    (unchangeable) produces fatalistic acceptance, which serves 
    institutional efficiency but misrepresents structural reality.
  </consequence>
</error>
```

---

### Error E₃: Type III — Snare-as-Tangled (Missing Full Extraction in C₂)

```xml
<error id="E₃">
  <type>Type III: Snare-as-Rope (Partial — Missing Full Extraction)</type>
  <agent>X₂</agent>
  <constraint>C₂</constraint>
  
  <actual_type>
    Dual classification:
    - From X₁'s index: Snare (χ = 1.08)
    - From X₂'s index: Tangled Rope (χ = 0.486)
  </actual_type>
  
  <perceived_type>
    Rope (pure coordination — intelligence gathering function)
  </perceived_type>
  
  <mechanism>
    X₂'s powerful position (π = 0.6) and arbitrage exit option deflect 
    extraction onto target (X₁). Biographical time horizon allows 
    strategic deployment without immediate consequences. X₂ experiences 
    genuine coordination value (military intelligence function) while 
    extraction is borne entirely by X₁.
  </mechanism>

  <observable>
    X₂ does not recognize complicity in extractive system. Treats 
    deceptive entrapment as legitimate intelligence tactic (pure 
    coordination) rather than hybrid mechanism that extracts from 
    targets while coordinating for operators. Does not question 
    asymmetric burden distribution.
  </observable>

  <index_conditions>
    Common at I = (powerful, biographical, arbitrage, regional).
    Power position deflects costs; arbitrage exit option provides 
    agency; biographical horizon distributes moral costs over time; 
    regional scope distributes responsibility across operators.
  </index_conditions>

  <correction_trigger>
    Would require:
    1. Shift to analytical position to recognize extraction borne 
       by targets, OR
    2. Role reversal (experience C₂ from powerless position), OR
    3. Institutional audit that makes extraction visible
  </correction_trigger>

  <consequence>
    Partial normalization enables C₂ operation. If X₂ recognized 
    full Snare status (from X₁'s index), might refuse participation 
    or demand protocol modification. Perceiving C₂ as Tangled Rope 
    (acknowledging some extraction) or Rope (pure coordination) 
    maintains operational willingness.
  </consequence>

  <structural_note>
    This error demonstrates indexical variance as source of moral 
    blindness. X₂'s classification (Tangled Rope, χ = 0.486) is 
    objectively true from X₂'s structural position. The error is 
    failing to recognize that the SAME constraint is Snare (χ = 1.08) 
    from X₁'s position. Both classifications are simultaneously true.
  </structural_note>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Perfect Institutional Rationality (PIR)

```xml
<rationality_model>
  <type>PIR</type>
  
  <justification>
    C₃ (Summary Execution Protocol) exhibits characteristics of PIR:
    
    1. **Utility maximization without bounds**: Protocol extracts 
       maximum value (elimination of threat) without negotiation or 
       proportionality constraints.
    
    2. **No Pareto-improving negotiation**: Once C₂ triggers C₃, no 
       bargaining occurs. X₁ has nothing to offer that would make 
       X₃ prefer non-execution.
    
    3. **Algorithmic enforcement**: Protocol operates mechanically. 
       X₃'s role is administrative (execute protocol), not evaluative 
       (assess alternatives).
    
    4. **Zero principal-agent slack**: Military hierarchy eliminates 
       discretion. X₃ cannot satisfice or risk-avert without 
       insubordination.
    
    5. **Implacable system**: C₃ does not respond to appeals, 
       circumstances, or humanitarian considerations. Operates as 
       deterministic function: Capture → Execution.
  </justification>

  <alternative_rejected>
    <model>BIR</model>
    <reason>
      BIR would require:
      - Satisficing behavior (accept "good enough" threat reduction)
      - Principal-agent problems (X₃ has discretion to deviate)
      - Risk aversion (prefer certain small gain over uncertain large gain)
      - Negotiation possibility (X₁ could offer information, cooperation)
      
      None of these features are present in C₃. The protocol is 
      deterministic and non-negotiable. This is not realistic 
      organizational behavior — it is implacable institutional logic.
    </reason>
  </alternative_rejected>

  <attractor_implications>
    PIR + Snare-dominated network → Deterministic Tragedy
    
    With PIR, C₃ runs to completion without deviation. No negotiated 
    equilibrium is possible because institutions do not satisfice. 
    Revolutionary rupture is blocked (X₁ has no power to disrupt 
    constraint logic). Seeded possibility is blocked (no underground 
    transformation when extraction is total and immediate).
    
    Only attractor compatible with PIR + (C₁→C₂→C₃) cascade is 
    Deterministic Tragedy.
  </attractor_implications>
</rationality_model>
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Deterministic Tragedy

```xml
<terminal_attractor>
  <type>Deterministic Tragedy</type>
  
  <definition>
    Constraints run to completion without deviation. No negotiation, 
    no rupture, no underground transformation. The constraint logic 
    executes fully and irreversibly.
  </definition>

  <compatibility_check>
    <rationality_model>PIR ✓</rationality_model>
    <constraint_profile>
      - C₁: Tangled Rope (feeds into C₂)
      - C₂: Snare (feeds into C₃)
      - C₃: Snare (terminal, maximum extraction)
      
      Snare-dominated cascade ✓
    </constraint_profile>
    <transformation_rules>
      - TR₁: C₁ → C₂ (deterministic exploitation of identity-lock)
      - TR₂: C₂ → C₃ (automatic cascade, no intermediate steps)
      - TR₃: Blocked (collective organization prevented)
      
      No escape paths ✓
    </transformation_rules>
    <attractor_matrix>
      PIR + Snare-dominated → Deterministic Tragedy ✓
    </attractor_matrix>
  </compatibility_check>

  <justification>
    1. **PIR eliminates negotiation**: C₃ operates algorithmically. 
       No satisficing, no principal-agent slack, no risk aversion. 
       Execution is deterministic function of capture.
    
    2. **Snare cascade is irreversible**: C₁ → C₂ → C₃ proceeds 
       without intervention points. Each transformation reduces 
       agency (moderate → powerless → powerless) and increases 
       extraction (χ: 0.63 → 1.08 → 1.20).
    
    3. **No rupture capacity**: X₁ never achieves power position 
       that would enable disruption of constraint logic. Remains 
       powerless throughout C₂ and C₃.
    
    4. **No seeded possibility**: Extraction is total (ε = 1.00 in C₃) 
       and immediate (T = immediate). No time for underground 
       transformation. No survivors to carry alternative logic.
    
    5. **Blocked reform paths**: TR₃ (collective organization) is 
       structurally prevented by identity-lock in C₁ and isolation 
       in C₂. No mechanism exists to shift from Snare to Rope.
  </justification>

  <alternative_attractors_rejected>
    <attractor>Negotiated Equilibrium</attractor>
    <reason>
      Requires BIR (satisficing institutions). PIR eliminates 
      negotiation. C₃ does not bargain.
    </reason>

    <attractor>Revolutionary Rupture</attractor>
    <reason>
      Requires power to disrupt constraint logic. X₁ remains 
      powerless throughout. No mechanism to challenge C₃'s 
      institutional authority.
    </reason>

    <attractor>Seeded Possibility</attractor>
    <reason>
      Requires time for underground transformation or survivors 
      to carry alternative logic. C₃'s immediate time horizon 
      and total extraction (ε = 1.00) eliminate both. No 
      underground exists when extraction is death.
    </reason>
  </alternative_attractors_rejected>

  <structural_inevitability>
    Given:
    - PIR (implacable institutions)
    - C₁ with identity-lock (prevents strategic evaluation)
    - C₂ with information asymmetry (enables entrapment)
    - C₃ with maximum extraction (ε = 1.00, Supp = 1.00)
    - TR₁ and TR₂ as deterministic cascades
    - TR₃ blocked
    
    Deterministic Tragedy is the ONLY reachable attractor. The 
    constraint network has no exit paths. This is not narrative 
    choice — it is structural inevitability given the formalized 
    mechanics.
  </structural_inevitability>
</terminal_attractor>
```

---

## VI. CONSTRAINT