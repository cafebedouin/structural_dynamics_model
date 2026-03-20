# STAGE 1: FORMAL SPECIFICATION
## "The Examination" - Indexed Constraint Mechanics

---

## I. CONSTRAINT FORMALIZATIONS

### C1: The Unspoken Social Divide

**Base Properties:**
```
ε(C1) = 0.70
Supp(C1) = 0.80
Coord(C1) = false
Asymmetric(C1) = true
```

**Indexed Classifications:**

**Character: Bertie and Billy**
```
Index: I₁ = (institutional, biographical, arbitrage, national)
π(institutional) = -0.2
σ(national) = 1.0

χ(C1, I₁) = ε(C1) × π(institutional) × σ(national)
χ(C1, I₁) = 0.70 × (-0.2) × 1.0
χ(C1, I₁) = -0.14

Classification: Rope (⊞)
Structural signature: Net beneficiary, coordination from their position
Experience: "The world is structured in a way that is comfortable and advantageous"
```

**Character: Oscar Maironi**
```
Index: I₂ = (powerless, biographical, trapped, national)
π(powerless) = 1.5
σ(national) = 1.0

χ(C1, I₂) = ε(C1) × π(powerless) × σ(national)
χ(C1, I₂) = 0.70 × 1.5 × 1.0
χ(C1, I₂) = 1.05

Classification: Snare (⊠)
Structural signature: High extraction, trapped, requires enforcement
Experience: "His origins define him and limit his options, forcing constant calculation"
```

**Indexical Variance Verification:**
```
Δχ = |χ(C1, I₁) - χ(C1, I₂)| = |-0.14 - 1.05| = 1.19
Variance: STRONG (crosses from net benefit to severe extraction)
Type divergence: Rope → Snare (maximum dramatic tension)
```

---

### C2: The Meritocracy Myth

**Base Properties:**
```
ε(C2) = 0.60
Supp(C2) = 0.70
Coord(C2) = true
Asymmetric(C2) = true
```

**Indexed Classifications:**

**Character: Bertie and Billy**
```
Index: I₃ = (powerful, biographical, mobile, national)
π(powerful) = 0.6
σ(national) = 1.0

χ(C2, I₃) = ε(C2) × π(powerful) × σ(national)
χ(C2, I₃) = 0.60 × 0.6 × 1.0
χ(C2, I₃) = 0.36

Classification: Rope (⊞)
Structural signature: Low extraction, coordination dominates, strategic engagement
Experience: "The academic system is a game with rules that can be learned"
```

**Character: Oscar Maironi**
```
Index: I₄ = (analytical, biographical, trapped, national)
π(analytical) = 1.15
σ(national) = 1.0

χ(C2, I₄) = ε(C2) × π(analytical) × σ(national)
χ(C2, I₄) = 0.60 × 1.15 × 1.0
χ(C2, I₄) = 0.69

Classification: Tangled Rope (⊞⊠)
Structural signature: Moderate-high extraction, genuine coordination + asymmetric cost
Experience: "Believes academic success is direct measure of virtue, provides superiority but traps him"
```

**Indexical Variance Verification:**
```
Δχ = |χ(C2, I₃) - χ(C2, I₄)| = |0.36 - 0.69| = 0.33
Variance: MODERATE (both see coordination, but Oscar bears higher cost)
Type divergence: Rope → Tangled Rope (navigable vs. conflicted)
```

---

### C3: The Final Examination

**Base Properties:**
```
ε(C3) = 0.80
Supp(C3) = 0.90
Coord(C3) = false
Asymmetric(C3) = true
```

**Indexed Classifications:**

**Character: Bertie and Billy**
```
Index: I₅ = (powerful, immediate, constrained, local)
π(powerful) = 0.6
σ(local) = 0.8

χ(C3, I₅) = ε(C3) × π(powerful) × σ(local)
χ(C3, I₅) = 0.80 × 0.6 × 0.8
χ(C3, I₅) = 0.384

Classification: Rope (⊞)
Structural signature: Manageable hurdle, game-like, low stakes
Experience: "A manageable hurdle, a deadline that prompts focused activity"
```

**Character: Oscar Maironi**
```
Index: I₆ = (powerless, biographical, trapped, local)
π(powerless) = 1.5
σ(local) = 0.8

χ(C3, I₆) = ε(C3) × π(powerless) × σ(local)
χ(C3, I₆) = 0.80 × 1.5 × 0.8
χ(C3, I₆) = 0.96

Classification: Snare (⊠)
Structural signature: High extraction, life-or-death stakes, no exit
Experience: "A day of judgment where entire year's labor will be weighed; failure catastrophic"
```

**Indexical Variance Verification:**
```
Δχ = |χ(C3, I₅) - χ(C3, I₆)| = |0.384 - 0.96| = 0.576
Variance: STRONG (crosses from manageable to catastrophic)
Type divergence: Rope → Snare (game vs. trap)
```

---

## II. TRANSFORMATION RULES (Index-Sensitive)

### TR1: Collective Organization (Oscar's Potential Path)

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>character = "Oscar"</condition>
    <action>organize_collective_with_other_poor_students</action>
    <context>recognize_shared_structural_position</context>
  </trigger>
  
  <index_change>
    <from>
      <index>(powerless, biographical, trapped, national)</index>
      <power_position>powerless (π=1.5)</power_position>
    </from>
    <to>
      <index>(organized, biographical, constrained, national)</index>
      <power_position>organized (π=0.4)</power_position>
    </to>
  </index_change>
  
  <constraint_reclassification constraint="C1">
    <before>
      <chi>1.05</chi>
      <type>Snare</type>
    </before>
    <after>
      <chi_calculation>0.70 × 0.4 × 1.0 = 0.28</chi_calculation>
      <chi>0.28</chi>
      <type>Rope</type>
    </after>
    <mechanism>Collective action shares burden, creates negotiating power</mechanism>
  </constraint_reclassification>
  
  <constraint_reclassification constraint="C3">
    <before>
      <chi>0.96</chi>
      <type>Snare</type>
    </before>
    <after>
      <chi_calculation>0.80 × 0.4 × 0.8 = 0.256</chi_calculation>
      <chi>0.256</chi>
      <type>Rope</type>
    </after>
    <mechanism>Shared study resources, mutual support reduces individual stakes</mechanism>
  </constraint_reclassification>
  
  <narrative_consequence>
    Oscar's experience transforms from isolated trap to navigable system.
    The examination becomes a collective challenge rather than individual judgment.
    However, this path is blocked in the story by C1 (social divide prevents recognition of shared position).
  </narrative_consequence>
</transformation_rule>
```

---

### TR2: Meritocracy Disillusionment (Oscar's Actual Arc)

```xml
<transformation_rule id="TR2">
  <trigger>
    <condition>character = "Oscar"</condition>
    <event>examination_results_announced</event>
    <outcome>diligence_not_rewarded_as_expected</outcome>
  </trigger>
  
  <index_change>
    <from>
      <index>(analytical, biographical, trapped, national)</index>
      <power_position>analytical (π=1.15)</power_position>
      <belief>meritocracy_is_real</belief>
    </from>
    <to>
      <index>(analytical, biographical, trapped, national)</index>
      <power_position>analytical (π=1.15)</power_position>
      <belief>meritocracy_is_myth</belief>
    </to>
  </index_change>
  
  <constraint_reclassification constraint="C2">
    <before>
      <chi>0.69</chi>
      <type>Tangled Rope</type>
      <experience>Genuine belief in coordination value</experience>
    </before>
    <after>
      <chi>0.69</chi>
      <type>Tangled Rope (recognized as such)</type>
      <experience>Sees both coordination AND extraction, previously only saw coordination</experience>
    </after>
    <mechanism>Error correction: Type III (Snare-as-Rope) → accurate Tangled Rope classification</mechanism>
  </constraint_reclassification>
  
  <error_type_transition>
    <from>Type III: Snare-as-Rope (missing extraction in C2)</from>
    <to>Accurate classification (sees hybrid nature)</to>
    <cost>Psychological: Loss of faith, bitterness, sense of betrayal</cost>
  </error_type_transition>
  
  <narrative_consequence>
    Oscar's worldview shatters. He realizes the system he mastered extracts asymmetrically.
    His analytical power (π=1.15) now reveals what it previously obscured.
    This is the story's central irony: his expertise in the system's rules makes the betrayal more complete.
  </narrative_consequence>
</transformation_rule>
```

---

### TR3: Wealthy Students' Confirmation (Bertie and Billy)

```xml
<transformation_rule id="TR3">
  <trigger>
    <condition>character = "Bertie and Billy"</condition>
    <event>examination_success</event>
    <outcome>last_minute_effort_rewarded</outcome>
  </trigger>
  
  <index_change>
    <from>
      <index>(powerful, biographical, mobile, national)</index>
    </from>
    <to>
      <index>(powerful, biographical, mobile, national)</index>
      <belief_reinforcement>system_works_as_expected</belief_reinforcement>
    </to>
  </index_change>
  
  <constraint_experience constraint="C2">
    <chi>0.36</chi>
    <type>Rope (confirmed)</type>
    <experience>System validated their approach: insight over toil, cleverness over diligence</experience>
  </constraint_experience>
  
  <error_type_maintenance>
    <type>Type III: Snare-as-Rope (for Oscar's experience)</type>
    <mechanism>Their success prevents them from seeing Oscar's extraction</mechanism>
    <consequence>Indexical blindness: cannot perceive constraint from powerless position</consequence>
  </error_type_maintenance>
  
  <narrative_consequence>
    Their worldview is reinforced. The system appears meritocratic from their index.
    They remain unaware of the structural advantage (C1) that enabled their success.
    This creates dramatic irony: readers see what characters cannot.
  </narrative_consequence>
</transformation_rule>
```

---

### TR4: Professor's Judgment (Institutional Rationality)

```xml
<transformation_rule id="TR4">
  <trigger>
    <condition>character = "Professor"</condition>
    <action>evaluate_examination_responses</action>
    <context>institutional_position</context>
  </trigger>
  
  <index>
    <power>institutional (π=-0.2)</power>
    <time>generational</time>
    <exit>mobile</exit>
    <scope>national</scope>
  </index>
  
  <constraint_experience constraint="C2">
    <chi_calculation>0.60 × (-0.2) × 1.0 = -0.12</chi_calculation>
    <chi>-0.12</chi>
    <type>Rope (net beneficiary)</type>
    <experience>System produces students who reflect institutional values</experience>
  </constraint_experience>
  
  <judgment_mechanism>
    <omega_unresolved>
      Is the professor rewarding:
      (a) Genuine insight born of lived experience (Bertie/Billy's leisure enables creativity)
      (b) Familiar, confident style of upper class (mistaking class markers for originality)
    </omega_unresolved>
    
    <structural_ambiguity>
      From professor's index (institutional, net beneficiary), both appear identical.
      The system cannot distinguish between:
      - Coordination value (genuine insight)
      - Extraction mechanism (reproducing class advantage)
    </structural_ambiguity>
  </judgment_mechanism>
  
  <narrative_consequence>
    The professor's judgment is structurally ambiguous.
    This ambiguity is not a character flaw—it's an indexical limitation.
    From institutional position, class advantage and genuine merit are indistinguishable.
  </narrative_consequence>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS (Observable Actions)

### E1: Oscar's Type III Error (Snare-as-Rope in C2)

```xml
<error_manifestation id="E1">
  <error_type>Type III: Snare-as-Rope (Missing Extraction)</error_type>
  <constraint>C2 (Meritocracy Myth)</constraint>
  <character>Oscar Maironi</character>
  
  <misclassification>
    <perceived_type>Rope (pure coordination)</perceived_type>
    <actual_type>Tangled Rope (coordination + extraction)</actual_type>
    <chi_perceived>Low (system is fair)</chi_perceived>
    <chi_actual>0.69 (moderate-high extraction)</chi_actual>
  </misclassification>
  
  <observable_actions>
    <action>Extreme diligence in study (treats system as pure meritocracy)</action>
    <action>Moral superiority over wealthy students (believes effort = virtue)</action>
    <action>Shock at examination results (expected direct reward for labor)</action>
    <action>Bitterness and sense of betrayal (system violated perceived rules)</action>
  </observable_actions>
  
  <index_explanation>
    <power>analytical (π=1.15)</power>
    <mechanism>His analytical skill makes him expert in system's rules</mechanism>
    <blindness>Expertise in rules prevents seeing extraction beneath coordination</blindness>
    <consequence>The better he understands the system, the more completely it traps him</consequence>
  </index_explanation>
  
  <correction_trigger>
    <event>Examination results (TR2)</event>
    <mechanism>Lived experience contradicts belief</mechanism>
    <cost>Psychological devastation, loss of faith</cost>
  </correction_trigger>
</error_manifestation>
```

---

### E2: Bertie and Billy's Type III Error (Snare-as-Rope for Oscar)

```xml
<error_manifestation id="E2">
  <error_type>Type III: Snare-as-Rope (Missing Extraction for Others)</error_type>
  <constraint>C1 (Social Divide), C2 (Meritocracy Myth)</constraint>
  <character>Bertie and Billy</character>
  
  <misclassification>
    <perceived_type>Rope (system works for everyone)</perceived_type>
    <actual_type_for_oscar>Snare (C1: χ=1.05), Tangled Rope (C2: χ=0.69)</actual_type_for_oscar>
    <indexical_blindness>Cannot perceive constraint from powerless position</indexical_blindness>
  </misclassification>
  
  <observable_actions>
    <action>Casual approach to examination (treat as game)</action>
    <action>Surprise at Oscar's intensity (don't understand his stakes)</action>
    <action>Comfortable assumption of success (system has always worked for them)</action>
    <action>Lack of awareness of structural advantage (C1 invisible from their index)</action>
  </observable_actions>
  
  <index_explanation>
    <power>institutional/powerful (π=-0.2 to 0.6)</power>
    <mechanism>Net beneficiary position prevents seeing extraction</mechanism>
    <blindness>Their experience (Rope) is objectively true from their index</blindness>
    <consequence>Structural position creates genuine but incomplete perception</consequence>
  </index_explanation>
  
  <correction_trigger>
    <event>None in story (error persists)</event>
    <mechanism>Success reinforces misclassification (TR3)</mechanism>
    <narrative_function>Creates dramatic irony, shows indexical blindness</narrative_function>
  </correction_trigger>
</error_manifestation>
```

---

### E3: Oscar's Type I Error (False Mountain in C1)

```xml
<error_manifestation id="E3">
  <error_type>Type I: False Mountain (Treating Changeable as Unchangeable)</error_type>
  <constraint>C1 (Social Divide)</constraint>
  <character>Oscar Maironi</character>
  
  <misclassification>
    <perceived_type>Mountain (unchangeable social order)</perceived_type>
    <actual_type>Snare (changeable through collective action)</actual_type>
    <mechanism>Trapped + immediate horizon makes it appear unchangeable</mechanism>
  </misclassification>
  
  <observable_actions>
    <action>Individual strategy (work within system rather than challenge it)</action>
    <action>No attempt to organize with other poor students</action>
    <action>Acceptance of social hierarchy as natural</action>
    <action>Focus on personal advancement rather than structural change</action>
  </observable_actions>
  
  <index_explanation>
    <power>powerless (π=1.5)</power>
    <time>biographical (cannot see beyond lifetime)</time>
    <exit>trapped (no alternatives visible)</exit>
    <mechanism>From this index, C1 genuinely appears as Mountain</mechanism>
    <objective_truth>Classification is correct from his structural position</objective_truth>
  </index_explanation>
  
  <blocked_transformation>
    <potential>TR1 (collective organization)</potential>
    <blocker>C1 itself prevents recognition of shared position</blocker>
    <consequence>The constraint that could be changed prevents its own transformation</consequence>
    <narrative_function>Structural tragedy (not character flaw)</narrative_function>
  </blocked_transformation>
</error_manifestation>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selection: Bounded Institutional Rationality (BIR)

```xml
<institutional_rationality_model>
  <type>Bounded Institutional Rationality (BIR)</type>
  
  <justification>
    <reason>The academic institution exhibits satisficing behavior under uncertainty</reason>
    <reason>Professor faces principal-agent problem (institutional goals vs. genuine merit assessment)</reason>
    <reason>Risk aversion: Rewards familiar patterns (upper-class confidence) over uncertain innovation</reason>
    <reason>Information asymmetry: Cannot fully distinguish class advantage from genuine insight</reason>
    <reason>Realistic human institution with cognitive limitations and social pressures</reason>
  </justification>
  
  <characteristics>
    <satisficing>Professor seeks "good enough" evaluation, not perfect meritocracy</satisficing>
    <uncertainty>Cannot perfectly measure merit, relies on proxies (confidence, style, familiarity)</uncertainty>
    <risk_aversion>Safer to reward students who fit institutional expectations</risk_aversion>
    <principal_agent>Professor's incentives may not align with pure merit assessment</principal_agent>
  </characteristics>
  
  <contrast_with_pir>
    <pir_would_produce>Deterministic Tragedy (perfect extraction, no negotiation)</pir_would_produce>
    <bir_produces>Negotiated Equilibrium or Seeded Possibility (imperfect system, room for agency)</bir_produces>
    <critical_difference>BIR allows for human error, institutional drift, and potential reform</critical_difference>
  </contrast_with_pir>
  
  <attractor_implications>
    <reachable>Negotiated Equilibrium, Seeded Possibility</reachable>
    <unreachable>Deterministic Tragedy (requires PIR)</unreachable>
    <mechanism>Bounded rationality creates space for negotiation and underground transformation</mechanism>
  </attractor_implications>
</institutional_rationality_model>
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected: Seeded Possibility

```xml
<terminal_attractor>
  <type>Seeded Possibility</type>
  <symbol>Surface tragedy, underground transformation</symbol>
  
  <justification>
    <surface_level>
      <outcome>Oscar fails to achieve expected reward</outcome>
      <appearance>System confirms existing hierarchy (C1 persists)</appearance>
      <visible_result>Wealthy students succeed, poor student disillusioned</visible_result>
      <constraint_state>C1, C2, C3 appear unchanged</constraint_state>
    </surface_level>
    
    <underground_level>
      <transformation>Oscar's error correction (E1 → TR2)</transformation>
      <knowledge_gain>Sees through meritocracy myth (C2 reclassified)</knowledge_gain>
      <potential>Analytical power (π=1.15) now reveals extraction</potential>
      <seed>Disillusionment is prerequisite for structural awareness</seed>
    </underground_level>
    
    <compatibility>
      <rationality_model>BIR (allows for imperfect outcomes and learning)</rationality_model>
      <constraint_types>Snare (C1, C3) + Tangled Rope (C2) = extraction visible but not deterministic</constraint_types>
      <character_agency>Oscar gains knowledge even as he loses faith</character_agency>
    </compatibility>
  </justification>
  
  <structural_signature>
    <surface>Constraints run to completion (examination happens, hierarchy confirmed)</surface>
    <underground>Character transformation (Oscar's worldview shifts from E1 to accurate classification)</underground>
    <future_potential>Knowledge gained enables future action (even if not shown in story)</future_potential>
    <narrative_tone>Tragic on surface, but not deterministic—seeds planted for change</narrative_tone>
  </structural_signature>
  
  <contrast_with_alternatives>
    <deterministic_tragedy>
      <difference>Would require PIR (perfect extraction, no learning)</difference>
      <why_not>Oscar does learn (TR2), system is imperfect (BIR)</why_not>
    </deterministic_tragedy>
    
    <negotiated_equilibrium>
      <difference>Would require explicit bargaining, stable compromise</difference>
      <why_not>No negotiation occurs, hierarchy persists unchanged</why_not>
    </negotiated_equilibrium>
    
    <revolutionary_rupture>
      <difference>Would require constraint logic itself disrupted</difference>
      <why_not>C1, C2, C3 remain structurally intact, no systemic break</why_not>
    </revolutionary_rupture>
  </contrast_with_alternatives>
  
  <narrative_function>
    <reader_experience>Feels tragic (Oscar's suffering) but not hopeless (he gains insight)</reader_experience>
    <thematic_resonance>Knowledge is power, even when immediate outcomes are negative</thematic_resonance>
    <future_implication>Oscar's disillusionment is first step toward potential collective action (TR1 blocked now, but seed planted)</future_implication>
  </narrative_function>
</terminal_attractor>
```

---

## VI. STRUCTURAL PHYSICS ARCS (Optional Stage 7-9 Elements)

### Arc 1: False Mountain Reveal (C2 - Meritocracy Myth)

```xml
<structural_physics_arc id="Arc1">
  <type>False Mountain Reveal</type>
  <constraint>C2 (Meritocracy Myth)</constraint>
  
  <initial_presentation>
    <perceived_as>Mountain (natural law of merit)</perceived_as>
    <character_belief>Oscar: "Hard work = success" (unchangeable truth)</character_belief>
    <social_narrative>"The examination is fair and objective"</social_narrative>
  </initial_presentation>
  
  <boltzmann_test_failure>
    <independence_check>Does C2 bind everyone equally regardless of power?</independence_check>
    <result>NO - χ varies by power position (0.36 for powerful, 0.69 for analytical)</result>
    
    <scope_check>Does C2 operate same at all scales?</scope_check>
    <result>YES - σ(national) = 1.0 for all characters (scope-invariant)</result>
    
    <enforcement_check>Does C2 require active maintenance?</enforcement_check>
    <result>YES - Supp(C2) = 0.70 (requires institutional enforcement)</result>
    
    <conclusion>C2 is NOT a Mountain (natural law)</conclusion>
    <actual_type>Tangled Rope (constructed system with coordination + extraction)</actual_type>
  </boltzmann_test_failure>
  
  <reveal_structure>
    <stage_1>Oscar accepts meritocracy as natural law (Type I error)</stage_1>
    <stage_2>Evidence accumulates: wealthy students succeed with less effort</stage_2>
    <stage_3>Examination results contradict expected outcomes</stage_3>
    <stage_4>Oscar realizes: system couples power with outcome (not natural law)</stage_4>
    <stage_5>Reclassification: Mountain → Tangled Rope (TR2)</stage_5>
  </reveal_structure>
  
  <dramatic_impact>
    <character>Oscar's worldview shatters (psychological cost of error correction)</character>
    <reader>Sees through "natural meritocracy" narrative</reader>
    <thematic>Exposes ideology as constructed constraint, not natural law</thematic>
  </dramatic_impact>
</structural_physics_arc>
```

---

### Arc 2: Purity Drift (C2 - Meritocracy System)

```xml
<structural_physics_arc id="Arc2">
  <type>Purity Drift (Pre-Symptomatic Decay)</type>
  <constraint>C2 (Meritocracy Myth)</constraint>
  
  <historical_trajectory>
    <origin>
      <state>Genuine coordination (early academic system)</state>
      <purity>High (coordination >> extraction)</purity>
      <function>Identify and develop talent regardless of origin</function>
    </origin>
    
    <drift_phase>
      <mechanism>Wealthy students gain advantages (tutors, leisure, confidence)</mechanism>
      <surface_metrics>System appears functional (students graduate, exams administered)</surface_metrics>
      <underground_decay>Coordination function hollowing out (merit ≠ reward)</underground_decay>
      <purity_decline>Extraction accumulates (class advantage compounds)</purity_decline>
    </drift_phase>
    
    <current_state>
      <purity>Moderate-low (extraction approaching coordination)</purity>
      <type>Tangled Rope (hybrid, but trending toward Snare)</type>
      <visible_symptoms>Oscar's experience (diligence unrewarded)</visible_symptoms>
      <structural_reality>Decay started long before symptoms appeared</structural_reality>
    </current_state>
  </historical_trajectory>
  
  <cassandra_character>
    <role>Oscar (analytical, π=1.15)</role>
    <perception>Sees system's rules clearly, but not its decay</perception>
    <blindness>Expertise in rules prevents seeing structural degradation</blindness>
    <tragic_irony>His analytical power makes him expert in a decaying system</tragic_irony>
  </cassandra_character>
  
  <narrative_function>
    <slow_crisis>System looks stable until Oscar's failure reveals decay</slow_crisis>
    <pre_symptomatic>Rot was present long before examination results</pre_symptomatic>
    <thematic>Institutions degrade slowly, invisibly, until crisis makes it visible</thematic>
  </narrative_function>
</structural_physics_arc>
```

---

### Arc 3: Network Contamination (C1 → C2 → C3)

```xml
<structural_physics_arc id="Arc3">
  <type>Network Contamination (Constraint Cascade)</type>
  <constraint_network>C1 → C2 → C3</constraint_network>
  
  <contamination_flow>
    <source>
      <constraint>C1 (Social Divide)</constraint>
      <purity>Low (pure extraction, ε=0.70)</purity>
      <type>Snare (from Oscar's index)</type>
    </source>
    
    <transmission>
      <mechanism>C1 feeds into C2 (social position affects academic system)</mechanism>
      <effect>C2's coordination function contaminated by C1's extraction</effect>
      <result>C2 cannot function as pure meritocracy while C1 persists</result>
    </transmission>
    
    <downstream_impact>
      <constraint>C3 (Final Examination)</constraint>
      <contamination>C3 inherits extraction from C1 and C2</contamination>
      <effective_purity>C3's intrinsic purity degraded by network context</effective_purity>
      <result>Examination cannot be fair while embedded in contaminated network</result>
    </downstream_impact>
  </contamination_flow>
  
  <isolated_reform_failure>
    <attempt>Reform C2 (make examination more fair)</attempt>
    <blocker>C1 persists (social divide continues to contaminate)</blocker>
    <result>Surgical reform of C2 fails without addressing C1</result>
    <mechanism>Network contamination propagates faster than local repair</mechanism>
  </isolated_reform_failure>
  
  <narrative_function>
    <structural_explanation>Why Oscar's diligence cannot succeed (C1 contaminates C2 and C3)</structural_explanation>
    <systemic_view>Individual constraints cannot be fixed in isolation</systemic_view>
    <thematic>Upstream extraction (C1) poisons downstream coordination (C2, C3)</thematic>
  </narrative_function>
  
  <reform_implication>
    <requirement>Must address C1 (social divide) to fix C2 (meritocracy) and C3 (examination)</requirement>
    <order>Upstream → Downstream (cannot fix C3 without fixing C2, cannot fix C2 without fixing C1)</order>
    <collective_action>TR1 (organize powerless) targets C1, enabling downstream reform</collective_action>
  </reform_implication>
</structural_physics_arc>
```

---

## VII. VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized (C1, C2, C3)

☑ All χ calculations shown with π and σ values:
  - C1: χ(I₁)=-0.14, χ(I₂)=1.05
  - C2: χ(I₃)=0.36, χ(I₄)=0.69
  - C3: χ(I₅)=0.384, χ(I₆)=0.96

☑ Transformation rules are testable (