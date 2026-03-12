# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Alice
X₂ ← Hatter
X₃ ← March Hare
X₄ ← Dormouse
X₅ ← Queen of Hearts
X₆ ← Cheshire Cat
X₇ ← Caterpillar
X₈ ← White Rabbit

C₁ ← Linguistic Slippage
C₂ ← Social Enforcement (incomplete in Stage 0)
C₃ ← Identity Instability (incomplete in Stage 0)
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Linguistic Slippage

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.20</epsilon>
    <suppression>0.00</suppression>
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
    <chi>ε × π × σ = 0.20 × 1.5 × 0.8 = 0.24</chi>
    <type>Rope</type>
    <threshold_check>χ = 0.24 ≤ 0.35 (rope_chi_ceiling): PASS</threshold_check>
    <threshold_check>ε = 0.20 ≤ 0.45 (rope_epsilon_ceiling): PASS</threshold_check>
    <rationale>
      Low extraction from semantic instability. Constraint provides minimal 
      coordination (shared communication system despite drift). Changeable 
      through collective semantic stabilization. Power position amplifies 
      extraction slightly (less control over meaning-making), but remains 
      within Rope threshold.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>moderate</power>             <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.20 × 1.0 × 0.8 = 0.16</chi>
    <type>Rope</type>
    <threshold_check>χ = 0.16 ≤ 0.35 (rope_chi_ceiling): PASS</threshold_check>
    <threshold_check>ε = 0.20 ≤ 0.45 (rope_epsilon_ceiling): PASS</threshold_check>
    <rationale>
      Moderate power position reduces extraction. Greater agency in 
      meaning-making (can impose interpretations more successfully). 
      Still experiences semantic drift as coordination mechanism rather 
      than extraction trap.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>moderate</power>             <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.20 × 1.0 × 0.8 = 0.16</chi>
    <type>Rope</type>
    <threshold_check>χ = 0.16 ≤ 0.35 (rope_chi_ceiling): PASS</threshold_check>
    <threshold_check>ε = 0.20 ≤ 0.45 (rope_epsilon_ceiling): PASS</threshold_check>
    <rationale>Identical structural position to X₂.</rationale>
  </agent>

  <agent ref="X₆">
    <index>
      <power>analytical</power>           <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>local</scope>                <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.20 × 1.15 × 0.8 = 0.184</chi>
    <type>Rope</type>
    <threshold_check>χ = 0.184 ≤ 0.35 (rope_chi_ceiling): PASS</threshold_check>
    <threshold_check>ε = 0.20 ≤ 0.45 (rope_epsilon_ceiling): PASS</threshold_check>
    <rationale>
      Analytical position provides meta-awareness of semantic drift without 
      being subject to its normalization pressure. Slight extraction increase 
      (π = 1.15) reflects cognitive cost of maintaining analytical distance. 
      Can observe slippage patterns invisible to embedded agents.
    </rationale>
  </agent>

  <indexical_variance>
    C₁: X₁ χ=0.24 → Rope, X₂ χ=0.16 → Rope, X₃ χ=0.16 → Rope, X₆ χ=0.184 → Rope
    
    Low variance. All agents classify C₁ as Rope despite power differences. 
    Extraction differential (0.24 vs 0.16) exists but insufficient to cross 
    type boundaries. Semantic instability operates as coordination mechanism 
    with mild extraction component across all structural positions.
    
    Key insight: Constraint's low base extraction (ε = 0.20) prevents power 
    amplification from producing Snare classification even at powerless index.
  </indexical_variance>

  <boltzmann_test>
    PASS
    
    Test procedure:
    1. Vary P: powerless (π=1.5) → moderate (π=1.0) → analytical (π=1.15)
       Result: All classify as Rope (χ range: 0.16-0.24)
    
    2. Vary S: local (σ=0.8) → regional (σ=0.9) → national (σ=1.0)
       Result: All classify as Rope (χ range: 0.18-0.20 at moderate power)
    
    3. Independence check: Type(C₁, I) = Rope for all tested indices
    
    Interpretation: C₁ exhibits natural-law-like invariance across structural 
    positions. However, this does NOT certify C₁ as Mountain because:
    - Changeable through collective action (semantic stabilization protocols)
    - Lacks natural emergence (constructed through social interaction)
    - Suppression = 0 satisfied, but ε = 0.20 > 0.25 (mountain_extractiveness_max)
    
    Boltzmann PASS indicates well-designed coordination mechanism, not natural law.
  </boltzmann_test>
</constraint>
```

---

## II. TRANSFORMATION RULES

### TR₁: Semantic Stabilization Protocol

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>establish_shared_reference_system</condition>
    <target>C₁</target>
    <agents>X₁, X₂, X₃</agents>
  </trigger>
  
  <index_change>
    <agent>X₁</agent>
    <from>E = identity_locked</from>
    <to>E = constrained</to>
    <rationale>
      Collective semantic protocol provides exit option (can appeal to 
      shared definitions) while maintaining identity connection to system.
    </rationale>
  </index_change>
  
  <chi_recalculation>
    <agent>X₁</agent>
    <before>χ = 0.20 × 1.5 × 0.8 = 0.24 → Rope</before>
    <after>χ = 0.15 × 1.5 × 0.8 = 0.18 → Rope</after>
    <mechanism>
      Base extraction reduced (ε: 0.20 → 0.15) through stabilization. 
      Power and scope unchanged. Type persists as Rope with lower extraction.
    </mechanism>
  </chi_recalculation>
  
  <type_change>Rope → Rope (improved)</type_change>
  
  <preconditions>
    <condition>Coordination = true (C₁ must provide coordination value)</condition>
    <condition>Collective action capacity (multiple agents must participate)</condition>
    <condition>No blocking constraint preventing semantic negotiation</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₂ (if social enforcement punishes semantic stabilization)</constraint>
    <constraint>High-suppression constraint preventing collective organization</constraint>
  </blocked_by>
</transformation_rule>
```

### TR₂: Semantic Drift Acceleration

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>abandon_shared_reference_maintenance</condition>
    <target>C₁</target>
    <agents>X₁, X₂, X₃</agents>
  </trigger>
  
  <index_change>
    <agent>X₁</agent>
    <from>P = powerless, E = identity_locked</from>
    <to>P = powerless, E = trapped</to>
    <rationale>
      Loss of semantic coordination removes exit option. Agent cannot 
      appeal to shared meanings because none exist.
    </rationale>
  </index_change>
  
  <chi_recalculation>
    <agent>X₁</agent>
    <before>χ = 0.20 × 1.5 × 0.8 = 0.24 → Rope</before>
    <after>χ = 0.35 × 1.5 × 0.8 = 0.42 → Tangled Rope</after>
    <mechanism>
      Base extraction increased (ε: 0.20 → 0.35) as semantic instability 
      becomes obstacle rather than coordination mechanism. Crosses into 
      Tangled Rope territory (0.42 > 0.35 rope_chi_ceiling, but < 0.70 
      snare_chi_threshold). Coordination value persists (shared language 
      system) but extraction now dominates experience.
    </mechanism>
  </chi_recalculation>
  
  <type_change>Rope → Tangled Rope</type_change>
  
  <preconditions>
    <condition>Collective maintenance effort withdrawn</condition>
    <condition>No institutional stabilization mechanism</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>Institutional semantic enforcement (would prevent drift)</constraint>
  </blocked_by>
</transformation_rule>
```

### TR₃: Analytical Intervention

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>external_observer_documents_slippage_patterns</condition>
    <target>C₁</target>
    <agent>X₆</agent>
  </trigger>
  
  <index_change>
    <agent>X₁</agent>
    <from>P = powerless</from>
    <to>P = moderate</to>
    <rationale>
      Documentation provides embedded agents with analytical tools. 
      Increases power through awareness of previously invisible patterns.
    </rationale>
  </index_change>
  
  <chi_recalculation>
    <agent>X₁</agent>
    <before>χ = 0.20 × 1.5 × 0.8 = 0.24 → Rope</before>
    <after>χ = 0.20 × 1.0 × 0.8 = 0.16 → Rope</after>
    <mechanism>
      Base extraction unchanged (ε = 0.20). Power modifier reduced 
      (π: 1.5 → 1.0) through analytical capacity transfer. Scope unchanged. 
      Type persists as Rope with reduced extraction.
    </mechanism>
  </chi_recalculation>
  
  <type_change>Rope → Rope (power-shifted)</type_change>
  
  <preconditions>
    <condition>Analytical agent (X₆) present and active</condition>
    <condition>Communication channel between analytical and embedded agents</condition>
    <condition>Embedded agents have capacity to integrate analytical tools</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>Information suppression constraint</constraint>
    <constraint>Cognitive capacity constraint preventing tool integration</constraint>
  </blocked_by>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### Error E₁: Type I — False Mountain

```xml
<error id="E₁">
  <type>I — False Mountain</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  <actual_type>Rope (χ = 0.24, changeable through collective action)</actual_type>
  <perceived_type>Mountain (treats semantic drift as unchangeable natural law)</perceived_type>
  <observable>
    Agent does not attempt collective semantic stabilization despite:
    - Low suppression (Supp = 0.00) indicating no enforcement preventing change
    - Coordination value present (shared communication system exists)
    - Multiple agents available for collective action
    - χ = 0.24 well within Rope threshold (changeable)
    
    Testable condition: When presented with semantic stabilization opportunity, 
    agent declines with justification appealing to inevitability rather than 
    cost-benefit analysis.
  </observable>
  <correction_trigger>
    Successful semantic stabilization by peer group demonstrates changeability. 
    Observation of TR₁ execution by similar agents would falsify Mountain 
    classification.
  </correction_trigger>
  <structural_mechanism>
    Immediate time horizon (T = immediate) combined with identity_locked exit 
    (E = identity_locked) produces Mountain-like phenomenology despite Rope 
    structure. Agent experiences semantic drift as immutable because:
    - Cannot imagine biographical-scale change
    - Identity fused to current semantic system
    - Powerless position amplifies extraction, making constraint feel natural
  </structural_mechanism>
</error>
```

### Error E₂: Type III — Snare-as-Rope (Missing Extraction)

```xml
<error id="E₂">
  <type>III — Snare-as-Rope (Missing Extraction)</type>
  <agent>X₂</agent>
  <constraint>C₁</constraint>
  <actual_type>
    Rope at X₂'s index (χ = 0.16), but would be Tangled Rope at powerless 
    index after drift (χ = 0.42 via TR₂)
  </actual_type>
  <perceived_type>
    Pure coordination mechanism with no extraction component
  </perceived_type>
  <observable>
    Agent advocates for abandoning semantic stabilization maintenance 
    (triggering TR₂) without recognizing extraction consequences for 
    powerless agents. Testable condition:
    - Proposes policy reducing semantic coordination
    - Justifies with efficiency arguments
    - Does not model differential impact by power position
    - Surprised when powerless agents report increased burden
  </observable>
  <correction_trigger>
    Calculate χ(C₁, powerless) after proposed change. If χ > 0.35, 
    extraction component exists regardless of moderate agent's experience.
  </correction_trigger>
  <structural_mechanism>
    Moderate power position (π = 1.0) dampens extraction to χ = 0.16. 
    Agent genuinely experiences C₁ as low-extraction coordination. Error 
    arises from generalizing local experience to all structural positions. 
    Analytical index (π = 1.15) would detect extraction differential.
  </structural_mechanism>
</error>
```

### Error E₃: Type II — Mountain Denial

```xml
<error id="E₃">
  <type>II — Mountain Denial</type>
  <agent>X₁</agent>
  <constraint>C₁</constraint>
  <actual_type>Rope (changeable, but requires collective action)</actual_type>
  <perceived_type>
    Infinitely malleable — individual can unilaterally change semantic system
  </perceived_type>
  <observable>
    Agent attempts unilateral semantic stabilization without collective 
    coordination. Testable condition:
    - Invents private reference system
    - Expects others to adopt without negotiation
    - Expends energy fighting semantic drift alone
    - Experiences depletion when unilateral effort fails
    
    Distinguishing feature: Treats changeable constraint as MORE changeable 
    than it structurally is (denies coordination requirement).
  </observable>
  <correction_trigger>
    Repeated failure of unilateral stabilization attempts. Recognition that 
    semantic systems require collective maintenance (Coord = true).
  </correction_trigger>
  <structural_mechanism>
    Powerless position (π = 1.5) amplifies extraction, creating urgency. 
    Immediate time horizon (T = immediate) prevents recognition of 
    biographical-scale collective action requirement. Agent correctly 
    identifies C₁ as non-Mountain but incorrectly estimates change difficulty.
  </structural_mechanism>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

```xml
<rationality_model>
  <type>BIR — Bounded Institutional Rationality</type>
  
  <justification>
    C₁ (Linguistic Slippage) exhibits properties incompatible with PIR:
    
    1. Coordination value present (Coord = true)
       - PIR would eliminate coordination for marginal utility gain
       - BIR preserves coordination under uncertainty
    
    2. Low suppression (Supp = 0.00)
       - No enforcement mechanism maintaining constraint
       - Emerges from satisficing behavior, not optimization
    
    3. Collective action requirement
       - TR₁ (semantic stabilization) requires multi-agent coordination
       - PIR agents would defect from collective maintenance
       - BIR agents satisfice with "good enough" semantic stability
    
    4. Principal-agent structure implicit
       - No central authority enforcing semantic system
       - Distributed maintenance through satisficing
    
    5. Risk aversion evident
       - Agents maintain semantic coordination despite extraction
       - PIR would abandon for marginal efficiency gain
       - BIR preserves coordination as insurance against worse outcomes
  </justification>
  
  <implications>
    Under BIR, C₁ tends toward:
    - Negotiated Equilibrium (agents bargain over semantic stability level)
    - Seeded Possibility (underground semantic innovations while maintaining 
      surface coordination)
    
    NOT toward:
    - Deterministic Tragedy (would require PIR + implacable extraction)
    - Revolutionary Rupture (C₁ lacks sufficient extraction to trigger)
  </implications>
  
  <attractor_compatibility>
    BIR + Rope-dominated system → Negotiated Equilibrium or Seeded Possibility
    
    From Attractor Compatibility Matrix:
    - Rope-dominated systems under BIR stabilize through bargaining
    - Low extraction (ε = 0.20) prevents tragedy
    - Coordination value (Coord = true) enables negotiation
    - No Mountains present to constrain negotiation space
  </attractor_compatibility>
</rationality_model>
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Negotiated Equilibrium

```xml
<terminal_attractor>
  <type>Negotiated Equilibrium</type>
  
  <justification>
    System reaches stable bargaining equilibrium over semantic coordination level:
    
    1. Constraint profile compatible
       - Single Rope (C₁) with low extraction (ε = 0.20)
       - No Mountains constraining negotiation space
       - No high-extraction Snares forcing tragedy
    
    2. Rationality model compatible
       - BIR enables satisficing bargains
       - Agents accept "good enough" semantic stability
       - Risk aversion prevents defection from coordination
    
    3. Transformation dynamics support equilibrium
       - TR₁ (stabilization) and TR₂ (drift) create negotiation space
       - Agents can adjust semantic coordination level
       - Feedback loop: drift → discomfort → stabilization → maintenance cost
    
    4. Error correction possible
       - E₁ (False Mountain) correctable through peer demonstration
       - E₂ (Snare-as-Rope) correctable through index analysis
       - E₃ (Mountain Denial) correctable through repeated failure
    
    5. Indexical variance low
       - All agents classify C₁ as Rope
       - No fundamental disagreement about constraint type
       - Negotiation over coordination level, not constraint nature
  </justification>
  
  <equilibrium_characteristics>
    <semantic_stability_level>
      Moderate — agents maintain sufficient coordination for communication 
      while tolerating some drift to reduce maintenance costs
    </semantic_stability_level>
    
    <bargaining_structure>
      Implicit negotiation through satisficing:
      - Powerless agents (X₁) push for higher stability (reduce extraction)
      - Moderate agents (X₂, X₃) accept moderate stability (balance coordination 
        value against maintenance cost)
      - Analytical agent (X₆) provides meta-stability through documentation
    </bargaining_structure>
    
    <stability_mechanism>
      Self-correcting feedback:
      - Excessive drift → communication breakdown → stabilization pressure
      - Excessive rigidity → maintenance cost → drift tolerance
      - Equilibrium at χ ≈ 0.20 (current level)
    </stability_mechanism>
    
    <perturbation_response>
      System returns to equilibrium after shocks:
      - External semantic disruption → collective restabilization (TR₁)
      - Maintenance effort withdrawal → drift until discomfort triggers 
        restabilization (TR₂ → TR₁)
    </perturbation_response>
  </equilibrium_characteristics>
  
  <alternative_attractors_rejected>
    <deterministic_tragedy>
      REJECTED — Requires PIR or high-extraction Snares. C₁ has ε = 0.20 
      (low extraction) and system operates under BIR (satisficing prevents 
      tragedy).
    </deterministic_tragedy>
    
    <revolutionary_rupture>
      REJECTED — Requires constraint logic disruption. C₁ provides genuine 
      coordination value (Coord = true) that agents want to preserve. No 
      motivation to rupture semantic system entirely.
    </revolutionary_rupture>
    
    <seeded_possibility>
      POSSIBLE BUT LESS LIKELY — Would require surface tragedy with underground 
      transformation. C₁'s low extraction (χ ≤ 0.24 for all agents) doesn't 
      create sufficient pressure for underground innovation. Agents can 
      negotiate openly under BIR.
    </seeded_possibility>
  </alternative_attractors_rejected>
</terminal_attractor>
```

---

## VI. VALIDATION CHECKLIST

```
☑ All constraints formalized with ε, Supp, Coord, Asym
  - C₁: ε=0.20, Supp=0.00, Coord=false, Asym=false

☑ All χ calculations shown with π and σ values
  - X₁: χ = 0.20 × 1.5 × 0.8 = 0.24
  - X₂: χ = 0.20 × 1.0 × 0.8 = 0.16
  - X₃: χ = 0.20 × 1.0 × 0.8 = 0.16
  - X₆: χ = 0.20 × 1.15 × 0.8 = 0.184

☑ All characters use variable names (X₁, X₂, ...) — no source identifiers in body
  - Variable mapping in header only
  - Body uses X₁, X₂, X₃, X₆ exclusively

☑ Variable mapping table present in header (for traceability before anonymization)
  - Complete mapping provided

☑ No source occupation, setting, or domain vocabulary anywhere in output
  - No references to specific narrative context
  - Abstract structural descriptions only

☑ No <experience>, <dialogue_markers>, or <narrative> fields
  - Structural analysis only

☑ Transformation rules use abstract trigger descriptions
  - TR₁: "establish_shared_reference_system"
  - TR₂: "abandon_shared_reference_maintenance"
  - TR₃: "external_observer_documents_slippage_patterns"

☑ Error observables are testable structural conditions
  - E₁: "does not attempt collective semantic stabilization despite..."
  - E₂: "advocates for abandoning semantic stabilization maintenance..."
  - E₃: "attempts unilateral semantic stabilization without collective coordination"

☑ Institutional rationality model specified (PIR/BIR) with justification
  - BIR selected with five-point justification

☑ Terminal attractor selected, justified, and compatible
  - Negotiated Equilibrium selected
  - Compatible with BIR + Rope-dominated system
  - Alternatives explicitly rejected with reasoning

☑ Indexical variance explicitly preserved across agents
  - Low variance documented (all agents classify as Rope)
  - Power differentials shown (χ range: 0.16-0.24)

☑ Boltzmann test run for each constraint claiming Mountain status
  - C₁ tested and PASSED independence check
  - Interpretation: well-designed coordination, not natural law

☑ No ambiguity in specifications
  - All thresholds checked explicitly
  - All calculations shown
  - All classifications justified

☑ Output is pure symbolic network — a reader cannot identify the source work
  - No source-specific vocabulary
  - Abstract structural descriptions only
  - Variable names used throughout
```

---

## VII. NOTES ON INCOMPLETE STAGE 0 DATA

Stage 0 analysis referenced C₂ (Social Enforcement) and C₃ (Identity Instability) but provided insufficient data for formalization. Required for complete specification:

### C₂ (Social Enforcement) — Missing Data:
- Base extractiveness (ε)
- Suppression level (Supp)
- Coordination value (Coord)
- Asymmetry (Asym)
- Per-agent index specifications
- Relationship to C₁ (feeds_into mechanism)

### C₃ (Identity Instability) — Missing Data:
- Base extractiveness (ε)
- Suppression level (Supp)
- Coordination value (Coord)
- Asymmetry (Asym)
- Per-agent index specifications
- Relationship to C₁ (feeds_into mechanism)

**Recommendation:** Complete Stage 0 analysis for C₂ and C₃ before proceeding to Stage 2. Current specification covers C₁ only. Full constraint network requires all three constraints formalized with transformation rules showing inter-constraint dynamics.

---

**END OF STAGE 1 FORMAL SPECIFICATION**