# STAGE 1: FORMAL CONSTRAINT SPECIFICATION

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Keeper (narrator)
X₂ ← Isa Wendl
X₃ ← Verentz's Grandson
G₁ ← Community (collective agent)
I₁ ← Ledger Institution (the system itself as agent)
```

---

## I. CONSTRAINT FORMALIZATIONS

### Constraint C₁: Value Incommensurability

```xml
<constraint id="C₁">
  <properties>
    <epsilon>0.0</epsilon>
    <suppression>0.0</suppression>
    <coordination>false</coordination>
    <asymmetry>false</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>analytical</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.0 × 1.15 × 0.8 = 0.0</chi>
    <type>Mountain</type>
    <threshold_check>
      ε ≤ 0.25 (mountain_extractiveness_max): PASS
      Supp ≤ 0.05 (mountain_suppression_ceiling): PASS
    </threshold_check>
    <rationale>
      Zero extraction, zero suppression. Natural emergence property: 
      incommensurability between direct reciprocal exchange and 
      quantified debt-settlement arises from structural incompatibility 
      of value systems, not human construction. Immutable from 
      biographical horizon — no action within lifetime can make 
      these value systems commensurable.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.0 × 1.5 × 0.8 = 0.0</chi>
    <type>Mountain</type>
    <threshold_check>
      ε ≤ 0.25: PASS
      Supp ≤ 0.05: PASS
    </threshold_check>
    <rationale>
      Same structural property. Power position irrelevant to 
      classification — incommensurability affects all agents equally.
      Trapped exit status reflects inability to escape C₃, not C₁.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>      <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.0 × (-0.2) × 0.8 = 0.0</chi>
    <type>Mountain</type>
    <threshold_check>
      ε ≤ 0.25: PASS
      Supp ≤ 0.05: PASS
    </threshold_check>
    <rationale>
      Institutional position does not alter fundamental 
      incommensurability. Generational horizon does not make 
      value systems commensurable. Classification invariant 
      across all tested indices.
    </rationale>
  </agent>

  <indexical_variance>None — Mountain classification invariant</indexical_variance>

  <boltzmann_test>
    <procedure>
      Tested indices: (analytical, biographical, analytical, local),
                     (powerless, biographical, trapped, local),
                     (institutional, generational, arbitrage, local)
      
      Type(C₁, I₁) = Mountain
      Type(C₁, I₂) = Mountain  
      Type(C₁, I₃) = Mountain
      
      Classification independent of P (analytical vs powerless vs institutional)
      Classification independent of T (biographical vs generational)
      Classification independent of E (analytical vs trapped vs arbitrage)
      Classification independent of S (all local, but property holds universally)
    </procedure>
    <result>PASS</result>
    <certification>
      C₁ exhibits Boltzmann independence. Incommensurability is a 
      structural property of value system incompatibility, not a 
      constructed constraint. Qualifies as Mountain.
    </certification>
  </boltzmann_test>
</constraint>
```

---

### Constraint C₂: Ideology of Objective Measurement

```xml
<constraint id="C₂">
  <properties>
    <epsilon>0.75</epsilon>
    <suppression>0.7</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × 1.15 × 0.8 = 0.69</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      χ ≥ 0.46 (tangled_chi_floor): PASS
      χ ≤ 0.70 (snare_chi_threshold): PASS
      Coord = true: PASS
      Asym = true: PASS
    </threshold_check>
    <rationale>
      Analytical position detects extraction (π = 1.15 breaks 
      normalization). Ideology provides coordination value 
      (legitimizes institutional function) while asymmetrically 
      distributing costs (justifies extraction from powerless). 
      Irreducible hybrid: genuine coordination AND extraction.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × 1.5 × 0.8 = 0.90</chi>
    <type>Snare</type>
    <threshold_check>
      χ > 0.70 (snare_chi_threshold): PASS
      ¬LowBaseExtraction (ε = 0.75): PASS
    </threshold_check>
    <rationale>
      Powerless position amplifies extraction. Ideology justifies 
      the system that traps agent. High suppression (0.7) maintains 
      belief despite lived contradiction. No coordination value 
      visible from this index — only extraction.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>      <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.75 × (-0.2) × 0.8 = -0.12</chi>
    <type>Rope</type>
    <threshold_check>
      χ ≤ 0.35 (rope_chi_ceiling): PASS
      χ ≤ 0 → ε ceiling bypassed: PASS
      Changeable from generational horizon: PASS
    </threshold_check>
    <rationale>
      Institutional position inverts extraction (π = -0.2). 
      Agent is net beneficiary — ideology legitimizes extraction 
      FROM system. Provides genuine coordination value (enables 
      institutional function). Changeable distinguishes from Mountain.
    </rationale>
  </agent>

  <indexical_variance>
    C₂: X₁ χ=0.69 → Tangled Rope, X₂ χ=0.90 → Snare, X₃ χ=-0.12 → Rope
    
    Same constraint exhibits three distinct types across power positions.
    Analytical index detects hybrid nature (coordination + extraction).
    Powerless index experiences pure extraction (Snare).
    Institutional index experiences pure coordination (Rope, net benefit).
    
    This is the canonical pattern of index-sensitive classification.
  </indexical_variance>

  <boltzmann_test>
    <procedure>
      Type(C₂, analytical) = Tangled Rope
      Type(C₂, powerless) = Snare
      Type(C₂, institutional) = Rope
      
      Classification varies with P (power position)
      Burden distribution asymmetric across power gradient
    </procedure>
    <result>FAIL</result>
    <certification>
      C₂ is constructed, not natural. Ideological constraint 
      requiring active suppression (0.7) to maintain. Power-dependent 
      classification disqualifies Mountain status regardless of 
      other properties.
    </certification>
  </boltzmann_test>
</constraint>
```

---

### Constraint C₃: Centralized Reputation Ledger

```xml
<constraint id="C₃">
  <properties>
    <epsilon>0.8</epsilon>
    <suppression>0.1</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </properties>

  <agent ref="X₁">
    <index>
      <power>analytical</power>        <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.15 × 0.8 = 0.736</chi>
    <type>Snare</type>
    <threshold_check>
      χ > 0.70 (snare_chi_threshold): PASS
      ¬LowBaseExtraction (ε = 0.8): PASS
    </threshold_check>
    <rationale>
      Analytical position detects high extraction despite low 
      suppression (0.1). System appears voluntary but structurally 
      coercive. Agent can analyze mechanism but cannot exit 
      (constrained exit, professional role obligations). 
      Extraction dominates coordination value from this index.
    </rationale>
  </agent>

  <agent ref="X₂">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.5 × 0.8 = 0.96</chi>
    <type>Snare</type>
    <threshold_check>
      χ > 0.70 (snare_chi_threshold): PASS
      ¬LowBaseExtraction (ε = 0.8): PASS
    </threshold_check>
    <rationale>
      Powerless position maximally amplifies extraction. 
      Trapped exit — no alternative coordination mechanisms 
      available. System extracts maximum value while providing 
      minimal coordination benefit to this agent.
    </rationale>
  </agent>

  <agent ref="X₃">
    <index>
      <power>institutional</power>      <!-- π = -0.2 -->
      <time>generational</time>
      <exit>arbitrage</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × (-0.2) × 0.8 = -0.128</chi>
    <type>Rope</type>
    <threshold_check>
      χ ≤ 0.35 (rope_chi_ceiling): PASS
      χ ≤ 0 → ε ceiling bypassed: PASS
    </threshold_check>
    <rationale>
      Institutional position inverts extraction. Agent owns/controls 
      ledger mechanism — extracts FROM system rather than being 
      extracted from. Arbitrage exit option (can play alternatives 
      against each other). Genuine coordination value for this agent.
    </rationale>
  </agent>

  <agent ref="X₂_post_entry">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>local</scope>              <!-- σ = 0.8 -->
    </index>
    <chi>ε × π × σ = 0.8 × 1.0 × 0.8 = 0.64</chi>
    <type>Tangled Rope</type>
    <threshold_check>
      χ ≥ 0.46 (tangled_chi_floor): PASS
      χ ≤ 0.70 (snare_chi_threshold): PASS
      Coord = true: PASS
      Asym = true: PASS
    </threshold_check>
    <rationale>
      Post-entry state: power position elevated to moderate 
      (system participation grants agency). Exit option shifts 
      to identity_locked (structurally mobile but cognitively 
      fused to constraint — success within system becomes 
      identity). Real coordination value now visible (access 
      to credit, reputation benefits) but extraction remains 
      high. Irreducible hybrid.
    </rationale>
  </agent>

  <indexical_variance>
    C₃: X₁ χ=0.736 → Snare, X₂ χ=0.96 → Snare, X₃ χ=-0.128 → Rope,
        X₂_post χ=0.64 → Tangled Rope
    
    Ledger exhibits maximum indexical variance. From powerless and 
    analytical positions: pure extraction (Snare). From institutional 
    position: pure coordination (Rope, net benefit). From successful 
    participant position: hybrid (Tangled Rope — real coordination 
    at high extractive cost).
    
    This variance is the mechanism of the tragedy: system appears 
    as Rope to beneficiaries, Snare to victims, Tangled to those 
    who succeed within it. Each classification is objectively true 
    from its index.
  </indexical_variance>

  <boltzmann_test>
    <procedure>
      Type(C₃, analytical) = Snare
      Type(C₃, powerless) = Snare
      Type(C₃, institutional) = Rope
      Type(C₃, moderate + identity_locked) = Tangled Rope
      
      Classification varies with P (power position)
      Classification varies with E (exit options)
      Burden distribution highly asymmetric
    </procedure>
    <result>FAIL</result>
    <certification>
      C₃ is constructed institutional mechanism. Power-dependent 
      and exit-dependent classification. High base extraction (0.8) 
      with asymmetric distribution. Not a natural law — a designed 
      system with differential impact by structural position.
    </certification>
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
    <target>C₃</target>
    <agent>X₂</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = organized (π=0.4), E = mobile</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.8 × 0.4 × 0.8 = 0.256 → Rope</after>
  </chi_recalculation>
  
  <type_change>Snare → Rope</type_change>
  
  <preconditions>
    <condition>Multiple agents at powerless index</condition>
    <condition>Communication channels available</condition>
    <condition>Alternative coordination mechanism feasible</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₂ (ideology normalizes atomization)</constraint>
    <constraint>C₃ (ledger creates coordination dependency)</constraint>
    <mechanism>
      High suppression in C₂ (0.7) prevents recognition of 
      collective action as legitimate. C₃'s coordination value 
      (even at high extraction) creates switching cost. 
      Agents who succeed within C₃ shift to identity_locked 
      exit status, removing them from collective action pool.
    </mechanism>
  </blocked_by>
  
  <omega_marker>
    Transformation theoretically possible (χ calculation shows 
    feasibility) but empirically blocked. Stage 0 identifies 
    this as unresolved: "why the community atomizes and adopts 
    the new system's logic so quickly, rather than mounting 
    collective resistance."
  </omega_marker>
</transformation_rule>
```

### TR₂: Ledger Entry (Powerless → Moderate)

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>successful_system_participation</condition>
    <target>C₃</target>
    <agent>X₂</agent>
  </trigger>
  
  <index_change>
    <from>P = powerless (π=1.5), E = trapped</from>
    <to>P = moderate (π=1.0), E = identity_locked</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
    <after>χ = 0.8 × 1.0 × 0.8 = 0.64 → Tangled Rope</after>
  </chi_recalculation>
  
  <type_change>Snare → Tangled Rope</type_change>
  
  <preconditions>
    <condition>Agent accumulates positive ledger score</condition>
    <condition>Score crosses threshold for system benefits</condition>
  </preconditions>
  
  <blocked_by>None (this is the system's designed pathway)</blocked_by>
  
  <mechanism>
    System converts victims into participants. Power position 
    elevates (moderate agency granted) but exit option degrades 
    (identity_locked — success within system becomes cognitive 
    anchor). Agent now experiences real coordination value 
    (access to credit, reputation benefits) but extraction 
    remains high (0.64 χ). Hybrid state is stable — agent 
    unlikely to challenge system that validates their success.
  </mechanism>
</transformation_rule>
```

### TR₃: Ideological Rejection

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>reject_measurement_ideology</condition>
    <target>C₂</target>
    <agent>X₁</agent>
  </trigger>
  
  <index_change>
    <from>P = analytical (π=1.15), E = constrained</from>
    <to>P = analytical (π=1.15), E = analytical</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.75 × 1.15 × 0.8 = 0.69 → Tangled Rope</before>
    <after>χ = 0.75 × 1.15 × 0.8 = 0.69 → Tangled Rope</after>
    <note>χ unchanged — exit shift does not affect power scaling</note>
  </chi_recalculation>
  
  <type_change>Tangled Rope → Tangled Rope (no type change)</type_change>
  
  <preconditions>
    <condition>Analytical position maintained</condition>
    <condition>Recognition of ideological construction</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₃ (professional role obligations)</constraint>
    <mechanism>
      Agent can achieve analytical exit from C₂ (recognize 
      ideology as constructed) but remains constrained by C₃ 
      (must operate within system professionally). Cognitive 
      liberation does not translate to structural liberation. 
      This is the narrator's position: sees the mechanism, 
      cannot escape it.
    </mechanism>
  </blocked_by>
</transformation_rule>
```

### TR₄: System Expansion

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>expand_ledger_scope</condition>
    <target>C₃</target>
    <agent>I₁</agent>
  </trigger>
  
  <index_change>
    <from>S = local (σ=0.8)</from>
    <to>S = regional (σ=0.9)</to>
  </index_change>
  
  <chi_recalculation>
    <agent ref="X₂">
      <before>χ = 0.8 × 1.5 × 0.8 = 0.96 → Snare</before>
      <after>χ = 0.8 × 1.5 × 0.9 = 1.08 → Snare</after>
    </agent>
    <agent ref="X₃">
      <before>χ = 0.8 × (-0.2) × 0.8 = -0.128 → Rope</before>
      <after>χ = 0.8 × (-0.2) × 0.9 = -0.144 → Rope</after>
    </agent>
  </chi_recalculation>
  
  <type_change>
    Snare → Snare (intensified)
    Rope → Rope (enhanced benefit)
  </type_change>
  
  <preconditions>
    <condition>Local system stabilized</condition>
    <condition>Institutional capacity for expansion</condition>
  </preconditions>
  
  <blocked_by>None (expansion is system's inherent logic)</blocked_by>
  
  <mechanism>
    Scope expansion amplifies existing asymmetry. Powerless 
    agents experience increased extraction (σ rises, verification 
    harder). Institutional agents experience increased benefit 
    (larger extraction base). Regional scope makes collective 
    resistance harder (coordination costs rise). This is the 
    story's conclusion: local tragedy replicates at scale.
  </mechanism>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### E₁: Type III — Snare-as-Rope (Missing Extraction)

```xml
<error id="E₁">
  <type>III — Snare-as-Rope</type>
  <agent>X₂_post_entry</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Tangled Rope (χ=0.64 from moderate index)</actual_type>
  <perceived_type>Rope (coordination without extraction)</perceived_type>
  
  <observable>
    Agent advocates for system expansion despite experiencing 
    χ=0.64 (high extraction). Does not recognize asymmetric 
    cost distribution. Treats personal success as evidence of 
    system fairness rather than evidence of power position shift.
  </observable>
  
  <correction_trigger>
    Recompute χ from powerless index (χ=0.96 → Snare). 
    Recognition that coordination value is index-dependent — 
    what appears as Rope from moderate position is Snare 
    from powerless position.
  </correction_trigger>
  
  <mechanism>
    Identity-locked exit status prevents recognition. Success 
    within system becomes cognitive anchor. Agent normalizes 
    extraction component of Tangled Rope, perceives only 
    coordination component. Classic error pattern: beneficiary 
    misclassifies hybrid as pure coordination.
  </mechanism>
</error>
```

### E₂: Type I — False Mountain

```xml
<error id="E₂">
  <type>I — False Mountain</type>
  <agent>G₁</agent>
  <constraint>C₃</constraint>
  
  <actual_type>Snare (χ=0.96 from powerless index)</actual_type>
  <perceived_type>Mountain (unchangeable terrain)</perceived_type>
  
  <observable>
    Collective does not attempt alternative coordination 
    mechanisms despite χ calculations indicating feasibility 
    (TR₁ shows organized χ=0.256 → Rope). Treats ledger as 
    inevitable rather than constructed. No resistance despite 
    high extraction.
  </observable>
  
  <correction_trigger>
    Boltzmann test failure. C₃ classification varies by power 
    position (Snare for powerless, Rope for institutional) → 
    constructed, not natural. Recognition that "unchangeable" 
    is index-dependent claim, not objective property.
  </correction_trigger>
  
  <mechanism>
    C₂ (ideology) normalizes C₃ as objective measurement system. 
    High suppression (0.7) in C₂ prevents recognition of C₃ as 
    constructed. Immediate time horizon makes biographical-scale 
    change appear impossible. Trapped exit status reinforces 
    perception of immutability.
  </mechanism>
</error>
```

### E₃: Type V.a — Tangled-as-Rope

```xml
<error id="E₃">
  <type>V.a — Tangled-as-Rope</type>
  <agent>X₁</agent>
  <constraint>C₂</constraint>
  
  <actual_type>Tangled Rope (χ=0.69 from analytical index)</actual_type>
  <perceived_type>Rope (pure coordination)</perceived_type>
  
  <observable>
    Agent continues professional role despite recognizing 
    extraction mechanism. Justifies participation through 
    coordination value (ideology provides meaning to work) 
    while minimizing extraction component (harm to others). 
    Does not attempt to reform or exit.
  </observable>
  
  <correction_trigger>
    Explicit χ decomposition: ε=0.75 (high base extraction), 
    Coord=true (genuine coordination), Asym=true (asymmetric 
    distribution). Recognition that coordination and extraction 
    are inseparable in this constraint — cannot preserve one 
    while eliminating the other.
  </correction_trigger>
  
  <mechanism>
    Analytical position detects hybrid nature (π=1.15 breaks 
    normalization) but constrained exit prevents action. 
    Agent rationalizes continued participation by emphasizing 
    coordination component, suppressing extraction component. 
    This is the narrator's error: sees the mechanism, 
    participates anyway, justifies through partial truth.
  </mechanism>
</error>
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Model Selection: Bounded Institutional Rationality (BIR)

```xml
<rationality_model>
  <type>BIR</type>
  
  <justification>
    I₁ (ledger institution) exhibits satisficing behavior under 
    uncertainty, not utility maximization. Evidence:
    
    1. Low suppression (0.1) in C₃ — system relies on voluntary 
       participation, not coercion. PIR would maximize extraction 
       through force.
    
    2. Coordination value genuinely provided — ledger solves real 
       coordination problems (credit access, reputation signaling). 
       PIR would extract without providing coordination.
    
    3. Principal-agent problems visible — X₃ (grandson) operates 
       ledger for profit, not perfect institutional optimization. 
       Human institution with human incentives.
    
    4. Risk aversion — system expansion (TR₄) is gradual (local → 
       regional), not immediate global maximization.
    
    5. Negotiation possible — agents can succeed within system 
       (TR₂), suggesting Pareto-improving bargains available. 
       PIR would permit only Pareto-improving changes, but BIR 
       allows satisficing equilibria.
  </justification>
  
  <implications>
    BIR model makes Negotiated Equilibrium and Seeded Possibility 
    attractors reachable. Deterministic Tragedy still possible 
    but not inevitable. System can stabilize at suboptimal 
    equilibrium (Tangled Rope dominant) rather than running to 
    pure extraction completion.
  </implications>
  
  <alternative_rejected>
    PIR rejected because:
    - Would predict higher suppression (maximize extraction through force)
    - Would predict no genuine coordination value (pure extraction)
    - Would predict immediate global expansion (maximize scale)
    - Would make only Deterministic Tragedy attractor reachable
    - Does not match empirical pattern of gradual, negotiated adoption
  </alternative_rejected>
</rationality_model>
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Seeded Possibility

```xml
<terminal_attractor>
  <type>Seeded Possibility</type>
  
  <definition>
    Surface tragedy (constraints run to apparent completion) with 
    underground transformation (alternative logic persists in 
    hidden form, seeds future rupture).
  </definition>
  
  <justification>
    1. Surface tragedy evident:
       - C₃ expands (TR₄: local → regional)
       - G₁ atomizes (E₂: False Mountain prevents collective action)
       - X₂ converts to participant (TR₂: Snare → Tangled Rope)
       - X₁ remains trapped in analytical recognition without escape
    
    2. Underground transformation markers:
       - C₄ (informal mutual aid) deferred but not eliminated — 
         "ghost of value" persists as memory/contrast
       - X₁'s analytical position (E₃) maintains recognition of 
         extraction mechanism — knowledge persists even if action blocked
       - C₁ (value incommensurability) remains Mountain — fundamental 
         incompatibility unresolved, will resurface
    
    3. Compatibility with BIR:
       - BIR permits suboptimal equilibria (Tangled Rope stable state)
       - BIR allows underground alternatives (not perfect optimization)
       - Seeded Possibility requires institutional satisficing, not 
         maximization
    
    4. Constraint profile compatibility:
       - Mountain-dominated (C₁ foundational)
       - Tangled Ropes present (C₂ from analytical, C₃ from moderate)
       - Snares present (C₂ from powerless, C₃ from powerless/analytical)
       - Matrix indicates: Seeded Possibility compatible with this mix
  </justification>
  
  <mechanism>
    System achieves stable extraction equilibrium (C₃ as Tangled Rope 
    for participants, Snare for excluded). Expansion proceeds (TR₄). 
    But:
    
    - C₁ (Mountain) ensures value incommensurability never resolved
    - C₄ (deferred) preserves memory of alternative coordination
    - X₁'s analytical recognition (even if trapped) maintains knowledge
    - Future agents at different indices may trigger rupture
    
    Surface: tragedy of the commons, ledger dominance, atomization.
    Underground: unresolved incommensurability, persistent alternatives, 
    analytical recognition. Seeds planted for future transformation.
  </mechanism>
  
  <alternatives_rejected>
    Deterministic Tragedy: Rejected because BIR permits equilibria, 
    not inevitable completion. System stabilizes at Tangled Rope, 
    does not run to pure Snare.
    
    Negotiated Equilibrium: Rejected because no evidence of bargaining 
    between I₁ and G₁. Atomization (E₂) prevents collective negotiation. 
    Individual agents negotiate (TR₂) but not collective.
    
    Revolutionary Rupture: Rejected because no mechanism for disrupting 
    constraint logic itself. C₁ (Mountain) blocks rupture — value 
    incommensurability cannot be eliminated, only suppressed.
  </alternatives_rejected>
</terminal_attractor>
```

---

## VI. VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized with ε, Supp, Coord, Asym
☑ All χ calculations shown with π and σ values
☑ All characters use variable names (X₁, X₂, X₃, G₁, I₁)
☑ Variable mapping table present in header (traceability)
☑ No source occupation, setting, or domain vocabulary in body
☑ No <experience>, <dialogue_markers>, or <narrative> fields
☑ Transformation rules use abstract trigger descriptions
☑ Error observables are testable structural conditions
☑ Institutional rationality model specified (BIR) with justification
☑ Terminal attractor selected (Seeded Possibility), justified, compatible
☑ Indexical variance explicitly preserved across agents
☑ Boltzmann test run for each constraint (C₁ PASS, C₂ FAIL, C₃ FAIL)
☑ No ambiguity in specifications
☑ Output is pure symbolic network
```

---

## VII. STRUCTURAL SUMMARY

### Constraint Network Topology

```
C₁ (Mountain) ──┬──> C₃ (Snare/Tangled/Rope)
                │
C₂ (Tangled/Snare/Rope) ──┘

Generation sequence: C₁ → C₂ → C₃
Dependency: C₃ downstream of both C₁ and C₂
```

### Dominant Patterns

1. **Indexical variance maximized in C₃:**
   - Powerless: Snare (χ=0.96)
   - Analytical: Snare (χ=0.736)
   - Moderate: Tangled Rope (χ=0.64)
   - Institutional: Rope (χ=-0.128)

2. **Transformation pathway:**
   - Powerless → Moderate (TR₂): Snare → Tangled Rope
   - System expansion (TR₄): Amplifies asymmetry
   - Collective action (TR₁): Blocked by C₂ and C₃

3. **Error cluster:**
   - E₁: Participants misclassify Tangled as Rope
   - E₂: Collective misclassifies Snare as Mountain
   - E₃: Analyst misclassifies Tangled as Rope

4. **Terminal state:**
   - Surface: Expansion, atomization, conversion
   - Underground: Incommensurability, memory, recognition
   - Attractor: Seeded Possibility

---

**END OF STAGE 1 FORMALIZATION**