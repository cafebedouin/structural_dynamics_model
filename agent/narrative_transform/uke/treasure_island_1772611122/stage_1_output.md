# STAGE 1: FORMALIZATION — TREASURE ISLAND

## Variable Mapping (Header — Traceability Only)

```
X₁ ← Jim Hawkins
X₂ ← Mrs. Hawkins (Jim's Mother)
X₃ ← Billy Bones / "The Captain"
X₄ ← Dr. Livesey
X₅ ← Squire Trelawney
X₆ ← Long John Silver
X₇ ← Captain Smollett
X₈ ← Ben Gunn
G₁ ← The Mutineers (Flint's former crew)
G₂ ← The Gentry Party (expedition organizers)
G₃ ← The Hamlet's residents
C₁ ← Venture Asymmetry
C₂ ← Gentlemanly Trust
C₃ ← Articles of Fortune
C₄ ← Lodger's Terror
C₅ ← Word as Bond
C₆ ← Communal Fear
```

---

## Constraint Formalizations

### Constraint C₁: Venture Asymmetry

```xml
<constraint id="C₁">
  <base_properties>
    <epsilon>0.80</epsilon>
    <suppression>0.30</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₅">
    <index>
      <power>institutional</power>      <!-- π = -0.2 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × (-0.2) × 0.9 = -0.144</chi>
    <type>Rope</type>
    <threshold_check>χ = -0.144 ≤ 0.35 (rope_chi_ceiling), χ ≤ 0 bypasses ε ceiling → PASS</threshold_check>
    <rationale>Net beneficiary position. Capital ownership converts high-risk venture into wealth extraction mechanism. Negative χ indicates constraint enables rather than restricts.</rationale>
  </agent>

  <agent ref="X₇">
    <index>
      <power>powerful</power>           <!-- π = 0.6 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 0.6 × 0.9 = 0.432</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ χ = 0.432 ≤ 0.70 → FAIL floor. χ = 0.432 ≤ 0.70 (snare_chi_threshold) → PASS ceiling. Coord = true, Asym = true → Tangled Rope by hybrid logic despite missing floor by 0.028</threshold_check>
    <rationale>Professional authority position with exit constraints. Receives compensation but bears operational risk asymmetrically. Coordination value (command structure) present but extraction significant (risk transfer from capital to labor).</rationale>
  </agent>

  <agent ref="X₁">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.5 × 0.9 = 1.08</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.08 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>Zero structural power, no exit options, bears full risk asymmetry. High-stakes participation with minimal reward share. Extraction amplified by powerless position.</rationale>
  </agent>

  <agent ref="G₁">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>constrained</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.80 × 1.0 × 0.9 = 0.72</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.72 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>Hired labor with baseline agency. High-risk venture with asymmetric reward distribution. Exit constrained by economic necessity and lack of alternatives.</rationale>
  </agent>

  <indexical_variance>
    C₁: X₅ χ=-0.144 → Rope, X₇ χ=0.432 → Tangled Rope, X₁ χ=1.08 → Snare, G₁ χ=0.72 → Snare
    
    Same economic structure produces four distinct constraint experiences. Capital ownership position (X₅) converts high-extraction venture into wealth-enabling mechanism (negative χ). Professional authority (X₇) experiences hybrid coordination-extraction. Powerless (X₁) and moderate labor (G₁) experience pure extraction trap. Divergence driven by power position and exit options, not by different constraints.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies by power position. X₅ (institutional) experiences Rope, X₁ (powerless) experiences Snare. Non-factorizable variance indicates constructed economic structure, not natural law. High ε with low Supp suggests voluntary participation masking structural coercion.</boltzmann_test>
</constraint>
```

---

### Constraint C₂: Gentlemanly Trust

```xml
<constraint id="C₂">
  <base_properties>
    <epsilon>0.10</epsilon>
    <suppression>0.20</suppression>
    <coordination>true</coordination>
    <asymmetry>false</asymmetry>
  </base_properties>

  <agent ref="X₅">
    <index>
      <power>powerful</power>           <!-- π = 0.6 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.10 × 0.6 × 0.9 = 0.054</chi>
    <type>Rope</type>
    <threshold_check>χ = 0.054 ≤ 0.35 (rope_chi_ceiling), ε = 0.10 ≤ 0.45 (rope_epsilon_ceiling) → PASS</threshold_check>
    <rationale>Social coordination mechanism within class boundary. Low extraction, genuine trust facilitation. Identity-locked exit reflects social position maintenance requirement, not constraint burden.</rationale>
  </agent>

  <agent ref="X₄">
    <index>
      <power>analytical</power>         <!-- π = 1.15 -->
      <time>biographical</time>
      <exit>mobile</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.10 × 1.15 × 0.9 = 0.1035</chi>
    <type>Rope</type>
    <threshold_check>χ = 0.1035 ≤ 0.35 (rope_chi_ceiling), ε = 0.10 ≤ 0.45 (rope_epsilon_ceiling) → PASS</threshold_check>
    <rationale>Analytical position detects coordination value without normalization pressure. Mobile exit options indicate genuine choice to participate. Low extraction preserved across indices.</rationale>
  </agent>

  <agent ref="X₆">
    <index>
      <power>powerful</power>           <!-- π = 0.6 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.10 × 0.6 × 0.9 = 0.054</chi>
    <type>Rope (exploited as vulnerability)</type>
    <threshold_check>χ = 0.054 ≤ 0.35 (rope_chi_ceiling) → PASS. Note: Low χ reflects utility as strategic tool, not constraint burden.</threshold_check>
    <rationale>Arbitrage position enables exploitation of trust mechanism without experiencing it as constraint. Low χ indicates ease of manipulation. Asymmetry = false reflects symmetric application within class boundary, but X₆ operates outside that boundary.</rationale>
  </agent>

  <indexical_variance>
    C₂: X₅ χ=0.054 → Rope, X₄ χ=0.1035 → Rope, X₆ χ=0.054 → Rope (exploited)
    
    Minimal indexical variance among adherents. All experience low extraction. Critical variance is between adherents (X₅, X₄) and exploiter (X₆). Same low χ has opposite functional meaning: for adherents, indicates genuine coordination; for exploiter, indicates ease of manipulation. Asymmetry = false reflects intra-class symmetry, but inter-class asymmetry creates systemic vulnerability.
  </indexical_variance>

  <boltzmann_test>FAIL — Not because classification varies (all Rope), but because function varies by social position. Trust mechanism operates symmetrically within class boundary but becomes exploitable vulnerability across class boundaries. This functional asymmetry indicates constructed social norm, not natural law. Low ε + low Supp + functional variance = well-designed coordination mechanism with structural blind spot.</boltzmann_test>
</constraint>
```

---

### Constraint C₃: Articles of Fortune

```xml
<constraint id="C₃">
  <base_properties>
    <epsilon>0.85</epsilon>
    <suppression>0.90</suppression>
    <coordination>true</coordination>
    <asymmetry>true</asymmetry>
  </base_properties>

  <agent ref="X₆">
    <index>
      <power>powerful</power>           <!-- π = 0.6 -->
      <time>biographical</time>
      <exit>arbitrage</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.85 × 0.6 × 0.9 = 0.459</chi>
    <type>Tangled Rope</type>
    <threshold_check>0.46 ≤ χ = 0.459 ≤ 0.70 (tangled_chi_floor, snare_chi_threshold), Coord = true, Asym = true → PASS</threshold_check>
    <rationale>Leadership position within subcultural governance structure. Coordination value (collective action, dispute resolution) genuine but extraction significant (violence enforcement, loyalty demands). Arbitrage exit reflects ability to navigate between systems, not freedom from constraint.</rationale>
  </agent>

  <agent ref="G₁" subagent="identity_locked_member">
    <index>
      <power>moderate</power>           <!-- π = 1.0 -->
      <time>biographical</time>
      <exit>identity_locked</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.85 × 1.0 × 0.9 = 0.765</chi>
    <type>Snare</type>
    <threshold_check>χ = 0.765 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>Baseline agency with identity fusion to subcultural system. High suppression (violence enforcement) dominates coordination value. Identity-locked exit reflects cognitive capture, not structural immobility.</rationale>
  </agent>

  <agent ref="X₁" context="captured">
    <index>
      <power>powerless</power>          <!-- π = 1.5 -->
      <time>biographical</time>
      <exit>trapped</exit>
      <scope>regional</scope>           <!-- σ = 0.9 -->
    </index>
    <chi>ε × π × σ = 0.85 × 1.5 × 0.9 = 1.1475</chi>
    <type>Snare</type>
    <threshold_check>χ = 1.1475 > 0.70 (snare_chi_threshold) → PASS</threshold_check>
    <rationale>Zero structural power, physically trapped. Extraction amplified by powerless position. No coordination value from this index — pure coercion.</rationale>
  </agent>

  <indexical_variance>
    C₃: X₆ χ=0.459 → Tangled Rope, G₁ χ=0.765 → Snare, X₁ χ=1.1475 → Snare
    
    Same subcultural governance structure produces three distinct experiences. Leadership position (X₆) experiences hybrid coordination-extraction. Identity-locked members (G₁) experience extraction trap with residual coordination. Powerless outsider (X₁) experiences pure coercion. Divergence driven by power position and identity fusion, not by different rule systems.
  </indexical_variance>

  <boltzmann_test>FAIL — Classification varies by power position. X₆ (powerful) experiences Tangled Rope, G₁ and X₁ (moderate/powerless) experience Snare. High ε + high Supp + power-dependent classification indicates constructed governance system enforced through violence, not natural emergence. Coordination value exists but is dominated by extraction for non-leadership positions.</boltzmann_test>
</constraint>
```

---

## Transformation Rules

### TR₁: Collective Organization Against Asymmetric Venture

```xml
<transformation_rule id="TR₁">
  <trigger>
    <condition>organize_collective_action</condition>
    <target>C₁</target>
    <agent>G₁</agent>
  </trigger>
  
  <index_change>
    <from>P = moderate (π=1.0), E = constrained</from>
    <to>P = organized (π=0.4), E = constrained</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.85 × 1.0 × 0.9 = 0.765 → Snare</before>
    <after>χ = 0.85 × 0.4 × 0.9 = 0.306 → Rope</after>
  </chi_recalculation>
  
  <type_change>Snare → Rope</type_change>
  
  <preconditions>
    <condition>Coordination channels available</condition>
    <condition>Collective action not suppressed by C₃</condition>
    <condition>Alternative governance structure feasible</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₃ (Articles of Fortune) — subcultural loyalty prevents cross-group organization</constraint>
    <constraint>C₂ (Gentlemanly Trust) — class-based trust excludes labor from negotiation</constraint>
  </blocked_by>
  
  <rationale>Collective organization reduces power asymmetry by enabling burden-sharing and collective bargaining. π shifts from moderate (1.0) to organized (0.4), reducing χ below rope_chi_ceiling. However, C₃ blocks this transformation by enforcing subcultural loyalty through violence, and C₂ blocks it by excluding labor from trust-based negotiation channels.</rationale>
</transformation_rule>
```

---

### TR₂: Trust Exploitation Enabling Infiltration

```xml
<transformation_rule id="TR₂">
  <trigger>
    <condition>exploit_trust_mechanism</condition>
    <target>C₂</target>
    <agent>X₆</agent>
  </trigger>
  
  <index_change>
    <from>P = powerful (π=0.6), E = arbitrage (outside trust boundary)</from>
    <to>P = powerful (π=0.6), E = arbitrage (inside trust boundary)</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.10 × 0.6 × 0.9 = 0.054 → Rope (as tool)</before>
    <after>χ = 0.10 × 0.6 × 0.9 = 0.054 → Rope (as vulnerability exploited)</after>
  </chi_recalculation>
  
  <type_change>No type change — functional transformation only</type_change>
  
  <preconditions>
    <condition>Trust mechanism applies behavioral heuristics (appearance-based classification)</condition>
    <condition>Agent can perform trust signals without internal commitment</condition>
    <condition>Verification costs exceed trust benefits within class boundary</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>None — C₂'s low Supp (0.20) indicates minimal enforcement, enabling exploitation</constraint>
  </blocked_by>
  
  <rationale>Trust mechanism designed for intra-class coordination becomes vulnerability when applied across class boundaries. X₆'s arbitrage position enables strategic performance of trust signals without constraint burden. χ remains constant (low extraction for exploiter), but functional meaning inverts: coordination mechanism becomes infiltration vector. This transformation enables C₁ (venture) to proceed with asymmetric information distribution.</rationale>
</transformation_rule>
```

---

### TR₃: Subcultural Governance Suppressing Alternative Organization

```xml
<transformation_rule id="TR₃">
  <trigger>
    <condition>enforce_subcultural_loyalty</condition>
    <target>C₃</target>
    <agent>X₆</agent>
  </trigger>
  
  <index_change>
    <from>G₁ member: P = moderate (π=1.0), E = constrained</from>
    <to>G₁ member: P = moderate (π=1.0), E = identity_locked</to>
  </index_change>
  
  <chi_recalculation>
    <before>χ = 0.85 × 1.0 × 0.9 = 0.765 → Snare</before>
    <after>χ = 0.85 × 1.0 × 0.9 = 0.765 → Snare (exit option eliminated)</after>
  </chi_recalculation>
  
  <type_change>No type change — exit constraint intensifies</type_change>
  
  <preconditions>
    <condition>High suppression (Supp = 0.90) enables violence enforcement</condition>
    <condition>Alternative organization (TR₁) attempted or threatened</condition>
    <condition>Subcultural identity provides cognitive framework</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>None — C₃'s high Supp (0.90) and high ε (0.85) enable this enforcement</constraint>
  </blocked_by>
  
  <rationale>Subcultural governance structure prevents collective organization against C₁ by enforcing loyalty through violence and identity fusion. Exit option shifts from constrained (high-cost departure possible) to identity_locked (cognitive fusion prevents consideration of alternatives). χ remains constant but exit constraint intensifies, blocking TR₁. This transformation maintains C₁'s asymmetric structure by preventing labor coordination.</rationale>
</transformation_rule>
```

---

### TR₄: Venture Completion Eliminating Coordination Value

```xml
<transformation_rule id="TR₄">
  <trigger>
    <condition>complete_venture_objective</condition>
    <target>C₁</target>
    <agent>X₅</agent>
  </trigger>
  
  <index_change>
    <from>All agents: venture active, coordination required</from>
    <to>All agents: venture complete, coordination obsolete</to>
  </index_change>
  
  <chi_recalculation>
    <X₅>
      <before>χ = 0.80 × (-0.2) × 0.9 = -0.144 → Rope</before>
      <after>χ = 0.80 × (-0.2) × 0.9 = -0.144 → Rope (wealth extracted, constraint dissolved)</after>
    </X₅>
    <G₁>
      <before>χ = 0.80 × 1.0 × 0.9 = 0.72 → Snare</before>
      <after>χ = 0.80 × 1.0 × 0.9 = 0.72 → Snare (labor expended, minimal reward)</after>
    </G₁>
  </chi_recalculation>
  
  <type_change>No type change — constraint dissolves with asymmetric outcome distribution</type_change>
  
  <preconditions>
    <condition>Venture objective achieved (treasure recovered)</condition>
    <condition>C₃ (subcultural governance) suppressed or eliminated</condition>
    <condition>Survival of capital-owning agents</condition>
  </preconditions>
  
  <blocked_by>
    <constraint>C₃ (Articles of Fortune) — if subcultural governance succeeds, outcome distribution inverts</constraint>
  </blocked_by>
  
  <rationale>Venture completion dissolves C₁ but preserves asymmetric outcome distribution. X₅ (institutional position) extracts wealth, G₁ (labor) receives minimal compensation. χ values remain constant through completion, reflecting that extraction was structural property of venture design, not emergent outcome. This transformation represents natural termination of time-bounded constraint with power-dependent benefit distribution.</rationale>
</transformation_rule>
```

---

## Error Manifestations

### Error E₁: Type III — Snare-as-Rope (Missing Extraction)

```xml
<error id="E₁">
  <type>Type III: Snare-as-Rope</type>
  <agent>X₇</agent>
  <constraint>C₁</constraint>
  <actual_type>Tangled Rope (χ=0.432 from powerful index)</actual_type>
  <perceived_type>Rope (coordination value emphasized, extraction minimized)</perceived_type>
  <observable>Enforces formal hierarchy without recognizing asymmetric risk distribution. Does not negotiate for risk-sharing mechanisms despite structural position enabling such negotiation. Treats venture as pure coordination problem.</observable>
  <correction_trigger>Recognition that χ(G₁) = 0.72 (Snare) while χ(X₇) = 0.432 (Tangled Rope) indicates extraction asymmetry, not coordination symmetry. Observing labor's constrained exit options versus own powerful position would reveal structural extraction.</correction_trigger>
  <rationale>Professional authority position (π=0.6) dampens extraction experience, leading to normalization. X₇ experiences genuine coordination value (command structure) but fails to detect that same structure is Snare for labor. Error driven by index-dependent perception: powerful position makes extraction invisible.</rationale>
</error>
```

---

### Error E₂: Type I — False Mountain

```xml
<error id="E₂">
  <type>Type I: False Mountain</type>
  <agent>G₁</agent>
  <constraint>C₃</constraint>
  <actual_type>Snare (χ=0.765 from moderate/identity_locked index)</actual_type>
  <perceived_type>Mountain (subcultural identity treated as unchangeable nature)</perceived_type>
  <observable>Does not attempt alternative organization despite χ indicating feasibility of collective action (TR₁). Treats subcultural loyalty as immutable identity rather than enforced constraint. Accepts violence enforcement as natural rather than constructed.</observable>
  <correction_trigger>Observing that classification varies by power position (X₆ experiences Tangled Rope, G₁ experiences Snare) would reveal constructed nature. Recognition that Supp = 0.90 indicates active enforcement, not natural emergence. Boltzmann test failure demonstrates non-natural constraint.</correction_trigger>
  <rationale>Identity-locked exit option (E = identity_locked) creates cognitive fusion between self and constraint. High suppression (0.90) normalizes violence enforcement. Immediate time horizon (T = immediate from trapped position) makes constraint appear unchangeable. Error driven by combination of identity fusion and normalization of enforcement mechanisms.</rationale>
</error>
```

---

### Error E₃: Type III — Snare-as-Rope (Class-Based Normalization)

```xml
<error id="E₃">
  <type>Type III: Snare-as-Rope</type>
  <agent>X₅</agent>
  <constraint>C₁</constraint>
  <actual_type>Rope for X₅ (χ=-0.144), Snare for G₁ (χ=0.72)</actual_type>
  <perceived_type>Rope (universal coordination mechanism)</perceived_type>
  <observable>Designs venture structure without recognizing asymmetric extraction. Treats labor participation as voluntary coordination rather than constrained by economic necessity. Does not implement risk-sharing mechanisms despite structural position enabling such design.</observable>
  <correction_trigger>Calculating χ from powerless index (X₁ χ=1.08) or moderate index (G₁ χ=0.72) would reveal Snare classification. Observing exit constraints (E = trapped, constrained) versus own arbitrage position would reveal structural coercion masked as voluntary participation.</correction_trigger>
  <rationale>Institutional position (π=-0.2) inverts constraint into wealth-enabling mechanism (negative χ). This inversion makes extraction invisible from beneficiary position. Error driven by index-dependent perception: net beneficiary position prevents detection of extraction experienced by other indices.</rationale>
</error>
```

---

### Error E₄: Type V.a — Tangled-as-Rope (Ignoring Extraction Component)

```xml
<error id="E₄">
  <type>Type V.a: Tangled-as-Rope</type>
  <agent>X₆</agent>
  <constraint>C₃</constraint>
  <actual_type>Tangled Rope (χ=0.459 from powerful/arbitrage index)</actual_type>
  <perceived_type>Rope (coordination mechanism for collective action)</perceived_type>
  <observable>Emphasizes collective governance and dispute resolution functions. Minimizes violence enforcement and loyalty extraction. Treats subcultural structure as pure coordination without recognizing asymmetric cost distribution (leadership versus membership burden).</observable>
  <correction_trigger>Calculating χ for identity_locked members (G₁ χ=0.765 → Snare) would reveal extraction component. Observing that Supp = 0.90 indicates high enforcement costs. Recognition that own arbitrage position (E = arbitrage) enables exit while members are identity_locked would reveal asymmetry.</correction_trigger>
  <rationale>Leadership position within hybrid structure experiences coordination value genuinely but extraction is dampened by powerful position (π=0.6). Arbitrage exit option enables strategic navigation, making constraint feel like tool rather than burden. Error driven by position-dependent experience of hybrid constraint: leader experiences coordination, members experience extraction.</rationale>
</error>
```

---

## Institutional Rationality Model

### Model Selection: Bounded Institutional Rationality (BIR)

**Justification:**

1. **Principal-agent problems present:** X₅ (capital owner) cannot perfectly monitor X₆ (hired agent). Information asymmetry enables TR₂ (trust exploitation). Perfect monitoring would prevent infiltration, but verification costs exceed trust benefits within class boundary (C₂ design assumption).

2. **Risk aversion evident:** X₇ (professional authority) prioritizes formal hierarchy and established procedures over optimal outcomes. Satisficing behavior (maintaining command structure) rather than utility maximization (negotiating risk-sharing).

3. **Uncertainty and incomplete information:** G₂ (gentry party) operates under uncertainty about G₁'s (mutineers') intentions. C₂ (trust mechanism) functions as heuristic under uncertainty, not perfect information processor.

4. **Organizational constraints:** C₁ (venture structure) reflects realistic organizational design with transaction costs, not frictionless optimization. Asymmetric risk distribution emerges from bounded rationality in contract design, not perfect extraction maximization.

**Implications for attractor selection:**

- **Deterministic Tragedy:** Incompatible. Would require perfect institutional optimization driving constraints to completion. BIR introduces negotiation space and suboptimal outcomes.

- **Negotiated Equilibrium:** Compatible. Principal-agent problems and information asymmetry create bargaining space. C₃ (subcultural governance) represents alternative equilibrium, not pure opposition.

- **Revolutionary Rupture:** Possible but requires external shock. BIR institutions resist rupture through satisficing and risk aversion, but incomplete information can enable sudden regime shifts.

- **Seeded Possibility:** Compatible. Bounded rationality creates gaps where alternative structures can develop. X₈ (marooned agent) represents possibility space outside main constraint network.

**Selected model:** BIR with emphasis on information asymmetry and principal-agent dynamics.

---

## Terminal Attractor Selection

### Selected Attractor: Negotiated Equilibrium

**Structural justification:**

1. **Constraint profile:** C₁ (Snare for labor, Rope for capital) + C₃ (Tangled Rope for leadership, Snare for members) creates two competing governance structures, not single dominant constraint. Mountain-dominated profile absent.

2. **Rationality model compatibility:** BIR enables negotiation through information asymmetry and satisficing. Perfect rationality would drive to Deterministic Tragedy, but bounded rationality creates bargaining space.

3. **Transformation rule dynamics:** TR₁ (collective organization) blocked by TR₃ (subcultural enforcement), creating stable tension rather than runaway extraction. TR₂ (trust exploitation) enables infiltration but not total domination. TR₄ (venture completion) dissolves C₁ but preserves power structure for future iterations.

4. **Indexical variance pattern:** Multiple stable classifications across indices (X₅ Rope, X₇ Tangled, G₁ Snare) indicate equilibrium rather than collapse. If all agents experienced Snare, trajectory would be Tragedy. If all experienced Rope, no conflict. Mixed profile suggests negotiated balance.

5. **Error manifestations:** E₁-E₄ show normalization and misclassification, not recognition and resistance. Errors stabilize system by preventing coordination (E₂ False Mountain blocks TR₁) and masking extraction (E₃ Snare-as-Rope prevents reform demands).

**Alternative attractors rejected:**

- **Deterministic Tragedy:** Requires Mountain-dominated profile or pure Snare cascade. C₁ and C₃ are high-extraction but not Mountains (Boltzmann failures). BIR prevents perfect extraction optimization.

- **Revolutionary Rupture:** Requires constraint logic disruption. TR₁ (collective organization) is blocked, not enabled. No transformation rule breaks constraint network itself. X₈ (marooned agent) exists outside network but doesn't disrupt it.

- **Seeded Possibility:** Requires surface tragedy with underground transformation. X₈ represents possibility space, but narrative resolution is explicit negotiated outcome (venture completion with power structure preserved), not hidden transformation. Surface and depth align.

**Equilibrium characteristics:**

- **Stable power asymmetry:** X₅ (institutional) maintains Rope classification, G₁ (moderate) maintains Snare classification. No transformation rules equalize χ values.

- **Competing governance structures:** C₁ (formal hierarchy) and C₃ (subcultural governance) reach stalemate, not synthesis. TR₄ (venture completion) dissolves immediate conflict but preserves structural conditions for recurrence.

- **Information asymmetry preserved:** TR₂ (trust exploitation) succeeds, indicating verification costs remain prohibitive. C₂ (trust mechanism) persists as systemic vulnerability.

- **Bounded optimization:** Outcome is satisficing (venture completed, losses acceptable) rather than maximizing (perfect extraction or perfect resistance).

**Testable prediction:** Future iterations will reproduce C₁ structure with similar indexical variance. No transformation rule permanently alters power distribution