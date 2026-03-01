# STAGE 1: FORMAL SPECIFICATION
## The Miller and His Ass: Operational Constraint Model

---

## I. CONSTRAINT FORMALIZATIONS

### C1: Market_Imperative

**Base Properties:**
```
ε(C1) = 0.45
Supp(C1) = 0.10
Coordination = TRUE
Asymmetric = TRUE
```

**Indexed Classification:**

**Miller (protagonist):**
```
Index: I_M = (moderate, biographical, constrained, local)
  π(moderate) = 1.0
  σ(local) = 0.8

χ(C1, I_M) = ε(C1) × π(moderate) × σ(local)
            = 0.45 × 1.0 × 0.8
            = 0.36

Classification: Tangled Rope (⊞⊠)
  Rationale: 0.30 ≤ χ < 0.46 (per logic_thresholds.md)
  Coordination present (market enables exchange)
  Moderate extraction (effort required, price uncertainty)
```

**Experience Formalization:**
```xml
<constraint_experience id="C1_Miller">
  <phenomenology>
    Must invest labor (grooming, transport) to access market
    Outcome uncertain (price depends on buyer assessment)
    Alternative: keep ass, lose exchange value
  </phenomenology>
  <action_space>
    - Prepare property for sale (coordination with market norms)
    - Accept price uncertainty (extraction via information asymmetry)
    - Navigate to town (coordination cost)
  </action_space>
  <error_potential>Type I (False Mountain) if believes market rules unchangeable</error_potential>
</constraint_experience>
```

---

### C2: Public_Performance_Demand

**Base Properties:**
```
ε(C2) = 0.80
Supp(C2) = 0.20
Coordination = FALSE
Asymmetric = TRUE
```

**Indexed Classifications:**

**Miller (victim):**
```
Index: I_M = (powerless, immediate, trapped, local)
  π(powerless) = 1.5
  σ(local) = 0.8

χ(C2, I_M) = ε(C2) × π(powerless) × σ(local)
            = 0.80 × 1.5 × 0.8
            = 0.96

Classification: Snare (⊠)
  Rationale: χ ≥ 0.70 (per logic_thresholds.md)
  High extraction (constant judgment, contradictory demands)
  No coordination value (observers gain nothing material)
  High suppression (social enforcement, no escape)
  Trapped exit (public road, cannot avoid observers)
```

**Observer (enforcer):**
```
Index: I_O = (powerful, immediate, arbitrage, local)
  π(powerful) = 0.6
  σ(local) = 0.8

χ(C2, I_O) = ε(C2) × π(powerful) × σ(local)
            = 0.80 × 0.6 × 0.8
            = 0.384

Classification: Rope (⊞)
  Rationale: χ < 0.30 (per logic_thresholds.md)
  Zero cost to enforce (verbal judgment only)
  Perceived coordination (maintaining social norms)
  Arbitrage exit (can choose to speak or not)
```

**Indexical Variance Analysis:**
```
Δχ = χ(Miller) - χ(Observer) = 0.96 - 0.384 = 0.576

Interpretation:
  - Same constraint experienced as Snare vs Rope
  - 150% divergence in effective extraction
  - Miller bears full cost, Observer bears none
  - Both classifications objectively true from respective indices
  - This variance IS the narrative engine
```

**Experience Formalizations:**

```xml
<constraint_experience id="C2_Miller">
  <phenomenology>
    Every action subject to immediate public judgment
    Judgments contradictory (ride = lazy, walk = cruel, carry = absurd)
    No escape (public road, continuous observation)
    Compliance feels mandatory (social pressure overwhelming)
  </phenomenology>
  <action_space>
    - Obey most recent judgment (extraction: abandon prior strategy)
    - Attempt to satisfy all judgments (extraction: impossible task)
    - Ignore judgments (blocked: social pressure too high)
  </action_space>
  <error_type>Type III (Snare-as-Rope) - initially treats social judgment as legitimate coordination</error_type>
</constraint_experience>

<constraint_experience id="C2_Observer">
  <phenomenology>
    Duty to correct improper behavior
    Zero cost to speak
    Immediate satisfaction from enforcement
    No responsibility for consequences
  </phenomenology>
  <action_space>
    - Voice judgment (coordination: maintain norms)
    - Withhold judgment (arbitrage: choose engagement)
  </action_space>
  <error_type>Type III (Snare-as-Rope) - genuinely believes enforcement is coordination</error_type>
</constraint_experience>
```

---

### C3: Material_Limits

**Base Properties:**
```
ε(C3) = 0.05
Supp(C3) = 0.00
Coordination = FALSE
Asymmetric = FALSE
```

**Indexed Classifications:**

**Ass (subject):**
```
Index: I_A = (powerless, immediate, trapped, local)
  π(powerless) = 1.5
  σ(local) = 0.8

χ(C3, I_A) = ε(C3) × π(powerless) × σ(local)
            = 0.05 × 1.5 × 0.8
            = 0.06

Classification: Mountain (■)
  Rationale: χ < 0.10, ε < 0.10 (per logic_thresholds.md)
  Natural law (animal panic response)
  No suppression needed (emerges from biology)
  Unchangeable from any index
```

**Miller (subject):**
```
Index: I_M = (powerless, immediate, trapped, local)
  π(powerless) = 1.5
  σ(local) = 0.8

χ(C3, I_M) = ε(C3) × π(powerless) × σ(local)
            = 0.05 × 1.5 × 0.8
            = 0.06

Classification: Mountain (■)
  Rationale: χ < 0.10, ε < 0.10
  Natural law (gravity, water, physical limits)
  No suppression needed (physics)
  Unchangeable from any index
```

**Power-Scaling Verification:**
```
Test: Does classification vary by power position?

Institutional index: I_I = (institutional, generational, mobile, global)
  χ(C3, I_I) = 0.05 × -0.2 × 1.2 = -0.012

Still Mountain (natural laws don't extract from anyone)
Classification invariant across power → TRUE MOUNTAIN confirmed
```

**Experience Formalizations:**

```xml
<constraint_experience id="C3_Ass">
  <phenomenology>
    Bound upside-down over noisy bridge
    Panic response (biological imperative)
    Struggle to escape (instinct, not choice)
  </phenomenology>
  <action_space>
    - Struggle (only available action)
    - Fall into river (consequence of struggle + physics)
  </action_space>
  <error_type>None (animals don't misclassify natural law)</error_type>
</constraint_experience>

<constraint_experience id="C3_Miller">
  <phenomenology>
    Cannot prevent ass from panicking
    Cannot prevent fall once struggle begins
    Cannot retrieve ass from river
  </phenomenology>
  <action_space>
    - Accept loss (only rational response to Mountain)
  </action_space>
  <error_type>Type II (Mountain Denial) if attempts to fight physics</error_type>
</constraint_experience>
```

---

## II. TRANSFORMATION RULES

### TR1: Initial Compliance (C2 activation)

```xml
<transformation_rule id="TR1">
  <trigger>
    <condition>Miller encounters first Observer judgment</condition>
    <state>C1 active (traveling to market), C2 dormant</state>
  </trigger>
  
  <index_change>
    <character>Miller</character>
    <from>
      <index>(moderate, biographical, constrained, local)</index>
      <active_constraints>[C1]</active_constraints>
    </from>
    <to>
      <index>(powerless, immediate, trapped, local)</index>
      <active_constraints>[C1, C2]</active_constraints>
    </to>
  </index_change>
  
  <chi_recalculation>
    <C1>
      <before>0.36 (Tangled Rope)</before>
      <after>0.54 (Tangled Rope, heavier)</after>
      <mechanism>π: moderate→powerless (1.0→1.5)</mechanism>
    </C1>
    <C2>
      <before>dormant</before>
      <after>0.96 (Snare)</after>
      <mechanism>Activated by social encounter</mechanism>
    </C2>
  </chi_recalculation>
  
  <consequence>
    <narrative>Miller obeys first judgment, abandons initial strategy</narrative>
    <structural>C2 now dominates decision-making, C1 goal recedes</structural>
    <error>Type III (treats Snare as Rope) - believes compliance is coordination</error>
  </consequence>
</transformation_rule>
```

---

### TR2: Escalating Contradiction (C2 intensification)

```xml
<transformation_rule id="TR2">
  <trigger>
    <condition>Miller encounters contradictory Observer judgment</condition>
    <state>C2 active, Miller has already complied once</state>
  </trigger>
  
  <index_change>
    <character>Miller</character>
    <from>
      <index>(powerless, immediate, trapped, local)</index>
      <time_horizon>immediate (can still imagine market arrival)</time_horizon>
    </from>
    <to>
      <index>(powerless, immediate, trapped, local)</index>
      <time_horizon>immediate (only current judgment matters)</time_horizon>
    </to>
  </index_change>
  
  <chi_recalculation>
    <C2>
      <before>0.96 (Snare)</before>
      <after>0.96 (Snare, intensified)</after>
      <mechanism>Contradiction increases suppression cost</mechanism>
    </C2>
    <C1>
      <before>0.54 (Tangled Rope)</before>
      <after>0.54 (Tangled Rope, increasingly irrelevant)</after>
      <mechanism>Market goal fades as C2 dominates attention</mechanism>
    </C1>
  </chi_recalculation>
  
  <consequence>
    <narrative>Miller obeys new judgment, contradicting previous action</narrative>
    <structural>C1 goal now unreachable (time/energy depleted by C2)</structural>
    <error>Type III persists (still treats judgments as legitimate)</error>
  </consequence>
  
  <iteration>
    <condition>Repeats with each new Observer encounter</condition>
    <effect>C2 extraction accumulates, C1 becomes impossible</effect>
  </iteration>
</transformation_rule>
```

---

### TR3: Absurd Compliance (C2 → C3 collision)

```xml
<transformation_rule id="TR3">
  <trigger>
    <condition>Miller attempts to satisfy contradictory demands simultaneously</condition>
    <state>C2 has driven Miller to carry ass on bridge</state>
  </trigger>
  
  <index_change>
    <character>Miller</character>
    <from>
      <index>(powerless, immediate, trapped, local)</index>
      <active_constraints>[C1 (dormant), C2 (dominant)]</active_constraints>
    </from>
    <to>
      <index>(powerless, immediate, trapped, local)</index>
      <active_constraints>[C2 (dominant), C3 (activated)]</active_constraints>
    </to>
  </index_change>
  
  <chi_recalculation>
    <C2>
      <before>0.96 (Snare)</before>
      <after>0.96 (Snare, terminal)</after>
      <mechanism>Final compliance attempt</mechanism>
    </C2>
    <C3>
      <before>dormant</before>
      <after>0.06 (Mountain)</after>
      <mechanism>Physical limits activated by absurd action</mechanism>
    </C3>
  </chi_recalculation>
  
  <consequence>
    <narrative>Ass panics on bridge, struggles, falls into river, drowns</narrative>
    <structural>C3 (Mountain) terminates C2 (Snare) by destroying its subject</structural>
    <error>Type II (Mountain Denial) - Miller treated physical limits as negotiable</error>
    <terminal>TRUE (C1 goal now impossible, C2 enforcement ends, C3 irreversible)</terminal>
  </consequence>
</transformation_rule>
```

---

### TR4: Recognition (Error correction, post-terminal)

```xml
<transformation_rule id="TR4">
  <trigger>
    <condition>Miller reflects after ass drowns</condition>
    <state>C3 has terminated the system</state>
  </trigger>
  
  <index_change>
    <character>Miller</character>
    <from>
      <index>(powerless, immediate, trapped, local)</index>
      <error>Type III (Snare-as-Rope)</error>
    </from>
    <to>
      <index>(analytical, biographical, mobile, local)</index>
      <error>Corrected (recognizes C2 as Snare)</error>
    </to>
  </index_change>
  
  <chi_recalculation>
    <C2>
      <before>0.96 (Snare, experienced)</before>
      <after>0.92 (Snare, recognized)</after>
      <mechanism>π: powerless→analytical (1.5→1.15), retrospective classification</mechanism>
    </C2>
  </chi_recalculation>
  
  <consequence>
    <narrative>"In trying to please everyone, I pleased no one and lost everything"</narrative>
    <structural>Error correction comes too late (C3 irreversible)</structural>
    <moral>Recognition without power to act = tragedy</moral>
  </consequence>
  
  <note>This transformation is post-terminal. It changes Miller's understanding but cannot reverse C3 (Mountain). Classic tragic structure: wisdom arrives after the point of no return.</note>
</transformation_rule>
```

---

## III. ERROR MANIFESTATIONS

### E1: Type III Error (Snare-as-Rope) - Miller

**Observable Actions:**
```
1. Immediate compliance with first judgment
   - Does not question legitimacy
   - Does not assess cost
   - Treats as coordination (maintaining social norms)

2. Continued compliance despite contradiction
   - Does not recognize pattern (judgments contradict)
   - Does not calculate cumulative cost
   - Each judgment treated as isolated coordination request

3. Escalating absurdity
   - Carries ass on bridge (physically dangerous)
   - Does not recognize impossibility of satisfying all demands
   - Treats contradictory demands as simultaneously valid
```

**Structural Signature:**
```
Error Type: III (Snare-as-Rope)
Mechanism: Normalizes extraction as coordination
Index Cause: (powerless, immediate, trapped, local)
  - Powerless: Cannot challenge enforcers
  - Immediate: No time to analyze pattern
  - Trapped: Cannot exit public road
  - Local: Each judgment feels personal, legitimate

Correction Trigger: C3 (Mountain) forces recognition
Correction Timing: Post-terminal (too late)
```

---

### E2: Type III Error (Snare-as-Rope) - Observers

**Observable Actions:**
```
1. Unsolicited judgment delivery
   - Believes judgment is helpful (coordination)
   - Does not calculate cost to Miller
   - Does not coordinate with other observers

2. No responsibility for consequences
   - Does not track Miller's compliance
   - Does not observe contradiction with other judgments
   - Walks away after enforcement

3. Genuine belief in legitimacy
   - Experiences zero cost (χ = 0.384, Rope)
   - Perceives social benefit (norm maintenance)
   - No awareness of extraction
```

**Structural Signature:**
```
Error Type: III (Snare-as-Rope)
Mechanism: Genuinely experiences Rope, unaware others experience Snare
Index Cause: (powerful, immediate, arbitrage, local)
  - Powerful: Can enforce without cost
  - Immediate: No long-term consequence tracking
  - Arbitrage: Can choose to engage or not
  - Local: Sees individual interaction, not systemic pattern

Correction Trigger: None in narrative (observers never learn)
Correction Timing: N/A (error persists)
```

**Critical Insight:**
```
Both Miller and Observers make Type III errors, but from opposite directions:
  - Miller: Experiences Snare, misclassifies as Rope (normalizes own oppression)
  - Observers: Experience Rope, unaware others experience Snare (normalized oppression of others)

This is NOT a disagreement about facts. Both classifications are objectively true from their respective indices. The error is in failing to recognize indexical variance.
```

---

### E3: Type II Error (Mountain Denial) - Miller

**Observable Action:**
```
Attempts to carry ass across bridge despite:
  - Physical danger (ass bound upside-down)
  - Biological limits (animal panic response)
  - Environmental hazard (noisy bridge, river below)
```

**Structural Signature:**
```
Error Type: II (Mountain Denial)
Mechanism: Treats unchangeable constraint (C3) as negotiable
Index Cause: C2 (Snare) has overridden rational assessment
  - Immediate time horizon: Only current judgment matters
  - Trapped exit: Must comply or face social consequences
  - Powerless: Cannot challenge the demand

Consequence: C3 (Mountain) asserts itself through physical law
  - Ass panics (biological imperative)
  - Ass falls (gravity)
  - Ass drowns (physical limits)

Correction: Forced by C3 (cannot deny Mountain once it acts)
```

---

## IV. INSTITUTIONAL RATIONALITY MODEL

### Selected Model: Bounded Institutional Rationality (BIR)

**Justification:**
```
The constraint system involves human social institutions (public judgment, market norms) rather than implacable algorithmic systems. Observers are not perfectly rational optimizers—they:

1. Satisfice under uncertainty
   - Don't calculate optimal enforcement strategy
   - Use heuristic (speak up when something seems wrong)
   - Don't coordinate with other observers

2. Exhibit principal-agent problems
   - Individual observer gains satisfaction (principal)
   - Miller bears cost (agent)
   - No mechanism to align incentives

3. Show risk aversion
   - Zero-cost enforcement preferred
   - No skin in the game
   - Avoid responsibility for consequences

4. Operate under bounded rationality
   - Don't track systemic effects
   - Don't recognize contradiction pattern
   - Local optimization only
```

**Implications for Attractor Selection:**
```
BIR makes the following attractors reachable:
  ✓ Deterministic Tragedy (constraints run to completion)
  ✓ Negotiated Equilibrium (if Miller had power to bargain)
  ✓ Seeded Possibility (if underground resistance existed)
  ✗ Revolutionary Rupture (requires coordinated action, not present)

BIR makes the following attractors unreachable:
  ✗ Perfect optimization (observers don't maximize utility globally)
  ✗ Algorithmic inevitability (human judgment is negotiable in principle)
```

**Contrast with PIR:**
```
If we used Perfect Institutional Rationality (PIR):
  - Observers would coordinate judgments (eliminate contradiction)
  - Observers would calculate optimal extraction (maximize Miller's compliance)
  - System would be implacable (no negotiation possible)
  - Would route toward Deterministic Tragedy only

BIR is more realistic:
  - Observers don't coordinate (contradiction emerges naturally)
  - Observers don't optimize (heuristic enforcement)
  - System is negotiable in principle (Miller could resist if organized)
  - Multiple attractors possible (tragedy is contingent, not inevitable)
```

---

## V. TERMINAL ATTRACTOR SELECTION

### Selected Attractor: Deterministic Tragedy

**Formal Specification:**
```
Terminal State: C3 (Mountain) terminates system
  - C1 (Market_Imperative) goal unreached
  - C2 (Public_Performance_Demand) enforcement ends (no subject)
  - C3 (Material_Limits) irreversible (ass drowned)

Attractor Type: Deterministic Tragedy
  - Constraints run to completion
  - No negotiation occurs
  - No structural transformation
  - Terminal state is absorbing (no exit)
```

**Justification:**

**1. Compatibility with Rationality Model (BIR):**
```
BIR allows Deterministic Tragedy when:
  - No collective action emerges (Miller isolated)
  - No bargaining power exists (Miller powerless)
  - Bounded rationality prevents correction (observers don't learn)
  - Satisficing behavior persists (observers continue enforcement)

All conditions met in narrative.
```

**2. Compatibility with Dominant Constraint Type:**
```
C2 (Public_Performance_Demand) is dominant:
  - Type: Snare (from Miller's index)
  - High extraction (χ = 0.96)
  - High suppression (Supp = 0.20)
  - No coordination value
  - Asymmetric (observers experience as Rope)

Snare + No collective action + No exit = Deterministic Tragedy
```

**3. Structural Path:**
```
C1 (Tangled Rope) → C2 (Snare) → C3 (Mountain)
  ↓                    ↓              ↓
Goal established → Goal blocked → Goal destroyed

Trajectory:
  t0: Miller has moderate power, constrained exit, biographical time
  t1: C2 activates, Miller becomes powerless, trapped, immediate time
  t2: C2 intensifies through contradiction, Miller's agency depletes
  t3: C2 drives absurd compliance, C3 activates
  t4: C3 terminates system irreversibly

No intervention point where:
  - Miller gains power (remains powerless)
  - Miller gains exit (remains trapped)
  - Miller gains time (remains immediate)
  - Observers correct error (remain unaware)
```

**4. Alternative Attractors Ruled Out:**

**Negotiated Equilibrium:**
```
Requires: Bargaining power, mutual recognition, iterative adjustment
Blocked by: Miller powerless, observers unaware of extraction, no iteration
Conclusion: Unreachable from initial conditions
```

**Revolutionary Rupture:**
```
Requires: Collective action, constraint logic disruption, structural transformation
Blocked by: Miller isolated, no organization, observers dispersed
Conclusion: Unreachable from initial conditions
```

**Seeded Possibility:**
```
Requires: Surface tragedy + underground transformation
Blocked by: No underground network, no collective memory, no future organizing
Conclusion: Unreachable from initial conditions
Note: Could be reached if narrative included other victims organizing, but text does not support this
```

---

### Attractor Mechanics

**Absorption Conditions:**
```
State is terminal when:
  1. C3 (Mountain) has acted (ass drowned)
  2. C1 goal is impossible (no property to sell)
  3. C2 enforcement ends (no subject to judge)
  4. No reversibility (cannot un-drown ass)

Formal: ∀t > t_terminal, State(t) = State(t_terminal)
```

**Trajectory Visualization:**
```
Agency Space:

High │        C1 (Tangled Rope)
     │         ╱
     │        ╱
     │       ╱ TR1
     │      ╱
     │     ╱
     │    ╱
     │   ╱ C2 (Snare)
     │  ╱
     │ ╱ TR2 (iteration)
     │╱
Low  ├──────────────────────────────> Time
     │                            ╲
     │                             ╲ TR3
     │                              ╲
     │                               ╲
     │                                ╲ C3 (Mountain)
     │                                 ●  [TERMINAL]
     │
     └─ Deterministic Tragedy attractor
```

---

## VI. STRUCTURAL PHYSICS ARCS (OPTIONAL)

### Arc 1: False Mountain Reveal (Implicit)

**Structure:**
```
Stage 1: C2 presented as natural social order
  - Observers treat judgment as duty (Mountain-like)
  - Miller initially accepts as legitimate (Mountain-like)
  - No visible enforcement mechanism (appears natural)

Stage 2: Evidence accumulates
  - Judgments contradict (natural laws don't contradict)
  - Burden asymmetric (natural laws don't discriminate by power)
  - Requires active enforcement (natural laws don't need enforcers)

Stage 3: Reveal (post-terminal)
  - Miller recognizes C2 as Snare (TR4)
  - Observers never recognize (error persists)
  - Reader recognizes through structural analysis

Boltzmann Test:
  Does C2 couple independent dimensions?
    - Power: Yes (powerless experience Snare, powerful experience Rope)
    - Scope: Yes (local enforcement, would fail at global scale)
  Conclusion: C2 is constructed constraint, not natural law
  
  False Mountain confirmed.
```

**Narrative Function:**
```
The story doesn't explicitly reveal C2 as False Mountain, but the structure invites this reading:
  - Social judgment appears natural, inevitable
  - Contradiction reveals construction
  - Tragedy demonstrates it's not natural law (natural law wouldn't require absurd compliance)

Reader performs Boltzmann test implicitly:
  "If this were natural law, it wouldn't contradict itself"
  "If this were natural law, it wouldn't require enforcement"
  "If this were natural law, everyone would experience it the same way"
```

---

### Arc 2: Purity Drift (Not Present)

**Analysis:**
```
Purity drift requires:
  - Constraint with initial coordination value
  - Gradual degradation over time
  - Surface metrics stable while structural health decays

C2 (Public_Performance_Demand) does not exhibit purity drift:
  - No initial coordination value (always extractive)
  - No temporal degradation (static throughout narrative)
  - No surface/depth divergence (extraction is immediate and visible)

Conclusion: Purity drift arc not applicable to this narrative.
```

---

### Arc 3: Network Contamination (Not Present)

**Analysis:**
```
Network contamination requires:
  - Focal constraint with high intrinsic purity
  - Neighboring constraints with low purity
  - Contamination propagation over time

Constraint network in this narrative:
  C1 (Market_Imperative): Moderate purity, isolated
  C2 (Public_Performance_Demand): Low purity, dominant
  C3 (Material_Limits): N/A (Mountain, no purity metric)

C1 does not contaminate C2 (no propagation path)
C2 does not contaminate C1 (C1 simply becomes irrelevant)

Conclusion: Network contamination arc not applicable to this narrative.
```

**Note on Structural Physics:**
```
This narrative is structurally simple:
  - Three constraints, linear dependency (C1 → C2 → C3)
  - No degradation over time (static system)
  - No network effects (constraints don't contaminate each other)

Structural physics arcs (False Mountain, Purity Drift, Contamination) are most relevant for:
  - Complex institutional systems
  - Long-term temporal dynamics
  - Multi-constraint networks

This fable is a clean demonstration of indexed constraint logic without requiring advanced structural physics.
```

---

## VII. VALIDATION CHECKLIST

```
☑ All Stage 0 constraints formalized
  ✓ C1: Market_Imperative (ε=0.45, Supp=0.10)
  ✓ C2: Public_Performance_Demand (ε=0.80, Supp=0.20)
  ✓ C3: Material_Limits (ε=0.05, Supp=0.00)

☑ All χ calculations shown with π and σ values
  ✓ C1_Miller: χ=0.36 (π=1.0, σ=0.8)
  ✓ C2_Miller: χ=0.96 (π=1.5, σ=0.8)
  ✓ C2_Observer: χ=0.384 (π=0.6, σ=0.8)
  ✓ C3_Ass: χ=0.06 (π=1.5, σ=0.8)
  ✓ C3_Miller: χ=0.06 (π=1.5, σ=0.8)

☑ Transformation rules are testable (IF-THEN format)
  ✓ TR1: Initial Compliance (C2 activation)
  ✓ TR2: Escalating Contradiction (C2 intensification)
  ✓ TR3: Absurd Compliance (C2 → C3 collision)
  ✓ TR4: Recognition (Error correction, post-terminal)

☑ Error types have observable manifestations
  ✓ E1: Type III (Snare-as-Rope) - Miller
  ✓ E2: Type III (Snare-as-Rope) - Observers
  ✓ E3: Type II (Mountain Denial) - Miller

☑ Institutional rationality model specified (PIR/BIR)
  ✓ Selected: Bounded Institutional Rationality (BIR)
  ✓ Justification: Human social institutions, satisficing, bounded rationality
  ✓ Implications: Multiple attractors possible, tragedy contingent not inevitable

☑ Terminal attractor selected, justified, and compatible with rationality model
  ✓ Selected: Deterministic Tragedy
  ✓ Compatible with BIR: ✓ (no collective action, no bargaining power)
  ✓ Compatible with dominant constraint (C2 Snare): ✓
  ✓ Structural path documented: ✓ (C1 → C2 → C3)
  ✓ Alternative attractors ruled out: ✓ (Negotiated, Revolutionary, Seeded)

☑ Indexical variance explicitly preserved
  ✓ C2: Snare (Miller) vs Rope (Observer) - Δχ = 0.576
  ✓ All classifications indexed to (P, T, E, S)
  ✓ Variance documented as narrative engine

☑ No ambiguity in specifications
  ✓ All parameters numerically specified
  ✓ All transformations have explicit triggers and consequences
  ✓ All error types have observable actions
  ✓ All classifications justified by threshold values
```

---

## VIII. IMPLEMENTATION NOTES

### For Computational Testing

**State Vector:**
```
S(t) = {
  Miller_Index: (P, T, E, S),
  Active_Constraints: [C1, C2, C3],
  χ_values: {C1: float, C2: float, C3: float},
  Error_State: {Type_I: bool, Type_II: bool, Type_III: bool},
  Terminal: bool
}
```

**Transition Function:**
```
S(t+1) = Transition(S(t), Event)

Where Event ∈ {
  Observer_Judgment,
  Compliance_Action,
  Physical_Consequence,
  Recognition
}
```

**Testable Predictions:**
```
1. If Miller encounters Observer_Judgment AND Error_State.Type_III = TRUE
   THEN Miller complies (TR1 or TR2)

2. If Miller complies with contradictory judgments
   