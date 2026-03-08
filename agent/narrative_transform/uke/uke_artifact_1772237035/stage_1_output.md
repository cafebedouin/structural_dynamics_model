# STAGE 1: FORMAL SPECIFICATION

**Model:** Copilot (conservative, rule-bound, integrity-first)

---

## I. CONSTRAINT FORMALIZATION

### C[1]: Athletic Performance as Social Capital

**Base Parameters:**
- ε = 0.55 (hybrid coordination-extraction)
- Supp = 0.8 (high enforcement via coaches, crowd, social rewards)
- Coord = true (coordinates eleven players toward collective goal)
- Asymmetric = true (star players capture disproportionate rewards)

**Transformation Rules:**

**TR1.1: Performance → Status**
```
IF yards_gained > threshold_star (e.g., 80-yard run)
THEN status = "campus_hero"
  AND social_capital += high
  AND romantic_access = "Louise-tier"
```

**TR1.2: Role Shift → Experience Shift**
```
IF role = "ball_carrier" 
THEN experience_type = Rope (χ ≈ 0.264)

IF role = "blocker"
THEN experience_type = Tangled_Rope (χ ≈ 0.44)
  AND satisfaction -= moderate
```

**TR1.3: Injury → System Exit**
```
IF injury = catastrophic (Diederich's broken neck)
THEN participation = terminated
  AND status = frozen_at_peak
  AND system_access = lost
```

**Error Manifestations:**

**E1.1: Type III (Snare-as-Rope) — Young Darling**
- Normalizes extraction (physical risk, subordination) as pure coordination
- Believes system rewards merit universally
- Fails to see asymmetric distribution until role changes

**E1.2: Type I (False Mountain) — Older Darling**
- Treats C[1] rules as universal, unchangeable laws
- "Everything since has been a decline" — believes peak performance = permanent truth
- Cannot adapt when system rules change (C[3] operates differently)

**Attractor:**
```
Terminal state: Memory crystallization
- The 80-yard run becomes fixed reference point
- All subsequent experience measured against this peak
- System rules internalized as identity ("I am the guy who...")
- Prevents adaptation to new constraint systems
```

**Constraint Network (Couplings):**

**Coupling 1.1: Performance ⟷ Social Status**
- Trigger: On-field success (yards, touchdowns)
- Consequence: Campus recognition, romantic access
- Propagation: Unidirectional (performance → status)
- Strength: High (0.8) during active participation
- Decay: Rapid after system exit

**Coupling 1.2: Role Assignment ⟷ Satisfaction**
- Trigger: Coach decision (ball carrier vs blocker)
- Consequence: Experiential shift (Rope → Tangled Rope)
- Propagation: Bidirectional (role affects satisfaction, dissatisfaction affects performance)
- Strength: Moderate (0.5)

**Coupling 1.3: Physical Capacity ⟷ System Access**
- Trigger: Injury or aging
- Consequence: Forced exit, status freeze
- Propagation: Unidirectional (body → access)
- Strength: Absolute (1.0) — no workarounds

---

### C[2]: The Marital Partnership

**Base Parameters (Time-Dependent):**

**Early Phase (t₀ to t₁):**
- ε = 0.20 (low extraction, mutual affection)
- Supp = 0.3 (maintained by mutual desire)
- Coord = true (genuine partnership)
- Asymmetric = false (balanced power)

**Late Phase (t₂ to t₃):**
- ε = 0.65 (high extraction, dependency)
- Supp = 0.7 (maintained by fear, pity, inertia)
- Coord = false (ceased to be partnership)
- Asymmetric = true (Louise holds all power)

**Transformation Rules:**

**TR2.1: Economic Shift → Power Inversion**
```
IF Darling.income = 0
  AND Louise.income = total_household
THEN power_balance = inverted
  AND Darling.agency → minimal
  AND Louise.burden → maximal
```

**TR2.2: Dependency → Exit Cost Escalation**
```
IF Darling.alternatives = none
  AND Darling.self_worth = depleted
THEN exit_cost = perceived_as_infinite
  AND constraint_type = Snare (from Darling's index)
```

**TR2.3: Resentment Accumulation**
```
FOR EACH day WHERE power_asymmetry = high
  resentment += small_increment
  
IF resentment > threshold
THEN emotional_distance = "patient, kindly, remote boredom"
  AND constraint_type = Tangled_Rope (from Louise's index)
```

**Error Manifestations:**

**E2.1: Type I (False Mountain) — Darling**
- Treats marriage as unchangeable despite being trapped
- "The hopelessness... would be too much to take"
- Cannot imagine exit despite misery

**E2.2: Type V.a (Tangled-as-Rope) — Louise (early)**
- Ignores emerging extraction patterns
- Focuses on coordination benefits (companionship, shared life)
- Misses early warning signs of dependency formation

**Attractor:**
```
Terminal state: Frozen dependency
- Darling: economically trapped, emotionally depleted, exit = impossible
- Louise: financially burdened, emotionally distant, exit = guilt-blocked
- System: stable but dead (high suppression, zero coordination)
- Equilibrium: mutual imprisonment via different mechanisms
```

**Constraint Network (Couplings):**

**Coupling 2.1: Economic Dependency ⟷ Exit Options**
- Trigger: Loss of Darling's income
- Consequence: Exit cost → infinite (from Darling's perspective)
- Propagation: Unidirectional (economics → agency)
- Strength: Absolute (1.0)

**Coupling 2.2: Self-Worth ⟷ System Tolerance**
- Trigger: Daily inadequacy experiences
- Consequence: Increased tolerance for extraction
- Propagation: Bidirectional (low worth → accept abuse → lower worth)
- Strength: High (0.8), accelerating feedback loop

**Coupling 2.3: Louise's Burden ⟷ Emotional Distance**
- Trigger: Financial + emotional labor accumulation
- Consequence: Protective detachment ("patient, kindly, remote boredom")
- Propagation: Unidirectional (burden → distance)
- Strength: Moderate (0.6), gradual accumulation

**Coupling 2.4: Time ⟷ System Degradation**
- Trigger: Each passing year
- Consequence: ε increases (0.20 → 0.65), coordination decreases
- Propagation: Unidirectional (time → decay)
- Strength: Moderate (0.5), entropy-driven

---

### C[3]: The New York Intellectual & Cultural Sphere

**Base Parameters:**
- ε = 0.40 (moderate extraction via exclusion)
- Supp = 0.6 (enforced through social signals, shared language)
- Coord = true (creates vibrant intellectual community)
- Asymmetric = true (cultural capital determines status)

**Transformation Rules:**

**TR3.1: Cultural Capital → Access**
```
IF knowledge(Klee, Picasso, Odets, Trotsky) > threshold
THEN access = full
  AND status = insider
  AND experience_type = Rope

IF knowledge < threshold
THEN access = peripheral
  AND status = outsider
  AND experience_type = Tangled_Rope or Snare
```

**TR3.2: Learning Investment → Capital Accumulation**
```
FOR EACH book_read, conversation_participated
  cultural_capital += increment
  
IF cultural_capital > threshold
THEN system transitions from Tangled_Rope → Rope
```

**TR3.3: Incompatibility → Alienation**
```
IF preferences = "pictures with horses"
  AND system_language = "abstract modernism"
THEN alienation += high
  AND participation_cost → prohibitive
```

**Error Manifestations:**

**E3.1: Type I (False Mountain) — Darling**
- Treats cultural preferences as unchangeable ("it was no use")
- Believes intellectual capacity is fixed
- Cannot see learning as viable path

**E3.2: Type III (Snare-as-Rope) — Louise (initially)**
- Normalizes exclusionary aspects
- Focuses on coordination benefits (stimulation, community)
- Doesn't recognize extraction from those without capital

**Attractor:**
```
Terminal state: Permanent outsider status (Darling)
- Cannot acquire cultural capital (lacks interest + capacity)
- Trapped by marriage to insider (Louise)
- Forced participation without access to benefits
- Equilibrium: perpetual alienation, "on the edge of things"
```

**Constraint Network (Couplings):**

**Coupling 3.1: Cultural Capital ⟷ System Access**
- Trigger: Knowledge demonstration (conversation, references)
- Consequence: Insider vs outsider status
- Propagation: Bidirectional (capital → access → more capital)
- Strength: High (0.8)

**Coupling 3.2: Marriage ⟷ Forced Participation**
- Trigger: Louise's membership in C[3]
- Consequence: Darling must attend despite alienation
- Propagation: Unidirectional (marriage → participation)
- Strength: Moderate (0.6)

**Coupling 3.3: Alienation ⟷ Learning Capacity**
- Trigger: Repeated exclusion experiences
- Consequence: Decreased motivation to learn
- Propagation: Bidirectional (alienation → less learning → more alienation)
- Strength: High (0.7), negative feedback loop

**Coupling 3.4: C[1] Memory ⟷ C[3] Rejection**
- Trigger: Comparison between systems
- Consequence: C[3] appears illegitimate (different rules)
- Propagation: Unidirectional (C[1] nostalgia → C[3] resentment)
- Strength: Moderate (0.5)

---

## II. UNDERSPECIFIED CONSTRAINT ZONES (UCZs)

### UCZ-1: The Eighty-Yard Run Memory

**Underlying Variable:** Memory intensity and emotional valence of peak experience

**Range:** [baseline_memory, terminal_attractor_fixation]

**Mechanism:** Temporal (intensifies over time as present deteriorates)

**Parameters:**
```javascript
const memoryIntensity = (elapsedYears, presentSatisfaction) => {
  // Memory becomes more vivid as present becomes worse
  const contrastAmplification = 1 / (presentSatisfaction + 0.1);
  const timeDecay = 1 - (0.05 * elapsedYears); // Slight natural decay
  return baseIntensity * contrastAmplification * timeDecay;
};
```

**Coupling Participation:**
- Affects Coupling 3.4 (C[1] Memory ⟷ C[3] Rejection)
- Affects Coupling 2.2 (Self-Worth ⟷ System Tolerance)
- Drives attractor formation in C[1]

**Index Appearance:**
- **Young Darling:** Memory = recent, vivid, motivating (Rope experience)
- **Middle Darling:** Memory = bittersweet, comparative (Tangled Rope)
- **Old Darling:** Memory = terminal attractor, prevents adaptation (Snare)

**Implementation Pattern:**
```javascript
const eightyYardRunMemory = (age, currentSatisfaction) => {
  const yearsElapsed = age - 20; // Age at peak
  const intensification = 1 + (yearsElapsed * 0.1);
  const contrastEffect = (1 - currentSatisfaction) * 2;
  
  return {
    intensity: baseIntensity * intensification * contrastEffect,
    valence: currentSatisfaction < 0.3 ? "terminal_attractor" : "nostalgia",
    behavioralEffect: currentSatisfaction < 0.3 ? "prevents_adaptation" : "motivates"
  };
};
```

---

### UCZ-2: Louise's Emotional Threshold

**Underlying Variable:** Point at which Louise's patience converts to permanent distance

**Range:** [engaged_support, patient_boredom, active_resentment]

**Mechanism:** Threshold-chaotic (small changes in Darling's behavior near threshold produce large outcome shifts)

**Parameters:**
```javascript
const louiseEmotionalState = (darlingEffort, accumulatedBurden) => {
  const threshold = 0.65; // Burden level triggering detachment
  const sensitivity = 0.05; // Small changes matter near threshold
  
  if (Math.abs(accumulatedBurden - threshold) < sensitivity) {
    // Near bifurcation point: noise dominates
    const noise = (Math.random() * 2 - 1) * sensitivity;
    return (accumulatedBurden + noise) > threshold 
      ? "patient_kindly_remote_boredom" 
      : "engaged_but_strained";
  }
  
  return accumulatedBurden > threshold 
    ? "patient_kindly_remote_boredom"
    : "engaged_support";
};
```

**Coupling Participation:**
- Affects Coupling 2.3 (Louise's Burden ⟷ Emotional Distance)
- Affects Coupling 2.1 (Economic Dependency ⟷ Exit Options) — her distance increases his perceived exit cost

**Index Appearance:**
- **Darling's perspective:** Unpredictable, confusing (why does she seem distant?)
- **Louise's perspective:** Gradual, inevitable (burden accumulation)
- **Reader's perspective:** Tragic threshold crossing

**Implementation Pattern:**
```javascript
const louiseThresholdUCZ = (burden, darlingEffort) => {
  const THRESHOLD = 0.65;
  const SENSITIVITY = 0.05;
  
  // Near threshold, small effort changes flip outcome
  if (Math.abs(burden - THRESHOLD) < SENSITIVITY) {
    const effectiveEffort = darlingEffort + (Math.random() * 2 - 1) * SENSITIVITY;
    return effectiveEffort > 0.3 
      ? { state: "engaged_but_strained", exitProbability: 0.1 }
      : { state: "patient_boredom", exitProbability: 0.3 };
  }
  
  // Far from threshold, deterministic
  return burden > THRESHOLD
    ? { state: "patient_boredom", exitProbability: 0.3 }
    : { state: "engaged_support", exitProbability: 0.05 };
};
```

---

## III. INTER-CONSTRAINT COUPLINGS

### Meta-Coupling M1: C[1] ⟷ C[2]

**Trigger:** C[1] system exit (aging, injury, obsolescence)

**Consequence:** C[2] power inversion (Darling loses status → Louise gains relative power)

**Propagation:** Unidirectional (C[1] collapse → C[2] transformation)

**Strength:** High (0.8)

**Mechanism:**
```
IF C[1].participation = terminated
THEN C[2].power_balance = inverted
  AND C[2].ε increases (0.20 → 0.65)
  AND Darling.experience shifts (Rope → Snare)
```

---

### Meta-Coupling M2: C[1] ⟷ C[3]

**Trigger:** Attempt to apply C[1] rules to C[3] context

**Consequence:** Systematic failure, alienation

**Propagation:** Unidirectional (C[1] expectations → C[3] rejection)

**Strength:** Moderate (0.6)

**Mechanism:**
```
IF Darling.strategy = apply_C1_rules_to_C3
  // e.g., "physical presence should matter," "effort should be rewarded"
THEN C[3].access = denied
  AND alienation += high
  AND Type I error reinforced (treats C[1] rules as universal)
```

---

### Meta-Coupling M3: C[2] ⟷ C[3]

**Trigger:** Louise's C[3] participation requires Darling's presence

**Consequence:** Forced exposure to alienating system

**Propagation:** Unidirectional (C[2] obligation → C[3] participation)

**Strength:** Moderate (0.6)

**Mechanism:**
```
IF Louise.C3_participation = active
  AND C[2].status = married
THEN Darling.C3_exposure = forced
  AND Darling.alienation accumulates
  AND C[2].burden increases (for both)
```

---

## IV. SYSTEM ATTRACTOR

**Global Terminal State:**

```
Darling:
  - Economically dependent (C[2] Snare)
  - Socially alienated (C[3] outsider)
  - Psychologically trapped (C[1] memory as terminal attractor)
  - Exit options: none (perceived)
  - Experience: "Everything since has been a decline"

Louise:
  - Economically burdened (C[2] Tangled Rope)
  - Socially thriving (C[3] Rope)
  - Emotionally distant (protective detachment)
  - Exit options: available but guilt-blocked
  - Experience: "Patient, kindly, remote boredom"

System Equilibrium:
  - High suppression (maintained by fear, pity, inertia)
  - Zero coordination (ceased to be partnership)
  - Stable but dead (entropy maximized)
  - No transformation path visible from inside
```

**Attractor Strength:** High (0.9) — very difficult to escape without external intervention

**Escape Conditions:**
```
Darling escape requires:
  - Economic independence (breaks C[2] Coupling 2.1)
  - OR psychological reframe (breaks C[1] terminal attractor)
  - OR external crisis (forces system reset)

Louise escape requires:
  - Overcoming guilt (breaks C[2] Coupling 2.3)
  - OR Darling's initiative (removes burden)
  - OR external permission structure

System escape requires:
  - Mutual recognition of terminal state
  - Coordinated exit or radical restructuring
  - (Probability: very low from current state)
```

---

## V. CAUSAL DAG (Software Implementation)

```
C[1]: Athletic Performance System
  ├─ TR1.1: Performance → Status
  ├─ TR1.2: Role → Experience
  ├─ TR1.3: Injury → Exit
  ├─ Coupling 1.1: Performance ⟷ Social Status (0.8)
  ├─ Coupling 1.2: Role ⟷ Satisfaction (0.5)
  ├─ Coupling 1.3: Physical Capacity → System Access (1.0)
  └─ UCZ-1: Memory Intensity (temporal)

C[2]: Marital Partnership
  ├─ TR2.1: Economic Shift → Power Inversion
  ├─ TR2.2: Dependency → Exit Cost Escalation
  ├─ TR2.3: Resentment Accumulation
  ├─ Coupling 2.1: Economic Dependency → Exit Options (1.0)
  ├─ Coupling 2.2: Self-Worth ⟷ System Tolerance (0.8)
  ├─ Coupling 2.3: Burden → Emotional Distance (0.6)
  ├─ Coupling 2.4: Time → System Degradation (0.5)
  └─ UCZ-2: Louise's Threshold (threshold-chaotic)

C[3]: Intellectual Sphere
  ├─ TR3.1: Cultural Capital → Access
  ├─ TR3.2: Learning → Capital Accumulation
  ├─ TR3.3: Incompatibility → Alienation
  ├─ Coupling 3.1: Cultural Capital ⟷ Access (0.8)
  ├─ Coupling 3.2: Marriage → Forced Participation (0.6)
  ├─ Coupling 3.3: Alienation ⟷ Learning (0.7)
  └─ Coupling 3.4: C[1] Memory → C[3] Rejection (0.5)

Meta-Couplings:
  ├─ M1: C[1] Exit → C[2] Power Inversion (0.8)
  ├─ M2: C[1] Rules → C[3] Failure (0.6)
  └─ M3: C[2] Obligation → C[3] Exposure (0.6)

Global Attractor:
  └─ Terminal State: Mutual Imprisonment (strength: 0.9)
```

---

## VI. BMK GATE (STAGE 1)

**Verification:**

□ **Does the specification contain at least 2 transformation rules?**
✓ YES — 9 transformation rules total (3 per constraint)

□ **Does the constraint network contain at least 1 coupling?**
✓ YES — 10 intra-constraint couplings + 3 meta-couplings

□ **Is there at least 1 perspectival gap (same C, different type from different index)?**
✓ YES — Multiple:
  - C[1]: Rope (Diederich) vs Tangled Rope (Darling as blocker)
  - C[2]: Rope (both early) → Snare (Darling late) vs Tangled Rope (Louise late)
  - C[3]: Rope (Louise, Flaherty) vs Tangled Rope/Snare (Darling)

□ **Is the attractor non-trivial (requires state changes to reach)?**
✓ YES — Terminal state requires:
  - C[1] system exit (aging/obsolescence)
  - C[2] power inversion (economic shift)
  - C[3] forced participation (marriage coupling)
  - Multiple transformation rule activations
  - UCZ threshold crossings

**Result:** ALL CHECKS PASSED → Proceed to Stage 2

---

**STAGE 1 COMPLETE**

**Deliverables:**
- 3 constraints fully formalized with transformation rules
- 10 intra-constraint couplings specified with propagation direction and strength
- 3 meta-couplings connecting constraint systems
- 2 UCZs with explicit ambiguity mechanisms
- Global attractor with escape conditions
- Complete causal DAG for software implementation
- BMK gate passed

**Ready for Stage 2: Naturalization in new setting**