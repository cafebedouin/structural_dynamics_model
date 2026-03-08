# STAGE 1: FORMAL SPECIFICATION

**Role:** The Logician  
**Model:** Copilot (conservative, rule-bound, integrity-first)

---

## I. CONSTRAINT FORMALIZATION

### C[1]: The Judgment of Onlookers

**Formal Definition:**
A social obligation constraint requiring conformity to contradictory, unsolicited advice from transient observers to avoid shame and ridicule.

**Base Parameters:**
- ε = 0.80 (high extraction)
- Supp = 0.40 (moderate enforcement through social pressure)
- Coord = false (mutually exclusive demands prevent goal achievement)

**Asymmetry Structure:**
- Cost-bearers: Miller, Son (implementation, confusion, property loss)
- Beneficiaries: Onlookers (zero-cost norm reinforcement, entertainment)

---

## II. TRANSFORMATION RULES

### TR[1]: Compliance Escalation
**Trigger:** Miller encounters new group of onlookers  
**Precondition:** Miller has not yet lost the Ass  
**State Change:**
```
Miller.agency → Miller.agency - 0.2
Miller.confusion → Miller.confusion + 0.3
Onlookers.satisfaction → Onlookers.satisfaction + 0.1
```
**Threshold:** When Miller.agency ≤ 0.2, proceed to TR[2]

### TR[2]: Terminal Extraction
**Trigger:** Miller.agency crosses critical threshold (≤ 0.2)  
**Precondition:** Miller has attempted to satisfy all contradictory demands  
**State Change:**
```
Miller.property → 0 (Ass lost)
Miller.dignity → 0 (public humiliation complete)
Miller.agency → 0 (total depletion)
System.state → TERMINAL
```
**Irreversibility:** Once property = 0, no recovery path exists

### TR[3]: Onlooker Refresh
**Trigger:** Miller moves to new location  
**Precondition:** System.state ≠ TERMINAL  
**State Change:**
```
Onlookers.identity → NEW_GROUP
Onlookers.advice → CONTRADICTS(previous_advice)
Miller.confusion → Miller.confusion + 0.2
```
**Coupling:** Feeds back into TR[1]

---

## III. CONSTRAINT NETWORK (CAUSAL DAG)

### Coupling[1]: Advice → Compliance
**Direction:** Onlookers.advice → Miller.action  
**Strength:** 0.9 (Miller nearly always complies)  
**Propagation:**
```
IF Onlookers.advice = X
THEN Miller.action = X (within 1 interaction cycle)
AND Miller.previous_plan = ABANDONED
```

### Coupling[2]: Compliance → Confusion
**Direction:** Miller.action → Miller.confusion  
**Strength:** 0.7 (cumulative effect)  
**Propagation:**
```
Miller.confusion(t+1) = Miller.confusion(t) + 0.3 × |contradiction_count|
WHERE contradiction_count = number of mutually exclusive demands encountered
```

### Coupling[3]: Confusion → Agency Depletion
**Direction:** Miller.confusion → Miller.agency  
**Strength:** 0.8 (strong negative correlation)  
**Propagation:**
```
Miller.agency(t+1) = Miller.agency(t) - (0.2 × Miller.confusion(t))
THRESHOLD: agency ≤ 0.2 triggers TR[2]
```

### Coupling[4]: Agency Depletion → Terminal Loss
**Direction:** Miller.agency → Miller.property  
**Strength:** 1.0 (deterministic at threshold)  
**Propagation:**
```
IF Miller.agency ≤ 0.2
THEN Miller.property → 0 (immediate)
AND System.state → TERMINAL (irreversible)
```

### Coupling[5]: Location Change → Advice Refresh
**Direction:** Miller.location → Onlookers.advice  
**Strength:** 1.0 (deterministic)  
**Propagation:**
```
IF Miller.location = NEW
THEN Onlookers.advice = CONTRADICTS(all_previous_advice)
AND Coupling[1] reactivates
```

---

## IV. ATTRACTOR DEFINITION

**Terminal State (Non-Trivial):**
```
System.attractor = {
  Miller.agency = 0,
  Miller.property = 0,
  Miller.dignity = 0,
  Miller.confusion = MAX,
  Onlookers.satisfaction = HIGH,
  System.state = TERMINAL
}
```

**Path to Attractor:**
1. Initial state: Miller has plan, property, agency
2. TR[1] fires repeatedly (Compliance Escalation)
3. Coupling[2] and Coupling[3] accumulate confusion and deplete agency
4. Miller.agency crosses threshold (≤ 0.2)
5. TR[2] fires (Terminal Extraction)
6. Attractor reached (irreversible)

**Non-Triviality:** Requires multiple state transitions through feedback loops. Cannot be reached in single step.

---

## V. UNDERSPECIFIED CONSTRAINT ZONES (UCZs)

### UCZ[1]: Onlooker Advice Content
**Underlying Variable:** Specific content of advice given by each group  
**Range:** [any action involving Miller, Son, Ass configuration]  
**Mechanism:** Index-dependent (resolves differently per onlooker group)  
**Parameters:**
```javascript
adviceGenerator = (previousAdvice) => {
  const options = [
    "both_walk",
    "miller_rides",
    "son_rides", 
    "both_ride",
    "carry_ass"
  ];
  // Must contradict previous advice
  return options.filter(x => x !== previousAdvice)[
    Math.floor(Math.random() * (options.length - 1))
  ];
}
```
**Coupling Participation:** Drives Coupling[1] (Advice → Compliance)  
**Index Appearance:**
- Group 1: "Make him walk, young lazybones!"
- Group 2: "What a selfish old man!"
- Group 3: "Aren't you ashamed of yourselves?"
- Group 4: "Did you ever see such a pair of fools?"

**Structural Constraint:** Each advice instance must:
1. Be mutually exclusive with previous advice
2. Be delivered with moral authority
3. Trigger Miller's compliance (Coupling[1])

### UCZ[2]: Threshold Timing
**Underlying Variable:** Exact number of compliance cycles before agency depletion  
**Range:** [3, 6] interactions  
**Mechanism:** Stochastic (random within bounded distribution)  
**Parameters:**
```javascript
agencyDepletion = (baseRate = 0.2, variance = 0.05) => {
  return () => baseRate + (Math.random() * 2 - 1) * variance;
}
```
**Coupling Participation:** Affects Coupling[3] (Confusion → Agency Depletion)  
**Index Appearance:** Fable shows 4 groups before terminal loss, but exact count varies

**Structural Constraint:**
- Must allow multiple compliance cycles (≥3) for escalation
- Must reach terminal threshold before infinite loop
- Variance preserves dramatic tension without changing outcome

---

## VI. PERSPECTIVAL GAPS (INDEXICAL VARIANCE)

### Gap[1]: C[1] Type Classification

**Miller's Index:**
- I = (powerless, immediate, trapped, local)
- χ = 0.80 × 1.5 × 0.8 = 0.96
- **Type: Snare (⊠)**
- **Experience:** Inescapable trap extracting autonomy and property

**Son's Index:**
- I = (powerless, immediate, trapped, local)
- χ = 0.80 × 1.5 × 0.8 = 0.96
- **Type: Snare (⊠)**
- **Experience:** Passive victim of same trap, no agency

**Onlookers' Index:**
- I = (institutional, immediate, arbitrage, local)
- χ = 0.80 × -0.2 × 0.8 = -0.128
- **Type: Rope (⊞)**
- **Experience:** Zero-cost social coordination mechanism

**Structural Significance:**
Same constraint C[1] is simultaneously:
- Destructive Snare (Miller, Son)
- Beneficial Rope (Onlookers)

Both classifications objectively true from respective indices.

### Gap[2]: Error Type Divergence

**Miller's Error:**
- **Type I (False Mountain)**
- Treats transient, contradictory social pressure as unchangeable force
- Believes he must comply to avoid shame
- **Structural Basis:** From (powerless, immediate, trapped, local), social pressure appears as immutable as gravity

**Onlookers' Error:**
- **Type III (Snare-as-Rope)**
- Normalize extraction as coordination
- Believe their advice is helpful social guidance
- **Structural Basis:** From (institutional, immediate, arbitrage, local), they don't experience the extraction they impose

---

## VII. CONSTRAINT NETWORK DIAGRAM

```
┌─────────────────────────────────────────────────────────────┐
│                    CONSTRAINT NETWORK                        │
│                 C[1]: Judgment of Onlookers                  │
└─────────────────────────────────────────────────────────────┘

Onlookers.advice ──[Coupling[1]: 0.9]──> Miller.action
                                              │
                                              │
                                              ▼
                                    Miller.confusion
                                              │
                                              │ [Coupling[2]: 0.7]
                                              │
                                              ▼
                                      Miller.agency
                                              │
                                              │ [Coupling[3]: 0.8]
                                              │
                                              ▼
                                    [Threshold: ≤ 0.2]
                                              │
                                              │ [Coupling[4]: 1.0]
                                              ▼
                                     Miller.property → 0
                                              │
                                              ▼
                                    System.state → TERMINAL
                                              │
                                              ▼
                                        ATTRACTOR

Miller.location ──[Coupling[5]: 1.0]──> Onlookers.advice
       │                                       │
       └───────────────[feedback loop]────────┘
```

**Feedback Structure:**
- Primary loop: Advice → Action → Confusion → Agency Depletion
- Refresh loop: Location Change → New Advice (resets advice, not confusion)
- Terminal condition: Agency ≤ 0.2 breaks loops, triggers irreversible loss

---

## VIII. BMK GATE VERIFICATION

### Checklist:

✅ **Does the specification contain at least 2 transformation rules?**
- Yes: TR[1] (Compliance Escalation), TR[2] (Terminal Extraction), TR[3] (Onlooker Refresh)

✅ **Does the constraint network contain at least 1 coupling?**
- Yes: 5 couplings specified with direction, strength, and propagation rules

✅ **Is there at least 1 perspectival gap (same C, different type from different index)?**
- Yes: C[1] is Snare (⊠) from Miller/Son indices, Rope (⊞) from Onlookers' index

✅ **Is the attractor non-trivial (requires state changes to reach)?**
- Yes: Requires multiple cycles through feedback loops, threshold crossing, irreversible state transition

**BMK GATE: PASSED**

---

## IX. IMPLEMENTATION NOTES FOR STAGE 4

### State Variables Required:
```javascript
const systemState = {
  miller: {
    agency: 1.0,        // [0, 1]
    confusion: 0.0,     // [0, ∞)
    property: 1.0,      // {0, 1} (has Ass or not)
    dignity: 1.0,       // [0, 1]
    location: "start"   // string
  },
  son: {
    agency: 0.0,        // always 0 (no independent action)
    dignity: 1.0        // [0, 1]
  },
  onlookers: {
    currentAdvice: null,
    previousAdvice: [],
    satisfaction: 0.0   // [0, 1]
  },
  system: {
    state: "ACTIVE",    // {"ACTIVE", "TERMINAL"}
    cycleCount: 0
  }
};
```

### Coupling Implementation:
```javascript
// Coupling[1]: Advice → Compliance
function applyAdvice(advice) {
  systemState.miller.action = advice;
  systemState.onlookers.previousAdvice.push(advice);
  return true; // compliance probability = 0.9
}

// Coupling[2]: Compliance → Confusion
function accumulateConfusion() {
  const contradictionCount = systemState.onlookers.previousAdvice.length;
  systemState.miller.confusion += 0.3 * contradictionCount;
}

// Coupling[3]: Confusion → Agency Depletion
function depleteAgency() {
  systemState.miller.agency -= 0.2 * systemState.miller.confusion;
  if (systemState.miller.agency <= 0.2) {
    triggerTerminalExtraction();
  }
}

// Coupling[4]: Agency Depletion → Terminal Loss
function triggerTerminalExtraction() {
  systemState.miller.property = 0;
  systemState.miller.dignity = 0;
  systemState.miller.agency = 0;
  systemState.system.state = "TERMINAL";
}

// Coupling[5]: Location Change → Advice Refresh
function refreshAdvice() {
  const previousAdvice = systemState.onlookers.currentAdvice;
  systemState.onlookers.currentAdvice = adviceGenerator(previousAdvice);
  systemState.miller.location = "new_location_" + systemState.system.cycleCount;
}
```

### UCZ Implementation:
```javascript
// UCZ[1]: Onlooker Advice Content (Index-dependent)
const adviceGenerator = (previousAdvice) => {
  const options = ["both_walk", "miller_rides", "son_rides", "both_ride", "carry_ass"];
  const available = options.filter(x => x !== previousAdvice);
  return available[Math.floor(Math.random() * available.length)];
};

// UCZ[2]: Threshold Timing (Stochastic)
const agencyDepletionRate = () => {
  const base = 0.2;
  const variance = 0.05;
  return base + (Math.random() * 2 - 1) * variance;
};
```

---

## X. FORMAL SPECIFICATION SUMMARY

**Constraint:** C[1] (Judgment of Onlookers)  
**Transformation Rules:** 3 (TR[1], TR[2], TR[3])  
**Couplings:** 5 (fully specified with direction, strength, propagation)  
**Perspectival Gaps:** 2 (type classification, error type)  
**Attractor:** Non-trivial terminal state (requires multi-step path)  
**UCZs:** 2 (advice content, threshold timing)

**Indexical Variance:**
- Miller/Son: Snare (⊠), χ = 0.96
- Onlookers: Rope (⊞), χ = -0.128

**Causal Structure:** Feedback loops with threshold-triggered irreversible transition

**BMK Gate:** PASSED (all criteria met)

---

**STAGE 1 COMPLETE**

This formal specification provides:
1. Calculable state transitions (transformation rules)
2. Causal propagation network (couplings as DAG)
3. Terminal condition (attractor)
4. Procedural ambiguity (UCZs with implementation patterns)
5. Indexical variance (perspectival gaps)

Ready for Stage 2 (Naturalization) or Stage 4 (Engine Implementation).