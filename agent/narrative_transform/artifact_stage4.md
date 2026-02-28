## STAGE 4: INTERACTION DESIGN & INDEXICAL REVELATION

**Role:** The Choreographer
**Model:** Grok (diplomatic, narrative-forward; excels at conflict mediation and social framing; or Qwen when it's being the Builder — test first, Qwen is volatile)
**Input:** Stage 1 specification (relabeled if air gap) + Stage 3 architecture
**Output:** Detailed interaction blueprint

### 4.1 Core Principle

In UKE_Narrative, Stage 3 plans how the *reader*  (editorially). discovers indexical variance through story events.
In UKE_Artifact, Stage 4 plans how the *user* discovers indexical variance through interaction.

The key difference: **the user has agency.** The revelation can't be sequenced like a story — it must be *available* as a navigable space.

### 4.2 Canonical State Object

All index views must derive from a single shared state. Generate the canonical state first, then derive each index view from it. This prevents contradictions between perspectives.

**Required Schema (Stage 4 Engine must export this structure):**

```javascript
const canonicalState = {
  constraints: {
    [C_id]: {
      value: number,          // Current operational value
      epsilon: number,        // ε from Stage 1
      chi: number,            // χ calculated value
      support: number,        // Supp from Stage 1
      type: string,           // "mountain" | "rope" | "snare" | etc.
      phase: string,          // "pre_TR1" | "post_TR1" | etc.
      ucz: null | {           // If this constraint has a UCZ
        mechanism: string,    // "stochastic" | "index_dependent" | etc.
        params: object        // Mechanism-specific parameters
      }
    }
  },
  transformationRules: {
    [TR_id]: {
      fired: boolean,
      progress: number,       // 0.0–1.0 toward trigger threshold
      threshold: number,
      reversible: boolean
    }
  },
  couplings: {
    [coupling_id]: {          // e.g., "C1_C2"
      source: C_id,
      target: C_id,
      strength: number,
      direction: string       // "unidirectional" | "bidirectional"
    }
  },
  system: {
    attractorProximity: number,   // 0.0–1.0
    hysteresisFlags: {            // Which perspective shifts have occurred
      [flag_id]: boolean
    },
    terminalReached: boolean
  }
};
```

**Index views are functions of canonical state:**

```javascript
// Each index is a read function, not a separate data store
function deriveIndexView(canonicalState, indexPosition) {
  return {
    // Same constraint, different label
    metrics: mapConstraintsToLabels(canonicalState, indexPosition),
    // Same state, different available actions
    actions: filterActionsByIndex(canonicalState, indexPosition),
    // Same value, different presentation
    feel: getInterfaceFeel(indexPosition)  // "viscous" | "glassy" | "clinical"
  };
}
```

**Validation:** The Engine (Stage 5, Step 1) must export a state object matching this schema. Missing required fields = regeneration.

### 4.3 Interaction Mapping

For each constraint in the specification:

```
Constraint: [C_id]
  Visible as: [What system element represents this?]
  Aesthetic signature: [From Stage 3.6]
  
  From Index A:
    - Metrics displayed: [what numbers, what labels]
    - Interface feel: [viscous / glassy / clinical]
    - Available actions: [what can the user do?]
    - Experienced type: [Snare — never labeled as such in Path A/C]
    
  From Index B:
    - Metrics displayed: [same numbers, different labels or framing]
    - Interface feel: [different from Index A]
    - Available actions: [different action set]
    - Experienced type: [Rope — never labeled]
    
  Revelation mechanism:
    - How does the user discover both views are of the same constraint?
    - What interaction triggers recognition?
```

### 4.4 The Hysteresis Condition

For at least one key constraint, design the revelation so that **once the user sees it from a second index, they cannot fully return** to experiencing it as they did from the first.

**What hysteresis is:** A design-phase condition that creates gravity for later stages. When the Architect specifies a hysteresis point well, the Fabricator will implement it because the specification makes it the path of least resistance — not because a checklist mandates it.

**What hysteresis is not:** A mechanical requirement that can be validated by grepping for `ghostState` variables. Cosmetic implementations (opacity overlays, tooltip ghosts) satisfy the letter but miss the spirit. If the hysteresis doesn't change what the user *can do* or *believes to be true*, it's decoration.

**Design pattern:**
1. User forms stable understanding from Index A
2. User triggers perspective shift to Index B — same data, different framing
3. User returns to Index A — original framing persists, but something has shifted
4. The shift may be: available actions changed, a metric now reads differently, information latency altered, or a previously trusted indicator is now suspect

**Minimum requirement:** One hysteresis point per artifact. The specification should make it feel inevitable, not forced.

**Structural Hysteresis Examples (not visual overlays):**

```
1. ACTION AVAILABILITY:
   Before shift: User can "Request Resources" from Index A.
   After shift to Index B and back: Action relabeled
   "Request Resources (Denied Twice)" — now has 30% failure rate.
   → User's agency was permanently reduced by knowledge.

2. METRIC TRUSTWORTHINESS:
   Before shift: "System Health: 78%" in Index A. User trusts it.
   After shift to Index B: Same metric reads "Reported Health: 78% (Audited: 42%)."
   Return to Index A: Metric reads "78%" but pulses subtly.
   → The number hasn't changed. The user's belief in it has.

3. INFORMATION LATENCY:
   Before shift: Data updates in real-time in Index A.
   After shift to Index B: Same data, 2-second delay.
   Return to Index A: Real-time again, but a brief lag ghost appears on changes.
   → User now suspects all data freshness.

4. STRUCTURAL REVELATION:
   Before shift: "Approval Required" gate blocks actions in Index A.
   After shift to Index B: User sees the gate is decorative — system ignores it.
   Return to Index A: Gate still present but now faintly outlined.
   → User knows the wall is fake. The wall is still there.
```

Each example changes what the user **can do**, **trusts**, or **believes** — not just what they see.

### 4.5 Misrecognition Tolerance & Anti-Help Constraint

Some users may walk away with a wrong but internally coherent model of the system. This is not a failure if the misunderstanding is structurally grounded in their index position.

```
MISRECOGNITION TOLERANCE:
  □ Can a user form a stable but incomplete understanding?
  □ Is that misunderstanding structurally grounded in their index?
  □ Does the system resist easy correction without perspective shift?
  □ Would the user need to *move* to discover they were wrong?
```

**Anti-Help Constraint:** For misrecognition-tolerant constraints, the artifact must not correct the user. This conflicts with model training toward helpfulness. Specify explicitly:

```
ANTI-HELP RULE (for misrecognition-tolerant constraints):
  - No tooltips explaining alternative interpretations
  - No warning dialogs before consequential actions
  - No corrective UI affordances
  - No "are you sure?" confirmations that reveal hidden information
  - No progress bars or tutorials guiding toward "correct" understanding
  
The artifact lets the user be confidently wrong.
This is art, not pedagogy.
```

**Design courage question:** After specifying misrecognition tolerance, ask: *Would you be comfortable letting a smart user leave with the wrong conclusion?* If no — the misrecognition tolerance is probably decorative.

**Anti-Help Validation Checklist (for Stage 6):**
```
For each misrecognition-tolerant constraint:
  □ No tooltips/hover text reveal alternative interpretations?
  □ No warning dialogs hint at hidden outcomes?
  □ No color coding or icons correct the user's model?
  □ No confirmations expose information not in current index?
  □ User can act on wrong-but-coherent model without interruption?
```

### 4.6 Constraint Shock Events

Non-reversible, non-telegraphed system transitions from transformation rules. **Shock events should feel ordinary when triggered and catastrophic only in hindsight.**

```
Shock Events:
  For each transformation rule (TR):
    - Can this be experienced as a shock?
    - If yes: what ordinary behavior accumulates toward the threshold?
    - What is lost when it fires?
    - How does the user reconstruct what happened?
```

### 4.7 State Transition Planning

```
Transformation: [TR_id]
  Trigger: [What user action or system event fires this?]
  Presentation: [Shock event? Gradual drift? Visible announcement?]
  Visual consequence: [What changes in the interface?]
  Index impact: [Which perspectives shift, and how?]
  Reversible: [Can/should the user undo this?]
```

### 4.8 Terminal State Design

```
Attractor: [from Stage 1]
  How does the system reach this state?
  What does it look like from each index position?
  Is it interactive or terminal?
  
  If real-world counterpart has an unresolved Omega:
    How does the artifact express the unresolved question?
    Can the user stress-test toward resolution?
```

### Stage 4 Tension Prompt (optional)
*"What interaction would make the user feel complicit in the constraint physics?"*

### Output

```
Complete interaction blueprint:
  - Canonical state object
  - Screen/view specifications per index position
  - Constraint-driven aesthetic assignments
  - Hysteresis point(s)
  - Shock events (mapped from transformation rules)
  - Misrecognition tolerance spec with anti-help constraints
  - State machine with transitions
  - Terminal state design
```

---
