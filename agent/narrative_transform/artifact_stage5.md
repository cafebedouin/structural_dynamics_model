## STAGE 5: ARTIFACT GENERATION (Modular)

**Role:** The Fabricator
**Model:** Claude (for React/interactive web; demonstrative, good at side-by-side contrast), Grok (for narrative-heavy Path A; mythic resonance), or Gemini (for infrastructure-heavy Path D; process documentation)
**Input:** Stage 3 architecture + Stage 4 interaction blueprint + Stage 1 specification (RELABELED if air gap)
**Output:** Working software artifact

**CRITICAL: If air gap is Full, the Fabricator session has NEVER seen the source material. It works only from the relabeled specification, architecture, and blueprint.**

### 5.1 Modular Fabrication (Three-Step Sequence)

Generating a complete artifact in one pass risks "logic drift" — the model focuses on CSS while forgetting causal propagation, or loses personality under technical load. Instead:

```
STEP 1 — THE ENGINE (logic only):
  Generate the constraint system as pure state management.
  - State variables for each constraint
  - Coupling propagation functions
  - Transformation rule triggers
  - UCZ resolution mechanisms
  - Canonical state object from Stage 4.2
  
  Output: A state machine / hook / module that is UGLY but CORRECT.
  No UI. No aesthetics. Just working constraint physics.
  
  UCZ smoke test: Call constraintReducer(state, {type: 'adjustC1', value: 0.9}) 5x.
  Stochastic UCZ must produce ≥2 different outcomes.
  If all 5 identical → UCZ is secretly deterministic. Fix before Step 2.

STEP 2 — THE VIEWPORTS (presentation only):
  Generate the UI components for each index position.
  - Visual elements per constraint
  - Aesthetic signatures from Stage 3.6
  - Index-to-feel mappings
  - Personality-driven text from Stage 3.7 (error messages, status reports)
  
  Output: Components that LOOK right but aren't connected to logic.

STEP 3 — THE BINDING (integration):
  Connect the Engine to the Viewports.
  - Wire state changes to visual updates
  - Implement hysteresis overlay
  - Implement shock event transitions
  - Test causal propagation end-to-end
  
  Output: Working artifact.
```

**Interface Contracts (Steps must be compatible):**

The Engine, Viewports, and Binding steps are generated in separate passes. To prevent incompatible outputs, each step must fulfill a contract:

```typescript
// STEP 1 ENGINE must export:

// State type — plain object, never a class instance
type CanonicalState = {
  constraints: Record<string, ConstraintState>;
  transformationRules: Record<string, TransformationRuleState>;
  couplings: Record<string, CouplingState>;
  system: SystemState;
};

// Pure initial state factory
function createInitialState(): CanonicalState;

// Pure reducer — returns new state, never mutates
function constraintReducer(state: CanonicalState, action: Action): CanonicalState;

// Pure index derivation — no side effects, no subscriptions
function deriveIndexView(state: CanonicalState, index: IndexPosition): IndexView;

// Pure coupling propagation — called inside reducer, not externally
function propagateCouplings(state: CanonicalState): CanonicalState;

// Temporal advancement (if needed) — also pure, called via dispatch
function advanceTime(state: CanonicalState, dt: number): CanonicalState;

// STEP 2 VIEWPORTS must:
// - Accept state as props (passed from parent), NOT call getState()
// - Emit actions via dispatch (from useReducer), NOT modify state
// - Re-render via React's own state mechanism, NOT manual subscribe()
// - Export one component per index position

// STEP 3 BINDING must:
// - Use useReducer(constraintReducer, createInitialState())
// - Pass state + dispatch to Viewport components as props
// - Implement index switching (including hysteresis flag setting)
// - Be the ONLY place where Engine and Viewports are aware of each other
```

**DO NOT use a mutable class with subscribe/notifyListeners.** The observer pattern causes double-render bugs when dispatch() and tick() both call notifyListeners(). React's useReducer provides the subscription mechanism — use it.

**Between-step validation:** Before proceeding from Step 1 to Step 2, the human operator verifies the Engine exports pure functions matching the contract above. Before Step 3, verify Viewports receive state as props, not via external getState() calls.

**Why this works:** Each step has a single focus. The model doesn't have to hold aesthetics and logic simultaneously. The human operator can review each step and catch drift before it compounds.

**Single-pass is acceptable** for simple topologies (≤3 constraints, ≤2 indices, ≤2 transformation rules). Above that, modularize.

### 5.2 Core Requirements (All Paths)

```
CAUSAL INTEGRATION (mandatory):
  Every metric must change system behavior when altered.
  If you change the number, does the system behave differently?
  If no → regenerate the Engine (Step 1).

CONSTRAINT TOPOLOGY PRESERVATION (mandatory):
  Every constraint from Stage 1 present as functional element.
  Every coupling implemented as causal propagation.
  Every transformation rule executable.

INDEXICAL VARIANCE (mandatory for Paths A, B, C, E):
  Different index positions produce visibly different experiences
  of the same underlying canonical state.

CONSTRAINT-DRIVEN AESTHETICS (mandatory):
  Type-to-interaction and index-to-feel mappings from Stage 3.6
  must be implemented, not just documented.

PERSONALITY FIDELITY (mandatory):
  System voice, diagnostic vocabulary, and behavioral constants
  from Stage 3.7 must be implemented consistently.
```

### 5.3 Dynamic Topology (Optional, Recommended)

The constraint network need not be static. Implement at least one of:

- **Drift:** Purity scores or ε values change slowly based on user interaction or time
- **Entropy:** UCZs spawn destabilizing effects if left unaddressed
- **Amnesia:** If the user avoids a constraint, its UI degrades. Neglect has consequence.

### 5.4 Path-Specific Requirements

**Path A (Diegetic System):**
```
Air gap: FULL (mandatory)
Framework invisibility: TOTAL
  - Zero constraint terminology in UI
  - System jargon internally consistent and original
  - Grep test: all framework terms return 0
  - Hollow center maintained
  - Source material unrecognizable
```

**Path B (Constraint Explorer):**
```
Air gap: PARTIAL
Framework visibility: CONTROLLED
  - Constraint types may be shown but experienced before labeled
  - Topological mapping explicit and navigable
  - Real data sourced and cited
  - Omega variable asymmetry visible
  - Context / Descent pattern: literary context → structural recognition → real descent
```

**Path C (Perspectival Engine):**
```
Air gap: FULL (mandatory)
Framework invisibility: HIGH
  - No labels, no types in UI
  - Indexical variance IS the experience
  - No position is "the correct one"
  - Source material unrecognizable
```

**Path D (Topology Bridge):**
```
Air gap: NONE
Framework visibility: HIGH (by design)
  - Constraint network is the UI
  - Types, metrics, drift all visible
  - Designed as aesthetic experience, not spreadsheet
```

**Path E (Parallel Resonance):**
```
Air gap: PARTIAL
Synchronization fidelity:
  - Literary and real panels temporally aligned
  - Constraint highlighting crosses domains
  - Bridge visually distinct
  - Recognition emerges gradually, not announced
```

### 5.5 Sanctioned Anomaly (Optional)

You may designate at most one constraint per artifact as "spectral." Spectral constraints may violate strict topology, must be felt as destabilizing, must not be explained in-framework, and must be documented in validation as intentional.

**Justification required:** Why does this anomaly serve the artifact? What would be lost if it were made normal? All other constraints must have perfect topology preservation.

---
