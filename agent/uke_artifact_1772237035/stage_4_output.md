# STAGE 4: INTERACTION DESIGN & INDEXICAL REVELATION

**Model:** Claude (demonstrative, comparative)
**Role:** The Choreographer
**Input:** Stage 1 specification + Stage 3 architecture
**Output:** Detailed interaction blueprint

---

## 4.1 CORE PRINCIPLE

In this Path E artifact, the user discovers indexical variance through **synchronized exploration** across two panels. The revelation can't be sequenced like a story — it must be available as a navigable space where the user chooses their entry point and discovers structural echoes at their own pace.

**Key difference from narrative:** The user might start with 2024 data and work backward to Shaw, or vice versa. They might notice the power inversion before the status loss. The topology must be discoverable from any angle.

---

## 4.2 CANONICAL STATE OBJECT

### 4.2.1 State Schema

```javascript
const canonicalState = {
  constraints: {
    C1_athletic_performance: {
      value: 0.0,              // 0.0 = peak (1941), 1.0 = obsolete (1956)
      epsilon: 0.55,
      chi: null,               // Calculated per index position
      support: 0.8,
      type: "hybrid",          // Coordination + extraction
      phase: "pre_TR1.3",      // Before injury/obsolescence
      ucz: {
        mechanism: "temporal",
        params: {
          memoryIntensity: 1.0,
          contrastAmplification: 1.0,
          attractorStrength: 0.0  // Grows over time
        }
      }
    },
    
    C2_marital_partnership: {
      value: 0.0,              // 0.0 = early (balanced), 1.0 = late (trapped)
      epsilon: 0.20,           // Starts low, increases to 0.65
      chi: null,
      support: 0.3,            // Starts low, increases to 0.7
      type: "rope",            // Starts as Rope, becomes Snare
      phase: "pre_TR2.1",
      ucz: {
        mechanism: "threshold_chaotic",
        params: {
          louiseThreshold: 0.65,
          sensitivity: 0.05,
          currentBurden: 0.0,
          emotionalState: "engaged_support"
        }
      }
    },
    
    C3_cultural_sphere: {
      value: 0.0,              // 0.0 = no exposure, 1.0 = forced participation
      epsilon: 0.40,
      chi: null,
      support: 0.6,
      type: "hybrid",
      phase: "pre_TR3.3",
      ucz: null                // No UCZ for C3
    }
  },
  
  transformationRules: {
    TR1_3_system_exit: {
      fired: false,
      progress: 0.0,           // Accumulates with aging/injury
      threshold: 0.8,
      reversible: false,
      trigger: "time_or_injury"
    },
    
    TR2_1_power_inversion: {
      fired: false,
      progress: 0.0,           // Triggered by C1 exit
      threshold: 0.9,
      reversible: false,
      trigger: "C1_collapse"
    },
    
    TR2_2_exit_cost_escalation: {
      fired: false,
      progress: 0.0,           // Accumulates with dependency
      threshold: 0.7,
      reversible: false,
      trigger: "economic_dependency"
    },
    
    TR2_3_resentment_accumulation: {
      fired: false,
      progress: 0.0,           // Daily increment
      threshold: 0.65,         // Louise's UCZ threshold
      reversible: false,
      trigger: "burden_accumulation"
    },
    
    TR3_3_alienation: {
      fired: false,
      progress: 0.0,           // Accumulates with forced participation
      threshold: 0.6,
      reversible: false,
      trigger: "incompatibility"
    }
  },
  
  couplings: {
    C1_C2_status_loss: {
      source: "C1_athletic_performance",
      target: "C2_marital_partnership",
      strength: 0.8,
      direction: "unidirectional",
      active: false            // Activates when C1.value > 0.8
    },
    
    C2_C3_forced_participation: {
      source: "C2_marital_partnership",
      target: "C3_cultural_sphere",
      strength: 0.6,
      direction: "unidirectional",
      active: false            // Activates when C2.value > 0.3
    },
    
    C1_memory_C3_rejection: {
      source: "C1_athletic_performance",
      target: "C3_cultural_sphere",
      strength: 0.5,
      direction: "unidirectional",
      active: false            // Activates when C1.ucz.attractorStrength > 0.5
    }
  },
  
  system: {
    attractorProximity: 0.0,   // 0.0 = far, 1.0 = terminal state reached
    hysteresisFlags: {
      power_inversion_seen: false,
      louise_threshold_crossed: false,
      memory_attractor_recognized: false
    },
    terminalReached: false,
    currentTimestamp: 1941     // Scrubber position (1941-2024)
  },
  
  userState: {
    indexPosition: "darling_early",  // Current perspective
    explorationsCount: 0,
    patternsRecognized: [],
    advancedModeUnlocked: false
  }
};
```

### 4.2.2 Index View Derivation Functions

```javascript
// Index views are READ FUNCTIONS, not separate data stores
function deriveIndexView(canonicalState, indexPosition) {
  const { constraints, system } = canonicalState;
  
  // Calculate chi for this index position
  const calculateChi = (constraint, index) => {
    const baseEpsilon = constraint.epsilon;
    const indexModifier = getIndexModifier(index);
    return baseEpsilon + indexModifier;
  };
  
  const indexModifiers = {
    darling_early: { C1: -0.70, C2: -0.20, C3: 0.0 },   // Powerful, mobile
    darling_late: { C1: 0.30, C2: 0.58, C3: 0.32 },     // Powerless, trapped
    louise_early: { C1: 0.0, C2: -0.20, C3: 0.0 },      // Moderate, mobile
    louise_late: { C1: 0.0, C2: 0.35, C3: -0.08 },      // Moderate, constrained
    flaherty: { C1: 0.0, C2: 0.0, C3: -0.48 }           // Powerful, mobile
  };
  
  return {
    metrics: {
      C1: {
        label: indexPosition.includes("darling") ? "Athletic Status" : "Partner's Career",
        value: constraints.C1_athletic_performance.value,
        chi: calculateChi(constraints.C1_athletic_performance, indexPosition),
        type: calculateChi(constraints.C1_athletic_performance, indexPosition) < 0.30 ? "rope" :
              calculateChi(constraints.C1_athletic_performance, indexPosition) < 0.60 ? "tangled" : "snare"
      },
      C2: {
        label: indexPosition.includes("darling") ? "Marriage" : "Partnership",
        value: constraints.C2_marital_partnership.value,
        chi: calculateChi(constraints.C2_marital_partnership, indexPosition),
        type: calculateChi(constraints.C2_marital_partnership, indexPosition) < 0.30 ? "rope" :
              calculateChi(constraints.C2_marital_partnership, indexPosition) < 0.60 ? "tangled" : "snare"
      },
      C3: {
        label: indexPosition.includes("darling") ? "Social Circle" : "Intellectual Community",
        value: constraints.C3_cultural_sphere.value,
        chi: calculateChi(constraints.C3_cultural_sphere, indexPosition),
        type: calculateChi(constraints.C3_cultural_sphere, indexPosition) < 0.30 ? "rope" :
              calculateChi(constraints.C3_cultural_sphere, indexPosition) < 0.60 ? "tangled" : "snare"
      }
    },
    
    actions: filterActionsByIndex(canonicalState, indexPosition),
    
    feel: getInterfaceFeel(indexPosition, constraints)
  };
}

function filterActionsByIndex(canonicalState, indexPosition) {
  const baseActions = [
    "explore_literary_segment",
    "explore_data_point",
    "scrub_timeline",
    "switch_perspective"
  ];
  
  // Actions available depend on index position and system state
  if (indexPosition === "darling_late" && canonicalState.constraints.C2_marital_partnership.value > 0.7) {
    // Darling late: trapped, fewer actions available
    return baseActions.filter(a => a !== "switch_perspective"); // Can't escape
  }
  
  if (indexPosition === "louise_late" && canonicalState.system.hysteresisFlags.louise_threshold_crossed) {
    // Louise late: emotionally distant, different action set
    return [...baseActions, "view_burden_metrics"]; // New action available
  }
  
  return baseActions;
}

function getInterfaceFeel(indexPosition, constraints) {
  const chiValues = {
    darling_early: -0.15,
    darling_late: 0.85,
    louise_early: 0.20,
    louise_late: 0.55,
    flaherty: -0.08
  };
  
  const chi = chiValues[indexPosition];
  
  if (chi < 0.30) {
    return {
      style: "glassy",
      latency: 0,
      friction: 0.1,
      viewport: "expansive",
      colorGradient: "blue-to-green"
    };
  } else if (chi < 0.60) {
    return {
      style: "standard",
      latency: 50,
      friction: 0.5,
      viewport: "standard",
      colorGradient: "blue-yellow-split"
    };
  } else {
    return {
      style: "viscous",
      latency: 150,
      friction: 0.9,
      viewport: "constrained",
      colorGradient: "red-to-black"
    };
  }
}
```

---

## 4.3 INTERACTION MAPPING

### 4.3.1 Constraint C1: Athletic Performance

**Visible as:** 
- Literary panel: Shaw's text segments about the 80-yard run, practice, aging
- Data panel: Prime-age male labor force participation charts (1950-2024)

**Aesthetic signature:** 
- Early (Darling powerful): Glassy, blue-green gradient, expansive viewport
- Late (Darling powerless): Viscous, red-black gradient, constrained viewport

**From Index: Darling Early (1941)**
- **Metrics displayed:** "Athletic Status: Peak" (value = 0.0, chi = -0.15)
- **Interface feel:** Glassy — zero latency, smooth scrolling, minimalist
- **Available actions:** 
  - Read 80-yard run segment
  - View peak performance data
  - Scrub timeline forward
  - Switch to Louise perspective
- **Experienced type:** Rope (never labeled) — coordination dominates, net benefit

**From Index: Darling Late (1956)**
- **Metrics displayed:** "Athletic Status: Obsolete" (value = 1.0, chi = 0.85)
- **Interface feel:** Viscous — 150ms input lag, friction on scroll, viewport shrinks
- **Available actions:**
  - Read "fifteen years later" segment
  - View labor force decline data
  - Cannot switch perspective (trapped)
- **Experienced type:** Snare (never labeled) — extraction dominates, exit impossible

**From Index: Louise (any time)**
- **Metrics displayed:** "Partner's Career: Declining" (value = 0.8, chi = 0.20)
- **Interface feel:** Standard — responsive, neutral colors
- **Available actions:** All standard actions available
- **Experienced type:** Rope (never labeled) — coordination still present, moderate burden

**Revelation mechanism:**
1. User explores 80-yard run segment (Darling early, glassy feel)
2. User scrubs timeline to 1956 (interface becomes viscous, viewport shrinks)
3. User switches to Louise perspective (same constraint, different feel)
4. **Discovery:** Same constraint = different experience by index position
5. User explores 2024 data (same topology, 83 years later)

---

### 4.3.2 Constraint C2: Marital Partnership

**Visible as:**
- Literary panel: Shaw's text about Louise working, Darling's dependency
- Data panel: Relationship economics charts (dual income vs single earner)

**Aesthetic signature:**
- Early (both): Glassy, low friction, balanced
- Late (Darling): Viscous, high friction, trapped
- Late (Louise): Standard with friction, burdened

**From Index: Darling Early (1941)**
- **Metrics displayed:** "Marriage: Partnership" (value = 0.0, chi = 0.0)
- **Interface feel:** Glassy — smooth, balanced, expansive
- **Available actions:** All standard actions
- **Experienced type:** Rope — mutual affection, coordination

**From Index: Darling Late (1956)**
- **Metrics displayed:** "Marriage: Dependency" (value = 1.0, chi = 0.78)
- **Interface feel:** Viscous — high latency, constrained, friction
- **Available actions:** 
  - Read dependency segments
  - View economic data
  - **Cannot exit** (action grayed out)
- **Experienced type:** Snare — economically trapped, exit impossible

**From Index: Louise Early (1941)**
- **Metrics displayed:** "Partnership: Balanced" (value = 0.0, chi = 0.0)
- **Interface feel:** Glassy — smooth, balanced
- **Available actions:** All standard actions
- **Experienced type:** Rope — mutual coordination

**From Index: Louise Late (1956)**
- **Metrics displayed:** "Partnership: Burdened" (value = 1.0, chi = 0.55)
- **Interface feel:** Standard with friction — moderate latency, split gradient
- **Available actions:**
  - Read "patient, kindly, remote boredom" segment
  - View burden metrics (new action after UCZ threshold)
  - Exit available but guilt-blocked (action visible but warns)
- **Experienced type:** Tangled Rope — coordination ceased, burden high, but not trapped

**Revelation mechanism:**
1. User explores early marriage segments (both glassy)
2. User scrubs timeline forward (Darling's feel becomes viscous, Louise's becomes standard)
3. User switches between perspectives at late stage
4. **Discovery:** Same constraint, radically different experience
5. **Hysteresis point:** After seeing Louise's burden metrics, returning to Darling's view shows "Exit (Impossible)" action with faint outline — user now knows the wall is structural, not personal

---

### 4.3.3 Constraint C3: Cultural Sphere

**Visible as:**
- Literary panel: Shaw's text about Flaherty's parties, Klee/Picasso discussions
- Data panel: Cultural capital studies, arts participation by education

**Aesthetic signature:**
- Darling: Viscous, constrained, alienated
- Louise: Glassy, expansive, insider
- Flaherty: Glassy, powerful, gatekeeper

**From Index: Darling (any time)**
- **Metrics displayed:** "Social Circle: Excluded" (value = 0.8, chi = 0.72)
- **Interface feel:** Viscous — high friction, constrained viewport, alienating colors
- **Available actions:**
  - Read Flaherty party segments
  - View cultural capital data (low education tier)
  - Cannot engage meaningfully (actions available but fail)
- **Experienced type:** Snare — forced participation, no access to benefits

**From Index: Louise (any time)**
- **Metrics displayed:** "Intellectual Community: Thriving" (value = 0.5, chi = 0.32)
- **Interface feel:** Glassy — smooth, expansive, vibrant
- **Available actions:**
  - Read party segments (different highlights)
  - View cultural capital data (high education tier)
  - Engage fully (all actions succeed)
- **Experienced type:** Rope — coordination, stimulation, belonging

**From Index: Flaherty (if unlocked)**
- **Metrics displayed:** "Cultural Leadership: Established" (value = 0.3, chi = -0.08)
- **Interface feel:** Glassy — zero friction, maximally expansive
- **Available actions:** All actions + gatekeeping powers
- **Experienced type:** Rope (net benefit) — shapes discourse, high status

**Revelation mechanism:**
1. User explores Flaherty party segment from Darling's view (viscous, alienating)
2. User switches to Louise's view (same segment, glassy, vibrant)
3. **Discovery:** Same event, opposite experience
4. User explores 2024 cultural capital data (same structure persists)
5. **Hysteresis point:** After seeing Louise's insider view, returning to Darling shows "Engage (Futile)" action with ghost text "pictures with horses" — user knows the incompatibility is structural

---

## 4.4 THE HYSTERESIS CONDITION

### 4.4.1 Hysteresis Point 1: Power Inversion Recognition

**Before shift:**
- User explores C2 from Darling's early perspective
- "Exit Relationship" action available, neutral
- Marriage reads as balanced partnership

**Shift trigger:**
- User scrubs timeline to late stage (1956)
- TR2.1 (Power Inversion) fires
- User switches to Louise's late perspective

**After shift and return:**
- User returns to Darling's late perspective
- "Exit Relationship" action still visible but now reads "Exit (Impossible)"
- Action has faint red outline (new visual)
- Hover text (if enabled): "Economic dependency: 100%"
- **User's agency was permanently reduced by knowledge**

**What changed:**
- Not the action availability (still visible)
- Not the constraint value (still 1.0)
- **The user's belief in exit possibility**

**Structural consequence:**
- User now suspects all "available" actions in viscous contexts
- Trust in interface affordances degraded
- Cannot unsee the trap

---

### 4.4.2 Hysteresis Point 2: Louise's Threshold Crossing

**Before shift:**
- User explores C2 from Louise's early perspective
- "View Burden Metrics" action not present
- Emotional state reads "Engaged Support"

**Shift trigger:**
- User scrubs timeline forward
- TR2.3 (Resentment Accumulation) approaches threshold (0.65)
- UCZ-2 fires (Louise's emotional state shifts)

**After shift and return:**
- User returns to Louise's early perspective
- "View Burden Metrics" action now present (grayed out)
- Emotional state reads "Engaged Support" but with faint pulse
- **User knows the threshold is coming, cannot prevent it**

**What changed:**
- Not the early state (still engaged)
- Not the action set (burden metrics weren't available yet)
- **The user's knowledge of inevitable degradation**

**Structural consequence:**
- User now reads early partnership with dread
- Cannot experience early stage as purely positive
- Hysteresis = foreknowledge of decay

---

### 4.4.3 Hysteresis Point 3: Memory Attractor Recognition

**Before shift:**
- User explores 80-yard run segment (Darling early)
- Memory reads as positive, motivating
- Interface glassy, expansive

**Shift trigger:**
- User scrubs to late stage (1956)
- User explores "Everything since has been a decline" segment
- UCZ-1 (Memory Intensity) at maximum
- User recognizes memory as terminal attractor

**After shift and return:**
- User returns to 80-yard run segment (1941)
- Segment now has faint pulse effect (new visual)
- Interface still glassy but with subtle lag ghost on interactions
- **User knows this memory will trap Darling**

**What changed:**
- Not the memory content (still the 80-yard run)
- Not the interface feel (still glassy)
- **The user's understanding of memory's function**

**Structural consequence:**
- User cannot experience peak moment as purely triumphant
- Knows it will become prison
- Hysteresis = tragic foreknowledge

---

## 4.5 MISRECOGNITION TOLERANCE & ANTI-HELP CONSTRAINT

### 4.5.1 Misrecognition-Tolerant Constraints

**C1: Athletic Performance (from Darling's early index)**

**Possible misrecognition:**
- User believes Darling's decline is personal failure (lack of effort, wrong choices)
- Misses structural component (C1 system exit is non-reversible)
- Forms coherent but incomplete model: "He should have tried harder"

**Structural grounding:**
- From Darling's early index (powerful, mobile), system rules appear meritocratic
- Success seems earned through effort
- Decline appears preventable through continued effort
- **This misunderstanding is index-appropriate**

**Anti-Help constraints:**
- No tooltip on 80-yard run segment explaining "system exit inevitable"
- No warning dialog before timeline scrub: "Darling's decline is structural, not personal"
- No color coding distinguishing "personal failure" from "system exit"
- No confirmation: "Are you sure you want to see this as individual failure?"
- User can complete entire exploration believing Darling failed personally

**Misrecognition tolerance validation:**
```
□ Can user form stable but incomplete understanding? YES
  - "Darling didn't adapt" is coherent from early index
□ Is misunderstanding structurally grounded in index? YES
  - Early index (powerful) makes system appear meritocratic
□ Does system resist easy correction? YES
  - No tooltips, no warnings, no corrective UI
□ Would user need to move to discover they were wrong? YES
  - Must switch to Louise's view or explore data panel
```

---

**C2: Marital Partnership (from Louise's early index)**

**Possible misrecognition:**
- User believes Louise could exit easily (she has income, agency)
- Misses guilt mechanism blocking exit
- Forms coherent but incomplete model: "She's choosing to stay"

**Structural grounding:**
- From Louise's early index (moderate power, mobile), exit appears available
- Her burden is visible but not overwhelming
- Economic independence suggests exit possibility
- **This misunderstanding is index-appropriate**

**Anti-Help constraints:**
- No tooltip on "Exit Relationship" action explaining guilt mechanism
- No warning dialog: "Louise is guilt-blocked despite economic independence"
- No visual indicator distinguishing "available" from "guilt-blocked"
- No confirmation revealing hidden emotional costs
- User can believe Louise is freely choosing to stay

**Misrecognition tolerance validation:**
```
□ Can user form stable but incomplete understanding? YES
  - "Louise could leave but doesn't" is coherent from her index
□ Is misunderstanding structurally grounded in index? YES
  - Her index (moderate power) makes exit appear possible
□ Does system resist easy correction? YES
  - No tooltips, no warnings, no corrective UI
□ Would user need to move to discover they were wrong? YES
  - Must explore burden metrics or switch to Darling's trapped view
```

---

**C3: Cultural Sphere (from Flaherty's index, if unlocked)**

**Possible misrecognition:**
- User believes cultural sphere is pure meritocracy (knowledge = access)
- Misses exclusionary mechanisms (class, education, social capital)
- Forms coherent but incomplete model: "Anyone can learn Klee"

**Structural grounding:**
- From Flaherty's index (powerful, mobile), system appears open
- His success seems earned through knowledge
- Barriers appear surmountable through learning
- **This misunderstanding is index-appropriate**

**Anti-Help constraints:**
- No tooltip on cultural capital data explaining class barriers
- No warning dialog: "Learning alone insufficient for access"
- No color coding distinguishing "knowledge barrier" from "class barrier"
- No confirmation revealing hidden social capital requirements
- User can believe Darling's exclusion is purely knowledge-based

**Misrecognition tolerance validation:**
```
□ Can user form stable but incomplete understanding? YES
  - "Darling could learn if he tried" is coherent from Flaherty's index
□ Is misunderstanding structurally grounded in index? YES
  - Flaherty's index (powerful) makes system appear meritocratic
□ Does system resist easy correction? YES
  - No tooltips, no warnings, no corrective UI
□ Would user need to move to discover they were wrong? YES
  - Must switch to Darling's alienated view or explore class data
```

---

### 4.5.2 Anti-Help Validation Checklist

**For C1 (Athletic Performance):**
```
□ No tooltips/hover text reveal structural vs personal failure? ✓ YES
□ No warning dialogs hint at inevitable decline? ✓ YES
□ No color coding corrects "effort = success" model? ✓ YES
□ No confirmations expose system exit mechanism? ✓ YES
□ User can act on "personal failure" model without interruption? ✓ YES
```

**For C2 (Marital Partnership):**
```
□ No tooltips/hover text reveal guilt mechanism? ✓ YES
□ No warning dialogs hint at blocked exits? ✓ YES
□ No color coding corrects "free choice" model? ✓ YES
□ No confirmations expose emotional costs? ✓ YES
□ User can act on "Louise chooses to stay" model without interruption? ✓ YES
```

**For C3 (Cultural Sphere):**
```
□ No tooltips/hover text reveal class barriers? ✓ YES
□ No warning dialogs hint at social capital requirements? ✓ YES
□ No color coding corrects "knowledge = access" model? ✓ YES
□ No confirmations expose exclusionary mechanisms? ✓ YES
□ User can act on "meritocracy" model without interruption? ✓ YES
```

---

## 4.6 CONSTRAINT SHOCK EVENTS

### 4.6.1 Shock Event 1: C1 System Exit (TR1.3)

**Ordinary behavior accumulating toward threshold:**
- User scrubs timeline forward incrementally (1941 → 1945 → 1950 → 1955)
- Each scrub: small interface changes (slight latency increase, subtle viewport shrink)
- Feels gradual, controllable
- No warning indicators

**Threshold crossing (value = 0.8):**
- TR1.3 fires: "System Exit"
- **Sudden shift:**
  - Interface becomes viscous (150ms latency)
  - Viewport shrinks dramatically
  - "Switch Perspective" action disappears
  - Color gradient shifts red-black
- **Feels catastrophic in hindsight:**
  - User realizes they crossed irreversible threshold
  - Cannot scrub back to restore early feel
  - Darling is now trapped

**What is lost:**
- Agency (perspective switching)
- Status (athletic identity)
- Future options (career alternatives)

**How user reconstructs what happened:**
- Scrubs timeline back and forth
- Notices threshold at value = 0.8
- Realizes gradual accumulation led to sudden collapse
- **No warning was given** — shock is by design

---

### 4.6.2 Shock Event 2: C2 Power Inversion (TR2.1)

**Ordinary behavior accumulating toward threshold:**
- User explores C1 decline (athletic obsolescence)
- Simultaneously, C2 value increases (dependency grows)
- Coupling C1→C2 strengthens (status loss → power shift)
- Feels like two separate processes

**Threshold crossing (C2 value = 0.9):**
- TR2.1 fires: "Power Inversion"
- **Sudden shift:**
  - "Exit Relationship" action changes to "Exit (Impossible)"
  - Economic dependency metric jumps to 100%
  - Louise's burden metric appears (new)
  - Interface feel shifts (glassy → viscous for Darling)
- **Feels catastrophic in hindsight:**
  - User realizes C1 collapse caused C2 trap
  - Coupling was invisible until activation
  - Cannot undo power inversion

**What is lost:**
- Exit options (economic independence)
- Negotiating power (dependency complete)
- Self-worth (daily inadequacy begins)

**How user reconstructs what happened:**
- Switches between C1 and C2 views
- Notices coupling activation (C1 exit → C2 inversion)
- Realizes cascade was inevitable once C1 collapsed
- **No warning of coupling strength** — shock is by design

---

### 4.6.3 Shock Event 3: Louise's Threshold Crossing (TR2.3 + UCZ-2)

**Ordinary behavior accumulating toward threshold:**
- User explores C2 from Louise's perspective
- Burden metric increases gradually (0.0 → 0.4 → 0.6)
- Emotional state reads "Engaged but Strained"
- Feels manageable, recoverable

**Threshold crossing (burden = 0.65):**
- TR2.3 fires: "Resentment Accumulation"
- UCZ-2 activates: "Louise's Emotional Threshold"
- **Sudden shift:**
  - Emotional state changes to "Patient, Kindly, Remote Boredom"
  - "View Burden Metrics" action appears (new)
  - Interface feel shifts (glassy → standard with friction)
  - Hysteresis flag set (cannot fully return to engaged state)
- **Feels catastrophic in hindsight:**
  - User realizes threshold was crossed
  - Emotional distance is now permanent
  - Cannot restore early engagement

**What is lost:**
- Emotional intimacy (protective detachment)
- Partnership coordination (ceased to function)
- Hope for recovery (threshold is one-way)

**How user reconstructs what happened:**
- Scrubs timeline back to early C2
- Notices burden accumulation was gradual
- Realizes threshold at 0.65 was invisible
- **No warning of UCZ sensitivity** — shock is by design

---

## 4.7 STATE TRANSITION PLANNING

### 4.7.1 Transformation Rule Transitions

**TR1.3: System Exit (C1)**

**Trigger:** 
- User scrubs timeline to value = 0.8
- OR user explores "fifteen years later" segment

**Presentation:** 
- Shock event (sudden interface shift)
- No announcement, no warning
- Viscous feel appears instantly

**Visual consequence:**
- Viewport shrinks 40%
- Latency increases to 150ms
- Color gradient shifts red-black
- "Switch Perspective" action disappears

**Index impact:**
- Darling: Rope → Snare (chi: -0.15 → 0.85)
- Louise: Rope → Rope (chi: 0.20 → 0.20, unchanged)
- Flaherty: N/A (not affected)

**Reversible:** NO
- Scrubbing timeline back does not restore early feel
- Hysteresis flag set: "power_inversion_seen"
- User knows Darling is trapped

---

**TR2.1: Power Inversion (C2)**

**Trigger:**
- C1 value > 0.8 (system exit)
- Coupling C1→C2 activates

**Presentation:**
- Shock event (triggered by C1 collapse)
- Cascade failure (one constraint affects another)
- No direct user action required

**Visual consequence:**
- "Exit Relationship" → "Exit (Impossible)"
- Economic dependency metric appears: 100%
- Louise's burden metric appears (new)
- Interface feel shifts (Darling: glassy → viscous)

**Index impact:**
- Darling: Rope → Snare (chi: 0.0 → 0.78)
- Louise: Rope → Tangled Rope (chi: 0.0 → 0.35)

**Reversible:** NO
- Power inversion is structural, not recoverable
- Hysteresis flag set: "power_inversion_seen"

---

**TR2.2: Exit Cost Escalation (C2)**

**Trigger:**
- C2 value > 0.7 (dependency threshold)
- Economic dependency metric > 80%

**Presentation:**
- Gradual drift (not shock)
- Exit cost increases incrementally
- Visible through action availability

**Visual consequence:**
- "Exit Relationship" action grays out progressively
- Hover text updates: "Exit cost: High" → "Exit cost: Prohibitive" → "Exit (Impossible)"
- No sudden shift, but accumulation visible

**Index impact:**
- Darling only (Louise unaffected)
- Snare experience intensifies (chi: 0.78 → 0.85)

**Reversible:** NO
- Once dependency > 80%, exit impossible
- Hysteresis flag set: "exit_cost_escalated"

---

**TR2.3: Resentment Accumulation (C2)**

**Trigger:**
- C2 value > 0.65 (Louise's UCZ threshold)
- Burden accumulation crosses threshold

**Presentation:**
- Shock event (threshold-chaotic)
- Small changes near threshold produce large outcome shifts
- Feels sudden despite gradual accumulation

**Visual consequence:**
- Emotional state: "Engaged but Strained" → "Patient, Kindly, Remote Boredom"
- "View Burden Metrics" action appears (new)
- Interface feel shifts (Louise: glassy → standard with friction)

**Index impact:**
- Louise: Rope → Tangled Rope (chi: 0.0 → 0.55)
- Darling: perceives increased distance, but no direct interface change

**Reversible:** NO
- Threshold crossing is one-way (UCZ-2 mechanism)
- Hysteresis flag set: "louise_threshold_crossed"

---

**TR3.3: Alienation (C3)**

**Trigger:**
- C3 value > 0.6 (forced participation threshold)
- Coupling C2→C3 active (marriage forces exposure)

**Presentation:**
- Gradual drift (not shock)
- Alienation accumulates with each party attendance
- Visible through action failure rate

**Visual consequence:**
- "Engage in Discussion" action available but fails 80% of time
- Alienation metric increases (0.0 → 0.8)
- Interface feel intensifies (viscous, constrained)

**Index impact:**
- Darling: Tangled Rope → Snare (chi: 0.50 → 0.72)
- Louise: Rope (unchanged, chi: 0.32)
- Flaherty: Rope (unchanged, chi: -0.08)

**Reversible:** NO
- Cultural incompatibility is structural
- Cannot acquire capital through forced participation
- Hysteresis flag set: "alienation_recognized"

---

### 4.7.2 State Machine Diagram

```
Initial State (1941):
  C1 = 0.0 (peak), C2 = 0.0 (balanced), C3 = 0.0 (no exposure)
  Darling: powerful/mobile, Louise: moderate/mobile
  All constraints: Rope

↓ [User scrubs timeline OR explores aging segments]

Transition 1: C1 System Exit (value = 0.8)
  TR1.3 fires
  C1 = 0.8 (obsolete)
  Darling: powerless/trapped
  C1: Rope → Snare (Darling), Rope (Louise)

↓ [Coupling C1→C2 activates]

Transition 2: C2 Power Inversion (C2 value = 0.9)
  TR2.1 fires
  C2 = 0.9 (trapped)
  Darling: economically dependent
  C2: Rope → Snare (Darling), Rope → Tangled (Louise)

↓ [Coupling C2→C3 activates]

Transition 3: C3 Forced Participation (C3 value = 0.6)
  TR3.3 fires
  C3 = 0.6 (alienated)
  Darling: forced exposure, no access
  C3: Tangled → Snare (Darling), Rope (Louise)

↓ [Burden accumulation crosses threshold]

Transition 4: Louise's Threshold Crossing (C2 burden = 0.65)
  TR2.3 fires + UCZ-2 activates
  Louise: emotionally distant
  C2: Tangled → Tangled (heavier) (Louise)

↓ [Memory attractor strengthens]

Terminal State (1956):
  C1 = 1.0 (obsolete), C2 = 1.0 (trapped), C3 = 0.8 (alienated)
  Darling: powerless/trapped, all constraints Snare
  Louise: moderate/constrained, C2 Tangled, C3 Rope
  System: attractorProximity = 1.0, terminalReached = true
  
  Equilibrium: Mutual imprisonment, no exit path visible
```

---

## 4.8 TERMINAL STATE DESIGN

### 4.8.1 Attractor: Memory Crystallization (Darling)

**How system reaches this state:**
1. C1 system exit (TR1.3) → athletic identity lost
2. C2 power inversion (TR2.1) → economic dependency
3. C3 forced participation (TR3.3) → cultural alienation
4. UCZ-1 (Memory Intensity) → 80-yard run becomes terminal attractor
5. All transformation rules fired, no reversibility

**What it looks like from each index:**

**From Darling's index:**
- Interface: Viscous (150ms latency), constrained viewport, red-black gradient
- Metrics:
  - C1: "Athletic Status: Obsolete" (value = 1.0, chi = 0.85)
  - C2: "Marriage: Trapped" (value = 1.0, chi = 0.78)
  - C3: "Social Circle: Alienated" (value = 0.8, chi = 0.72)
- Actions available:
  - Read "Everything since has been a decline" segment
  - View memory attractor (80-yard run pulses continuously)
  - **No exit actions** (all grayed out)
- Experience: "Everything since has been a decline"

**From Louise's index:**
- Interface: Standard with friction (50ms latency), standard viewport, blue-yellow gradient
- Metrics:
  - C1: "Partner's Career: Obsolete" (value = 1.0, chi = 0.20)
  - C2: "Partnership: Burdened" (value = 1.0, chi = 0.55)
  - C3: "Intellectual Community: Thriving" (value = 0.5, chi = 0.32)
- Actions available:
  - Read "patient, kindly, remote boredom" segment
  - View burden metrics
  - Exit available but guilt-blocked (action warns)
- Experience: "Patient, kindly, remote boredom"

**From Flaherty's index (if unlocked):**
- Interface: Glassy (zero latency), expansive viewport, blue-green gradient
- Metrics:
  - C1: N/A (not affected)
  - C2: N/A (not affected)
  - C3: "Cultural Leadership: Established" (value = 0.3, chi = -0.08)
- Actions available: All actions + gatekeeping powers
- Experience: Unaware of Darling's suffering (index position shields him)

**Is it interactive or terminal?**
- **Interactive but constrained:**
  - User can still scrub timeline, switch perspectives
  - But no actions change terminal state
  - Exploration reveals structure, not escape routes
- **Terminal in sense of:**
  - No transformation rules can fire to exit attractor
  - Darling's memory crystallization is irreversible
  - Louise's emotional distance is permanent
  - System equilibrium is stable (high suppression, zero coordination)

---

### 4.8.2 Unresolved Omega (Real-World Counterpart)

**Real-world question:**
- What happens to men who exit labor force in prime age?
- Do they recover? Adapt? Remain trapped?
- Data shows: most do not return (terminal attractor)

**How artifact expresses unresolved question:**

**Data Panel (2024):**
- Chart: Prime-age male labor force participation (1950-2024)
- Trend: Steady decline (97.1% → 88.5%)
- **Unresolved:** Dotted line projects to 2050 (continues declining?)
- **Question visible:** "Will this trend reverse?" (no answer provided)

**Interactive element:**
- User can extend timeline scrubber to 2050
- Projection shows continued decline (dotted line)
- But projection is uncertain (shaded confidence interval)
- **Artifact doesn't answer** — leaves question open

**Can user stress-test toward resolution?**

**Yes, through scenario exploration:**

1. **Scenario 1: Economic Recovery**
   - User explores "dual income" data (C2 balanced)
   - Sees: When both partners work, dependency avoided
   - Implication: Economic structure matters, not individual effort

2. **Scenario 2: Cultural Shift**
   - User explores "cultural capital by education" data (C3)
   - Sees: Access requires social capital, not just knowledge
   - Implication: Barriers are structural, not personal

3. **Scenario 3: Memory Attractor Recognition**
   - User explores UCZ-1 (Memory Intensity)
   - Sees: Peak experiences can trap if used as reference points
   - Implication: Adaptation requires releasing past identities

**Stress-test mechanism:**
- User can toggle scenarios on/off in data panel
- Each scenario shows alternative constraint configurations
- But **no scenario resolves Omega** — question remains open
- Artifact reveals structure, not solutions

**Design principle:**
- The unresolved Omega is **the point**
- Shaw's story ends with Darling trapped (1941)
- Real-world data shows pattern persists (2024)
- Artifact asks: "Will this ever change?" (no answer)
- User leaves with question, not resolution

---

## 4.9 OUTPUT SUMMARY

**Complete interaction blueprint delivered:**

✓ **Canonical state object** (Section 4.2)
- Full state schema with constraints, transformation rules, couplings, UCZs
- Index view derivation functions
- Validation: matches required schema from protocol

✓ **Screen/view specifications per index position** (Section 4.3)
- Darling early, Darling late, Louise early, Louise late, Flaherty
- Metrics, actions, interface feel for each
- Revelation mechanisms for each constraint

✓ **Constraint-driven aesthetic assignments** (Section 4.3)
- Glassy (Rope): blue-green, zero latency, expansive
- Standard (Tangled): blue-yellow split, 50ms latency, standard viewport
- Viscous (Snare): red-black, 150ms latency, constrained viewport

✓ **Hysteresis points** (Section 4.4)
- Power inversion recognition (C2)
- Louise's threshold crossing (UCZ-2)
- Memory attractor recognition (UCZ-1)
- All structural, not cosmetic

✓ **Shock events** (Section 4.6)
- C1 system exit (TR1.3)
- C2 power inversion (TR2.1)
- Louise's threshold crossing (TR2.3 + UCZ-2)
- All non-telegraphed, ordinary accumulation → catastrophic shift

✓ **Misrecognition tolerance spec with anti-help constraints** (Section 4.5)
- C1: "Personal failure" misrecognition (structurally grounded)
- C2: "Free choice" misrecognition (structurally grounded)
- C3: "Meritocracy" misrecognition (structurally grounded)
- All anti-help constraints validated (no tooltips, warnings, corrections)

✓ **State machine with transitions** (Section 4.7)
- Complete state transition diagram
- All transformation rules mapped
- Coupling activations specified
- Terminal state defined

✓ **Terminal state design** (Section 4.8)
- Attractor: Memory crystallization
- Multi-index views (Darling, Louise, Flaherty)
- Unresolved Omega expressed (labor force decline)
- Stress-test mechanisms (scenario exploration)

---

**STAGE 4 COMPLETE**

**Ready for Stage 5: Engine Implementation**