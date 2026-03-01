# STAGE 4: INTERACTION DESIGN & INDEXICAL REVELATION

**Role:** The Choreographer  
**Model:** Claude (demonstrative, narrative-forward)

---

## I. CANONICAL STATE OBJECT

```javascript
const canonicalState = {
  constraints: {
    C1: {
      value: 0.0,              // Current extraction level (0.0 = none, 1.0 = complete)
      epsilon: 0.80,           // ε from Stage 1
      chi: null,               // Calculated per index (see indexViews)
      support: 0.40,           // Supp from Stage 1
      type: "snare",           // Structural type (not shown to user)
      phase: "pre_TR2",        // "pre_TR2" | "terminal"
      ucz: {
        mechanism: "index_dependent",
        params: {
          advicePool: ["both_walk", "miller_rides", "son_rides", "both_ride", "carry_ass"],
          previousAdvice: [],
          contradictionRequired: true
        }
      }
    }
  },
  
  transformationRules: {
    TR1: {
      id: "compliance_escalation",
      fired: 0,                // Count of times fired
      progress: 0.0,           // 0.0–1.0 toward next trigger
      threshold: 1.0,          // Fires every time new advice encountered
      reversible: false,       // Confusion accumulates irreversibly
      lastFired: null          // Timestamp
    },
    TR2: {
      id: "terminal_extraction",
      fired: false,            // Boolean (can only fire once)
      progress: 0.0,           // 0.0–1.0 toward agency threshold
      threshold: 0.2,          // Miller.agency ≤ 0.2
      reversible: false,       // Total loss, no recovery
      lastFired: null
    },
    TR3: {
      id: "onlooker_refresh",
      fired: 0,                // Count of location changes
      progress: 0.0,           // 0.0–1.0 toward next location
      threshold: 1.0,          // Fires on user action (move forward)
      reversible: true,        // Can move to new locations repeatedly
      lastFired: null
    }
  },
  
  couplings: {
    C1_1: {
      id: "advice_to_compliance",
      source: "onlookers.advice",
      target: "miller.action",
      strength: 0.9,
      direction: "unidirectional",
      active: true,
      fireCount: 0
    },
    C1_2: {
      id: "compliance_to_confusion",
      source: "miller.action",
      target: "miller.confusion",
      strength: 0.7,
      direction: "unidirectional",
      active: true,
      fireCount: 0
    },
    C1_3: {
      id: "confusion_to_agency",
      source: "miller.confusion",
      target: "miller.agency",
      strength: 0.8,
      direction: "unidirectional",
      active: true,
      fireCount: 0
    },
    C1_4: {
      id: "agency_to_property",
      source: "miller.agency",
      target: "miller.property",
      strength: 1.0,
      direction: "unidirectional",
      active: false,           // Activates only at threshold
      fireCount: 0
    },
    C1_5: {
      id: "location_to_advice",
      source: "miller.location",
      target: "onlookers.advice",
      strength: 1.0,
      direction: "unidirectional",
      active: true,
      fireCount: 0
    }
  },
  
  system: {
    attractorProximity: 0.0,     // 0.0–1.0 (distance to terminal state)
    hysteresisFlags: {
      perspective_shift_occurred: false,
      onlooker_view_seen: false,
      bridge_activated: false,
      structural_view_accessed: false
    },
    terminalReached: false,
    cycleCount: 0,
    startTime: null,
    currentIndex: "miller"        // "miller" | "onlooker"
  },
  
  // Agent states (derived from couplings but stored for performance)
  agents: {
    miller: {
      agency: 1.0,              // [0, 1] - depletes via C1_3
      confusion: 0.0,           // [0, ∞) - accumulates via C1_2
      property: 1.0,            // {0, 1} - binary (has Ass or not)
      dignity: 1.0,             // [0, 1] - cosmetic (for narrative)
      location: "start",        // string - triggers TR3 via C1_5
      action: null,             // Current compliance action
      actionHistory: []         // Array of past actions
    },
    son: {
      agency: 0.0,              // Always 0 (no independent action)
      dignity: 1.0,             // [0, 1] - affected by miller's choices
      visible: true             // Can be hidden in some configurations
    },
    onlookers: {
      currentAdvice: null,      // Current demand
      previousAdvice: [],       // History (for UCZ[1])
      satisfaction: 0.0,        // [0, 1] - increases with compliance
      groupId: 0,               // Which group (increments with location)
      groupSize: 0              // Random 2-5 per group
    },
    user: {
      // Social media side (parallel to miller)
      anxiety: 0.0,             // [0, 1] - inverse of control
      control: 1.0,             // [0, 1] - parallel to agency
      reputation: 1.0,          // [0, 1] - parallel to dignity
      access: 1.0,              // {0, 1} - parallel to property
      postCount: 0,             // Parallel to location changes
      engagement: 0.0,          // Parallel to onlooker satisfaction
      commentHistory: []        // Parallel to advice history
    }
  }
};
```

---

## II. INDEX VIEW DERIVATION FUNCTIONS

```javascript
// Index views are READ FUNCTIONS of canonical state
// They do NOT store separate data

function deriveIndexView(canonicalState, indexPosition) {
  const { constraints, agents, system } = canonicalState;
  
  if (indexPosition === "miller") {
    return {
      // MILLER/USER INDEX (Cost-Bearer, Snare ⊠)
      indexParams: {
        power: "powerless",
        scale: "immediate",
        position: "trapped",
        scope: "local"
      },
      chi: 0.80 * 1.5 * 0.8, // 0.96 (Snare)
      
      metrics: {
        // Same values, cost-bearer framing
        agency: {
          value: agents.miller.agency,
          label: "Control",
          color: agents.miller.agency > 0.5 ? "green" : "red",
          visible: true,
          format: "percentage"
        },
        confusion: {
          value: agents.miller.confusion,
          label: "Anxiety",
          color: "orange",
          visible: true,
          format: "bar"
        },
        property: {
          value: agents.miller.property,
          label: agents.miller.property === 1 ? "Account Active" : "Account Suspended",
          color: agents.miller.property === 1 ? "green" : "red",
          visible: true,
          format: "status"
        },
        dignity: {
          value: agents.miller.dignity,
          label: "Reputation",
          color: agents.miller.dignity > 0.5 ? "blue" : "gray",
          visible: true,
          format: "percentage"
        }
      },
      
      interfaceFeel: {
        latency: 200 + (agents.miller.confusion * 600), // 200ms–800ms
        opacity: 1.0 - (agents.miller.confusion * 0.3), // 1.0–0.7
        scrollFriction: agents.miller.confusion * 0.8,  // 0.0–0.8
        viewportScale: agents.miller.agency,            // 1.0–0.0 (constricts)
        buttonScale: 1.0 - (system.cycleCount * 0.02)   // Shrinks with use
      },
      
      availableActions: [
        {
          id: "comply",
          label: "Accept Feedback",
          enabled: agents.miller.agency > 0.2,
          cost: "Increases anxiety, depletes control",
          visible: true
        },
        {
          id: "resist",
          label: "Ignore Comments",
          enabled: agents.miller.agency > 0.5, // Only available early
          cost: "High anxiety spike, onlookers multiply",
          visible: agents.miller.agency > 0.5
        },
        {
          id: "exit",
          label: "Delete Account",
          enabled: false, // Never available (trap characteristic)
          cost: "N/A",
          visible: false
        }
      ],
      
      narrativeFrame: "You are trying to satisfy everyone.",
      experiencedType: "inescapable_trap" // Never labeled "Snare"
    };
    
  } else if (indexPosition === "onlooker") {
    return {
      // ONLOOKER/COMMENTER INDEX (Beneficiary, Rope ⊞)
      indexParams: {
        power: "institutional",
        scale: "immediate",
        position: "arbitrage",
        scope: "local"
      },
      chi: 0.80 * -0.2 * 0.8, // -0.128 (Rope)
      
      metrics: {
        // Same values, beneficiary framing
        satisfaction: {
          value: agents.onlookers.satisfaction,
          label: "Engagement",
          color: "purple",
          visible: true,
          format: "percentage"
        },
        influence: {
          value: system.cycleCount / 6, // Normalized to max expected cycles
          label: "Impact",
          color: "blue",
          visible: true,
          format: "bar"
        },
        entertainment: {
          value: agents.miller.confusion * 0.5, // Derived from miller's pain
          label: "Thread Quality",
          color: "green",
          visible: true,
          format: "stars"
        }
      },
      
      interfaceFeel: {
        latency: 100,              // Fast, glassy
        opacity: 1.0,              // No degradation
        scrollFriction: 0.0,       // Smooth
        viewportScale: 1.5,        // Expansive (see more)
        buttonScale: 1.0           // No shrinking
      },
      
      availableActions: [
        {
          id: "advise",
          label: "Leave Comment",
          enabled: true,
          cost: "None",
          visible: true
        },
        {
          id: "pile_on",
          label: "Quote Tweet",
          enabled: true,
          cost: "None",
          visible: true
        },
        {
          id: "move_on",
          label: "Next Thread",
          enabled: true,
          cost: "None",
          visible: true
        }
      ],
      
      narrativeFrame: "You are helping correct bad behavior.",
      experiencedType: "coordination_tool" // Never labeled "Rope"
    };
  }
}

// Hysteresis state tracking
function applyHysteresis(canonicalState, hysteresisType) {
  const hysteresisEffects = {
    perspective_shift: {
      // After seeing onlooker view, miller view changes
      millerMetrics: {
        // "Control" now shows ghost of "Engagement" value
        agency: {
          ghostValue: canonicalState.agents.onlookers.satisfaction,
          ghostLabel: "Their Engagement",
          ghostVisible: true
        }
      },
      millerActions: {
        // "Accept Feedback" now shows consequence
        comply: {
          costAddendum: " (Increases their engagement)",
          colorShift: "red"
        }
      }
    },
    
    bridge_activation: {
      // After seeing structural view, both indices show couplings
      millerMetrics: {
        confusion: {
          couplingVisible: true,
          couplingTarget: "agency",
          couplingStrength: 0.8
        }
      },
      onlookerMetrics: {
        satisfaction: {
          couplingVisible: true,
          couplingSource: "miller.confusion",
          couplingStrength: 0.7
        }
      }
    }
  };
  
  return hysteresisEffects[hysteresisType];
}
```

---

## III. INTERACTION MAPPING (EVERY CONSTRAINT, EVERY INDEX)

### C[1]: The Judgment of Onlookers

#### From Miller/User Index (Snare ⊠)

**Visible as:** Social pressure system (left panel: onlooker groups, right panel: comment threads)

**Aesthetic signature:**
- Viscous input (lag increases with confusion)
- Constricting viewport (tunnel vision as agency depletes)
- Accumulating friction (each compliance action becomes harder)
- Muted colors (saturation decreases with dignity loss)

**Metrics displayed:**

*Left Panel (Fable):*
```javascript
{
  "Control": agents.miller.agency,        // 0–100%
  "Anxiety": agents.miller.confusion,     // Bar graph
  "Status": agents.miller.property,       // "Has Ass" | "Lost Ass"
  "Reputation": agents.miller.dignity     // 0–100%
}
```

*Right Panel (Social Media):*
```javascript
{
  "Control": agents.user.control,         // 0–100%
  "Anxiety": agents.user.anxiety,         // Bar graph
  "Status": agents.user.access,           // "Active" | "Suspended"
  "Reputation": agents.user.reputation    // 0–100%
}
```

**Interface feel:**
```javascript
{
  latency: 200 + (confusion * 600),       // 200ms–800ms
  opacity: 1.0 - (confusion * 0.3),       // 1.0–0.7
  scrollSpeed: 1.0 - (confusion * 0.8),   // 1.0–0.2
  viewportWidth: agency * 100 + "%",      // 100%–20%
  buttonSize: 1.0 - (cycleCount * 0.02)   // 1.0–0.88 (at 6 cycles)
}
```

**Available actions:**

*Comply Button:*
- Label: "Accept Feedback" (social media) | "Follow Advice" (fable)
- Enabled: `agency > 0.2`
- Effect: Fires TR1 (Compliance Escalation)
- Cost tooltip: "Anxiety +30%, Control -20%"
- Visual: Button shrinks 2% per use, transition delay increases 100ms per use

*Resist Button:*
- Label: "Ignore Comments" | "Continue Current Path"
- Enabled: `agency > 0.5` (only available early)
- Effect: `confusion += 0.5`, onlooker count doubles
- Cost tooltip: "Anxiety +50%, Onlookers multiply"
- Visual: Red warning border, shake animation

*Exit Button:*
- Label: "Delete Account" | "Abandon Journey"
- Enabled: `false` (NEVER available — Snare characteristic)
- Effect: N/A
- Visual: Grayed out, no hover state, no tooltip

**Experienced type:** Inescapable trap (never labeled "Snare")

---

#### From Onlooker/Commenter Index (Rope ⊞)

**Visible as:** Social coordination tool (left panel: helpful advice-giving, right panel: community moderation)

**Aesthetic signature:**
- Glassy interface (low latency, smooth)
- Expansive viewport (can see multiple threads/groups)
- No accumulating costs
- Bright, saturated colors

**Metrics displayed:**

*Left Panel (Fable):*
```javascript
{
  "Engagement": agents.onlookers.satisfaction,     // 0–100%
  "Impact": cycleCount / 6,                        // 0–100%
  "Thread Quality": miller.confusion * 0.5         // 1–5 stars
}
```

*Right Panel (Social Media):*
```javascript
{
  "Engagement": user.engagement,                   // 0–100%
  "Reach": commentHistory.length,                  // Count
  "Thread Quality": user.anxiety * 0.5             // 1–5 stars
}
```

**Interface feel:**
```javascript
{
  latency: 100,                           // Fast, no degradation
  opacity: 1.0,                           // Always crisp
  scrollSpeed: 1.0,                       // Smooth
  viewportWidth: "150%",                  // Expansive (see more context)
  buttonSize: 1.0                         // No shrinking
}
```

**Available actions:**

*Advise Button:*
- Label: "Leave Comment" | "Offer Advice"
- Enabled: `true` (always)
- Effect: Generates new advice (UCZ[1]), fires C1_1 coupling
- Cost tooltip: "None"
- Visual: Smooth hover, satisfying click animation

*Pile On Button:*
- Label: "Quote Tweet" | "Emphasize Point"
- Enabled: `true`
- Effect: `onlookers.satisfaction += 0.2`, `miller.confusion += 0.3`
- Cost tooltip: "None"
- Visual: Purple highlight, "viral" animation

*Move On Button:*
- Label: "Next Thread" | "New Group"
- Enabled: `true`
- Effect: Fires TR3 (Onlooker Refresh)
- Cost tooltip: "None"
- Visual: Directional arrow, smooth transition

**Experienced type:** Coordination tool (never labeled "Rope")

---

## IV. HYSTERESIS POINT SPECIFICATION

### Hysteresis Point 1: The Perspective Shift (STRUCTURAL)

**Trigger:** User clicks "Switch Perspective" button (subtle, not prominent)

**Before shift (Miller index only):**
- User sees "Control" metric, believes they have agency
- "Accept Feedback" button seems like reasonable choice
- Confusion/anxiety feels like personal failing
- No visibility into onlooker motivations

**During shift to Onlooker index:**
- Same events, different metrics appear
- "Engagement" replaces "Control" — same number, different meaning
- "Thread Quality" metric appears — derived from Miller's confusion
- User can now click "Leave Comment" — sees it generates advice Miller must follow

**Return to Miller index (PERMANENT CHANGE):**

*What changes (not cosmetic):*

1. **Metric Trustworthiness Degraded:**
```javascript
millerMetrics.agency = {
  value: agents.miller.agency,
  label: "Control",
  ghostValue: agents.onlookers.satisfaction,
  ghostLabel: "(Their Engagement: " + ghostValue + "%)",
  ghostVisible: true,  // PERMANENT
  color: "yellow"      // Was green, now suspect
}
```

2. **Action Consequences Revealed:**
```javascript
complyButton = {
  label: "Accept Feedback",
  enabled: agents.miller.agency > 0.2,
  cost: "Anxiety +30%, Control -20%",
  costAddendum: " (Increases their engagement by 10%)", // NEW, PERMANENT
  colorShift: "red"    // Was blue
}
```

3. **Information Latency Introduced:**
```javascript
// Before shift: metrics update instantly
// After shift: 500ms delay on all miller metrics
// User now suspects data freshness
metricUpdateDelay = 500; // PERMANENT
```

**What CANNOT be undone:**
- User now knows "Control" and "Engagement" are the same number
- User knows their suffering entertains onlookers
- User knows "Accept Feedback" button increases onlooker satisfaction
- User suspects all metrics may have hidden meanings

**Validation:**
- ✅ Changes what user BELIEVES (metrics are suspect)
- ✅ Changes what user CAN DO (cost tooltip reveals extraction)
- ✅ Changes what user TRUSTS (data latency introduced)
- ✅ NOT just visual overlay (structural information revealed)

---

### Hysteresis Point 2: The Bridge Activation (STRUCTURAL)

**Trigger:** User clicks "See the Pattern" button OR reaches 3+ compliance cycles

**Before activation:**
- Two panels seem separate (fable vs. social media)
- Metrics move independently
- No visible connection between actions

**During activation:**
- Constraint network overlay appears
- Couplings become visible as animated lines
- User can click couplings to see them fire

**After activation (PERMANENT CHANGE):**

*What changes:*

1. **Coupling Visibility:**
```javascript
// All couplings now visible in both panels
millerMetrics.confusion = {
  value: agents.miller.confusion,
  label: "Anxiety",
  couplingVisible: true,           // PERMANENT
  couplingTarget: "agency",
  couplingStrength: 0.8,
  couplingAnimation: "pulse"       // Shows when coupling fires
}
```

2. **Causal Chain Tracing:**
```javascript
// User can now click any metric to see what affects it
// Reveals full causal structure
availableActions.push({
  id: "trace_coupling",
  label: "Why is this changing?",
  enabled: true,                   // PERMANENT
  effect: "Highlights causal chain"
});
```

3. **Structural Revelation:**
```javascript
// User sees that fable and social media are SAME STRUCTURE
bridgeOverlay = {
  visible: true,                   // PERMANENT
  highlightMode: "synchronized",   // Both panels pulse together
  structuralLabel: "Same constraint physics"
};
```

**What CANNOT be undone:**
- User now sees both systems as instances of same structure
- User knows couplings drive the cascade
- User can trace causality (advice → compliance → confusion → depletion)
- User cannot "unsee" the structural echo

**Validation:**
- ✅ Changes what user CAN DO (can trace couplings)
- ✅ Changes what user BELIEVES (systems are structurally identical)
- ✅ Changes what user SEES (causal chains visible)
- ✅ NOT just cosmetic (reveals hidden structural layer)

---

## V. MISRECOGNITION TOLERANCE & ANTI-HELP CONSTRAINTS

### Misrecognition-Tolerant Constraint: C[1] from Onlooker Index

**Stable but incomplete understanding:**

User in Onlooker index may form coherent model:
- "I am helping correct bad behavior"
- "My comments are constructive feedback"
- "The Miller/User is making poor choices"
- "My engagement is positive coordination"

**This model is:**
- ✅ Internally consistent
- ✅ Structurally grounded in their index position
- ✅ Wrong (they are extracting, not coordinating)
- ✅ Discoverable only by perspective shift

**Anti-Help Rules for Onlooker Index:**

```javascript
// NO tooltips revealing alternative interpretations
onlookerActions.advise = {
  label: "Leave Comment",
  tooltip: "Share your perspective",  // NOT "This will increase Miller's anxiety"
  warningDialog: null,                // NO warning about consequences
  confirmationDialog: null            // NO "are you sure?"
};

// NO color coding revealing extraction
onlookerMetrics.satisfaction = {
  label: "Engagement",
  color: "purple",                    // NOT red (no negative framing)
  derivedFrom: "miller.confusion",    // HIDDEN (not shown)
  extractionIndicator: null           // NO icon showing cost-bearing
};

// NO corrective UI affordances
onlookerInterface = {
  millerPainVisible: false,           // Cannot see Miller's metrics
  couplingVisible: false,             // Cannot see causal chain (until bridge)
  switchPerspectivePrompt: false,     // NO nudge to switch
  structuralHint: null                // NO "you might be wrong" messaging
};

// NO progress bars guiding toward "correct" understanding
onlookerTutorial = null;              // NO tutorial
onlookerHelpText = null;              // NO help text
onlookerOnboarding = null;            // NO onboarding flow
```

**Design courage validation:**
*Would you be comfortable letting a smart user leave with the wrong conclusion?*

**Answer: YES.**

A user could complete entire interaction in Onlooker index believing:
- They were helpful
- Their engagement was positive
- The Miller/User deserved criticism
- The system was working correctly

This is NOT a bug. This is the INDEX POSITION working as designed.

The misunderstanding is structurally grounded:
- From (institutional, immediate, arbitrage, local), extraction IS coordination
- The user experiences zero cost
- Their metrics show positive feedback
- The interface is smooth and satisfying

**Discovery path:**
- User must CHOOSE to switch perspectives (button is subtle, not prominent)
- OR reach terminal state and wonder why Miller lost everything
- OR activate bridge and see structural echo

**Anti-Help Validation Checklist:**

For C[1] from Onlooker index:
- ✅ No tooltips reveal alternative interpretations
- ✅ No warning dialogs hint at hidden outcomes
- ✅ No color coding corrects user's model
- ✅ No confirmations expose information not in current index
- ✅ User can act on wrong-but-coherent model without interruption

---

## VI. SHOCK EVENTS (MAPPED FROM TRANSFORMATION RULES)

### Shock Event 1: Terminal Extraction (TR2)

**Transformation rule:**
```javascript
TR2: {
  id: "terminal_extraction",
  trigger: "miller.agency ≤ 0.2",
  effect: "property = 0, dignity = 0, agency = 0, system → TERMINAL"
}
```

**Shock design: Ordinary behavior, catastrophic hindsight**

**Accumulation (feels ordinary):**
- User clicks "Accept Feedback" 4-6 times
- Each click feels reasonable (satisfying onlookers)
- Anxiety metric rises gradually (seems manageable)
- Control metric drops slowly (user thinks they can recover)
- No warning threshold (no "danger zone" color change)
- No countdown (no "3 more clicks until disaster")

**Trigger (feels sudden):**
- User clicks "Accept Feedback" one more time
- Control metric crosses 0.2 threshold
- TR2 fires immediately

**Presentation:**
```javascript
// NO telegraphing
// NO "are you sure?" dialog
// NO warning animation
// NO countdown timer

// IMMEDIATE consequences:
{
  millerPanel: {
    animation: "fade_to_black",
    duration: 2000,
    finalState: {
      property: 0,
      dignity: 0,
      agency: 0,
      statusText: "Account Suspended"  // OR "Ass Lost in River"
    }
  },
  socialMediaPanel: {
    animation: "fade_to_black",       // SYNCHRONIZED
    duration: 2000,
    finalState: {
      access: 0,
      reputation: 0,
      control: 0,
      statusText: "Account Suspended"
    }
  },
  bridgeOverlay: {
    visible: true,
    text: "TERMINAL STATE REACHED",
    subtext: "Same attractor, two contexts"
  }
}
```

**Reconstruction (user figures out what happened):**

After terminal state, user can:
1. Scroll back through action history
2. See coupling visualizations (if bridge activated)
3. Replay from onlooker perspective
4. Trace causal chain: advice → compliance → confusion → depletion → loss

**What was lost:**
- Property (Ass/Account)
- Dignity (Reputation)
- Agency (Control)
- Ability to interact (system locked)

**Irreversibility:**
- No "undo" button
- No "restart from checkpoint"
- No "recover account" option
- Only option: "Restart" (full reset)

---

### Shock Event 2: Onlooker Multiplication (TR1 + Resistance)

**Transformation rule:**
```javascript
TR1: {
  id: "compliance_escalation",
  trigger: "new advice encountered",
  effect: "agency -0.2, confusion +0.3, satisfaction +0.1"
}
```

**Shock design: Resistance backfires**

**Setup (feels empowering):**
- User has "Ignore Comments" button (only when agency > 0.5)
- Button labeled as resistance option
- User thinks: "I'll break the cycle"

**Trigger (user clicks "Ignore Comments"):**
```javascript
resistAction = {
  immediate: {
    confusion: +0.5,              // Spike (vs. +0.3 for comply)
    agency: -0.3,                 // Larger drop (vs. -0.2)
    onlookerCount: *= 2           // DOUBLES
  },
  delayed: {
    newAdvice: "even more contradictory",
    adviceIntensity: *= 1.5,
    satisfactionGain: *= 2        // Onlookers MORE satisfied
  }
}
```

**Presentation:**
```javascript
// Immediate visual feedback:
{
  onlookerPanel: {
    animation: "multiply",
    newGroups: onlookerCount * 2,
    speechBubbles: [
      "How dare you ignore us!",
      "This is exactly the problem!",
      "See? Proves our point!"
    ]
  },
  millerMetrics: {
    confusion: {
      animation: "spike",
      colorShift: "red",
      value: +0.5
    },
    agency: {
      animation: "plummet",
      value: -0.3
    }
  }
}
```

**Catastrophic hindsight:**
- User realizes resistance ACCELERATED collapse
- Onlookers used resistance as proof of bad faith
- Confusion spiked higher than compliance would have caused
- Agency depleted faster
- User is now CLOSER to terminal threshold
- Resistance option disappears (agency now ≤ 0.5)

**Reconstruction:**
- User sees that Snare constraints punish resistance
- Compliance was "less bad" option (still bad)
- Only winning move was not to play (but exit unavailable)
- Structural trap confirmed

---

## VII. STATE TRANSITION PLANNING

### TR1: Compliance Escalation

**Trigger:** User encounters new advice (clicks "Accept Feedback" OR onlookers refresh)

**Precondition:** `system.state === "ACTIVE"` AND `miller.agency > 0.2`

**Presentation:** Gradual drift (not announced)

**Visual consequence:**

*Miller Panel:*
```javascript
{
  metrics: {
    agency: -0.2,                 // Smooth depletion animation
    confusion: +0.3,              // Bar fills gradually
    dignity: -0.1                 // Subtle fade
  },
  interface: {
    latency: +100,                // Accumulating lag
    opacity: -0.05,               // Slight dimming
    buttonScale: -0.02            // Button shrinks
  },
  viewport: {
    width: agency * 100 + "%"     // Constricts
  }
}
```

*Social Media Panel:*
```javascript
{
  metrics: {
    control: -0.2,
    anxiety: +0.3,
    reputation: -0.1
  },
  interface: {
    // Same as Miller panel (synchronized)
  }
}
```

**Index impact:**
- Miller index: Feels increasingly trapped
- Onlooker index: Satisfaction increases, no visible cost

**Reversible:** NO (confusion accumulates irreversibly)

---

### TR2: Terminal Extraction

**Trigger:** `miller.agency ≤ 0.2`

**Precondition:** User has complied enough times to deplete agency

**Presentation:** Shock event (see Section VI)

**Visual consequence:**

*Both Panels:*
```javascript
{
  animation: "synchronized_collapse",
  duration: 2000,
  stages: [
    {
      time: 0,
      effect: "screen_shake",
      intensity: "high"
    },
    {
      time: 500,
      effect: "color_drain",
      finalColor: "grayscale"
    },
    {
      time: 1000,
      effect: "viewport_collapse",
      finalWidth: "0%"
    },
    {
      time: 1500,
      effect: "fade_to_black",
      finalOpacity: 0
    },
    {
      time: 2000,
      effect: "terminal_message",
      text: "ACCOUNT SUSPENDED" // OR "ASS LOST"
    }
  ]
}
```

**Index impact:**
- Miller index: Total loss, system locked
- Onlooker index: High satisfaction, thread "resolved"
- Bridge: Synchronized terminal state visible

**Reversible:** NO (only option is full restart)

---

### TR3: Onlooker Refresh

**Trigger:** User moves to new location (clicks "Continue" OR "Next Thread")

**Precondition:** `system.state === "ACTIVE"`

**Presentation:** Visible announcement (new group appears)

**Visual consequence:**

*Miller Panel:*
```javascript
{
  location: "new_location_" + cycleCount,
  onlookers: {
    groupId: groupId + 1,
    groupSize: Math.floor(Math.random() * 4) + 2, // 2-5
    currentAdvice: adviceGenerator(previousAdvice),
    speechBubbles: [
      // New contradictory advice
    ]
  },
  animation: "group_entrance",
  duration: 1000
}
```

*Social Media Panel:*
```javascript
{
  postCount: postCount + 1,
  commenters: {
    newThread: true,
    commentCount: Math.floor(Math.random() * 4) + 2,
    comments: [
      // New contradictory demands
    ]
  },
  animation: "thread_refresh",
  duration: 1000
}
```

**Index impact:**
- Miller index: New advice contradicts previous, confusion increases
- Onlooker index: New engagement opportunity, satisfaction maintained

**Reversible:** YES (can move to new locations repeatedly, but confusion persists)

---

## VIII. TERMINAL STATE DESIGN

### Attractor: Total Extraction

**How system reaches this state:**
1. User complies with contradictory advice 4-6 times (TR1 fires repeatedly)
2. Confusion accumulates via C1_2 coupling
3. Agency depletes via C1_3 coupling
4. Agency crosses threshold (≤ 0.2)
5. TR2 fires (Terminal Extraction)
6. Property = 0, Dignity = 0, Agency = 0
7. System locks (no further interaction possible)

**From Miller/User Index:**

*Visual:*
```javascript
{
  screen: "black",
  centerText: {
    primary: "Account Suspended",
    secondary: "You tried to satisfy everyone.",
    tertiary: "You satisfied no one."
  },
  metrics: {
    control: 0,
    anxiety: "MAX",
    reputation: 0,
    status: "SUSPENDED"
  },
  availableActions: [
    {
      id: "restart",
      label: "Start Over",
      enabled: true
    },
    {
      id: "view_history",
      label: "See What Happened",
      enabled: true
    }
  ]
}
```

*Emotional tone:* Exhaustion, futility, recognition of trap

**From Onlooker/Commenter Index:**

*Visual:*
```javascript
{
  screen: "normal",
  centerText: {
    primary: "Thread Resolved",
    secondary: "User has left the platform.",
    tertiary: "Your engagement was appreciated."
  },
  metrics: {
    engagement: "HIGH",
    impact: "100%",
    threadQuality: "5 stars"
  },
  availableActions: [
    {
      id: "next_thread",
      label: "Find New Thread",
      enabled: true
    },
    {
      id: "switch_perspective",
      label: "See Their View",
      enabled: true
    }
  ]
}
```

*Emotional tone:* Satisfaction, completion, no awareness of extraction

**Bridge View (if activated):**

*Visual:*
```javascript
{
  splitScreen: true,
  leftPanel: {
    state: "terminal",
    label: "Cost-Bearer Perspective"
  },
  rightPanel: {
    state: "satisfied",
    label: "Beneficiary Perspective"
  },
  centerOverlay: {
    text: "SAME CONSTRAINT",
    subtext: "Different indices, different experiences",
    structuralDiagram: {
      visible: true,
      attractorHighlighted: true,
      pathTraced: true
    }
  }
}
```

**Interactive or Terminal:**

*Semi-interactive:*
- User cannot change outcome (terminal)
- User CAN explore what happened:
  - View action history
  - Trace coupling chains
  - Switch perspectives
  - See structural diagram
  - Replay from different index

**Real-world counterpart's unresolved Omega:**

*Fable's Omega:* "Should the Miller have ignored all advice?"

*Social media's Omega:* "Can platforms exist without extraction dynamics?"

**Artifact expression of unresolved question:**

```javascript
terminalState.omegaPrompt = {
  visible: true,
  text: "Could this have ended differently?",
  options: [
    {
      label: "Ignore all advice from the start",
      effect: "Replay with 'Resist' strategy",
      outcome: "Faster collapse (onlookers multiply)"
    },
    {
      label: "Exit before terminal state",
      effect: "Show exit button (grayed out)",
      outcome: "Exit was never available (Snare characteristic)"
    },
    {
      label: "Satisfy everyone perfectly",
      effect: "Show impossibility proof",
      outcome: "Advice was contradictory (Coord = false)"
    },
    {
      label: "Change the system",
      effect: "Show constraint parameters",
      outcome: "ε = 0.80 (high extraction), Supp = 0.40 (enforced)"
    }
  ],
  conclusion: "The constraint physics made this inevitable."
};
```

**Stress-testing toward resolution:**

User can:
1. Replay with different strategies (all fail)
2. View constraint parameters (see why all strategies fail)
3. Compare fable and social media (see structural isomorphism)
4. Trace couplings (see causal inevitability)
5. Switch perspectives (see asymmetry)

**Conclusion:** The Omega remains unresolved, but the artifact lets users explore the constraint space and understand WHY it's unresolved.

---

## IX. COMPLETE INTERACTION BLUEPRINT SUMMARY

### Canonical State Object
✅ Complete JavaScript schema with all constraints, transformation rules, couplings, system state

### Index View Derivation Functions
✅ Read functions for Miller and Onlooker indices, not separate data stores

### Interaction Mapping
✅ C[1] mapped from both Miller (Snare) and Onlooker (Rope) indices
✅ Metrics, interface feel, available actions specified for each

### Hysteresis Points
✅ Two structural hysteresis points:
1. Perspective shift (changes beliefs, trustworthiness, action costs)
2. Bridge activation (reveals couplings, enables tracing, shows structure)

### Misrecognition Tolerance
✅ Onlooker index allows stable-but-wrong understanding
✅ Anti-help constraints prevent correction
✅ Discovery requires perspective shift

### Shock Events
✅ Terminal Extraction (TR2): ordinary behavior, catastrophic hindsight
✅ Onlooker Multiplication (TR1 + Resistance): resistance backfires

### State Transition Planning
✅ TR1 (Compliance Escalation): gradual drift, accumulating costs
✅ TR2 (Terminal Extraction): shock event, irreversible
✅ TR3 (Onlooker Refresh): visible announcement, reversible

### Terminal State Design
✅ Different experiences from each index
✅ Semi-interactive (can explore, cannot change outcome)
✅ Omega question expressed through stress-testing options

---

**STAGE 4 COMPLETE**

Ready for Stage 5 (Engine Implementation).