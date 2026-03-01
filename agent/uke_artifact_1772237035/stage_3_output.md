# STAGE 3: PATH SELECTION & SYSTEM NATURALIZATION

**Model:** Claude (demonstrative, comparative)
**Input:** Stage 1 formal specification
**Output:** Path selection + modality selection + system architecture + system personality specification

---

## 3.1 PATH SELECTION

### Decision Matrix Evaluation

**1. DOES MATCHING REAL-WORLD TOPOLOGY EXIST?**
✓ Yes, with rich data potential

The constraint topology maps to:
- **Athletic decline narratives** (aging athletes, career transitions)
- **Economic dependency in relationships** (breadwinner dynamics, power shifts)
- **Cultural capital gatekeeping** (academic/artistic circles, insider/outsider dynamics)

All three have documented real-world counterparts with available data.

**2. WHAT IS THE DOMINANT CONSTRAINT PATTERN?**
- **Cascade failure** (C[1] exit → C[2] power inversion → C[3] forced exposure)
- **Perspectival gap** (same constraints experienced as Rope/Tangled/Snare by different indices)
- **Temporal degradation** (C[2] transforms from ε=0.20 → ε=0.65 over time)

**Mixed pattern with strong perspectival component.**

**3. WHO IS THE AUDIENCE?**
Primary: General public + literary audience
Secondary: Anyone who has experienced status loss, economic dependency, or cultural exclusion

**4. WHAT SERVES THE CREATIVITY?**
The story's power lies in **indexical variance** — Darling experiences all three constraints as increasingly extractive while Louise experiences C[3] as pure Rope. The 80-yard run memory functions as a **terminal attractor** that prevents adaptation.

### PATH DECISION: **Path E — Parallel Resonance ("The Mirror-Tender")**

**Justification:**

This topology demands **side-by-side comparison** because:

1. **The literary source is culturally familiar** — Irwin Shaw's story is a canonical American text about masculine decline
2. **The real-world counterpart is surprising** — the same constraint topology appears in:
   - Prime-age male labor force participation decline (C[1] analog)
   - Economic dependency in modern relationships (C[2] analog)  
   - Cultural capital barriers (C[3] analog)
3. **The bridge IS the revelation** — recognizing the structural echo across 80 years reveals that these aren't individual failures but **constraint topology patterns**

**Why not other paths:**

- **Path A (Diegetic):** Would require full air gap, losing the Shaw text's cultural resonance
- **Path B (Explorer):** Too explanatory; would lecture about the mapping
- **Path C (Perspectival):** Loses the temporal dimension (1941 → 2024)
- **Path D (Topology Bridge):** No literary anchor; would become pure sociology

**Creative misalignment note:** We're choosing Path E despite it being the hardest to execute because the **temporal bridge** (1941 Shaw → 2024 data) is itself a constraint topology — the same patterns persist across generations, which is the point.

---

## 3.2 MODALITY SELECTION

**Primary Modality:** Split Panel (✅ 1st choice for Path E)

**Secondary Modality:** Dashboard elements within panels (for data visualization)

**Compatibility:** ✅ Natural fit

### Modality Specification

**Left Panel: Literary Source (1941)**
- Shaw's text presented in fragments
- Key constraint moments highlighted
- Character indices visible through text selection
- Darling's χ values implicit in narrative

**Right Panel: Real-World Data (2024)**
- Labor force participation data (C[1])
- Relationship economics data (C[2])
- Cultural capital studies (C[3])
- Same constraint structure, different surface

**Bridge Mechanism:**
- **Synchronized highlighting** — when user explores C[1] in Shaw text, corresponding data appears in right panel
- **Constraint type indicators** — visual markers show when Darling experiences Rope→Tangled→Snare transition
- **Index position markers** — show Darling vs Louise vs Flaherty structural positions
- **Temporal scrubber** — slide from 1941 to 2024, watch topology persist

**Anti-pattern enforcement:**
- **No "compare" button** that reconciles perspectives
- **No unified dashboard** that flattens indexical variance
- **No explanatory text** that lectures about the mapping

The bridge must be **discovered through interaction**, not announced.

---

## 3.3 AIR GAP LEVEL

**Partial Air Gap**

**Rationale:**
- Literary source (Shaw) retains original terminology
- Real-world data uses standard sociological language
- Framework terminology (χ, ε, coupling) **never appears in interface**
- Bridge is structural, not semantic

**What's visible:**
- "Status loss" not "C[1] exit"
- "Economic dependency" not "power inversion"
- "Cultural exclusion" not "C[3] forced participation"

**What's hidden:**
- All Greek letters (ε, χ, π, σ)
- Type classifications (Rope, Snare, Tangled)
- Coupling notation (M1, M2, M3)
- Transformation rules (TR1.1, TR2.2, etc.)

**Exception:** Advanced mode (optional) reveals framework layer for users who want to see the skeleton.

---

## 3.4 SYSTEM ARCHITECTURE NATURALIZATION

### Path E Architecture: Parallel Resonance

**Component 1: Literary Panel**

```javascript
const LiteraryPanel = {
  content: shawText, // "The Eighty-Yard Run" full text
  
  highlightableSegments: [
    {
      id: "eighty_yard_run",
      text: "He was running now...",
      constraintMapping: "C[1].peak_performance",
      darlingIndex: { P: "powerful", T: "immediate", E: "mobile", S: "local" },
      louiseIndex: { P: "moderate", T: "biographical", E: "mobile", S: "local" },
      χ_darling: -0.15, // Rope (net benefit)
      χ_louise: 0.20    // Rope (coordination)
    },
    {
      id: "fifteen_years_later",
      text: "Fifteen years. Married, getting a little fat...",
      constraintMapping: "C[1].system_exit",
      darlingIndex: { P: "powerless", T: "immediate", E: "trapped", S: "local" },
      χ_darling: 0.85   // Snare
    },
    {
      id: "louise_working",
      text: "Louise was working now, editing...",
      constraintMapping: "C[2].power_inversion",
      darlingIndex: { P: "powerless", T: "biographical", E: "trapped", S: "local" },
      louiseIndex: { P: "moderate", T: "biographical", E: "constrained", S: "local" },
      χ_darling: 0.78,  // Snare
      χ_louise: 0.55    // Tangled Rope
    },
    {
      id: "flaherty_party",
      text: "Flaherty was talking about Klee...",
      constraintMapping: "C[3].cultural_capital",
      darlingIndex: { P: "powerless", T: "immediate", E: "trapped", S: "local" },
      louiseIndex: { P: "moderate", T: "biographical", E: "mobile", S: "national" },
      flahertyIndex: { P: "powerful", T: "generational", E: "mobile", S: "national" },
      χ_darling: 0.72,   // Snare
      χ_louise: 0.32,    // Rope
      χ_flaherty: -0.08  // Rope (net benefit)
    }
  ],
  
  interactionMode: "discovery", // User clicks text, highlights propagate
  
  visualEncoding: {
    darlingExperience: "red-gradient", // Intensifies toward Snare
    louiseExperience: "blue-stable",   // Remains Rope/Tangled
    memoryAttractor: "pulse-effect"    // 80-yard run pulses when referenced
  }
};
```

**Component 2: Data Panel**

```javascript
const DataPanel = {
  datasets: [
    {
      id: "prime_age_male_lfp",
      source: "BLS, 1950-2024",
      constraintMapping: "C[1]",
      metrics: [
        { year: 1950, lfp: 97.1, status: "peak" },
        { year: 1970, lfp: 96.4, status: "stable" },
        { year: 1990, lfp: 93.4, status: "declining" },
        { year: 2024, lfp: 88.5, status: "crisis" }
      ],
      indexPositions: {
        employed: { P: "moderate", χ: 0.35 },
        unemployed_short: { P: "powerless", χ: 0.65 },
        unemployed_long: { P: "powerless", χ: 0.85 } // Snare
      }
    },
    {
      id: "relationship_economics",
      source: "Pew Research, 2000-2024",
      constraintMapping: "C[2]",
      metrics: [
        { 
          scenario: "dual_income", 
          powerBalance: 0.5, 
          satisfaction: 0.7,
          χ_both: 0.30 // Rope
        },
        { 
          scenario: "single_earner_voluntary", 
          powerBalance: 0.65, 
          satisfaction: 0.55,
          χ_earner: 0.45,      // Tangled
          χ_dependent: 0.60    // Tangled
        },
        { 
          scenario: "single_earner_forced", 
          powerBalance: 0.85, 
          satisfaction: 0.25,
          χ_earner: 0.65,      // Tangled (heavy)
          χ_dependent: 0.85    // Snare
        }
      ]
    },
    {
      id: "cultural_capital_access",
      source: "Bourdieu studies, arts participation data",
      constraintMapping: "C[3]",
      metrics: [
        { 
          education: "graduate_degree", 
          arts_participation: 0.72, 
          χ: 0.25 // Rope
        },
        { 
          education: "bachelors", 
          arts_participation: 0.48, 
          χ: 0.50 // Tangled
        },
        { 
          education: "high_school", 
          arts_participation: 0.18, 
          χ: 0.75 // Snare
        }
      ]
    }
  ],
  
  visualizationMode: "synchronized", // Highlights match literary panel
  
  temporalScrubber: {
    range: [1941, 2024],
    showTopologyPersistence: true, // Same structure across 83 years
    highlightThresholdCrossings: true // When χ crosses Rope→Tangled→Snare
  }
};
```

**Component 3: Bridge Mechanism**

```javascript
const BridgeMechanism = {
  synchronization: {
    trigger: "user_interaction", // Click, hover, scrub
    
    literaryToData: (segment) => {
      // When user explores Shaw text segment
      const mapping = segment.constraintMapping;
      const relevantData = DataPanel.datasets.find(d => d.constraintMapping === mapping);
      
      return {
        highlightData: relevantData,
        showIndexPositions: true,
        animateχTransition: segment.χ_darling > 0.65 // If Snare, pulse
      };
    },
    
    dataToLiterary: (dataPoint) => {
      // When user explores data
      const relevantSegments = LiteraryPanel.highlightableSegments.filter(
        s => s.constraintMapping === dataPoint.constraintMapping
      );
      
      return {
        highlightSegments: relevantSegments,
        showCharacterExperience: true,
        revealIndexicalVariance: true // Louise vs Darling
      };
    }
  },
  
  revelationSequence: [
    {
      stage: "surface",
      userSees: "Two stories side by side (1941 fiction, 2024 data)",
      frameworkHidden: true
    },
    {
      stage: "pattern_recognition",
      trigger: "user explores 3+ constraint mappings",
      userSees: "Same structure in both (status loss → dependency → exclusion)",
      frameworkHidden: true
    },
    {
      stage: "indexical_variance",
      trigger: "user compares Darling vs Louise experience",
      userSees: "Same constraint = different experience by position",
      frameworkHidden: true
    },
    {
      stage: "temporal_persistence",
      trigger: "user scrubs timeline",
      userSees: "Topology unchanged across 83 years",
      frameworkHidden: true
    },
    {
      stage: "advanced_mode",
      trigger: "user requests framework layer",
      userSees: "Full constraint logic (ε, χ, couplings, UCZs)",
      frameworkVisible: true
    }
  ],
  
  antiPatterns: {
    noCompareButton: true,        // Don't reconcile perspectives
    noUnifiedDashboard: true,     // Keep panels distinct
    noExplanatoryText: true,      // Discovery, not lecture
    noFrameworkLeakage: true      // Greek letters hidden until advanced mode
  }
};
```

---

## 3.5 CONSTRAINT-DRIVEN AESTHETICS

### Type-to-Interaction Mapping

**Mountain (■):** Not present in this topology (all constraints are changeable)

**Rope (⊞):** 
- **Visual:** Smooth gradients, flowing connections, low friction
- **Interactive:** Responsive, glassy feel
- **Applied to:** Louise's C[3] experience, early C[2] for both, Darling's C[1] peak

**Snare (⊠):**
- **Visual:** Viscous, high contrast, constrained viewport
- **Interactive:** Input lag, friction, boundaries visible
- **Applied to:** Darling's late C[1], C[2], C[3] experiences

**Tangled Rope (⊞⊠):**
- **Visual:** Conflicting feedback, dual-color gradients (coordination + extraction)
- **Interactive:** Some actions smooth, others trigger secondary effects
- **Applied to:** Louise's late C[2] experience, moderate positions in data

**Scaffold (⊡):** Not present (no temporary structures)

**Piton (⊟):** Not present (no degraded theater)

### Index-to-Feel Mapping

**Darling's trajectory:**
- **Early (powerful/mobile):** Glassy — low latency, expansive view, minimalist
- **Middle (moderate/constrained):** Standard — responsive but boundaries visible
- **Late (powerless/trapped):** Viscous — input lag, constrained viewport, high friction

**Louise's experience:**
- **C[3] (moderate/mobile):** Glassy — cultural sphere is Rope for her
- **Late C[2] (moderate/constrained):** Standard with friction — Tangled Rope feel

**Data panel:**
- **High χ positions:** Viscous rendering (Snare experience)
- **Low χ positions:** Glassy rendering (Rope experience)
- **Moderate χ:** Standard (Tangled Rope)

### Visual Encoding

```javascript
const ConstraintAesthetics = {
  χ_to_visual: (χ_value, index_position) => {
    if (χ_value < 0.30) {
      return {
        gradient: "blue-to-green",
        opacity: 0.9,
        latency: 0,
        friction: 0.1,
        viewport: "expansive",
        feel: "glassy" // Rope
      };
    } else if (χ_value < 0.60) {
      return {
        gradient: "blue-yellow-split",
        opacity: 0.7,
        latency: 50,
        friction: 0.5,
        viewport: "standard",