## STAGE 3: PATH SELECTION & SYSTEM NATURALIZATION

**Role:** The Architect
**Model:** Claude (demonstrative, comparative; strong at side-by-side contrasts; or Grok for narrative/mythic resonance when Path A)
**Input:** Stage 1 formal specification (relabeled if air gap applies)
**Output:** Path selection + modality selection + system architecture + system personality specification

### 3.1 Path Selection (The Structural Relationship)

The path determines the *structural relationship* between the user and the constraint physics.

#### Available Paths

**Path A — Diegetic System ("The Hermit")**
*"You're dropped into a broken instrument whose behavior is the story; you debug your way to understanding."*

The software IS a fictional system whose operational behavior embodies the constraint topology. The user interacts as if operating or debugging.

- *Precedent:* theta7_terminal.tsx
- *Creative bias:* Builds worlds from residue and jargon
- *Best when:* Cascade failure, feedback loops, or orphaned purpose
- *Failure mode:* Lore-heavy instead of constraint-heavy; aesthetic swallows physics
- *Framework visibility:* Zero
- *Air gap:* Full (mandatory)

**Path B — Constraint Explorer ("The Cartographer")**
*"A structural déjà vu machine: one topology, two worlds, you walk the bridge."*

Maps a literary source's constraint topology onto real-world phenomena sharing the same structure.

- *Precedent:* Metamorphosis → Prime-Age Male Unwork
- *Creative bias:* Loves mapping fiction to reality
- *Best when:* A real-world constraint story with matching topology exists
- *Failure mode:* Over-explains the bridge; becomes a lecture
- *Framework visibility:* Semi-visible
- *Air gap:* Partial

**Path C — Perspectival Engine ("The Shifter")**
*"A morality play without a moral: same world, rotated, no one gets the whole of it."*

Interactive experience where the user inhabits different index positions.

- *Precedent:* Multi-perspective constraint analysis
- *Creative bias:* Obsessed with perspective
- *Best when:* Dominant drama is perspectival gap
- *Failure mode:* One perspective is secretly "correct"
- *Framework visibility:* Low
- *Air gap:* Full (mandatory)

**Path D — Topology Bridge ("The Anatomist")**
*"A scientific instrument that slowly admits it is also an oracle."*

Start from real-world data directly. No literary intermediary.

- *Precedent:* Prolog constraint stories with interactive front end
- *Creative bias:* Wants to expose the skeleton
- *Best when:* Real-world data is rich enough to carry the experience alone
- *Failure mode:* Becomes a dashboard
- *Framework visibility:* High
- *Air gap:* None

**Path E — Parallel Resonance ("The Mirror-Tender")**
*"Split-screen haunting: different surfaces, same skeleton, your cursor is the tuning fork."*

Present literary source and real-world counterpart side by side.

- *Precedent:* Metamorphosis / Unwork juxtaposition
- *Creative bias:* Lives for the moment of recognition across domains
- *Best when:* Literary source is culturally familiar AND real counterpart is surprising
- *Failure mode:* The bridge overshadows both shores
- *Framework visibility:* Medium
- *Air gap:* Partial

#### 3.2 Path Decision Matrix

Evaluate in order. First decisive factor wins:

```
1. DOES MATCHING REAL-WORLD TOPOLOGY EXIST?
   ├─ Yes, with rich data    → Path B, D, or E viable
   ├─ Yes, but data is thin  → Path B or E
   └─ No / not yet           → Path A or C only

2. WHAT IS THE DOMINANT CONSTRAINT PATTERN?
   ├─ Cascade failure         → Path A or B
   ├─ Perspectival gap        → Path C
   ├─ Temporal degradation    → Path D
   ├─ Structural echo         → Path E
   └─ Mixed                   → Path B (most flexible)

3. WHO IS THE AUDIENCE?
   ├─ LLMs                   → Path A
   ├─ General public          → Path B or E
   ├─ Domain experts          → Path D
   └─ Art/literature audience → Path A or C

4. WHAT SERVES THE CREATIVITY?
   Tiebreaker. Choose the path that produces the most surprising recognition.
   If still undecided: default to Path B (most flexible) + Terminal modality.
```

#### 3.3 Creative Misalignment (Optional)

You may deliberately choose a non-optimal path if the mismatch itself reveals something. You may hybridize paths. Document the intent.

### 3.4 Modality Selection (The Rendering)

The modality determines *how* the path is rendered. Path and modality are independent axes.

**Path/Modality Compatibility Matrix:**

```
              Terminal  Dashboard  IntFiction  Visualization  SplitPanel  Game
Path A         ✅(1st)    ⚠️          ⚠️            ❌            ❌        ⚠️
Path B         ⚠️        ✅          ❌            ⚠️           ✅(1st)    ❌
Path C         ❌        ⚠️         ✅(1st)        ⚠️           ✅         ✅
Path D         ❌       ✅(1st)      ❌           ✅(1st)        ⚠️        ❌
Path E         ❌        ✅          ❌            ⚠️           ✅(1st)    ❌

✅ = natural fit    ⚠️ = possible with adaptation    ❌ = likely incoherent
(1st) = default/primary modality for this path
```

**For ⚠️ combinations:** Document why this non-default pairing serves the topology. Use these adaptation patterns:

```
ADAPTATION PATTERNS FOR ⚠️ COMBINATIONS:

Path A + Dashboard:
  Terminal aesthetics (monospace, system logs) inside dashboard containers.
  Widgets look like terminal outputs. System-monitoring frame, diegetic content.

Path A + Interactive Fiction:
  System logs become journal entries or found documents.
  Metrics become character perceptions ("The air thickens," "Joints resist").
  Transformation rules become revelations discovered through text exploration.

Path A + Game:
  Constraint physics become resource management mechanics.
  Transformation rules become level transitions. Attractor = endgame state.
  Diegetic frame preserved: the game IS the system, not a game ABOUT the system.

Path B + Terminal:
  Split terminal panes (tmux-style). Literary side: narrative fragments.
  Real side: data streams. Bridge: shared constraint highlighting.

Path C + Dashboard:
  Each index gets its own dashboard panel. Switching indices rotates
  which panel is primary. Hysteresis: elements from prior panel persist.
  Anti-help: no "compare" view that reconciles perspectives.

Path D + Split Panel:
  Topology bridge on one side, temporal scrubber on other.
  Or: raw data vs. interpreted data — same topology, different granularity.
```

**Default fallback rule:** If the model cannot confidently justify a non-default modality, it must select the default (marked `(1st)`) for the chosen path. Incoherent pairings are worse than conservative ones.

**Selection principle:** The modality should make constraint dynamics *native* to the interface vocabulary.

**Modality may shift within an artifact** — a Path B artifact might use split panel for the literary/real bridge but shift to dashboard when the user drills into a specific constraint.

### 3.5 System Architecture Naturalization

Once path and modality are selected, naturalize the constraint topology into a system architecture.

**For Path A (Diegetic System):**
- Map constraints to system components (services, sensors, processes)
- Map transformation rules to state transitions with thresholds
- Map constraint network to causal propagation rules
- Map error manifestations to observable system failures
- Map attractor to terminal system state
- Select Resonance Engine mode (A–G) or invent new mode
- System jargon: internally consistent, ungoogleable

**For Path B (Constraint Explorer):**
- Document topological isomorphism explicitly
- Map literary characters to real structural roles
- Design index-position selection mechanism
- Determine real data sources for metrics (cited)
- Design the Context / Descent transition
- Handle Omega variable asymmetry

**For Path C (Perspectival Engine):**
- Determine navigable indices (minimum 2, ideal 3–4)
- For each index: what user sees, how metrics are labeled, what actions are available
- Design revelation mechanism: discoverable, not announced
- Ensure no index is "the correct one"

**For Path D (Topology Bridge):**
- Constraint network as navigable graph
- Temporal drift as animation or scrubber
- Purity scores as visual health indicators
- Omega variables as explicit open questions

**For Path E (Parallel Resonance):**
- Determine synchronization points
- Constraint highlighting crosses domains in real time
- The bridge is visually distinct from both panels

### 3.6 Constraint-Driven Aesthetics

The constraint topology should drive the artifact's visual and interactive language. This is structural expression, not decoration.

**Type-to-Interaction Mapping:**

| Type | Visual/Interactive Signature |
|------|------------------------------|
| Mountain (■) | Immovable elements. Cannot be closed, resized, or dismissed. High visual weight. |
| Rope (⊞) | Directional flow. Linear navigation. Smooth, responsive interaction. |
| Snare (⊠) | Interaction costs accumulate. Actions become harder over time. Subtle friction. |
| Tangled Rope (⊞⊠) | Conflicting feedback. One action triggers an unrelated secondary effect. |
| Scaffold (⊡) | Temporary elements. Visible expiration. Fade as they approach sunset. |
| Piton (⊟) | Residual, degraded. Low opacity. Present but non-functional. Ghost elements. |

**Index-to-Feel Mapping:**

| Index Position | Interface Character |
|----------------|---------------------|
| Powerless/Trapped | Viscous — input lag, constrained viewport, high-friction scrolling |
| Moderate/Constrained | Standard — responsive but with visible boundaries |
| Powerful/Mobile | Glassy — low latency, expansive view, minimalist |
| Institutional | Clinical — clean, abstract, metrics-forward |
| Analytical | Transparent — all layers visible, observatory feel |

These are defaults. Override when the topology demands it — but document why.

**Aesthetic Betrayal (Optional):**
You may deliberately violate one constraint-driven aesthetic mapping if the violation itself reveals the topology more sharply. A Snare that feels glassy. A Mountain that dissolves. Maximum one betrayal per artifact. The betrayal must change *functionality*, not just appearance — and must be documented as intentional.

### 3.7 System Personality Specification

Define the artifact's personality with the rigor of a character sheet. **The personality must be a function of the constraint topology, not an independent creative layer.** Every trait should trace back to a constraint, coupling, or transformation rule.

```
VOICE:
  Register: [formal/informal/clinical/broken/lyrical/bureaucratic]
  Vocabulary domain: [what jargon family does it speak?]
  Emotional baseline: [what does it feel when idle?]
  Derives from: [which constraint or coupling drives this?]

DIAGNOSTIC VOCABULARY:
  Error messages sound like: [example]
  Status reports sound like: [example]
  User prompts sound like: [example]
  Derives from: [which error manifestations or system states?]

BEHAVIORAL CONSTANTS:
  Response to user success: [how does it react?]
  Response to user failure: [how does it react?]
  Response to being ignored: [what happens if the user doesn't interact?]
  Response to being probed: [what happens if the user tests boundaries?]
  Derives from: [which transformation rules or attractor properties?]

SELF-DESCRIPTION (3–5 sentences):
  Written as if the artifact were describing itself.
  Captures temperament: what it wants, what it fears, what it can't quite say.
```

**Personality grounding test:** If you remove a constraint from the topology, at least one personality trait should become incoherent. If the personality survives topology changes intact, it's decorative, not structural.

### Stage 3 Tension Prompt (optional)
*"What would be the wrong path for this topology — and what surprising artifact would that produce?"*

### Output

```
Stage 3 Output:
  - Selected path (A/B/C/D/E or hybrid) with justification
  - Selected modality with compatibility rating
  - Air gap level (Full/Partial/None) confirmed
  - System architecture document
  - Constraint-driven aesthetic specification
  - System personality specification
```

---
