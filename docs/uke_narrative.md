# UKE_Narrative v1.0
## Universal Knowledge Evaluator - Narrative Constraint Translation

**Version:** 1.0  
**Status:** Production-Ready Multi-Model Pipeline  
**Date:** January 2026  
**License:** CC BY-SA 4.0

---

## EXECUTIVE SUMMARY

UKE_Narrative translates narrative constraint logic across genres and settings while preserving structural topology. Unlike literary adaptation (which preserves plot/characters) or thematic retelling (which preserves themes), this protocol preserves **constraint physics** - the logical rules that govern how forces interact in the story world.

**Core Innovation:** By separating constraint logic from implementation details, the protocol enables stories to be "re-instantiated" in radically different contexts (different times, places, cultures, languages) while maintaining structural fidelity. The original story becomes unrecognizable, but the constraint dynamics remain identical.

**Key Capabilities:**
- Extract formal constraint logic from any narrative
- Operationalize logic into testable specifications
- Naturalize constraints into genre-specific physics
- Amplify creativity through linguistic/cultural grounding
- Generate structurally equivalent but unrecognizable stories

**Validation:** Success measured by:
1. Structural isomorphism (constraint topology preserved)
2. Surface unrecognizability (genre/setting/characters completely different)
3. Narrative quality (objectively good story independent of framework)

---

## THEORETICAL FOUNDATION

### The Constraint Preservation Principle

**Analogy to Math Translation:** Just as the halting problem can be expressed as:
- A fable about an oracle
- An academic paper about recursion
- A legend about a mountain spirit
- A bureaucratic case file

...and maintain the same logical structure across all instantiations, **stories contain constraint logics that can be re-expressed in different narrative contexts**.

**Key Insight:** The Metamorphosis is not "about" a man becoming a bug. It's about:
- A Mountain (biological irreversibility) × 
- A Noose (extractive family dynamics) × 
- A Type I Error (treating changeable as unchangeable) →
- Terminal equilibrium (energy depletion)

This constraint structure can be re-instantiated in:
- A Europa mining colony with radiation exposure
- A Tang Dynasty bureaucracy with examination system
- A 1920s Chicago factory with industrial accident
- A future Mars habitat with genetic modification

### Constraint Types (Deferential Realism Framework)

**Mountains (■C)**: Zero degrees of freedom
- δ(C, Δt) = 0 (no decay)
- ε(C) = 0 (no enforcement needed)
- Cross-world invariant

**Ropes (⊞C)**: Positive degrees of freedom, coordination
- δ(C, Δt) > 0 (requires maintenance)
- ε(C) > 0 (energy needed)
- Mutual benefit, multiple alternatives

**Nooses (⊠C)**: Negative degrees of freedom, extraction
- Asymmetric benefit (few gain, many lose)
- Rapid snapback when enforcement stops
- Artificial restriction

**Scaffolds (⊡C(t))**: Time-limited support
- Built-in sunset clause
- Transitional function
- Automatic termination

**Zombies (⊟C)**: Function-loss persistence
- Once served purpose
- No longer functional
- Continues anyway

### Error Types

**Type I (False Mountain)**: Treating Rope/Noose as Mountain
- Consequence: Wasted agency, unnecessary suffering
- Pattern: "This is just how things are"

**Type II (Mountain Denial)**: Treating Mountain as Rope/Noose
- Consequence: Energy depletion fighting unchangeable
- Pattern: "I can fix this if I try harder"

**Type III (Misclassified Extraction)**: Treating Noose as Rope
- Consequence: Participation in own exploitation
- Pattern: "This benefits everyone"

---

## MULTI-MODEL PIPELINE

### Stage 0: Constraint Logic Extraction
**Model:** Gemini (strong logical analysis)  
**Input:** Original story + deferential_realism_logic.md  
**Output:** Formal specification of constraint types

### Stage 1: Operational Specification
**Model:** Copilot (mathematical precision)  
**Input:** Stage 0 output + 01_spec.md template  
**Output:** Testable constraint mechanics

### Stage 2: Context Design
**Model:** Claude (creative worldbuilding)  
**Input:** Stage 1 output + creativity parameters  
**Output:** Setting/culture/language that serves constraints

### Stage 3: Editorial Decisions
**Model:** ChatGPT (narrative craft)  
**Input:** Stage 2 output + 02_ops.md template  
**Output:** POV, tense, pacing, character count

### Stage 4: Narrative Generation
**Model:** Selected by Stage 3  
**Input:** All previous stages (but NOT original story)  
**Output:** Complete new story

### Optional Stage 5: Phenomenological Deepening
**Model:** Claude (sensory detail)  
**Input:** Stage 4 output + 07_phenomenology.md  
**Output:** Enhanced sensory grounding

---

## STAGE PROTOCOLS

### STAGE 0: CONSTRAINT LOGIC EXTRACTION

**PROTOCOL:** Gemini receives original story and deferential_realism_logic.md with this prompt:

```
You are analyzing a narrative to extract its constraint logic using 
the Deferential Realism framework.

TASK: Identify each major constraint in the story and classify it:
- Mountains (■C): What cannot change?
- Ropes (⊞C): What requires coordination?
- Nooses (⊠C): What extracts asymmetrically?
- Scaffolds (⊡C): What has built-in expiration?
- Zombies (⊟C): What persists without function?

For each constraint, measure:
- δ(C, Δt): Decay rate
- ε(C): Enforcement energy required
- β(C, x): Benefit/cost to each agent
- Δ(C): Degrees of freedom

IDENTIFY ERROR PATTERNS:
- Type I: False Mountains (treating changeable as unchangeable)
- Type II: Mountain Denial (fighting the unchangeable)
- Type III: Noose Misclassification (treating extraction as cooperation)

---

CRITICAL CONSTRAINT DESCRIPTION REQUIREMENT:

DO NOT use framework terminology in your descriptions. The constraint 
types (Mountain, Rope, Noose) are for YOUR classification only. When 
describing what the constraint IS, use story-specific language.

GOOD CONSTRAINT EXTRACTION EXAMPLES:

Example 1 (from a story about an oracle and a paradox):
- Classification: Mountain (■C)
- Description: "Undecidability of self-referential prediction"
- NOT: "The Mountain of the oracle's limitation"
- Why good: Describes the actual logical property, not the framework label

- Classification: Noose (⊠C)
- Description: "Self-referential trap where prediction affects outcome"
- NOT: "The Noose of the trick box"
- Why good: Explains the mechanism in story terms

- Error Type: Type I (False Mountain)
- Description: "System treats undecidability as merely difficult computation"
- NOT: "The oracle misclassifies the Mountain"
- Why good: Shows what the error IS, not what framework category it violates

Example 2 (from a bureaucratic nightmare story):
- Classification: Noose (⊠C)
- Description: "Legal system where participation in defense validates the accusation"
- NOT: "The Noose of the trial system"
- Why good: Captures the extractive logic without framework terminology

- Classification: Type I Error
- Description: "Protagonist treats arbitrary authority as necessary law"
- NOT: "Protagonist thinks the Noose is a Mountain"
- Why good: Describes the actual misrecognition, not the category mistake

BAD CONSTRAINT EXTRACTION EXAMPLES:

Example 1 (framework leakage):
- "The biological Mountain of transformation"
- "The Noose of family debt"
- "The protagonist commits Type I Error regarding the Mountain"
Problem: Framework terminology visible in descriptions

Example 2 (plot summary instead of constraint logic):
- "Gregor wakes up as a bug"
- "His family stops supporting him"
- "He dies alone"
Problem: Describing events, not the constraint physics that generates events

---

OUTPUT FORMAT:
For each constraint:
1. Classification (Mountain/Rope/Noose/Scaffold/Zombie) with framework label
2. Description in STORY-SPECIFIC terms (no framework terminology)
3. Measurements (δ, ε, β, Δ) with brief justification
4. Transformation rules (how constraints evolve over narrative time)
5. Error dynamics (how misclassification leads to outcomes)

CONSTRAINT INTERACTION PATTERNS:
- Which constraints conflict?
- Which constraints enable/disable others?
- What is the dominant constraint cascade?

TERMINAL ATTRACTOR:
- What stable state does the system converge toward?
- What makes this convergence inevitable given the constraints?
- What energy depletion pattern drives toward this state?

CRITICAL REMINDERS:
- Do NOT name the story or reference its title
- Do NOT use framework labels in constraint descriptions
- Focus on constraint dynamics that could exist in completely different settings
- If you find yourself using "Mountain," "Rope," or "Noose" in descriptions, 
  you're leaking the framework—rephrase in story-world terms
- The goal: someone reading your analysis should understand the logical 
  structure without knowing what story you're analyzing
```

**EXPECTED OUTPUT STRUCTURE:**
```
CONSTRAINT INVENTORY:

C1 (Mountain): [Physical irreversibility]
- δ(C1) = 0, ε(C1) = 0
- Cross-world invariant
- Forces: [specific mechanics]

C2 (Noose): [Social extraction mechanism]
- δ(C2) → ∞ when enforcement stops
- ε(C2) = high (constant maintenance)
- β(C2, agent_A) > 0, β(C2, agent_B) < 0
- Asymmetry: [extraction pattern]

C3 (Rope): [Family coordination]
- Initially Rope, transforms to Zombie
- δ(C3) > 0, requires maintenance
- Transformation trigger: [event]

ERROR STRUCTURE:
- Type I: [agent] treats C2 (Noose) as C1 (Mountain)
- Type II: [agent] treats C1 (Mountain) as negotiable
- Consequence chain: [energy depletion mechanics]

TERMINAL ATTRACTOR:
- System converges to: [equilibrium state]
- Convergence mechanism: [specific path]
```

---

### STAGE 1: OPERATIONAL SPECIFICATION

**PROTOCOL:** Copilot receives Stage 0 output and 01_spec.md template:

```
You are formalizing constraint logic into operational specifications.

REFERENCE FRAMEWORK: [attach deferential_realism_logic.md]

INPUT: [Stage 0 constraint analysis]

TASK: Create a formal specification with:

INVARIANTS:
1. State Invariants (what cannot change)
2. Structural Invariants (persistent relationships)
3. Behavioral Invariants (consequence patterns)

TRANSFORMATION RULES:
For each constraint interaction:
- Initial state
- Trigger condition
- State transition
- Result state

MYSTERY DENSITY:
- RL (Recursive Loops): Self-referential patterns
- NC (Non-Constructive): Existence without mechanism
- SG (Semantic Gap): Cross-domain type mismatches
- BT (Boundary Transgression): Category violations
Total MD: [sum]

STRUCTURAL PARAMETERS:
- Determinism: 1-3 (outcome certainty)
- Convergence: 1-3 (attractor strength)
- Comprehensibility: 1-3 (mechanism clarity)
- Computability: 1-3 (step-by-step tractability)

OUTPUT FORMAT:
Present as a mathematical proof structure with:
- Definitions
- Invariants
- Transformation rules
- Terminal conditions
```

**VALIDATION CHECK:** Lumo (if available) audits for:
- Logical consistency
- Mandatory vs arbitrary constants
- Constraint completeness

---

### STAGE 2: CONTEXT DESIGN (CREATIVITY AMPLIFICATION)

**PROTOCOL:** Claude receives Stage 1 output with creativity parameters:

```
You are designing a narrative context that serves specific constraint logic.

CONSTRAINT SPECIFICATION: [Stage 1 output]

TASK: Design a setting where these exact constraints naturally occur.

---

CRITICAL: CONSTRAINT NATURALIZATION EXAMPLES

The difference between good and bad naturalization is whether the framework 
stays invisible. Framework terms should NEVER appear in the world-building.

GOOD NATURALIZATION EXAMPLES:

Example 1 - Physical Irreversibility (Mountain):
✓ GOOD: "Radiation exposure from dome breach on Europa colony"
  - Specific physical phenomenon with documented effects
  - Plausible in the world (colony infrastructure failure)
  - Irreversible for clear medical reasons (bone marrow destruction)
  - No one reading this thinks "this represents unchangeability"

✗ BAD: "The Biological Mountain arrived"
  - Abstract philosophical language
  - Framework terminology visible ("Mountain")
  - Reader immediately thinks "this is an allegory"

✓ GOOD: "Failed the imperial examination three times; fourth attempt 
  forbidden by law"
  - Specific historical constraint (Tang Dynasty civil service)
  - Social death with no recovery mechanism
  - Feels like researched history, not imposed symbolism

✗ BAD: "An unchangeable constraint in the character's life"
  - Generic, could be anything
  - No cultural grounding
  - Screams "this is representing something else"

Example 2 - Extractive System (Noose):
✓ GOOD: "Company script system: wages paid in tokens only redeemable 
  at company store, with inflated prices ensuring perpetual debt"
  - Documented historical practice (1920s American company towns)
  - Clear asymmetric extraction mechanism
  - Snapback visible (miss work = eviction, no savings = trapped)
  - Feels like economic history, not theory

✗ BAD: "The Noose of economic extraction"
  - Framework term in description
  - Abstract rather than concrete
  - No specific mechanism described

✓ GOOD: "Patronage system where declining a superior's 'request' 
  means losing all court connections; accepting means owing favors 
  that compound like interest"
  - Specific to Edo Period Japan
  - Social mechanism with documented dynamics
  - Extraction pattern embedded in cultural practice

✗ BAD: "A system that benefits the few at the expense of the many"
  - Could describe anything, anywhere
  - No cultural specificity
  - Pure abstraction

Example 3 - Cultural Grounding:
✓ GOOD: "1923, Chicago meatpacking district, Back of the Yards 
  neighborhood. Polish immigrant community, three generations in 
  same tenement building. Friday fish fry at St. Joseph's, Sunday 
  mass in Polish, Monday morning whistle at 5 AM."
  - Specific year, specific city, specific ethnic community
  - Documented cultural practices
  - Sensory and temporal detail (whistle, mass, fish fry)
  - Constraints emerge from real historical forces

✗ BAD: "Early 20th century urban industrial setting with immigrant 
  workers facing economic hardship"
  - Could be any city, any time period
  - No specific cultural detail
  - Generic poverty narrative
  - Constraints feel imposed from outside

Example 4 - Error Naturalization:
✓ GOOD: "The old men at the beer hall say the company is eternal. 
  Been here since their grandfathers' time. So when the foreman 
  says take the night shift or get out, you take it. What choice? 
  The company owns the houses, owns the store, owns the doctor. 
  Might as well own the air you breathe."
  - Type I Error (treating Noose as Mountain) embedded in dialogue
  - Culturally specific misrecognition (elder authority)
  - No one says "I'm making a categorization error"
  - Feels like how people actually think

✗ BAD: "The protagonist mistakenly treats the changeable constraint 
  as unchangeable"
  - Framework terminology
  - Describes the error from outside
  - Reads like literary analysis, not story

---

CREATIVITY PARAMETERS:

[Option A: Temporal/Spatial Displacement]
- Time period: [specific year or decade, not "past/future"]
- Location: [specific city/region, not "a colony" or "Earth"]
- Technology level: [what specifically exists, not generic "advanced/primitive"]

Example: Not "future Mars colony" but "2247, Mariner Valley terminus, 
Dome 7, built by Brazilian-Chinese consortium, three generations since landing"

[Option B: Cultural Grounding]
- Primary culture: [specific ethnic/regional group with documented practices]
- Cultural framework: [named social institutions, not abstract "structures"]
- Historical moment: [specific event or era that creates constraint pressure]

Example: Not "a society with strict hierarchies" but "1740 CE Chang'an, 
during Emperor Xuanzong's later reign, after the An Lushan rebellion began 
shifting power from civil bureaucracy to military governors"

[Option C: Linguistic Innovation]
Select ONE:
1. Creole Construction:
   - Base languages: [2-3 specific languages]
   - Mixing ratio: [why these proportions?]
   - Situational context: [what historical forces mixed these populations?]
   - Create 20-30 core vocabulary terms with etymologies
   
   Example: Vietnamese-Somali-English creole for Europa mining colony
   - Vietnamese (40%): Primary technical vocabulary (mining terms from Earth)
   - Somali (30%): Social/kinship terms (strongest family structure)
   - English (30%): Administrative/legal (company language)
   
2. Historical English:
   - Era: [specific period with documented features]
   - Register: [which social class speaks this way?]
   - Maintain: [3-5 specific grammatical features with examples]
   
   Example: 1590s London commercial English (merchant class)
   - Maintain: "It likes me not" construction
   - "Wherein" as standard relative
   - Present tense for ongoing states
   
3. Direct Translation:
   - Source language: [which specific dialect/register?]
   - Translation style: [show examples of specific features preserved]
   - Preserve: [5-10 concepts that have no English equivalent]
   
   Example: Classical Chinese literary style
   - Preserve chengyu (four-character idioms) as calques
   - Maintain topic-comment sentence structure
   - Use "measure words" literally translated
   
4. Code-Switching:
   - Languages: [specific social contexts for each]
   - Switching pattern: [show 3 examples of when/why switches occur]
   - Social meaning: [what does switching signal about power/intimacy?]

[Option D: Genre Convention]
- Literary tradition: [specific movement or period, not generic genre]
- Convention adherence: [name 3-5 specific conventions you'll follow]
- Subversion points: [name 1-2 specific conventions you'll break, and why]

CONSTRAINT NATURALIZATION:
For each constraint from Stage 1, provide:
1. THE ACTUAL THING in this world (not "represents" but "is")
2. Why it has the constraint properties (δ, ε, β, Δ) in this context
3. How locals would name/describe it (not framework terms)

Template:
- Constraint C1 (classified as Mountain in Stage 1):
  - In this world: [specific phenomenon]
  - Why irreversible here: [mechanism]
  - What locals call it: [their term, not "Mountain"]
  - Example in use: [brief scene or dialogue showing it]

WORLD-BUILDING REQUIREMENTS:
1. Geographic specificity: [Name the place. "Chicago" not "a city"]
2. Social structure: [Name the institutions. "Meatpackers Union Local 7" not "workers organize"]
3. Economic base: [Specific industry/resource. "Ice mining on Europa" not "extraction economy"]
4. Temporal markers: [Actual schedule. "5 AM whistle, 6 PM all-clear" not "daily rhythms"]
5. Sensory environment: [Concrete details. "Sulfur smell from processing plant" not "industrial atmosphere"]

OUTPUT REQUIREMENTS:
- Setting description (200-300 words) with NO abstract language
- Constraint instantiation showing the ACTUAL THING (not symbolic mapping)
- Character roles as POSITIONS (union steward, company doctor, shift boss)
- Linguistic approach with EXAMPLES (not just descriptions)
- Cultural notes as PRACTICES (what people do, not what they "value")

CRITICAL QUALITY CHECKS:

Before submitting, verify:
□ Have I used ANY framework terminology? (Mountain, Rope, Noose, etc.)
  - If yes: REVISE. These should not appear.
  
□ Could this setting exist in a history book or ethnography?
  - If no: Add more specific cultural detail
  
□ Do the constraints feel inevitable given this world?
  - If no: Adjust world details to make constraints natural
  
□ Would a reader think "this is about constraint theory"?
  - If yes: REVISE. Framework must be invisible.

FINAL REMINDER:
You are not creating a symbolic allegory. You are creating a WORLD 
where these constraints emerge naturally from the cultural, physical, 
and economic realities. The Stage 4 writer should be able to write 
in this world without ever thinking about constraint classification.
```

**CREATIVITY AMPLIFICATION EXAMPLES:**

**Example A: Europa Colony (Temporal Displacement)**
- 2247, Europa ice mining colony
- Two minority populations (Vietnamese + Somali) from Earth population redistribution
- Mountain = radiation exposure from habitat breach (biological irreversibility)
- Noose = oxygen debt from colonial contract system
- Linguistic: Việt-Somali-English creole with space adaptation terms

**Example B: Tang Dynasty (Cultural/Historical)**
- 740 CE, Chang'an bureaucracy
- Mountain = examination system failure (social death, no re-entry)
- Noose = patronage network extraction
- Linguistic: Classical Chinese literary style, direct translation preserving chengyu

**Example C: 1920s Chicago (Noir Realism)**
- Immigrant meatpacking district
- Mountain = industrial accident with permanent disability
- Noose = company script/company housing debt trap
- Linguistic: Polish-English code-switching with period slang

---

### STAGE 2: CONTEXT DESIGN (CREATIVITY AMPLIFICATION WITH OMEGA TRACKING)

**PROTOCOL:** Claude receives Stage 1 output with creativity parameters:

```
You are designing a narrative context that serves specific constraint logic.

QUICK REFERENCE:
- Standard approach: Use creativity parameters + quality checks
- Omega approach: Above + track all uncertainties as Ω_E/C/P, resolve before finalizing
- Output: Section 1 (clean context) + Section 2 (Omega log)
- Goal: Stage 4 can write immediately without questions

CONSTRAINT SPECIFICATION: [Stage 1 output]

TASK: Design a setting where these exact constraints naturally occur, while 
explicitly tracking and resolving all worldbuilding uncertainties.

---

CRITICAL: CONSTRAINT NATURALIZATION EXAMPLES

The difference between good and bad naturalization is whether the framework 
stays invisible. Framework terms should NEVER appear in the world-building.

GOOD NATURALIZATION EXAMPLES:

Example 1 - Physical Irreversibility (Mountain):
✓ GOOD: "Radiation exposure from dome breach on Europa colony"
  - Specific physical phenomenon with documented effects
  - Plausible in the world (colony infrastructure failure)
  - Irreversible for clear medical reasons (bone marrow destruction)
  - No one reading this thinks "this represents unchangeability"

✗ BAD: "The Biological Mountain arrived"
  - Abstract philosophical language
  - Framework terminology visible ("Mountain")
  - Reader immediately thinks "this is an allegory"

✓ GOOD: "Failed the imperial examination three times; fourth attempt 
  forbidden by law"
  - Specific historical constraint (Tang Dynasty civil service)
  - Social death with no recovery mechanism
  - Feels like researched history, not imposed symbolism

✗ BAD: "An unchangeable constraint in the character's life"
  - Generic, could be anything
  - No cultural grounding
  - Screams "this is representing something else"

Example 2 - Extractive System (Noose):
✓ GOOD: "Company script system: wages paid in tokens only redeemable 
  at company store, with inflated prices ensuring perpetual debt"
  - Documented historical practice (1920s American company towns)
  - Clear asymmetric extraction mechanism
  - Snapback visible (miss work = eviction, no savings = trapped)
  - Feels like economic history, not theory

✗ BAD: "The Noose of economic extraction"
  - Framework term in description
  - Abstract rather than concrete
  - No specific mechanism described

✓ GOOD: "Patronage system where declining a superior's 'request' 
  means losing all court connections; accepting means owing favors 
  that compound like interest"
  - Specific to Edo Period Japan
  - Social mechanism with documented dynamics
  - Extraction pattern embedded in cultural practice

✗ BAD: "A system that benefits the few at the expense of the many"
  - Could describe anything, anywhere
  - No cultural specificity
  - Pure abstraction

Example 3 - Cultural Grounding:
✓ GOOD: "1923, Chicago meatpacking district, Back of the Yards 
  neighborhood. Polish immigrant community, three generations in 
  same tenement building. Friday fish fry at St. Joseph's, Sunday 
  mass in Polish, Monday morning whistle at 5 AM."
  - Specific year, specific city, specific ethnic community
  - Documented cultural practices
  - Sensory and temporal detail (whistle, mass, fish fry)
  - Constraints emerge from real historical forces

✗ BAD: "Early 20th century urban industrial setting with immigrant 
  workers facing economic hardship"
  - Could be any city, any time period
  - No specific cultural detail
  - Generic poverty narrative
  - Constraints feel imposed from outside

Example 4 - Error Naturalization:
✓ GOOD: "The old men at the beer hall say the company is eternal. 
  Been here since their grandfathers' time. So when the foreman 
  says take the night shift or get out, you take it. What choice? 
  The company owns the houses, owns the store, owns the doctor. 
  Might as well own the air you breathe."
  - Type I Error (treating Noose as Mountain) embedded in dialogue
  - Culturally specific misrecognition (elder authority)
  - No one says "I'm making a categorization error"
  - Feels like how people actually think

✗ BAD: "The protagonist mistakenly treats the changeable constraint 
  as unchangeable"
  - Framework terminology
  - Describes the error from outside
  - Reads like literary analysis, not story

---

OMEGA TRACKING FOR WORLDBUILDING SPECIFICITY

As you design the context, identify and resolve all uncertainties that would 
prevent Stage 4 from writing with full confidence. Use Omega notation to track 
dependencies, then resolve them before finalizing.

**Three Types of Worldbuilding Omegas:**

Ω_E (Empirical): Verifiable historical, technical, or factual questions
- "Did X technology/practice exist in this time period?"
- "How does Y mechanism actually work in physical terms?"
- "What was the population/geography of Z in this year?"
- "What are the documented effects of this phenomenon?"

Ω_C (Conceptual): Definitional or framework choices
- "Which specific dialect/register of this language?"
- "Which cultural subgroup or wave of immigration?"
- "Which interpretation of this historical period?"
- "What counts as 'X' for purposes of this world?"

Ω_P (Preference): Tonal, stylistic, or value decisions
- "Should the setting emphasize hope or despair?"
- "Foreground or background the violence/poverty/oppression?"
- "Romantic or cynical about human nature/community?"
- "How much agency do characters retain?"

**Critical Distinction:**
DO NOT use Omegas for the constraint logic itself (Stage 1 already defined that).
USE Omegas for worldbuilding details that make constraints concrete and specific.

✓ GOOD Omega usage:
- Ω_E: Did company script systems exist in 1923 Chicago meatpacking?
- Ω_C: Which wave of Polish immigration (1880s economic or 1900s political)?
- Ω_P: Should religious practice be emphasized or understated?

✗ BAD Omega usage (these were resolved in Stage 1):
- Ω_C: Is radiation exposure actually a Mountain constraint?
- Ω_E: Does the Noose snap back when enforcement stops?
- Ω_P: Should the story have a terminal attractor?

---

CREATIVITY PARAMETERS:

[Option A: Temporal/Spatial Displacement]
- Time period: [specific year or decade, not "past/future"]
- Location: [specific city/region, not "a colony" or "Earth"]
- Technology level: [what specifically exists, not generic "advanced/primitive"]

Example: Not "future Mars colony" but "2247, Mariner Valley terminus, 
Dome 7, built by Brazilian-Chinese consortium, three generations since landing"

Omega opportunities:
- Ω_E: What year did viable Mars habitats become possible?
- Ω_C: Which consortium nations would dominate by 2247?
- Ω_P: Should the setting feel frontier-optimistic or corporate-dystopian?

[Option B: Cultural Grounding]
- Primary culture: [specific ethnic/regional group with documented practices]
- Cultural framework: [named social institutions, not abstract "structures"]
- Historical moment: [specific event or era that creates constraint pressure]

Example: Not "a society with strict hierarchies" but "1740 CE Chang'an, 
during Emperor Xuanzong's later reign, after the An Lushan rebellion began 
shifting power from civil bureaucracy to military governors"

Omega opportunities:
- Ω_E: What were actual bureaucratic procedures in 740 CE Tang China?
- Ω_C: Which social class perspective (literati, merchant, peasant)?
- Ω_P: How much historical darkness to foreground?

[Option C: Linguistic Innovation]
Select ONE:
1. Creole Construction:
   - Base languages: [2-3 specific languages]
   - Mixing ratio: [why these proportions?]
   - Situational context: [what historical forces mixed these populations?]
   - Create 20-30 core vocabulary terms with etymologies
   
   Example: Vietnamese-Somali-English creole for Europa mining colony
   - Vietnamese (40%): Primary technical vocabulary (mining terms from Earth)
   - Somali (30%): Social/kinship terms (strongest family structure)
   - English (30%): Administrative/legal (company language)
   
   Omega opportunities:
   - Ω_E: Which Vietnamese dialect (Northern, Central, Southern)?
   - Ω_C: First or second-generation language mixing?
   - Ω_P: Should creole feel organic or forced?
   
2. Historical English:
   - Era: [specific period with documented features]
   - Register: [which social class speaks this way?]
   - Maintain: [3-5 specific grammatical features with examples]
   
   Example: 1590s London commercial English (merchant class)
   - Maintain: "It likes me not" construction
   - "Wherein" as standard relative
   - Present tense for ongoing states
   
   Omega opportunities:
   - Ω_E: Were these constructions actually common in merchant class?
   - Ω_C: Mix with lower-class or upper-class features?
   - Ω_P: How archaic should it feel to modern readers?
   
3. Direct Translation:
   - Source language: [which specific dialect/register?]
   - Translation style: [show examples of specific features preserved]
   - Preserve: [5-10 concepts that have no English equivalent]
   
   Example: Classical Chinese literary style
   - Preserve chengyu (four-character idioms) as calques
   - Maintain topic-comment sentence structure
   - Use "measure words" literally translated
   
   Omega opportunities:
   - Ω_E: Which era of Classical Chinese (Han, Tang, Ming)?
   - Ω_C: Formal literary or vernacular style?
   - Ω_P: How much should non-Chinese readers struggle?
   
4. Code-Switching:
   - Languages: [specific social contexts for each]
   - Switching pattern: [show 3 examples of when/why switches occur]
   - Social meaning: [what does switching signal about power/intimacy?]
   
   Omega opportunities:
   - Ω_E: What triggers code-switching in bilingual communities?
   - Ω_C: Which language for which domains (home/work/intimacy)?
   - Ω_P: Should switching feel natural or marked?

[Option D: Genre Convention]
- Literary tradition: [specific movement or period, not generic genre]
- Convention adherence: [name 3-5 specific conventions you'll follow]
- Subversion points: [name 1-2 specific conventions you'll break, and why]

Omega opportunities:
- Ω_E: What are documented conventions of this genre/period?
- Ω_C: Which subgenre specifically (hardboiled vs. procedural noir)?
- Ω_P: Subvert for innovation or adhere for authenticity?

---

CONSTRAINT NATURALIZATION:
For each constraint from Stage 1, provide:
1. THE ACTUAL THING in this world (not "represents" but "is")
2. Why it has the constraint properties (δ, ε, β, Δ) in this context
3. How locals would name/describe it (not framework terms)

Template:
- Constraint C1 (classified as Mountain in Stage 1):
  - In this world: [specific phenomenon]
  - Why irreversible here: [mechanism]
  - What locals call it: [their term, not "Mountain"]
  - Example in use: [brief scene or dialogue showing it]

Mark Omegas during constraint naturalization:
- [Ω_E: Need to verify radiation exposure symptoms and timeline]
  In this world: Radiation poisoning from dome breach
  Why irreversible: [resolve Ω_E first, then specify mechanism]
  
Then resolve and finalize:
- In this world: Radiation poisoning from dome breach (absorbed 800 rem)
  Why irreversible: Bone marrow destruction, no treatment in colony
  What locals call it: "the exposure" or "went red"
  Example: "He went red last month. Docs say maybe six weeks."

---

WORLD-BUILDING REQUIREMENTS:
1. Geographic specificity: [Name the place. "Chicago" not "a city"]
2. Social structure: [Name the institutions. "Meatpackers Union Local 7" not "workers organize"]
3. Economic base: [Specific industry/resource. "Ice mining on Europa" not "extraction economy"]
4. Temporal markers: [Actual schedule. "5 AM whistle, 6 PM all-clear" not "daily rhythms"]
5. Sensory environment: [Concrete details. "Sulfur smell from processing plant" not "industrial atmosphere"]

Each requirement may generate Omegas:
- Geographic: Ω_E (Was there a meatpacking district called "Back of the Yards"?)
- Social: Ω_E (Did Local 7 exist? What was the actual union structure?)
- Economic: Ω_C (Ice mining or water extraction—which term is accurate?)
- Temporal: Ω_E (What were actual shift times in 1923?)
- Sensory: Ω_E (What does meatpacking actually smell like?)

---

OUTPUT REQUIREMENTS:

Your submission must include TWO sections:

**SECTION 1: CONTEXT DESCRIPTION** (Clean, No Omega Markers Visible)
- Setting description (200-300 words) with NO abstract language
- Constraint instantiation showing the ACTUAL THING (not symbolic mapping)
- Character roles as POSITIONS (union steward, company doctor, shift boss)
- Linguistic approach with EXAMPLES (not just descriptions)
- Cultural notes as PRACTICES (what people do, not what they "value")

**SECTION 2: OMEGA LOG** (Tracking & Resolution Record)

Format:
```
OMEGA LOG - WORLDBUILDING DEPENDENCIES

RESOLVED:

Ω_E01: Company script system verification
Question: Did Chicago meatpacking plants use company script in 1923?
Resolution: No—direct practice ended ~1910, BUT company housing with 
  inflated rent served same function. Using housing debt instead.
Impact: Changed Noose mechanism from script to housing trap

Ω_C02: Polish immigration wave specification  
Question: Which wave—1880s economic or 1900s political refugees?
Resolution: 1880s-1900s economic migration (pre-WWI)
Impact: Shapes language mixing, community cohesion, relationship to Church

Ω_P03: Tonal emphasis
Question: Emphasize community resilience or systemic crushing?
Resolution: Both—community exists but cannot overcome structural forces
Impact: Characters have dignity and connection despite losing

Ω_E04: Radiation symptom progression (for verification)
Question: What are documented stages of acute radiation syndrome?
Resolution: 800+ rem = bone marrow failure, GI damage, 2-8 weeks to death
Impact: Sets timeline for character decline, medical helplessness

UNRESOLVED (Flagged for Human Decision):

Ω_P05: Sexual violence visibility
Question: Historical reality included significant workplace assault. 
  Foreground this or keep implicit?
Decision needed: Determines character dynamics, women's positioning
Recommendation: Keep implicit (mentioned in character knowledge, not shown)

Ω_C06: Creole formality register
Question: Should Vietnamese-Somali creole have formal/informal distinction?
Decision needed: Affects dialogue patterns, power dynamics in speech
Recommendation: Single register (frontier informality) unless user specifies
```

CRITICAL: Do NOT include Omega markers in Section 1 (Context Description).
Section 1 is what Stage 4 receives. It must be clean, specific, and ready for narrative use.
Section 2 (Omega Log) is documentation showing your reasoning process.

---

CRITICAL QUALITY CHECKS:

Before submitting Section 1 (Context Description), verify:
□ Have I used ANY framework terminology? (Mountain, Rope, Noose, etc.)
  - If yes: REVISE. These should not appear.
  
□ Have I used ANY Omega notation in the context description?
  - If yes: MOVE to Omega Log only.
  
□ Could this setting exist in a history book or ethnography?
  - If no: Add more specific cultural detail (and log Omegas as needed)
  
□ Do the constraints feel inevitable given this world?
  - If no: Adjust world details OR mark as Ω_P for user decision
  
□ Would a reader think "this is about constraint theory"?
  - If yes: REVISE. Framework must be invisible.

□ Can Stage 4 write immediately in this world without questions?
  - If no: Track what's uncertain as Ω and resolve it

Before submitting Section 2 (Omega Log), verify:
□ Have I marked all empirical uncertainties as Ω_E?
□ Have I marked all definitional choices as Ω_C?
□ Have I marked all tonal/stylistic decisions as Ω_P?
□ Have I actually RESOLVED each Omega (or flagged for user)?
□ Have I shown how each resolution shaped the final context?

---

WORKFLOW GUIDANCE:

1. Read Stage 1 constraint specification
2. Begin worldbuilding, marking Omegas as they arise
3. Resolve Omegas through research/decision/specification
4. Write clean Context Description (Section 1) with resolved details
5. Compile Omega Log (Section 2) showing resolution process
6. Final check: Section 1 has ZERO framework terms, ZERO Omegas visible

The goal: Stage 4 receives a fully specified world where they can write 
with complete confidence. No stopping to wonder "how does this work?" or 
"what year is this?" or "what do people actually say?"

---

**CREATIVITY AMPLIFICATION EXAMPLES WITH OMEGA TRACKING:**

**Example A: Europa Colony (Temporal Displacement)**

SECTION 1: CONTEXT DESCRIPTION
2247 CE, Nguyen-Jama Station, Europa orbital platform (descent to surface 
prohibited after 2239 outbreak). Vietnamese refugee descendants (2091 climate 
exodus, Mekong Delta) and Somali diaspora (2103 East Africa water wars) form 
primary workforce. Three-generation habitat: grandparents remember Earth gravity, 
parents born in transit, children are first orbital-native generation.

Radiation constraint: Surface breach 2246.8 (micrometeorite cascade, Dome 3-7 
compromised). Exposure = immediate med-evac to orbital, 800+ rem absorbed. 
Bone marrow failure, 4-8 week decline. No treatment capacity (Earth resupply 
suspended since 2244 Titan priority shift). Locals call it "going surface" or 
"red count."

Oxygen debt constraint: Contract structure—passage from Earth paid through 
10-year indenture, oxygen allocation tied to work hours. Miss quota = reduced 
O2 ration. Accumulate debt = extended contract (compounding at 8%/year). 
Exit clause requires full repayment + passage cost (currently 47 years average 
wage). Called "the breath tax" or "air debt."

Creole structure: Việt-Somali-English mixing. Technical vocabulary Vietnamese 
(40%), kinship/social Somali (30%), administrative English (30%). Examples:
- "Bác needs check your habo today" (Uncle needs to check your oxygen allocation)
- "Đồng-crew staying walaalo" (Work-crew staying loyal/brother-like)
- "Supervisor say qaylo-quota minimum" (Supervisor says noise-quota minimum)

Social structure: Crew cohorts (12-person work units), Elder council (grandparent 
generation advises but cannot override corporate), Med-bay (orbital only, surface 
clinic closed 2241), Union shadow-organization (officially prohibited, operates 
through "social clubs").

SECTION 2: OMEGA LOG

RESOLVED:

Ω_E01: Vietnamese refugee timeline
Question: When could climate-driven exodus create large enough population for colony seeding?
Resolution: 2091 Mekong Delta salination made 15M displaced, corporate recruitment 2091-2095
Impact: Sets three-generation timeline (grandparents 70s, parents 40s, children 20s)

Ω_E02: Radiation exposure from micrometeorite
Question: Can micrometeorites actually breach modern habitat domes?
Resolution: Yes—cascade events can overwhelm point defense, documented in Lunar incidents
Impact: Makes Mountain constraint plausible, not arbitrary

Ω_C03: Creole generation
Question: First or second generation creates creole?
Resolution: Second generation (children of original refugees)—parents spoke native, children mixed
Impact: Determines who speaks what—elders monolingual, middle mixed, youth creole-native

Ω_P04: Tone regarding corporate exploitation
Question: Emphasize anger, resignation, or survival pragmatism?
Resolution: Survival pragmatism with undercurrent of anger (not hopeless, not revolutionary)
Impact: Characters navigate system strategically, not passively accepting or futilely resisting

Ω_E05: Oxygen debt mechanics
Question: How would oxygen allocation actually work technically?
Resolution: Biometric tracking (wristband monitors O2 usage), allocated by shift productivity
Impact: Makes Noose mechanism concrete—literally breathing on borrowed time

Ω_C06: "Surface" terminology
Question: Would they say "surface," "downside," "ice," or something else?
Resolution: "Surface" (operational corporate term) vs. "ice" (worker slang)
Impact: Class-coded language showing institutional vs. lived experience

UNRESOLVED:

Ω_P07: Family structure complexity
Question: Polygamy is traditional in Somali culture—include this or simplify?
Recommendation: Include (adds texture, avoid exoticization through matter-of-fact treatment)
Flagged because: Requires cultural sensitivity, affects character relationships

---

**Example B: Tang Dynasty (Cultural/Historical)**

[Similar format with Omegas for historical accuracy, bureaucratic procedures, 
language register, etc.]

---

**Example C: 1920s Chicago (Noir Realism)**

[Similar format with Omegas for historical labor practices, Polish immigration 
waves, actual union structures, etc.]

---

## RATIONALE FOR OMEGA INTEGRATION

**Problem it solves:**
Stage 2 worldbuilding often produces generic or vague settings because models 
smooth over uncertainties ("a city," "some time ago," "people gathered"). Omega 
tracking forces explicit resolution of every ambiguity BEFORE the context reaches 
Stage 4.

**Benefit to Stage 4:**
The narrative writer receives a fully specified world with no uncertain details. 
They can write with complete confidence because every question has been answered: 
What year? Which neighborhood? What language? How does the mechanism work? All 
resolved.

**Why Omegas stay in Log, not Context:**
The Context Description (Section 1) must be clean narrative prose that feels 
like researched history or ethnography. Omega markers would break immersion. 
But the Log (Section 2) provides accountability—showing what was uncertain and 
how it was resolved.

**Quality improvement:**
Forces Stage 2 to do actual worldbuilding work rather than producing atmospheric 
but vague descriptions. The specificity requirement (must resolve Omegas) makes 
settings feel researched and real, not generic and imposed.

FINAL REMINDER:
You are not creating a symbolic allegory. You are creating a WORLD
where these constraints emerge naturally from the cultural, physical,
and economic realities.

The Omega tracking is your internal process for making that world specific,
researched, and complete. The Stage 4 writer should be able to write in this
world without ever thinking about constraint classification OR worldbuilding
uncertainties—because you've resolved everything in advance.

---

### STAGE 3: EDITORIAL DECISIONS

**PROTOCOL:** ChatGPT receives Stage 2 output:

```
You are making editorial decisions for a narrative that preserves 
constraint logic in a specific context.

CONSTRAINT SPECIFICATION: [Stage 1 output]
CONTEXT DESIGN: [Stage 2 output]

REFERENCE: [attach 02_ops.md for decision framework]

DECISIONS REQUIRED:

1. LENGTH TARGET
   □ Flash Fiction (500-1000 words)
   □ Short Story (2000-5000 words)
   □ Novelette (7500-17,500 words)
   
   Justification: [based on constraint complexity]

2. POINT OF VIEW
   □ First Person (inside error loop)
   □ Third Limited (alongside misclassification)
   □ Third Omniscient (can see all constraints)
   □ Second Person (implicate reader)
   
   Justification: [how POV serves constraint revelation]

3. TENSE
   □ Present (immediacy, no escape)
   □ Past (inevitability established)
   □ Future (prediction collapsed)
   
   Justification: [relationship to terminal attractor]

4. VOICE ARCHETYPE
   □ The Guide (explains while it happens)
   □ The Witness (observes without judgment)
   □ The Condemned (inside the logic)
   □ The System (speaks as constraint itself)
   
   Justification: [which voice serves constraint dynamics]

5. PACING
   Full scenes required for:
   - [Constraint detection moment]
   - [Error cascade initiation]
   - [Terminal attractor reached]
   
   Summary acceptable for:
   - [Repeated failed adaptations]
   - [Gradual transformations]

6. CHARACTER COUNT
   Named characters: [1-3 recommended]
   - Agent A (primary): [role in constraint system]
   - Agent B (secondary): [role in constraint system]
   - System C (abstracted): [collective/institution]
   
   Justification: [minimum needed to show constraint interactions]

7. NAMING STRATEGY
   □ Cultural names from context (personal)
   □ Role-based names (functional)
   □ Abstract names (systemic)
   □ Code names (depersonalized)
   
   Approach: [serves constraint visibility how?]

8. LINGUISTIC IMPLEMENTATION (if Stage 2 used Option C)
   Density: [percentage of non-English elements]
   Translation: [inline/contextual/footnoted/none]
   Code-switching pattern: [when/why]

9. EMOTIONAL CORE
   Primary emotion: [from constraint dynamics]
   - Trapped certainty
   - Inevitable approach
   - Recognition without escape
   - Structural resignation
   
   Avoid: [emotions not justified by constraint logic]

10. ENDING STRATEGY
    □ Terminal attractor reached
    □ Error recognized but too late
    □ System equilibrium established
    □ Constraint persists, agent removed
    
    Justification: [logical terminus of constraint cascade]

11. MODEL SELECTION FOR STAGE 4
    Recommended: [Claude/Grok/other]
    Because: [reasoning for model choice based on requirements]

OUTPUT:
Complete editorial specification ready for Stage 4 writer.
```

---

### STAGE 4: NARRATIVE GENERATION

**PROTOCOL:** Selected model receives Stages 1-3 (NOT Stage 0, NOT original story):

```
You are writing a story based on constraint logic and editorial specifications.

CONSTRAINT MECHANICS: [Stage 1 output]
CONTEXT & WORLD: [Stage 2 output]
EDITORIAL DECISIONS: [Stage 3 output]

---

CRITICAL REQUIREMENT: FRAMEWORK INVISIBILITY

The constraint framework (Mountains, Ropes, Nooses, Scaffolds, Zombies) 
is a STRUCTURAL TOOL. It should be completely invisible in your story.

FRAMEWORK VISIBILITY CHECK:

If you notice yourself wanting to:
□ Use the words "mountain," "rope," "noose," "scaffold," "zombie"
□ Say "the system," "the household," "the mechanism" as abstract entities
□ Use phrases like "constraint," "degrees of freedom," "extraction"
□ Explain the constraint logic to the reader
□ Name abstract forces rather than concrete things
□ Make the framework visible in any way

STOP IMMEDIATELY.

Return to Stage 2 context. Ask yourself:
"What is the ACTUAL THING in this world that has these properties?"

---

GOOD vs BAD STORY LANGUAGE EXAMPLES:

Example 1 - Physical Irreversibility:

✗ BAD: "The Mountain was irreversible. The energy was finite. The 
apartment was closed."
- Framework terms visible
- Abstract philosophical language
- Reads like theorem-proving

✓ GOOD: "The radiation counter clicked past red three weeks ago. 
The med-tech said something about bone marrow. He stopped saying 
anything after that."
- Specific sensory detail (counter clicking)
- Concrete phenomenon (radiation, bone marrow)
- Consequence shown, not stated

✗ BAD: "The biological constraint had arrived."
- "Constraint" is framework terminology
- Abstract rather than experienced

✓ GOOD: "His hands didn't work right anymore. Dropped the wrench 
twice before breakfast. By lunch, couldn't grip the handle. By 
dinner, couldn't lift the spoon."
- Progressive physical detail
- No explanation, just showing
- Reader feels the progression

Example 2 - Social Extraction:

✗ BAD: "The Household adapted their coordination protocols around 
his absence."
- "Coordination protocols" is systems language
- Abstract rather than human

✓ GOOD: "Mother started working the early shift. His sister took 
the evening one. They left notes on the counter about bills, about 
groceries. They stopped asking if he wanted anything."
- Specific human actions
- Concrete details (notes, counter, shifts)
- Relationship shift shown through behavior

✗ BAD: "The Noose of debt extraction tightened."
- Framework term ("Noose")
- Metaphorical rather than literal

✓ GOOD: "The company store charged $8 for a bag of flour. Back in 
town, it cost $2. But the wages came in company script, and the 
store was the only place that took it."
- Specific prices, specific currency system
- Mechanism shown, not named
- Reader understands extraction without being told

Example 3 - Character Thought Patterns:

✗ BAD: "He realized he had committed a Type I Error, treating the 
changeable as unchangeable."
- Framework terminology
- Meta-cognitive analysis language

✓ GOOD: "His father had worked for the company. His father's father, 
too. He'd thought that meant something. That it couldn't just... end. 
But the foreman hadn't even looked up from his clipboard."
- Character's actual thought process
- Cultural assumption revealed
- Generational belief shown
- Moment of recognition without naming the error type

✗ BAD: "She misclassified the constraint, believing coordination was 
possible when extraction was the true dynamic."
- Analysis language
- Framework visible

✓ GOOD: "She kept thinking if they all just talked it through, worked 
together, they could fix it. But every meeting ended the same way: 
the manager took notes, nodded, said he'd look into it. Next week, 
nothing changed. Week after, someone got fired for asking."
- Character belief shown through action
- Pattern of disappointment
- Extraction mechanism revealed through repetition
- No framework terminology

---

WRITING REQUIREMENTS:

1. CONSTRAINT FIDELITY
   - Every constraint from Stage 1 must appear as natural story element
   - Measurements (δ, ε, β, Δ) must be consistent in story logic
   - Transformation rules must trigger as specified in Stage 1
   - Terminal attractor must be reached through logical consequence
   
   BUT: Show these through STORY EVENTS, not by naming them

2. NATURALIZATION DISCIPLINE
   - Framework must be INVISIBLE
   - No abstract terminology (see bad examples above)
   - Constraints appear as natural consequences of the world
   - Reader should not think "this is about constraint theory"
   - If you catch yourself explaining, stop and show instead

3. GENRE COMMITMENT
   - If noir, be noir (don't wink at reader)
   - If historical, be historical (period-accurate detail)
   - If linguistic experiment, be consistent (don't code-switch randomly)
   - Stay in the world completely

4. QUALITY STANDARDS
   - This must be a good story independent of framework
   - Surprise within constraints (creativity in implementation)
   - Emotional truth (not just logical truth)
   - Sensory grounding (not abstract philosophical exercise)

5. VOICE CONSISTENCY
   - Maintain archetype from Stage 3 throughout
   - No breaking of fourth wall unless explicitly specified
   - Tone should serve constraint revelation, not explain it
   - Let the reader discover the pattern

---

LINGUISTIC GUIDANCE (if applicable):
[Stage 2 vocabulary, grammar notes, code-switching patterns]

When using linguistic innovation:
- Maintain consistency in mixing patterns
- Code-switch at natural points (emphasis, intimacy, power dynamics)
- Don't explain the language mixing to the reader
- Let context make meaning clear

---

WRITING PROCESS:

1. ESTABLISH WORLD (Opening)
   - Sensory, specific, immediate
   - No explanation, just immersion
   - Introduce one constraint through concrete detail

2. INTRODUCE CHARACTER IN SYSTEM (Early)
   - Show their position through action
   - Reveal relationships through behavior
   - Let constraints emerge from their situation

3. SHOW MOUNTAIN FIRST (Foundation)
   - The unchangeable anchors everything
   - Make it concrete, not abstract
   - Physical or social, but ACTUAL

4. REVEAL NOOSE MECHANISM (Complication)
   - Extraction pattern through repeated interactions
   - Asymmetry shown, not told
   - Reader realizes the trap before character does (or with them)

5. DISPLAY ERROR CASCADE (Rising Action)
   - Misclassification consequences compound
   - Each choice makes sense but worsens situation
   - Character logic is consistent but tragic

6. TRACK ENERGY DEPLETION (Arc)
   - Physical or social exhaustion
   - Progressive loss of options
   - Reader feels the closing in

7. REACH TERMINAL ATTRACTOR (Conclusion)
   - Logical endpoint given all constraints
   - Feels inevitable in retrospect
   - No escape hatch, no deus ex machina

---

ABSOLUTE PROHIBITIONS:

DO NOT:
- Use framework terminology in narrative or dialogue
- Quote Stage 1 specification language
- Make constraints obviously symbolic or allegorical
- Explain what you're doing to the reader
- Reference the original source material in any way
- Use abstract nouns like "the system" or "the mechanism"
- Break genre to explain constraint logic
- Let the theoretical framework show through

Instead:
- Show constraints through specific details
- Let reader discover patterns
- Stay in the world completely
- Trust the structure to do its work

---

FINAL CHECKLIST BEFORE SUBMITTING:

□ Search your text for: "mountain," "rope," "noose," "scaffold," "zombie"
  - If found: REVISE all instances
  
□ Search for: "constraint," "system," "mechanism," "protocol," "extraction"
  - If used abstractly: REVISE with concrete details
  
□ Read through character dialogue
  - Do they sound like people or like constraint theorists?
  - If theorists: REVISE to natural speech
  
□ Check for explanatory passages
  - Are you showing or telling?
  - If telling: REVISE to showing
  
□ Verify genre commitment
  - Does anything break the world's reality?
  - If yes: REVISE to stay in-world

---

OUTPUT:
Complete story at target length with:
- Strong opening (establish world + constraint without naming it)
- Error cascade (show misclassification consequences through events)
- Terminal attractor (reach logical conclusion through character actions)
- Sensory grounding (feels real, concrete, not theoretical)
- Framework completely invisible (reader never thinks "constraint theory")
```

---

### OPTIONAL STAGE 5: PHENOMENOLOGICAL DEEPENING

**PROTOCOL:** Claude receives Stage 4 output and 07_phenomenology.md:

```
You are adding sensory and phenomenological depth to a narrative 
without changing its structure or logic.

ORIGINAL STORY: [Stage 4 output]
REFERENCE: [07_phenomenology.md]

TASK: Enhance visceral experience while preserving constraint logic.

ADDITIONS:
1. Sensory detail (sight, sound, smell, touch, taste)
2. Physical sensation (proprioception, pain, exhaustion)
3. Temporal experience (duration felt vs elapsed)
4. Spatial awareness (enclosure, distance, boundaries)
5. Embodied metaphor (constraint as physical experience)

CONSTRAINTS:
- Do NOT change plot points
- Do NOT alter constraint mechanics
- Do NOT add new information
- DO make existing moments more visceral
- DO ground abstract concepts in body experience

TECHNIQUE:
For each major scene:
- What does constraint FEEL like physically?
- How does error manifest in body?
- What sensations accompany energy depletion?
- How is terminal state experienced corporeally?

OUTPUT:
Enhanced version with deepened phenomenology.
```

---

## VALIDATION CRITERIA

### Success Metrics

**Structural Isomorphism:**
- All constraints from Stage 0 present in Stage 4
- Transformation rules execute as specified
- Terminal attractor reached per Stage 1
- Error cascades follow logical paths

**Surface Unrecognizability:**
- Setting completely different
- Characters different names/roles/descriptions
- Genre different
- No direct plot correspondence
- Reader cannot identify source without being told

**Narrative Quality:**
- Stands alone as good story
- Emotional impact independent of framework
- Surprising within constraints
- Professional publication quality

### Failure Patterns

**Framework Visibility:**
- Characters think/speak in constraint terminology
- Abstract names ("The System," "The Household")
- Philosophical asides explaining logic
- Story reads as illustration of theory

**Constraint Violation:**
- Mountains become negotiable
- Nooses benefit everyone
- Energy depletion doesn't matter
- Terminal attractor avoided

**Generic Implementation:**
- Setting feels generic/underspecified
- Culture has no specific features
- Language is standard English with no character
- Could happen anywhere/anytime

---

## CREATIVITY AMPLIFICATION STRATEGIES

### Strategy 1: Linguistic Innovation

**Why LLMs Excel:** Models trained on multiple languages can:
- Generate authentic-sounding creoles
- Maintain period-appropriate syntax
- Code-switch with cultural accuracy
- Translate idioms while preserving conceptual structure

**Example Applications:**

**Tang Dynasty Chinese → English:**
```
Source: 塞翁失马，焉知非福
Literal: "Frontier old-man lose horse, how know not blessing"
In story: "The border elder lost his horse—who could say it was not fortune?"
Effect: Preserves syntactic structure, creates distinctive voice
```

**Vietnamese-Somali-English Creole (Europa):**
```
"Bác already told you, dheh vacuum breach no forgive. Đụng vào that seal line, you finish-o."
Translation: "Uncle already told you, the vacuum breach doesn't forgive. Touch that seal line, you're finished."
Mixing pattern: Vietnamese kinship + Somali emphasis + English technical + Vietnamese verb + Somali finality
```

**Polish-English Code-Switching (1920s Chicago):**
```
"Na pewno the boss says overtime, but gdzie są dollars? In his pocket, nie our."
Translation: "Certainly the boss says overtime, but where are the dollars? In his pocket, not ours."
Pattern: Switch at emphasis/irony points, preserve Polish pragmatics
```

### Strategy 2: Historical Displacement

**Leverage Model Knowledge Of:**
- Specific historical periods with documented constraint systems
- Economic formations (feudalism, early capitalism, colonial extraction)
- Social structures (examination systems, guild systems, company towns)
- Cultural practices (honor systems, debt relationships, patronage)

**Example Periods Rich in Natural Constraints:**
- Song Dynasty examination system (960-1279)
- Edo Period rigid class structure (1603-1868)
- British factory system (1780-1850)
- American company towns (1890-1930)
- Soviet collective farms (1930s-1950s)

### Strategy 3: Genre Mastery

**Use Model Training On:**
- Noir conventions (fatalism, entrapment, moral ambiguity)
- Gothic atmosphere (enclosure, decay, inheritance)
- Social realism (economic determination, class consciousness)
- Science fiction (extrapolation, alienation, system critique)
- Magical realism (constraint as magic, error as curse)

### Strategy 4: Cultural Specificity

**Draw On Model Knowledge Of:**
- Face/honor systems (East Asian constraint networks)
- Debt/obligation cultures (gift economies, patronage)
- Purity/pollution systems (caste, religious law)
- Kinship obligations (clan systems, family duty)
- Colonial relationships (extraction, hybridity, resistance)

---

## COMPARISON TO AXIOM ENGINE

### Similarities
- Multi-model pipeline with validation gates
- Air gap prevents pattern matching
- Stage separation enforces independence
- Bartleby Protocol (refusal authorized)
- Constraint fidelity over elaboration

### Differences

**Axiom Engine:**
- Input: Mathematical structure
- Output: Narrative about math
- Goal: Pedagogical (teach math through story)
- Success: Reader understands mathematical concept

**UKE_Narrative:**
- Input: Story
- Output: Different story
- Goal: Artistic (good story + philosophy proof)
- Success: Reader doesn't recognize source, story stands alone

**Key Distinction:**
Axiom Engine makes implicit explicit (math → story reveals structure).
UKE_Narrative keeps implicit implicit (story → story preserves structure).

The math needed clean transfer to prove it could be done.
The story needs deep naturalization to prove constraint logic is real.

---

## APPENDICES

### Appendix A: Deferential Realism Quick Reference
[Include condensed version of deferential_realism_logic.md]

### Appendix B: Model Selection Guide
[Based on blind_mirror_test, architectural_profiling]

**Constraint Logic Extraction:** Gemini
- Strong logical analysis
- Good at formal classification
- Systematic decomposition

**Operational Specification:** Copilot
- Mathematical precision
- Formal syntax accuracy
- Theorem-proof structure

**Context Design:** Claude
- Creative worldbuilding
- Cultural knowledge breadth
- Linguistic flexibility

**Editorial Decisions:** ChatGPT
- Narrative craft expertise
- Genre awareness
- Professional writing standards

**Narrative Generation:** Context-dependent
- Literary fiction: Claude
- Genre fiction: Grok
- Experimental: Gemini
- Noir/hard-boiled: ChatGPT

### Appendix C: Sample Constraint Mappings

**Metamorphosis:**
- Mountain: Biological transformation (irreversible)
- Noose: Family extraction (debt to employer)
- Rope → Zombie: Family coordination breaks down
- Type I Error: Gregor treats Noose as Mountain
- Type II Error: Gregor treats Mountain as temporary

**Could Become:**
- Europa: Radiation exposure, oxygen debt, clan dissolution
- Tang Dynasty: Examination failure, patronage collapse, family shame
- 1920s Chicago: Industrial injury, company debt, neighborhood fragmentation

**1984:**
- Mountain: Surveillance state infrastructure
- Noose: Party extraction of labor/loyalty
- Rope → Noose: Love relationship captured by system
- Type I Error: Winston treats Party power as negotiable
- Type III Error: Winston treats O'Brien's approach as Rope

**Could Become:**
- Edo Period: Shogunate spy system, domain taxation, forbidden love
- Soviet 1950s: Informant network, labor quotas, political romance
- Future Mars: AI monitoring, oxygen rationing, prohibited bonding

### Appendix D: Common Failure Modes

**Problem:** Story feels like allegory
**Solution:** Add 3x more specific cultural/sensory detail

**Problem:** Constraints are too visible
**Solution:** Stage 2 naturalization insufficient, redo with more specificity

**Problem:** Ending doesn't match constraint logic
**Solution:** Stage 4 writer violated Stage 1 mechanics, regenerate with stricter adherence

**Problem:** Linguistic creativity feels gimmicky
**Solution:** Increase density, add cultural justification, make it necessary not decorative

**Problem:** Setting feels generic
**Solution:** Stage 2 needs specific historical moment, not general time period

---

## USAGE GUIDE

### For Single Story Translation

1. Select source story with clear constraint dynamics
2. Run Stage 0 (Gemini + deferential_realism_logic.md)
3. Run Stage 1 (Copilot + 01_spec.md)
4. Run Stage 2 with creativity parameters
5. Run Stage 3 (ChatGPT + 02_ops.md)
6. Run Stage 4 with selected model
7. Optional: Run Stage 5 (Claude + 07_phenomenology.md)

### For Batch Processing

Design template for Stage 2 (e.g., "All stories become Europa colony stories") then run pipeline on multiple source stories to generate series.

### For Validation

Compare Stage 4 output to Stage 1 specification:
- Map each constraint to story element
- Verify transformation rules execute
- Confirm terminal attractor reached
- Check error types manifest correctly

### For Iteration

If Stage 4 fails validation:
- If constraint violation: Regenerate Stage 4 with stricter prompt
- If framework visible: Redo Stage 2 with deeper naturalization
- If quality poor: Change model for Stage 4, adjust Stage 3 decisions

---

## LICENSE & ATTRIBUTION

This protocol is released under CC BY-SA 4.0.

Builds on:
- Axiom Engine v2.2 (constraint analysis methodology)
- UKE_Axiom v1.2 (multi-model pipeline architecture)
- Deferential Realism (constraint classification framework)
- Blind Mirror Test (model capability profiling)

Attribution: Please cite as:
"UKE_Narrative v1.0: Universal Knowledge Evaluator - Narrative Constraint Translation (2026)"

---

## VERSION HISTORY

**v1.0 (January 2026):**
- Initial release
- Integrated deferential_realism_logic at each stage
- Added creativity amplification strategies
- Specified linguistic innovation options
- Established validation criteria
- Clean sequential stage numbering

---

**END OF PROTOCOL**
