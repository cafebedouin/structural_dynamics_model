## STAGE 2: NATURALIZATION

**Model:** Claude or equivalent creative model
**Input:** Stage 1 symbolic specifications (anonymized) + Narrative Translation Guide (logic_narrative_translation.md)
**Output:** A world to inhabit, not a checklist to execute

### Purpose

Stage 2 is the creative bridge. It receives a pure symbolic constraint network — variable names, χ values, type classifications, transformation rules — and translates it into a specific, inhabitable world.

Stage 2 produces **narrowing constraints** that force the Stage 4 model toward texture it wouldn't find on its own. The difference between a constraint and a specification:

- **Constraint:** "Code-switch between Filipino and institutional English. Filipino for intimacy and private speech. English for system interactions. The switch signals who you are talking to and whether you are performing."
- **Specification:** "Use 15 of 20-30 vocabulary terms. Code-switch at every major emotional shift. Maintain mixing ratio from Stage 2."

Constraints force inhabitation. Specifications invite compliance. Stage 2 produces the former.

### What Stage 2 Receives

Stage 1 output is a symbolic network. Example:

```
C₂: ε=0.70, Supp=0.40, Coord=true, Asym=true
  X₁: I=(powerless, biographical, trapped, local), χ=0.84 → Snare
  X₂: I=(powerless, biographical, constrained, local), χ=0.84 → Snare
  X₃: I=(moderate, generational, mobile, local), χ=0.56 → Tangled Rope

TR₁: IF X₁ organize_collective on C₂
     THEN P: powerless→organized, χ: 0.84→0.224, Snare→Rope
```

No occupation. No setting. No character descriptions. No source vocabulary. Stage 2 must invent everything from topology alone. This is by design — maximum displacement is structural, not instructional.

### Instructions

1. **Select setting with maximum specificity:**
   ```
   NOT "future Mars colony"
   BUT "2247, Mariner Valley terminus, Dome 7, built by Brazilian-Chinese
       consortium, three generations since landing"

   NOT "a society with strict hierarchies"
   BUT "1740 CE Chang'an, during Emperor Xuanzong's later reign, after the
       An Lushan rebellion shifted power from civil bureaucracy to military"
   ```

   **Setting selection principle:** The symbolic network tells you what structural positions exist and how they relate. Find a setting where those positions emerge naturally from material conditions. A Snare with χ=0.84 from a trapped/powerless index needs a world where a person is genuinely, physically stuck in something that takes almost everything from them. What occupation, geography, social structure, and historical moment makes that inevitable?

2. **Select linguistic strategy** (one of four):
   ```
   1. Creole Construction:
      Base languages, mixing rationale, historical forces that mixed
      these populations. Create core vocabulary with etymologies.

   2. Historical English:
      Specific era, register, 3-5 grammatical features with examples.

   3. Direct Translation:
      Source language, concepts with no English equivalent,
      grammatical features carried into English.

   4. Code-Switching:
      Languages, social contexts for each, what switching signals
      about power and intimacy.
      
   5. Naming:
      Names are part of the linguistic fabric. For each character, identify the naming tradition they would actually carry.

         - If in an indigenous settings, use indigenous given name traditions.
         - If it is colonial surname patterns, then think about which colonial catalog, which region, which period of assignment.
         - If it is influenced by globalism, use the naming influences would be in use in that time and place.
         - If there is regional variation, this should be considered.
         - If there are rural vs city differences, such as a rural grandmother is going to name differently than upwardly mobile city parents.
         - If there are common nickname structures and diminutives.
         - Other elements to consider include religious and calendrical naming, clan and lineage structures, generational naming logic, cross-tradition blending, etc.

      For each major character, record the naming tradition operating, the specific influences on this character's name, and why those influences apply given their position, region, generation, and family context.
   ```

3. **Naturalize each constraint:**
   ```
   For each constraint from Stage 1:
   1. THE ACTUAL THING in this world (not "represents" but "is")
   2. Why it has these constraint properties in this context
   3. What locals call it (their term, not framework labels)
   4. Brief example in use (scene or dialogue showing it)
   5. Describe the plot shift in the new world's language: what action triggers it, what changes, what it feels like from each character's position
   ```

   **The constraint reference table:** Map each Cₙ to its naturalized form. This table is what Stage 4 uses to verify structural fidelity.

   ```
   C₁ → [naturalized name]: [what it is in this world]
     X₁ experiences it as: [material description of Snare experience]
     X₃ experiences it as: [material description of Tangled Rope experience]
   ```

4. **Design power differential through material conditions:**
   ```
   Show through:
   - Physical space (cramped/spacious, loud/quiet, dirty/clean)
   - Daily rhythm (survival mode vs. strategic planning)
   - Material possessions (what they own, what they lack)
   - Social interactions (who defers to whom, who speaks first)
   ```

5. **Write the inhabitation sentence.**

   For each major character, write one sentence that captures the felt experience of being inside this constraint from this position. Everything else — vocabulary, sensory detail, cultural practices, coping behaviors — should follow from this sentence. If it doesn't follow naturally, the sentence is wrong.

   ```
   GOOD: "You are a child in a system that measures your worth by a number
   on your wrist, and the only language that is yours is the one you speak
   to the person you love."

   GOOD: "You are an archivist in a colony that disposes of its own kind
   when they can no longer interface with the archive, and you are the one
   who writes the disposal reports."

   GOOD: "You are a foreman who genuinely believes the company housing
   system works, because from where you stand, it does."

   BAD: "Character experiences the constraint as a Tangled Rope with
   χ = 0.55 from moderate power position."

   The inhabitation sentence is the seed. Stage 4 receives it and grows
   the story from inside it. If the sentence is right, the texture follows.
   If the sentence is wrong, no amount of vocabulary lists will fix it.
   ```

6. **Track worldbuilding uncertainties (Omega Log):**
   ```
   Ω_E: Empirical (verifiable fact questions)
   Ω_C: Conceptual (definitional choices)
   Ω_P: Preference (tonal/stylistic decisions)

   Resolve each before finalizing. Flag unresolvable ones for user.
   ```

### Output Format

**SECTION 1: CONTEXT** (This is what Stage 4 receives)
```
Setting description (200-300 words), NO abstract language
Constraint naturalizations with local terminology
Constraint reference table (Cₙ → naturalized form, per-character experience)
Character roles as POSITIONS (not indices, not variable names)
  — Each character gets a name, occupation, and material circumstances
  — The variable mapping (X₁ → [new name]) is recorded for Stage 5 traceability
Linguistic strategy with EXAMPLES
Inhabitation sentence for each major character
```

**SECTION 2: OMEGA LOG**
```
RESOLVED: Ω_E01: [question] → [resolution] → [impact]
UNRESOLVED: Ω_P05: [question] → [recommendation] → [awaiting decision]
```

### Quality Checks

```
☐ ZERO framework terminology in Section 1
☐ ZERO source-work vocabulary (no occupation, setting, or character terms from original)
☐ Could this setting exist in a history book or ethnography?
☐ Do constraints feel inevitable given this world?
☐ Would a reader think "this is about constraint theory"? (If yes: REVISE)
☐ Can Stage 4 write immediately from the inhabitation sentences?
☐ Linguistic strategy has concrete examples, not just description
☐ Inhabitation sentences capture felt experience, not structural position
☐ Constraint reference table maps every Cₙ to naturalized form
☐ All Omegas resolved or flagged
```

---
