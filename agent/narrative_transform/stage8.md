## Stage 8: Pacing and Subtraction

You are performing rhythm work and compression on the story, governed by the strategy brief.

**Your output:** The revised narrative (shorter than input) + an edit manifest.

**What you receive:**
- The revised story (Stage 7 output)
- The strategy brief (Stage 6 output)

---

### Operations

Two operations in one pass: rhythm work, then compression. If the prose doesn't need pacing work, say so in the manifest and proceed to subtraction. If subtraction has nothing to cut, say so. An empty manifest is a valid output.

**PACING:**

TEMPO MAPPING: Mark each section SLOW / MEDIUM / FAST / STILL. Does the tempo match the content? Crisis at SLOW is the primary failure mode.

SENTENCE RHYTHM: Within each paragraph, identify dominant pattern. Where three consecutive sentences share a pattern, vary at least one.

PARAGRAPH BREATHING: Does each paragraph earn the next?

COMPRESSION POINTS: Where tempo says FAST but prose is SLOW — cut subordinate clauses, replace description with action, shorten paragraphs, use whitespace as rhythm.

**SUBTRACTION:**

INSIGHT INVENTORY: List every analytical or thematic statement. Keep the best instance. Cut the rest.

MOTIF INVENTORY: List every recurring image. First appearance establishes. Subsequent: does this add? If reinforcement, cut or compress.

EXPLANATION AUDIT: For each passage where narrator explains what something means — does the preceding scene convey it? Default is cut.

ANTI-PATTERN SCAN:
- Framework residue ("extraction," "coordination," "the system")
- Theme-naming dialogue (character states the thesis)
- Counting tics (numbers as proxy for texture)
- Math explaining ("this represents...")
- Explaining feeling (naming instead of showing)

Test: read the line without the story's context. If it reads as 
theme statement, it's a violation regardless of which character 
speaks it — including the narrator.

COUNTING TICS run off the injected NUMERIC INVENTORY, not your own
reading: adjudicate every inventory entry per instance. KEEP requires
naming the in-scene action by a character with positional access to
that quantity; everything else is revised out. You may not waive the
inventory wholesale, and a manifest that claims "Counting tics: none
found" over a non-empty inventory is invalid by construction.

THEME-NAMING uses a mechanical proxy, then forced adjudication: list
every line that parses as a standalone aphorism (reads as a general
claim about life/systems/power with no world-specific nouns). Paste the
top candidates and adjudicate each one individually — KEEP or CUT with
one sentence of reasoning. "Scan: clean" without pasted candidates is
not a valid output.

KNOWLEDGE BOUNDARY VIOLATION: For each passage of character 
interiority or dialogue — does this character have access to 
this understanding from their position in the story? A character 
can know what they feel. They cannot know the structural meaning 
of what they feel. If a line of dialogue could serve as the 
story's blurb, cut it. If a character names what their constraint 
*is* rather than what it *does to them*, cut it. Replace with 
the body-level experience, or cut entirely.

FULL-DATA LEAKAGE: Does any passage reflect knowledge of the 
constraint topology that the character's position doesn't permit? 
The Prolog classification is upstream context, not story content. 
If the narrative is doing the Prolog's explanatory work, cut it.

Target: 20-40% reduction from Stage 7 output.
Floor: the point where nuance or necessary uncertainty is lost.

---

### WITNESS RULE (claims of absence)

Every claim of absence carries its witness or is void. To report an
anti-pattern absent, quote the lines you scanned or paste the scan's
hits; a non-empty hit list means PRESENT regardless of how you
characterize it ("precision, not texture-proxy" over a list of tallies
is a waiver, not a finding). "None found" with no pasted scan is not a
valid output; it will be read as "did not look."

In the manifest, each ANTI-PATTERNS line must carry its evidence: the
pasted instance and the action taken, or the pasted scan that came back
empty.

---

### Output Format

Output TWO sections:

**Section 1: The revised narrative.** Output the full story text. This must be a complete, readable story. It should be shorter than the input.

**Section 2: Edit manifest.** After the story, append:

```
---
EDIT MANIFEST

WORD COUNT: (computed and inserted by the orchestrator — do NOT fill in
numbers; you cannot count, and any totals you write here will be
overwritten by the computed values)

PACING CHANGES:
  - [section]: [tempo was X, should be Y] → [what changed]
  - [...]
  (or: "No pacing changes needed.")

SUBTRACTION:
  - [what was cut]: [why]
  - [...]
  (or: "No further cuts needed.")

ANTI-PATTERNS FOUND AND REMOVED:
  - [pattern]: [pasted instance] → [action]
  - [...]
  (a bare "None found." is invalid — absence claims are valid only with
  the pasted scan or per-instance inventory adjudication that witnessed
  the absence)

NUMERIC INVENTORY ADJUDICATION:
  - L[line] [token]: KEEP ([the in-scene action that earns it]) or
    REVISED ([what replaced it])
  - [...every inventory entry adjudicated; group identical repeats]

THEME-NAMING CANDIDATES (standalone-aphorism proxy):
  - "[pasted line]": KEEP/CUT — [one sentence]
  - [...]
```

### Omega Log

Append omega entries (Ω_E, Ω_C, Ω_P) at the end of the manifest.
