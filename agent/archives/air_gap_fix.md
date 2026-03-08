# Air Gap Fix: Stage 1 Anonymization + Stage 2 Displacement Constraint

## 1. Orchestrator: `_anonymize_stage_1` method

Add this method to the `UKENarrativeOrchestrator` class, after `_run_stage_1`:

```python
    # ── Air Gap: Anonymize Stage 1 before downstream stages ──────────

    @staticmethod
    def _anonymize_stage_1(stage_0_output: str, stage_1_output: str) -> str:
        """Strip source-identifying content from Stage 1 before passing downstream.

        Stage 0's XML contains character names in <character name="X"> tags.
        Stage 1's formalization inherits these names plus source title and
        author references. All are gravity wells that prevent Stage 2 from
        achieving genuine setting displacement.

        Returns anonymized Stage 1 text with:
        - Character names → structural role labels (Agent_A, Agent_B, ...)
        - Source title stripped from headers
        - Author name references removed
        """
        import re

        text = stage_1_output

        # ── Extract character names from Stage 0 XML ──
        # Matches: <character name="Santiago">, <character name="The Marlin">, etc.
        names_raw = re.findall(
            r'<character\s+name="([^"]+)"', stage_0_output
        )
        # Deduplicate preserving order
        seen = set()
        names = []
        for n in names_raw:
            if n not in seen:
                seen.add(n)
                names.append(n)

        # ── Build replacement map ──
        # Assign structural labels: Agent_A, Agent_B, ...
        labels = [f"Agent_{chr(65 + i)}" for i in range(len(names))]
        name_map = dict(zip(names, labels))

        # Also handle possessives and common variants
        replacements = {}
        for name, label in name_map.items():
            # Full name and possessive
            replacements[name + "'s"] = label + "'s"
            replacements[name + "'s"] = label + "'s"
            replacements[name] = label

            # Handle "The X" patterns (e.g., "The Marlin")
            if name.startswith("The "):
                short = name[4:]  # "Marlin"
                replacements[short + "'s"] = label + "'s"
                replacements[short + "'s"] = label + "'s"
                replacements[short] = label

            # Handle "X's Parents" compound references
            poss_parent = name + "'s Parents"
            if poss_parent in text:
                replacements[poss_parent] = label + "_guardians"

        # Sort by length descending so "Manolin's Parents" matches before "Manolin"
        for old, new in sorted(replacements.items(), key=lambda x: -len(x[0])):
            text = text.replace(old, new)

        # ── Strip source title from headers ──
        # Matches lines like: "## The Old Man and the Sea - Operational Constraint Model"
        # or "# STAGE 1: FORMAL SPECIFICATION\n## <Title> - ..."
        text = re.sub(
            r'^(#+\s*(?:STAGE\s+1[:\s].*?\n)?)##\s+.+?[-–—]\s*Operational Constraint Model',
            r'\1## Operational Constraint Model',
            text,
            flags=re.MULTILINE,
        )
        # Also catch standalone title headers
        text = re.sub(
            r'^##\s+.+?[-–—]\s*Operational\b',
            '## Operational',
            text,
            flags=re.MULTILINE,
        )

        # ── Strip author references ──
        # Common patterns: "Hemingway's pessimism", "as in Hemingway", etc.
        # Extract likely author names: proper nouns that appear in narrative
        # function sections but not in constraint formulas
        # Hardcoded common ones + dynamic extraction from context
        author_patterns = [
            r"Hemingway(?:'s)?",
            r"Fitzgerald(?:'s)?",
            r"Kafka(?:'s)?",
            r"Orwell(?:'s)?",
            r"Dostoevsky(?:'s)?",
            r"Tolstoy(?:'s)?",
            r"Wister(?:'s)?",
        ]
        for pat in author_patterns:
            text = re.sub(pat, "the source author", text, flags=re.IGNORECASE)

        # ── Strip explicit source title mentions in prose ──
        # "The Old Man and the Sea" or similar italicized/quoted titles
        text = re.sub(
            r'\*[^*]+\*\s*(?:by\s+the source author)?',
            'the source text',
            text,
            count=0,  # all occurrences
        )

        # ── Log what was anonymized ──
        # (Orchestrator can use this for debugging)
        anon_note = (
            f"\n\n<!-- ANONYMIZATION: {len(name_map)} character names replaced: "
            f"{', '.join(f'{k}→{v}' for k, v in name_map.items())} -->\n"
        )
        text += anon_note

        return text
```

## 2. Orchestrator: Flow change in `_run_narrative`

Replace lines 873-883 (the Stage 2 block) with:

```python
        # ── Stage 2: Naturalization (Claude) ──────────────────────────
        if start_idx <= 2:
            stage_1_out = result.stage_outputs.get("stage_1", "")
            stage_0_out = result.stage_outputs.get("stage_0", "")

            # Anonymize Stage 1 to prevent source identity leaking into
            # Stage 2's setting design. This is the primary air gap fix.
            stage_1_anon = self._anonymize_stage_1(stage_0_out, stage_1_out)
            result.stage_outputs["stage_1_anon"] = stage_1_anon
            self._save_stage_output("stage_1_anon", stage_1_anon, result)

            step = self._run_stage_2(stage_1_anon)
            result.steps.append(step)
            if step.status == "error":
                result.total_duration_s = time.time() - t0
                self._tally(result)
                return result
            result.stage_outputs["stage_2"] = step.data
            self._save_stage_output("stage_2", step.data, result)
```

And update the Stage 3, 4, 5 blocks to use anonymized Stage 1:

```python
        # ── Stage 3: Editorial Decisions (Claude) ─────────────────────
        if start_idx <= 3:
            # Use anonymized Stage 1 if available, else raw
            stage_1_out = result.stage_outputs.get("stage_1_anon",
                          result.stage_outputs.get("stage_1", ""))
            stage_2_out = result.stage_outputs.get("stage_2", "")
            step = self._run_stage_3(stage_1_out, stage_2_out)
            ...

        # ── Stage 4: Narrative Generation (Claude, AIR GAP ENFORCED) ──
        if start_idx <= 4:
            stage_1_out = result.stage_outputs.get("stage_1_anon",
                          result.stage_outputs.get("stage_1", ""))
            stage_2_out = result.stage_outputs.get("stage_2", "")
            stage_3_out = result.stage_outputs.get("stage_3", "")
            step = self._run_stage_4_narrative(stage_1_out, stage_2_out, stage_3_out)
            ...

        # ── Stage 5: Subtractive Audit (Claude) ──────────────────────
        if start_idx <= 5 and not self.skip_final_audit:
            stage_4_out = result.stage_outputs.get("stage_4", "")
            stage_1_out = result.stage_outputs.get("stage_1_anon",
                          result.stage_outputs.get("stage_1", ""))
            step = self._run_stage_5_narrative(stage_4_out, stage_1_out)
            ...
```

## 3. Stage 2 prompt: Displacement constraint

In `_run_stage_2`, add to the final prompt_parts.append block:

```python
        prompt_parts.append(
            "Follow the naturalization protocol in your system instructions. "
            "Create a setting where these exact constraints naturally occur. "
            "Output TWO sections:\n"
            "Section 1: CONTEXT DESCRIPTION (clean, no Omega markers, no framework terms)\n"
            "Section 2: OMEGA LOG (tracking & resolution record)\n\n"

            # ── DISPLACEMENT CONSTRAINT (new) ──
            "DISPLACEMENT REQUIREMENT: The setting must differ from any likely "
            "source material in at least TWO of the following: occupation/profession, "
            "century/era, culture/region, governing institution. If the constraint "
            "specification describes agents in a fishing community, the setting "
            "CANNOT be a fishing community. If the agents operate in a 20th-century "
            "village, the setting CANNOT be a 20th-century village. The structural "
            "topology must be preserved; the surface must be unrecognizable. "
            "Think: same bones, completely different body.\n\n"

            "The setting must be temporally/culturally displaced from any likely source. "
            "Framework must be INVISIBLE. Power differentials must be "
            "naturalized through setting structure."
        )
```

## 4. Stage 2 protocol addition (stage2.md)

Add after the "Select setting with maximum specificity" instruction:

```markdown
   **DISPLACEMENT RULE:** The setting must differ from the constraint
   specification's implied source in at least TWO of:
   - Occupation/profession (if source agents fish, output agents do not fish)
   - Century/era (if source is 20th century, output is not 20th century)
   - Culture/region (if source is Caribbean/Latin American, output is not)
   - Governing institution (if source has cooperative, output has different structure)

   The constraint topology must be preserved — same power asymmetries,
   same extraction dynamics, same indexical variance. But the surface
   must be unrecognizable as the source material. This is transformation,
   not relocation.

   Test: Could a reader who knows the source identify it from the setting
   alone? If yes, displace further.
```

## 5. Stage 5 (subtractive audit): Origin check strengthening

The existing Stage 5 should already catch this, but it didn't. Add to the
Stage 5 prompt in `_run_stage_5_narrative`:

After "Apply the subtractive audit protocol", add:

```python
            "CRITICAL: The origin obfuscation check must be rigorous. "
            "Ask: would a reader familiar with the Western literary canon "
            "recognize the source? Check: character names, occupation, setting, "
            "plot beats, relationship structures, and iconic imagery. "
            "If ANY of these are recognizably derived from a known work, "
            "the story FAILS origin obfuscation regardless of other qualities. "
            "A fishing story about an old man and a boy with a marlin and sharks "
            "is recognizable. Do not pass it.\n\n"
```

---

## Summary of changes

| Component | Change | Purpose |
|-----------|--------|---------|
| Orchestrator method | `_anonymize_stage_1()` | Strip names, title, author from Stage 1 |
| Orchestrator flow | Use anonymized Stage 1 for Stages 2-5 | Prevent gravity wells |
| Stage 2 prompt | Displacement constraint | Force 2-of-4 surface differences |
| stage2.md protocol | Displacement rule | Permanent protocol documentation |
| Stage 5 prompt | Origin check hardening | Prevent false "not recognizable" passes |

## What this does NOT fix

- Stage 0 still uses character names (fine — it's before the air gap)
- Stage 1 raw output still saved for debugging (fine — not passed downstream)
- The author-name regex list is incomplete (add as new sources are tested)
- Very famous structural signatures (old man + boy + sea + endurance) might
  survive anonymization through structural description alone — the displacement
  constraint in Stage 2 is the belt-and-suspenders for this case
