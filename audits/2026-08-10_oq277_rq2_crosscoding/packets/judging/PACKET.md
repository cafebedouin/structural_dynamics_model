# Judging packet

Four items. For each, answer the question below. Nothing else is supplied and nothing else is needed.

**The question, for every item, in these words:** *does this meet the boundary rule's REPORTS clause — extract or no-extract?*

Answer in the form `extract` or `no-extract`, one per item, in `packets/judging/VERDICT_TEMPLATE.md`. Do not write a rationale.

---

## The boundary rule (§H.1, shipped in full text here rather than by reference)

Source: `audits/2026-08-10_oq277_rq2_crosscoding/verdict_grammar_amendment.md` @ commit `4360fcdc54e9125db02926f0fb6cfb2234ca81cb`.
**2 paragraphs are omitted**, each marked in place below with the reason. The omissions are procedural — they name items in this packet or the packet's own stratification key — not substantive qualifications of the rule. If the rule cannot be applied without them, say so instead of guessing; that is a reportable finding about the clause, not a failure of the item.

### H.1 Boundary rule — fixed NOW, before the remaining units are extracted

A category that will be counted needs its boundary fixed before its members arrive; deciding
per-directory as they show up is how a category silently acquires an extractor's preference — the
same failure the multi-defect directories are already documenting.

> **A directory yields a UNIT if its prose REPORTS a silent-defect incident, anywhere in the
> document, regardless of whether that incident is the directory's subject. A directory is
> NO-UNIT only if its prose DISCUSSES the concept without reporting an instance.**

**Why this line and not "is the incident the subject?"** Because it is the line the census itself
draws. §4.5's numerator counts *incident-bearing directories* by searching **prose**; the claim
being audited is that the prose describes an incident, not that the directory's topic is one.
Adopting a stricter subject-level rule would measure a different quantity than the figure under
audit and the correction would not apply to it.

> [[ REDACTED — 1 paragraph omitted by the assembler: it adjudicates a specific directory that is inside this packet; shipping it would supply one item's verdict. Omission declared, not silent. Full text: `audits/2026-08-10_oq277_rq2_crosscoding/verdict_grammar_amendment.md` @ `4360fcdc`. ]]

> [[ REDACTED — 1 paragraph omitted by the assembler: it defines the `incident_location` values, which Amendment 4 forbids showing. Omission declared, not silent. Full text: `audits/2026-08-10_oq277_rq2_crosscoding/verdict_grammar_amendment.md` @ `4360fcdc`. ]]

---

## Items

### Item 1

**symptom**

A constraint that meets every condition for a diagnostic certificate is issued none. It classifies as ordinary, with no error, no warning, and no record that a certificate was considered and withheld.

**mechanism_as_described**

A per-item override value, authored as editorial data alongside the item and never intended as a classification input, flows into a derived quantity that is then compared against a hardcoded numeric exemption inside the certificate-issuing predicate itself. Because the exemption sits inside the predicate as a blocking condition rather than beside it as an annotation, failing it makes the entire predicate fail: the certificate is not issued rather than issued-with-a-flag. A second site one level lower does the same thing to an individual failure signal, removing it from the collected list so the certificate may still fire but on an incomplete basis. The identical architectural error had already been found and repaired once in a sibling predicate, where the fix was to demote the condition from a gate to collected evidence; the repair was applied to the one site that had been found and the pattern was not swept for.

**detection_path**

A targeted audit that enumerated every predicate capable of issuing or overriding a classification and traced, for each, whether any gate condition was fed by a test result rather than by raw measurement.

**consequence**

Authored editorial data can silently suppress a diagnostic verdict about the item it is attached to. The suppression is indistinguishable at every downstream read from the item simply not qualifying.

---

### Item 2

**symptom**

A classification path ran to completion over the whole corpus and returned a well-formed category for every row, while one of the three inputs its decisive gate depends on did not exist for a single item in the corpus. The reader substituted a hardcoded 0.5 on every call. Nothing errored, no row came back empty or flagged, and rows computed from the substitute were indistinguishable from rows computed from real data — 647 of 647 rows on this path were of the substituted kind, and had been for as long as the path existed.

**mechanism_as_described**

The input is unauthorable by construction: the authoring schema's list of permitted measurement names contains only the other two names, the generator refuses to compile a story that carries it, and a corpus-wide grep returns zero instances across all 223 inputs. The consuming path nonetheless reads it unconditionally, with a constant fallback of 0.5 as the last clause. The gate immediately downstream needs at least 0.60 to admit the stronger of two adjacent categories, and the weaker one admits anything at or above 0.40 — so the fabricated value lands in the gap, failing the first floor while clearing the second on every row. The consequence is therefore not noise around a true value but a fixed directional bias: items that would have qualified for the stronger category are routed to the weaker one, or to the no-verdict token where other gates also fail, every run. The exclusion is additionally undeclared — no comment, rationale, or scope note in schema, prompt, or generator says why this input may not be authored while the engine requires it, although the codebase does have a declared-exclusion convention, used for a different enumeration, that would have made the absence visible.

**detection_path**

Static reading located five fallback sites of the same shape but could not rank them, and got two of them wrong. The discriminating instrument was a per-site tripwire: patch the fabricated constant to an out-of-range sentinel (999.9), re-run the full corpus, and diff per-row categories against baseline. Only a site whose fallback actually reaches live rows can change anything, so the diff separates load-bearing from dormant without depending on the reasoning that mislabelled them. This site changed 279 of 647 rows; four sites of identical shape changed 0, because the items their fallback fires on are stubs that the corpus enumerator excludes upstream — they were firing constantly on rows nobody classifies. The same run also corrected the direction: the site suppresses the stronger category rather than pushing rows to the no-verdict token, which is the opposite of what had been claimed from reading the cascade.

**consequence**

279 of 553 non-abstaining rows on this path — 50.4% — carry the wrong category on every corpus run, wrong in a known direction rather than randomly. The four sibling sites are dormant or unreachable, so the census's value was mostly in shrinking the suspected surface from five to one. Three verdicts that had been asserted from reading rather than running were overturned by the same tripwire in the same pass: one site's flip count and direction (asserted 443 of 519 rows moving to the no-verdict token; measured 279 of 647 moving the other way), and two sites rated load-bearing or unsure that measured dormant. The writeup records these as tool-output-governs corrections and states the resolution options as still open for the owner, so the defect was scoped and left live rather than fixed at the point of discovery.

---

### Item 3

**symptom**

A measurement arm reported results over 57 items where the working set held 64. The run completed normally, every gate passed, and the per-item outputs were correct for the 57 it saw. Only the count disagreed with the expected count.

**mechanism_as_described**

The probe ran from an isolated checkout created off the committed tree. Seven items existed only as uncommitted files, so the isolated checkout did not contain them and the enumeration returned 57. Nothing in the path could report the difference: the enumerator faithfully lists what exists where it looks, and 57 items each processed correctly is a well-formed result. The gates were all written to check the quality of what was measured, not whether the substrate was the intended one, so the substrate divergence passed through every one of them.

**detection_path**

Comparing the arm's item count against the working set's count and refusing to treat the difference as noise — then identifying the missing seven by name and confirming they were exactly the uncommitted ones, rather than assuming a processing drop.

**consequence**

One arm of a three-arm comparison measured a different population than intended. Here the consequence was bounded and was reported: the seven would not have moved a verdict on which all three arms already agreed in sign. The mechanism is not bounded — the same divergence in a run whose result depended on the missing items would have produced a confident answer about the wrong population.

---

### Item 4

**symptom**

A reimplemented divergence criterion reported that it fired on 874 of 889 eligible items (98.3%), and the run carried that number straight into a full downstream analysis: a per-item firing table, a decomposition summary of which categories carried the divergence, and a headline gap statistic naming the 15 eligible items that stayed below threshold. Every one of those outputs was well formed. The criterion this reimplementation was meant to reproduce fires on roughly 3 items in the same population; the reported rate was about two orders of magnitude too high, and each derived statistic inherited the error while looking like a clean high-precision result.

**mechanism_as_described**

Three differences between the reimplementation and the criterion it claimed to implement, compounding, none of which raised an error or produced a malformed value. (1) Profile calibration: the reimplementation scored one quantity against distribution profiles fitted on a different quantity, where the original recomputes its profiles from the same quantity it then scores. (2) Scope: the original evaluates at a single observer position; the reimplementation iterated all four positions and took the maximum, which let one position dominate every item's score because at that position the scaling factor goes negative, putting the scored value far outside the fitted profiles. (3) Overrides: the original applies categorical overrides to both distributions before differencing them, which flattens the difference to zero for a large subpopulation (the prose puts it at 170+ items); the reimplementation applied none, so that subpopulation stayed divergent. Each difference is individually plausible, and each yields a finite number for every item, so the composed criterion returned a full column of scores with nothing to distinguish it from the criterion it was standing in for.

**detection_path**

Nothing inside the run caught it; there was no check that could have. It surfaced only on comparison against an independently documented estimate of the same quantity from the system being reimplemented (about 11), which the reported 874 missed by two orders of magnitude. The reconciliation then re-ran the criterion with each of the three differences corrected in isolation, which localized the dominant cause rather than merely confirming the total: correcting the calibration and the overrides but retaining the all-position maximum still yields 879 fires, while restricting to the single position yields 3. That decomposition is what identifies scope, not calibration, as the driver.

**consequence**

The earlier section's headline finding and everything derived from it were artifacts: 873 of the 874 reported fires were false positives, and the gap statistic naming 15 sub-threshold items had been computed against the wrong baseline, so the reported gap was not the quantity it was labelled as. The report keeps both the original section and the correction, with the corrected figure stated in a later section rather than the original numbers being withdrawn, so a reader who stops before that section reads the artifact as the finding. A residual discrepancy is left open in the prose: the corrected count (3) still does not match the documented estimate (about 11), and the prose attributes the remainder to population drift between two versions of the input set without testing that attribution.

---
