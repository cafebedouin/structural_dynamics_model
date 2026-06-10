# OQ-94 read-site pass — sorting the beneficiary/coordination consumer surface by the ruled per-site rule

**Date:** 2026-06-10. **Rule applied (OQ-94, ruled 2026-06-10):** sort each read-site by *which
question the read asks* — mountain-likeness → capture-gating sound; coordination-despite-extraction
→ gating forbidden, split signal required; sites the rule does not cleanly sort → escalate.

## Probe-scope correction first (self-caught this pass)

The OQ-94 "seven-consumer list" was **truncation-bounded, not complete**: the original census ran
`grep -rn has_coordination_function prolog/*.pl | ... | head -15`. The untruncated re-run (this
pass; raw output below) finds **12 files, 33 sites** — the truncation concealed `drl_core.pl`
(the classification cascade itself, sites :346 and :373), `maxent_classifier.pl` (the estimator's
boolean_spec mirror), `omega1_audit.pl`, and additional `gap_diagnostic.pl`/`signature_detection.pl`
sites. The reviewer's consumer-#8 concern was literally correct. Lesson recorded in OQ-94: a
probe-scope statement must name its output limits (`head`, `-m`, pagination), not only predicate
and glob.

## Untruncated census (positive control: the previously-known 7 files all reappear)

`has_coordination_function` consumers (12 files): drift_events :125 :203; transition_paths :35
:82 :164; drl_boltzmann_analysis :111 :119 :123; gap_diagnostic :135 :431 :479 :482; **drl_core
:346 :373**; logical_fingerprint :176 :226 :444; data_repair :163 (comment at a producer site);
**maxent_classifier :174 :178 :187 :188**; cs_pattern_detection :360 (comment);
invertibility_analysis :120; **omega1_audit** :124 :626 :638 :667 :752 :864 :886;
**signature_detection :1019 :1122**.

`constraint_beneficiary` direct reads and the derived `agent_beneficiary/2`
(`narrative_ontology.pl:377-379` = constraint_beneficiary minus the non-agent registry) are
covered per-site below.

## The sort

### SOUND — mountain-likeness reads; beneficiary presence already disqualifies; capture-strengthening is the same semantics, stronger
| site | what the read asks |
|---|---|
| `drl_core.pl:285-288` `natural_law_without_beneficiary` (consumed at :334 snare-block, :363 tangled_rope-block; maxent `forbidden` specs :172,:175; surfaced in invertibility/omega1_audit) | emerges naturally + no enforcement + no beneficiary → natural law. Holds-without-enforcement, literally. |
| `signature_detection.pl:229` `count_power_beneficiaries` (via `agent_beneficiary`) → NL gate `BeneficiaryCount == 0` | does anyone win? → defeats natural_law certification |
| `signature_detection.pl:1340` false_summit_mountain gate | mountain metrics + agent beneficiaries EXIST → false summit ("agent-beneficiary presence is the structural signal of constructedness") |
| `cs_pattern_detection.pl:249` `cs_verdict(false_natural_law_constraint)` | NL pattern + beneficiary exists → FNL verdict |

### FORBIDDEN — the read needs coordination-despite-extraction or the raw declaration; a capture-gate would break or false-fire it
| site | what the read asks |
|---|---|
| `drl_core.pl:373` tangled_rope clause (+ maxent :177-179 `required` specs) | coordination AND asymmetric extraction co-occur — the defining tangled_rope cell |
| `transition_paths.pl:35,82,164` | rope→tangled_rope / scaffold→tangled_rope / terminal tangled_rope: coordination present while extraction rises |
| `drift_events.pl:125,203` coordination_loss | HAD coordination, losing it while extraction persists (tangled_rope→snare decay); gate would blind decay detection exactly for captured constraints |
| `drl_boltzmann_analysis.pl:110-125` `separability_factor` | do coordination and extraction co-occur (reform value); gate demotes captured constraints to "nothing to preserve" (0.9→0.3) |
| `logical_fingerprint.pl:226` `coercion_without_coordination`, `:444` `nonsensically_coupled` | NAF over the raw declaration — a gate would make captured constraints false-fire both voids |
| `cs_pattern_detection.pl:264` `cs_naturalized_mountain` | low ε + beneficiary + victim co-occurrence structure |

### ESCALATE — a THIRD question the rule's two buckets do not cover: benignity certification ("is this genuine/benign coordination?")
| site | what the read asks | why it escalates |
|---|---|---|
| `drl_core.pl:346` scaffold clause (+ maxent :173 `boolean_spec(scaffold, has_coordination_function, required)`) | low χ + coordination function + temporality → certify benign temporary coordination (scaffold) | **This is the prototype's witnessed wrong-direction mechanism**: capturer seats → scaffold via this clause; without the fact they fall through to naturalized (:389). Gate-on-not-captured here is plausibly CORRECT (capture defeats benignity) — but that is neither mountain-likeness nor co-occurrence; it is a third question, and ruling it is the operator's. |
| `signature_detection.pl:1019` CI_Rope certification | Boltzmann-compliant + scope-invariant + coordination function → certify genuine coordination | same benignity question one layer up; FSM intercepts *mountain-metric* beneficiary-bearers first, but a captured low-ε non-mountain profile can reach this gate |
| `signature_detection.pl:1122` `determine_pure_subtype(pure_coordination)` | purity-path subtype label | same family, commentary-grade stakes |

Estimator-classifier congruence note: any ruling on `drl_core:346`/`:373` must be applied to the
`maxent_classifier` boolean_spec table in the same change — the estimator mirrors the cascade.

### RAW-SURFACE — descriptive flag/export/diagnostic reads; must reflect the declared fact; no capture question asked
`logical_fingerprint.pl:176,182` (fingerprint properties — note `coordination` and
`has_beneficiaries` are ALREADY separate bits, split-ready); `invertibility_analysis.pl:120`;
`gap_diagnostic.pl:135,431,479,482`; `global_delta_report.pl:75,302`; `report_generator.pl:366`;
`json_report.pl:315`; `constraint_indexing.pl:906-908` (perspectival access restriction);
`omega1_audit.pl` (7 sites, audit reporting).

### BENEFITS-FROM structural reads — the split option's "benefits-from" side; not coordination reads
`constraint_indexing.pl:458` (`agent_beneficiary` → HasBeneficiaries → `power_role_heuristic` d —
the GAP-10 capture-blind d-derivation, already OQ-92's subject); `drl_purity_network.pl:112-117`
(network edges via shared beneficiary/victim atoms).

### SPLIT-UPGRADE candidate — the one read that semantically wants gain_flow
`constraint_bridge.pl:96`: collects beneficiaries of snare/tangled_rope constraints as
*extraction actors* — asks receives-the-extraction, currently approximated by benefits-from.
First consumer to migrate when the authored surface lands.

### PRODUCER (not a consumer) — fabrication hazard, OQ-93 family
`data_repair.pl:124-131` invents `constraint_beneficiary(C, inferred_institutional)` from metrics
(E>0.46 ∧ S>0.40) and `:163-168` invents `constraint_beneficiary(C, coordinated_group)` from a
scaffold declaration; `:391-395` persists them. DR-AUDIT path only (`scenario_manager`, OQ-93
census). Circularity: on that path, high extraction MANUFACTURES a beneficiary, which then feeds
`has_coordination_function`. Any rewire of the consumer surface must include this bridge.

### N/A — demo data, declarations, comments
`tangled_rope_examples.pl` (8: multifile decls + canonical example facts);
`constraint_instances.pl:155` (engine demo); `cs_pattern_detection.pl:360`,
`signature_detection.pl:94,106,212-219`, `data_repair.pl:163`, `constraint_indexing.pl:450-451`
(comments); `narrative_ontology.pl` (definitions).

## Verdict

The rule sorted the live surface cleanly into both of its buckets plus the raw/structural
categories, and isolated exactly one coherent unsorted family — **benignity certification**
(scaffold clause, CI_Rope gate, pure_coordination subtype, + the maxent scaffold spec). That
family contains the prototype's witnessed wrong-direction site, so it is not a corner case: it is
the fight. Escalated to the operator per the ruling's escalation clause. The step-3
preregistration therefore carries TWO operator questions after all — the diffuse-gate tolerance,
and the benignity-family ruling (gate scaffold/CI_Rope certification on not-captured?) — the
second now a sharply-specified yes/no over three named sites with a witness in hand, not an open
adjudication over fifteen files.

Under-claim: the sort classifies what each read ASKS (clause-level inspection, pasted in the
session transcript); it does not measure firing frequency on any corpus — prevalence of each
site's influence is a separate sweep (the archive breadth option, with its spike requirement).
