% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Equal Protection as Strict Anti-Classification (Colorblind Reading)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This story authors the colorblind (strict anti-classification) reading of
 *   the equal protection kernel: the Constitution forbids any state use of
 *   racial classification, remedial or invidious alike, tracing to Justice
 *   Harlan's Plessy dissent ('Our Constitution is color-blind'). Under this
 *   reading, race-conscious admissions and employment programs — including
 *   those designed to remedy historical subordination — are themselves the
 *   constitutional harm, because the injury is the classification, not merely
 *   a disparate outcome. This is a DIFFERENT constraint from the remedial
 *   reading (which locates the harm in caste perpetuation and treats
 *   race-conscious remedies as permissible) and the diversity reading (which
 *   treats race as one compelling-interest factor among many). The three
 *   readings are not the same constraint measured three ways — they have
 *   different victim sets, different beneficiary sets, and different epsilon
 *   values, and each gets its own story per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - asian_american_applicants: statistical beneficiaries of anti-classification review (moderate/constrained)
 *   - white_applicants_denied_admission: classification-harm beneficiaries under this reading (moderate/constrained)
 *   - colorblind_legal_movement: doctrinal agenda-setter and organizing beneficiary (organized/mobile)
 *   - race_conscious_admissions_programs: institutional payer forced to dismantle programs (institutional/constrained)
 *   - underrepresented_minority_applicants: primary payer, loses remedial pathway (powerless/trapped)
 *   - diversity_office_administrators: professional payer, programs and roles eliminated (moderate/constrained)
 *   - federal_and_state_courts: agenda-setter and analytical observer administering doctrine (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.38).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection as Strict Anti-Classification (Colorblind Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c').
narrative_ontology:cs_kernel_codification('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', fixed_text).
narrative_ontology:cs_authority_grounding('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', lineage).
narrative_ontology:cs_interpretation_layer_present('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c').
narrative_ontology:cs_reading_relation('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', foundational, state_racial_classification_categorically_forbidden).
narrative_ontology:cs_axiom_status(state_racial_classification_categorically_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', state_racial_classification_categorically_forbidden, deontological).
narrative_ontology:cs_axiom('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', foundational, individual_not_group_is_the_constitutional_unit).
narrative_ontology:cs_axiom_status(individual_not_group_is_the_constitutional_unit, holdable).
narrative_ontology:cs_axiom_grounding('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', individual_not_group_is_the_constitutional_unit, deontological).
narrative_ontology:cs_reference_frame('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', harlan_anti_classification_dissent).
narrative_ontology:cs_drift_state('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', post_sffa_v_harvard_2023, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5fc5fba4-a2ac-4e80-aafd-6c05a0aa272c', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, asian_american_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, white_applicants_denied_admission).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_legal_movement).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, race_conscious_admissions_programs).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, diversity_office_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Apply to selective institutions and, under statistical analyses cited in litigation, face lower admission odds at equivalent academic profiles when race-conscious review is used. Under this reading, the anti-classification principle directly protects them by forbidding the institution from weighing race at all. Their exit option is limited to litigation or applying to institutions that have already abandoned race-conscious review.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, asian_american_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Some are denied admission at institutions using race-conscious review and argue the denial constitutes a racial classification harm regardless of whether they would have been admitted under a race-blind process. The colorblind reading treats the classification itself, not only the outcome, as their injury.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, white_applicants_denied_admission, beneficiary,
    moderate, biographical, constrained, national).

% Litigation organizations, originalist legal scholars, and allied jurists advance the anti-classification reading through strategic litigation, citing Justice Harlan's Plessy dissent as the founding text. They set the doctrinal agenda by selecting test cases and briefing strategy, and benefit reputationally and institutionally from doctrinal victories that entrench this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_legal_movement, agenda_setter,
    organized, generational, mobile, national).

% Universities and other state actors that have built admissions or hiring frameworks incorporating race as a factor must dismantle or substantially rework these programs under this reading. They bear compliance costs, litigation exposure, and loss of a tool they consider necessary for pursuing diversity goals, with no substitute doctrine offered by this reading for the problem the programs were built to address.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_conscious_admissions_programs, payer,
    institutional, generational, constrained, national).

% Black, Latino, and Indigenous applicants who benefited from race-conscious review lose an admissions pathway that partially offset K-12 resource disparities, wealth gaps, and legacy/donor preferences that this reading leaves untouched. Their exit option is essentially none — they cannot litigate their way back into a race-conscious framework once this reading controls doctrine, and alternative race-neutral proxies (socioeconomic status, geography) are imperfect substitutes.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, underrepresented_minority_applicants, payer,
    powerless, biographical, trapped, national).

% Administrators who designed and ran race-conscious admissions and diversity programs face professional displacement, program shutdowns, and personal liability exposure in litigation as their institutions retool under compliance pressure from this reading's doctrinal victories.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, diversity_office_administrators, payer,
    moderate, biographical, constrained, national).

% Adjudicate equal protection claims and, where this reading controls a given panel or era, apply strict scrutiny in a manner that treats any racial classification, remedial or otherwise, as presumptively unconstitutional. Courts administer the doctrine and could in principle shift to a different reading, but their institutional legitimacy is now partly staked on doctrinal consistency with precedent built on this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, federal_and_state_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, federal_and_state_courts, observer).

% The Reconstruction Congress that drafted the Fourteenth Amendment also enacted race-conscious relief measures (Freedmen's Bureau legislation) contemporaneously with the Amendment's ratification. Their own practice complicates the colorblind reading's claim to fidelity with framing-era understanding, but this historical evidence is not a living party to current litigation and enters the record only through historians' briefs, not as a direct voice.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, excluded_legislative_drafters_of_reconstruction_amendments, excluded,
    powerless, civilizational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable, rule-like test — no racial classification by the state, ever — that courts, legislatures, and institutions can apply without case-by-case balancing of competing group interests, reducing litigation uncertainty and insulating decision-makers from having to adjudicate whose racial disadvantage counts and how much.
% TRANSFER_FUNCTION: Moves admissions and employment opportunities from applicants who would have benefited under race-conscious review (primarily underrepresented minority applicants) to applicants who benefit from race-blind review (disproportionately Asian American and white applicants at the margin), and moves compliance and redesign costs onto institutions that had built programs around race-conscious criteria.
% ABSENT_VOICES: The Reconstruction-era legislators who paired the Fourteenth Amendment with race-targeted relief are not present to explain their own contemporaneous practice; K-12 students facing under-resourced schools whose disparities race-conscious admissions was designed to partially offset are not litigants and have no direct voice in the doctrinal contest between readings.
% DISAPPEARANCE_RATIONALE: If this reading's doctrinal dominance disappeared, institutions could reintroduce race-conscious criteria without strict-scrutiny risk, admissions offices would rebuild diversity programs, litigation organizations built around anti-classification strategy would lose their primary doctrinal lever, and the demographic composition of selective admissions would shift measurably within an admissions cycle.
% FOUNDING_PROBLEM: Justice Harlan's Plessy dissent was built to reject the doctrine that the state may classify citizens by race for purposes of enforcing subordination (separate-but-equal segregation) — his colorblind principle targeted state-imposed racial caste, not race-conscious remediation of that caste's effects.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the colorblind litigation movement (including scholars cited in remedial-reading briefs) attest that Harlan's dissent was aimed at segregationist classification, not at remedial race-consciousness, and note his own contemporaneous statements distinguishing Chinese immigrants as a race he considered properly excludable — complicating a clean 'universal colorblindness' genealogy. The colorblind legal movement itself attests the founding problem (any state racial classification) remains live and applies with equal force to remedial classification; this attestation comes from the reading's own principal beneficiary and organizing movement, not from an independent source.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 (moderate-high, within the expected 0.35-0.50 band) because under this reading's own lights, the harm is the act of racial classification itself, independent of whether any given applicant would have gained or lost admission under a race-blind process — this makes the extraction diffuse but real: it falls on every institution and applicant touched by race-conscious frameworks, and concentrates hardest on underrepresented minority applicants who lose a specific, previously available pathway. Suppression (0.38) reflects the doctrine's move from persuasive dissent to binding strict-scrutiny rule: once courts adopt this reading, institutions face real coercive pressure (litigation exposure, loss of federal funding eligibility) to comply, not mere moral suasion. Theater ratio is modest (0.20) because enforcement mechanisms (strict scrutiny review, litigation, compliance audits) are genuinely functional, not merely performative — though some institutional 'race-neutral' proxies adopted in response function partly as compliance theater that reintroduces race-correlated effects without race-conscious intent.
 *
 * PERSPECTIVAL GAP:
 *   From the colorblind legal movement's and successful applicants' seats, this reading looks like principled coordination around a clean, administrable rule that finally honors the framing generation's stated (if inconsistently practiced) ideal. From the seat of underrepresented minority applicants and the institutions that built remedial programs, the same doctrinal move looks like extraction: a formally neutral rule applied onto a substantively unequal starting distribution, removing one of the few tools available to correct for compounding disadvantage while leaving legacy admissions, donor preferences, and geographic sorting untouched. The engine should register this asymmetry rather than resolve it — the divergence is exactly what a kernel-reading story is supposed to expose.
 *
 * DIRECTIONALITY LOGIC:
 *   Asian American and white applicants denied admission under race-conscious review sit near the beneficiary end: the classification's removal directly serves their competitive position (even though many individual members of these groups did not personally benefit and may hold contrary political views — directionality is structural, not attitudinal). The colorblind legal movement is a clear structural beneficiary — it collects doctrinal wins, funding, and institutional standing from advancing this reading, though it does not administer admissions itself, hence 'agenda_setter' rather than a revenue-collecting beneficiary. Race-conscious institutions and underrepresented minority applicants sit near the target end: institutions bear compliance costs and lose a policy tool; applicants lose access. Courts are dual-positioned (agenda_setter/observer) because they administer the doctrine but their 'exit' is analytical rather than material — they do not personally bear or collect the transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a genealogy tension rather than resolving it: Harlan's dissent targeted state-imposed racial subordination (segregation), and this reading's extension of that principle to forbid remedial race-consciousness is itself a contested doctrinal move, not a straightforward continuation. The founding_problem_status is authored as contested precisely because the colorblind movement's own attestation that the founding problem (any racial classification) remains fully live is not corroborated by independent legal historians, who read the original dissent as narrower. This keeps the story from smuggling in a flattering, self-serving genealogy as settled fact — the mismatch between a 'dead/narrower' historical problem and a 'world_rearranges' disappearance verdict is exactly the kind of capture signal this framework is built to flag for downstream review, without letting the narrative field itself drive classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harlan_dissent_original_scope_ambiguity,
    'Did Justice Harlan''s ''color-blind Constitution'' language, read in its full historical context, target only invidious/subordinating racial classification (segregation), or did it articulate a universal anti-classification principle that would equally forbid remedial race-consciousness?',
    'Close textual and historical analysis of the full Plessy dissent alongside Harlan''s other opinions and contemporaneous statements (including his views on Chinese exclusion), cross-referenced against Reconstruction Congress''s simultaneous enactment of race-targeted relief legislation (Freedmen''s Bureau acts) under the same constitutional generation.',
    'If Harlan''s principle was narrowly anti-subordination, the colorblind reading''s claim to textual/historical fidelity weakens substantially, and the reading would be better characterized as a modern doctrinal innovation dressed in inherited language rather than a direct continuation — this would not change the reading''s current legal force but would reframe its genealogical legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harlan_dissent_original_scope_ambiguity, conceptual, 'Whether the colorblind reading''s founding text supports a universal or narrow anti-classification principle.').

omega_variable(
    classification_harm_vs_outcome_harm,
    'Is the cognizable equal-protection injury the act of racial classification itself (regardless of whether the classified individual would have obtained a different outcome under a race-blind process), or is the injury properly measured only by demonstrated counterfactual outcome effects?',
    'Doctrinal analysis of standing requirements in equal protection litigation and whether courts require plaintiffs to show they would have been admitted/hired but for the classification, versus treating the classification''s mere existence as sufficient injury.',
    'If classification-itself is the harm (as this reading holds), extraction is diffuse and applies even absent any demonstrated individual counterfactual loss — this is the basis for the 0.42 epsilon authored here. If outcome-effect is required, actual extraction incidence would be narrower and concentrated only on demonstrated counterfactual losers, likely lowering the effective epsilon for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_harm_vs_outcome_harm, conceptual, 'Whether the constitutional injury under this reading is classification per se or demonstrated outcome displacement.').

omega_variable(
    colorblind_reading_kernel_framing_alternative,
    'Is the correct kernel-level framing ''equal protection commitment with three live readings'' (as authored here), or is there a more fundamental prior kernel — ''what counts as a racial classification at all'' (e.g., does facially race-neutral proxy selection for race-correlated outcomes count as classification under this reading) — that this story''s cs_structure declarations do not surface?',
    'Trace subsequent litigation over race-neutral proxies (percent plans, socioeconomic-status admissions criteria adopted post-SFFA v. Harvard) to determine whether courts applying this reading treat proxy-driven race correlation as itself a forbidden classification, which would suggest a deeper, unauthored kernel about what ''racial classification'' means.',
    'If courts extend strict scrutiny to race-neutral proxies with disparate racial effects, the colorblind reading''s ε would need to rise further (or a fourth constraint story would need to be written for a ''proxy-classification reading''), since the harm and victim set would expand beyond directly race-conscious programs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colorblind_reading_kernel_framing_alternative, conceptual, 'Whether a deeper unauthored kernel about the definition of ''racial classification'' underlies the three declared readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__colorblind_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement(equa_tr_t1996, equal_protection_commitment__colorblind_reading, theater_ratio, 1996, 0.12).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__colorblind_reading, theater_ratio, 2003, 0.14).
narrative_ontology:measurement(equa_tr_t2013, equal_protection_commitment__colorblind_reading, theater_ratio, 2013, 0.17).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__colorblind_reading, theater_ratio, 2023, 0.2).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_commitment__colorblind_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__colorblind_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(equa_be_t1996, equal_protection_commitment__colorblind_reading, base_extractiveness, 1996, 0.28).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__colorblind_reading, base_extractiveness, 2003, 0.3).
narrative_ontology:measurement(equa_be_t2013, equal_protection_commitment__colorblind_reading, base_extractiveness, 2013, 0.34).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__colorblind_reading, base_extractiveness, 2023, 0.42).
narrative_ontology:measurement(equa_be_t2024, equal_protection_commitment__colorblind_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__colorblind_reading, suppression_requirement, 1954, 0.1).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.15).
narrative_ontology:measurement(equa_su_t1996, equal_protection_commitment__colorblind_reading, suppression_requirement, 1996, 0.2).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__colorblind_reading, suppression_requirement, 2003, 0.24).
narrative_ontology:measurement(equa_su_t2013, equal_protection_commitment__colorblind_reading, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__colorblind_reading, suppression_requirement, 2023, 0.38).
narrative_ontology:measurement(equa_su_t2024, equal_protection_commitment__colorblind_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, diversity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'equal protection commitment' kernel: colorblind_reading (this file), remedial_reading, and diversity_reading. Each reading has a distinct victim set, beneficiary set, and epsilon value because each locates the constitutional harm differently — classification-itself (colorblind), caste perpetuation (remedial), or insufficiently justified race-conscious balancing absent compelling interest (diversity). They are linked via affects_constraints rather than merged into one story because merging would violate epsilon-invariance: measuring 'equal protection' by the colorblind reading's lights yields ε≈0.42, while measuring it by the remedial reading's lights yields a structurally different, much lower ε for the same underlying programs (since the remedial reading treats those programs as legitimate coordination, not harm).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
