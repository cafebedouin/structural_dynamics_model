% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Competent Individual Sovereignty Over End-of-Life Decision (Autonomy Reading)
 *   domain: bioethics/medical/legal
 *
 * SUMMARY:
 *   This constraint is the autonomy reading of a deeply contested kernel: who
 *   holds legitimate authority over end-of-life decisions? The autonomy
 *   reading asserts that competent individuals possess sovereign authority to
 *   choose death when they assess their suffering as unacceptable. This
 *   reading has strengthened in some jurisdictions (Oregon, Netherlands,
 *   Belgium, Canada, Switzerland) and remains contested in others. The
 *   claim/metric divergence is deliberate: the autonomy reading is CLAIMED as
 *   tangled_rope (coordination function: aligning decisions with individual
 *   values; extraction: removal of competing institutional authority;
 *   enforcement: legal/medical gatekeeping to ensure competence and prevent
 *   coercion). The metrics (extractiveness 0.58, suppression 0.72) describe a
 *   constraint whose persistence depends on actively suppressing competing
 *   institutional claims (sanctity, protective institutional authority) and
 *   whose coordination benefits are asymmetric (beneficiaries gain choice;
 *   payers lose institutional control). The engine computes how this diverges
 *   across seats: institutional actors read this as pure extraction of their
 *   authority; individuals read it as removal of oppressive gatekeeping;
 *   disability advocates read it as hidden coercion. The framework models one
 *   reading in isolation and links it to siblings via the constraint family
 *   network.
 *
 * KEY AGENTS:
 *   - competent_individuals_seeking_death: the beneficiary seat; possess decision authority under this reading; immediate time horizon, trapped exit
 *   - individuals_denied_access_to_death: the victim seat; bear the cost of institutional gatekeeping; powerless, trapped
 *   - healthcare_professionals_facilitators: dual-positioned beneficiary/payer; gain clarity of role but bear moral/liability costs; moderate power, constrained exit
 *   - religious_institutional_actors: payer seat; lose institutional authority over end-of-life decisions; organized power, constrained exit
 *   - disability_rights_advocates: excluded; argue autonomy is obscured coercion; would require vulnerability-protection multi-gate structure
 *   - slippery_slope_risk_bearers: excluded; hypothetical future persons at risk from pressure; powerless, trapped
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.58).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.72).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Competent Individual Sovereignty Over End-of-Life Decision (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "bioethics/medical/legal").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '7f58f4d1-81f9-44ff-872e-7ceb5d5ff087').
narrative_ontology:cs_kernel_codification('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', formalized).
narrative_ontology:cs_authority_grounding('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', lineage).
narrative_ontology:cs_interpretation_layer_present('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087').
narrative_ontology:cs_reading_relation('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', foundational, individual_competent_authority_is_binding).
narrative_ontology:cs_axiom_status(individual_competent_authority_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', individual_competent_authority_is_binding, deontological).
narrative_ontology:cs_axiom('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', foundational, suffering_assessment_is_individual_prerogative).
narrative_ontology:cs_axiom_status(suffering_assessment_is_individual_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', suffering_assessment_is_individual_prerogative, deontological).
narrative_ontology:cs_reference_frame('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', institutional_medical_paternalism).
narrative_ontology:cs_drift_state('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', contemporary_post_legalization_jurisdictions, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7f58f4d1-81f9-44ff-872e-7ceb5d5ff087', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals_seeking_death).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitators).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, individuals_denied_access_to_death).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, persons_experiencing_prolonged_suffering).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint extracts from those it governs — in this case, institutional authorities (medical paternalism, religious doctrine, legal gatekeeping). High extractiveness (0.58) reflects that institutional actors lose unilateral decision authority. Suppression (0.72) is high because the constraint's persistence depends on actively suppressing competing institutional claims and preventing religious or legal actors from refusing to facilitate death. The constraint requires enforcement: in jurisdictions where it is not enforced, institutional gatekeeping persists; where it is enforced, legal liability and professional expectations force compliance. Theater ratio (0.29) is moderate-low: the autonomy rationale is genuine (respecting individual choice is a real coordination function), but a significant portion of enforcement activity defends individual choice against institutional resistance rather than solving a coordination problem. The measurement trajectory shows extractiveness rising initially (as jurisdictions legalize and enforcement machinery develops) then plateauing (after enforcement normalizes and institutional resistance shifts from legal to professional/cultural resistance). Suppression rises slightly more steeply (institutional actors are slower to normalize the constraint) then stabilizes. The separation between extractiveness and suppression growth rates reflects that institutional actors resist coordination while individual beneficiaries adapt.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (competent individuals), the constraint removes oppressive paternalism and aligns medical practice with individual values — a pure coordination gain. From the institutional seat (medical authorities, religious organizations), the constraint extracts their legitimate authority and forces them to facilitate choices they believe are wrong — pure extraction. From the excluded disability-rights seat, the constraint obscures coercive pressure on vulnerable populations and should be replaced with a vulnerability-protection multi-gate architecture that requires institutional checkpoints. From the analytical seat, all three perspectives are structural: the constraint genuinely transfers authority from institutions to individuals (extraction from institutional perspective), genuinely enables choice for some (coordination from beneficiary perspective), and genuinely creates risk of coercion for vulnerable populations (supporting the excluded advocates). The engine computes different types per seat by reading the same structural data — beneficiary seat reads coordination, payer seat reads extraction, excluded seat predicts coercion-risk escalation. This divergence is the framework's measurement target.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent individuals seeking death have d near the beneficiary end (0.0–0.2): they benefit from removal of barriers and have zero exit options (the decision is terminal). Healthcare professionals facilitators have d near symmetric (0.4–0.6): they gain clarity of role and permission but bear moral/liability costs; they can exit by refusing to practice in jurisdictions with the constraint, but professional norms and career investment create constrained exit. Religious institutional actors have d near the target end (0.7–0.9): they lose institutional control over end-of-life decisions and are forced to accommodate or refuse to participate. Their exit is constrained (they can withdraw from healthcare systems but cannot prevent other providers from offering the service). Disability-rights advocates, though excluded from the decision architecture, experience the constraint's operation asymmetrically: they would argue their populations face pressure to choose death. Slippery-slope risk bearers (hypothetical future persons) have d at the maximum target end (1.0) because they face potential coercive pressure they cannot defend against. The directionality variation is generated from the structural data (beneficiary/victim declarations + exit options + power); no overrides are needed because the structural data accurately captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading's founding problem (medical paternalism denies individual autonomy in end-of-life decisions) remains live and contested. The constraint exists because the founding problem persists — individuals still face institutional barriers, and autonomy advocates argue this justifies strong enforcement. However, mandatrophy pressure exists from the vulnerability-protection reading: if the founding problem is reframed as 'how do we prevent both denial and coercion,' the autonomy reading's solution (individual choice with minimal gatekeeping) does not fully address coercion-risk, suggesting the founding problem is only partially solved. The measurement of suppression (0.72) and the identified slippery-slope risk indicate that the constraint's persistence may depend on continued suppression of alternative readings (sanctity, vulnerability-protection) rather than on solving its core coordination problem. This is mandatrophy pressure: the constraint persists partly because it must suppress competing claims, not purely because individual autonomy is the best solution to end-of-life authority. The constraint has not yet crossed into full mandatrophy (the founding problem remains live), but the vulnerability-protection reading's challenge suggests the constraint is solving a narrower problem than it claims — individual choice when competence is assured — while externalizing coercion risks to vulnerable populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_coercion_boundary,
    'When does social pressure to choose death constitute coercion that invalidates the autonomy claim, versus a constraint''s normal operation within an imperfect world?',
    'Comparative analysis of disability-community outcomes in high-access (legal assisted death with minimal gatekeeping) vs. high-support (robust social services for severe conditions) jurisdictions. If high-access + low-support produces disproportionate death requests from disabled populations, and high-support reduces requests, the boundary shifts toward coercion-finding.',
    'If coercion is common, the autonomy reading loses its claim to respect individual choice — the choice is pre-corrupted by abandonment. The victim set would expand to include disabled persons whose ''choice'' reflects systemic pressure rather than genuine autonomy. This would support vulnerability-protection reading''s two-tier architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_coercion_boundary, empirical, 'Whether autonomy-framed choice can be distinguished from socially-coerced choice in end-of-life contexts.').

omega_variable(
    competence_boundary_contestation,
    'What defines competence to make end-of-life decisions, and who adjudicates that boundary? Does the autonomy reading foreclose other readings'' competence criteria?',
    'Examine case law and ethical practice standards across jurisdictions: if competence assessment is uniform and narrow (e.g., rational understanding of consequences), the autonomy reading holds and forecloses alternative criteria; if assessment is contested and variable (e.g., some jurisdictions require absence of depression, others do not), the boundary is conceptually open and the readings coexist.',
    'A narrow, stable competence definition supports the autonomy reading''s claim to clear individual authority. A contested, variable definition suggests the competence boundary itself is a site where coercion and protection conflict — supporting the conceptual-coexistence diagnosis and the vulnerability-protection reading''s multi-gate structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_boundary_contestation, empirical, 'Whether competence assessment criteria are stable or contested across institutions and jurisdictions.').

omega_variable(
    slippery_slope_empirical_status,
    'Do jurisdictions that recognize individual autonomy over end-of-life decisions show evidence of pressure on vulnerable populations to choose death (the slippery slope), or do safeguards prevent it?',
    'Long-term demographic and outcome analysis from jurisdictions with legal assisted dying (Oregon, Netherlands, Belgium, Canada, Switzerland). Measure (1) uptake rates by age, disability status, socioeconomic class; (2) documented cases of pressure or coercion; (3) outcomes after rule tightening (e.g., Belgium''s expansion to minors — did this create downstream pressure?); (4) compare to jurisdictions with strong social support for living with severe conditions.',
    'If slippery slope is empirically real (higher death requests in marginalized populations, documented coercion cases, outcomes change when rules tighten), the autonomy reading''s externalization of this risk is a weakness — the reading becomes a cover for latent extraction from vulnerable populations. If slippery slope is not empirically supported (rates stable across populations, no documented coercion, outcomes robust to rule changes), the autonomy reading''s handling of the risk is justified and the vulnerability-protection reading''s caution is precautionary rather than empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_status, empirical, 'Whether the slippery-slope risk of pressure on vulnerable populations is empirically realized in jurisdictions with legal assisted dying.').

omega_variable(
    autonomy_sanctity_logical_relationship,
    'Does the autonomy reading''s claim (competent individuals possess sovereign authority over their own death) logically foreclose the sanctity reading''s claim (human life possesses intrinsic value independent of individual will), or do they coexist as different frameworks?',
    'Formal analysis of the logical relationship: if both claims can be held in the same framework (e.g., ''life has intrinsic value, AND individuals have authority to waive it''), they coexist; if one claims authority over the question the other claims is closed, they foreclose. Examine whether any contemporary legal system or ethical framework actually holds both simultaneously.',
    'If they foreclose each other, the autonomy and sanctity readings are competing monolithic claims and cannot coexist in a single institutional framework — jurisdictions must choose. If they coexist, the institutional question becomes how to recognize both (e.g., by respecting individual choice while preserving institutional conscientious objection or by multi-gate gatekeeping). This distinction determines whether the readings are binary competitors or can be reconciled via the vulnerability-protection reading''s multi-tier structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_sanctity_logical_relationship, conceptual, 'Logical and institutional compatibility between autonomy and sanctity readings of end-of-life authority.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (legal barriers, institutional refusal, professional gatekeeping) or internalized (individuals internalize the cultural prohibition and do not request death even when they would benefit from access)?',
    'Post-legalization outcome studies: if jurisdictions that legalize autonomy see large increases in death requests (indicating previous suppression was internalized/hidden), suppression is primarily internalized. If legalization produces small changes in request rates (indicating previous barriers were structural but most individuals did not want access), suppression is primarily structural. Qualitative interviews with individuals requesting death pre- and post-legalization document whether the change is from ''I was not permitted'' (structural) or ''I did not know I could ask'' (internalized).',
    'If suppression is primarily internalized, the autonomy reading''s enforcement removes less barrier than it appears — many individuals have learned not to want what they are not permitted. Post-legalization societies show higher rates of death requests, indicating a large previously-hidden demand pool. If suppression is primarily structural, legalization removes a real barrier but does not unlock hidden demand. The distinction affects the measured effectiveness of the constraint and the victim count.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of end-of-life choice is structural or internalized in the current regime.').

omega_variable(
    reading_kernel_contestation,
    'This constraint is ONE READING of the contested kernel ''end_of_life_decision_authority''. The three sibling readings (autonomy_reading, sanctity_reading, vulnerability_protection_reading) rest on incompatible core premises about who holds legitimate authority. How should the logical relationships be characterized?',
    'Examine whether the readings can coexist within a single legal framework (coexists_with) or whether one logically rules out the others (forecloses). Map whether autonomy-framing (individual authority) and sanctity-framing (inherent value) are contradictory or separable. Assess whether vulnerability-protection (institutional checkpoints) is a third position or a meta-framework that encompasses both.',
    'If autonomy and sanctity foreclose each other, jurisdictions must choose monolithically. If they coexist, multi-reading frameworks become possible (e.g., respecting autonomy while preserving institutional sanctity claims via conscientious objection). If vulnerability-protection is a meta-framework, it reframes the question from ''who decides'' to ''how do we prevent both denial and coercion'' — shifting the kernel contest from binary to ternary. This is the core structural question for the constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Logical relationship between autonomy, sanctity, and vulnerability-protection readings of end-of-life decision authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(end__tr_t5, end_of_life_decision_authority__autonomy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(end__tr_t15, end_of_life_decision_authority__autonomy_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__autonomy_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(end__tr_t25, end_of_life_decision_authority__autonomy_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(end__tr_t30, end_of_life_decision_authority__autonomy_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(end__tr_t35, end_of_life_decision_authority__autonomy_reading, theater_ratio, 35, 0.29).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__autonomy_reading, theater_ratio, 40, 0.29).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(end__be_t5, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(end__be_t15, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(end__be_t25, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement(end__be_t30, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(end__be_t35, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(end__su_t5, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(end__su_t15, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(end__su_t25, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(end__su_t30, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(end__su_t35, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__autonomy_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The autonomy reading is one of three ε-invariant constraint stories decomposing the contested kernel 'end_of_life_decision_authority'. Each sibling reading instantiates a different structural claim about who holds legitimate authority and what counts as coercion vs. protection. The autonomy reading has higher extractiveness because it transfers authority away from institutional actors; the sanctity reading would have lower extractiveness because it preserves institutional/doctrinal authority; the vulnerability-protection reading would show high extractiveness from both flanks (requiring both autonomy advocates and institutional actors to accept multi-gate gatekeeping). The three stories are linked via this affects_constraints field and should be read as a family, not as isolated constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
