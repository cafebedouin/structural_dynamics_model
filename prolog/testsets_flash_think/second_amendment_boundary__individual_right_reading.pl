% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment: Individual Right Interpretation
 *   domain: constitutional_law/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, where the operative clause establishes a pre-existing
 *   individual right to bear arms, and the prefatory militia clause states a
 *   purpose but does not limit the right's scope. This interpretation shields
 *   private gun ownership from extensive state regulation, leading to a
 *   constitutionally protected firearms market. The claimed type is
 *   'mountain' because proponents assert it as a natural, fundamental right,
 *   triggering False Summit Mountain (FSM) detection due to identifiable
 *   beneficiaries and victims. The metrics reflect the societal costs and
 *   regulatory suppression resulting from this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.8).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.85).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment: Individual Right Interpretation").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).
domain_priors:emerges_naturally(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '4d119fa9-7b72-4346-aefd-cc398f0759b4').
narrative_ontology:cs_kernel_codification('4d119fa9-7b72-4346-aefd-cc398f0759b4', fixed_text).
narrative_ontology:cs_authority_grounding('4d119fa9-7b72-4346-aefd-cc398f0759b4', lineage).
narrative_ontology:cs_interpretation_layer_present('4d119fa9-7b72-4346-aefd-cc398f0759b4').
narrative_ontology:cs_reading_relation('4d119fa9-7b72-4346-aefd-cc398f0759b4', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('4d119fa9-7b72-4346-aefd-cc398f0759b4', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('4d119fa9-7b72-4346-aefd-cc398f0759b4', foundational, individual_right_pre_exists_state).
narrative_ontology:cs_axiom_status(individual_right_pre_exists_state, holdable).
narrative_ontology:cs_axiom_grounding('4d119fa9-7b72-4346-aefd-cc398f0759b4', individual_right_pre_exists_state, deontological).
narrative_ontology:cs_axiom('4d119fa9-7b72-4346-aefd-cc398f0759b4', foundational, militia_clause_is_prefatory).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory, holdable).
narrative_ontology:cs_axiom_grounding('4d119fa9-7b72-4346-aefd-cc398f0759b4', militia_clause_is_prefatory, conventional).
narrative_ontology:cs_reference_frame('4d119fa9-7b72-4346-aefd-cc398f0759b4', original_intent_individual_right).
narrative_ontology:cs_drift_state('4d119fa9-7b72-4346-aefd-cc398f0759b4', contemporary_judicial_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4d119fa9-7b72-4346-aefd-cc398f0759b4', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, victims_of_gun_violence).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, public_health_advocates).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, law_enforcement_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the constitutional protection of their right to own firearms, which limits state and federal regulation. They actively defend this interpretation through advocacy and litigation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_owners, beneficiary,
    organized, biographical, mobile, national).

% Benefits from a constitutionally shielded market for firearms, with reduced regulatory burdens and increased demand. They fund legal challenges and lobbying efforts to maintain this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Bear the direct and indirect costs of gun violence, including physical harm, psychological trauma, and loss of life. Their ability to seek legislative remedies for gun violence is severely constrained by this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, victims_of_gun_violence, payer,
    powerless, immediate, trapped, local).

% Work to reduce gun violence through public health interventions and policy changes. Their efforts are consistently challenged and often blocked by legal arguments grounded in this interpretation of the Second Amendment.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_health_advocates, payer,
    organized, generational, constrained, national).

% Face increased risks and operational challenges due to the widespread availability of firearms and limitations on their ability to regulate them. They often advocate for stricter gun laws but are constrained by judicial precedent.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, law_enforcement_agencies, payer,
    institutional, biographical, constrained, national).

% The ultimate arbiter of constitutional meaning, whose interpretations establish the legal boundaries of the Second Amendment. Their rulings actively shape the scope and enforcement of this constraint.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Responsible for enacting laws, but their ability to pass gun control legislation is severely limited by judicial interpretations of the Second Amendment. They must navigate the constitutional boundaries set by the courts.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, legislators, agenda_setter,
    powerful, biographical, constrained, national).

% Advocate for a reading of the Second Amendment that ties the right to bear arms to militia service. While they may support individual gun ownership, their specific interpretation is marginalized by the dominant individual-right reading.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, militia_advocates, excluded,
    organized, biographical, constrained, national).

% Believe the Second Amendment protects a right to armed resistance against tyrannical government. This reading, while sometimes overlapping with individual rights, is not the primary focus of the mainstream individual-right interpretation and is often disavowed by its proponents.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, insurrectionist_theorists, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, nationally recognized individual right to bear arms, providing legal certainty for gun owners and the firearms industry regarding the scope of their constitutional protections.
% TRANSFER_FUNCTION: Transfers the burden of mitigating gun violence and its societal costs from individual gun owners and the firearms industry to society at large, by severely limiting legislative and regulatory options for gun control.
% ABSENT_VOICES: Victims of gun violence, public health experts, and those advocating for a collective or militia-focused interpretation of the Second Amendment are often marginalized in legal discourse dominated by the individual-right framework. Their perspectives on societal costs and alternative interpretations are structurally excluded from the core legal debate.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, states and the federal government would rapidly enact a wide range of gun control laws, the firearms market would face significant new regulations, and the legal landscape for gun ownership would be fundamentally altered, leading to a major reorganization of public safety and individual rights frameworks.
% FOUNDING_PROBLEM: To protect the right of individuals to own firearms, often framed as a fundamental right for self-defense and as a check against potential government overreach, rooted in historical concerns about a standing army.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (gun owners, firearms industry) assert the problem of self-defense and potential tyranny is still live. Opponents (public health, victims' advocates, some legal scholars) argue the founding problem has largely shifted to the pervasive issue of gun violence, and that the current interpretation exacerbates this problem; historical analysis and public safety data from independent researchers corroborate this shifted-function reading.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_boundary__individual_right_reading),
    narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because this interpretation imposes significant societal costs in terms of gun violence, which are borne by victims and public services. Suppression is also high (0.85) as it actively blocks legislative efforts to enact gun control. The theater ratio is low (0.1) because the legal arguments and judicial processes are genuine, not performative. Accessibility collapse is high (0.9) for alternative regulatory approaches. Resistance is high (0.7) from those advocating for gun control. The temporal measurements show a gradual increase in extractiveness and suppression as the interpretation has been solidified and its societal impacts have become more pronounced over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of gun owners and the firearms industry, this interpretation is a vital protection of a fundamental right. From the perspective of victims of gun violence and public health advocates, it is a structure that enables significant societal harm by preventing effective regulation. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Gun owners and the firearms industry are clear beneficiaries (low directionality), as the interpretation protects their interests and market. Victims of gun violence, public health advocates, and law enforcement agencies are targets (high directionality), bearing the costs and facing suppressed regulatory options. The Supreme Court and legislators act as agenda-setters, shaping and navigating the legal boundaries of this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_legal_construct,
    'Is the individual right to bear arms truly a pre-existing natural law, or is its scope and application a legal construct shaped by judicial interpretation and societal context?',
    'Comparative legal analysis across jurisdictions with different constitutional traditions, and historical analysis of the amendment''s drafting and early interpretations.',
    'If primarily a legal construct, its ''mountain'' claim is a false summit, reclassifying it as a Tangled Rope or Snare, highlighting its constructed nature and the beneficiaries of that construction. If genuinely natural, its classification as a Mountain would be affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_legal_construct, conceptual, 'Ambiguity regarding the naturalness vs. constructedness of the individual right.').

omega_variable(
    scope_of_arms,
    'Does the term ''arms'' in the Second Amendment, as interpreted by this reading, encompass all modern firearms, or is its scope limited to those types of weapons common at the time of the amendment''s ratification?',
    'Further judicial rulings clarifying the types of weapons protected, or legislative action defining ''arms'' in a constitutionally permissible way.',
    'A narrower definition of ''arms'' would reduce the constraint''s extractiveness by allowing regulation of certain weapon types; a broader definition would increase extractiveness by further shielding the firearms market.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_arms, empirical, 'Uncertainty regarding the specific types of firearms protected by the individual right.').

omega_variable(
    balancing_test_efficacy,
    'How effectively does the current judicial ''balancing test'' (e.g., intermediate scrutiny) weigh the individual right against compelling public safety interests, and does it genuinely allow for meaningful regulation?',
    'Empirical studies on the impact of regulations permitted under the balancing test, and legal analysis of how often such tests actually uphold gun control measures.',
    'If the balancing test is found to be largely deferential to the individual right, the constraint''s suppression of gun control is higher than acknowledged; if it genuinely allows for effective regulation, suppression is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_efficacy, empirical, 'Effectiveness of judicial balancing tests in mediating the individual right and public safety.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gun control primarily structural (legal precedent, judicial review) or internalized (a cultural belief in an absolute right to bear arms that persists even if legal barriers were lowered)?',
    'Post-ruling legislative behavior: if legislative efforts to enact gun control remain low even after a favorable judicial ruling, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the cultural belief would continue to impede legislative action even with legal space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gun control efforts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_boundary__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_boundary__individual_right_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_boundary__individual_right_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_boundary__individual_right_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_boundary__individual_right_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(seco_tr_t2020, second_amendment_boundary__individual_right_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_boundary__individual_right_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(seco_be_t1980, second_amendment_boundary__individual_right_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(seco_be_t1990, second_amendment_boundary__individual_right_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(seco_be_t2000, second_amendment_boundary__individual_right_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(seco_be_t2010, second_amendment_boundary__individual_right_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(seco_be_t2020, second_amendment_boundary__individual_right_reading, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_boundary__individual_right_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(seco_su_t1980, second_amendment_boundary__individual_right_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(seco_su_t1990, second_amendment_boundary__individual_right_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(seco_su_t2000, second_amendment_boundary__individual_right_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(seco_su_t2010, second_amendment_boundary__individual_right_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(seco_su_t2020, second_amendment_boundary__individual_right_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, gun_control_legislation).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, public_safety_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
