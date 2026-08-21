% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: State's Affirmative Duty to Eliminate Oppressive Religious Practices (Reformist Reading)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the 'reformist reading' of constitutional
 *   secularism, which posits an affirmative state duty to intervene in and
 *   eliminate religious practices deemed oppressive to marginalized groups,
 *   even if it supersedes claims of religious autonomy. This reading is
 *   highly extractive of religious autonomy and requires significant state
 *   enforcement, often against strong resistance from conservative religious
 *   factions. It is one reading of the broader 'constitutional_secularism'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.85).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.9).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, snare).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "State's Affirmative Duty to Eliminate Oppressive Religious Practices (Reformist Reading)").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '3b6e4c77-7225-47f3-b8ba-cbf620861c7a').
narrative_ontology:cs_kernel_codification('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', formalized).
narrative_ontology:cs_authority_grounding('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', lineage).
narrative_ontology:cs_interpretation_layer_present('3b6e4c77-7225-47f3-b8ba-cbf620861c7a').
narrative_ontology:cs_reading_relation('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', foundational, state_duty_to_ensure_substantive_equality).
narrative_ontology:cs_axiom_status(state_duty_to_ensure_substantive_equality, holdable).
narrative_ontology:cs_axiom_grounding('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', state_duty_to_ensure_substantive_equality, deontological).
narrative_ontology:cs_axiom('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', foundational, religious_autonomy_subordinate_to_equality).
narrative_ontology:cs_axiom_status(religious_autonomy_subordinate_to_equality, holdable).
narrative_ontology:cs_axiom_grounding('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', religious_autonomy_subordinate_to_equality, conventional).
narrative_ontology:cs_reference_frame('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', substantive_equality_framework).
narrative_ontology:cs_drift_state('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', contemporary_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3b6e4c77-7225-47f3-b8ba-cbf620861c7a', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_in_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the constitutional duty, enacting laws and issuing rulings that mandate the elimination of religious practices deemed oppressive. Gains legitimacy and power by being seen as the protector of marginalized groups, but faces resistance from religious communities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_judiciary_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Are the primary beneficiaries of state intervention, as practices like caste discrimination within religious contexts are targeted for elimination. Their ability to exit oppressive structures is often limited, making state action crucial.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    powerless, generational, trapped, national).

% Benefit from the elimination of gender-discriminatory religious practices, gaining greater autonomy and equality within their communities. Their ability to challenge these practices from within is often limited by social and religious norms.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_in_religious_communities, beneficiary,
    powerless, biographical, constrained, national).

% Bear the direct costs of this constraint, as their traditional religious practices and autonomy are curtailed or eliminated by state mandate. They view this as an infringement on fundamental religious freedom and often mobilize resistance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives, payer,
    organized, generational, identity_locked, national).

% Are compelled to alter or abandon long-standing practices and internal governance structures to comply with state directives. They face a choice between compliance, which may alienate their conservative adherents, or resistance, which risks legal penalties.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_institutions, payer,
    institutional, generational, constrained, national).

% Argue that the state should maintain strict neutrality and equal distance from all religions, without interfering in internal religious matters, even for reform. Their perspective is directly foreclosed by the reformist reading's premise of affirmative state duty.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, strict_neutrality_advocates, excluded,
    organized, biographical, analytical, national).

% Support state intervention for social reform but may differ on the scope or intensity of the 'affirmative duty' compared to the reformist reading. They observe the implementation and its effects, often providing critical analysis.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, principled_intervention_advocates, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, state_judiciary_legislature).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social norms and legal frameworks to ensure substantive equality and human dignity for marginalized groups, by actively reforming religious practices deemed oppressive, thereby fostering a more inclusive national identity.
% TRANSFER_FUNCTION: Transfers autonomy and decision-making power over certain religious practices from religious communities and institutions to the state, and ultimately to marginalized groups, in the name of social justice and equality.
% ABSENT_VOICES: Those who advocate for an absolute interpretation of religious freedom, asserting that the state has no legitimate authority to interfere in any religious practice, regardless of its social impact. Their arguments are systematically excluded by the premise of an affirmative state duty.
% DISAPPEARANCE_RATIONALE: If this constitutional duty vanished, the state would cease its active reform efforts, leading to the re-entrenchment of oppressive religious practices. Marginalized groups would lose a critical avenue for redress, and the social landscape would revert to patterns of inequality, forcing a reorganization of advocacy and resistance.
% FOUNDING_PROBLEM: The historical and ongoing oppression of marginalized groups (e.g., scheduled castes, women) within religious communities, where religious autonomy claims were used to justify practices that violated fundamental human rights and constitutional principles of equality.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, social justice movements, and independent legal scholars consistently corroborate the ongoing nature of these oppressive practices and the need for state intervention. While religious conservatives dispute the 'oppressive' label, the lived experiences of marginalized groups and empirical studies support the problem's persistence.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the state actively curtails religious autonomy, which is a significant cost for affected communities. Suppression is very high (0.90) as the state must actively enforce its mandates against deeply held religious beliefs and practices, often through legal and administrative means. Theater ratio is low (0.10) because the state's duty is genuinely active and interventionist, not merely performative. Resistance is very high (0.95) due to the fundamental nature of the rights being challenged and the identity-locked positions of the victims. Accessibility collapse is high (0.75) as the state's legal authority effectively removes the 'alternative' of continuing oppressive practices without consequence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups and the state, this constraint is a necessary mechanism for achieving substantive equality and justice. From the perspective of religious conservatives and institutions, it is an illegitimate overreach and an attack on fundamental religious freedom. The engine's classification as a Snare reflects the high extraction and suppression inherent in this reading, regardless of its stated reformist goals.
 *
 * DIRECTIONALITY LOGIC:
 *   The state (judiciary/legislature) acts as the agenda-setter, gaining legitimacy and power through its role as a social reformer. Marginalized groups (scheduled castes, women) are the direct beneficiaries, as the constraint aims to alleviate their oppression. Religious conservatives and institutions are the primary payers/victims, experiencing a direct loss of autonomy and traditional practices. Advocates for strict neutrality are structurally excluded, as their core premise is incompatible with this reading's affirmative duty.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's persistence is justified by its proponents as addressing a 'live' founding problem of ongoing oppression. However, its high extractiveness and suppression, coupled with strong resistance, indicate that its continued operation is not a matter of simple coordination but rather an active imposition of a particular social vision, maintained through state power. The debate over its legitimacy is central to its ongoing contestation, rather than a sign of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a valid instantiation of the ''constitutional_secularism'' kernel, or does its strong interventionist stance fundamentally alter the kernel''s nature?',
    'Analysis of constitutional jurisprudence and political theory to determine if the ''affirmative duty'' can be coherently derived from the foundational texts and principles of secularism, or if it represents a departure.',
    'If it fundamentally alters the kernel, it might be reclassified as a distinct, more extractive constraint that merely *claims* to be a reading of secularism. If valid, its classification as a Snare highlights the extractive potential within certain interpretations of secularism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''reformist_reading'' of the ''constitutional_secularism'' kernel.').

omega_variable(
    oppression_definition_ambiguity,
    'What constitutes ''oppression'' in religious practices, and who legitimately defines it (state, religious community, marginalized group)?',
    'Development of clear, universally accepted criteria for ''oppression'' that are not solely state-centric, or a robust, inclusive deliberative process involving all stakeholders to define and identify such practices.',
    'If the definition is solely state-imposed, the constraint''s suppression and extractiveness are amplified due to lack of consent. If a consensual definition emerges, it could shift towards a Tangled Rope or even a Rope, with reduced resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oppression_definition_ambiguity, preference, 'Ambiguity in defining ''oppression'' in religious practices.').

omega_variable(
    religious_autonomy_vs_equality_balance,
    'What is the optimal balance between religious autonomy and the state''s duty to ensure equality, and is the reformist reading''s supersession of autonomy proportionate?',
    'Comparative constitutional analysis across diverse secular states, and ongoing public discourse and judicial review that explicitly weighs the proportionality of state intervention against religious freedom claims.',
    'If the supersession is deemed disproportionate, the constraint''s legitimacy would be undermined, potentially leading to increased resistance and calls for reclassification towards a more benign type or outright abolition. If proportionate, its Snare classification is affirmed as a necessary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_autonomy_vs_equality_balance, conceptual, 'Proportionality of state intervention in religious autonomy for equality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1970, constitutional_secularism__reformist_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cons_tr_t1980, constitutional_secularism__reformist_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(cons_tr_t1990, constitutional_secularism__reformist_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(cons_tr_t2000, constitutional_secularism__reformist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(cons_tr_t2010, constitutional_secularism__reformist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(cons_tr_t2020, constitutional_secularism__reformist_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1970, constitutional_secularism__reformist_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(cons_be_t1980, constitutional_secularism__reformist_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(cons_be_t1990, constitutional_secularism__reformist_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(cons_be_t2000, constitutional_secularism__reformist_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(cons_be_t2010, constitutional_secularism__reformist_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(cons_be_t2020, constitutional_secularism__reformist_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1970, constitutional_secularism__reformist_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(cons_su_t1980, constitutional_secularism__reformist_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(cons_su_t1990, constitutional_secularism__reformist_reading, suppression_requirement, 1990, 0.83).
narrative_ontology:measurement(cons_su_t2000, constitutional_secularism__reformist_reading, suppression_requirement, 2000, 0.87).
narrative_ontology:measurement(cons_su_t2010, constitutional_secularism__reformist_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(cons_su_t2020, constitutional_secularism__reformist_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, religious_personal_laws).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, freedom_of_conscience).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, state_funding_of_religious_schools).

% DUAL FORMULATION NOTE:
% This constraint is the 'reformist_reading' of the 'constitutional_secularism' kernel, distinct from 'strict_neutrality_reading' and 'principled_intervention_reading' due to its strong affirmative duty and higher extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
