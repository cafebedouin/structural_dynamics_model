% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy (Composite Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the legitimacy of honor violence, viewed
 *   through a 'composite reading' that acknowledges its decline was due to
 *   both external costs (e.g., state legal intervention) and internal
 *   conceptual redefinition of 'honor' itself. Historically a deeply
 *   entrenched social mechanism, its legitimacy has eroded over time,
 *   transitioning from a highly extractive system to a degraded, inertial
 *   form. The claimed type 'piton' reflects its current state of decline,
 *   where its primary function has atrophied, but residual elements persist
 *   due to inertia and performative maintenance, particularly in certain
 *   cultural contexts.
 *
 * KEY AGENTS:
 *   - honor_bound_elites: Agenda-setter/Beneficiary (institutional/identity_locked)
 *   - community_patriarchs: Beneficiary (powerful/constrained)
 *   - victims_of_violence: Payer (powerless/trapped)
 *   - disgraced_families: Payer (powerless/constrained)
 *   - legal_authorities: Observer/Agenda-setter (institutional/analytical)
 *   - social_reformers: Excluded (moderate/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.65).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.7).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy (Composite Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '9e671274-0d57-4e8c-91c3-ac8e7c02667c').
narrative_ontology:cs_kernel_codification('9e671274-0d57-4e8c-91c3-ac8e7c02667c', implicit).
narrative_ontology:cs_authority_grounding('9e671274-0d57-4e8c-91c3-ac8e7c02667c', practice).
narrative_ontology:cs_interpretation_layer_present('9e671274-0d57-4e8c-91c3-ac8e7c02667c').
narrative_ontology:cs_reading_relation('9e671274-0d57-4e8c-91c3-ac8e7c02667c', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e671274-0d57-4e8c-91c3-ac8e7c02667c', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('9e671274-0d57-4e8c-91c3-ac8e7c02667c', foundational, violence_is_a_legitimate_expression_of_honor).
narrative_ontology:cs_axiom_status(violence_is_a_legitimate_expression_of_honor, overridden).
narrative_ontology:cs_axiom_grounding('9e671274-0d57-4e8c-91c3-ac8e7c02667c', violence_is_a_legitimate_expression_of_honor, deontological).
narrative_ontology:cs_axiom('9e671274-0d57-4e8c-91c3-ac8e7c02667c', foundational, honor_is_a_social_construct_subject_to_redefinition).
narrative_ontology:cs_axiom_status(honor_is_a_social_construct_subject_to_redefinition, holdable).
narrative_ontology:cs_axiom_grounding('9e671274-0d57-4e8c-91c3-ac8e7c02667c', honor_is_a_social_construct_subject_to_redefinition, empirically_contingent).
narrative_ontology:cs_axiom('9e671274-0d57-4e8c-91c3-ac8e7c02667c', foundational, external_costs_can_erode_social_practices).
narrative_ontology:cs_axiom_status(external_costs_can_erode_social_practices, holdable).
narrative_ontology:cs_axiom_grounding('9e671274-0d57-4e8c-91c3-ac8e7c02667c', external_costs_can_erode_social_practices, empirically_contingent).
narrative_ontology:cs_reference_frame('9e671274-0d57-4e8c-91c3-ac8e7c02667c', traditional_honor_code_legitimacy).
narrative_ontology:cs_drift_state('9e671274-0d57-4e8c-91c3-ac8e7c02667c', contemporary_human_rights_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('9e671274-0d57-4e8c-91c3-ac8e7c02667c', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, honor_bound_elites).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, community_patriarchs).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, victims_of_violence).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, disgraced_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically, these individuals defined and enforced the honor code, using violence to maintain status and social order. Their identity and power were deeply intertwined with the system. As the system declined, their authority eroded.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, honor_bound_elites, agenda_setter,
    institutional, generational, identity_locked, local).

% Benefited from the social order and status conferred by the honor system, often acting as enforcers or adjudicators of disputes. Their adherence to the code was a source of respect and influence.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, community_patriarchs, beneficiary,
    powerful, biographical, constrained, local).

% Suffered physical harm, social ostracization, or death as a result of honor violence. They had virtually no exit options within the system and bore the direct costs.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, victims_of_violence, payer,
    powerless, immediate, trapped, local).

% Families whose honor was perceived to be violated, leading to social stigma, economic hardship, and often pressure to engage in retaliatory violence. Their options were limited by social norms and fear of further dishonor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, disgraced_families, payer,
    powerless, generational, constrained, local).

% External state or religious legal systems that increasingly challenged the legitimacy of honor violence, imposing legal costs and penalties. Their growing power contributed to the 'drop' mechanism of decline.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, legal_authorities, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, legal_authorities, agenda_setter).

% Advocated for the redefinition of honor to exclude violence and for the abolition of honor codes. Initially excluded from traditional power structures, their efforts contributed to the 'contraction' mechanism of decline.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, social_reformers, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, it coordinated social behavior, maintained status hierarchies, and provided a framework for dispute resolution within communities, particularly where state legal systems were weak or mistrusted.
% TRANSFER_FUNCTION: Transferred social status, respect, and control to those who upheld and enforced the honor code, while imposing violence, subjugation, and social exclusion on those who violated or were perceived to violate it.
% ABSENT_VOICES: Victims of violence, women, and those who rejected the premise of honor-based violence were systematically excluded from the discourse. They would have argued for the inherent injustice and brutality of the system.
% DISAPPEARANCE_RATIONALE: If honor violence and its underlying legitimacy vanished overnight, the social structures, power dynamics, and dispute resolution mechanisms in historically affected communities would undergo profound reorganization. New legal and social norms would fill the vacuum, and the concept of 'honor' itself would be fundamentally re-evaluated.
% FOUNDING_PROBLEM: To establish and maintain social order, enforce norms, and resolve perceived affronts to personal or family reputation in societies with weak centralized authority or strong emphasis on personal and collective honor.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal reforms, sociological studies of traditional societies, and human rights reports from international bodies corroborate that the original problem of maintaining order through honor violence is largely superseded by modern legal systems and evolving social ethics, though residual practices persist in some regions.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high initial extractiveness and suppression reflect the historical reality of honor violence as a coercive system. The decline in these metrics over the interval (1800-2000) reflects the 'overdetermined decline' described by the composite reading. Theater ratio rises as the functional justification for violence diminishes, but the social inertia and performative aspects of 'honor' persist. Accessibility collapse remains high for victims, even as the system declines, due to internalized norms and social pressure. Resistance is moderate, reflecting both historical opposition and the difficulty of challenging deeply embedded cultural practices.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of honor-bound elites, the system was a necessary mechanism for social order and personal dignity, justifying its costs. For victims, it was pure extraction and subjugation. Legal authorities and social reformers view it as an anachronistic and harmful practice that must be dismantled. The composite reading attempts to integrate these perspectives by explaining the multi-faceted nature of its decline.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-bound elites and community patriarchs were beneficiaries, gaining status and control (low d). Victims and disgraced families were targets, bearing the direct costs of violence and social stigma (high d). Legal authorities, as external enforcers of new norms, acted to suppress the constraint, while social reformers actively worked to redefine the underlying concepts, shifting the directionality of the entire system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: its original problem (maintaining social order via honor violence) is largely 'dead' due to the rise of state legal systems and evolving ethics. However, the constraint persists as a 'piton' due to institutional inertia, identity-locked adherence, and residual social pressure, rather than genuine functional necessity. The composite reading highlights that this mandatrophy was driven by both external pressures (making the 'drop' mechanism effective) and internal conceptual shifts (the 'contraction' mechanism), preventing a mislabeling as purely external or internal decline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_weight_of_decline_mechanisms,
    'What was the relative contribution of ''external costs'' (drop) versus ''conceptual redefinition'' (contraction) to the overall decline of honor violence legitimacy?',
    'Comparative historical analysis across different societies and time periods, correlating the timing and intensity of legal reforms with shifts in cultural definitions of honor.',
    'If external costs were dominant, the decline is primarily a story of state power and legal enforcement. If conceptual redefinition was dominant, it''s a story of evolving social ethics and internal cultural change. The composite reading asserts both, but their relative weight impacts policy interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_weight_of_decline_mechanisms, empirical, 'Relative importance of external vs. internal drivers of decline.').

omega_variable(
    honor_violence_legitimacy_kernel_reading,
    'This constraint is the ''composite_reading'' of the ''honor_violence_legitimacy'' kernel. How would the classification change if a sibling reading were adopted?',
    'Adopting the ''honor_violence_legitimacy__drop_reading'' would emphasize external legal and economic pressures, potentially leading to a classification that highlights the role of state enforcement in dismantling the constraint. Adopting the ''honor_violence_legitimacy__contraction_reading'' would emphasize internal cultural shifts, leading to a classification focused on the evolution of social norms and identity.',
    'The ''composite_reading'' provides a more comprehensive understanding of the decline, potentially leading to a more nuanced classification that captures both coercive and cultural aspects. Sibling readings would offer partial, but distinct, classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(honor_violence_legitimacy_kernel_reading, conceptual, 'Impact of adopting a sibling reading of the honor_violence_legitimacy kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, economic sanctions) or internalized (social shame, fear of ostracization, identity-locked adherence to honor code)?',
    'Post-exit suppression trajectory: if honor violence persists in communities after legal mechanisms are removed, reclassify as partially internalized. Ethnographic studies on the persistence of honor-based norms despite legal prohibition.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — individuals carry the suppression with them after formal mechanisms are removed, making the piton more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in honor violence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 1800, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__composite_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__composite_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__composite_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(hono_tr_t1950, honor_violence_legitimacy__composite_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(hono_tr_t2000, honor_violence_legitimacy__composite_reading, theater_ratio, 2000, 0.4).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__composite_reading, base_extractiveness, 1800, 0.85).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__composite_reading, base_extractiveness, 1850, 0.78).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__composite_reading, base_extractiveness, 1900, 0.72).
narrative_ontology:measurement(hono_be_t1950, honor_violence_legitimacy__composite_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(hono_be_t2000, honor_violence_legitimacy__composite_reading, base_extractiveness, 2000, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__composite_reading, suppression_requirement, 1800, 0.9).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__composite_reading, suppression_requirement, 1850, 0.82).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__composite_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(hono_su_t1950, honor_violence_legitimacy__composite_reading, suppression_requirement, 1950, 0.71).
narrative_ontology:measurement(hono_su_t2000, honor_violence_legitimacy__composite_reading, suppression_requirement, 2000, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
