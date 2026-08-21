% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Dueling's Structural Legitimacy (Drop Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the 'drop_reading' of the 'honor_violence_legitimacy'
 *   kernel. It posits that dueling remained structurally legitimate within
 *   certain social circles but became practically rare due to increasing
 *   external costs (e.g., legal penalties, social stigma, changing economic
 *   structures). The underlying social code of honor, and dueling's place
 *   within it as a 'thinkable' if rarely practiced option, persisted, in
 *   contrast to readings that emphasize a redefinition of honor itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.25).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.15).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Dueling's Structural Legitimacy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '9606e165-aeeb-4a8e-a645-9093fd420799').
narrative_ontology:cs_kernel_codification('9606e165-aeeb-4a8e-a645-9093fd420799', implicit).
narrative_ontology:cs_authority_grounding('9606e165-aeeb-4a8e-a645-9093fd420799', practice).
narrative_ontology:cs_interpretation_layer_present('9606e165-aeeb-4a8e-a645-9093fd420799').
narrative_ontology:cs_reading_relation('9606e165-aeeb-4a8e-a645-9093fd420799', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9606e165-aeeb-4a8e-a645-9093fd420799', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('9606e165-aeeb-4a8e-a645-9093fd420799', foundational, honor_requires_personal_defense).
narrative_ontology:cs_axiom_status(honor_requires_personal_defense, holdable).
narrative_ontology:cs_axiom_grounding('9606e165-aeeb-4a8e-a645-9093fd420799', honor_requires_personal_defense, deontological).
narrative_ontology:cs_axiom('9606e165-aeeb-4a8e-a645-9093fd420799', secondary, external_costs_deter_practice).
narrative_ontology:cs_axiom_status(external_costs_deter_practice, holdable).
narrative_ontology:cs_axiom_grounding('9606e165-aeeb-4a8e-a645-9093fd420799', external_costs_deter_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('9606e165-aeeb-4a8e-a645-9093fd420799', honor_code_as_self_adjudicating).
narrative_ontology:cs_drift_state('9606e165-aeeb-4a8e-a645-9093fd420799', post_enlightenment_legal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9606e165-aeeb-4a8e-a645-9093fd420799', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, social_elites).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, honor_system_adherents).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, individuals_facing_honor_challenges).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, society_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and uphold the honor code, benefiting from the social order and status hierarchies it maintains. They tacitly acknowledge dueling's legitimacy even as its practice declines.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, social_elites, agenda_setter,
    institutional, generational, mobile, national).

% Benefit from the abstract concept of honor being defensible, even if they do not personally engage in dueling. Their social standing is tied to the honor system.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, honor_system_adherents, beneficiary,
    moderate, biographical, constrained, local).

% Bear the social pressure, stigma, and potential costs of honor disputes, even if dueling itself is rare. Their social identity is often tied to upholding honor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, individuals_facing_honor_challenges, payer,
    powerless, immediate, identity_locked, local).

% Officially condemn dueling and enforce laws against it, but may tacitly acknowledge its social legitimacy or the underlying honor code in certain contexts, contributing to its persistence as a 'thinkable' option.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, legal_authorities, observer,
    institutional, generational, analytical, national).

% Increasingly views dueling as barbaric and anachronistic, contributing to its practical decline through social pressure and stigma, but not directly challenging its structural legitimacy within elite circles.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, public_opinion, excluded,
    organized, biographical, constrained, national).

% Bears the diffuse costs of a social system that implicitly condones violence for honor, even if the practice is rare. This includes the potential for violence and the perpetuation of rigid social hierarchies.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, society_at_large, payer,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal, if costly and increasingly rare, mechanism for resolving extreme honor disputes among elites, thereby maintaining social order and status hierarchies within those circles.
% TRANSFER_FUNCTION: Transfers social capital and status to those who uphold the honor code, and potentially physical harm or death to those who fail to defend their honor, even if the latter is rare in practice.
% ABSENT_VOICES: Those who reject the entire concept of honor violence, or those outside the elite circles where dueling was practiced, are largely excluded. They would argue for legal resolution and an end to violence, but their voices are not central to the honor system's internal logic.
% DISAPPEARANCE_RATIONALE: If the structural legitimacy of dueling vanished overnight, the entire social system of honor that it underpinned would need to find new, non-violent mechanisms for dispute resolution, or risk collapse and redefinition of social status.
% FOUNDING_PROBLEM: To provide a definitive, public means for gentlemen to defend their honor against perceived slights, preventing endless feuds and maintaining social order among elites by offering a 'final' resolution.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal records show attempts to suppress dueling while acknowledging its social function. Sociological analyses from outside the elite circles confirm the shift in practice and the persistence of the underlying honor code, indicating the original problem is now largely handled by other means, but the social structure persists.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Piton because its primary function (resolving honor disputes through dueling) has atrophied in practice, but its structural legitimacy persists due to institutional inertia and the continued social value placed on honor. Extractiveness and suppression are low because the actual practice is rare, meaning it's not actively extracting or suppressing on a wide scale. However, the residual threat and social pressure (reflected in 'individuals_facing_honor_challenges' and 'society_at_large' as victims) mean these metrics are not zero. The theater ratio is moderate, reflecting the performative maintenance of the idea of dueling's legitimacy, even as its practical utility declined.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social elites, the constraint represents a legitimate, if archaic, mechanism for maintaining honor. From the perspective of individuals facing honor challenges, it represents a persistent social pressure or threat. The engine's classification as a Piton captures the divergence between the claimed legitimacy and the atrophied practical function.
 *
 * DIRECTIONALITY LOGIC:
 *   Social elites and honor system adherents are beneficiaries, as they benefit from the abstract concept of honor being defensible and the social order it underpins, even without active dueling. Individuals facing honor challenges and society at large are victims, bearing the social pressure and diffuse costs of a system that still implicitly condones violence for honor. Legal authorities and public opinion act as observers or excluded parties, influencing the external costs that make dueling rare.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (providing a definitive resolution for honor disputes through violence) has largely atrophied in practice due to external costs. However, its structural legitimacy persists due to institutional inertia and the continued social value placed on honor. It functions more as a theatrical maintenance of a past social order than an active mechanism, fitting the Piton classification. The 'founding_problem_status' being 'dead' while the 'disappearance_verdict' is 'world_rearranges' further supports the Piton classification, indicating a mandate that has outlived its original function but whose removal would still cause significant social reorganization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_legitimacy_vs_social_acceptance,
    'Is dueling''s ''structural legitimacy'' truly distinct from its ''social acceptance'' in practice, or does the decline in practice inevitably erode legitimacy over time?',
    'Longitudinal studies of social norms and legal enforcement: if legal condemnation and social stigma eventually lead to a complete rejection of dueling''s underlying principles, then legitimacy is not independent of practice.',
    'If legitimacy is tied to practice, the constraint''s true extractiveness and suppression might be lower than currently estimated, as the social structure itself is weakening. If distinct, the Piton classification holds more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_legitimacy_vs_social_acceptance, conceptual, 'Distinction between formal legitimacy and practical social acceptance.').

omega_variable(
    external_costs_vs_internal_redefinition,
    'To what extent did external costs (e.g., legal penalties, changing economic structures) *cause* the decline in dueling, versus an internal redefinition of honor itself (as argued by the ''contraction_reading'')?',
    'Comparative historical analysis across societies with varying legal/economic pressures but similar honor codes: if decline correlates strongly with external costs regardless of internal redefinition, this reading is strengthened.',
    'If external costs were the dominant factor, this ''drop_reading'' is strongly supported. If internal redefinition was more significant, the ''contraction_reading'' gains strength, potentially reclassifying this constraint as a less impactful Piton or even a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_costs_vs_internal_redefinition, empirical, 'Relative causal weight of external costs versus internal redefinition in dueling''s decline.').

omega_variable(
    persistence_of_conceptual_availability,
    'How does the persistence of dueling as a ''thinkable'' option (this reading) interact with the redefinition of honor (contraction_reading) and the overdetermined decline (composite_reading)?',
    'Analysis of literary and cultural representations of honor disputes over time: if dueling continues to appear as a valid, if tragic, option in narratives, it supports its conceptual availability.',
    'If conceptual availability is strong, it reinforces the Piton classification by highlighting the theatrical maintenance of the idea. If it fades, the ''contraction_reading'' gains ground, suggesting a more fundamental shift in the honor system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_of_conceptual_availability, conceptual, 'Interaction between conceptual availability and honor redefinition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__drop_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1725, honor_violence_legitimacy__drop_reading, theater_ratio, 1725, 0.15).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__drop_reading, theater_ratio, 1750, 0.2).
narrative_ontology:measurement(hono_tr_t1775, honor_violence_legitimacy__drop_reading, theater_ratio, 1775, 0.25).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__drop_reading, theater_ratio, 1800, 0.28).
narrative_ontology:measurement(hono_tr_t1825, honor_violence_legitimacy__drop_reading, theater_ratio, 1825, 0.3).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__drop_reading, theater_ratio, 1850, 0.3).
narrative_ontology:measurement(hono_tr_t1875, honor_violence_legitimacy__drop_reading, theater_ratio, 1875, 0.3).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__drop_reading, theater_ratio, 1900, 0.3).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__drop_reading, base_extractiveness, 1700, 0.4).
narrative_ontology:measurement(hono_be_t1725, honor_violence_legitimacy__drop_reading, base_extractiveness, 1725, 0.35).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__drop_reading, base_extractiveness, 1750, 0.3).
narrative_ontology:measurement(hono_be_t1775, honor_violence_legitimacy__drop_reading, base_extractiveness, 1775, 0.28).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__drop_reading, base_extractiveness, 1800, 0.26).
narrative_ontology:measurement(hono_be_t1825, honor_violence_legitimacy__drop_reading, base_extractiveness, 1825, 0.25).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__drop_reading, base_extractiveness, 1850, 0.25).
narrative_ontology:measurement(hono_be_t1875, honor_violence_legitimacy__drop_reading, base_extractiveness, 1875, 0.25).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__drop_reading, base_extractiveness, 1900, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__drop_reading, suppression_requirement, 1700, 0.25).
narrative_ontology:measurement(hono_su_t1725, honor_violence_legitimacy__drop_reading, suppression_requirement, 1725, 0.22).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__drop_reading, suppression_requirement, 1750, 0.19).
narrative_ontology:measurement(hono_su_t1775, honor_violence_legitimacy__drop_reading, suppression_requirement, 1775, 0.17).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__drop_reading, suppression_requirement, 1800, 0.16).
narrative_ontology:measurement(hono_su_t1825, honor_violence_legitimacy__drop_reading, suppression_requirement, 1825, 0.15).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__drop_reading, suppression_requirement, 1850, 0.15).
narrative_ontology:measurement(hono_su_t1875, honor_violence_legitimacy__drop_reading, suppression_requirement, 1875, 0.15).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__drop_reading, suppression_requirement, 1900, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'drop_reading' of the 'honor_violence_legitimacy' kernel, focusing on the decline of dueling due to external costs while maintaining structural legitimacy. It is distinct from the 'contraction_reading' (honor redefined to exclude violence) and the 'composite_reading' (both factors operating simultaneously).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
