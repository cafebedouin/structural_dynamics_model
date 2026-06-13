% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Catastrophe Memory Transmission (Hybrid Embedded Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid embedded' reading of catastrophe
 *   memory transmission, where survival competence is inextricably linked to
 *   symbolic ritual form. Ritual fidelity is not merely cultural preservation
 *   but the direct transmission of operational capacity through
 *   non-propositional, embodied knowledge. The constraint is claimed as a
 *   Rope due to its genuine coordination function, with a Mountain-like
 *   substrate in the embodied nature of the knowledge. There are no clear
 *   victims unless the practice is discontinued, which would degrade the
 *   community's survival capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe Memory Transmission (Hybrid Embedded Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:emerges_naturally(catastrophe_memory_transmission__hybrid_embedded_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'c48f9bb7-f2e3-4652-b16b-16054bd751a2').
narrative_ontology:cs_kernel_codification('c48f9bb7-f2e3-4652-b16b-16054bd751a2', implicit).
narrative_ontology:cs_authority_grounding('c48f9bb7-f2e3-4652-b16b-16054bd751a2', practice).
narrative_ontology:cs_interpretation_layer_present('c48f9bb7-f2e3-4652-b16b-16054bd751a2').
narrative_ontology:cs_reading_relation('c48f9bb7-f2e3-4652-b16b-16054bd751a2', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c48f9bb7-f2e3-4652-b16b-16054bd751a2', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_axiom('c48f9bb7-f2e3-4652-b16b-16054bd751a2', foundational, form_and_function_are_inseparable).
narrative_ontology:cs_axiom_status(form_and_function_are_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('c48f9bb7-f2e3-4652-b16b-16054bd751a2', form_and_function_are_inseparable, empirically_contingent).
narrative_ontology:cs_axiom('c48f9bb7-f2e3-4652-b16b-16054bd751a2', foundational, non_propositional_knowledge_is_primary).
narrative_ontology:cs_axiom_status(non_propositional_knowledge_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('c48f9bb7-f2e3-4652-b16b-16054bd751a2', non_propositional_knowledge_is_primary, empirically_contingent).
narrative_ontology:cs_reference_frame('c48f9bb7-f2e3-4652-b16b-16054bd751a2', ancestral_fidelity_framework).
narrative_ontology:cs_drift_state('c48f9bb7-f2e3-4652-b16b-16054bd751a2', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c48f9bb7-f2e3-4652-b16b-16054bd751a2', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, embodied_cognition_theory).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__hybrid_embedded_reading, cultural_transmission_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the transmission of survival knowledge and collective identity through ritual. Their sense of belonging and capacity to respond to future crises is tied to the fidelity of these practices. Deviating from the ritual form feels like a betrayal of their ancestors and a threat to their future.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, community_members, beneficiary,
    organized, generational, identity_locked, local).

% Are responsible for maintaining the fidelity of the ritual. They are the primary conduits for transmitting the embedded knowledge. Their authority and social role are derived from their competence in performing and teaching the ritual. Any deviation from the form is seen as a failure of their duty.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_practitioners, agenda_setter,
    powerful, biographical, constrained, local).

% Study the community's rituals to understand mechanisms of cultural transmission and resilience. They analyze the relationship between symbolic form and operational competence without direct participation or benefit from the ritual's function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, external_observers, observer,
    analytical, generational, analytical, global).

% Argue for simplifying or updating rituals to make them more accessible or relevant to contemporary life. They are often seen as threatening the integrity of the embedded knowledge and are excluded from decision-making regarding ritual form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, modernization_advocates, excluded,
    moderate, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action and memory by embedding survival competence directly into symbolic ritual forms, ensuring that the 'how' of survival is transmitted through the 'what' of practice, rather than explicit instruction.
% TRANSFER_FUNCTION: Transfers non-propositional, embodied knowledge and collective resilience strategies across generations through the faithful enactment of ritual forms. It also transfers social cohesion and identity.
% ABSENT_VOICES: Modernization advocates who prioritize efficiency or contemporary relevance over ritual fidelity are excluded. They would argue that the strict adherence to form is an unnecessary burden, potentially losing the embedded knowledge through simplification.
% DISAPPEARANCE_RATIONALE: If the constraint of inseparable form and competence vanished, the community would lose its primary mechanism for transmitting survival knowledge and collective identity. Rituals would become mere performance, and the capacity to respond to future catastrophes would degrade, leading to a profound cultural and practical rearrangement.
% FOUNDING_PROBLEM: How to transmit complex, context-dependent survival knowledge and collective resilience strategies across generations, especially after a catastrophic event, when explicit propositional knowledge is insufficient or easily lost.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies and historical records from outside the community corroborate that communities facing recurrent threats develop highly ritualized practices for knowledge transmission. The ongoing existence of environmental or social threats confirms the problem remains live, as attested by external researchers and disaster relief organizations.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the 'cost' of ritual fidelity is the 'cost' of the coordination itself – the effort required to maintain a complex, embodied knowledge system. Suppression is also low (0.2) as adherence is largely self-enforced through social norms and the perceived necessity for survival, rather than overt coercion. Theater ratio is minimal (0.1) because the ritual's primary function is genuinely operational, not performative. Accessibility collapse is high (0.8) because once the link between form and competence is understood, alternatives for transmitting this specific type of knowledge are severely limited. Resistance is low (0.05) because the community largely accepts the necessity of fidelity for survival.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members and practitioners, the constraint is a vital, almost natural, mechanism for survival. From an external, purely rationalist perspective, the strict adherence to form might appear inefficient or arbitrary, but this reading asserts that such a view misses the embedded, non-propositional nature of the knowledge.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are beneficiaries (d near 0.0) as they gain survival competence and identity. Ritual practitioners are agenda-setters (d near 0.1) as they administer the constraint, but also benefit from the community's resilience. There are no direct victims, as the 'cost' is the coordination itself. Modernization advocates are excluded, as their proposals would undermine the constraint's core mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'To what extent is the operational competence truly inseparable from the symbolic form, or could the ''content'' be extracted and transmitted through more ''efficient'' means?',
    'Empirical study of communities that have attempted to modernize or simplify their rituals after a catastrophe: does the loss of fidelity correlate with a measurable degradation in survival outcomes or collective resilience?',
    'If separable, the constraint''s ''mountain'' aspect (natural inseparability) would weaken, and its ''rope'' aspect (coordination cost) might be re-evaluated as potentially extractive if fidelity is enforced beyond functional necessity. If inseparable, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'The degree to which ritual form and operational function are truly co-constitutive.').

omega_variable(
    identity_vs_competence_priority,
    'Is the primary function of ritual fidelity the transmission of survival competence, or the preservation of collective identity and meaning, with competence as a secondary effect?',
    'Analysis of community narratives and responses to external threats: do they prioritize the ''correctness'' of action for survival, or the ''meaningfulness'' of the ritual for identity, when faced with conflicting pressures?',
    'If identity is primary, the constraint leans closer to the ''symbol_continuity_reading'' (a different constraint), potentially shifting its classification towards a more purely ''rope'' or even ''tangled_rope'' if identity is enforced coercively. If competence is primary, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_vs_competence_priority, conceptual, 'The primary purpose of ritual fidelity: survival competence or identity preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 75, 0.09).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 75, 0.19).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'catastrophe_memory_transmission' kernel. This 'hybrid embedded' reading emphasizes the co-constitution of ritual form and operational competence, distinct from readings that prioritize either symbolic continuity or explicit operational competence alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
