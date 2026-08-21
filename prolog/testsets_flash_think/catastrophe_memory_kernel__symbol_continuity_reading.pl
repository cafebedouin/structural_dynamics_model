% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual for Symbolic Continuity and Collective Identity
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in preserving symbolic
 *   continuity and collective identity within a community, particularly in
 *   the context of remembering a past catastrophe. It is one reading of the
 *   broader 'catastrophe_memory_kernel', focusing on the ritual's role as an
 *   identity marker and transmitter of shared meaning, rather than its
 *   adaptive survival function or trauma encoding. The constraint's low
 *   extractiveness reflects its primary function as a coordination mechanism
 *   for identity, with costs arising mainly from rigidity rather than direct
 *   extraction.
 *
 * KEY AGENTS:
 *   - community_members: Primary beneficiaries of identity coordination, but also payers of ritual rigidity (organized/identity_locked)
 *   - ritual_leaders: Agenda-setters who administer and interpret the rituals (institutional/constrained)
 *   - community_members_seeking_change: Payers of ritual rigidity, often excluded from modifying practices (moderate/constrained)
 *   - external_observers: Analytical observers studying the ritual's function (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual for Symbolic Continuity and Collective Identity").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '1b43e379-8eaf-44f8-85b9-2747aff45bd0').
narrative_ontology:cs_kernel_codification('1b43e379-8eaf-44f8-85b9-2747aff45bd0', implicit).
narrative_ontology:cs_authority_grounding('1b43e379-8eaf-44f8-85b9-2747aff45bd0', practice).
narrative_ontology:cs_interpretation_layer_present('1b43e379-8eaf-44f8-85b9-2747aff45bd0').
narrative_ontology:cs_reading_relation('1b43e379-8eaf-44f8-85b9-2747aff45bd0', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b43e379-8eaf-44f8-85b9-2747aff45bd0', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b43e379-8eaf-44f8-85b9-2747aff45bd0', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('1b43e379-8eaf-44f8-85b9-2747aff45bd0', foundational, identity_requires_unbroken_narrative).
narrative_ontology:cs_axiom_status(identity_requires_unbroken_narrative, holdable).
narrative_ontology:cs_axiom_grounding('1b43e379-8eaf-44f8-85b9-2747aff45bd0', identity_requires_unbroken_narrative, conventional).
narrative_ontology:cs_reference_frame('1b43e379-8eaf-44f8-85b9-2747aff45bd0', unbroken_lineage_of_meaning).
narrative_ontology:cs_drift_state('1b43e379-8eaf-44f8-85b9-2747aff45bd0', contemporary_secularization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b43e379-8eaf-44f8-85b9-2747aff45bd0', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, community_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, community_members_seeking_change).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, symbolic_continuity_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals to reinforce their collective identity and connection to a shared past. They benefit from the sense of belonging and continuity, but may bear the cost of ritual rigidity or anachronism.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, community_members, beneficiary,
    organized, generational, identity_locked, local).

% Administer and interpret the rituals, ensuring their proper execution and transmission across generations. Their authority is often derived from their role in preserving tradition, making them beneficiaries of the constraint's persistence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Experience the rigidity of ritual as a cost, finding it difficult to adapt practices to contemporary needs or interpretations. They may feel excluded if their calls for modification are resisted, bearing the cost of maintaining a form that no longer fully resonates.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, community_members_seeking_change, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, community_members_seeking_change, excluded).

% Academics (e.g., anthropologists, historians) who study the ritual's function in preserving memory and identity. They analyze its structure and impact without direct participation or benefit/cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and identity by providing a shared symbolic framework and practice through which community members connect to their past and reinforce their belonging in the present.
% TRANSFER_FUNCTION: Transfers symbolic meaning, historical narratives, and a sense of collective identity from past generations to present and future community members, primarily through shared experience and narrative repetition.
% ABSENT_VOICES: Those who question the efficacy or relevance of the ritual in its current form, or who advocate for radical adaptation or abandonment, are often marginalized or excluded from the interpretive process, as their views threaten the perceived continuity.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would likely experience a significant loss of collective identity, fragmentation of shared memory, and a weakening of social cohesion, leading to a reorganization of how its members understand their past and present.
% FOUNDING_PROBLEM: The problem of preserving the memory of a catastrophic event and maintaining a distinct collective identity in its aftermath, ensuring that the community's narrative and values endure across generations.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and historians studying the community's history and social structures corroborate that the challenge of maintaining collective memory and identity in the face of external pressures remains a live concern, even if the specific catastrophe is distant.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the ritual's primary function is to coordinate identity and meaning, not to extract resources. Any 'extraction' is diffuse, in the form of rigidity costs borne by those seeking change. Suppression (0.40) is moderate, stemming from social pressure to conform to tradition and the difficulty of challenging established practices. Theater ratio (0.50) is moderate, reflecting the performative nature of ritual, where symbolic acts are crucial for transmitting meaning, but also where some elements may persist due to inertia rather than active function. Accessibility collapse (0.40) is moderate; while alternatives for identity formation exist, the ritual offers a deeply embedded and powerful one. Resistance (0.10) is low, as internal challenges are often subtle and focused on adaptation rather than outright rejection.
 *
 * PERSPECTIVAL GAP:
 *   Community members who strongly identify with the tradition perceive the ritual as a pure rope, essential for their collective being. Those seeking change, however, experience it as a constraint that limits adaptation and imposes costs through its rigidity. Ritual leaders often view it as a necessary, if sometimes challenging, duty to preserve the lineage.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are beneficiaries as they gain a strong sense of identity and belonging. Ritual leaders also benefit from the authority and purpose derived from their role. Community members seeking change are victims, as they bear the costs of the ritual's inflexibility and the social pressure to conform. The constraint primarily subsidizes collective identity and meaning-making.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this ritual primarily about symbolic continuity, or does it also encode survival competence, intergenerational trauma, or group boundary maintenance?',
    'Comparative ethnographic studies across communities with similar historical catastrophes, historical analysis of ritual evolution, and psychological assessment of participants'' experiences.',
    'Resolution could lead to reclassification of the ritual under a different reading of the ''catastrophe_memory_kernel'', potentially altering its perceived beneficiaries, victims, and extractiveness profile if, for example, a strong trauma-encoding function is identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity regarding the primary function of the ritual within the broader catastrophe memory kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of ritual modification structural (e.g., lack of alternative practices, institutional inertia) or internalized (e.g., deep-seated belief in the sacredness of tradition, fear of identity loss)?',
    'Longitudinal studies tracking community responses to external pressures for change, and qualitative research exploring individual motivations for adherence versus resistance. If suppression persists after external barriers are removed, it suggests internalized mechanisms.',
    'If internalized suppression is dominant, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would make the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ritual modification.').

omega_variable(
    symbolic_efficacy_vs_performance,
    'To what extent does the ritual genuinely transmit symbolic continuity and identity, versus merely performing a theatrical maintenance of tradition without deep engagement?',
    'Sociological and psychological studies measuring the actual impact of ritual participation on identity formation and historical understanding, compared to self-reported adherence or mere attendance. A high theater ratio with low actual impact would indicate a degraded function.',
    'If the ritual''s efficacy is found to be low despite high performance, the constraint''s ''rope'' classification might shift towards ''piton'', indicating an atrophied function maintained by inertia and theatricality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_efficacy_vs_performance, empirical, 'The actual efficacy of symbolic transmission versus mere performative maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 80, 0.4).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'catastrophe_memory_kernel', each focusing on a distinct function of ritual in response to collective memory of catastrophe. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
