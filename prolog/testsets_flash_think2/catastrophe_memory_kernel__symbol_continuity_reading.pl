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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual for Symbolic Continuity of Catastrophe Memory
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in preserving symbolic
 *   continuity and collective identity across time, specifically in the
 *   context of remembering a past catastrophe. It is one reading of the
 *   'catastrophe_memory_kernel', focusing on the ritual's role as an identity
 *   marker and transmitter of cultural memory. The constraint's low
 *   extractiveness reflects its primary function as a coordination mechanism
 *   for identity, though it imposes costs through rigidity.
 *
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
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual for Symbolic Continuity of Catastrophe Memory").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'a692fdeb-88c6-4510-9033-acb45da2fe16').
narrative_ontology:cs_kernel_codification('a692fdeb-88c6-4510-9033-acb45da2fe16', formalized).
narrative_ontology:cs_authority_grounding('a692fdeb-88c6-4510-9033-acb45da2fe16', lineage).
narrative_ontology:cs_interpretation_layer_present('a692fdeb-88c6-4510-9033-acb45da2fe16').
narrative_ontology:cs_reading_relation('a692fdeb-88c6-4510-9033-acb45da2fe16', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a692fdeb-88c6-4510-9033-acb45da2fe16', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('a692fdeb-88c6-4510-9033-acb45da2fe16', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('a692fdeb-88c6-4510-9033-acb45da2fe16', foundational, symbolic_transmission_is_identity).
narrative_ontology:cs_axiom_status(symbolic_transmission_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('a692fdeb-88c6-4510-9033-acb45da2fe16', symbolic_transmission_is_identity, deontological).
narrative_ontology:cs_axiom('a692fdeb-88c6-4510-9033-acb45da2fe16', secondary, fidelity_to_form_ensures_continuity).
narrative_ontology:cs_axiom_status(fidelity_to_form_ensures_continuity, holdable).
narrative_ontology:cs_axiom_grounding('a692fdeb-88c6-4510-9033-acb45da2fe16', fidelity_to_form_ensures_continuity, conventional).
narrative_ontology:cs_reference_frame('a692fdeb-88c6-4510-9033-acb45da2fe16', unbroken_lineage_of_remembrance).
narrative_ontology:cs_drift_state('a692fdeb-88c6-4510-9033-acb45da2fe16', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a692fdeb-88c6-4510-9033-acb45da2fe16', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, community_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, individual_mourners).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_reformers).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_preservation).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive a strong sense of collective identity, belonging, and historical continuity from participating in and upholding the ritual practices. The ritual provides a stable framework for understanding their shared past.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, community_members, beneficiary,
    moderate, generational, constrained, regional).

% Are responsible for maintaining the fidelity of ritual forms, transmitting them across generations, and interpreting their symbolic meaning. Their authority is often tied to the preservation of this continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_leaders, agenda_setter,
    organized, generational, constrained, regional).

% Are expected to conform to prescribed ritual forms, which may limit their individual expression of grief or their ability to adapt practices to personal needs. Deviation can lead to social exclusion or a sense of alienation from the collective identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, individual_mourners, payer,
    powerless, immediate, identity_locked, local).

% Advocate for modifications to ritual practices to better suit contemporary social contexts or individual needs. They often face resistance from ritual leaders and community members who prioritize strict adherence to tradition for the sake of continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_reformers, excluded,
    moderate, biographical, constrained, regional).

% Analyze the sociological, anthropological, and psychological functions of ritual in preserving collective memory and identity, often from an academic or detached perspective.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared understanding of past catastrophic events, reinforces collective identity, and transmits cultural memory and symbolic meaning across generations, ensuring group cohesion and historical awareness.
% TRANSFER_FUNCTION: Transfers symbolic meaning, historical narrative, and a sense of collective identity from past generations to present and future community members, at the cost of individual flexibility in mourning practices and potential adaptive modification of the ritual.
% ABSENT_VOICES: Adaptive reformers and those who prioritize individual expression in grief are often marginalized; they would argue for more flexible or contemporary forms of remembrance, but their voices are suppressed by the emphasis on traditional fidelity.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community's shared memory of the catastrophe would fragment, collective identity would weaken, and the symbolic link to past generations would be lost. This would lead to a significant reorganization of group cohesion, potentially leading to dissolution or a radical shift in identity.
% FOUNDING_PROBLEM: The threat of collective identity dissolution, loss of shared memory, and fragmentation of cultural heritage following a catastrophic event that challenged the community's existence.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of the community, historical records of the catastrophe, and sociological analyses of group cohesion, often from outside the immediate community, corroborate the ongoing need for identity maintenance and the role of ritual in addressing this problem.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.15) because the primary 'cost' is the rigidity and lack of adaptive modification, which is inherent to the goal of continuity, rather than direct material extraction. Suppression is moderate (0.4) as deviation from ritual is met with social pressure and exclusion, but not typically coercive enforcement. Theater ratio is moderate (0.5) because while the ritual has genuine functional aspects in identity formation, its performative and symbolic elements are central to its operation. Accessibility collapse is moderate (0.4) as alternative forms of identity formation exist, but the ritual offers a uniquely potent and historically grounded path. Resistance is low (0.2) because most participants value the continuity, and dissenters are often marginalized.
 *
 * PERSPECTIVAL GAP:
 *   Ritual leaders and most community members perceive the ritual as an essential, low-cost mechanism for identity preservation. Individual mourners and adaptive reformers, however, experience the rigidity and suppression of alternative expressions as a significant cost, even if they value the overall goal of continuity. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members are the primary beneficiaries, gaining collective identity and continuity. Ritual leaders also benefit from their role in maintaining this continuity. Individual mourners and adaptive reformers bear the costs of conformity and suppressed innovation, respectively, placing them as targets. External observers are analytical seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving collective identity and symbolic continuity after catastrophe) remains live. The low extractiveness and 'rope' classification suggest it is largely functional coordination, though the costs of rigidity are borne by specific groups. There is no evidence of the mandate having atrophied, but rather a contestation over the *means* of fulfilling it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rigidity_necessity_ambiguity,
    'Is the observed ritual rigidity truly necessary for preserving symbolic continuity, or is it an unnecessary cost imposed by tradition that could be adapted without loss of identity?',
    'Comparative studies of similar communities that have successfully adapted their rituals while maintaining identity, or longitudinal studies of this community if adaptive changes are introduced.',
    'If rigidity is found to be unnecessary, the effective extractiveness on individual mourners and adaptive reformers would be re-evaluated as higher, indicating a less efficient form of identity coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigidity_necessity_ambiguity, empirical, 'Whether ritual rigidity is a functional requirement or an avoidable cost for symbolic continuity.').

omega_variable(
    reading_interdependence_ambiguity,
    'To what extent does the ''symbol_continuity_reading'' depend on or implicitly incorporate elements of the ''boundary_maintenance_reading'' or ''trauma_encoding_reading'' for its full effect?',
    'Detailed textual analysis of ritual narratives and ethnographic observation of ritual practice to identify explicit or implicit references to group boundaries or trauma processing within the symbolic continuity framework.',
    'If strong interdependence is found, the classification of this constraint might need to be re-evaluated as part of a more complex, multi-functional ''tangled rope'' or ''snare'' if the other functions are found to be highly extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_interdependence_ambiguity, conceptual, 'Interdependence of this reading with other catastrophe memory kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.13).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.14).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.39).
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
% This constraint is one of four distinct readings of the 'catastrophe_memory_kernel', each focusing on a different function of ritual in response to collective trauma. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
