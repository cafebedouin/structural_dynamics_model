% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission: Symbol Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes a reading of catastrophe memory transmission
 *   where ritual's primary function is to preserve communal identity and
 *   mourning practices through the faithful transmission of symbolic forms.
 *   The constraint is claimed as a Tangled Rope, reflecting the dual function
 *   of identity maintenance (coordination) and the sacrifice of adaptive
 *   capacity (extraction). The metrics reflect a system that, while providing
 *   genuine communal good, increasingly demands conformity and suppresses
 *   deviation to maintain its symbolic integrity.
 *
 * KEY AGENTS:
 *   - communal_identity_continuity: Primary beneficiary (institutional/identity_locked) — benefits from constraint
 *   - ritual_practitioners: Agenda setter (organized/constrained) — enforces fidelity
 *   - community_members: Payer/Beneficiary (moderate/identity_locked) — participates, conforms, gains belonging
 *   - adaptive_capacity: Primary victim (powerless/trapped) — sacrificed for form
 *   - individual_autonomy: Secondary victim (powerless/identity_locked) — constrained by collective form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission: Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'ea674ff7-ed9a-45b5-9db0-6c798cdab769').
narrative_ontology:cs_kernel_codification('ea674ff7-ed9a-45b5-9db0-6c798cdab769', formalized).
narrative_ontology:cs_authority_grounding('ea674ff7-ed9a-45b5-9db0-6c798cdab769', lineage).
narrative_ontology:cs_interpretation_layer_present('ea674ff7-ed9a-45b5-9db0-6c798cdab769').
narrative_ontology:cs_reading_relation('ea674ff7-ed9a-45b5-9db0-6c798cdab769', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea674ff7-ed9a-45b5-9db0-6c798cdab769', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('ea674ff7-ed9a-45b5-9db0-6c798cdab769', foundational, symbolic_form_is_identity).
narrative_ontology:cs_axiom_status(symbolic_form_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('ea674ff7-ed9a-45b5-9db0-6c798cdab769', symbolic_form_is_identity, deontological).
narrative_ontology:cs_axiom('ea674ff7-ed9a-45b5-9db0-6c798cdab769', secondary, fidelity_ensures_survival).
narrative_ontology:cs_axiom_status(fidelity_ensures_survival, holdable).
narrative_ontology:cs_axiom_grounding('ea674ff7-ed9a-45b5-9db0-6c798cdab769', fidelity_ensures_survival, conventional).
narrative_ontology:cs_reference_frame('ea674ff7-ed9a-45b5-9db0-6c798cdab769', unbroken_symbolic_transmission).
narrative_ontology:cs_drift_state('ea674ff7-ed9a-45b5-9db0-6c798cdab769', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ea674ff7-ed9a-45b5-9db0-6c798cdab769', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, community_members).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract concept of the community's enduring identity, which is preserved and reinforced through the faithful transmission of symbolic ritual forms. It benefits from the constraint by maintaining its coherence and distinctiveness across generations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity, beneficiary,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).

% The individuals responsible for performing and transmitting the ritual. They enforce fidelity to symbolic forms, ensuring continuity. Their identity is often deeply intertwined with their role, making deviation difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_practitioners, agenda_setter,
    organized, biographical, constrained, local).

% Participate in the ritual, gaining a sense of belonging and shared meaning. They pay by conforming to prescribed forms, potentially sacrificing individual expression or adaptive responses to new challenges for the sake of communal cohesion. Their identity is often fused with the community's.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, community_members, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, community_members, beneficiary).

% The community's ability to respond flexibly and effectively to novel environmental or social challenges. It is sacrificed when strict adherence to symbolic form prevents necessary innovation or pragmatic adjustments to ritual practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity).

% The capacity for individual members to interpret, adapt, or deviate from ritual forms based on personal conviction or changing circumstances. It is constrained by the emphasis on collective symbolic fidelity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy).

% Academics and researchers who study the ritual's function and impact on the community, often from an outside perspective. They analyze the trade-offs between symbolic continuity and other communal goods.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and identity by providing a shared symbolic framework for understanding past catastrophes and maintaining communal cohesion through ritualized mourning and remembrance.
% TRANSFER_FUNCTION: Transfers a sense of shared identity and historical continuity from past generations to current and future members, in exchange for adherence to specific symbolic forms and practices.
% ABSENT_VOICES: Those who prioritize pragmatic adaptation or individual expression over strict symbolic fidelity are often marginalized or silenced, as their concerns threaten the perceived integrity of the ritual's form. Their voices would advocate for more flexible or evolving practices.
% DISAPPEARANCE_RATIONALE: If the ritual and its emphasis on symbolic continuity vanished, the community's shared identity and collective memory of catastrophe would fragment, leading to a loss of cohesion and a re-evaluation of its historical narrative. The social fabric would significantly alter.
% FOUNDING_PROBLEM: The problem of preserving communal identity and the memory of a catastrophic event across generations, ensuring that the trauma and its lessons are not forgotten, and that the community remains unified in its remembrance.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and historians attest to the ongoing need for this ritual to prevent the erosion of identity and memory. External anthropologists corroborate the ritual's function in maintaining social cohesion and historical awareness, even while noting its costs.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the emphasis on symbolic fidelity demands a significant cost in terms of adaptive flexibility and individual expression. Suppression (0.7) is also high, as deviation from prescribed forms is actively discouraged to maintain the ritual's perceived authenticity and communal function. The theater ratio (0.4) indicates that while the ritual serves a genuine purpose, a substantial portion of its maintenance is performative, reinforcing identity through form rather than directly addressing current challenges. The increasing trend in extractiveness and suppression over time reflects a hardening of the constraint as the community prioritizes symbolic purity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'communal_identity_continuity' and 'ritual_practitioners', the constraint is a necessary 'Rope' for survival, ensuring the community's essence endures. From the perspective of 'adaptive_capacity' and 'individual_autonomy', it operates as a 'Snare', trapping the community in rigid forms that hinder its ability to evolve or for individuals to express themselves authentically. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   'Communal_identity_continuity' is the ultimate beneficiary, as the ritual directly serves its preservation. 'Ritual_practitioners' are agenda-setters, benefiting from their role in maintaining the tradition. 'Community_members' are both beneficiaries (belonging) and payers (conformity). 'Adaptive_capacity' and 'individual_autonomy' are victims, bearing the costs of rigidity and suppression. The 'identity_locked' exit option for community members and individual autonomy reflects the deep fusion of self with communal identity, making exit from the ritual's demands extremely difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'Rope' (as the community might claim) by highlighting the significant extraction of adaptive capacity and individual autonomy. It also avoids mislabeling it as a pure 'Snare' by acknowledging the genuine coordination function of identity and memory preservation. The 'Tangled Rope' classification captures the inherent trade-off and the active enforcement required to maintain this balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_operational_value,
    'To what extent is the symbolic fidelity of the ritual truly distinct from its operational utility in transmitting survival competence?',
    'Comparative ethnographic studies of communities with similar catastrophe narratives but different ritual transmission strategies, assessing long-term survival and adaptation outcomes.',
    'If symbolic fidelity is found to be largely separable from operational competence, the extraction of adaptive capacity is more pronounced, pushing the classification closer to a Snare. If they are deeply intertwined, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_operational_value, empirical, 'Distinguishing the intrinsic value of symbolic form from its instrumental value.').

omega_variable(
    identity_lock_strength,
    'How strong is the ''identity_locked'' exit option for community members and individual autonomy? Is it a genuine fusion of self with communal identity, or a more superficial social pressure?',
    'Longitudinal studies tracking individuals who attempt to deviate from ritual norms, observing social consequences, psychological impact, and eventual reintegration or exclusion.',
    'If the identity lock is weaker than assessed, the suppression metric might be overstated, and the effective extraction from individuals would be lower. If stronger, the current assessment is accurate or even understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Assessing the depth and enforceability of identity fusion with the ritual.').

omega_variable(
    mandatrophy_of_symbolic_form,
    'Has the specific symbolic form of the ritual outlived its original function, becoming an end in itself rather than a means to preserve identity?',
    'Historical analysis comparing the ritual''s original context and form with its contemporary practice, identifying points where the ''why'' of the ritual became secondary to the ''how''.',
    'If the symbolic form has mandatrophied, the ''theater_ratio'' would be higher, and the constraint would lean more towards a ''Piton'' or a ''Snare'' where the original coordination function has atrophied.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_symbolic_form, conceptual, 'Whether the symbolic form has become an inertial constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
