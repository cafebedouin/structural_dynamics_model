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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual Preserving Symbolic Continuity and Collective Identity
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in preserving symbolic
 *   continuity and collective identity, particularly in the context of
 *   catastrophic memory. It is one reading of the
 *   'catastrophe_memory_kernel', focusing on the transmission of symbols
 *   rather than adaptive survival or trauma encoding. The constraint is
 *   claimed as a Rope, reflecting its genuine coordination function in
 *   maintaining social cohesion, with relatively low extractiveness and
 *   suppression, but with a cost in adaptive flexibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual Preserving Symbolic Continuity and Collective Identity").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '3485a883-44fd-4fe9-9f5b-694eb07ef4f9').
narrative_ontology:cs_kernel_codification('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', implicit).
narrative_ontology:cs_authority_grounding('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', practice).
narrative_ontology:cs_interpretation_layer_present('3485a883-44fd-4fe9-9f5b-694eb07ef4f9').
narrative_ontology:cs_reading_relation('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', foundational, symbolic_transmission_is_identity).
narrative_ontology:cs_axiom_status(symbolic_transmission_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', symbolic_transmission_is_identity, deontological).
narrative_ontology:cs_reference_frame('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', unbroken_symbolic_lineage).
narrative_ontology:cs_drift_state('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', contemporary_globalized_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3485a883-44fd-4fe9-9f5b-694eb07ef4f9', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, cultural_tradition).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that reinforce their shared history and identity, finding meaning and belonging. Their identity is deeply intertwined with the continuity of these symbolic practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, community_members, beneficiary,
    organized, generational, identity_locked, local).

% The abstract entity of the tradition itself, which benefits from its own perpetuation and transmission across generations. Its 'survival' is the goal of the ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, cultural_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, cultural_tradition).

% The inherent pressure for rituals to evolve or adapt to changing social contexts, which is resisted by the emphasis on strict symbolic continuity. This 'cost' is the rigidity and potential anachronism of the ritual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_pressure, payer,
    analytical, biographical, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_pressure).

% Administer and interpret the rituals, ensuring their faithful transmission. They are custodians of the symbolic continuity and enforce adherence to established forms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_leaders, agenda_setter,
    institutional, biographical, constrained, local).

% Study the ritual practices from an academic or anthropological perspective, analyzing their function in preserving identity and memory without direct participation or benefit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, external_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and identity by providing shared symbolic frameworks and practices that link past, present, and future generations within a community.
% TRANSFER_FUNCTION: Transfers symbolic meaning, historical narratives, and a sense of belonging from the past to current and future community members, reinforcing collective identity.
% ABSENT_VOICES: Those who advocate for radical ritual innovation or adaptation to modern contexts are often marginalized, as their proposals threaten the perceived symbolic continuity. Their voices are suppressed by the emphasis on tradition.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community's collective identity would fragment, historical memory would become diffuse, and the sense of intergenerational continuity would erode, leading to a significant reorganization of social cohesion.
% FOUNDING_PROBLEM: The threat of collective amnesia or identity dissolution following a catastrophic event, where shared symbols and practices were needed to bind the community across time.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and historians attest to the ongoing need for symbolic continuity to maintain identity, citing historical precedents of cultural loss. External sociological studies corroborate the role of ritual in maintaining group cohesion and memory.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.25) because the primary 'cost' is the rigidity imposed on adaptive modification, not a direct material transfer. Suppression (0.4) is moderate, reflecting the social pressure to conform to ritual norms for the sake of group identity, but not overt coercion. Theater ratio is low (0.1) as the ritual's symbolic function is largely genuine. Accessibility collapse (0.6) indicates that while alternatives to ritual exist for memory, they don't fully replace its identity-binding function. Resistance (0.15) is low, as most participants value the continuity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a vital source of identity and belonging. From an analytical perspective, the cost of maintaining symbolic continuity might be seen as hindering adaptive responses to new challenges. The engine's classification will reflect this balance of coordination and subtle cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and the abstract 'cultural_tradition' are beneficiaries, gaining identity and perpetuation. The 'adaptive_modification_pressure' is a conceptual victim, representing the cost of rigidity. Ritual leaders act as agenda-setters, ensuring adherence. This reading emphasizes the positive coordination function of ritual for identity, with the 'cost' being the sacrifice of flexibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preserving identity) remains live, as attested by community members and corroborated by external analysis. There is no significant mandatrophy, as the ritual continues to serve its core function, even if it imposes a cost on adaptive change. The low theater ratio supports this, indicating genuine function over mere performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_adaptive_function,
    'Is the primary function of this ritual symbolic transmission, or does it also encode adaptive survival knowledge?',
    'Ethnographic study of ritual content and participant interpretations, cross-referenced with historical outcomes of communities facing similar catastrophes.',
    'If a significant adaptive function is found, the extractiveness (cost of rigidity) might be re-evaluated against the survival benefit, potentially shifting the classification towards a more complex Tangled Rope or even a Rope with higher utility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_adaptive_function, empirical, 'Distinguishing the symbolic function from potential practical survival encoding.').

omega_variable(
    identity_lock_strength,
    'How strong is the ''identity_locked'' exit option for community members? Is it a genuine internal bond or reinforced by external social pressure?',
    'Longitudinal studies of individuals who attempt to disengage from the ritual practices, observing social consequences and personal identity shifts.',
    'If external pressure is dominant, the suppression metric might be higher than currently assessed, and the ''identity_locked'' status might lean more towards ''constrained'' or ''trapped'' for some individuals, impacting their directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Assessing the nature and strength of identity-based commitment to the ritual.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint best framed as primarily about symbolic continuity, or is its core function better captured by one of the sibling readings (survival competence, trauma encoding, boundary maintenance)?',
    'Comparative analysis of the explanatory power of each reading for observed ritual persistence and community outcomes, guided by the specific research question.',
    'Adopting a different framing would instantiate a different constraint with potentially different extractiveness, suppression, and claimed type, leading to a distinct classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'The choice of primary interpretive frame for the catastrophe memory kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.09).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.23).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 75, 0.24).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 75, 0.39).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_kernel', focusing on symbolic continuity. It coexists with and influences other readings that emphasize adaptive survival, trauma encoding, or boundary maintenance, as these functions are often intertwined in ritual practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
