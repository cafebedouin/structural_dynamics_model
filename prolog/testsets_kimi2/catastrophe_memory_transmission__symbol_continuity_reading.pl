% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission: Symbol Continuity Reading
 *   domain: religious/collective_memory/ritual
 *
 * SUMMARY:
 *   This constraint is the symbol_continuity_reading of the
 *   catastrophe_memory_transmission kernel. It instantiates the claim that
 *   ritual preserves identity and mourning-practice as intrinsic communal
 *   goods, with transmission of symbolic form as the survival mechanism
 *   itself. Sibling readings include operational_competence_reading (ritual
 *   encodes survival competence through pattern recognition) and
 *   hybrid_embedded_reading (competence and symbolic form are inseparable).
 *   This reading is distinguished by its willingness to sacrifice operational
 *   adaptation to preserve symbolic fidelity, generating a distinct
 *   beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - communal_identity_collective: beneficiary (organized/identity_locked/regional) â receives continuity of identity through ritual fidelity
 *   - practicing_members: payer (moderate/identity_locked/local) â bear costs of forgone adaptive capacity and suppressed deviation
 *   - ritual_guardians: agenda_setter (organized/constrained/regional) â enforce form and adjudicate legitimacy
 *   - reformist_members: excluded (moderate/constrained/local) â advocate adaptation but are structurally silenced
 *   - anthropological_observers: observer (analytical/analytical/global) â study the system without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.7).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.75).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission: Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious/collective_memory/ritual").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'b70b4192-c82e-457d-b75a-f05674ec8b01').
narrative_ontology:cs_kernel_codification('b70b4192-c82e-457d-b75a-f05674ec8b01', fixed_text).
narrative_ontology:cs_authority_grounding('b70b4192-c82e-457d-b75a-f05674ec8b01', lineage).
narrative_ontology:cs_interpretation_layer_present('b70b4192-c82e-457d-b75a-f05674ec8b01').
narrative_ontology:cs_reading_relation('b70b4192-c82e-457d-b75a-f05674ec8b01', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b70b4192-c82e-457d-b75a-f05674ec8b01', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('b70b4192-c82e-457d-b75a-f05674ec8b01', foundational, symbolic_form_as_intrinsic_survival_mechanism).
narrative_ontology:cs_axiom_status(symbolic_form_as_intrinsic_survival_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('b70b4192-c82e-457d-b75a-f05674ec8b01', symbolic_form_as_intrinsic_survival_mechanism, deontological).
narrative_ontology:cs_axiom('b70b4192-c82e-457d-b75a-f05674ec8b01', secondary, ritual_fidelity_over_operational_response).
narrative_ontology:cs_axiom_status(ritual_fidelity_over_operational_response, holdable).
narrative_ontology:cs_axiom_grounding('b70b4192-c82e-457d-b75a-f05674ec8b01', ritual_fidelity_over_operational_response, conventional).
narrative_ontology:cs_reference_frame('b70b4192-c82e-457d-b75a-f05674ec8b01', symbolic_continuity_as_collective_identity).
narrative_ontology:cs_drift_state('b70b4192-c82e-457d-b75a-f05674ec8b01', contemporary_adaptive_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b70b4192-c82e-457d-b75a-f05674ec8b01', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_collective).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, practicing_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives continuity of collective identity, mourning-practice coherence, and intergenerational recognition through the rigid transmission of symbolic ritual form. Its survival-as-identity is purchased by the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_collective, beneficiary,
    organized, generational, identity_locked, regional).

% Perform the ritual under fidelity demands, bearing the opportunity costs of forgone adaptive strategies, suppressed emotional responses to catastrophe, and constrained individual agency. Exit entails exile from communal identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, practicing_members, payer,
    moderate, biographical, identity_locked, local).

% Transmit, correct, and enforce ritual form across generations. Their authority derives from successful preservation of symbolic continuity; they adjudicate deviations and maintain the boundary between legitimate and illegitimate practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_guardians, agenda_setter,
    organized, generational, constrained, regional).

% Advocate for operational adaptation of ritual to changing environmental or catastrophe conditions. Their voices are structurally excluded from legitimacy because adaptation threatens the symbolic-fidelity criterion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, reformist_members, excluded,
    moderate, biographical, constrained, local).

% Document and analyze the ritual system as a memory-transmission technology, observing the tension between symbolic fidelity and operational responsiveness without institutional stake in either outcome.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, anthropological_observers, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves communal identity and mourning-practice across catastrophic rupture by providing a non-propositional, embodied mechanism for intergenerational continuity.
% TRANSFER_FUNCTION: Moves symbolic form and collective identity from ritual guardians and past generations to current practicing members; extracts adaptive flexibility and individualized grief-response from practicing members to maintain formal purity.
% ABSENT_VOICES: Reformist members and operationally oriented practitioners who would adapt ritual to environmental demands are excluded from legitimacy; their exclusion is enforced by guardian authority and identity-lock.
% DISAPPEARANCE_RATIONALE: If ritual transmission vanished, the communal identity structure would fragment; mourning practices would diverge, and the group's self-understanding as a continuous entity surviving catastrophe would dissolve.
% FOUNDING_PROBLEM: Catastrophic rupture threatened communal memory dissolution and identity loss; ritual was constructed to encode and transmit identity across breaks in ordinary social life.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological observers attest to the historical problem of memory-loss across catastrophe. Ritual guardians claim the problem remains live. Reformist members attest the problem has shifted to environmental mismatch and operational maladaptation rather than memory loss, corroborating a changed founding condition from outside the beneficiary set.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.7, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.70) reflects the substantial cost of forgone adaptive capacity that practicing members pay to maintain symbolic form. Suppression (0.75) captures the active enforcement of ritual fidelity by guardians and the identity-locked exit options that prevent practitioners from deviating. Theater ratio (0.52) acknowledges that a significant portion of ritual activity is performative maintenance of group boundary rather than direct operational function. Accessibility collapse (0.80) is high because alternative ritual forms or non-ritual memory strategies are rendered unthinkable by identity fusion. Resistance (0.35) is moderate: some reformist members push against fidelity demands, but identity-lock and guardian authority mute organized resistance. The metrics and claimed type are authored independently: the constraint is claimed as tangled_rope because genuine coordination (identity continuity) and asymmetric extraction (adaptive capacity loss) are structurally co-present and actively enforced.
 *
 * PERSPECTIVAL GAP:
 *   The ritual_guardian and communal_identity_collective seats experience the constraint as necessary coordination without which the group dissolves; the practicing_member and reformist_member seats experience it as rigid extraction of their adaptive and expressive capacities. The engine computes this divergence from structural data: agenda_setter/beneficiary seats with identity-locked but authority-protected exit versus payer seats with identity-locked and socially policed exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual guardians and the communal identity collective sit near the beneficiary end (low d): they are subsidized by the constraint's preservation of their authority and existence. Practicing members sit near the target end (high d): they pay through constrained adaptability and suppressed deviation. Reformist members are excluded from the conversation entirely, their exclusion constituting part of the suppression structure. The directionality is amplified by the identity-locked exit option, which traps targets in the high-d position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists classification as pure rope because the victim structure is real: practicing members demonstrably lose adaptive capacity. It resists classification as pure snare because the coordination function is not cover but genuine: without symbolic transmission, communal identity across catastrophe is observably fragile. The tangled_rope classification captures the hybrid: the same structure that coordinates identity continuity simultaneously extracts adaptive capacity, and requires active enforcement (guardian correction, social sanction) to hold the hybrid in place. A snare reading would need to show that identity continuity is merely a legitimating story; the authored metrics and structural declarations treat identity continuity as a real output, making tangled_rope the structurally honest claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_operational_separability,
    'Can symbolic continuity be maintained independently of operational adaptation, or is the claim of their separability a post-hoc justification for ritual rigidity?',
    'Comparative ethnographic analysis of communities facing identical catastrophic pressures: if symbolically rigid but operationally maladaptive communities persist while adaptive but symbolically altered communities dissolve, separability is supported; if both dissolve, the axiom is challenged.',
    'If inseparable, this reading collapses toward the hybrid_embedded_reading; if separable, the tangled_rope classification holds with distinct victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_operational_separability, empirical, 'Whether symbolic form and operational competence are structurally separable in ritual transmission.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the enforcement of ritual fidelity accomplished through external social sanction or through internalized identity fusion?',
    'Post-exit trajectory observation: if practitioners who leave the community continue to enforce fidelity on themselves or experience distress independent of social contact, suppression is partially internalized.',
    'If internalized, effective extraction exceeds structural measures; the constraint operates as cognitive capture, strengthening the tangled_rope''s extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in ritual fidelity.').

omega_variable(
    communal_identity_reification,
    'Does the communal_identity_collective exist as a genuine agent capable of receiving benefit, or is it a reified abstraction that obscures distribution of costs and benefits among specific members?',
    'Game-theoretic or network analysis of benefit distribution within the community: if specific subgroups capture the prestige and security of continuity while others bear the costs, the collective beneficiary designation masks an internal snare.',
    'If reified, the beneficiary/victim structure may need decomposition into intra-communal power asymmetries, potentially shifting classification toward snare for the cost-bearing subgroup.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_identity_reification, conceptual, 'Whether the communal identity beneficiary is a reified abstraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.46).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is the symbol-continuity reading of the catastrophe_memory_transmission kernel. It stands in a family with operational_competence_reading and hybrid_embedded_reading, which assign different structural properties (epsilon, beneficiary/victim sets, coordination vs extraction balance) to the same ritual phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
