% ============================================================================
% CONSTRAINT STORY: institutional_memory_loss
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_institutional_memory_loss, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: institutional_memory_loss
 * human_readable: The Amnesiac Organization
 * domain: organizational/political/technological
 * * SUMMARY:
 * A scenario where rapid personnel turnover, over-reliance on ephemeral digital
 * communications, and the retirement of "tacit knowledge" holders cause an
 * institution to lose the "why" behind its own internal constraints. This
 * "Rope" for maintaining administrative consistency becomes a "Snare" for
 * new members, who are forced to follow obsolete or dangerous protocols
 * they can no longer explain, liquidating the organization's adaptive agency.
 * * KEY AGENTS:
 * - New Employee: Subject (Powerless)
 * - Legacy Administrator: Beneficiary (Institutional)
 * - Knowledge Management Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) reflects the siphoning of subject agency into the
% maintenance of "ghost protocols" that no longer serve a functional purpose.
domain_priors:base_extractiveness(institutional_memory_loss, 0.84).
domain_priors:suppression_score(institutional_memory_loss, 0.73).   % High suppression: questioning the "why" is seen as a breach of protocol.
domain_priors:theater_ratio(institutional_memory_loss, 0.91).       % Extreme theater: meticulous "onboarding" rituals masking a lack of substance.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(institutional_memory_loss, extractiveness, 0.84).
narrative_ontology:constraint_metric(institutional_memory_loss, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(institutional_memory_loss, theater_ratio, 0.91).

% Constraint self-claim (what does the constraint claim to be?)
% The procedures are framed as necessary for coordination and order.
narrative_ontology:constraint_claim(institutional_memory_loss, piton).

% Binary flags
domain_priors:requires_active_enforcement(institutional_memory_loss). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(institutional_memory_loss, legacy_administrators).
narrative_ontology:constraint_victim(institutional_memory_loss, new_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The new hire is trapped: they must obey rules they don't understand to
% survive the bureaucracy, while the actual rationale for those rules is lost.
constraint_indexing:constraint_classification(institutional_memory_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The administrator views the memory loss as a Rope—it ensures absolute
% procedural compliance and coordination without the "friction" of debate.
constraint_indexing:constraint_classification(institutional_memory_loss, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Standard Operating Procedure"
% is an inert spike; it remains in place solely through institutional inertia.
constraint_indexing:constraint_classification(institutional_memory_loss, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and suppression (0.73) as a hybrid signature,
% recognizing the coercive enforcement of a system that still has beneficiaries.
constraint_indexing:constraint_classification(institutional_memory_loss, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_memory_loss_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(institutional_memory_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_memory_loss, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify that the high extraction value is consistent with a Snare/Tangled Rope classification.
    narrative_ontology:constraint_metric(institutional_memory_loss, extractiveness, E),
    E >= 0.46.

test(piton_trigger) :-
    % Ensure high theater ratio (0.91) correctly triggers the Piton classification.
    domain_priors:theater_ratio(institutional_memory_loss, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(institutional_memory_loss, piton,
        context(agent_power(analytical), _, _, _)).

:- end_tests(institutional_memory_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the
 * "coordination" benefit of institutional consistency is achieved by
 * liquidating the subject's primary reasoning agency. The high theater (0.91)
 * and high suppression (0.73) create a complex situation where the constraint
 * can be simultaneously viewed as a Piton (inert, theatrical) and a
 * Tangled Rope (actively enforced with asymmetric extraction).
 *
 * * PERSPECTIVAL GAP:
 * The New Employee feels a Snare because they are running a machine they
 * don't have the manual for. The Administrator sees a Rope because the
 * preservation of form coordinates the stability of the hierarchy.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical
 * observer, the "Knowledge Base" is no longer functional (Theater 0.91);
 * it is an inert spike siphoning 0.84 of the subject's cognitive surplus.
 * The Tangled Rope classification correctly identifies that this inert system
 * is still actively enforced, benefiting one group at the expense of another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_tacit_recovery,
    'Can tacit knowledge be digitized, or is its loss a Mountain of biology (Snare vs Mountain)?',
    'Tracking the failure rate of "AI-reconstructed" protocols in high-risk environments.',
    'If AI succeeds: Snare of current storage tech. If AI fails: Mountain of Human Experience.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(institutional_memory_loss, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a system degrading over time. Initially, it was a
% functional coordination mechanism (low extraction, low theater). Over the
% interval, knowledge was lost, theater increased, and extraction accumulated
% as procedures became detached from their purpose.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(institutional_memory_loss_tr_t0, institutional_memory_loss, theater_ratio, 0, 0.10).
narrative_ontology:measurement(institutional_memory_loss_tr_t5, institutional_memory_loss, theater_ratio, 5, 0.55).
narrative_ontology:measurement(institutional_memory_loss_tr_t10, institutional_memory_loss, theater_ratio, 10, 0.91).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(institutional_memory_loss_ex_t0, institutional_memory_loss, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(institutional_memory_loss_ex_t5, institutional_memory_loss, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(institutional_memory_loss_ex_t10, institutional_memory_loss, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's original function was coordination via procedural rules.
narrative_ontology:coordination_type(institutional_memory_loss, enforcement_mechanism).

% narrative_ontology:affects_constraint(institutional_memory_loss, [other_constraint_id]).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */