% ============================================================================
% CONSTRAINT STORY: israel_surplus_vote_agreements
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_israel_surplus_vote_agreements, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: israel_surplus_vote_agreements
 * human_readable: Surplus-Vote Agreements (Bader-Ofer Method)
 * domain: political
 * * SUMMARY:
 * A system in Israeli elections where two parties sign a pre-election agreement to be treated as a
 * single list for allocating "surplus" or leftover seats. While it prevents vote wastage, the
 * mathematical implementation (Bader-Ofer method) slightly favors larger lists, often resulting
 * in an "extraction" of electoral value from the smaller partner to the larger one within the alliance.
 * * KEY AGENTS:
 * - Small Partner Party: Subject (Powerless) - Often provides the votes that secure the extra seat but doesn't receive it.
 * - Large Partner Party: Beneficiary (Institutional) - Statistically more likely to receive the allocated leftover seat.
 * - Central Elections Committee: Auditor (Analytical) - Manages the complex calculations to ensure legal adherence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Moderate extraction: The Bader-Ofer method's bias towards larger lists means surplus
% votes from a smaller party often "graduate" to seat a candidate for the larger party.
domain_priors:base_extractiveness(israel_surplus_vote_agreements, 0.47).
% Moderate suppression: Parties are not forced to sign these, but the "waste"
% alternative (losing surplus votes entirely) makes it a semi-coercive strategic necessity.
domain_priors:suppression_score(israel_surplus_vote_agreements, 0.42).
% Low theater: The system is a highly functional mathematical logic with little performative aspect.
domain_priors:theater_ratio(israel_surplus_vote_agreements, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, extractiveness, 0.47).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(israel_surplus_vote_agreements, theater_ratio, 0.10).

% Constraint self-claim: The system is publicly framed as a tool for efficient coordination.
narrative_ontology:constraint_claim(israel_surplus_vote_agreements, tangled_rope).
narrative_ontology:human_readable(israel_surplus_vote_agreements, "Surplus-Vote Agreements (Bader-Ofer Method)").
narrative_ontology:topic_domain(israel_surplus_vote_agreements, "political").

% Binary flags
domain_priors:requires_active_enforcement(israel_surplus_vote_agreements). % The Central Elections Committee must enforce the calculations.

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(israel_surplus_vote_agreements, large_partner_party).
narrative_ontology:constraint_victim(israel_surplus_vote_agreements, small_partner_party).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a smaller party that provides 0.9 seats' worth of surplus votes only for the
% larger partner to "capture" the resulting seat, this feels like a predatory Snare.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the larger party and the overall bloc, it is a Rope (coordination)
% that ensures no votes are "wasted" in the fight for a governing majority.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: it coordinates blocs to maximize seat allocation but
% asymmetrically extracts influence from small parties to empower large ones.
constraint_indexing:constraint_classification(israel_surplus_vote_agreements, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_surplus_vote_agreements_tests).

test(perspectival_gap) :-
    % Verify it acts as a coordination tool for institutions but a snare for the small partner.
    constraint_indexing:constraint_classification(israel_surplus_vote_agreements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_surplus_vote_agreements, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    % Verify the base extractiveness is in the high-extraction range for Snare/Tangled Rope.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(israel_surplus_vote_agreements, ExtMetricName, E),
    E >= 0.46.

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(israel_surplus_vote_agreements),
    narrative_ontology:constraint_beneficiary(israel_surplus_vote_agreements, _),
    narrative_ontology:constraint_victim(israel_surplus_vote_agreements, _).

:- end_tests(israel_surplus_vote_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Bader-Ofer method is a canonical Tangled Rope. Its coordination function (bloc consolidation)
 * is inextricably linked to its extractive function (favoring larger lists). The system presents
 * a choice between guaranteed vote wastage (running alone) and probable value extraction (signing an
 * agreement). This creates a perspectival gap: the larger party sees it as pure coordination (Rope),
 * while the smaller party experiences it as a necessary but potentially costly trap (Snare). The
 * analytical view, which sees both functions operating simultaneously, correctly identifies it as
 * a Tangled Rope.
 * * MANDATROPHY ANALYSIS:
 * By classifying this as a Tangled Rope, the system avoids mislabeling it as a pure Snare.
 * While it extracts value, it also provides a genuine coordination benefit by preventing the
 * total loss of surplus votes, which would be a worse outcome for all parties in the bloc.
 * This acknowledges the hybrid nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_bader_ofer_bias,
    'At what specific seat-count delta does the "benefit" of the alliance for a small party turn into a net loss of representational power?',
    'Stochastic modeling of surplus seat distribution across multiple historical and simulated election cycles.',
    'Determines if the constraint is a Scaffold (helping small parties survive) or a Snare (cannibalizing them).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(israel_surplus_vote_agreements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The Bader-Ofer method is a stable, structural feature of the electoral law.
% Its extractive properties have not significantly changed over time. Therefore,
% we model a flat trajectory for both extraction and theater.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(isva_tr_t0, israel_surplus_vote_agreements, theater_ratio, 0, 0.10).
narrative_ontology:measurement(isva_tr_t5, israel_surplus_vote_agreements, theater_ratio, 5, 0.10).
narrative_ontology:measurement(isva_tr_t10, israel_surplus_vote_agreements, theater_ratio, 10, 0.10).

% Extraction over time (stable and moderate):
narrative_ontology:measurement(isva_ex_t0, israel_surplus_vote_agreements, base_extractiveness, 0, 0.47).
narrative_ontology:measurement(isva_ex_t5, israel_surplus_vote_agreements, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(isva_ex_t10, israel_surplus_vote_agreements, base_extractiveness, 10, 0.47).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The system is a formal mechanism for allocating a scarce resource (parliamentary seats).
narrative_ontology:coordination_type(israel_surplus_vote_agreements, resource_allocation).

% No network relationships or floor overrides are defined for this constraint.
% narrative_ontology:boltzmann_floor_override(israel_surplus_vote_agreements, 0.0).
% narrative_ontology:affects_constraint(israel_surplus_vote_agreements, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */