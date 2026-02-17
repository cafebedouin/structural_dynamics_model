% ============================================================================
% CONSTRAINT STORY: portugal_ad_stability_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_portugal_ad_stability_2026, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: portugal_ad_stability_2026
 * human_readable: The AD Minority Government Stability (The "Presidential" Scaffold)
 * domain: political
 * * SUMMARY:
 * The stability of the Aliança Democrática (AD) minority government functions
 * as a Scaffold. It relies on the mediating influence of the outgoing President,
 * Marcelo Rebelo de Sousa, and a temporary truce between the PS and PSD.
 * This constraint has an implicit sunset clause: the January 2026 Presidential
 * election. The outcome of the election—specifically whether the new President
 * is a "stabilizer" or a "disruptor"—will determine if this scaffold is
 * replaced by a more permanent coordination (Rope) or collapses into a Snare.
 * * KEY AGENTS:
 * - Luís Montenegro (AD Government): Architect (Organized) - Managing the fragile minority balance.
 * - Political Establishment: Beneficiary (Institutional) - Benefits from avoiding a constitutional crisis.
 * - Opposition Voters/Civil Service: Subject (Powerless) - Dependent on budget stability, but unable to challenge policies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is moderate; the government coordinates but requires suppression of dissent.
domain_priors:base_extractiveness(portugal_ad_stability_2026, 0.35).
% Suppression: Significant; parties are discouraged from "rocking the boat" before the election.
domain_priors:suppression_score(portugal_ad_stability_2026, 0.45).
% Theater: Moderate; constant negotiations are partially performative for the electorate.
domain_priors:theater_ratio(portugal_ad_stability_2026, 0.30).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(portugal_ad_stability_2026, extractiveness, 0.35).
narrative_ontology:constraint_metric(portugal_ad_stability_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(portugal_ad_stability_2026, theater_ratio, 0.30).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(portugal_ad_stability_2026, scaffold).
narrative_ontology:human_readable(portugal_ad_stability_2026, "The AD Minority Government Stability (The \"Presidential\" Scaffold)").

% Binary flags
domain_priors:requires_active_enforcement(portugal_ad_stability_2026).
narrative_ontology:has_sunset_clause(portugal_ad_stability_2026). % Mandatory for Scaffold

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(portugal_ad_stability_2026, political_establishment).
narrative_ontology:constraint_victim(portugal_ad_stability_2026, opposition_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE ARCHITECT (SCAFFOLD)
% Tolerated high suppression of political rivalry only because it expires with the new mandate.
constraint_indexing:constraint_classification(portugal_ad_stability_2026, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(portugal_ad_stability_2026).

% PERSPECTIVE 2: THE INSTITUTIONAL LOYALIST (ROPE)
% Viewed as necessary coordination to avoid a mid-cycle constitutional crisis.
constraint_indexing:constraint_classification(portugal_ad_stability_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE POLICY CRITIC (SNARE)
% Viewed as a trap where the need for "stability" is used to extract concessions and suppress dissent.
constraint_indexing:constraint_classification(portugal_ad_stability_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portugal_ad_stability_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(portugal_ad_stability_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portugal_ad_stability_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(scaffold_validation) :-
    % Verify that the scaffold is correctly identified with its sunset clause.
    constraint_indexing:constraint_classification(portugal_ad_stability_2026, scaffold, context(agent_power(organized), _, _, _)),
    narrative_ontology:has_sunset_clause(portugal_ad_stability_2026).

:- end_tests(portugal_ad_stability_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The AD government is classified as a Scaffold because its survival is
 * conditional on the current presidential term. The sunset clause is the
 * January 2026 presidential election date. Once the new President is sworn
 * in, the "coordination" provided by Marcelo's mediation expires.
 * The base extractiveness (0.35) and suppression (0.45) are significant but
 * below the thresholds for a pure Snare, reflecting the temporary and negotiated
 * nature of the arrangement. The presence of a beneficiary (political establishment)
 * confirms its coordination function, a requirement for Scaffold classification.
 *
 * PERSPECTIVAL GAP:
 * The government (Architect, Organized) sees a necessary Scaffold to pass budgets.
 * The institutional establishment sees a Rope, a necessary tool for national stability.
 * Opposition voters (Subject, Powerless) see a Snare that limits their ability to
 * challenge austerity or specific AD policies under the guise of "national interest".
 *
 * MANDATROPHY ANALYSIS:
 * The Scaffold classification is critical here. It prevents the system from
 * mislabeling this temporary, high-suppression coordination as either a permanent
 * Snare (ignoring its coordination function) or a permanent Tangled Rope (ignoring
 * its explicit expiry date). The `has_sunset_clause/1` fact is the key differentiator
 * that correctly identifies this as a transitional state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_presidential_dissolution,
    'Will the winner of the 2026 election dismantle the AD stability scaffold by calling early legislative elections?',
    'Observation of the new President\'s first speech to the Council of State.',
    'Collapse (Scaffold removed) vs Solidification (Scaffold becomes Rope)',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(portugal_ad_stability_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required
% for lifecycle drift detection under current system rules.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This political arrangement acts as a mechanism to enforce stability and
% suppress challenges, making it an enforcement mechanism.
narrative_ontology:coordination_type(portugal_ad_stability_2026, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (portugal_government_stability_ad)
% ============================================================================

% --- Analytical perspective classification ---
% chi = 0.35 * 1.15 (analytical) * 1.2 (global) = 0.483
% Classification: tangled_rope
constraint_indexing:constraint_classification(portugal_ad_stability_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
