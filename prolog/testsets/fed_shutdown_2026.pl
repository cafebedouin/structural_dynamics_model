% ============================================================================
% CONSTRAINT STORY: fed_shutdown_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_federal_shutdown, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================= */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: fed_shutdown_2026
 * human_readable: The 2026 DHS/ICE Funding Standoff
 * domain: political/economic
 * * SUMMARY:
 * A federal government shutdown triggered by a standoff over ICE oversight 
 * following the deaths of two U.S. citizens. The constraint functions as a 
 * Tangled Rope: it coordinates ideological boundaries for legislative blocs 
 * but extracts significant systemic value by delaying critical economic 
 * indicators like the January jobs report.
 * * KEY AGENTS:
 * - Investors/Federal Workers: Subject (Powerless) - Blinded and unpaid.
 * - Legislative Blocs: Beneficiary (Institutional) - Using friction as leverage.
 * - Market Analysts: Auditor (Analytical) - Navigating the information void.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(fed_shutdown_2026, 0.58).
domain_priors:suppression_score(fed_shutdown_2026, 0.62).
domain_priors:theater_ratio(fed_shutdown_2026, 0.85).

% Primary keys for classification engine
% These must match the parameter names in the system config.
narrative_ontology:constraint_metric(fed_shutdown_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(fed_shutdown_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fed_shutdown_2026, theater_ratio, 0.85).

% Constraint classification claim
narrative_ontology:constraint_claim(fed_shutdown_2026, tangled_rope).
narrative_ontology:human_readable(fed_shutdown_2026, "The 2026 DHS/ICE Funding Standoff").
narrative_ontology:topic_domain(fed_shutdown_2026, "political/economic").

% Tangled Rope Structural Property Declarations
% Required by the linter and the canonical classifier to derive 
% has_coordination_function/1 and has_asymmetric_extraction/1.
domain_priors:requires_active_enforcement(fed_shutdown_2026).
narrative_ontology:constraint_beneficiary(fed_shutdown_2026, legislative_blocs).
narrative_ontology:constraint_victim(fed_shutdown_2026, federal_workers_and_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% Effective Extraction: 0.58 * 1.5 (powerless) * 1.0 (national) = 0.87.
% At χ > 0.46, the subject feels this as a predatory trap (Snare).
constraint_indexing:constraint_classification(fed_shutdown_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (TANGLED ROPE)
% Effective Extraction: 0.58 * -0.2 (institutional) * 1.0 = -0.116.
% To the institution, the friction is a necessary coordination tool.
constraint_indexing:constraint_classification(fed_shutdown_2026, tangled_rope, 
    context(agent_power(institutional), 
            time_horizon(biographical), 
            exit_options(constrained), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context for system-wide claims.
constraint_indexing:constraint_classification(fed_shutdown_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE FEDERAL CONTRACTOR (TANGLED ROPE)
% Effective Extraction: 0.58 * 1.0 (moderate) * 1.0 (national) = 0.58.
% Exit Option: arbitrage (pivot to non-government programs).
% With arbitrage, the classification remains 'tangled_rope' rather than 
% 'snare' because the agent maintains a functional link to the system.
constraint_indexing:constraint_classification(fed_shutdown_2026, tangled_rope, 
    context(agent_power(moderate), 
            time_horizon(biographical), 
            exit_options(arbitrage), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fed_shutdown_2026_tests).

test(perspectival_gap) :-
    % Verify that the powerless feel a Snare while the institution sees a Rope.
    constraint_indexing:constraint_classification(fed_shutdown_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fed_shutdown_2026, tangled_rope, context(agent_power(institutional), _, _, _)).

test(tangled_rope_properties) :-
    % Ensure all structural requirements for Tangled Rope are present.
    domain_priors:requires_active_enforcement(fed_shutdown_2026),
    narrative_ontology:constraint_beneficiary(fed_shutdown_2026, _),
    narrative_ontology:constraint_victim(fed_shutdown_2026, _).

test(contractor_mitigation) :-
    % Verify that contractors experience lower effective extraction than the public
    % due to power scaling, even if the base extraction is identical.
    constraint_indexing:constraint_classification(fed_shutdown_2026, TypePublic, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fed_shutdown_2026, TypeContractor, context(agent_power(moderate), _, exit_options(arbitrage), _)),
    TypePublic = snare,
    TypeContractor = tangled_rope.

:- end_tests(fed_shutdown_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.58 base extraction is high enough to trigger the Omega Variable 
 * requirement. Because the theater_ratio (0.85) is extreme, this constraint 
 * is highly visible but provides negative utility to the powerless due to 
 * the scaling formula. The "Tangled Rope" classification is mandatory here 
 * because there is a genuine coordination function (legislative negotiation) 
 * paired with active enforcement (the shutdown) and asymmetric victims.
 *
 * MANDATROPHY ANALYSIS:
 * The classification as "Tangled Rope" resolves the Mandatrophy by 
 * acknowledging that while the system is extracting value from the public, 
 * it is not doing so pointlessly—it is the "cost" of institutional boundary 
 * maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_shutdown_duration,
    'Will the House pass the two-week stopgap measure by Feb 8?',
    'Direct monitoring of House floor votes and legislative trackers.',
    'Success reverts the Tangled Rope to a Scaffold; failure moves it toward a Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fed_shutdown_2026, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio (Theatrical maintenance of the standoff)
narrative_ontology:measurement(fs_tr_t0, fed_shutdown_2026, theater_ratio, 0, 0.85).
narrative_ontology:measurement(fs_tr_t7, fed_shutdown_2026, theater_ratio, 7, 0.88).
narrative_ontology:measurement(fs_tr_t14, fed_shutdown_2026, theater_ratio, 14, 0.92).

% Base Extractiveness (Increasing cost of systemic entropy/data delay)
narrative_ontology:measurement(fs_be_t0, fed_shutdown_2026, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(fs_be_t7, fed_shutdown_2026, base_extractiveness, 7, 0.65).
narrative_ontology:measurement(fs_be_t14, fed_shutdown_2026, base_extractiveness, 14, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
