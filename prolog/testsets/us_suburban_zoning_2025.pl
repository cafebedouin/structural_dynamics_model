% ============================================================================
% CONSTRAINT STORY: us_suburban_zoning_2025
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_us_suburban_zoning_2025, []).

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
 * * constraint_id: us_suburban_zoning_2025
 * human_readable: Single-Family Suburban Zoning Codes
 * domain: political/legal/economic
 * * SUMMARY:
 * Local zoning codes in the United States restrict large swaths of metropolitan land to low-density,
 * single-family homes. This legal constraint effectively bans apartment buildings and other forms of
 * dense housing, creating a severe mismatch between existing housing stock and modern demographic needs,
 * which drives up housing costs and exacerbates inequality.
 * * KEY AGENTS:
 * - Middle-Income Renters: Subjects of the housing shortage, facing high costs and limited options (Powerless).
 * - Established Homeowners: Beneficiaries of artificially inflated property values and protected neighborhood character (Institutional).
 * - Urban Planners/Policy Analysts: Observers studying the systemic effects of zoning policy (Analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(us_suburban_zoning_2025, 0.70). % High asymmetry; homeowners gain value while renters face shortages.
domain_priors:suppression_score(us_suburban_zoning_2025, 0.65).   % Alternatives like apartment buildings are legally banned in many areas.
domain_priors:theater_ratio(us_suburban_zoning_2025, 0.15).       % Enforcement is functional (permits, inspections), not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(us_suburban_zoning_2025, extractiveness, 0.70).
narrative_ontology:constraint_metric(us_suburban_zoning_2025, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_suburban_zoning_2025, theater_ratio, 0.15).

% Constraint self-claim: Zoning laws claim to be a coordination mechanism for orderly development.
narrative_ontology:constraint_claim(us_suburban_zoning_2025, tangled_rope).
narrative_ontology:human_readable(us_suburban_zoning_2025, "Single-Family Suburban Zoning Codes").

% Binary flags: Requires active enforcement via municipal planning boards, permits, and inspections.
domain_priors:requires_active_enforcement(us_suburban_zoning_2025).

% Structural property derivation hooks for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(us_suburban_zoning_2025, established_homeowners).
narrative_ontology:constraint_victim(us_suburban_zoning_2025, middle_income_renters).
narrative_ontology:constraint_victim(us_suburban_zoning_2025, multifamily_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (RENTER)
% For those priced out of the market, zoning is a coercive trap that limits
% their housing options and extracts significant portions of their income.
% χ = 0.70 (ε) * 1.5 (π(powerless)) * 0.9 (σ(regional)) = 0.945. High extraction.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (HOMEOWNER)
% For established homeowners, zoning is a beneficial coordination tool that
% protects property values and maintains neighborhood character.
% χ = 0.70 (ε) * -0.2 (π(institutional)) * 1.0 (σ(national)) = -0.14. Perceived as pure benefit.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% An analyst sees both the coordination function (organizing land use) and the
% severe asymmetric extraction. Combined with active enforcement, this is a
% canonical Tangled Rope.
% χ = 0.70 (ε) * 1.15 (π(analytical)) * 1.2 (σ(global)) = 0.966.
constraint_indexing:constraint_classification(us_suburban_zoning_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_suburban_zoning_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_observer_classification) :-
    constraint_indexing:constraint_classification(us_suburban_zoning_2025, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_structural_properties) :-
    narrative_ontology:constraint_beneficiary(us_suburban_zoning_2025, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(us_suburban_zoning_2025, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(us_suburban_zoning_2025).

test(high_extraction_threshold) :-
    narrative_ontology:constraint_metric(us_suburban_zoning_2025, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(us_suburban_zoning_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores reflect a system with a stated coordination goal (orderly development)
 * that now functions primarily as an extractive mechanism. Extractiveness (0.70) is high
 * due to the wealth transfer from renters to landowners via artificial scarcity. Suppression
 * (0.65) is high because denser housing types are legally prohibited.
 *
 * The PERSPECTIVAL GAP is stark:
 * - For a RENTER (powerless), the system is a 'Snare'. It's a coercive trap with no easy exit,
 *   driving up costs and limiting life choices.
 * - For a HOMEOWNER (institutional), it's a 'Rope'. It coordinates behavior to protect their
 *   largest asset, appearing as a purely beneficial rule set.
 * - For an ANALYST, it's a 'Tangled Rope'. The system possesses a genuine (if outdated)
 *   coordination function but has been captured to serve an extractive purpose, requiring
 *   active legal enforcement to maintain the asymmetry.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A simple Snare classification would miss the fact that zoning does perform a real
 * coordination function that beneficiaries value. A Rope classification would ignore the
 * immense negative externalities and coercive extraction imposed on victims. The
 * 'Tangled Rope' classification correctly identifies this dual nature, preventing the
 * system from mislabeling a complex, contested system as either pure coordination or
 * pure extraction. It captures the essence of a mandate that has drifted from its
 * original purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_zoning_1,
    'Will state-level legislative overrides (e.g., California ADU laws) successfully dismantle local zoning power, or will NIMBY resistance render them ineffective?',
    'Measure the net change in housing unit density in single-family zones in states with pre-emption laws over a 10-year period.',
    'If overrides are effective, the constraint may evolve towards a Rope. If they fail, it remains a potent Tangled Rope/Snare.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_zoning_2,
    'Is the primary driver of homeowner opposition economic (protecting property values) or cultural (preserving neighborhood character)?',
    'Survey data correlating opposition to upzoning with homeowner financial leverage vs. stated cultural concerns.',
    'If economic, financial incentives could resolve the conflict. If cultural, the constraint is more deeply entrenched and resistant to change.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Interval represents the period from the rise of modern zoning to the present.
narrative_ontology:interval(us_suburban_zoning_2025, 0, 55). % Approx 1970-2025

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows how zoning's extractive nature grew as housing demand
% outpaced the supply model it enforces.
% T=0 (c. 1970), T=27 (c. 1997), T=55 (c. 2025).

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(us_suburban_zoning_2025_tr_t0, us_suburban_zoning_2025, theater_ratio, 0, 0.10).
narrative_ontology:measurement(us_suburban_zoning_2025_tr_t27, us_suburban_zoning_2025, theater_ratio, 27, 0.12).
narrative_ontology:measurement(us_suburban_zoning_2025_tr_t55, us_suburban_zoning_2025, theater_ratio, 55, 0.15).

% Extraction over time (grows significantly):
narrative_ontology:measurement(us_suburban_zoning_2025_ex_t0, us_suburban_zoning_2025, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(us_suburban_zoning_2025_ex_t27, us_suburban_zoning_2025, base_extractiveness, 27, 0.45).
narrative_ontology:measurement(us_suburban_zoning_2025_ex_t55, us_suburban_zoning_2025, base_extractiveness, 55, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: Zoning is a classic example of a resource allocation mechanism (for land).
narrative_ontology:coordination_type(us_suburban_zoning_2025, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */