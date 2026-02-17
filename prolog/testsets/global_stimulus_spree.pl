% ============================================================================
% CONSTRAINT STORY: global_stimulus_spree
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_global_stimulus_spree, []).

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
 * * constraint_id: global_stimulus_spree
 * human_readable: The 2026 Global Fiscal Stimulus Surge
 * domain: economic/political
 * * SUMMARY:
 * Governments are deploying multitrillion-dollar stimulus packages to fuel AI,
 * green energy, and rearmament. While boosting near-term growth, this creates
 * a "red flag" vulnerability of soaring debt and interest payments, extracting
 * value from future generations to solve present-day coordination problems.
 * * KEY AGENTS:
 * - Future Taxpayers: Subject (Powerless/Trapped in debt cycle)
 * - G7/G20 Governments & Subsidized Industries: Beneficiary (Institutional/Mobile)
 * - The Bond Market Auditor: Auditor (Analytical/Detection of risk)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.62) due to record interest payments and debt-to-GDP
% projected to exceed 100%, representing a significant transfer of future
% wealth to the present.
domain_priors:base_extractiveness(global_stimulus_spree, 0.62).
domain_priors:suppression_score(global_stimulus_spree, 0.45).
domain_priors:theater_ratio(global_stimulus_spree, 0.30).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(global_stimulus_spree, extractiveness, 0.62).
narrative_ontology:constraint_metric(global_stimulus_spree, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(global_stimulus_spree, theater_ratio, 0.30).

% Constraint self-claim (what does the constraint claim to be?)
% Governments frame this as essential coordination for national security and economic vitality.
narrative_ontology:constraint_claim(global_stimulus_spree, tangled_rope).
narrative_ontology:human_readable(global_stimulus_spree, "The 2026 Global Fiscal Stimulus Surge").

% Binary flags
% Required for Tangled Rope: enforcement of taxation and debt servicing.
domain_priors:requires_active_enforcement(global_stimulus_spree).
% Required for Scaffold: many stimulus programs have explicit end-dates or are
% tied to specific political terms, acting as a de facto sunset clause.
narrative_ontology:has_sunset_clause(global_stimulus_spree).

% Structural property derivation hooks:
% Required for Tangled Rope and Scaffold.
narrative_ontology:constraint_beneficiary(global_stimulus_spree, subsidized_industries).
narrative_ontology:constraint_victim(global_stimulus_spree, future_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE FUTURE TAXPAYER (SNARE)
% The debt burden is a trap where future generations pay the bill
% for current growth-sapping shocks.
constraint_indexing:constraint_classification(global_stimulus_spree, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE NATIONAL GOVERNMENT (ROPE)
% Viewed as essential coordination for generational investments
% in infrastructure and sovereignty.
constraint_indexing:constraint_classification(global_stimulus_spree, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CHIEF ECONOMIST (TANGLED ROPE)
% Detects vulnerabilities bubbling under the surface where coordination
% (investment in key sectors) and extraction (intergenerational debt) are inextricably linked.
constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE CAMPAIGN STRATEGIST (SCAFFOLD)
% Temporary support provided to voters before snap elections or transitions,
% justified by a sunset clause (program end dates).
constraint_indexing:constraint_classification(global_stimulus_spree, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))) :-
    narrative_ontology:has_sunset_clause(global_stimulus_spree).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_stimulus_spree_tests).

test(perspectival_gap) :-
    % Verify the stimulus is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(global_stimulus_spree, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_stimulus_spree, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(global_stimulus_spree, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify all three structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(global_stimulus_spree),
    narrative_ontology:constraint_beneficiary(global_stimulus_spree, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(global_stimulus_spree, _). % derives has_asymmetric_extraction

test(scaffold_structural_properties) :-
    % Verify both structural requirements for Scaffold are met.
    narrative_ontology:has_sunset_clause(global_stimulus_spree),
    narrative_ontology:constraint_beneficiary(global_stimulus_spree, _). % derives has_coordination_function

test(threshold_validation) :-
    narrative_ontology:constraint_metric(global_stimulus_spree, extractiveness, E),
    E >= 0.46.

:- end_tests(global_stimulus_spree_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.62) is driven by IMF projections that global public
 * debt will exceed 100% of GDP by 2029, representing a massive, coercive
 * transfer of wealth from future taxpayers. The Perspectival Gap is stark:
 * governments (institutional) see a Rope for coordinating vital investments,
 * while future taxpayers (powerless) are caught in a Snare of inescapable debt.
 * The analytical view resolves this as a Tangled Rope, acknowledging both the
 * genuine coordination function (beneficiaries exist) and the severe asymmetric
 * extraction (victims exist). The Scaffold classification is justified by the
 * explicitly temporary nature of many stimulus programs, which function as
 * political sunset clauses.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical here. Without it, the system might
 * default to a pure Snare, ignoring the fact that the spending targets genuine
 * collective action problems (energy transition, rearmament, AI infrastructure).
 * This classification correctly captures the hybrid nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ai_productivity,
    'Will AI investments yield enough productivity growth to outpace the debt interest spiral?',
    'Analysis of GDP-to-Interest ratios in U.S. and Germany by 2030.',
    'Success = Re-classification as Rope; Failure = Hard collapse into kinetic Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents the period from
% the pandemic-era spending surge (T=0) to the 2029 IMF projections (T=10).
narrative_ontology:interval(global_stimulus_spree, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (0.62 > 0.46).
% Models the intensification of debt burden and political theater over the
% 2020-2029 period.

% Theater ratio over time (slight increase as initial crisis response becomes politicized):
narrative_ontology:measurement(gss_tr_t0, global_stimulus_spree, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gss_tr_t5, global_stimulus_spree, theater_ratio, 5, 0.25).
narrative_ontology:measurement(gss_tr_t10, global_stimulus_spree, theater_ratio, 10, 0.30).

% Extraction over time (ramps up as debt accumulates and interest rates rise):
narrative_ontology:measurement(gss_ex_t0, global_stimulus_spree, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gss_ex_t5, global_stimulus_spree, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(gss_ex_t10, global_stimulus_spree, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: Fiscal stimulus is a primary tool for resource allocation.
narrative_ontology:coordination_type(global_stimulus_spree, resource_allocation).

% Network relationships: This level of fiscal activity directly impacts the
% stability and perceived risk of government debt.
narrative_ontology:affects_constraint(global_stimulus_spree, sovereign_debt_stability).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */