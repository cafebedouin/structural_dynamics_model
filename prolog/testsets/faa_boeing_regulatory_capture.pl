% ============================================================================
% CONSTRAINT STORY: faa_boeing_regulatory_capture
% ============================================================================
% Version: 5.3 (Deferential Realism Core + Sigmoid Directionality v5.0)
% Logic: 5.3 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Directionality Override)
% Generated: 2026-02-08
%
% PURPOSE: Phase 2 test case for v5.0 directionality refactoring.
% Exercises directionality_override/3 to prove inter-institutional
% differentiation works as intended.
%
% THE PROBLEM v5.0 SOLVES:
% The old power_modifier/2 dispatch gave ALL institutional actors pi=-0.20,
% regardless of whether they were regulator (FAA) or regulated (Boeing).
% Both got the same canonical d=0.00.
%
% With directionality_override/3 at d=0.30, the derivation chain produces
% a DIFFERENT d value (0.30) than the structural derivation would (0.15),
% demonstrating that the override takes priority. The f(d) shifts from
% -0.015 (structural) to 0.194 (override), changing chi from -0.006 to
% 0.077. Both classify as rope, but the chi values differ — proving the
% mechanism works. Future per-agent overrides will enable different
% institutional actors to cross gate boundaries.
% ============================================================================

:- module(constraint_faa_boeing_regulatory_capture, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *
 * constraint_id: faa_boeing_regulatory_capture
 * human_readable: FAA-Boeing Organizational Designation Authorization (ODA)
 * domain: political/economic
 *
 * SUMMARY:
 * The FAA's Organizational Designation Authorization allows Boeing to
 * self-certify aircraft safety, creating a structure where the regulated
 * entity effectively controls its own oversight. Post-737 MAX crashes
 * (2018-2019), this revealed a classic regulatory capture pattern: the
 * regulator (FAA) had become dependent on the regulated (Boeing) for
 * technical expertise and funding, while Boeing extracted regulatory
 * forbearance. This is the paradigm case for inter-institutional
 * directionality — two institutional actors on opposite sides of the
 * same constraint.
 *
 * KEY AGENTS:
 * - Flying Public: Subject (Powerless) - Bears all safety risk with no voice.
 * - Boeing: Institutional (Regulated) - Captures regulatory process, extracts
 *   forbearance. High directionality despite institutional power.
 * - FAA: Institutional (Regulator) - Nominally the enforcer but structurally
 *   captured. Low directionality (beneficiary of revolving-door employment).
 * - NTSB Investigator: Analytical - Sees the tangled rope of coordination
 *   (aviation safety) entangled with extraction (self-certification capture).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% ε = 0.40: Moderate extraction via self-certification cost savings,
% deferred maintenance, accelerated production timelines.
% Calibrated below snare eps floor (0.46) so gate interactions are clean:
%   - Powerless chi lands in tangled_rope range (not snare)
%   - Institutional chi lands in rope range (eps <= 0.45)
domain_priors:base_extractiveness(faa_boeing_regulatory_capture, 0.40).

% Suppression = 0.55: Significant barriers to alternative oversight
% (congressional lobbying, revolving-door staffing, technical complexity).
% Above tangled_rope supp floor (0.40) but below snare supp floor (0.60).
domain_priors:suppression_score(faa_boeing_regulatory_capture, 0.55).

% Theater = 0.45: Moderate performative compliance — ODA audits,
% congressional hearings, safety culture memos.
domain_priors:theater_ratio(faa_boeing_regulatory_capture, 0.45).

% Metric facts for classification engine
narrative_ontology:constraint_metric(faa_boeing_regulatory_capture, extractiveness, 0.40).
narrative_ontology:constraint_metric(faa_boeing_regulatory_capture, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(faa_boeing_regulatory_capture, theater_ratio, 0.45).

% Analytical classification: Tangled Rope
% The ODA genuinely coordinates (aviation safety requires manufacturer
% expertise) but extraction has grown into the coordination structure.
narrative_ontology:constraint_claim(faa_boeing_regulatory_capture, tangled_rope).

% Structural properties — 2 beneficiaries, 2 victims
% (Deliberately below coalition threshold of 3 victims to keep
% powerless chi unmodified by coalition modeling.)
narrative_ontology:constraint_beneficiary(faa_boeing_regulatory_capture, boeing_shareholders).
narrative_ontology:constraint_beneficiary(faa_boeing_regulatory_capture, faa_career_staff).
narrative_ontology:constraint_victim(faa_boeing_regulatory_capture, flying_public).
narrative_ontology:constraint_victim(faa_boeing_regulatory_capture, airline_workers).
domain_priors:requires_active_enforcement(faa_boeing_regulatory_capture).

/* ==========================================================================
   2B. DIRECTIONALITY OVERRIDES (v5.0)
   ========================================================================== */

% THE CORE v5.0 MECHANISM: Inter-institutional differentiation.
%
% Derivation chain for institutional power level:
%   1. Override check: directionality_override/3 → d=0.30 (FIRES)
%   2. Structural derivation (skipped): would give d=0.15
%      (power_role_heuristic(institutional, true, _, 0.15) + exit_modulation(mobile, 0.00))
%   3. Canonical fallback (skipped): would give d=0.00
%
% The override at d=0.30 captures domain knowledge that the FAA is not
% a typical institutional beneficiary. The revolving-door dynamics make
% the FAA more exposed than a pure beneficiary (d=0.00) but still net
% positive — they experience some extraction through reputation damage,
% congressional scrutiny, and staff demoralization from compromised safety.
%
% f(0.30) ≈ 0.194 — a small positive modifier, vs:
%   f(0.15) ≈ -0.015 (structural derivation)
%   f(0.00) ≈ -0.119 (canonical institutional)
%
% This produces chi = 0.40 * 0.194 * 1.0 = 0.077, still in rope range
% (chi ≤ 0.35, eps ≤ 0.45), but demonstrably different from the
% structural or canonical chi values.
%
constraint_indexing:directionality_override(faa_boeing_regulatory_capture, institutional, 0.30).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE FLYING PUBLIC (TANGLED ROPE)
% Powerless agent, trapped, national scope.
% Structural derivation: has_victims=true → d=0.85, exit=trapped → +0.05 → d=0.90
% f(0.90) ≈ 1.36
% χ = 0.40 * 1.36 * 1.0 ≈ 0.54 → tangled_rope range (0.40-0.90)
% Snare fails: eps=0.40 < 0.46 (snare_epsilon_floor)
constraint_indexing:constraint_classification(faa_boeing_regulatory_capture, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE FAA (ROPE — captured regulator sees coordination)
% Institutional agent with directionality_override d=0.30.
% f(0.30) ≈ 0.194
% χ = 0.40 * 0.194 * 1.0 ≈ 0.077 → below rope ceiling (0.35)
% eps=0.40 ≤ 0.45 (rope_epsilon_ceiling) → rope ✓
% The FAA perceives the ODA as a coordination mechanism (delegation of
% expertise), not as extraction. This is the regulator's captured view.
constraint_indexing:constraint_classification(faa_boeing_regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NTSB INVESTIGATOR (TANGLED ROPE)
% Analytical observer sees both the genuine coordination function
% (aviation safety requires ODA-like delegation) AND the extraction
% (Boeing captured the process).
% Structural derivation: analytical → d=0.72, f(d) ≈ 1.14
% χ = 0.40 * 1.14 * 1.2 = 0.55 → tangled_rope range (0.40-0.90)
constraint_indexing:constraint_classification(faa_boeing_regulatory_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(faa_boeing_regulatory_capture_tests).

% --- Perspectival Gap Tests ---

test(perspectival_gap_powerless_institutional) :-
    constraint_indexing:constraint_classification(
        faa_boeing_regulatory_capture, TypeP,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(
        faa_boeing_regulatory_capture, TypeI,
        context(agent_power(institutional), _, _, _)),
    TypeP \= TypeI.

test(analytical_is_tangled_rope) :-
    constraint_indexing:constraint_classification(
        faa_boeing_regulatory_capture, tangled_rope,
        context(agent_power(analytical), _, _, _)).

% --- Directionality Override Mechanism Tests ---

test(override_exists) :-
    constraint_indexing:directionality_override(
        faa_boeing_regulatory_capture, institutional, D),
    D > 0.0,
    D < 1.0.

test(override_takes_priority_over_structural_derivation) :-
    % The override (d=0.30) should take priority over the structural
    % derivation (d=0.15 from power_role_heuristic(institutional, true, _, 0.15)).
    Ctx = context(agent_power(institutional), time_horizon(generational),
                  exit_options(mobile), spatial_scope(national)),
    constraint_indexing:derive_directionality(faa_boeing_regulatory_capture, Ctx, D),
    % D should be 0.30 (override), not 0.15 (structural)
    abs(D - 0.30) < 0.001.

test(structural_derivation_fires_without_override) :-
    % For powerless (no override declared), structural derivation fires.
    % power_role_heuristic(powerless, _, true, 0.85) + exit_modulation(trapped, 0.05) = 0.90
    Ctx = context(agent_power(powerless), time_horizon(biographical),
                  exit_options(trapped), spatial_scope(national)),
    constraint_indexing:derive_directionality(faa_boeing_regulatory_capture, Ctx, D),
    abs(D - 0.90) < 0.001.

test(sigmoid_produces_different_modifiers) :-
    % The override d=0.30 produces a meaningfully different f(d) than
    % the structural derivation d=0.15 would have.
    constraint_indexing:sigmoid_f(0.30, F_override),
    constraint_indexing:sigmoid_f(0.15, F_structural),
    F_override > F_structural,
    Diff is F_override - F_structural,
    Diff > 0.10.

test(chi_differs_between_perspectives) :-
    % Powerless chi should be dramatically higher than institutional chi,
    % demonstrating that directionality creates meaningful perspectival gaps.
    CtxP = context(agent_power(powerless), time_horizon(biographical),
                   exit_options(trapped), spatial_scope(national)),
    CtxI = context(agent_power(institutional), time_horizon(generational),
                   exit_options(mobile), spatial_scope(national)),
    constraint_indexing:extractiveness_for_agent(faa_boeing_regulatory_capture, CtxP, ChiP),
    constraint_indexing:extractiveness_for_agent(faa_boeing_regulatory_capture, CtxI, ChiI),
    ChiP > 0.40,    % Powerless chi should be substantial
    ChiI < 0.15,    % Institutional chi should be very low
    Ratio is ChiP / max(ChiI, 0.001),
    Ratio > 3.0.    % At least 3x difference

:- end_tests(faa_boeing_regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This testset is the paradigm case for v5.0 directionality. The old
 * power_modifier/2 dispatch gave ALL institutional actors pi=-0.20,
 * regardless of whether they were regulator or regulated.
 *
 * The v5.0 derivation chain demonstrates three paths:
 *   1. OVERRIDE: institutional gets d=0.30 (from directionality_override/3),
 *      NOT d=0.15 (from structural derivation) or d=0.00 (from canonical).
 *   2. STRUCTURAL: powerless gets d=0.90 (from power_role_heuristic +
 *      exit_modulation), derived from beneficiary/victim declarations.
 *   3. CANONICAL: analytical gets d=0.72 (from canonical_d_for_power),
 *      because power_role_heuristic for analytical ignores beneficiary/
 *      victim data and always returns 0.72.
 *
 * PERSPECTIVAL GAP:
 * The flying public (d=0.90, f≈1.36, chi≈0.54) sees a tangled rope —
 * they feel the extraction from self-certification failures but the
 * constraint also provides genuine safety coordination. The FAA
 * (d=0.30, f≈0.19, chi≈0.08) sees a rope — the ODA is "how aviation
 * regulation works." The analyst (d=0.72, f≈1.14, chi≈0.55) sees the
 * tangled rope — genuine coordination entangled with captured extraction.
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents two errors:
 * (1) Calling the ODA a pure snare would miss the genuine need for
 *     manufacturer expertise in certification.
 * (2) Calling it a pure rope would miss that Boeing captured the
 *     oversight process to extract regulatory forbearance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_faa_boeing_recapture,
    'Can the post-MAX regulatory reforms (2020-2024) structurally reverse capture, or will the technical complexity monopoly reassert itself?',
    'Longitudinal analysis of ODA audit rejection rates, FAA direct-inspection hours, and Boeing compliance metrics post-reform.',
    'If reforms hold: tangled_rope with declining extraction (drift toward rope). If capture reasserts: extraction accumulation toward snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(faa_boeing_regulatory_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (rising: performative compliance culture grows)
narrative_ontology:measurement(faa_boeing_tr_t0, faa_boeing_regulatory_capture, theater_ratio, 0, 0.25).
narrative_ontology:measurement(faa_boeing_tr_t5, faa_boeing_regulatory_capture, theater_ratio, 5, 0.35).
narrative_ontology:measurement(faa_boeing_tr_t10, faa_boeing_regulatory_capture, theater_ratio, 10, 0.45).

% Extraction over time (rising: ODA delegation scope expanded 2005-2020)
narrative_ontology:measurement(faa_boeing_ex_t0, faa_boeing_regulatory_capture, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(faa_boeing_ex_t5, faa_boeing_regulatory_capture, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(faa_boeing_ex_t10, faa_boeing_regulatory_capture, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(faa_boeing_regulatory_capture, enforcement_mechanism).

% Network: Regulatory capture influences broader aviation supply chain oversight
narrative_ontology:affects_constraint(faa_boeing_regulatory_capture, us_aviation_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
