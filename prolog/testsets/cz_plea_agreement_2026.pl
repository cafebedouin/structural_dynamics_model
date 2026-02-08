% ============================================================================
% CONSTRAINT STORY: cz_plea_agreement_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_cz_plea, []).

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
 * * constraint_id: cz_plea_agreement_2026
 * human_readable: CZ and Binance Global Regulatory Settlement
 * domain: economic/political/legal
 * * SUMMARY:
 * This constraint represents the 2023 plea deal where Changpeng Zhao (CZ) pleaded
 * guilty to failing to maintain an effective AML program.
 * It indicates a structural "exit" from the US market for Binance, a multi-billion
 * dollar extraction from the entity, and a lifetime industry ban (later modified
 * by a 2025 pardon).
 * * KEY AGENTS:
 * - [Changpeng Zhao & Binance]: Subject (Powerless/Trapped) - Barred from leadership and
 * fined $4.3B (entity) + $50M (personal).
 * - [US DOJ/FinCEN/CFTC]: Beneficiary (Institutional) - Secured one of the
 * largest corporate resolutions ($4.3B).
 * - [Independent Monitor]: Auditor (Analytical) - Five-year oversight of
 * operations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extremely high extraction (0.85) due to the $4.3B penalty and permanent loss of CEO role.
domain_priors:base_extractiveness(cz_plea_agreement_2026, 0.85).

% High suppression (0.78); the agreement mandates a "complete exit from the United States".
domain_priors:suppression_score(cz_plea_agreement_2026, 0.78).

% Low theater (0.25); the monitorship and massive payouts are functional, non-performative limits.
domain_priors:theater_ratio(cz_plea_agreement_2026, 0.25).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cz_plea_agreement_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(cz_plea_agreement_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(cz_plea_agreement_2026, theater_ratio, 0.25).

% Constraint self-claim: The plea deal is an enforcement action claiming to coordinate AML standards.
narrative_ontology:constraint_claim(cz_plea_agreement_2026, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(cz_plea_agreement_2026). % Required for Tangled Rope

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(cz_plea_agreement_2026, us_regulatory_agencies).
narrative_ontology:constraint_victim(cz_plea_agreement_2026, binance_and_founder).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For CZ, the plea is a snare: he is "trapped" into a $50M personal fine and
% barred from his life's work at Binance.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For US Regulators, the deal is a rope: it coordinates global AML standards
% and forces one of the largest players into the "regulated rails".
constraint_indexing:constraint_classification(cz_plea_agreement_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Observers detect a hybrid: it stabilizes the market (coordination) but
% functions as an asymmetric extraction of wealth from offshore to onshore.
constraint_indexing:constraint_classification(cz_plea_agreement_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cz_plea_tests).

test(perspectival_gap) :-
    % Verify the Subject (CZ) feels a Snare, while the State views it as a Rope.
    constraint_indexing:constraint_classification(cz_plea_agreement_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cz_plea_agreement_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(cz_plea_agreement_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(cz_plea_agreement_2026, E),
    E > 0.46. % Triggers high-extraction requirements.

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(cz_plea_agreement_2026),
    narrative_ontology:constraint_beneficiary(cz_plea_agreement_2026, _),
    narrative_ontology:constraint_victim(cz_plea_agreement_2026, _).

:- end_tests(cz_plea_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) is extreme due to the $4.3 billion entity-level
 * penalty combined with CZ's personal $50M fine and forced resignation.
 * The Perspectival Gap highlights that while the state sees "legal coordination,"
 * the founder experiences the dismantling of his leadership (Snare).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification prevents viewing this as purely punitive;
 * it acknowledges the genuine coordination function (creating "regulated rails"
 * for the 2026 crypto "Super-Cycle" by removing ambiguity) while also
 * correctly identifying the massive asymmetric extraction involved. This resolves
 * the mandatrophy by not misclassifying a hybrid as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_pardon_stability,
    'Does the 2025 Presidential Pardon nullify the "Snare" or merely relocate it?',
    'Analysis of CZs 2026 Davos advisory work vs. ongoing monitorship restrictions.',
    'If leadership returns, it was a Scaffold; if ban remains in spirit, it is a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cz_plea_agreement_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the interval from the 2023 Plea (T=0) to the 2026 Post-Pardon era (T=10).
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio: Drops as performative "growth at all costs" is replaced
% by functional, monitored compliance.
narrative_ontology:measurement(cz_tr_t0, cz_plea_agreement_2026, theater_ratio, 0, 0.65).
narrative_ontology:measurement(cz_tr_t5, cz_plea_agreement_2026, theater_ratio, 5, 0.40).
narrative_ontology:measurement(cz_tr_t10, cz_plea_agreement_2026, theater_ratio, 10, 0.25).

% Extraction: Peaks at settlement (T=0) and stabilizes as legal fees
% and fines are paid out.
narrative_ontology:measurement(cz_ex_t0, cz_plea_agreement_2026, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(cz_ex_t5, cz_plea_agreement_2026, base_extractiveness, 5, 0.88).
narrative_ontology:measurement(cz_ex_t10, cz_plea_agreement_2026, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The plea deal is a form of regulatory enforcement.
narrative_ontology:coordination_type(cz_plea_agreement_2026, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(cz_plea_agreement_2026, 0.1).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(cz_plea_agreement_2026, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */