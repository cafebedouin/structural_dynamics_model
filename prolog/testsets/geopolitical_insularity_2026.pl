% ============================================================================
% CONSTRAINT STORY: geopolitical_insularity_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2023-10-27
% ============================================================================

:- module(constraint_geopolitical_insularity_2026, []).

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
 * * constraint_id: geopolitical_insularity_2026
 * human_readable: Geopolitical Nationalist Insularity
 * domain: geopolitical/economic
 * * SUMMARY:
 * A world order defined by the "Great Realignment" where trust is a zero-sum
 * nationalist asset. 65% of people fear foreign disinformation, and 34%
 * would actively reduce foreign market presence to protect domestic stability. This
 * creates a high-friction environment for global trade, imposing a "protectionist tax".
 * * KEY AGENTS:
 * - Domestic Worker: Subject (Powerless) - Fears job loss from trade wars (66%).
 * - Adapted Multinational Corp: Beneficiary (Institutional) - Pivots to "Polynational" models to capture protected markets.
 * - Global Auditor: Auditor (Analytical) - Tracks the 31pt domestic trust gap in Canada as a metric of extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction (0.60) reflects the productivity hit and "protectionist tax" on consumers and non-adapted businesses.
domain_priors:base_extractiveness(geopolitical_insularity_2026, 0.60).
% Suppression (0.70) reflects that a supermajority (70%) are unwilling to trust foreign actors, suppressing alternative trade models.
domain_priors:suppression_score(geopolitical_insularity_2026, 0.70).
% Theater ratio (0.55) reflects high "Brand Nationalism" performativity vs actual functional economic coordination.
domain_priors:theater_ratio(geopolitical_insularity_2026, 0.55).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(geopolitical_insularity_2026, extractiveness, 0.60).
narrative_ontology:constraint_metric(geopolitical_insularity_2026, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(geopolitical_insularity_2026, theater_ratio, 0.55).

% Constraint self-claim: The system claims to be an enforcement mechanism for national values and security.
narrative_ontology:constraint_claim(geopolitical_insularity_2026, tangled_rope).

% Binary flags and structural properties
domain_priors:requires_active_enforcement(geopolitical_insularity_2026). % Required for Tangled Rope

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(geopolitical_insularity_2026, adapted_multinational_corporations).
narrative_ontology:constraint_victim(geopolitical_insularity_2026, domestic_workers_and_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE DOMESTIC WORKER (SNARE)
% Sees global trade as a trap; 66% worry trade policies will hurt their employer.
% χ = 0.60 * 1.5 (powerless) * 1.0 (national) = 0.90. High extraction felt directly.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ADAPTED MULTINATIONAL (ROPE)
% Views the nationalist rules as a new coordination game. By adopting a "Polynational"
% model, they bypass foreign distrust and benefit from protected markets.
% χ = 0.60 * -0.2 (institutional) * 1.2 (global) = -0.144. The constraint is a net benefit.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects both the coordination function (aligning business with national sentiment)
% and the severe asymmetric extraction imposed on workers and consumers.
% χ = 0.60 * 1.15 (analytical) * 1.2 (global) = 0.828. High extraction is visible.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_insularity_2026_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between the worker (snare) and the corporation (rope).
    constraint_indexing:constraint_classification(geopolitical_insularity_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_insularity_2026, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_insularity_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(geopolitical_insularity_2026),
    narrative_ontology:constraint_beneficiary(geopolitical_insularity_2026, _),
    narrative_ontology:constraint_victim(geopolitical_insularity_2026, _).

test(high_extraction_and_suppression) :-
    % Verify the base metrics meet the thresholds for a severe constraint.
    domain_priors:base_extractiveness(geopolitical_insularity_2026, E), E >= 0.46,
    domain_priors:suppression_score(geopolitical_insularity_2026, S), S >= 0.60.

:- end_tests(geopolitical_insularity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models the economic friction from rising nationalism. The key
 * perspectival gap is between the domestic worker, who feels the negative
 * consequences (job insecurity, higher prices) as a Snare, and the adapted
 * multinational corporation, which leverages the new rules to its advantage,
 * experiencing it as a Rope for coordinating in protected markets.
 * The 0.60 extraction score reflects the "protectionist tax" borne by society,
 * while the 0.70 suppression score is driven by widespread public distrust of
 * foreign entities, which actively suppresses free trade alternatives.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The analytical classification as Tangled Rope is critical. A simpler analysis
 * might see only the harm to workers and label the system a pure Snare. This would
 * miss the genuine (if coercive) coordination function that benefits adapted
 * institutional players. By identifying both the beneficiaries (coordination) and
 * victims (asymmetric extraction), the Tangled Rope classification provides a
 * more complete and stable model of the system's dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_geo_2026,
    'Will the "Polynational" corporate model resolve the 29pt trust gap in developed markets, or is it merely a temporary adaptation to a fundamentally autarkic trend?',
    'Tracking trust scores and market share of foreign companies that adopt 100% local leadership and branding over a 5-year period.',
    'Success implies a path back to modified global trade (Tangled Rope); failure implies a slide into deeper economic fragmentation (Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(geopolitical_insularity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models a sharp increase in
% both nationalist performativity (theater) and economic extraction post-2020.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(geo_tr_t0, geopolitical_insularity_2026, theater_ratio, 0, 0.30).
narrative_ontology:measurement(geo_tr_t5, geopolitical_insularity_2026, theater_ratio, 5, 0.45).
narrative_ontology:measurement(geo_tr_t10, geopolitical_insularity_2026, theater_ratio, 10, 0.55).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(geo_ex_t0, geopolitical_insularity_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(geo_ex_t5, geopolitical_insularity_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(geo_ex_t10, geopolitical_insularity_2026, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The constraint functions by enforcing alignment with national sentiment.
narrative_ontology:coordination_type(geopolitical_insularity_2026, enforcement_mechanism).

% Network relationships: Nationalist insularity directly impacts the stability and
% structure of global supply chains, particularly in sensitive sectors.
narrative_ontology:affects_constraint(geopolitical_insularity_2026, semiconductor_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */