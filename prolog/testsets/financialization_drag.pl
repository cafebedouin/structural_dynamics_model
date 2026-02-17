% ============================================================================
% CONSTRAINT STORY: financial_drag
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_financial_drag, []).

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
 * * constraint_id: financial_drag
 * human_readable: The Financialization Gravity Well
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where the primary mechanism for resource allocation shifts from
 * real-world production to financial engineering. Over time, the "Rope" of
 * capital coordination becomes a parasitic "Tangled Rope" as the financial sector
 * siphons the surplus of productive industries to service its own internal
 * complexity. This creates a "drag" on innovation, where the smartest minds
 * are incentivized to optimize spreadsheets rather than physics.
 * * KEY AGENTS:
 * - Productive Sector (e.g., Industrial R&D Lead): Subject (Powerless)
 * - Asset Managers (e.g., Investment Firm): Beneficiary (Institutional)
 * - Macro-Economic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the "drag" siphons the species' real
% productive potential into zero-sum arbitrage.
domain_priors:base_extractiveness(financial_drag, 0.84).
domain_priors:suppression_score(financial_drag, 0.72).
domain_priors:theater_ratio(financial_drag, 0.78). % High theater: Focus on "quarterly earnings" over long-term utility.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(financial_drag, extractiveness, 0.84).
narrative_ontology:constraint_metric(financial_drag, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(financial_drag, theater_ratio, 0.78).

% Constraint self-claim: It presents itself as a pure coordination mechanism.
narrative_ontology:constraint_claim(financial_drag, tangled_rope).
narrative_ontology:human_readable(financial_drag, "The Financialization Gravity Well").

% Structural properties required for Tangled Rope classification
domain_priors:requires_active_enforcement(financial_drag).
narrative_ontology:constraint_beneficiary(financial_drag, asset_managers).
narrative_ontology:constraint_victim(financial_drag, productive_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the R&D lead, financialization is a snare: they cannot fund long-term
% breakthroughs because the "cost of capital" is tethered to short-term returns.
constraint_indexing:constraint_classification(financial_drag, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views financialization as a vital Rope—the ultimate tool
% for global capital coordination and "efficient" market clearing.
constraint_indexing:constraint_classification(financial_drag, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of coordination intent (Rope)
% masking deep, structural extraction of real-world value (Snare).
constraint_indexing:constraint_classification(financial_drag, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(financial_drag_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(financial_drag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(financial_drag, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(tangled_rope_structural_properties) :-
    % Verify that all structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(financial_drag, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(financial_drag, _),     % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(financial_drag).

test(analytical_classification_is_tangled_rope) :-
    % Verify the canonical analytical observer correctly classifies the constraint as Tangled Rope.
    constraint_indexing:constraint_classification(financial_drag, tangled_rope,
        context(agent_power(analytical),
                time_horizon(civilizational),
                exit_options(analytical),
                spatial_scope(global))).

:- end_tests(financial_drag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high base extractiveness (0.84) and suppression (0.72) model a system
 * where capital allocation, once a coordination mechanism (Rope), has become
 * primarily extractive. The financial sector's complexity and incentive
 * structures siphon value from the productive economy, creating a drag on
 * long-term innovation. The high theater ratio (0.78) reflects the focus on
 * performative metrics like quarterly earnings over genuine productive utility.
 * Enforcement is active via regulatory frameworks that prioritize shareholder
 * value and the legal complexity that shields financial instruments.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. For the 'productive_sector' (e.g., an R&D lead), the
 * system is a Snare; capital is inaccessible for long-term, non-financialized
 * projects. For 'asset_managers', it is a Rope of immense power, efficiently
 * allocating capital on a global scale to maximize returns, which they view
 * as the definition of coordination.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The extreme extraction (0.84) risks a simplistic "Snare" classification,
 * which would be a mandatrophy error—a failure to see the mandate. The system
 * *does* perform a coordination function, which is why it persists. The
 * Tangled Rope classification resolves this by acknowledging both the genuine
 * coordination role (perceived by beneficiaries) and the severe, asymmetric
 * extraction (felt by victims). The high theater ratio is a symptom of this
 * decay, where the original mandate (efficient allocation for production) has
 * been replaced by a proxy (maximizing financial returns), nearing a Piton state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_financial_drag,
    'Is the drag an emergent property of complex markets (Mountain) or a constructed system of regulatory capture (Snare)?',
    'Analysis of legislative changes and lobbying efforts correlating with increased financial sector profits vs. productive sector stagnation.',
    'If Mountain: The drag is an unavoidable cost of global capital coordination. If Snare: The drag is reversible through policy intervention.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(financial_drag, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The narrative describes a degradation from a functional Rope to a parasitic
% Tangled Rope. The measurements model this lifecycle drift.
% At T=0, capital markets were a functional Rope, with low extraction and theater.
narrative_ontology:measurement(financial_drag_tr_t0, financial_drag, theater_ratio, 0, 0.15).
narrative_ontology:measurement(financial_drag_ex_t0, financial_drag, base_extractiveness, 0, 0.20).

% By T=5, financial engineering has become dominant, increasing extraction and performative metrics.
narrative_ontology:measurement(financial_drag_tr_t5, financial_drag, theater_ratio, 5, 0.55).
narrative_ontology:measurement(financial_drag_ex_t5, financial_drag, base_extractiveness, 5, 0.65).

% At T=10, the system is in its current state of high extraction and theater.
narrative_ontology:measurement(financial_drag_tr_t10, financial_drag, theater_ratio, 10, 0.78).
narrative_ontology:measurement(financial_drag_ex_t10, financial_drag, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(financial_drag, resource_allocation).

% Network relationships (structural influence edges)
% Financialization drag directly suppresses the potential for long-term,
% capital-intensive R&D.
narrative_ontology:affects_constraint(financial_drag, industrial_innovation_rate).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */