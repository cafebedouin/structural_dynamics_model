% ============================================================================
% CONSTRAINT STORY: perovskite_self_etching
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_perovskite_etching, []).

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
 * * constraint_id: perovskite_self_etching
 * human_readable: The 2D Perovskite Machinability Constraint
 * domain: technological/semiconductors
 * * SUMMARY:
 * This constraint models the dominance of traditional, high-cost, and destructive
 * lithography techniques when applied to soft lead halide perovskites. These
 * established methods act as a barrier to innovation in perovskite-based
 * optoelectronics. The development of alternative "self-etching" techniques
 * reveals the constructed nature of this barrier.
 * * KEY AGENTS:
 * - Perovskite Device Researchers: Subject (Powerless)
 * - Traditional Lithography Vendors: Beneficiary (Institutional)
 * - Semiconductor Industry Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction (0.52) represents the exponential cost increases and material
% damage imposed by traditional EUV scaling on this specific material class.
domain_priors:base_extractiveness(perovskite_self_etching, 0.52). % Snare extraction >= 0.46
domain_priors:suppression_score(perovskite_self_etching, 0.65).   % High suppression due to industry dominance and lack of viable alternatives.
domain_priors:theater_ratio(perovskite_self_etching, 0.15).       % Low theater; the tech is highly functional for silicon, just not perovskites.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(perovskite_self_etching, extractiveness, 0.52).
narrative_ontology:constraint_metric(perovskite_self_etching, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(perovskite_self_etching, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% The incumbent industry presents the limitations as an unavoidable physical law.
narrative_ontology:constraint_claim(perovskite_self_etching, tangled_rope).
narrative_ontology:human_readable(perovskite_self_etching, "The 2D Perovskite Machinability Constraint").
narrative_ontology:topic_domain(perovskite_self_etching, "technological/semiconductors").

% Binary flags
domain_priors:requires_active_enforcement(perovskite_self_etching). % Required for Tangled Rope. Represents IP protection and supply chain control.
narrative_ontology:has_sunset_clause(perovskite_self_etching).      % Mandatory if Scaffold. Represents the temporary nature of the self-etching research platform.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(perovskite_self_etching, traditional_lithography_vendors).
narrative_ontology:constraint_victim(perovskite_self_etching, perovskite_device_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE RESEARCHER (SNARE)
% Traditional lithography is a trap that damages soft materials and consumes
% research budgets, with no viable exit.
% χ = 0.52 * 1.5 (powerless) * 0.8 (local) = 0.624
constraint_indexing:constraint_classification(perovskite_self_etching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE LITHOGRAPHY VENDOR (ROPE)
% From the incumbent's perspective, their technology is a pure coordination
% good. The negative effective extraction reflects the immense profits and
% subsidies that make the constraint beneficial to them.
% χ = 0.52 * -0.2 (institutional) * 1.2 (global) = -0.1248
constraint_indexing:constraint_classification(perovskite_self_etching, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the essential coordination function of lithography for
% the wider semiconductor industry and the asymmetric extraction imposed on
% emerging material scientists.
% χ = 0.52 * 1.15 (analytical) * 1.2 (global) = 0.7176
constraint_indexing:constraint_classification(perovskite_self_etching, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PLATFORM ARCHITECT (SCAFFOLD)
% This perspective models the "self-etching" solution as a temporary research
% platform (Scaffold) designed to be superseded by more advanced techniques.
constraint_indexing:constraint_classification(perovskite_self_etching, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(perovskite_self_etching).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perovskite_etching_tests).

test(perspectival_gap_subject_beneficiary) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(perovskite_self_etching, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perovskite_self_etching, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the gap into a Tangled Rope.
    constraint_indexing:constraint_classification(perovskite_self_etching, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_snare) :-
    domain_priors:base_extractiveness(perovskite_self_etching, E),
    E >= 0.46. % Correct for v3.4 Snare threshold.

:- end_tests(perovskite_etching_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extraction of 0.52 is high, reflecting the severe economic and
 * material cost of forcing an established technology (lithography) onto an
 * unsuitable new material (perovskites). The suppression score of 0.65
 * reflects the market dominance and entrenched infrastructure of incumbents.
 * The Perspectival Gap is stark: researchers see a 'Snare', while vendors
 * see a beneficial 'Rope' (due to negative effective extraction for them).
 * The analytical classification correctly resolves this conflict as a
 * 'Tangled Rope', acknowledging both the valid coordination function of
 * lithography in general and its specific, asymmetric extractive effect here.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical. A simpler model might see the
 * high extraction and label the entire system a Snare, missing its legitimate
 * coordination role for silicon-based electronics. This model correctly
 * identifies that the constraint is a hybrid: a Rope for one domain that has
 * become a Snare for another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_perovskite_self_etching,
    'Is the material damage from lithography a fixed Mountain of physics or a Snare of impure chemical precursors?',
    'Comparative study of etching on crystals grown with varying precursor purity.',
    'Physics = Immutable Mountain; Impurity = A solvable Snare, reducible to a Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(perovskite_self_etching, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the increasing misfit between traditional lithography
% and advanced perovskite materials over the research interval. Extraction
% rises as scaling demands intensify.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(perovskite_self_etching_tr_t0, perovskite_self_etching, theater_ratio, 0, 0.10).
narrative_ontology:measurement(perovskite_self_etching_tr_t5, perovskite_self_etching, theater_ratio, 5, 0.12).
narrative_ontology:measurement(perovskite_self_etching_tr_t10, perovskite_self_etching, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(perovskite_self_etching_ex_t0, perovskite_self_etching, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(perovskite_self_etching_ex_t5, perovskite_self_etching, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(perovskite_self_etching_ex_t10, perovskite_self_etching, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Lithography is a foundational part of the global semiconductor infrastructure.
narrative_ontology:coordination_type(perovskite_self_etching, global_infrastructure).

% Network relationships (structural influence edges)
% The constraints on perovskite fabrication directly impact the broader
% semiconductor supply chain's ability to diversify.
narrative_ontology:affects_constraint(perovskite_self_etching, semiconductor_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */