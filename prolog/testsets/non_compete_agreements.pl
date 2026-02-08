% ============================================================================
% CONSTRAINT STORY: non_compete_agreements
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_non_compete_agreements, []).

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
 * * constraint_id: non_compete_agreements
 * human_readable: Non-Compete Agreements (Post-Employment Restraint)
 * domain: economic/political
 * * SUMMARY:
 * A non-compete agreement is a legal contract where an employee agrees not to
 * enter into competition with an employer for a specified period and
 * geographic area after the employment relationship ends. It is a direct
 * legal restraint on labor mobility, claiming to protect trade secrets while
 * suppressing wages and worker mobility.
 * * KEY AGENTS:
 * - Transitioning Worker: Subject (Powerless), prevented from using their skills.
 * - Incumbent Corporation: Beneficiary (Institutional), protects market position.
 * - Regulator/Auditor: Analytical observer, weighs economic impacts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(non_compete_agreements, 0.80). % Snare extraction >= 0.46. Extracts future earning potential.
domain_priors:suppression_score(non_compete_agreements, 0.70).   % High suppression via legal "chilling effects".
domain_priors:theater_ratio(non_compete_agreements, 0.10).       % Low theater; these are actively and functionally enforced.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(non_compete_agreements, extractiveness, 0.80).
narrative_ontology:constraint_metric(non_compete_agreements, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(non_compete_agreements, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination tool to protect intellectual property.
narrative_ontology:constraint_claim(non_compete_agreements, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(non_compete_agreements). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(non_compete_agreements, incumbent_corporations).
narrative_ontology:constraint_victim(non_compete_agreements, transitioning_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE JUNIOR WORKER (SNARE)
% For a worker with limited savings, a non-compete is a trap. They cannot
% afford to fight it, and the threat of an injunction prevents them from
% taking a new job in their sector. It is a pure extraction of their right to work.
% χ = 0.80 * 1.5 (powerless) * 1.0 (national) = 1.20 (High Snare)
constraint_indexing:constraint_classification(non_compete_agreements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INCUMBENT FIRM (ROPE)
% To the corporate strategist, the agreement is a Rope. It coordinates the
% protection of the company's intellectual property and training investments,
% preventing them from being captured by competitors.
% χ = 0.80 * -0.2 (institutional) * 1.0 (national) = -0.16 (Felt as a benefit)
constraint_indexing:constraint_classification(non_compete_agreements, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (protecting legitimate trade
% secrets) and the asymmetric extraction (suppressing wages and mobility for
% workers who possess no secrets). It requires active enforcement and has clear
% winners and losers, making it a canonical Tangled Rope.
% χ = 0.80 * 1.15 (analytical) * 1.2 (global) = 1.104 (High Tangled Rope)
constraint_indexing:constraint_classification(non_compete_agreements, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(non_compete_agreements_tests).

test(perspectival_gap) :-
    % Verify the gap between the worker (Snare) and the firm (Rope).
    constraint_indexing:constraint_classification(non_compete_agreements, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(non_compete_agreements, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(non_compete_agreements, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three conditions for Tangled Rope are met.
    domain_priors:requires_active_enforcement(non_compete_agreements),
    narrative_ontology:constraint_beneficiary(non_compete_agreements, _), % Derives has_coordination_function
    narrative_ontology:constraint_victim(non_compete_agreements, _).     % Derives has_asymmetric_extraction

test(threshold_validation) :-
    % Verify this is correctly identified as a high-extraction constraint.
    narrative_ontology:constraint_metric(non_compete_agreements, extractiveness, E),
    E >= 0.46.

:- end_tests(non_compete_agreements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 0.80 because non-competes directly extract
 * future earning potential from workers, often without direct compensation for
 * this restriction. The suppression score of 0.70 reflects the "chilling effect"
 * of potential litigation, which prevents workers from even applying for jobs.
 *
 * The Perspectival Gap is stark:
 * - For the worker (powerless, trapped), it's a Snare that removes their agency.
 * - For the firm (institutional, mobile), it's a Rope that coordinates IP protection.
 * - The analytical view must reconcile these. Because the constraint has a
 *   legitimate coordination function (beneficiaries exist) AND severe asymmetric
 *   extraction (victims exist), and requires active legal enforcement, it is
 *   classified as a Tangled Rope. The original file's classification of
 *   "Mountain" was incorrect, as mountains must have very low extraction.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] By classifying this as a Tangled Rope, the system avoids
 * two errors. It doesn't dismiss the constraint as a pure Snare (ignoring the
 * coordination claims about IP) nor does it accept the "Rope" claim at face
 * value (ignoring the immense extraction from workers). The Tangled Rope
 * classification correctly identifies it as a hybrid instrument where a
 * coordination mechanism has been weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_non_compete_agreements,
    'Will federal bans on non-competes survive judicial review and be effectively enforced?',
    'Supreme Court rulings on administrative agency power and subsequent state-level enforcement data.',
    'If bans are upheld and enforced, the constraint degrades into a Piton (theatrically maintained). If struck down, it remains a powerful Tangled Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(non_compete_agreements, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the expansion of non-compete usage from high-level executives
% to lower-wage workers over the last decade, increasing its extractive nature.
% Theater ratio remains low as enforcement is functional.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(nca_tr_t0, non_compete_agreements, theater_ratio, 0, 0.10).
narrative_ontology:measurement(nca_tr_t5, non_compete_agreements, theater_ratio, 5, 0.10).
narrative_ontology:measurement(nca_tr_t10, non_compete_agreements, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(nca_ex_t0, non_compete_agreements, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(nca_ex_t5, non_compete_agreements, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(nca_ex_t10, non_compete_agreements, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: It's a legal tool used to enforce a specific outcome.
narrative_ontology:coordination_type(non_compete_agreements, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */