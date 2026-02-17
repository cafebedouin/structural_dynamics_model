% ============================================================================
% CONSTRAINT STORY: openscholar_peer_review
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_openscholar_peer_review, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: openscholar_peer_review
 * human_readable: Traditional Academic Peer Review
 * domain: technological
 * * SUMMARY:
 * Traditional academic peer review, where experts assess research before publication, acts as a quality control mechanism. The OpenScholar AI model aims to replicate or augment this process, but the inherent extraction (time commitment, potential bias) and suppression (alternative publication venues discouraged) remain.
 * * KEY AGENTS:
 * - Researcher: Subject (Powerless)
 * - Journal Editor: Beneficiary (Institutional)
 * - Scientific Community: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(openscholar_peer_review, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(openscholar_peer_review, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(openscholar_peer_review, 0.30).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(openscholar_peer_review, extractiveness, 0.55).
narrative_ontology:constraint_metric(openscholar_peer_review, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(openscholar_peer_review, theater_ratio, 0.30).

% Constraint classification claim
narrative_ontology:constraint_claim(openscholar_peer_review, tangled_rope).
narrative_ontology:human_readable(openscholar_peer_review, "Traditional Academic Peer Review").

% Binary flags
% narrative_ontology:has_sunset_clause(openscholar_peer_review).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(openscholar_peer_review). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(openscholar_peer_review, academic_journals).
narrative_ontology:constraint_victim(openscholar_peer_review, researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE/MOUNTAIN)
% High extraction felt as an immutable limit or predatory trap.
% NOTE: This may be upgraded to 'organized' power if a critical mass of victims exists.
constraint_indexing:constraint_classification(openscholar_peer_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(openscholar_peer_review, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(openscholar_peer_review, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openscholar_peer_review_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(openscholar_peer_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openscholar_peer_review, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(openscholar_peer_review, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(openscholar_peer_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Researchers (powerless, trapped) feel the extraction of peer review as a snare because it requires significant time and effort with no guarantee of publication. Journal editors (institutional, mobile) see it as a necessary rope for maintaining quality and prestige. The analytical observer, considering the broader system, recognizes the tangled rope nature: it provides coordination (quality control) but also extracts from researchers and suppresses alternative publication methods. The perspectival gap arises because researchers directly experience the costs, while editors benefit from the system's stability.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling as pure extraction (Snare) by recognizing the coordination function of peer review – ensuring a baseline level of scientific rigor and preventing the unchecked proliferation of unsubstantiated claims. It prevents mislabeling as pure coordination (Rope) by acknowledging the inherent asymmetric power dynamics and extraction of labor from researchers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_openscholar_peer_review,
    'To what extent does peer review genuinely improve the quality of published research, versus merely filtering for conformity?',
    'Longitudinal studies comparing peer-reviewed publications to those published outside the system, controlling for methodology and impact.',
    'If peer review significantly improves quality, the constraint is more justifiable. If it mainly enforces conformity, the extraction becomes more problematic.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openscholar_peer_review, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(openscholar_peer_review_tr_t0, openscholar_peer_review, theater_ratio, 0, 0.20).
narrative_ontology:measurement(openscholar_peer_review_tr_t5, openscholar_peer_review, theater_ratio, 5, 0.30).
narrative_ontology:measurement(openscholar_peer_review_tr_t10, openscholar_peer_review, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(openscholar_peer_review_ex_t0, openscholar_peer_review, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(openscholar_peer_review_ex_t5, openscholar_peer_review, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(openscholar_peer_review_ex_t10, openscholar_peer_review, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */