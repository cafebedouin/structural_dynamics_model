% ============================================================================
% CONSTRAINT STORY: cloudflare_dual_class_asymmetry
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_cloudflare_dual_class_asymmetry, []).

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
 * * constraint_id: cloudflare_dual_class_asymmetry
 * human_readable: Cloudflare Dual-Class Voting Control
 * domain: economic
 * * SUMMARY:
 * Cloudflare's S-1 details a dual-class stock structure where Class B shares
 * carry 10 votes per share, while the offered Class A shares carry only
 * one vote. This mechanism ensures that the founders and pre-IPO
 * insiders retain overwhelming voting power, effectively insulating
 * management from public shareholder influence while raising public capital.
 * * KEY AGENTS:
 * - Public Shareholders: Subject (Powerless)
 * - Founders & Insiders: Beneficiary (Institutional)
 * - Corporate Governance Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cloudflare_dual_class_asymmetry, 0.80). % Snare extraction >= 0.46
domain_priors:suppression_score(cloudflare_dual_class_asymmetry, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cloudflare_dual_class_asymmetry, 0.10).       % Low theater; this is a functional, not performative, system.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, extractiveness, 0.80).
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(cloudflare_dual_class_asymmetry, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It is presented as a necessary legal structure for long-term stability.
narrative_ontology:constraint_claim(cloudflare_dual_class_asymmetry, tangled_rope).
narrative_ontology:human_readable(cloudflare_dual_class_asymmetry, "Cloudflare Dual-Class Voting Control").

% Binary flags
domain_priors:requires_active_enforcement(cloudflare_dual_class_asymmetry). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(cloudflare_dual_class_asymmetry, founders_and_insiders).
narrative_ontology:constraint_victim(cloudflare_dual_class_asymmetry, public_shareholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE PUBLIC SHAREHOLDER (SNARE)
% They provide capital but are trapped with no governance rights.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE FOUNDERS (ROPE)
% Viewed as an essential coordination tool to protect long-term vision from
% short-term market pressures after going public.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system has a genuine coordination function (for founders) but also
% imposes severe, asymmetric extraction (on public shareholders) and requires
% active legal enforcement. This is the canonical signature of a Tangled Rope.
constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cloudflare_dual_class_asymmetry_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(cloudflare_dual_class_asymmetry, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(cloudflare_dual_class_asymmetry, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(cloudflare_dual_class_asymmetry, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(cloudflare_dual_class_asymmetry).

:- end_tests(cloudflare_dual_class_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original file misclassified the analytical perspective as a 'Mountain'.
 * A Mountain must have very low extraction and suppression. This system has
 * extremely high values for both (0.80 and 0.90). The correct analytical
 * classification is 'Tangled Rope' because it meets all three canonical
 * requirements:
 * 1. It has a genuine coordination function (a 'Rope' for founders protecting their vision).
 * 2. It has asymmetric extraction (a 'Snare' for public shareholders with no voting rights).
 * 3. It requires active legal enforcement (via corporate charter and Delaware law).
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.80) triggers mandatrophy. The system resolves this by
 * correctly identifying the structure as a Tangled Rope. This prevents the
 * misclassification of the founders' coordination tool as either pure altruism
 * (Rope) or pure predation (Snare). It is a hybrid system where coordination
 * for one group is enabled by the extraction from another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cloudflare_fiduciary_duty,
    'Will Delaware courts uphold the partnership right to waive fiduciary duties, or will a legal Mountain emerge to protect minority investors?',
    'Monitor subsequent litigation in Delaware Chancery Court regarding limited partnership agreements.',
    'If duties are waived, the Snare aspect intensifies. If not, a weak Rope-like protection emerges for investors.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cloudflare_dual_class_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This structure is established at IPO and is designed to be stable.
% The trajectory is flat, reflecting the durable nature of the legal arrangement.
%
% Theater ratio over time:
narrative_ontology:measurement(cf_tr_t0, cloudflare_dual_class_asymmetry, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cf_tr_t5, cloudflare_dual_class_asymmetry, theater_ratio, 5, 0.10).
narrative_ontology:measurement(cf_tr_t10, cloudflare_dual_class_asymmetry, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(cf_ex_t0, cloudflare_dual_class_asymmetry, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(cf_ex_t5, cloudflare_dual_class_asymmetry, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(cf_ex_t10, cloudflare_dual_class_asymmetry, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The dual-class structure is a mechanism for allocating voting power, a key resource.
narrative_ontology:coordination_type(cloudflare_dual_class_asymmetry, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */