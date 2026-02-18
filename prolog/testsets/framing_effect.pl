% ============================================================================
% CONSTRAINT STORY: framing_effect
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_framing_effect, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: framing_effect
 * human_readable: The Framing Effect (as applied in choice architecture)
 * domain: social/political/technological
 * * SUMMARY:
 * The framing effect is a cognitive bias where people decide on options based on
 * whether the options are presented with positive or negative connotations (e.g., as a loss or as a gain).
 * While the bias itself is a feature of human cognition, this constraint models its active deployment
 * in social systems (marketing, politics) to engineer specific outcomes.
 * * KEY AGENTS:
 * - The Consumer/Voter: Subject (Powerless) whose choices are shaped by engineered frames.
 * - The Campaign Strategist/Marketer: Beneficiary (Institutional) who uses framing as a tool to coordinate behavior.
 * - The Systems Auditor: Auditor (Analytical) who observes the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(framing_effect, 0.55). % Extracts rational agency and economic value.
domain_priors:suppression_score(framing_effect, 0.50).   % Suppresses neutrally-presented information.
domain_priors:theater_ratio(framing_effect, 0.15).       % The application is highly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(framing_effect, extractiveness, 0.55).
narrative_ontology:constraint_metric(framing_effect, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(framing_effect, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Beneficiaries claim it's a tool for effective communication (coordination).
% The system sees it as a constructed mechanism.
narrative_ontology:constraint_claim(framing_effect, tangled_rope).
narrative_ontology:human_readable(framing_effect, "The Framing Effect (as applied in choice architecture)").
narrative_ontology:topic_domain(framing_effect, "social/political/technological").

% Binary flags
% The *application* of the bias requires active enforcement of the message/frame.
domain_priors:requires_active_enforcement(framing_effect).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(framing_effect, marketers_and_strategists).
narrative_ontology:constraint_victim(framing_effect, consumers_and_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the individual, a deceptive frame is a Snare. It extracts their wealth or
% autonomy by manipulating their intuitive judgments.
constraint_indexing:constraint_classification(framing_effect, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the marketer or strategist, framing is a Rope. It is a coordination
% mechanism used to "pull" collective behavior toward a desired outcome.
constraint_indexing:constraint_classification(framing_effect, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the systemic application of framing is a Tangled Rope. It has a
% genuine coordination function for its architects but relies on asymmetric
% extraction from its targets and requires active enforcement (messaging campaigns)
% to maintain the desired frame.
constraint_indexing:constraint_classification(framing_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(framing_effect_tests).

test(perspectival_gap_snare_to_rope) :-
    % Verify the gap between the powerless (Snare) and institutional (Rope) views.
    constraint_indexing:constraint_classification(framing_effect, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(framing_effect, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(framing_effect, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % A Tangled Rope requires all three structural properties to be present.
    narrative_ontology:constraint_beneficiary(framing_effect, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(framing_effect, _),       % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(framing_effect).

:- end_tests(framing_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core modeling decision is to distinguish the cognitive bias itself (a potential Mountain)
 * from its systemic application (a Tangled Rope). This file models the latter.
 * The base extractiveness of 0.55 reflects the significant transfer of value (economic, political)
 * from the victim to the beneficiary. The suppression score of 0.50 reflects how a dominant
 * frame actively suppresses consideration of neutral or alternative presentations of the same data.
 *
 * The Perspectival Gap is stark:
 * - The 'powerless' consumer, whose rational agency is extracted, perceives a Snare.
 * - The 'institutional' marketer, who uses it to align consumer behavior, perceives a Rope.
 * - The 'analytical' observer sees both functions operating simultaneously, which is the definition of a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a pure Snare would be a mandatrophy error, as it would ignore the genuine
 * (albeit self-serving) coordination function that framing provides for institutional actors.
 * The Tangled Rope classification correctly captures this duality, preventing the system from
 * oversimplifying the mechanism as pure predation. It acknowledges that the tool works for coordination
 * precisely *because* it extracts agency from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_framing_effect,
    'Is the underlying cognitive bias a hard Mountain of neurology, or a malleable Rope of culture that could be untangled with training?',
    'Long-term studies of expert decision-makers trained in cognitive bias mitigation versus control groups in high-stakes environments.',
    'If Mountain, protection requires external regulation of choice architecture. If Rope, protection can be achieved through education and debiasing techniques.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(framing_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The application of framing has been a consistent tool in marketing and politics.
% The metrics are modeled as stable over the observed interval.
% Extraction > 0.46 requires temporal data.

% Theater ratio over time:
narrative_ontology:measurement(framing_effect_tr_t0, framing_effect, theater_ratio, 0, 0.15).
narrative_ontology:measurement(framing_effect_tr_t5, framing_effect, theater_ratio, 5, 0.15).
narrative_ontology:measurement(framing_effect_tr_t10, framing_effect, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(framing_effect_ex_t0, framing_effect, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(framing_effect_ex_t5, framing_effect, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(framing_effect_ex_t10, framing_effect, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Framing is used to standardize how information is interpreted to guide behavior.
narrative_ontology:coordination_type(framing_effect, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */