% ============================================================================
% CONSTRAINT STORY: hanlons_razor
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-09
% ============================================================================

:- module(constraint_hanlons_razor, []).

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
 * * constraint_id: hanlons_razor
 * human_readable: Hanlon's Razor ("Never attribute to malice that which is adequately explained by stupidity.")
 * domain: social/cognitive
 * * SUMMARY:
 * Hanlon's Razor is a heuristic that constrains how we model the intentions of others. It suggests incompetence is a more frequent cause of negative outcomes than coordinated malevolence. While it serves a coordination function by reducing social friction and preventing cycles of retaliation, it can also be used by institutions to extract accountability, framing systemic harm as isolated, unavoidable mistakes.
 * * KEY AGENTS:
 * - The Marginalized Victim: Subject to systemic harm framed as "incompetence." (Powerless)
 * - The Bureaucracy: Uses the razor to manage public relations and deflect accountability. (Institutional)
 * - The Systems Analyst: Observes the dual function of the heuristic. (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(hanlons_razor, 0.55). % Mountain <= 0.15, Rope <= 0.15, Snare/Tangled Rope >= 0.46
domain_priors:suppression_score(hanlons_razor, 0.40).   % Structural property (raw, unscaled). The razor suppresses malice-based hypotheses.
domain_priors:theater_ratio(hanlons_razor, 0.10).       % Piton detection (>= 0.70). The heuristic is primarily functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(hanlons_razor, extractiveness, 0.55).
narrative_ontology:constraint_metric(hanlons_razor, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(hanlons_razor, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a tool for rational coordination.
narrative_ontology:constraint_claim(hanlons_razor, tangled_rope).
narrative_ontology:topic_domain(hanlons_razor, "social/cognitive").
narrative_ontology:human_readable(hanlons_razor, "Hanlon's Razor (\"Never attribute to malice that which is adequately explained by stupidity.\")").

% Binary flags
domain_priors:requires_active_enforcement(hanlons_razor). % Required for Tangled Rope. Enforcement is social/cultural pressure.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(hanlons_razor, incompetent_actors).
narrative_ontology:constraint_victim(hanlons_razor, victims_of_systemic_neglect).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a victim of systemic harm, the razor is a Snare. It invalidates their
% experience and strangles their ability to demand accountability by reframing
% systemic malice or neglect as a series of isolated, forgivable "mistakes."
% χ = 0.55 * 1.5 (powerless) * 1.0 (national) = 0.825
constraint_indexing:constraint_classification(hanlons_razor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For an institution, the razor is a Rope. It's a powerful public relations tool
% to coordinate a narrative of "unfortunate error" rather than "systemic failure,"
% preserving institutional legitimacy and avoiding costly reforms.
% χ = 0.55 * -0.2 (institutional) * 1.0 (national) = -0.11 (felt as a benefit)
constraint_indexing:constraint_classification(hanlons_razor, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both sides. The razor is a coordination tool (Rope) that
% reduces social friction, but it also enables asymmetric extraction of
% accountability (Snare), making it a classic Tangled Rope.
% χ = 0.55 * 1.15 (analytical) * 1.2 (global) = 0.759
constraint_indexing:constraint_classification(hanlons_razor, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanlons_razor_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(hanlons_razor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanlons_razor, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    % Verify the base extraction is in the high-extraction range for a Tangled Rope.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hanlons_razor, ExtMetricName, E),
    E >= 0.46.

test(tangled_rope_structural_properties) :-
    % Verify the necessary structural properties for a Tangled Rope are present.
    domain_priors:requires_active_enforcement(hanlons_razor),
    narrative_ontology:constraint_beneficiary(hanlons_razor, _),
    narrative_ontology:constraint_victim(hanlons_razor, _).

:- end_tests(hanlons_razor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is set to 0.55 to reflect the significant value of
 * "accountability" that is extracted from victims and transferred to institutions.
 * When a systemic failure causing harm is framed as "stupidity," the institution
 * avoids the cost of reform, litigation, and loss of legitimacy. This high
 * extraction is what creates the sharp perspectival gap: the institution sees
 * a useful coordination tool (Rope), while the victim experiences a trap that
 * denies them justice (Snare). The suppression score of 0.4 reflects the
 * heuristic's power in making alternative explanations (i.e., systemic malice
 * or deliberate neglect) seem paranoid or unreasonable.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope is critical. A naive analysis might see
 * only the Snare aspect (harm to victims) or only the Rope aspect (social
 * lubrication). The Tangled Rope classification correctly identifies that the
 * constraint's power comes from its dual nature: the socially useful coordination
 * function provides plausible deniability for its extractive function. This
 * prevents mislabeling the entire heuristic as pure extraction, acknowledging
 * its genuine, if weaponized, coordination role.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hanlons_razor,
    "Is there an inherent, information-theoretic limit to distinguishing high-entropy incompetence from low-signal, systemic malice?",
    "Analysis of institutional failure modes across domains (e.g., finance, engineering, policy) to find statistical signatures of intent vs. incompetence.",
    "If a limit exists, the constraint has a Mountain-like core. If not, it is a purely constructed Tangled Rope that can be dismantled with better analytical tools.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hanlons_razor, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint shows how the heuristic,
% once a simple observation, became an institutionalized tool for evading
% accountability. Extraction accumulates as its use becomes more strategic.

% Theater ratio over time (slight increase as it becomes a performative defense):
narrative_ontology:measurement(hanlons_razor_tr_t0, hanlons_razor, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hanlons_razor_tr_t5, hanlons_razor, theater_ratio, 5, 0.08).
narrative_ontology:measurement(hanlons_razor_tr_t10, hanlons_razor, theater_ratio, 10, 0.10).

% Extraction over time (increases as institutions learn to weaponize it):
narrative_ontology:measurement(hanlons_razor_ex_t0, hanlons_razor, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(hanlons_razor_ex_t5, hanlons_razor, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(hanlons_razor_ex_t10, hanlons_razor, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Hanlon's Razor functions as a standard for interpreting information about intent.
narrative_ontology:coordination_type(hanlons_razor, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */