% ============================================================================
% CONSTRAINT STORY: the_wall_procedural_barrier
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_the_wall_procedural_barrier, []).

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
 * * constraint_id: the_wall_procedural_barrier
 * human_readable: The Intelligence/Law Enforcement Information Sharing Barrier ("The Wall")
 * domain: legal/institutional
 * * SUMMARY:
 * "The Wall" refers to procedures restricting information sharing between
 * intelligence investigators (CIA/FBI) and criminal prosecutors (DOJ) in the US
 * from roughly 1995-2001. Initially intended to protect the legality of FISA
 * warrants, it became a pervasive, bureaucratic barrier that prevented the
 * connection of critical intelligence dots prior to the 9/11 attacks.
 * * KEY AGENTS:
 * - FBI Field Agent: Subject (Powerless), unable to get needed information.
 * - OIPR Gatekeeper: Beneficiary (Institutional), controlled FISA data flow.
 * - 9/11 Commission: Auditor (Analytical), identified the wall as a critical failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(the_wall_procedural_barrier, 0.80). % Snare extraction >= 0.46
domain_priors:suppression_score(the_wall_procedural_barrier, 0.90).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(the_wall_procedural_barrier, 0.10).       % Not a Piton (<= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(the_wall_procedural_barrier, extractiveness, 0.80).
narrative_ontology:constraint_metric(the_wall_procedural_barrier, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(the_wall_procedural_barrier, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claimed to be a necessary coordination mechanism to protect legal purity.
narrative_ontology:constraint_claim(the_wall_procedural_barrier, snare).
narrative_ontology:topic_domain(the_wall_procedural_barrier, "legal/institutional").
narrative_ontology:human_readable(the_wall_procedural_barrier, "The Intelligence/Law Enforcement Information Sharing Barrier (\"The Wall\")").

% Binary flags
domain_priors:requires_active_enforcement(the_wall_procedural_barrier).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(the_wall_procedural_barrier, institutional_gatekeepers).
narrative_ontology:constraint_victim(the_wall_procedural_barrier, national_security_agents).
narrative_ontology:constraint_victim(the_wall_procedural_barrier, intelligence_gap_affected_civilians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (FBI FIELD AGENT) - MOUNTAIN
% For the field agent, The Wall was an incomprehensible, immovable bureaucratic
% obstacle. It was a fact of their job they could not change or reason with,
% directly impeding their ability to act on information.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (OIPR GATEKEEPER) - ROPE
% From the perspective of the DOJ gatekeepers, "The Wall" was a 'Rope'—a necessary
% coordination tool to maintain the legal purity of criminal cases and prevent
% intelligence gathered under FISA from tainting prosecutions.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (9/11 COMMISSION) - SNARE
% With hindsight, the Commission saw the Wall as a 'Snare'. It was an extractive
% system that choked the flow of vital information, directly enabling the 9/11
% plot to succeed by preventing agents from connecting the dots.
constraint_indexing:constraint_classification(the_wall_procedural_barrier, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_wall_procedural_barrier_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == mountain,
    TypeInstitutional == rope.

test(analytical_classification_is_snare) :-
    constraint_indexing:constraint_classification(the_wall_procedural_barrier, snare, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(the_wall_procedural_barrier, ExtMetricName, E),
    E >= 0.46. % Ensures it meets the Snare threshold.

:- end_tests(the_wall_procedural_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that, while claiming to be a coordination mechanism
 * for legal purity (Rope), became a pathologically rigid barrier that destroyed
 * value (public safety) and suppressed functional alternatives.
 *
 * * Base Extractiveness (0.80): Extremely high. The constraint did not transfer
 *   value so much as destroy it, by extracting the ability of agents to perform
 *   their primary function, leading to catastrophic failure.
 * * Suppression (0.90): Near total. The procedures were actively enforced by
 *   institutional gatekeepers (DOJ's OIPR) who could deny FISA warrants,
 *   effectively suppressing any attempt at cross-functional collaboration.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. For the institutional gatekeeper, it was a 'Rope' ensuring
 * procedural correctness, a core part of their professional identity and power.
 * For the powerless field agent, it was an immovable 'Mountain' of bureaucracy.
 * The analytical view, with access to the tragic outcome, correctly identifies
 * it as a 'Snare' that actively prevented the system from working.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This is a classic case of Mandatrophy. A rule intended to solve one problem
 * (legal taint in prosecutions) created a far larger, fatal problem (intelligence
 * failure). The high extraction score is justified by the catastrophic destruction
 * of the system's primary goal (national security). The perspectival analysis
 * resolves the ambiguity by showing how the same object can be a tool to one
 * agent and a fatal trap to the system as a whole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_the_wall_procedural_barrier,
    'Was the barrier''s deadly rigidity derived from a strict interpretation of FISA law (Mountain) or from institutional risk-aversion within the DOJ/FBI (Snare)?',
    'A comparative legal audit of FISA vs. internal FBI/DOJ policy memos from the period.',
    'Determines if resolution required legislative change (changing a Mountain) or merely decisive leadership (cutting a Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(the_wall_procedural_barrier, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began with a defensible, low-extraction rationale (protecting
% prosecutions) but intensified over time into a rigid, highly extractive barrier
% as bureaucratic inertia and risk-aversion grew.
%
% Theater ratio over time (remains low; this was a functional, not performative, system):
narrative_ontology:measurement(twpb_tr_t0, the_wall_procedural_barrier, theater_ratio, 0, 0.10).
narrative_ontology:measurement(twpb_tr_t5, the_wall_procedural_barrier, theater_ratio, 5, 0.10).
narrative_ontology:measurement(twpb_tr_t10, the_wall_procedural_barrier, theater_ratio, 10, 0.10).

% Extraction over time (shows significant extraction_accumulation drift):
narrative_ontology:measurement(twpb_ex_t0, the_wall_procedural_barrier, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(twpb_ex_t5, the_wall_procedural_barrier, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(twpb_ex_t10, the_wall_procedural_barrier, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's claimed function was to enforce a legal procedure.
narrative_ontology:coordination_type(the_wall_procedural_barrier, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */