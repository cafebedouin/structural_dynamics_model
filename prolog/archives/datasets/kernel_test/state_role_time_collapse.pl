% ============================================================================
% CONSTRAINT STORY: state_role_time_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_role_time_collapse, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_role_time_collapse
 *   human_readable: State-Role-Time Collapse in Chat-Based Interfaces
 *   domain: human_computer_interaction/cognitive_ergonomics
 *
 * SUMMARY:
 *   The chat interface for AI interaction collapses three structural
 *   dimensions that traditional software development environments keep
 *   separate: state (no versioning, no artifact boundaries, no persistent
 *   working directory), roles (brainstorming, drafting, debugging, and
 *   auditing are undifferentiated in a single conversational stream), and
 *   time (no persistent trajectory across sessions, no branching history).
 *   This collapse is presented as inherent to conversational AI but is
 *   actually a design choice optimizing for cognitive immediacy and adoption
 *   simplicity. The constraint exhibits mountain classification from all
 *   perspectives because the tradeoff appears fundamental to the chat
 *   metaphor itself — adding the missing structure would transform the
 *   interface into something that is no longer 'chat.' However, the omega
 *   variables identify empirical questions that could reveal this mountain as
 *   contingent: hybrid interfaces might preserve conversational flow while
 *   layering in state/role/time structure, or the adoption advantage might be
 *   separable from the collapse itself. The very low extractiveness (0.08)
 *   reflects that the collapse is not primarily an extraction mechanism — it
 *   is a genuine design tradeoff where simplicity is purchased at the cost of
 *   structure. Users who need versioning, role separation, or persistent
 *   state can exit to traditional development environments; the chat
 *   interface does not suppress these alternatives.
 *
 * KEY AGENTS:
 *   - Novice User: Powerless/trapped — perceives chat as the only modality; no awareness of alternatives
 *   - Professional User: Moderate/constrained — recognizes limitations but sees them as inherent to chat; works around via external tools
 *   - Platform Provider: Institutional/mobile — could build additional features but faces architectural constraints; the collapse is load-bearing for adoption
 *   - HCI Researcher: Analytical/analytical — sees the collapse as a fundamental tradeoff in interface design space, not a contingent implementation choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_role_time_collapse, 0.08).
domain_priors:suppression_score(state_role_time_collapse, 0.03).
domain_priors:theater_ratio(state_role_time_collapse, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_role_time_collapse, extractiveness, 0.08).
narrative_ontology:constraint_metric(state_role_time_collapse, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(state_role_time_collapse, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_role_time_collapse, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(state_role_time_collapse, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_role_time_collapse, mountain).
narrative_ontology:human_readable(state_role_time_collapse, "State-Role-Time Collapse in Chat-Based Interfaces").
narrative_ontology:topic_domain(state_role_time_collapse, "human_computer_interaction/cognitive_ergonomics").

domain_priors:emerges_naturally(state_role_time_collapse).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVICE USER (MOUNTAIN) — Experiences the chat interface as the only available modality. No awareness of alternative interaction paradigms (versioning, role separation, persistent state). The collapse is perceived as inherent to computer interaction itself.
constraint_indexing:constraint_classification(state_role_time_collapse, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PROFESSIONAL USER (MOUNTAIN) — Recognizes the limitations but perceives them as fundamental to conversational AI architecture. Can work around the constraint through external tooling (copy-paste to version control, manual role switching) but sees the collapse itself as unchangeable within the chat paradigm.
constraint_indexing:constraint_classification(state_role_time_collapse, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM PROVIDER (MOUNTAIN) — Could theoretically build versioning/role/state features but faces architectural constraints: the chat metaphor's simplicity is load-bearing for adoption; adding state management reintroduces complexity the chat interface was designed to eliminate. The collapse is a design tradeoff, not pure extraction.
constraint_indexing:constraint_classification(state_role_time_collapse, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — The state-role-time collapse is a structural property of the conversational interface metaphor. Chat is inherently sequential and stateless by design — it optimizes for cognitive immediacy at the cost of structural memory. This is not a contingent implementation choice but a fundamental tradeoff in interface design space. Alternative paradigms (IDEs, version control systems, multi-agent frameworks) exist but solve different problems.
constraint_indexing:constraint_classification(state_role_time_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_role_time_collapse_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(state_role_time_collapse, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_role_time_collapse, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_role_time_collapse, ExtMetricName, E),
    domain_priors:suppression_score(state_role_time_collapse, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_role_time_collapse),
    narrative_ontology:constraint_metric(state_role_time_collapse, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_role_time_collapse, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_role_time_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The state-role-time collapse is not primarily extractive — it is a design tradeoff. Users lose structure but gain immediacy and simplicity. The small extractive component reflects opportunity cost: users who would benefit from versioning/role/state features must use external tools or alternative interfaces, creating friction. But this is not suppression of alternatives (users can trivially switch to IDEs, version control, multi-agent frameworks) — it is the inherent limitation of optimizing for one set of affordances over another. Suppression (0.03): Negligible. The chat interface does not suppress alternative interaction paradigms. Users can exit to traditional development environments at any time. The constraint is the chat metaphor's internal structure, not a barrier preventing access to other tools. Accessibility collapse (0.92): Very high. Once a user adopts the chat paradigm for a given task, the state-role-time collapse is unavoidable within that paradigm. There is no way to 'add versioning to chat' without fundamentally changing what chat is. Resistance (0.08): Very low. The collapse emerges naturally from the conversational metaphor's design logic. It is not maintained by active enforcement or institutional pressure — it is a structural property of sequential, stateless interaction. Theater ratio (0.12): Very low. The chat interface is functionally what it claims to be: a conversational interaction modality. The collapse is not performative or vestigial — it is the actual design.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify as mountain, which is the expected pattern for a genuine natural law or structural limit. The novice user sees the collapse as inherent to computers. The professional user sees it as inherent to chat. The platform provider sees it as an architectural constraint. The analytical observer sees it as a fundamental tradeoff in design space. The uniformity across perspectives is diagnostic: when a constraint appears immutable from every structural position, it is either a true natural law or a very successful naturalization. The omega variables test which: if hybrid interfaces prove viable, the mountain was a false summit; if they fail, the tradeoff is genuine.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the constraint is a genuine design tradeoff, not an extraction mechanism. The chat interface optimizes for cognitive immediacy and low learning curve by collapsing state/role/time structure. Users who need that structure can exit to alternative tools. The platform provider is not extracting rent by withholding features — they are making a coherent design choice where adding the missing structure would destroy the simplicity that makes chat valuable. The analytical observer sees this as a fundamental tradeoff in interface design space: you cannot have both the immediacy of conversation and the structure of a development environment in the same interaction paradigm. The omega variables identify the empirical questions that could falsify this mountain classification (hybrid interfaces, separable adoption factors), but current evidence supports the tradeoff as structural.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy because it is not primarily extractive. The state-role-time collapse is a design tradeoff, not a mechanism for asymmetric benefit. The very low extractiveness (0.08) and negligible suppression (0.03) reflect that users who need the missing structure can exit to alternative tools without significant cost. The mountain classification is uniform across perspectives because the constraint is a structural property of the chat metaphor itself, not a contingent institutional arrangement. The analytical challenge is distinguishing this genuine mountain from a false summit — hence the omega variables testing whether the tradeoff is truly necessary or merely conventional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphor_necessity,
    'Is the chat metaphor''s state collapse a necessary consequence of conversational interaction, or could a hybrid interface preserve conversational flow while maintaining state/role/time structure?',
    'Empirical testing of hybrid interfaces that layer versioning/role-switching onto chat; measurement of cognitive load, task completion rates, and user preference across paradigms',
    'If hybrid interfaces prove viable without cognitive overhead: the collapse is a contingent design choice (Rope/Scaffold from some perspectives). If hybrids introduce prohibitive complexity: the collapse is genuinely structural (Mountain universally).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphor_necessity, empirical, 'Whether chat metaphor necessitates state-role-time collapse').

omega_variable(
    adoption_barrier_source,
    'Does the chat interface''s adoption advantage derive from the state collapse itself (simplicity as feature) or from other factors (natural language, low learning curve) that could be preserved in a more structured interface?',
    'User studies comparing adoption rates and learning curves for chat vs structured-but-conversational interfaces; analysis of which specific features drive adoption',
    'If adoption advantage is separable from state collapse: providers could add structure without losing users (constraint becomes Rope). If inseparable: the collapse is load-bearing for the interface''s value proposition (Mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adoption_barrier_source, empirical, 'Whether state collapse is load-bearing for chat adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_role_time_collapse, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(srtc_tr_t0, state_role_time_collapse, theater_ratio, 0, 0.1).
narrative_ontology:measurement(srtc_tr_t3, state_role_time_collapse, theater_ratio, 3, 0.11).
narrative_ontology:measurement(srtc_tr_t6, state_role_time_collapse, theater_ratio, 6, 0.12).

% Extraction over time
narrative_ontology:measurement(srtc_be_t0, state_role_time_collapse, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(srtc_be_t3, state_role_time_collapse, base_extractiveness, 3, 0.075).
narrative_ontology:measurement(srtc_be_t6, state_role_time_collapse, base_extractiveness, 6, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_role_time_collapse, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a single structural claim: the chat metaphor collapses state/role/time dimensions. No decomposition is needed because the observable (presence/absence of versioning, role differentiation, session persistence) yields a stable epsilon value regardless of measurement approach. Alternative interface paradigms (IDEs, version control, multi-agent frameworks) are not different measurements of this constraint — they are different constraints entirely, solving different problems in different design spaces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
