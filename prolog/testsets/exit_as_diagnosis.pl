% ============================================================================
% CONSTRAINT STORY: exit_as_diagnosis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exit_as_diagnosis, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exit_as_diagnosis
 *   human_readable: Exit as Diagnostic Signal in Organizational Legitimacy
 *   domain: political_philosophy/organizational_theory/ethics_of_agency
 *
 * SUMMARY:
 *   Exit-as-diagnosis is the structural constraint that makes departure a
 *   more complete form of refusal than argument, because exit withdraws
 *   legitimacy rather than contesting terms. This constraint sits downstream
 *   of two others: exit_cost_asymmetry (the mountain-level structural fact
 *   that exit costs differ by power and mobility) and voice_without_exit (the
 *   tangled rope where voice mechanisms exist but lack enforcement power).
 *   The diagnostic value of exit depends on the asymmetry: if exit were
 *   costless for all actors, it would provide no information (everyone would
 *   exit at the first disagreement). If exit were impossible for all actors,
 *   voice would be the only mechanism. The constraint extracts by making exit
 *   costly for some actors while preserving it as a low-cost option for
 *   others, which creates adverse selection — the actors most capable of
 *   reform leave, while the actors most harmed by dysfunction stay. The
 *   theater_ratio (0.38) reflects that voice mechanisms (grievance
 *   procedures, feedback surveys, town halls) are partly performative: they
 *   exist to absorb dissent and create the appearance of responsiveness, but
 *   they rarely enable structural change. Exit bypasses this theater by
 *   withdrawing participation entirely, which makes it diagnostic — but only
 *   for those who can afford it.
 *
 * KEY AGENTS:
 *   - Exit-Constrained Actors: Primary victims (powerless/trapped) — economic dependency, visa status, caregiving obligations, or geographic isolation make exit prohibitively costly; continued presence is legible as consent when it is actually captivity
 *   - Exit-Capable Actors: Primary beneficiaries (powerful/arbitrage) — high-skill professionals with portable credentials and competitive offers; exit is a low-cost diagnostic signal that efficiently communicates irreconcilable disagreement
 *   - Voice-Exhausted Employees: Secondary victims (moderate/constrained) — have attempted internal reform through voice mechanisms; exit becomes diagnostic only after voice fails, revealing that voice was theater
 *   - Source Organization: Mixed position (institutional/constrained) — receives diagnostic information from exit but bears cost of talent loss and legitimacy damage; adverse selection concentrates exit-constrained actors
 *   - Receiving Organization: Secondary beneficiary (institutional/arbitrage) — receives pre-filtered talent that has performed costly signaling through exit
 *   - Labor Organizing Coalition: Organized agents (organized/mobile) — building infrastructure (union contracts, sectoral bargaining, benefits portability) to reduce exit cost asymmetry and restore voice credibility
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the exit-voice tradeoff as an immutable organizational law when it is actually a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exit_as_diagnosis, 0.48).
domain_priors:suppression_score(exit_as_diagnosis, 0.52).
domain_priors:theater_ratio(exit_as_diagnosis, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exit_as_diagnosis, extractiveness, 0.48).
narrative_ontology:constraint_metric(exit_as_diagnosis, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(exit_as_diagnosis, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exit_as_diagnosis, tangled_rope).
narrative_ontology:human_readable(exit_as_diagnosis, "Exit as Diagnostic Signal in Organizational Legitimacy").
narrative_ontology:topic_domain(exit_as_diagnosis, "political_philosophy/organizational_theory/ethics_of_agency").

domain_priors:requires_active_enforcement(exit_as_diagnosis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exit_as_diagnosis, exit_capable_actors).
narrative_ontology:constraint_beneficiary(exit_as_diagnosis, receiving_organizations).
narrative_ontology:constraint_victim(exit_as_diagnosis, exit_constrained_actors).
narrative_ontology:constraint_victim(exit_as_diagnosis, source_organization_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXIT-CONSTRAINED ACTOR (SNARE) — Trapped by economic dependency, visa status, caregiving obligations, or geographic isolation. Sees exit as diagnosis but cannot exercise it. The constraint extracts by making their continued presence legible as consent when it is actually structural captivity. Maximum experienced extraction — the diagnostic signal is unavailable to those who need it most.
constraint_indexing:constraint_classification(exit_as_diagnosis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: VOICE-EXHAUSTED EMPLOYEE (TANGLED ROPE) — Has filed grievances, participated in feedback mechanisms, attempted internal reform. Exit becomes diagnostic only after voice fails — the timing reveals that voice was theater. Experiences both coordination (exit provides information about organizational dysfunction) and extraction (the cost of exit is high, and staying signals false consent). The constraint coordinates legitimate information flow but extracts through the asymmetry between exit cost and voice efficacy.
constraint_indexing:constraint_classification(exit_as_diagnosis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXIT-CAPABLE PROFESSIONAL (ROPE) — High-skill worker with portable credentials and competitive offers. Exit is a low-cost diagnostic signal that efficiently communicates irreconcilable disagreement. The constraint coordinates: exit provides clean information to both parties (the organization learns the problem is unfixable; the professional avoids prolonged conflict). Net beneficiary — extraction runs toward this agent through their ability to withdraw legitimacy costlessly.
constraint_indexing:constraint_classification(exit_as_diagnosis, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR ORGANIZING COALITION (SCAFFOLD) — Organized labor sees exit-as-diagnosis as a temporary coordination problem with a sunset: collective bargaining, just-cause termination protections, and portable benefits reduce exit cost asymmetry. When exit costs equalize, the diagnostic signal becomes symmetric — both parties can exit at similar cost, which restores voice credibility. The coalition is building the infrastructure (union contracts, sectoral bargaining, benefits portability) that makes exit a genuine coordination mechanism rather than an extraction vector. Estimated sunset: 15-25 years for labor law reform to restore exit symmetry in key sectors.
constraint_indexing:constraint_classification(exit_as_diagnosis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RECEIVING ORGANIZATION (ROPE) — Benefits from exit-as-diagnosis by receiving talent that has already performed costly signaling. An employee who exits rather than staying and complaining has revealed high conviction and low tolerance for dysfunction. The receiving organization gets pre-filtered candidates. Experiences the constraint as coordination — exit provides valuable information at low cost to the receiver.
constraint_indexing:constraint_classification(exit_as_diagnosis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SOURCE ORGANIZATION (TANGLED ROPE) — Loses talent and receives diagnostic information simultaneously. Exit reveals problems that voice did not surface (either because voice was ignored or because the problem is unfixable within current constraints). The organization benefits from the information (coordination function) but bears the cost of talent loss and legitimacy damage (extraction). The constraint is tangled: exit provides genuine diagnostic value but also punishes the organization asymmetrically when exit-capable actors leave and exit-constrained actors stay, creating adverse selection.
constraint_indexing:constraint_classification(exit_as_diagnosis, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / HIRSCHMAN NATURALIZATION (MOUNTAIN) — From a civilizational perspective, the exit-voice-loyalty tradeoff appears as an immutable feature of organizational life: exit and voice are substitute mechanisms for expressing dissent, and the choice between them is determined by cost structure and preference. This perspective naturalizes the constraint as a law of organizational behavior. However, the structural data contradicts the mountain classification — exit cost asymmetry is a contingent institutional arrangement (at-will employment, non-portable benefits, non-compete clauses), not a natural law. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(exit_as_diagnosis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exit_as_diagnosis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exit_as_diagnosis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exit_as_diagnosis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exit_as_diagnosis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exit_as_diagnosis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts by making exit costly for some actors while preserving it as a low-cost option for others. Exit-capable actors experience exit as coordination (clean information flow), while exit-constrained actors experience it as extraction (their continued presence is misread as consent). The asymmetry creates adverse selection: the actors most capable of reform leave, while the actors most harmed by dysfunction stay. This is not pure extraction (the diagnostic signal does provide genuine information), but the extraction is significant and structural. Suppression (0.52): Moderate-high. Exit-constrained actors face economic dependency, visa status requirements, caregiving obligations, non-compete clauses, and non-portable benefits. Voice mechanisms exist but are partly theatrical — they absorb dissent without enabling change. The suppression is not total (some actors can exit, and some voice mechanisms do work), but it is substantial. Theater ratio (0.38): Moderate. Voice mechanisms (grievance procedures, feedback surveys, town halls) are partly performative. They exist to create the appearance of responsiveness and to absorb dissent, but they rarely enable structural change. Exit bypasses this theater by withdrawing participation entirely, which makes it diagnostic. The theater has increased over the interval as organizations have professionalized feedback mechanisms without increasing their responsiveness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — exit as a diagnostic signal — appears as coordination, extraction, or a temporary problem depending on the observer's position. Exit-capable actors see coordination (Rope) — exit provides clean information at low cost. Exit-constrained actors see extraction (Snare) — their inability to exit makes their continued presence legible as consent when it is actually captivity. Voice-exhausted employees see mixed coordination and extraction (Tangled Rope) — exit provides information but only after costly voice failure. The source organization sees Tangled Rope from a different angle — it receives diagnostic information but bears talent loss and adverse selection. The labor organizing coalition sees a temporary problem with a sunset (Scaffold) — collective bargaining and benefits portability will reduce exit cost asymmetry. The analytical observer risks seeing an immutable law (Mountain) — the exit-voice tradeoff is naturalized as organizational physics — but the structural data reveals this as a false summit: exit cost asymmetry is a contingent institutional arrangement (at-will employment, non-portable benefits, non-compete clauses), not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Exit-capable actors are beneficiaries: they experience low exit costs and can withdraw legitimacy costlessly, which gives them structural power. The engine derives low d (beneficiary + arbitrage exit) → low/negative chi. Exit-constrained actors are victims: they bear high exit costs and cannot exercise the diagnostic signal, which makes their continued presence legible as consent. The engine derives high d (victim + trapped exit) → high chi. Voice-exhausted employees are victims with constrained exit: they have attempted voice and failed, which reveals that voice was theater. The engine derives moderate-high d (victim + constrained exit) → moderate-high chi. The source organization is a victim with constrained exit: it loses talent and receives diagnostic information simultaneously, experiencing both coordination and extraction. The receiving organization is a beneficiary with arbitrage exit: it receives pre-filtered talent at low cost. The labor organizing coalition has mobile exit and sees a sunset: it is building infrastructure to reduce exit cost asymmetry. The analytical observer risks naturalizing the constraint as a mountain, but the structural data (contingent institutional arrangements) contradicts this.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that exit-as-diagnosis is simultaneously a coordination mechanism (it provides genuine information about irreconcilable disagreement) and an extraction mechanism (it operates asymmetrically, concentrating costs on exit-constrained actors and benefits on exit-capable actors). The coordination function is real: exit bypasses voice theater and provides clean information. The extraction function is also real: the asymmetry creates adverse selection and makes continued presence legible as consent when it is actually captivity. The tangled rope classification captures both: exit coordinates information flow AND extracts through cost asymmetry. The perspectival gap is diagnostic: exit-capable actors see coordination (their experience is genuine), exit-constrained actors see extraction (their experience is also genuine), and the analytical observer risks naturalizing the asymmetry as an immutable organizational law when it is actually a contingent institutional arrangement that labor organizing is actively working to dismantle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_timing_threshold,
    'How long must voice fail before exit becomes diagnostic rather than premature?',
    'Longitudinal analysis of exit timing relative to grievance filing; correlation between voice duration and post-exit organizational reform',
    'If threshold is short (< 6 months): exit may precede good-faith voice, reducing diagnostic value. If threshold is long (> 3 years): exit-constrained actors are trapped in prolonged voice theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_timing_threshold, empirical, 'Voice duration threshold before exit becomes diagnostic').

omega_variable(
    adverse_selection_severity,
    'Does exit-as-diagnosis create adverse selection where only exit-capable actors leave, concentrating exit-constrained actors in dysfunctional organizations?',
    'Comparison of exit rates by skill portability and economic dependency; measurement of organizational dysfunction metrics post-exit wave',
    'If severe: the diagnostic signal accelerates organizational decline by removing the actors most capable of reform. If mild: exit provides clean information without destabilizing the organization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adverse_selection_severity, empirical, 'Whether exit creates adverse selection in source organizations').

omega_variable(
    legitimacy_withdrawal_mechanism,
    'Does exit withdraw legitimacy from the source organization, or does it merely remove one dissenting voice?',
    'Network analysis of post-exit reputation effects; measurement of recruiting difficulty and external perception changes following high-profile exits',
    'If exit withdraws legitimacy: the diagnostic signal has external enforcement power. If exit is invisible: the constraint is purely internal coordination with no legitimacy effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_withdrawal_mechanism, empirical, 'Whether exit has external legitimacy effects beyond internal information').

omega_variable(
    voice_theater_detection,
    'Can organizations distinguish genuine voice mechanisms from voice theater (feedback systems that exist to absorb dissent without enabling change)?',
    'Comparison of grievance resolution rates, policy change rates following feedback, and exit rates among employees who used voice mechanisms vs those who did not',
    'If organizations cannot distinguish: voice theater persists and exit remains the only credible diagnostic. If organizations can distinguish: voice mechanisms can be reformed to restore credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voice_theater_detection, conceptual, 'Whether voice theater is detectable and correctable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exit_as_diagnosis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exit_diag_tr_t0, exit_as_diagnosis, theater_ratio, 0, 0.25).
narrative_ontology:measurement(exit_diag_tr_t5, exit_as_diagnosis, theater_ratio, 5, 0.32).
narrative_ontology:measurement(exit_diag_tr_t10, exit_as_diagnosis, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(exit_diag_be_t0, exit_as_diagnosis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exit_diag_be_t5, exit_as_diagnosis, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(exit_diag_be_t10, exit_as_diagnosis, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exit_as_diagnosis, information_standard).

% DUAL FORMULATION NOTE:
% Exit-as-diagnosis sits downstream of exit_cost_asymmetry (the mountain-level structural fact that exit costs differ by power and mobility) and voice_without_exit (the tangled rope where voice mechanisms exist but lack enforcement power). The diagnostic value of exit depends on the asymmetry created by the upstream constraints. This constraint has its own extractiveness (0.48) reflecting the adverse selection and legitimacy withdrawal mechanisms, distinct from the upstream constraints' extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
