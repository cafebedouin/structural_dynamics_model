% ============================================================================
% CONSTRAINT STORY: signal_without_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_signal_without_control, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: signal_without_control
 *   human_readable: The Passive Observational Trap
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Passive Observational Trap describes a structural constraint where
 *   agents possess complete visibility into a system's state through
 *   high-fidelity data streams but are systematically barred from adjusting
 *   the system's control parameters. This constraint manifests across
 *   multiple domains: financial regulation (traders observe markets they
 *   cannot influence), healthcare systems (patients access their data but
 *   cannot modify treatment protocols), infrastructure management (citizens
 *   observe air quality and water safety but cannot mandate operator
 *   improvements), and technological governance (users monitor AI system
 *   behavior but cannot modify training parameters). The constraint operates
 *   by decoupling observation from control authority — the signaling
 *   apparatus is transparent and abundant, while the control apparatus
 *   remains gatekept and monopolized. This creates a form of psychological
 *   torture disguised as transparency: observers gain knowledge of problems
 *   while simultaneously losing agency to remedy them. The extractiveness
 *   increases over time as observers accumulate signal data confirming their
 *   powerlessness.
 *
 * KEY AGENTS:
 *   - Passive Observers: Primary victims (powerless/trapped) — access full signal streams but retain zero control authority; bear consequences of system dysfunction
 *   - Dependent Stakeholders: Secondary victims (moderate/constrained) — have partial agency and can propose changes but lack enforcement power to implement them
 *   - System Operators: Primary beneficiaries (institutional/arbitrage) — monopolize control authority while selectively consuming observer signals; extract authority rent
 *   - Gatekeeping Authorities: Secondary beneficiary (institutional/arbitrage) — maintain signal-control decoupling through institutional rules; enforce boundary between observation and control
 *   - Regulatory Authority: Mixed actor (organized/mobile) — mandates signal disclosure but retains limited actual control over operational parameters
 *   - Institutional Theater: Performative actor (institutional/arbitrage) — maintains advisory councils and feedback loops that create appearance of observer influence without actual control transfer
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing control monopoly as technical necessity rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(signal_without_control, 0.58).
domain_priors:suppression_score(signal_without_control, 0.68).
domain_priors:theater_ratio(signal_without_control, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(signal_without_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(signal_without_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(signal_without_control, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(signal_without_control, snare).
narrative_ontology:human_readable(signal_without_control, "The Passive Observational Trap").
narrative_ontology:topic_domain(signal_without_control, "technological/social").

domain_priors:requires_active_enforcement(signal_without_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(signal_without_control, system_operators).
narrative_ontology:constraint_beneficiary(signal_without_control, gatekeeping_authorities).
narrative_ontology:constraint_victim(signal_without_control, passive_observers).
narrative_ontology:constraint_victim(signal_without_control, dependent_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PASSIVE OBSERVER (SNARE) — Full access to data streams describing system state but zero capacity to alter parameters. Trapped by information asymmetry: sees the problem but cannot act. Experiences high extraction as the observer bears consequences of system dysfunction while unable to remedy it. Knowledge of the threat without power to respond creates maximum frustration and dependency.
constraint_indexing:constraint_classification(signal_without_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPENDENT STAKEHOLDER (SNARE) — Has some agency but constrained by structural dependence on the system operators. Can propose changes based on data access but lacks enforcement power. Constrained exit: leaving the system entirely has high cost. Moderate but significant extraction: observer bears risk while operator retains control authority.
constraint_indexing:constraint_classification(signal_without_control, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEM OPERATOR (ROPE) — Retains exclusive control over parameter adjustment. Experiences the constraint as pure coordination benefit: data signal without control maintains their monopoly on decision authority. Can selectively act on observer inputs or ignore them. Benefits from the informational asymmetry while avoiding accountability for failures.
constraint_indexing:constraint_classification(signal_without_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Possesses mandated oversight and can compel disclosure of signals but retains limited actual control over operator adjustments. Mixed: coordination function (enforcement of transparency) + asymmetric extraction (regulatory authority extracts compliance costs from operators without bearing operational risk). Moderate exit options through policy change.
constraint_indexing:constraint_classification(signal_without_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL THEATER (PITON) — Formal feedback loops and advisory councils that receive signal data but possess no enforcement mechanism. Performative participation maintains legitimacy of the system while avoiding meaningful power redistribution. Theater ratio elevated by the ritual of consultation without implementation. The institutional apparatus is degraded — maintains appearance of responsiveness without functional control transfer.
constraint_indexing:constraint_classification(signal_without_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW) — From a universal perspective, signal without control may appear as an inherent limit of complex systems: decoupling of observation and intervention is a technical necessity in some domains. However, this naturalization obscures the structural choice to withhold control. The engine will identify this as a false summit — signal-control coupling is technically feasible in many contexts and the decoupling reflects institutional gatekeeping rather than immutable law.
constraint_indexing:constraint_classification(signal_without_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(signal_without_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(signal_without_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(signal_without_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(signal_without_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(signal_without_control, TR),
    TR >= 0.70.

:- end_tests(signal_without_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately-high. The constraint extracts authority value from observers via knowledge without power. Observers gain information advantage (reducing their ignorance cost) but experience authority deficit (they cannot act). The net extraction is high because the psychological cost of seeing a problem without power to fix it exceeds the benefit of knowing about the problem. The extractiveness has grown over the interval as signal fidelity has improved (big data, real-time monitoring, granular metrics) — better signals make powerlessness more acute. Suppression (0.68): High. Multiple mechanisms enforce the control boundary: (1) technical architecture that decouples monitoring from adjustment; (2) institutional rules that reserve control authority for operators; (3) legal/contractual restrictions on observer actions; (4) coalition-breaking through individualized data access (observers compete for operator attention rather than organizing collectively). Suppression is enforced actively — the system could be redesigned but is maintained through deliberate gatekeeping. Theater ratio (0.55): Moderate-high. The transparency apparatus (dashboards, reports, public data) creates performative responsiveness while actual control authority remains locked. Advisory boards and feedback channels provide theatrical participation without implementation authority. Theater has increased as operators have invested in transparency theater to deflect demands for actual control transfer.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence. The system operator (institutional/arbitrage) experiences this as pure Rope — they coordinate with observers through signal-based feedback while retaining exclusive control authority. The passive observer (powerless/trapped) experiences this as Snare — they see the problem clearly but cannot act, creating maximum frustration and extraction. The regulatory authority (organized/mobile) experiences this as Tangled Rope — they can mandate transparency and extract compliance costs from operators, but control actual system adjustment remains limited. The piton perspective reveals the institutional machinery as degraded and performative — advisory councils simulate influence without actual power redistribution. The mountain perspective risks falsely naturalizing the control monopoly, but the structural analysis shows it is a contingent institutional choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the signal-control boundary. Observers positioned 'below' the control boundary (powerless, trapped) experience high d → high f(d) → high χ. System operators positioned 'above' the control boundary (institutional, arbitrage) experience low d → negative f(d) → negative χ (they benefit). The regulatory authority straddles the boundary (organized, mobile exit) — they can mandate transparency but limited control authority, producing moderate d → moderate f(d) → moderate χ. The dependent stakeholder has constrained exit but some agency, producing d slightly lower than fully trapped but higher than beneficiary. Suppression is not scaled by directionality — it is a raw structural property of the control-barrier enforcement. The system is designed to maintain high suppression regardless of who the observer is; no amount of power or exit mobility can overcome the architectural decoupling.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_fidelity_sufficiency,
    'At what fidelity level does signal access without control become functionally equivalent to having control?',
    'Empirical testing: provide signal access to constrained agents; measure variance in outcomes vs outcomes when control is transferred; identify fidelity threshold where observer recommendations achieve parity with operator decisions',
    'If threshold is high: signal without control remains extractive (Snare persists). If threshold is low: signal access approximates control authority (reclassifies as Rope or Tangled Rope with reduced extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_fidelity_sufficiency, empirical, 'Threshold fidelity at which signal access approximates control').

omega_variable(
    operator_benevolence_dependency,
    'How much does the constraint''s extractiveness depend on operator willingness to act on signals vs structural control denial?',
    'Comparative case analysis: operators with identical signal access but different institutional incentives for responsiveness; measure compliance rate of operator to observer recommendations; test counterfactual operator types',
    'If dependency is high: constraint is behavioral/incentive-based (remediable through incentive alignment, reduces extraction). If dependency is low: constraint is structural (control monopoly is enforced regardless of signals, extraction persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operator_benevolence_dependency, empirical, 'Whether constraint is behavioral or structural control denial').

omega_variable(
    alternative_control_pathways,
    'Can observers access control through alternative mechanisms (indirect pressure, coalition formation, exit threat, technical workarounds)?',
    'Historical case study of successful observer pressure campaigns; identification of alternative control mechanisms (board representation, shareholder action, whistleblower networks, competitive entry); measurement of alternative pathway accessibility cost',
    'If alternatives exist and are accessible: exit is less trapped than appears (reclassifies as constrained or mobile, reduces chi). If alternatives are blocked: trap is reinforced (confirms Snare classification, confirms high suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_control_pathways, empirical, 'Accessibility of alternative control pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(signal_without_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swc_tr_t0, signal_without_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(swc_tr_t5, signal_without_control, theater_ratio, 5, 0.48).
narrative_ontology:measurement(swc_tr_t10, signal_without_control, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(swc_be_t0, signal_without_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(swc_be_t5, signal_without_control, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(swc_be_t10, signal_without_control, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(signal_without_control, information_standard).
narrative_ontology:affects_constraint(signal_without_control, opacity_resistance).
narrative_ontology:affects_constraint(signal_without_control, authority_legitimacy_asymmetry).
narrative_ontology:affects_constraint(signal_without_control, stakeholder_coalition_barrier).

% DUAL FORMULATION NOTE:
% The signal-without-control constraint is distinct from but related to three upstream constraints: (1) Opacity Resistance — technical/political barriers to signal generation itself; (2) Authority Legitimacy Asymmetry — the institutional justification for control monopoly; (3) Stakeholder Coalition Barrier — mechanisms that prevent observers from organizing. This story models the constraint imposed by the signal-control decoupling itself, downstream of opacity barriers but upstream of coalition failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(signal_without_control, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
