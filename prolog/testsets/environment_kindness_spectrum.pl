% ============================================================================
% CONSTRAINT STORY: environment_kindness_spectrum
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_environment_kindness_spectrum, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: environment_kindness_spectrum
 *   human_readable: Environment Kindness Spectrum in Learning Domains
 *   domain: cognitive_science/expertise_development/learning_theory
 *
 * SUMMARY:
 *   The environment kindness spectrum, formalized by Robin Hogarth and
 *   extended by Kahneman & Klein's 'Conditions for Intuitive Expertise'
 *   framework, describes a structural property of learning domains: the
 *   degree to which they provide stable recurring patterns and immediate
 *   accurate feedback (kind environments) versus shifting patterns and
 *   delayed or misleading feedback (wicked environments). Chess, radiology,
 *   and livestock judging are kind — patterns recur, feedback is rapid and
 *   accurate, and deliberate practice reliably produces expertise. Clinical
 *   medicine, entrepreneurship, and stock trading are wicked — patterns
 *   shift, feedback is delayed and confounded, and experience does not
 *   reliably produce valid intuition. This is not a pedagogical choice or
 *   institutional arrangement. It is a physical and informational constraint
 *   determined by: (1) causal distance between action and outcome (feedback
 *   latency), (2) stability of the generative process producing problem
 *   instances (pattern recurrence), and (3) signal-to-noise ratio in outcome
 *   attribution (feedback accuracy). The constraint exhibits near-zero
 *   extraction because no agent benefits asymmetrically from the domain's
 *   inherent structure — all learners face the same feedback properties. The
 *   minimal extractiveness (0.08) reflects only the unavoidable
 *   information-theoretic cost of learning: even in the kindest environment,
 *   pattern recognition requires exposure to multiple instances, and feedback
 *   transmission has non-zero latency. This is a genuine natural law — a
 *   Mountain from all perspectives.
 *
 * KEY AGENTS:
 *   - Novice Learner: Powerless/trapped — cannot change domain structure; experiences kindness spectrum as immutable property of the task
 *   - Expert Practitioner: Powerful/mobile — can choose domains but cannot alter their feedback properties; recognizes structural differences across domains
 *   - Educational Institution: Institutional/arbitrage — designs curricula around domain properties but cannot change the underlying kindness structure
 *   - Cognitive Scientist: Analytical/analytical — observes kindness spectrum as structural property of task environments, not social construction
 *   - Professional Licensing Body: Organized/constrained — adapts training requirements to domain feedback properties but cannot alter those properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(environment_kindness_spectrum, 0.08).
domain_priors:suppression_score(environment_kindness_spectrum, 0.03).
domain_priors:theater_ratio(environment_kindness_spectrum, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(environment_kindness_spectrum, extractiveness, 0.08).
narrative_ontology:constraint_metric(environment_kindness_spectrum, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(environment_kindness_spectrum, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(environment_kindness_spectrum, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(environment_kindness_spectrum, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(environment_kindness_spectrum, mountain).
narrative_ontology:human_readable(environment_kindness_spectrum, "Environment Kindness Spectrum in Learning Domains").
narrative_ontology:topic_domain(environment_kindness_spectrum, "cognitive_science/expertise_development/learning_theory").

domain_priors:emerges_naturally(environment_kindness_spectrum).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVICE LEARNER (MOUNTAIN) — Cannot change the fundamental structure of the domain. Chess patterns recur; clinical diagnosis patterns shift. The learner experiences this as an immutable property of the domain itself. No exit from the domain's inherent feedback structure.
constraint_indexing:constraint_classification(environment_kindness_spectrum, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: EXPERT PRACTITIONER (MOUNTAIN) — Can choose domains but cannot change their kindness structure. Recognizes that chess mastery follows different learning curves than stock trading mastery because the domains have different inherent feedback properties. Mobile across domains but each domain's structure is fixed.
constraint_indexing:constraint_classification(environment_kindness_spectrum, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTION (MOUNTAIN) — Designs curricula around domain properties but cannot alter the fundamental kindness structure. Can choose to teach chess (kind) or entrepreneurship (wicked) but the feedback latency and pattern stability of each domain are structural constraints, not pedagogical choices.
constraint_indexing:constraint_classification(environment_kindness_spectrum, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Observes that the kindness spectrum is a structural property of task environments, not a social construction. Feedback latency is determined by causal distance between action and outcome; pattern recurrence is determined by the stability of underlying generative processes. These are physical and informational constraints, not institutional arrangements.
constraint_indexing:constraint_classification(environment_kindness_spectrum, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: PROFESSIONAL LICENSING BODY (MOUNTAIN) — Can set training requirements and certification standards but cannot change whether a domain provides kind or wicked learning environments. Medical diagnosis has inherently delayed feedback (treatment outcomes unfold over weeks/months); radiology has relatively immediate feedback (biopsy confirms or refutes interpretation). Licensing bodies adapt to these constraints; they do not create them.
constraint_indexing:constraint_classification(environment_kindness_spectrum, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(environment_kindness_spectrum_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(environment_kindness_spectrum, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(environment_kindness_spectrum, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(environment_kindness_spectrum, ExtMetricName, E),
    domain_priors:suppression_score(environment_kindness_spectrum, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(environment_kindness_spectrum),
    narrative_ontology:constraint_metric(environment_kindness_spectrum, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(environment_kindness_spectrum, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(environment_kindness_spectrum_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Near-zero. The minimal value reflects only the unavoidable information-theoretic cost of learning — even in maximally kind environments (chess, radiology), pattern recognition requires exposure to multiple problem instances, and feedback has non-zero transmission latency. No agent extracts asymmetrically from the domain's structure. The chess grandmaster does not benefit at the expense of the novice from the fact that chess patterns recur — both face the same structural property. The radiologist does not extract from the medical student because biopsy feedback is rapid — both benefit equally from the domain's kindness. Suppression (0.03): Near-zero. Learners can exit unkind domains and enter kind ones (though domain choice may be constrained by other factors — economic, geographic, social — those are separate constraints, not properties of the kindness spectrum itself). The spectrum does not suppress alternatives; it describes the alternatives. Accessibility collapse (0.92): Very high. All observers with access to the domain converge on the same assessment of its kindness structure. Chess players universally recognize that chess provides stable recurring patterns and immediate feedback. Entrepreneurs universally recognize that business outcomes are delayed and confounded. The convergence is near-total because the feedback properties are objective features of the task environment. Resistance (0.08): Very low. Attempts to change a domain's kindness structure face physical and informational barriers, not social resistance. You cannot make stock trading kind by institutional reform — market patterns are generated by complex adaptive systems with inherent unpredictability. You cannot make radiology wicked by policy — biopsy results provide ground truth regardless of institutional arrangements. Theater ratio (0.15): Very low. The constraint has minimal performative content. Feedback latency is measured in objective time units (seconds, days, years). Pattern recurrence is quantified by correlation across problem instances. Rule stability is tracked by out-of-sample prediction accuracy. These are direct observables, not proxies or rituals.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists for this constraint. All five perspectives classify as Mountain because the kindness spectrum is a genuine natural law — a structural property of task environments determined by physical causation (feedback latency), information theory (pattern stability), and statistical mechanics (noise in outcome attribution). The novice cannot change it. The expert cannot change it. The institution cannot change it. The licensing body cannot change it. The analytical observer recognizes it as unchangeable. The uniformity across perspectives is the diagnostic signature of a true Mountain: accessibility collapse is very high (0.92), resistance is very low (0.08), and the constraint emerges naturally from the physics and information theory of learning environments. This is not a false summit — there are no identifiable beneficiaries, no institutional arrangements that could be reformed to alter the feedback structure, and no perspectival position from which the constraint appears mutable.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the kindness spectrum does not create asymmetric extraction. All agents within a domain face the same feedback structure. The novice and the expert both experience chess as kind; both experience entrepreneurship as wicked. The difference in their skill levels reflects accumulated exposure to feedback, not differential access to the domain's structural properties. The minimal extractiveness (0.08) is symmetric — it represents the information-theoretic floor for learning in any domain, kind or wicked. This floor is unavoidable (you cannot learn patterns without exposure to instances) but it is not extractive (no agent captures the cost borne by others). All perspectives derive d ≈ 0.50 (symmetric) from the absence of beneficiary/victim declarations, producing f(d) ≈ 0.65, and χ ≈ 0.08 × 0.65 = 0.052 after scope adjustment. This is well below the Mountain threshold (χ ≤ 0.25) from all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN EXEMPLAR: This constraint demonstrates what a genuine natural law looks like in the Deferential Realism framework. It is not a coordination mechanism (no beneficiaries), not an extraction mechanism (no victims), not a degraded institution (theater ratio is minimal and stable), and not a temporary scaffold (no sunset clause — the kindness spectrum is a permanent feature of task environments). The constraint is a structural property of the world that all agents must navigate but none can change. The minimal extractiveness (0.08) reflects the unavoidable information-theoretic cost of learning, not asymmetric capture. The framework's mountain gates are designed to pass constraints like this while flagging false summits (constraints presented as natural law but with identifiable beneficiaries or institutional enforcement). The environment kindness spectrum passes all gates: emerges naturally (true), accessibility collapse (0.92 ≥ 0.85), resistance (0.08 ≤ 0.15), extractiveness (0.08 ≤ 0.25), suppression (0.03 ≤ 0.05). No beneficiaries declared, so FSM does not trigger. This is a true Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(environment_kindness_spectrum, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(env_kind_tr_t0, environment_kindness_spectrum, theater_ratio, 0, 0.15).
narrative_ontology:measurement(env_kind_tr_t50, environment_kindness_spectrum, theater_ratio, 50, 0.15).
narrative_ontology:measurement(env_kind_tr_t100, environment_kindness_spectrum, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(env_kind_be_t0, environment_kindness_spectrum, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(env_kind_be_t50, environment_kindness_spectrum, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(env_kind_be_t100, environment_kindness_spectrum, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(environment_kindness_spectrum, information_standard).

% DUAL FORMULATION NOTE:
% The environment kindness spectrum is a foundational constraint in learning theory. It does not decompose into multiple stories because its epsilon value is invariant across observables: feedback latency, pattern recurrence, and rule stability all converge on the same structural assessment. A domain that has delayed feedback also has low pattern recurrence and low rule stability (wicked); a domain with immediate feedback also has high pattern recurrence and high rule stability (kind). These are not separate constraints but different measurements of the same underlying structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
