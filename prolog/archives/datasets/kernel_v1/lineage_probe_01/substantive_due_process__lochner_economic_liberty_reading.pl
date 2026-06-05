% ============================================================================
% CONSTRAINT STORY: substantive_due_process__lochner_economic_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substantive_due_process__lochner_economic_liberty_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substantive_due_process__lochner_economic_liberty_reading
 *   human_readable: Substantive Due Process: Lochner Economic Liberty Reading
 *   domain: constitutional_law/doctrinal
 *
 * SUMMARY:
 *   Substantive due process under the Fourteenth Amendment has been read to
 *   protect economic liberty in the form of freedom of contract, particularly
 *   regarding wages and hours legislation. This is the Lochner reading, named
 *   for the 1905 case Lochner v. New York, in which the Supreme Court struck
 *   down a New York law limiting bakers' working hours as a violation of
 *   liberty of contract. The reading takes the Fourteenth Amendment's Due
 *   Process Clause to guarantee not merely procedural fairness but
 *   substantive economic freedoms. During the Lochner era (roughly
 *   1905–1937), the Court struck down dozens of state and federal protective
 *   labor laws—hours limits, minimum wage laws, workplace safety
 *   regulations—in the name of defending workers' and employers' freedom to
 *   contract without governmental interference. This reading treats
 *   bargaining inequality between capital and labor as a matter of individual
 *   liberty rather than as a structural suppression problem amenable to
 *   legislative remedy. The doctrine's extractiveness increased over time as
 *   the Court became more aggressive in striking down protective legislation
 *   and as the gap between doctrinal rhetoric ('liberty of contract') and
 *   actual function (protecting capital's negotiating advantage) widened. The
 *   theater ratio rose as courts used increasingly formalistic language to
 *   reach predetermined outcomes. By the 1930s, the doctrine's performative
 *   content dominated its functional content.
 *
 * KEY AGENTS:
 *   - Workers/Laborers: Primary victims (powerless/trapped) — dependent on wage labor; cannot bargain individually with capital; legislative protection is constitutionally forbidden
 *   - Employers/Capital: Primary beneficiaries (institutional/arbitrage) — benefit from unequal bargaining power constitutionalized as 'liberty'; can relocate or adapt strategy
 *   - Progressive Legislators: Secondary victims (organized/constrained) — pass protective laws that courts strike down; have political will but no constitutional authority to override judicial veto
 *   - Supreme Court: Constrained institutional enforcer (institutional/constrained) — must apply doctrine but increasingly uncomfortable with outcomes; becomes the instrument of suppression
 *   - Lochner Doctrine as Pattern: Degraded institutional practice (institutional/arbitrage) — once functioned as coordination principle; becomes performative theater maintaining extractive outcomes
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees doctrine as constitutionalization of bargaining inequality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substantive_due_process__lochner_economic_liberty_reading, 0.62).
domain_priors:suppression_score(substantive_due_process__lochner_economic_liberty_reading, 0.68).
domain_priors:theater_ratio(substantive_due_process__lochner_economic_liberty_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substantive_due_process__lochner_economic_liberty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substantive_due_process__lochner_economic_liberty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(substantive_due_process__lochner_economic_liberty_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substantive_due_process__lochner_economic_liberty_reading, snare).
narrative_ontology:human_readable(substantive_due_process__lochner_economic_liberty_reading, "Substantive Due Process: Lochner Economic Liberty Reading").
narrative_ontology:topic_domain(substantive_due_process__lochner_economic_liberty_reading, "constitutional_law/doctrinal").

domain_priors:requires_active_enforcement(substantive_due_process__lochner_economic_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substantive_due_process__lochner_economic_liberty_reading, 'f8982c33-cbad-4538-a2ed-a0d35f501a9e').
narrative_ontology:cs_kernel_codification('f8982c33-cbad-4538-a2ed-a0d35f501a9e', fixed_text).
narrative_ontology:cs_authority_grounding('f8982c33-cbad-4538-a2ed-a0d35f501a9e', lineage).
narrative_ontology:cs_interpretation_layer_present('f8982c33-cbad-4538-a2ed-a0d35f501a9e').
narrative_ontology:cs_reading_relation('f8982c33-cbad-4538-a2ed-a0d35f501a9e', substantive_due_process__history_tradition_test_reading, forecloses).
narrative_ontology:cs_reading_relation('f8982c33-cbad-4538-a2ed-a0d35f501a9e', substantive_due_process__privacy_line_reading, coexists_with).
narrative_ontology:cs_axiom('f8982c33-cbad-4538-a2ed-a0d35f501a9e', foundational, economic_liberty_is_substantive_constitutional_right).
narrative_ontology:cs_axiom_status(economic_liberty_is_substantive_constitutional_right, overridden).
narrative_ontology:cs_axiom_grounding('f8982c33-cbad-4538-a2ed-a0d35f501a9e', economic_liberty_is_substantive_constitutional_right, deontological).
narrative_ontology:cs_axiom('f8982c33-cbad-4538-a2ed-a0d35f501a9e', foundational, liberty_of_contract_superior_to_protective_legislation).
narrative_ontology:cs_axiom_status(liberty_of_contract_superior_to_protective_legislation, overridden).
narrative_ontology:cs_axiom_grounding('f8982c33-cbad-4538-a2ed-a0d35f501a9e', liberty_of_contract_superior_to_protective_legislation, deontological).
narrative_ontology:cs_reference_frame('f8982c33-cbad-4538-a2ed-a0d35f501a9e', liberty_of_contract_supremacy).
narrative_ontology:cs_drift_state('f8982c33-cbad-4538-a2ed-a0d35f501a9e', post_1937_new_deal, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('f8982c33-cbad-4538-a2ed-a0d35f501a9e', '').
narrative_ontology:cs_kernel_id(substantive_due_process__lochner_economic_liberty_reading, substantive_due_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substantive_due_process__lochner_economic_liberty_reading, employers).
narrative_ontology:constraint_beneficiary(substantive_due_process__lochner_economic_liberty_reading, laissez_faire_doctrine).
narrative_ontology:constraint_victim(substantive_due_process__lochner_economic_liberty_reading, protective_labor_legislation).
narrative_ontology:constraint_victim(substantive_due_process__lochner_economic_liberty_reading, working_class_bargaining_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKER/LABORER (SNARE) — Trapped by economic dependency and by the constitutional doctrine's elevation of employer liberty of contract above protective legislation. Cannot exit the low-wage, long-hours regime because alternatives are closed off and the court has constitutionalized the suppression. Maximum extraction: the doctrine itself becomes the mechanism preventing legislative escape.
constraint_indexing:constraint_classification(substantive_due_process__lochner_economic_liberty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYER/CAPITAL (ROPE) — Experiences the doctrine as pure coordination: the liberty of contract principle solves the coordination problem of defining what employment terms are constitutionally protected. Net beneficiary with arbitrage options. Can exit to another jurisdiction or adapt business model, but the doctrine's framing makes the constraint appear as a natural freedom rather than as extraction.
constraint_indexing:constraint_classification(substantive_due_process__lochner_economic_liberty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRESSIVE LEGISLATORS (SNARE) — Organized but constrained by the judicial veto. Can pass protective legislation (hours laws, minimum wage) but the Supreme Court strikes it down in the name of liberty of contract. Suppression is active and enforced: legislative alternatives are prohibited by constitutional doctrine. High extraction because the political will exists but is suppressed by judicial enforcement.
constraint_indexing:constraint_classification(substantive_due_process__lochner_economic_liberty_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COURTS (TANGLED ROPE) — Institutional actor constrained by doctrinal commitments. The courts experience the doctrine as both coordination (defining constitutional boundaries) and extraction (from the perspective of those whose legislation is struck down). The courts are enforcing a principle they cannot easily abandon without reversing precedent. Mixed extraction and coordination function — they are not pure extractors but constrained tools of the doctrine.
constraint_indexing:constraint_classification(substantive_due_process__lochner_economic_liberty_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LOCHNER DOCTRINE AS INSTITUTIONAL PATTERN (PITON) — Once functioned as a genuine coordination mechanism (defining the boundary between liberty and permissible regulation). But by the 1930s, the doctrine's primary function had degraded into theatrical performance. Courts used 'liberty of contract' language to reach predetermined outcomes favoring capital, with increasingly transparent inconsistency. By mid-century, the doctrine persisted through institutional inertia despite acknowledged failure. The piton classification reflects that the doctrine's stated function (principled defense of liberty) no longer matches its actual function (blocking redistribution) — it is maintained through the theater of doctrinal continuity rather than through structural force.
constraint_indexing:constraint_classification(substantive_due_process__lochner_economic_liberty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/universal perspective, the doctrine's suppression of protective legislation constitutes a structural enforcement mechanism elevated to constitutional principle. The doctrine takes contingent bargaining inequality (workers' weaker negotiating position due to capital concentration) and constitutionalizes it as 'liberty of contract.' The analytical perspective sees this as pure extraction: the inequality that would normally trigger legislative correction is instead locked in place by constitutional doctrine and enforced by courts.
constraint_indexing:constraint_classification(substantive_due_process__lochner_economic_liberty_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substantive_due_process__lochner_economic_liberty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substantive_due_process__lochner_economic_liberty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substantive_due_process__lochner_economic_liberty_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substantive_due_process__lochner_economic_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(substantive_due_process__lochner_economic_liberty_reading, TR),
    TR >= 0.70.

:- end_tests(substantive_due_process__lochner_economic_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The doctrine suppresses protective legislation that would otherwise reduce bargaining inequality. Workers cannot exit the constraint through normal political channels because the Court has made protective legislation unconstitutional. The extraction flows from workers to employers via the constitutionalization of inequality. Rising trajectory (0.35 → 0.62 over 40 years) reflects increasing aggressiveness of the Court and widening gap between empirical outcomes and doctrinal language. Suppression (0.68): High. The mechanism that prevents workers from accessing legislative protection is itself constitutional — the Supreme Court's authority to strike down legislation. This is structural suppression enforced by the highest legal authority. The suppression rose as the Court became more willing to invalidate protective laws. Theater ratio (0.55): Moderate-high. The doctrine's stated function is to protect individual liberty of contract. Its actual function is to prevent redistribution in favor of capital. The gap between stated and actual function widened over time as courts used increasingly formalistic reasoning ('liberty' language masking inequality outcomes). By the 1930s, judges were explicitly acknowledging the doctrine's contradiction but continued applying it through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a severe perspectival gap between beneficiaries and victims. Employers see the doctrine as coordinate (Rope) — a principled protection of liberty that enables voluntary exchange. Workers see it as pure extraction (Snare) — suppression of their only available remedy (legislative protection). Progressive legislators see judicial veto power (Snare) — their enacted will is reversed by constitutional doctrine. The courts see doctrinal constraint (Tangled Rope) — they must apply the principle but increasingly recognize its extractive consequences. The doctrine itself degrades to Piton — maintained by institutional inertia despite acknowledged failure. The analytical observer sees the entire structure as Snare — the constitutionalization of bargaining inequality is the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality from the agent's structural position relative to the constraint. Workers are trapped (no exit) and victims (extraction flows from them), producing maximum d and high f(d). Employers are beneficiaries (extraction flows toward them) with arbitrage options (can exit to other jurisdictions), producing low d and negative f(d). Progressive legislators are organized but constrained by judicial veto — they have political power but no constitutional authority to override the constraint. The courts themselves are constrained by doctrinal precedent; they experience the doctrine as both coordination (defining constitutional boundaries) and extraction (from the perspective of those harmed by struck-down legislation). The piton classification reflects that the doctrine's primary function has degraded — it persists through institutional continuity rather than through genuine coordination or legitimate constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this reading is resolved by recognizing that the constraint's classification depends entirely on the observer's structural position. The doctrine appears as coordinate (Rope) only from the beneficiary perspective (employers, capital). From all victim perspectives (workers, protective legislation, progressive legislators), it appears as extraction (Snare). The doctrine's own degradation to Piton by the mid-twentieth century reflects that the institutional actors most committed to maintaining it (the courts) had come to recognize the gap between its stated function and its extractive outcomes. The resolution of the mandatrophy is the overruling of Lochner in 1937, when the Court abandoned the economic liberty reading and permitted protective legislation to stand. This was not a discovery that the doctrine was wrong — it was a choice to abandon a reading that could no longer be reconciled with social outcomes and institutional legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liberty_vs_coercion_framing,
    'Is the absence of maximum working hours and minimum wage laws a positive liberty (freedom of contract) or a form of structural coercion (forced acceptance of employer terms by economically dependent workers)?',
    'Comparative institutional analysis: outcomes in jurisdictions with protective legislation vs. laissez-faire regimes; worker mobility and bargaining power measurements; counterfactual analysis of what workers would choose under genuine voluntary conditions.',
    'If liberty frame is accurate: doctrine is coordinate/rope from all perspectives. If coercion frame is accurate: doctrine is snare/extraction from all perspectives. The framing choice determines whether the doctrine is legitimate or extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liberty_vs_coercion_framing, conceptual, 'Whether contract freedom or bargaining coercion is the dominant structural principle').

omega_variable(
    alternative_reading_logical_coherence,
    'Can this reading coexist with the history-tradition test reading and the privacy line reading within a single constitutional framework, or do they foreclose one another?',
    'Doctrinal analysis: examine whether accepting this reading''s axioms (substantive due process protects economic liberty) logically requires rejecting the other readings'' axioms. Trace the doctrinal history to identify points where explicit choice between readings occurred.',
    'If readings foreclose: the doctrine contains irreducible contradiction requiring a choice of which reading governs. If readings coexist: the doctrine is genuinely multivalent, with different institutional actors holding different readings simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_logical_coherence, conceptual, 'Whether the three readings can coexist or whether one forecloses the others').

omega_variable(
    empirical_wages_hours_correlation,
    'Did workers'' wages, hours, and living conditions actually improve after Lochner was overruled (1937 onward), or did other factors (World War II, union organization, technological change) drive improvements?',
    'Time-series analysis of wages, hours, working conditions, and union density before and after Lochner overturn; cross-sectional comparison with other countries during the same period; counterfactual modeling of what would have occurred without Lochner.',
    'If Lochner overturn was causal: the doctrine was indeed suppressing protective legislation, confirming extractiveness. If other factors dominated: extractiveness may be lower than base assessment (doctrine was one factor among many).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_wages_hours_correlation, empirical, 'Whether Lochner overturn caused improvement in labor conditions').

omega_variable(
    boundary_between_economic_and_privacy_readings,
    'What structural principle distinguishes the economic liberty reading from the privacy line reading? Both invoke substantive due process — what makes one about commerce and the other about intimacy?',
    'Doctrinal archaeology: trace the Supreme Court''s implicit and explicit reasoning for why economic liberty fell away while privacy rights persisted. Identify the distinguishing principle (if any) vs. the possibility that the boundary is entirely contingent on political shifts.',
    'If a principled distinction exists: the doctrine has internal coherence even across readings. If boundary is contingent: the doctrine is fundamentally arbitrary, and classification must reflect this as a conceptual omega rather than a structural one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_between_economic_and_privacy_readings, conceptual, 'What principle distinguishes economic from privacy substantive due process readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substantive_due_process__lochner_economic_liberty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lochner_theater_1895, substantive_due_process__lochner_economic_liberty_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lochner_theater_1910, substantive_due_process__lochner_economic_liberty_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(lochner_theater_1935, substantive_due_process__lochner_economic_liberty_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(lochner_extract_1895, substantive_due_process__lochner_economic_liberty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lochner_extract_1910, substantive_due_process__lochner_economic_liberty_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(lochner_extract_1935, substantive_due_process__lochner_economic_liberty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lochner_suppress_1895, substantive_due_process__lochner_economic_liberty_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(lochner_suppress_1910, substantive_due_process__lochner_economic_liberty_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(lochner_suppress_1935, substantive_due_process__lochner_economic_liberty_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substantive_due_process__lochner_economic_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substantive_due_process__lochner_economic_liberty_reading, substantive_due_process__history_tradition_test_reading).
narrative_ontology:affects_constraint(substantive_due_process__lochner_economic_liberty_reading, substantive_due_process__privacy_line_reading).

% DUAL FORMULATION NOTE:
% The lochner_economic_liberty_reading is structurally distinct from the history_tradition_test_reading and privacy_line_reading because each instantiates a different constraint with different beneficiary/victim structures and different ε values. The economic liberty reading has high extractiveness (0.62) because it suppresses protective legislation. The privacy line reading has lower extractiveness because it protects a different domain (intimate personal decisions) with different suppression mechanisms. The history-tradition test reading has the lowest extractiveness because it acts as a brake on substantive due process expansion. The three readings share a kernel (the text of the Fourteenth Amendment Due Process Clause) but decompose into separate constraints with different structural properties. They are linked via network.affects_constraints because a shift in one reading affects the authority and legitimacy of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
