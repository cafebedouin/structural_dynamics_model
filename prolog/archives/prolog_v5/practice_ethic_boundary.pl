% ============================================================================
% CONSTRAINT STORY: practice_ethic_boundary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_practice_ethic_boundary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: practice_ethic_boundary
 *   human_readable: Practice-Ethic Boundary Classification (Door-Holding Case)
 *   domain: applied_ethics/social_psychology/phenomenology_of_attention
 *
 * SUMMARY:
 *   The practice-ethic boundary is a philosophical distinction that claims to
 *   track a real difference in the normative status of behavioral patterns:
 *   ethics are non-negotiable commitments that constitute moral identity,
 *   while practices are culturally contingent conventions that can be
 *   pragmatically adjusted. The door-holding case is a diagnostic exemplar
 *   because it sits precisely on the boundary — some agents experience
 *   abandoning door-holding as ethical failure (suggesting it's an ethic),
 *   others experience it as pragmatic adjustment to context (suggesting it's
 *   a practice), and the phenomenological difference itself may be
 *   constructed by prior exposure to the philosophical distinction. The
 *   constraint exhibits tangled rope structure: the boundary serves a genuine
 *   coordination function (enables moral discourse by distinguishing
 *   obligatory from optional) but also extracts by naturalizing a contingent
 *   philosophical category as a phenomenological given, thereby foreclosing
 *   pragmatic adjustment for agents who have internalized the boundary as
 *   identity-constitutive. The extraction has accumulated over the interval
 *   (1950-2025) as analytic philosophy's influence has spread the
 *   practice-ethic distinction into popular moral discourse, increasing the
 *   number of agents who experience the boundary as binding. Theater ratio
 *   has also increased as the boundary is maintained through increasingly
 *   elaborate philosophical argumentation that ordinary agents cannot access
 *   or evaluate, yet are expected to navigate.
 *
 * KEY AGENTS:
 *   - Ordinary Moral Agent: Primary victim (powerless/identity_locked) — cannot determine from within phenomenology whether door-holding is ethic or practice, yet classification determines whether abandonment is permissible; extraction is cognitive labor of constant self-monitoring and foreclosed pragmatic adjustment
 *   - Cross-Cultural Practitioner: Secondary victim (moderate/constrained) — bears cost of navigating multiple normative systems where boundary is drawn differently; benefits from boundary when it enables code-switching but extraction occurs when boundary itself is contested
 *   - Philosophical Category Maintenance: Primary beneficiary (institutional/arbitrage) — the boundary's contestedness generates ongoing professional work; can redefine boundaries when convenient; experiences constraint as pure coordination
 *   - Moral Psychology Researchers: Mixed position (institutional/constrained) — benefit from research questions generated by boundary but constrained by how it shapes experimental design; career incentives favor working within boundary rather than critiquing it
 *   - Virtue Ethics Revival Coalition: Organized agents (organized/mobile) — see boundary as temporary artifact being dissolved by practice-based ethics; have exit option via alternative frameworks; sunset mechanism is maturation of virtue ethics
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible hybridity: genuine coordination function coexists with extraction via naturalization of contingent distinction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(practice_ethic_boundary, 0.58).
domain_priors:suppression_score(practice_ethic_boundary, 0.62).
domain_priors:theater_ratio(practice_ethic_boundary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(practice_ethic_boundary, extractiveness, 0.58).
narrative_ontology:constraint_metric(practice_ethic_boundary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(practice_ethic_boundary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(practice_ethic_boundary, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(practice_ethic_boundary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(practice_ethic_boundary, tangled_rope).
narrative_ontology:human_readable(practice_ethic_boundary, "Practice-Ethic Boundary Classification (Door-Holding Case)").
narrative_ontology:topic_domain(practice_ethic_boundary, "applied_ethics/social_psychology/phenomenology_of_attention").

domain_priors:requires_active_enforcement(practice_ethic_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(practice_ethic_boundary, philosophical_category_maintenance).
narrative_ontology:constraint_beneficiary(practice_ethic_boundary, moral_psychology_researchers).
narrative_ontology:constraint_victim(practice_ethic_boundary, ordinary_moral_agents).
narrative_ontology:constraint_victim(practice_ethic_boundary, cross_cultural_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY MORAL AGENT (SNARE) — Identity-locked by internalized categorical distinction between ethics (non-negotiable) and practices (adjustable). Cannot abandon door-holding without experiencing it as ethical failure rather than pragmatic adjustment. The classification itself creates the binding — if door-holding is an ethic, abandoning it is moral corruption; if it's a practice, abandoning it is mere adaptation. The agent cannot determine which it is from within their phenomenology, yet the classification determines whether exit is morally permissible. Maximum extraction: the boundary policing extracts cognitive labor (constant self-monitoring: 'Am I being ethical or just polite?') and forecloses pragmatic adjustment.
constraint_indexing:constraint_classification(practice_ethic_boundary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CROSS-CULTURAL PRACTITIONER (TANGLED ROPE) — Constrained by need to navigate multiple normative systems (home culture treats door-holding as ethic; host culture treats it as intrusive practice). Benefits from the categorical distinction when it enables code-switching ('this is just local practice, not my ethics'), but extraction occurs when the boundary itself is contested — what counts as 'mere practice' in one context is 'ethical failure' in another. The coordination function (enabling cultural navigation) coexists with extraction (the boundary is enforced asymmetrically by philosophical gatekeepers who don't bear the cost of misclassification).
constraint_indexing:constraint_classification(practice_ethic_boundary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PHILOSOPHICAL CATEGORY MAINTENANCE (ROPE) — Primary beneficiary. The practice-ethic boundary enables professional philosophy to claim jurisdiction over what counts as moral vs merely conventional. Door-holding cases generate papers, conferences, and pedagogical examples. The boundary's contestedness is a feature, not a bug — it creates ongoing work. Experiences the constraint as pure coordination: the categorical distinction organizes moral inquiry and enables productive debate. Net beneficiary with arbitrage exit — can redefine boundaries when convenient.
constraint_indexing:constraint_classification(practice_ethic_boundary, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MORAL PSYCHOLOGY RESEARCHERS (TANGLED ROPE) — Institutional actors who benefit from the boundary (it generates research questions: 'Do people experience door-holding as moral obligation or social convention?') but are also constrained by it (the categorical distinction shapes experimental design and interpretation in ways that may miss how ordinary agents actually navigate these situations). The boundary coordinates research programs but also extracts by forcing empirical phenomena into pre-defined philosophical categories. Constrained exit: can critique the boundary but career incentives favor working within it.
constraint_indexing:constraint_classification(practice_ethic_boundary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VIRTUE ETHICS REVIVAL COALITION (SCAFFOLD) — Organized agents (neo-Aristotelians, care ethicists, practice-based ethicists) see the sharp practice-ethic boundary as a temporary artifact of 20th-century analytic philosophy that is being dissolved by virtue ethics and practice theory. The sunset mechanism: as virtue ethics reframes ethics as cultivated dispositions rather than rule-following, the question 'Is door-holding an ethic or a practice?' dissolves — it's a habituated response that shapes character. The coalition has mobile exit (can work in virtue ethics frameworks that don't require the boundary) and sees the constraint's extraction declining as alternative frameworks mature.
constraint_indexing:constraint_classification(practice_ethic_boundary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical position, the practice-ethic boundary serves a genuine coordination function (enables moral discourse by distinguishing obligatory from optional) but also extracts by naturalizing a contingent philosophical distinction as a phenomenological given. The boundary is neither pure coordination (Rope) nor pure extraction (Snare) — it's a conceptual infrastructure that both enables and constrains moral reasoning. The extraction is visible: ordinary agents experience distress when the boundary is unclear (door-holding case), yet the boundary itself is a product of philosophical theory, not a natural kind. The coordination is also visible: without some practice-ethic distinction, moral discourse collapses into relativism. Tangled Rope classification reflects irreducible hybridity.
constraint_indexing:constraint_classification(practice_ethic_boundary, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(practice_ethic_boundary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(practice_ethic_boundary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(practice_ethic_boundary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(practice_ethic_boundary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(practice_ethic_boundary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The boundary extracts cognitive labor (agents must constantly monitor whether behaviors are ethics or practices, and the classification determines whether adjustment is permissible) and forecloses pragmatic adaptation for identity-locked agents. The extraction is not maximal because some agents (cross-cultural practitioners, virtue ethicists) have developed strategies to navigate or bypass the boundary. Suppression (0.62): Moderate-high. Significant barriers to exiting the boundary include: (1) identity-lock for agents who have internalized the distinction as constitutive of moral selfhood, (2) philosophical authority that presents the boundary as tracking natural kinds rather than contingent categories, (3) lack of accessible alternative frameworks in popular moral discourse, (4) social enforcement (abandoning door-holding risks being perceived as rude or immoral, and the agent cannot control which perception applies). Theater ratio (0.48): Moderate. The boundary is maintained partly through genuine philosophical argumentation (conceptual analysis of obligation, stability, phenomenology) but increasingly through performative boundary-policing that ordinary agents cannot evaluate. The theater has increased over the interval as the distinction has spread into popular discourse without the philosophical apparatus that would enable agents to critically assess it. The coordination function is real (the boundary does enable moral discourse) but the maintenance mechanism has become partly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The ordinary moral agent sees a snare (identity-locked, cannot exit without moral failure, maximum extraction). The cross-cultural practitioner sees tangled rope (constrained, mixed benefit and cost, coordination function coexists with extraction). Philosophical category maintenance sees rope (pure coordination, net beneficiary, arbitrage exit). Moral psychology researchers see tangled rope (institutional position but constrained, benefit from boundary but also limited by it). The virtue ethics coalition sees scaffold (temporary problem with sunset, mobile exit, declining extraction as alternative frameworks mature). The analytical observer sees tangled rope (irreducible hybridity, genuine coordination coexists with extraction via naturalization). The gap between the ordinary agent's snare and the philosopher's rope is the core diagnostic: the same boundary that enables professional moral discourse forecloses pragmatic adjustment for agents who have internalized it as identity-constitutive. The gap between the identity-locked ordinary agent and the mobile virtue ethicist reveals that the binding is cognitive (internalized categorical distinction) rather than structural (the alternative frameworks exist but are not accessible to agents without philosophical training).
 *
 * DIRECTIONALITY LOGIC:
 *   The ordinary moral agent is identity-locked (cannot exit without abandoning moral self-concept) and is a victim (bears cognitive labor cost and foreclosed adjustment). This produces high d (victim + identity_locked → d ≈ 0.89) and high experienced extraction. The cross-cultural practitioner is constrained (high cost to exit but possible) and is both victim (bears navigation cost) and beneficiary (can use boundary for code-switching), producing moderate d (mixed position + constrained → d ≈ 0.55). Philosophical category maintenance is institutional with arbitrage exit and is primary beneficiary (the boundary generates professional work), producing low d (beneficiary + arbitrage → d ≈ 0.05) and negative experienced extraction (net benefit). Moral psychology researchers are institutional but constrained (career incentives bind them to the boundary) and mixed (benefit from research questions, constrained by categorical framing), producing moderate d (mixed + constrained → d ≈ 0.35). The virtue ethics coalition is organized with mobile exit and sees declining extraction (the boundary is dissolving), producing low d (beneficiary of dissolution + mobile → d ≈ 0.25). The analytical observer has analytical exit and sees the structural hybridity, producing moderate d (analytical position → d ≈ 0.72 per canonical fallback, but the genuine coordination function moderates this to ≈ 0.60).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the practice-ethic boundary is neither pure coordination (Rope) nor pure extraction (Snare) but irreducibly hybrid (Tangled Rope from the analytical position). The coordination function is genuine: the boundary enables moral discourse by distinguishing obligatory from optional, and without some such distinction, moral reasoning collapses into relativism. The extraction is also genuine: the boundary naturalizes a contingent philosophical distinction as a phenomenological given, thereby binding agents who have internalized it and foreclosing pragmatic adjustment. The mandatrophy question 'Is the boundary tracking a real feature of moral experience or imposing a philosophical category?' is resolved by recognizing that it does both — the boundary coordinates moral inquiry AND extracts by presenting itself as natural rather than constructed. The perspectival variation (snare for identity-locked agents, rope for philosophers, scaffold for virtue ethicists) shows that the hybridity is structural, not a measurement artifact. The constraint is tangled rope because both functions are irreducible: removing the coordination function would collapse moral discourse, but the coordination function necessarily involves extraction because any categorical boundary will bind some agents while benefiting others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phenomenological_primacy,
    'Is the felt difference between abandoning door-holding (experienced as ethical failure by some agents) and abandoning other habits (experienced as pragmatic adjustment) evidence of a real practice-ethic boundary, or is the phenomenology itself constructed by prior exposure to the philosophical distinction?',
    'Cross-cultural phenomenological studies comparing agents raised in philosophical traditions that emphasize the practice-ethic boundary vs traditions that don''t; developmental studies tracking when children begin to experience the phenomenological difference and whether it correlates with exposure to categorical moral language',
    'If phenomenology is primary: the boundary tracks a real feature of moral experience, and the constraint is more Rope than Snare (genuine coordination of pre-theoretical moral intuitions). If phenomenology is constructed: the boundary is more Snare than Rope (philosophical categories colonizing ordinary moral experience).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(phenomenological_primacy, empirical, 'Whether phenomenological difference between ethics and practices is primary or constructed').

omega_variable(
    stability_threshold,
    'What degree of cross-context stability is required for a behavioral pattern to count as an ethic rather than a practice? Door-holding is stable within a culture but varies across cultures — does this make it a practice (culturally contingent) or an ethic (stable within the agent''s normative community)?',
    'Conceptual analysis of stability criteria in existing ethical theories; empirical mapping of which behaviors philosophers classify as ethics vs practices and what stability thresholds they implicitly use',
    'If threshold is high (cross-cultural universality required): most candidate ethics are reclassified as practices, and the boundary loses normative force. If threshold is low (within-community stability sufficient): the boundary becomes trivial (almost everything stable is an ethic).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_threshold, conceptual, 'Stability threshold for practice-ethic classification').

omega_variable(
    obligation_generation_mechanism,
    'Does door-holding generate obligations to others (if I hold the door, you should acknowledge; if you don''t hold the door for me, you''ve wronged me) because it''s an ethic, or does the obligation-generation itself constitute what makes it an ethic?',
    'Longitudinal tracking of how obligation-language emerges around behavioral patterns; experimental manipulation of whether a behavior is framed as ethic vs practice and measurement of whether obligation-attributions follow the framing or precede it',
    'If obligation-generation is downstream of ethic classification: the boundary is doing real conceptual work (distinguishing obligatory from optional). If obligation-generation constitutes ethic classification: the boundary is circular (we call things ethics because they generate obligations, and they generate obligations because we call them ethics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_generation_mechanism, empirical, 'Causal direction between obligation-generation and ethic classification').

omega_variable(
    introspective_access_reliability,
    'Can ordinary agents reliably introspect whether abandoning door-holding would feel like ethical failure or pragmatic adjustment, or is the introspective report itself shaped by the philosophical framing of the question?',
    'Comparison of introspective reports elicited with neutral framing (''How would you feel if you stopped holding doors?'') vs philosophical framing (''Would stopping be an ethical failure or a pragmatic adjustment?''); test-retest reliability of introspective classifications',
    'If introspection is reliable: agents have privileged access to the practice-ethic boundary in their own case, and the boundary tracks something real. If introspection is unreliable: the boundary is a philosophical imposition that ordinary agents cannot consistently apply even to their own behavior.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(introspective_access_reliability, empirical, 'Reliability of introspective access to practice-ethic boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(practice_ethic_boundary, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(peb_theater_1950, practice_ethic_boundary, theater_ratio, 0, 0.3).
narrative_ontology:measurement(peb_theater_1975, practice_ethic_boundary, theater_ratio, 25, 0.38).
narrative_ontology:measurement(peb_theater_2000, practice_ethic_boundary, theater_ratio, 50, 0.48).
narrative_ontology:measurement(peb_theater_2010, practice_ethic_boundary, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(peb_extract_1950, practice_ethic_boundary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(peb_extract_1975, practice_ethic_boundary, base_extractiveness, 25, 0.46).
narrative_ontology:measurement(peb_extract_2000, practice_ethic_boundary, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(peb_extract_2010, practice_ethic_boundary, base_extractiveness, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(practice_ethic_boundary, identity_coordination).
narrative_ontology:boltzmann_floor_override(practice_ethic_boundary, 0.08).

% DUAL FORMULATION NOTE:
% The practice-ethic boundary is downstream of introspective_access_limits (mountain: agents cannot reliably introspect the source of their phenomenology, so cannot determine whether the felt difference between abandoning door-holding and abandoning other habits is evidence of a real boundary or constructed by prior exposure to the philosophical distinction) and misattribution_feedback_loop (tangled rope: agents attribute their distress at abandoning door-holding to the behavior's ethical status, which reinforces the boundary, which increases the distress, creating a feedback loop that makes the boundary appear more natural than it is). The practice-ethic boundary has its own extractiveness (0.58) reflecting the cognitive labor and foreclosed adjustment; the upstream constraints have their own extractiveness values reflecting the epistemic limits (introspective_access_limits ε ≈ 0.08, mountain) and feedback dynamics (misattribution_feedback_loop ε ≈ 0.52, tangled rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(practice_ethic_boundary, analytical, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
