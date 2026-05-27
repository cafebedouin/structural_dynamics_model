% ============================================================================
% CONSTRAINT STORY: birth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birth_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: birth_reading
 *   human_readable: Moral Status Begins at Birth (Bodily Autonomy Reading)
 *   domain: bioethics/constitutional_law/reproductive_rights
 *
 * SUMMARY:
 *   The birth reading instantiates one contending interpretation of the
 *   personhood_boundary kernel — the constitutional, legal, and moral
 *   commitment to where human personhood begins and what rights attach to
 *   that status. This reading grounds moral status in exit from the maternal
 *   body (birth event) rather than in conception or viability. Under birth
 *   reading, the pregnant person holds sole decision-making authority over
 *   pregnancy continuation, abortion is unrestricted or permitted up to and
 *   including late-term abortion, and the state cannot enforce fetal
 *   personhood. The constraint coordinates reproductive autonomy, medical
 *   authority, and parental responsibility around the principle that bodily
 *   integrity is inviolable and that moral status acquisition occurs at
 *   birth, not before. The reading exhibits low extractiveness (0.35) and low
 *   theater (0.30), indicating strong institutional coherence with its
 *   organizing principle. The constraint is primarily a Rope from the
 *   perspective of the pregnant person and the liberal autonomy framework —
 *   genuine coordination with minimal coercion. However, different
 *   perspectives produce different classifications: the state experiences
 *   tangled coordination and extraction; the fetus experiences snare (zero
 *   moral status); medical institutions experience piton degradation
 *   (contradiction between clinical treatment and moral status denial). The
 *   reading is one stable solution to the personhood boundary question,
 *   coexisting with conception_reading and viability_reading as alternative
 *   normative frameworks instantiated in different jurisdictions and
 *   traditions.
 *
 * KEY AGENTS:
 *   - Pregnant Person: Primary beneficiary and decision-maker (moderate/constrained) — holds full moral and legal authority over pregnancy; can exit through abortion, birth, or other choices; experiences constraint as enabling coordination
 *   - Liberal Autonomy Framework: Institutional beneficiary (institutional/arbitrage) — grounds legitimacy in bodily autonomy principle; reinforced by birth reading; experiences no extraction
 *   - Reproductive Justice Coalition: Organized agents (organized/mobile) — advocates for abortion access; sees birth reading as protective scaffold with vulnerability to political erosion
 *   - State Authority: Mixed beneficiary and enforcer (powerful/arbitrage) — coordinates medical regulation without fetal personhood enforcement; retains power to regulate timing/procedures; moderate extraction
 *   - Fetus: Zero moral status under this reading (powerless/trapped) — cannot advocate, organize, or exit; bears full cost of status denial; snare perspective
 *   - Medical and Obstetric Institutions: Partial beneficiary with internal contradiction (institutional/arbitrage) — provides abortion services and prenatal care simultaneously; maintains contradiction through compartmentalization; piton degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees birth reading as coherent institutional choice, not natural law; low false-summit risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birth_reading, 0.35).
domain_priors:suppression_score(birth_reading, 0.25).
domain_priors:theater_ratio(birth_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birth_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(birth_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(birth_reading, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birth_reading, rope).
narrative_ontology:human_readable(birth_reading, "Moral Status Begins at Birth (Bodily Autonomy Reading)").
narrative_ontology:topic_domain(birth_reading, "bioethics/constitutional_law/reproductive_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(birth_reading, '525de304-fe7b-4dcf-a270-74f6e90f169a').
narrative_ontology:cs_created_at('525de304-fe7b-4dcf-a270-74f6e90f169a', '').
narrative_ontology:cs_kernel_codification('525de304-fe7b-4dcf-a270-74f6e90f169a', formalized).
narrative_ontology:cs_authority_grounding('525de304-fe7b-4dcf-a270-74f6e90f169a', lineage).
narrative_ontology:cs_interpretation_layer_present('525de304-fe7b-4dcf-a270-74f6e90f169a').
narrative_ontology:cs_kernel_id(birth_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('525de304-fe7b-4dcf-a270-74f6e90f169a', conception_reading, forecloses).
narrative_ontology:cs_reading_relation('525de304-fe7b-4dcf-a270-74f6e90f169a', viability_reading, coexists_with).
narrative_ontology:cs_axiom('525de304-fe7b-4dcf-a270-74f6e90f169a', foundational, moral_status_at_birth).
narrative_ontology:cs_axiom_status(moral_status_at_birth, holdable).
narrative_ontology:cs_axiom('525de304-fe7b-4dcf-a270-74f6e90f169a', foundational, bodily_autonomy_is_trump).
narrative_ontology:cs_axiom_status(bodily_autonomy_is_trump, holdable).
narrative_ontology:cs_reference_frame('525de304-fe7b-4dcf-a270-74f6e90f169a', bodily_autonomy_liberal_framework).
narrative_ontology:cs_drift_state('525de304-fe7b-4dcf-a270-74f6e90f169a', contemporary_post_dobbs_pluralism, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(birth_reading, pregnant_person).
narrative_ontology:constraint_beneficiary(birth_reading, liberal_autonomy_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (ROPE) — Under birth reading, the pregnant person holds sole moral and legal authority over pregnancy continuation. Coordination function: the constraint coordinates parental responsibility, fetal development, and bodily autonomy by granting the pregnant person full decision-making authority. No coercive asymmetry — the pregnant person can exit (via abortion, birth, or other reproductive decisions) at biographical cost but without legal prohibition. The constraint is experienced as enabling rather than extractive.
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: LIBERAL RIGHTS FRAMEWORK (ROPE) — Institutional actor grounding its legitimacy in bodily autonomy as a foundational moral principle. Experiences the constraint as pure coordination: birth reading organizes reproductive rights, medical decision-making authority, and abortion access around the principle that bodily integrity is inviolable. The framework benefits from the constraint (it instantiates and reinforces liberal autonomy doctrine) and experiences no extraction — the mechanism is self-reinforcing without coercive overhead.
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REPRODUCTIVE JUSTICE COALITION (SCAFFOLD) — Organized actors (abortion rights advocates, reproductive autonomy organizations) see birth reading as a temporary structural solution that achieves partial coordination around pregnant person authority but remains vulnerable to political erosion. Experiences the constraint as scaffolding: it provides necessary protection of abortion access during a particular political window, but the sunsets and vulnerabilities are real. Exit is mobile (activists can shift to other jurisdictions or advocacy strategies), and the coalition perceives the constraint as supporting but not guaranteed long-term.
constraint_indexing:constraint_classification(birth_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE AUTHORITY (TANGLED ROPE) — The state experiences the constraint as mixed coordination and extraction. Coordination function: birth reading allows the state to regulate reproduction without enforcing fetal personhood, avoiding the resource-intensive enforcement apparatus required by conception reading. The state coordinates medical regulation, informed consent, and viability thresholds while respecting bodily autonomy. But the state also extracts: it maintains enforcement authority over the timing of birth, regulates abortion procedures, and retains power to set gestational limits. The state has arbitrage (can shift frameworks across jurisdictions), so experienced extraction is moderate rather than maximal.
constraint_indexing:constraint_classification(birth_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE FETUS (SNARE) — Under birth reading, the fetus has no moral status until birth; cannot organize, advocate, or exit; bears full cost of the reading's framework without representation. The fetus cannot be a beneficiary (under this reading's axiom, it has no moral claims) and cannot escape the constraint (it exists only within the constraint structure). Maximum powerlessness and trappedness. This perspective instantiates the reading's core structural claim: that pre-birth moral status is null.
constraint_indexing:constraint_classification(birth_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: MEDICAL AND OBSTETRIC INSTITUTIONS (PITON) — Medical system experiences birth reading as partially degraded. The constraint requires obstetric institutions to provide abortion services and respect bodily autonomy while simultaneously maintaining capacity for fetal monitoring, viability assessment, and postnatal care. The theater ratio reflects institutional bifurcation: prenatal care is organized around fetal well-being (implicitly assigning quasi-moral status to the fetus), while abortion provision is organized around bodily autonomy (denying prenatal moral status). The institutions maintain this contradiction through practice compartmentalization, not through coherent framework. Theater persists because the reading's axiom (no prenatal moral status) conflicts with obstetric realities that treat the fetus as a clinical entity requiring specialized attention.
constraint_indexing:constraint_classification(birth_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — From a universal civilizational view, birth reading is a coherent framework for organizing reproductive autonomy and parental responsibility. It coordinates exit (birth is the transition point for moral status acquisition) and establishes clear decision-making authority (pregnant person holds it until birth). The framework is internally consistent and produces low theater when the institutional contradiction (clinical fetal status vs. moral status denial) is resolved in favor of bodily autonomy. No false summit — the analytical observer can see that birth reading is a deliberate institutional choice, not a natural law.
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(birth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birth_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(birth_reading, TR),
    TR >= 0.70.

:- end_tests(birth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate-low. The constraint extracts minimal value through coercive means. The pregnant person experiences enabling coordination, not extraction. The state retains enforcement authority but uses it primarily for medical regulation (informed consent, procedure safety, viability assessment) rather than for moral status enforcement. The fetus, having no moral status under this reading, cannot be an extraction target — it is simply outside the moral consideration set. The moderate extractiveness reflects that the state retains some power to regulate late-abortion timing and medical procedures, and the medical establishment maintains institutional authority over prenatal care. Suppression (0.25): Low-moderate. Barriers to exit exist but are primarily biographical (pregnancy duration, bodily experience) rather than legal or coercive. The pregnant person can legally exit through abortion with access-based constraints (availability, cost, travel distance in some jurisdictions). Suppression reflects the biological reality of pregnancy, not the constraint's coercive structure. Theater ratio (0.30): Low. The birth reading produces institutional coherence when bodily autonomy is treated as the trump principle. Theater rises to the degree that institutions compartmentalize (treating the fetus as clinically significant while morally null), but the core constraint mechanism (pregnant person holds decision-making authority) is functionally transparent. Medical bifurcation creates performance theater (prenatal care oriented toward fetal well-being despite status denial), but at modest level.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the pregnant person (Rope), the fetus (Snare), and the state (Tangled Rope) is maximal. The pregnant person experiences the constraint as enabling coordination around their autonomy and medical authority. The fetus experiences zero moral status — it is not even a victim in the classical sense because it has no claims the constraint could violate. The state experiences tangled coordination and extraction: it coordinates medical regulation efficiently under birth reading (avoiding the enforcement costs of fetal personhood) while simultaneously retaining power to set procedural limits and regulate the timing of abortion. The reproductive justice coalition experiences the constraint as scaffold — a real but vulnerable protection of autonomy. Medical institutions experience piton degradation: they must provide abortion services while maintaining obstetric specialty knowledge that implicitly treats the fetus as clinically significant, creating institutional contradiction. The analytical observer sees coherent institutional choice without false summitry. The key insight: the perspectival gap is not a failure of the framework but a feature of the reading itself — birth reading assigns dramatically different moral and legal status to the pregnant person (full authority) and the fetus (zero status), so different observers must classify the constraint differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. The pregnant person is the primary beneficiary (low d → negative effective extraction) with constrained exit (biographical cost of pregnancy duration, but legal abortion access). The liberal autonomy framework is institutional beneficiary with arbitrage exit (can shift jurisdictions, can reframe legal doctrine). The fetus has zero moral status under this reading and cannot be a victim of the constraint (it is simply outside the moral consideration set). The state is a mixed beneficiary (gains from coordination without fetal enforcement costs) and partial extractor (retains procedural regulation authority), producing moderate d and moderate chi. The reproductive justice coalition has organized power and mobile exit (can advocate, relocate, shift strategy), reducing experienced extraction. Medical institutions have institutional arbitrage and can compartmentalize, moderating extraction. The piton classification of medical institutions derives from theater ratio (0.30) exceeding threshold plus the contradiction between clinical fetal treatment and moral status denial — the constraint persists through institutional inertia despite the contradiction, not because it functions perfectly.
 *
 * MANDATROPHY ANALYSIS:
 *   Birth reading avoids mandatrophy by maintaining low extractiveness (0.35) and low theater (0.30). The constraint coordinates genuine benefits (pregnant person autonomy, state efficiency, medical authority) without imposing maximal coercion. The mandatrophy threat arises if theater increases — if the institutional bifurcation (clinical fetal status vs. moral status denial) becomes unsustainable and the medical system begins to treat the contradiction as unresolvable, theater_ratio can rise toward 0.70+ and the constraint can shift toward piton degradation or snare (if enforcement tightens). Temporal measurement across 50 years shows stable theater (0.25→0.30) and stable extractiveness (0.32→0.35), indicating the institutional equilibrium is holding. The analytical observer avoids the false summit by recognizing that birth reading is a deliberate framework choice, not a natural law of morality or biology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_grounding_contention,
    'What property grounds moral status — biological continuity (conception), viability (consciousness/brain function), birth (exit from maternal body), or postnatal capacities (breathing, feeding)?',
    'This is a CONCEPTUAL omega. No empirical discovery resolves which property ''correctly'' grounds moral status — different metaphysical frameworks (liberal autonomy, natural law, capability-based, potentiality-based) assign the grounding property differently. The birth reading chooses birth/bodily autonomy as the grounding criterion.',
    'This omega determines which sibling readings (conception_reading, viability_reading) coexist with birth_reading or foreclose it. If moral status grounding is accepted as genuinely contested rather than discoverable, birth_reading coexists with siblings. If grounding is treated as discoverable via neuroscience or potentiality analysis, readings may foreclose one another.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_grounding_contention, conceptual, 'What property grounds moral status — choice of criterion determines reading relations').

omega_variable(
    bodily_autonomy_vs_potentiality_conflict,
    'When bodily autonomy and fetal potentiality conflict, which normative principle takes priority in adjudicating moral claims?',
    'This is a PREFERENCE omega — different liberal and communitarian traditions weight these principles differently. Some weight bodily autonomy as trump (birth reading); others weight potentiality as trump (conception reading); others use viability as a compromise threshold (viability_reading). No framework-independent way to resolve; the reading chooses bodily autonomy as priority.',
    'Birth reading foreclose frames that treat potentiality as equal to autonomy from the start. But potentiality-first frameworks can coexist with birth_reading if they accept that potentiality does not override autonomy before birth. This omega clarifies whether the readings are logically incompatible or just different priority weightings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_potentiality_conflict, preference, 'Priority weighting of bodily autonomy vs. fetal potentiality — reading-dependent choice').

omega_variable(
    institutional_bifurcation_sustainability,
    'Can obstetric institutions sustainably maintain the contradiction between treating the fetus as a clinical entity (requiring specialized prenatal care) and denying it prenatal moral status (permitting abortion)?',
    'EMPIRICAL: Study institutional practice in high-autonomy jurisdictions (US post-Dobbs repeal, Canada, Scandinavia). Measure: (1) Can pregnant persons and physicians coordinate around shared clinical understanding of fetal development while maintaining moral status denial? (2) Does the compartmentalization produce decision-making errors, harm, or institutional degradation over time? (3) Do institutions explicitly frame fetal monitoring as maternal health care (resolving the contradiction) or as fetal protection (reinscribing moral status)?',
    'If contradiction is unsustainable, birth reading''s piton theater_ratio will rise over time, and the institutional perspective will shift toward snare or tangled_rope as the system breaks down. If sustainable, theater_ratio stabilizes. This is a key lifecycle measurement for birth_reading over generational timescales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_bifurcation_sustainability, empirical, 'Whether medical bifurcation (fetal as clinical entity + no moral status) can persist').

omega_variable(
    sibling_reading_logical_relations,
    'Do conception_reading, viability_reading, and birth_reading logically foreclose one another, or do they coexist as genuinely live alternative frameworks?',
    'CONCEPTUAL: Examine whether each reading''s core axiom (the grounding criterion for moral status) logically entails rejection of the others'' core axioms. If conception reading axiom (potentiality = moral status from conception) directly contradicts birth reading axiom (moral status acquired at birth), they foreclose. If they merely weight competing principles (autonomy vs. potentiality) differently, they coexist.',
    'This omega determines cs_structure.reading_relations values. If readings foreclose: ''forecloses'' relation; higher cross-reading conflict. If readings coexist: ''coexists_with'' relation; framework pluralism is coherent. The choice affects how the engine models the personhood_boundary kernel and its competing readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_relations, conceptual, 'Whether personhood-boundary siblings logically foreclose or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(birth_theater_t0, birth_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(birth_theater_t25, birth_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(birth_theater_t50, birth_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(birth_extract_t0, birth_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(birth_extract_t25, birth_reading, base_extractiveness, 25, 0.34).
narrative_ontology:measurement(birth_extract_t50, birth_reading, base_extractiveness, 50, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(birth_reading, identity_coordination).
narrative_ontology:affects_constraint(birth_reading, conception_reading).
narrative_ontology:affects_constraint(birth_reading, viability_reading).
narrative_ontology:affects_constraint(birth_reading, abortion_access_infrastructure).
narrative_ontology:affects_constraint(birth_reading, medical_authorization_and_consent).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three constraint stories: birth_reading (this file), conception_reading, and viability_reading. Each is a structurally distinct constraint with different epsilon values, beneficiary/victim structures, and classification profiles. They are linked via network.affects_constraints because the choice of one reading changes the operating environment and legitimacy conditions for the others. Birth reading instantiates bodily autonomy as the organizing principle; it influences (but does not foreclose) viability_reading, which can be framed as a compromise between autonomy and potentiality. Birth reading forecloses any framework that treats fetal potentiality as equal to pregnant person autonomy from conception forward.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
