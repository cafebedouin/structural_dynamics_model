% ============================================================================
% CONSTRAINT STORY: epistemic_automation_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_automation_dependency, []).

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
 *   constraint_id: epistemic_automation_dependency
 *   human_readable: Epistemic Automation Dependency
 *   domain: epistemology/technology/knowledge_systems
 *
 * SUMMARY:
 *   Epistemic Automation Dependency describes the structural constraint
 *   created when knowledge-seeking agents become locked into opaque automated
 *   systems that are simultaneously enabling and extractive. The constraint
 *   arises not from the mere existence of automation but from the specific
 *   institutional choices: platforms designed for opacity, opacity
 *   legitimized through performative transparency rituals, and user bases
 *   whose professional identities become fused with platform mastery. This
 *   constraint demonstrates the full spectrum of DR classification across
 *   seven perspectives, revealing how the same structural phenomenon can
 *   appear as a coordination mechanism (rope), a temporary institutional
 *   problem with sunset logic (scaffold), a degraded ritual (piton), a mixed
 *   extraction-coordination hybrid (tangled rope), pure extraction for the
 *   powerless (snare), and a false natural law (mountain). The extractiveness
 *   trajectory (0.22 → 0.58) over 25 years reflects accumulating dependency
 *   as platforms become institutionalized; theater ratio (0.35 → 0.75)
 *   reflects the growing gap between claimed transparency and actual
 *   interpretability. The constraint operates globally but enforcement is
 *   strongest at the national/institutional scale where competitive pressures
 *   force adoption.
 *
 * KEY AGENTS:
 *   - Knowledge Seekers: Primary victim (powerless/trapped) — dependent on platforms for epistemic access with no viable alternatives at scale
 *   - Disciplinary Experts: Primary victim (powerless/identity_locked) — structurally mobile but professionally fused with platform competence; exit would require identity abandonment
 *   - Research Institutions: Mixed beneficiary-victim (organized/constrained) — gain efficiency benefits but bear interpretability costs; constrained by competitive pressure
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture network effects, user lock-in, and data monopolies; low constraint cost from their perspective
 *   - Interpretability Infrastructure: Performative actor (institutional/arbitrage) — maintains transparency theater through regulatory compliance and institutional rituals without reducing opacity
 *   - Open Epistemic Coalition: Organized resistance (organized/constrained) — building alternative transparent methodologies; constrained by resource requirements and adoption barriers
 *   - Analytical Observer: Risk of naturalization (analytical/analytical) — temptation to frame automation dependency as immutable law rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_automation_dependency, 0.58).
domain_priors:suppression_score(epistemic_automation_dependency, 0.62).
domain_priors:theater_ratio(epistemic_automation_dependency, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_automation_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_automation_dependency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(epistemic_automation_dependency, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_automation_dependency, tangled_rope).
narrative_ontology:human_readable(epistemic_automation_dependency, "Epistemic Automation Dependency").
narrative_ontology:topic_domain(epistemic_automation_dependency, "epistemology/technology/knowledge_systems").

domain_priors:requires_active_enforcement(epistemic_automation_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_automation_dependency, automation_platform_operators).
narrative_ontology:constraint_beneficiary(epistemic_automation_dependency, efficiency_metrics_institutions).
narrative_ontology:constraint_victim(epistemic_automation_dependency, human_epistemic_reliability).
narrative_ontology:constraint_victim(epistemic_automation_dependency, interpretability_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNOWLEDGE SEEKER (SNARE) — Trapped by dependence on opaque automation systems for epistemic access. Cannot exit without abandoning modern knowledge practices. Bears full extraction cost: forced reliance on black-box outputs without interpretability. No alternative pathway exists at scale.
constraint_indexing:constraint_classification(epistemic_automation_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISCIPLINARY EXPERT (SNARE via identity_locked) — Structurally mobile (could reject automation) but identity-fused with epistemic competence as defined by platform mastery. Professional identity constituted through ability to operate within automated systems. Exit would require abandoning earned expertise and professional status. Experiences the constraint as immutable because their self-concept depends on the system.
constraint_indexing:constraint_classification(epistemic_automation_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: RESEARCH INSTITUTION (TANGLED ROPE) — Constrained by resource requirements and competitive pressure but also benefits from automation efficiency. Genuine coordination function: platforms enable large-scale research coordination. Asymmetric extraction: efficiency gains flow to platforms; interpretability costs borne by researchers. Active enforcement: institutions adopt systems through pressure to compete.
constraint_indexing:constraint_classification(epistemic_automation_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences automation as coordination mechanism. Benefits from network effects and user dependency. Arbitrage options: can redirect platform to alternative uses, license technology, integrate upstream. Low extraction from their perspective — they see the system solving coordination problems (matching queries to knowledge, scaling analysis).
constraint_indexing:constraint_classification(epistemic_automation_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERPRETABILITY THEATER (PITON) — Formal commitments to explainability, interpretability reports, and transparency documentation are substantially performative. The ritual persists through institutional inertia (regulatory compliance, institutional review boards) despite limited functional interpretability. Theater ratio (0.68) reflects that explanations often rationalize rather than illuminate. The system maintains appearance of transparency without reducing actual opacity.
constraint_indexing:constraint_classification(epistemic_automation_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN EPISTEMIC COALITION (SCAFFOLD) — Organized agents (open science movements, transparent methodology communities, epistemic peer production) are building alternative pathways: distributed peer review, replicable workflows, human-interpretable algorithmic design. These create sunset logic for the automation dependency — as transparent methodologies mature and become normalized, closed-box automation loses legitimacy. Constraint has bounded time horizon: 15-25 years for cultural shift toward epistemic transparency.
constraint_indexing:constraint_classification(epistemic_automation_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — Temptation to see automation dependency as inevitable law of knowledge systems: any sufficiently complex domain requires automation; humans cannot scale without tools; opacity is the price of power. This perspective naturalizes what is structurally contingent. The false summit frames extraction as inherent to epistemology rather than as a policy choice about how to structure automation adoption and governance.
constraint_indexing:constraint_classification(epistemic_automation_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_automation_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_automation_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_automation_dependency, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_automation_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_automation_dependency, TR),
    TR >= 0.70.

:- end_tests(epistemic_automation_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The measurement trajectory shows accumulating dependency as automation systems become institutionalized (0.22 → 0.58 over 25 years). This elevation reflects not technological inevitability but institutional choices: platforms designed for opacity, adoption driven by competitive pressure rather than epistemic benefit, and user bases whose alternatives shrink as the ecosystem consolidates. The starting value (0.22) represents early automation adoption when alternatives were viable; the current value reflects lock-in. Suppression (0.62): Moderate-high. Barriers to exit include: institutional requirements (universities mandate certain platforms), professional standards (expertise now defined by platform competence), economic dependency (career advancement tied to platform mastery), and cognitive capture (identity fusion with the system). Not absolute — some researchers do maintain parallel practices — but substantial. Theater ratio (0.68): High and increasing. Interpretability reports, explainability documentation, and transparency commitments are substantially performative. The reports explain *how* systems work (operationally) without illuminating *why* they produce specific outputs (epistemically). Theater has increased as platforms become larger and more complex — opacity grows while transparency claims expand, creating growing gap between claimed and actual interpretability.
 *
 * PERSPECTIVAL GAP:
 *   Seven perspectives, six classification types, maximum diagnostic value. The powerless/trapped agent sees snare (maximal extraction). The powerless/identity_locked agent also sees snare (but for cognitive reasons: exit is structurally available but experientially unavailable). The organized/constrained agent sees tangled rope (mixed coordination-extraction). The institutional beneficiary sees rope (coordination logic dominates). The institutional theater-keeper sees piton (performative degradation). The organized open science coalition sees scaffold (sunset logic real and structural). The civilizational analytical observer risks seeing mountain (false summit: naturalizing the contingent). This spectrum is diagnostic: it shows that automation dependency is not a uniform constraint but a presheaf — different structural readings depending on observer position. No single classification is 'correct'; all are accurate from their respective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Platform operators: beneficiaries with arbitrage exit → d ≈ 0.10 → low/negative f(d) → low chi. Research institutions: mixed (some benefits, constrained exit) → d ≈ 0.55 → f(d) ≈ 0.75 → moderate-high chi. Knowledge seekers: victims with trapped exit → d ≈ 0.92 → f(d) ≈ 1.35 → high chi. Disciplinary experts: victims but identity-locked exit (structurally mobile but psychologically trapped) → d ≈ 0.88 → f(d) ≈ 1.28. The identity_locked exit option is critical: it produces snare classification despite moderate d value, because the agent cannot exercise their theoretical mobility. The gap between identity_locked (d=0.88 → snare) and constrained (d=0.88 → tangled_rope at moderate power) reveals the identity-fusion mechanism — same structural metrics, different exit psychology, different experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION THROUGH PERSPECTIVAL ANALYSIS: The mandatrophy ('Is automation dependency coordination or extraction?') is resolved by showing it is BOTH — it is tangled rope from the institutional perspective (genuine coordination + asymmetric extraction), while appearing as snare to powerless individuals, rope to beneficiaries, and scaffold to organized resistance. The constraint does not have a single 'true' type; it has a presheaf of types indexed by observer position. The false mountain perspective (analytical/civilizational seeing it as inherent law) is revealed through the contrast: if it were truly immutable, why do different observers at the same factual moment see different types? The spectrum of classifications shows the constraint is contingent institutional design choice, not natural law. The mandatrophy resolves when we stop asking 'which type?' and ask instead 'what structural conditions would make each perspective's classification accurate?' That question reveals the institutional decisions that produce the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_threshold,
    'Is the opacity of modern automation systems technically necessary or institutionally chosen?',
    'Empirical comparison of interpretability-by-design systems against closed-box systems controlling for task complexity. Examine whether interpretable alternatives exist at equal performance.',
    'If technical necessity: opacity is structural feature of knowledge systems (supports mountain classification). If institutional choice: opacity is extraction mechanism (supports snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_threshold, empirical, 'Whether automation opacity is technically necessary or institutionally chosen').

omega_variable(
    identity_lock_persistence,
    'When disciplinary experts exit automated systems, do they retain identity-fusion with the constraint or achieve cognitive liberation?',
    'Longitudinal study of experts who have rejected automation adoption. Track whether they report identity crisis or identity relief; measure epistemic confidence before/after exit.',
    'If identity fusion persists: identity_locked classification is accurate, and cognitive capture is severe. If identity relief: exit was merely constrained (high cost), not identity-locked.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Persistence of identity fusion after expert exit from automation').

omega_variable(
    interpretability_theater_functionality,
    'Do interpretability reports and explainability documentation produce measurable changes in user understanding or decision-making quality?',
    'A/B testing: user comprehension and decision quality with vs. without interpretability explanations. Measure whether explanations reduce overconfidence or improve error detection.',
    'If functional: theater ratio should be lower, and explanation systems have real coordination value. If performative: theater ratio of 0.68+ is accurate, and interpretability is institutional ritual.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretability_theater_functionality, empirical, 'Whether interpretability documentation produces functional understanding').

omega_variable(
    alternative_epistemic_viability,
    'Can open, transparent, human-interpretable epistemic systems scale to handle real-world knowledge work at competitive speed and breadth?',
    'Implementation experiments: deploy interpretable alternatives to closed-box systems in controlled domains. Measure performance, adoption, and institutional friction.',
    'If viable: scaffold sunset is real and structural. Automation dependency is temporary policy choice. If not viable: dependency may be snare with no exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemic_viability, empirical, 'Scalability and viability of transparent epistemic alternatives').

omega_variable(
    extraction_vs_coordination_balance,
    'What proportion of platform benefits flows to operators vs. users? Is the system near-coordinative (Rope) or significantly extractive (Snare/Tangled Rope)?',
    'Economic analysis: capture ratio between platform revenue/valuation growth and measurable user epistemic benefit. Track whether users have alternative providers or are vendor-locked.',
    'If capture > 70%: system is primarily extractive (Snare). If capture < 40%: system is primarily coordinative (Rope). 40-70% supports Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_balance, empirical, 'Distribution of benefits between platform operators and users').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_automation_dependency, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epaut_tr_t0, epistemic_automation_dependency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(epaut_tr_t7, epistemic_automation_dependency, theater_ratio, 7, 0.52).
narrative_ontology:measurement(epaut_tr_t15, epistemic_automation_dependency, theater_ratio, 15, 0.68).
narrative_ontology:measurement(epaut_tr_t22, epistemic_automation_dependency, theater_ratio, 22, 0.75).

% Extraction over time
narrative_ontology:measurement(epaut_be_t0, epistemic_automation_dependency, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(epaut_be_t7, epistemic_automation_dependency, base_extractiveness, 7, 0.4).
narrative_ontology:measurement(epaut_be_t15, epistemic_automation_dependency, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(epaut_be_t22, epistemic_automation_dependency, base_extractiveness, 22, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_automation_dependency, information_standard).
narrative_ontology:boltzmann_floor_override(epistemic_automation_dependency, 0.12).
narrative_ontology:affects_constraint(epistemic_automation_dependency, algorithmic_opacity_legitimacy).
narrative_ontology:affects_constraint(epistemic_automation_dependency, expert_knowledge_lock_in).
narrative_ontology:affects_constraint(epistemic_automation_dependency, institutional_transparency_theater).

% DUAL FORMULATION NOTE:
% Epistemic automation dependency is upstream of domain-specific knowledge automation constraints (search, translation, synthesis, recommendation). Each domain constraint exhibits its own extractiveness reflecting local institutional arrangements; the epistemic automation dependency represents the structural general form. The three downstream constraints inherit the family's perspectival structure but specialize it to particular knowledge domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_automation_dependency, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
