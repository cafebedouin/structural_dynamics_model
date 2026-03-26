% ============================================================================
% CONSTRAINT STORY: cognitive_liberty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_liberty_defense, []).

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
 *   constraint_id: cognitive_liberty_defense
 *   human_readable: Cognitive Liberty Defense: The Right to Mental Autonomy Under Coercive Influence
 *   domain: cognitive_science/ethics/governance
 *
 * SUMMARY:
 *   Cognitive liberty defense refers to the structural constraint created by
 *   systematic manipulation of cognitive processes through information
 *   infrastructure, algorithmic targeting, behavioral nudging, and
 *   neurotechnological intervention. This constraint pits individual and
 *   collective autonomy against extraction mechanisms operated by
 *   institutional actors (platforms, advertisers, state actors) with
 *   asymmetric control over information environments and behavioral
 *   manipulation infrastructure. The constraint has intensified dramatically
 *   over the past 15 years as algorithmic targeting, attention harvesting,
 *   and behavioral nudging have scaled from narrow marketing applications to
 *   population-level information warfare. The seven perspectives reveal the
 *   full structural spectrum: powerless individuals trapped in manipulative
 *   environments; populations with identity-fused acceptance of the
 *   constraint; platforms that coordinate genuine communication services
 *   while extracting attention and behavioral data; regulatory beneficiaries
 *   who profit from the targeting infrastructure; organized advocates
 *   fighting for cognitive liberty; degraded privacy frameworks that perform
 *   protection without providing it; and the analytical observer who sees the
 *   constraint as a fundamental assault on autonomy.
 *
 * KEY AGENTS:
 *   - Individual Cognitive Agents: Primary victims (powerless/trapped) — subject to sustained targeting, dark patterns, and manipulation; awareness of extraction is itself suppressed
 *   - Populations Exposed to Generational Targeting: Primary victims (powerless/identity_locked) — internalized manipulated framing as authentic preference; exit requires abandoning constructed identity
 *   - Platform Architecture Providers: Primary beneficiary (institutional/constrained) — extract attention and behavioral data; provide genuine coordination goods; constrained because business model restructuring would be expensive
 *   - Advertising and Data Networks: Secondary beneficiary (institutional/arbitrage) — profit from targeting infrastructure; high exit flexibility through market substitution
 *   - Regulatory Capture Beneficiaries: Tertiary beneficiary (institutional/arbitrage) — incumbent platforms with regulatory moats preventing new entrants with different cognitive targeting models
 *   - Cognitive Liberty Defense Movement: Organized advocates (organized/constrained) — see both coordination goods and extractive overlay; constrained by need to work through existing institutions
 *   - Legacy Privacy Framework: Institutional actor (institutional/arbitrage) — maintains performative protection; persists through inertia despite inadequacy to cognitive targeting mechanisms
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees full extraction architecture operating across information environments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_liberty_defense, 0.58).
domain_priors:suppression_score(cognitive_liberty_defense, 0.65).
domain_priors:theater_ratio(cognitive_liberty_defense, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_liberty_defense, extractiveness, 0.58).
narrative_ontology:constraint_metric(cognitive_liberty_defense, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cognitive_liberty_defense, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_liberty_defense, snare).
narrative_ontology:human_readable(cognitive_liberty_defense, "Cognitive Liberty Defense: The Right to Mental Autonomy Under Coercive Influence").
narrative_ontology:topic_domain(cognitive_liberty_defense, "cognitive_science/ethics/governance").

domain_priors:requires_active_enforcement(cognitive_liberty_defense).
% --- Structural relationships ---
narrative_ontology:constraint_victim(cognitive_liberty_defense, individual_cognitive_agents).
narrative_ontology:constraint_victim(cognitive_liberty_defense, epistemic_autonomy).
narrative_ontology:constraint_victim(cognitive_liberty_defense, informed_decision_making).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COGNITIVELY TARGETED INDIVIDUAL (SNARE) — Subject to sustained manipulation through algorithmic feeds, dark patterns, microtargeted disinformation, or neurotechnologies. Cannot easily identify the extraction mechanism. Trapped because awareness itself is suppressed; the very tools of exit (information, reflection, social support) are compromised. Maximum experienced extraction because the agent bears the cost of degraded autonomy while the targeting infrastructure benefits.
constraint_indexing:constraint_classification(cognitive_liberty_defense, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COGNITIVELY CONDITIONED POPULATION (SNARE, IDENTITY_LOCKED) — Over generational timescales, populations exposed to persistent cognitive targeting internalize the framing. Identity fusion occurs: citizens come to believe the manipulated narratives reflect their authentic preferences. Exit would require abandoning not just the specific manipulation but the identity constructed through generations of exposure. The binding is no longer purely extractive pressure but cognitive self-conception — yet the snare classification persists because the underlying extraction mechanism (concentration of cognitive control) remains.
constraint_indexing:constraint_classification(cognitive_liberty_defense, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM ARCHITECTURE PROVIDER (TANGLED ROPE) — Provides genuine coordination: information distribution, social connection, content discovery. These are real coordination goods. Simultaneously extracts through attention harvesting, behavioral nudging, and algorithmic ranking that privileges engagement over accuracy. Requires active enforcement of the extraction mechanism (algorithmic optimization, data collection, ranking algorithms). Constrained exit because the platform is economically embedded in digital ecosystems; abandoning cognitive targeting would require fundamental business model restructuring.
constraint_indexing:constraint_classification(cognitive_liberty_defense, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY CAPTURE BENEFICIARY (ROPE) — Industry actors, advertising networks, and incumbent platforms experience the cognitive liberty constraint as pure coordination: technical standards for data interoperability, targeting methodologies, and measurement. From their position, the constraint solves a genuine collective action problem (preventing chaotic fragmentation of targeting infrastructure). Arbitrage exit because these actors can switch between different targeting platforms or adapt to regulatory changes with manageable cost. Net extraction runs toward these beneficiaries.
constraint_indexing:constraint_classification(cognitive_liberty_defense, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: COGNITIVE LIBERTY DEFENSE MOVEMENT (TANGLED ROPE) — Organized agents (digital rights advocates, neurorights activists, regulatory bodies) recognize both the coordination goods (digital platforms have enabled genuine communication) and the extractive overlay (attention harvesting, behavioral manipulation). They see the constraint as solvable through governance (data protection regulation, algorithmic transparency, neurotechnological oversight). Constrained exit because movement actors operate within existing political and economic institutions; transforming cognitive targeting requires coordinating across state, corporate, and technical actors.
constraint_indexing:constraint_classification(cognitive_liberty_defense, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY PRIVACY FRAMEWORK (PITON) — Traditional privacy law (GDPR, CCPA) was designed for data protection in an era of static files and explicit transactions. Applied to cognitive targeting via algorithmic feeds and real-time behavioral inference, the framework is largely performative: users check a box consenting to terms they cannot parse; data minimization requirements are circumvented through aggregate collection; transparency reports document what the public cannot understand. The framework persists through institutional inertia despite dramatically reduced functional verification. Theater ratio is high because compliance is about appearing to respect privacy, not about preventing cognitive extraction.
constraint_indexing:constraint_classification(cognitive_liberty_defense, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL VIEW (SNARE) — From a civilizational scope, cognitive liberty is a fundamental structural good — the capacity to form preferences, beliefs, and identities free from systematic coercive influence. The constraint operates universally wherever information is mediated through infrastructure controlled by actors with incentives misaligned from recipient autonomy. The analytical observer sees the full extraction architecture: systematic targeting of cognitive vulnerabilities, manipulation of information environments, behavioral nudging optimized for extractive goals (advertising revenue, political mobilization, behavioral control) rather than recipient welfare. This is not inherent to information technology but rather to ownership and incentive structures that make extraction the dominant strategy.
constraint_indexing:constraint_classification(cognitive_liberty_defense, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_liberty_defense_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_liberty_defense, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_liberty_defense, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cognitive_liberty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_liberty_defense, TR),
    TR >= 0.70.

:- end_tests(cognitive_liberty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting sustained targeting of cognitive vulnerabilities with measurable effects on preferences, beliefs, and behaviors. The constraint is not total (humans retain agency) but systematic and difficult to escape without infrastructure-level changes. Theater ratio (0.48): Moderate, below the Piton threshold. Cognitive targeting operates through actual algorithmic mechanisms (not purely performative) but includes significant theater: privacy disclosures that mask rather than reveal targeting; 'preferences' that users believe are autonomous but are shaped by nudges; 'transparency' reports that document what observers cannot understand. The constraint has intensified because algorithmic capability has outpaced user awareness and regulatory response. Suppression (0.65): Moderate-high. Multiple barriers to cognitive liberty: asymmetric information (users don't know what's being targeted or how); technical opacity (algorithms are proprietary); economic barriers (leaving platforms imposes coordination costs on networks); psychological barriers (internalized framing makes exit psychologically difficult); institutional barriers (regulatory frameworks protect incumbent platforms). Not total suppression because awareness campaigns, regulatory proposals, and technical countermeasures exist; but barriers are substantial.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap appears between the trapped individual (Snare at biographical scale) and the platform provider (Tangled Rope). The individual experiences pure extraction because their cognitive processes are targeted without consent or understanding; the platform experiences mixed coordination (real social good) and extraction (behavioral data harvesting). This gap reflects asymmetric information and asymmetric control: the platform knows what it is doing; the individual does not. Secondary gap appears between legacy privacy frameworks (Piton — performative) and the actual extraction architecture (Snare — substantive). Privacy law creates theater that masks the constraint rather than addressing it. The organized defense movement (Tangled Rope) sees a path to reduced extraction through governance, while the analytical observer (Snare at universal scale) sees the constraint as structural to information asymmetries and will persist unless fundamentally restructured.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain produces high d (victim) for powerless agents with trapped exit — they bear extraction without alternative. Generational exposure creates identity_locked status: d is derived from victim status + identity fusion, producing a stable lock that appears immutable at biographical scale. Institutional platforms derive d from beneficiary status + constrained exit (business model dependence) — moderate extraction experienced from the platform's perspective because they coordinate genuine goods alongside extraction. Beneficiaries with arbitrage options (advertising networks, regulatory incumbents) have low d because they can exit the cognitive targeting regime if extraction becomes unprofitable. Organized advocates have moderate d (constrained exit through institutional resistance) but recognize both benefits and harms, producing Tangled Rope rather than pure Snare. The analytical observer's d derives from the universal scope and analytical power — they see the full extraction mechanism but cannot directly intervene.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly distinguishing coordination goods (information distribution, social connection, content discovery) from extraction mechanisms (attention harvesting, behavioral manipulation, cognitive targeting). The platform perspective (Tangled Rope) acknowledges both — the constraint genuinely coordinates while simultaneously extracting. The snare perspectives (individual, population, analytical) recognize that the extraction is primary and the coordination benefit flows disproportionately to institutional actors, not to the targets. The classification resolves the semantic tension: 'cognitive liberty defense' is not about preventing information technology coordination but about preventing the extraction overlay. The Piton classification (legacy privacy) reveals the false solution trap — appearing to address cognitive liberty while actually preserving extraction. The constraint is not mislabeled as coordination (which would be mandatrophy) nor as pure extraction without recognizing real coordination goods (which would also be false). The Snare from powerless/trapped perspective is the classification that captures the structural reality for most agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_effect_distinction,
    'Does cognitive liberty constraint require demonstrable intent to manipulate or does systematic effect suffice?',
    'Case law analysis of liability standards; empirical studies of algorithmic effects independent of platform intent documentation; neuroscientific assessment of manipulation thresholds',
    'If intent required: platforms can evade liability through architectural opacity and plausible deniability. If effect sufficient: much larger design space falls under constraint; classification remains Snare. Current doctrine leans intent, potentially misclassifying high-extraction mechanisms as low-harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_effect_distinction, empirical, 'Whether cognitive liberty violations require demonstrable intent or can be established through effect').

omega_variable(
    neurotechnological_escalation,
    'As neurotechnologies (brain-computer interfaces, real-time neural monitoring, direct neural manipulation) mature, does the constraint shift from Snare to something structurally different?',
    'Longitudinal tracking of neurotechnological capabilities; assessment of whether neural-level targeting creates fundamentally new extraction mechanisms vs. scaling existing ones; neurorights legislative framework development',
    'If scaling: classification remains Snare with higher extractiveness. If fundamentally new: may require new constraint type designation. Current framework may become inadequate at neural intervention scales.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(neurotechnological_escalation, empirical, 'Whether neurotechnological targeting creates fundamentally new extraction mechanisms').

omega_variable(
    population_heterogeneity_in_vulnerability,
    'Does cognitive liberty constraint apply uniformly across populations or do age, cognitive development, mental health status, and educational background create structurally distinct vulnerability profiles?',
    'Demographic analysis of targeting effectiveness across age cohorts, cognitive profiles, and socioeconomic groups; assessment of whether high-vulnerability subpopulations face classification closer to Mountain (inescapable) vs. Rope (coordinate-able)',
    'If uniform: single constraint story is appropriate. If heterogeneous: may require decomposition into separate stories per vulnerability class, each with different extractiveness and exit options. Current analysis may mask differential vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(population_heterogeneity_in_vulnerability, empirical, 'Whether cognitive liberty constraint affects all populations uniformly or creates stratified vulnerability').

omega_variable(
    collective_vs_individual_targeting,
    'Is cognitive liberty constraint fundamentally about individual autonomy or collective epistemic health? Does the distinction change the classification?',
    'Philosophical analysis of autonomy concepts (individual decision-making vs. collective epistemic commons); empirical assessment of whether individual-level interventions (e.g., media literacy) can protect against collective-level manipulation (polarization, coordination failures)',
    'If individual: exit is theoretically possible through personal vigilance and cognitive discipline. If collective: individual exit is insufficient; constraint approaches Mountain (inescapable unless the entire information environment changes). Current framing emphasizes individual agency, potentially understating extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_vs_individual_targeting, conceptual, 'Whether cognitive liberty is fundamentally individual or collective phenomenon').

omega_variable(
    regulation_effectiveness_threshold,
    'Is there a regulatory intensity threshold above which cognitive liberty constraints can be reduced to Rope or Scaffold, or does the profit motive for cognitive targeting always regenerate extractive mechanisms?',
    'Comparative analysis of regulatory regimes (EU, China, USA) and their effect on targeting intensity; assessment of compliance costs vs. continued extraction revenue; longitudinal tracking of workarounds when specific targeting methods are prohibited',
    'If threshold exists and is achievable: organized defense movement (Perspective 5) can reach meaningful outcome. If regenerative: constraints persist as Snare indefinitely. Current policy assumes threshold exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_effectiveness_threshold, empirical, 'Whether cognitive targeting extraction can be regulated below critical extractiveness threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_liberty_defense, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cogn_tr_t0, cognitive_liberty_defense, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cogn_tr_t5, cognitive_liberty_defense, theater_ratio, 5, 0.38).
narrative_ontology:measurement(cogn_tr_t10, cognitive_liberty_defense, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cogn_be_t0, cognitive_liberty_defense, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cogn_be_t5, cognitive_liberty_defense, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cogn_be_t10, cognitive_liberty_defense, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_liberty_defense, information_standard).
narrative_ontology:affects_constraint(cognitive_liberty_defense, algorithmic_opacity).
narrative_ontology:affects_constraint(cognitive_liberty_defense, attention_economy_concentration).
narrative_ontology:affects_constraint(cognitive_liberty_defense, neurorights_protection).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognitive_liberty_defense, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
