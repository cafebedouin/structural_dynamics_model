% ============================================================================
% CONSTRAINT STORY: social_tribalism_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_tribalism_amplification, []).

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
 *   constraint_id: social_tribalism_amplification
 *   human_readable: Social Tribalism Amplification Through Information Networks
 *   domain: social_dynamics/information_systems/polarization
 *
 * SUMMARY:
 *   Social tribalism amplification is a constraint that emerges at the
 *   intersection of human social psychology and information network
 *   architecture. The constraint operates on two overlapping levels: (1)
 *   genuine human capacity for tribal bonding and in-group cooperation, which
 *   solves real coordination problems but also enables out-group
 *   exploitation; and (2) algorithmic amplification mechanisms in digital
 *   platforms that optimize for engagement by rewarding tribal content,
 *   outrage, and identity-reinforcing narratives. The constraint exhibits a
 *   marked intensification over the 10-year interval (base extractiveness
 *   rising from 0.28 to 0.58, theater ratio from 0.35 to 0.58), indicating
 *   that the feedback loop between tribal psychology and algorithmic
 *   amplification has accelerated. This creates a tangled hybrid: genuine
 *   tribal coordination functions coexist with extraction mechanisms that
 *   concentrate benefits to tribal leaders, platform operators, and
 *   attention-extracting actors while imposing costs on epistemic commons,
 *   cross-tribal understanding, and individual cognitive autonomy. The
 *   constraint is not a pure extraction (Snare) because tribal formation does
 *   solve real coordination and identity problems. It is not pure
 *   coordination (Rope) because the extraction component is significant and
 *   asymmetric. It is a Tangled Rope: requiring active enforcement
 *   (algorithmic amplification, tribal incentive structures, social
 *   punishment for defection) while serving genuine coordination functions.
 *
 * KEY AGENTS:
 *   - Isolated Individuals: Primary victims (powerless/trapped) — caught in algorithmic echo chambers with minimal exit options; identity fused with tribe
 *   - Bridge-Builders: Secondary victims (moderate/constrained) — bear costs of cross-tribal dialogue (social punishment, emotional labor) while generating epistemic benefits; constrained but not trapped
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract engagement, attention, and data while coordinating network effects; high exit options
 *   - Tribal Political Leaders: Secondary beneficiaries (powerful/mobile) — extract status, authority, funding while coordinating genuine tribal interests; identity-fused despite mobility
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good that bears costs of tribal polarization and belief degradation; no exit, no advocacy
 *   - Civic Institutions: Institutional actors (institutional/constrained) — once bridged tribalism but now largely performative; maintaining theater while function atrophies
 *   - Deliberative Democracy Movements: Organized agents (organized/mobile) — attempting to build structural alternatives with sunset logic; have exit strategies and vision for their replacement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_tribalism_amplification, 0.58).
domain_priors:suppression_score(social_tribalism_amplification, 0.65).
domain_priors:theater_ratio(social_tribalism_amplification, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_tribalism_amplification, extractiveness, 0.58).
narrative_ontology:constraint_metric(social_tribalism_amplification, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(social_tribalism_amplification, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_tribalism_amplification, tangled_rope).
narrative_ontology:human_readable(social_tribalism_amplification, "Social Tribalism Amplification Through Information Networks").
narrative_ontology:topic_domain(social_tribalism_amplification, "social_dynamics/information_systems/polarization").

domain_priors:requires_active_enforcement(social_tribalism_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_tribalism_amplification, attention_extractors).
narrative_ontology:constraint_beneficiary(social_tribalism_amplification, tribal_leaders).
narrative_ontology:constraint_beneficiary(social_tribalism_amplification, engagement_algorithms).
narrative_ontology:constraint_victim(social_tribalism_amplification, epistemic_commons).
narrative_ontology:constraint_victim(social_tribalism_amplification, cross_tribal_understanding).
narrative_ontology:constraint_victim(social_tribalism_amplification, individual_cognitive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED INDIVIDUAL (SNARE) — A person embedded in a dominant tribal echo chamber with no material exit from the information environment. Trapped by algorithmic feed design, social peer pressure, and identity fusion with the tribe. Bears full cost of tribal commitment (cognitive rigidity, social isolation from out-tribe, vulnerability to tribal narratives). No viable exit path without complete relocation or identity destruction.
constraint_indexing:constraint_classification(social_tribalism_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BRIDGE-BUILDER (TANGLED ROPE) — A person with some cognitive and social resources attempting cross-tribal dialogue. Benefits from coordination (genuine relationships across tribal lines, epistemic diversity, mutual problem-solving) while bearing extraction costs (social punishment from home tribe, professional risk, emotional labor of maintaining relationships under tribal pressure). Constrained by social dependency and career concerns, but not completely trapped.
constraint_indexing:constraint_classification(social_tribalism_amplification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the tribalism constraint as a coordination mechanism: tribal formation enables network effects, user retention, and community building. Benefits from algorithmic amplification of tribal content through engagement metrics. The constraint enables their primary function (connecting people) while also extracting value (attention, data, engagement). Has exit options through API changes, algorithm modifications, or policy shifts.
constraint_indexing:constraint_classification(social_tribalism_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRIBAL POLITICAL LEADER (TANGLED ROPE) — Coordinates genuine collective action and shared identity (coordination function: mobilizing voters, building coalitions, creating political power) while extracting personal status, funding, and authority. Mobile in principle but identity-fused with the tribe — their career and identity are constituted through tribal leadership. Benefits from tribalism amplification but also genuinely coordinates tribal interests.
constraint_indexing:constraint_classification(social_tribalism_amplification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVIC INSTITUTION (PITON) — Traditional institutional structures (local civic organizations, religious institutions, unions) once coordinated cross-tribal understanding and shared civic identity. These institutions are now largely performative — their coordination function has atrophied while the theater of civic participation persists. Theater ratio high due to the mismatch between institutional form and actual function. Maintained through inertia rather than efficacy.
constraint_indexing:constraint_classification(social_tribalism_amplification, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DELIBERATIVE DEMOCRACY MOVEMENT (SCAFFOLD) — Organized civil society actors building alternative structures (citizens' assemblies, structured dialogue programs, epistemic friction platforms) that reduce tribalism amplification through institutional design. See the tribalism constraint as temporary, solvable through better communication infrastructure and deliberative processes. Constrained by resource limitations but mobile — they can pivot strategies. Clear sunset clause: if deliberative institutions scale, they reduce amplification mechanisms.
constraint_indexing:constraint_classification(social_tribalism_amplification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, human tribalism is both a genuine coordination mechanism (enabling in-group cooperation, identity, belonging) and an extraction mechanism (enabling out-group exploitation, zero-sum conflict, authority capture). The constraint amplifies both simultaneously. The analytical perspective sees genuine coordination function (tribal bonding solves real coordination problems) alongside asymmetric extraction (out-tribe members and epistemic commons bear costs, in-tribe leaders extract authority and status).
constraint_indexing:constraint_classification(social_tribalism_amplification, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_tribalism_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_tribalism_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_tribalism_amplification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_tribalism_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_tribalism_amplification, TR),
    TR >= 0.70.

:- end_tests(social_tribalism_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts significant value to tribal leaders and platform operators (status, authority, attention, data) while imposing costs on out-tribe members and epistemic commons. However, the extractiveness is not as severe as a pure Snare (0.66+) because genuine tribal coordination functions persist — tribal bonding does solve identity and cooperation problems. The rising trajectory from 0.28 to 0.58 reflects algorithmic amplification layering extraction mechanisms on top of natural tribalism, indicating that the extraction component is increasing while the coordination component persists. Suppression (0.65): Moderate-high. Barriers to exit include algorithmic feed design (difficult to avoid tribal content), social peer pressure and exclusion threats, identity fusion (exit requires identity death), career dependencies on tribal networks, and geographic clustering. These are substantial but not total — some exit is possible at high cost. Theater ratio (0.58): Moderate-high and rising. A significant portion of tribal performance is performative — virtue signaling within the tribe, public commitment displays, ritualized outrage cycles, identity-affirming content sharing. However, coordination functions remain genuine (actual resource allocation, political mobilization, emotional support). The rising theater ratio suggests that performative content is increasingly amplified relative to functional content.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps reveal why single-position analysis fails for tribalism constraints. An institutional actor (platform operator) sees a well-functioning coordination mechanism with manageable extraction — engagement optimization aligns with user preferences and network effects. A powerless actor sees pure extraction — they have no choice in tribal membership, face algorithmic manipulation, and bear identity costs. Both are seeing the same constraint structure. Neither is wrong. The constraint is genuinely both coordination and extraction; which one dominates depends entirely on where you sit in the power structure. The gap between institutional (Rope) and powerless (Snare) perspectives is diagnostic: it signals that the coordination function is real but benefits are concentrating, and extraction is real and asymmetrically distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators (institutional/arbitrage) benefit from tribal amplification because engagement optimization directly serves platform function (network effects, retention, ad targeting). Their exit options are high (they can modify algorithms, change policies, shift to different business models) — they hold power over the mechanism. Derived d ≈ 0.10, f(d) ≈ -0.05, producing χ ≈ 0.58 × (-0.05) × 1.2 ≈ negative or near-zero from their perspective. They experience the constraint as beneficial coordination. Tribal political leaders (powerful/mobile) benefit from tribalism amplification and coordinate genuine collective action. Their exit options are mobile (they could depolarize, adopt bridging strategies) but identity-locked — they've built their career on tribal leadership. Derived d ≈ 0.35, f(d) ≈ 0.35, producing moderate effective extraction from their perspective. Isolated individuals (powerless/trapped) are victims of tribal amplification with no exit. Algorithmic filters trap them in confirming environments; social peer pressure enforces tribal identity; material dependencies (job networks, housing, family) are tribally embedded. Derived d ≈ 0.95, f(d) ≈ 1.42, producing χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 — they experience maximum effective extraction. Bridge-builders (moderate/constrained) bear costs of cross-tribal dialogue (social punishment, career risk) but benefit from epistemic diversity and genuine relationships. Derived d ≈ 0.65, f(d) ≈ 1.00, producing moderate effective extraction. The epistemic commons is a victim with no agent — it bears costs (belief degradation, polarization, loss of shared epistemic standards) but cannot resist, organize, or exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy surfaces here through the coordinate variation of coordination and extraction across perspectives. From the institutional position, tribalism is pure coordination (network effects, user engagement) with minimal extraction. From the powerless position, it is pure extraction (identity trap, algorithmic lock, information ecosystem capture). From the analytical position, it is genuine hybrid: tribalism solves real coordination problems (identity, in-group cooperation, collective action) while simultaneously extracting status, authority, and attention from those who participate. The constraint cannot be classified as a single type because its structure is genuinely two-faced: one face provides real coordination benefits (tribal bonding, collective efficacy, shared identity), the other imposes real extraction costs (polarization, out-group dehumanization, epistemic lock). The tangled rope classification correctly captures this: the constraint requires active enforcement (algorithmic amplification, tribal incentive structures, social punishment for defection) precisely because it must continually reproduce both functions simultaneously. If enforcement relaxed, the coordination function would persist (humans naturally form tribes) but the extraction asymmetry would erode as bridge-building became costless. If enforcement increased, extraction would intensify but coordination function might collapse into pure dominance. The mandatrophy resolves by recognizing that the constraint is legitimately both — neither function is an epiphenomenon of the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_intentionality,
    'Is tribalism amplification a deliberate extraction mechanism designed into platforms, or an unintended emergent consequence of engagement optimization?',
    'Internal platform documentation, algorithmic audits, design decision history, A/B testing logs comparing engagement with tribal vs bridging content',
    'If deliberate: platforms are principal extractors (institutional power + arbitrage exit), snare classification strengthens. If unintended: extraction is mechanism not intent, reducing moral culpability but not structural extraction. Classification likely stable either way (tangled rope to snare spectrum).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_intentionality, empirical, 'Whether tribalism amplification is deliberately engineered or emergent').

omega_variable(
    identity_lock_irreversibility,
    'For individuals in deep tribal echo chambers, is the suppression structural (removable by changing environment) or internalized (persisting after environment change)?',
    'Longitudinal studies of individuals who exit tribal environments; measurement of belief persistence and identity revision post-exit; comparison of exit barriers before and after relocation',
    'If structural: individuals classified as trapped can become mobile through relocation; suppression value overstates binding force. If internalized: identity_locked classification justified; individuals carry suppression with them; effective suppression higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_irreversibility, empirical, 'Whether tribal identity lock is structural or internalized suppression').

omega_variable(
    coordination_function_reality,
    'Does tribal formation in modern information environments actually solve coordination problems (genuine Rope function), or does it primarily enable extraction and conflict?',
    'Comparative analysis of tribal coordination success rates vs cross-tribal coordination; measurement of actual collective action outcomes by tribalism index; historical comparison with pre-digital tribal structures',
    'If genuine coordination: tangled rope classification justified; suppression is coordination cost. If primarily extraction: snare classification more accurate; suppression is coercion, not coordination cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_reality, empirical, 'Whether modern tribalism solves real coordination problems').

omega_variable(
    deliberative_scalability,
    'Can deliberative democracy and structured dialogue institutions scale to interrupt tribalism amplification at platform scale, or do they remain marginal relative to algorithmic engagement incentives?',
    'Measurement of deliberative institution adoption and reach; correlation between deliberative participation and reduced tribal polarization; cost and time comparison with algorithmic scaling',
    'If scalable: scaffold classification confirmed; sunset clause is real (10-20 year horizon). If marginal: scaffold is aspirational rather than structural; tribalism amplification persists as tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deliberative_scalability, empirical, 'Whether deliberative institutions can scale against algorithmic tribalism').

omega_variable(
    exit_cost_measurement,
    'What are the actual measurable costs of exit for individuals and communities embedded in tribal structures (relocation, social isolation, career damage, identity reconstruction)?',
    'Survey data on exit costs; qualitative interviews with bridge-builders and exit-completers; analysis of social network disruption and economic consequences',
    'If costs are very high (>50% income loss, complete social isolation): trapped classification justified for powerless agents. If moderate (career setback, partial isolation): constrained classification more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Quantified exit costs for tribal members').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_tribalism_amplification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tribalism_tr_t0, social_tribalism_amplification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tribalism_tr_t3, social_tribalism_amplification, theater_ratio, 3, 0.42).
narrative_ontology:measurement(tribalism_tr_t6, social_tribalism_amplification, theater_ratio, 6, 0.55).
narrative_ontology:measurement(tribalism_tr_t10, social_tribalism_amplification, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(tribalism_be_t0, social_tribalism_amplification, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tribalism_be_t3, social_tribalism_amplification, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(tribalism_be_t6, social_tribalism_amplification, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(tribalism_be_t10, social_tribalism_amplification, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_tribalism_amplification, identity_coordination).
narrative_ontology:affects_constraint(social_tribalism_amplification, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(social_tribalism_amplification, political_polarization_lock_in).
narrative_ontology:affects_constraint(social_tribalism_amplification, epistemic_commons_degradation).

% DUAL FORMULATION NOTE:
% Social tribalism amplification decomposes into three structurally distinct constraint stories: (1) tribalism_natural_human (ε=0.05, Rope) — the genuine human capacity for tribal bonding and in-group coordination; (2) algorithmic_amplification_tribalism (ε=0.72, Snare) — platform design choices that explicitly reward tribal engagement; (3) social_tribalism_amplification (ε=0.58, Tangled Rope) — the hybrid where both functions coexist. This story addresses the hybrid. Natural tribalism (ε=0.05) influences both algorithmic amplification (ε=0.72, downstream) and the hybrid (ε=0.58). Algorithmic amplification influences the hybrid. These three stories form a family; all members link to each other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_tribalism_amplification, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
