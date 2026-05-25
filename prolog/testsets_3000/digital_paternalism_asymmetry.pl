% ============================================================================
% CONSTRAINT STORY: digital_paternalism_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_paternalism_asymmetry, []).

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
 *   constraint_id: digital_paternalism_asymmetry
 *   human_readable: Digital Paternalism Asymmetry: Protective Architecture as Extractive Coordination
 *   domain: technology_governance/platform_design
 *
 * SUMMARY:
 *   Digital paternalism asymmetry describes the structural constraint where
 *   platform operators unilaterally control user environments in the name of
 *   protection, safety, and coordination — while systematically extracting
 *   value (data, attention, behavioral conformity) from users who have
 *   limited exit options. This constraint operates across social media,
 *   content platforms, app stores, and digital services at global scale. The
 *   core asymmetry is that users perceive restrictions as imposed guardrails
 *   while operators perceive them as coordination mechanisms that serve both
 *   user protection and business interests. The constraint exhibits all six
 *   DR types from different perspectives: snare for trapped powerless users,
 *   tangled rope for moderate creators and sophisticated users, rope for
 *   operators, scaffold for regulatory coalitions building interoperability,
 *   piton for the degraded safety theater institution, and a contestable
 *   mountain for the civilizational view that sees protection as inherent.
 *   The theater ratio has increased over the past decade as platforms have
 *   layered paternalistic features (content filtering, demotion, algorithmic
 *   curation) atop initial coordination functions, and this layering has not
 *   been transparent to users. The extractiveness trajectory shows platform
 *   operators incrementally raising the ceiling on what counts as 'protected'
 *   behavior, capturing value from each expansion.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — face insurmountable platform lock-in and cannot opt out of paternalistic restrictions; bear full cost of autonomy extraction
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — control user environment architecture, extract data and attention, can adjust policies unilaterally
 *   - Small Creators: Secondary victims (moderate/constrained) — depend on platform distribution but face demonetization and algorithmic opacity; have some exit options but switching costs are high
 *   - Sophisticated Users: Secondary beneficiary-victims (powerful/mobile) — benefit from baseline moderation but extract value from workarounds and technical knowledge; exit capacity higher than powerless users
 *   - Regulatory Agencies: Organized intermediary (organized/mobile) — GDPR, FTC, national regulators building mandate for data portability and algorithmic transparency; create sunset pathways for lock-in
 *   - Digital Rights Coalition: Organized advocates (organized/mobile) — privacy organizations, civil rights groups, interoperability consortia; see paternalism as solvable through regulation and technical standards
 *   - Safety Theater Institution: Institutional actor (institutional/arbitrage) — content moderation vendors, trust and safety teams, policy researchers; maintain paternalism through inertia and funding dependency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_paternalism_asymmetry, 0.58).
domain_priors:suppression_score(digital_paternalism_asymmetry, 0.62).
domain_priors:theater_ratio(digital_paternalism_asymmetry, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_paternalism_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_paternalism_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(digital_paternalism_asymmetry, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_paternalism_asymmetry, tangled_rope).
narrative_ontology:human_readable(digital_paternalism_asymmetry, "Digital Paternalism Asymmetry: Protective Architecture as Extractive Coordination").
narrative_ontology:topic_domain(digital_paternalism_asymmetry, "technology_governance/platform_design").

domain_priors:requires_active_enforcement(digital_paternalism_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_paternalism_asymmetry, platform_operators).
narrative_ontology:constraint_beneficiary(digital_paternalism_asymmetry, regulatory_agencies).
narrative_ontology:constraint_victim(digital_paternalism_asymmetry, end_users).
narrative_ontology:constraint_victim(digital_paternalism_asymmetry, user_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED USER (SNARE) — Users face insurmountable barriers to exit: platform lock-in through network effects, no viable alternatives with equivalent functionality, switching costs (data transfer, social graph, habit), and pervasive environmental embedding. Paternalistic restrictions (content filtering, choice architecture, algorithmic nudging) are presented as protection but function as extraction of user attention, data, and behavioral control. The user cannot exit and cannot opt out of the paternalistic regime.
constraint_indexing:constraint_classification(digital_paternalism_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL CREATOR (TANGLED ROPE) — Content creators of modest scale benefit from platform coordination (audience aggregation, algorithmic distribution, monetization infrastructure) but also suffer extraction through algorithmic curation opacity, demonetization policies, and asymmetric content rules. They have some exit options (migrate to alternative platforms, direct audience relationships) but face significant switching costs and dependency on platform algorithms. Experience genuine coordination alongside meaningful asymmetric extraction.
constraint_indexing:constraint_classification(digital_paternalism_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences paternalism as coordination mechanism: safety features, content moderation, algorithmic ranking all serve operational goals of platform stability, risk mitigation, and value capture. Has full exit option (can adjust policies, create new features, rebalance incentives) and arbitrage capacity (can deploy resources elsewhere). Benefits from the extraction embedded in the constraint architecture. Paternalism is the platform's tool for coordination and control.
constraint_indexing:constraint_classification(digital_paternalism_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized agents (privacy advocates, data protection regulators like GDPR bodies, digital rights organizations) perceive paternalism as a temporary coordination problem solvable through regulation, transparency mandates, and technical standards (interoperability, portability, algorithmic explainability). These have sunset logic: as GDPR enforcement matures, as data portability standards become technical reality, and as interoperable platforms emerge, the platform's monopoly on 'protective' architecture weakens. Exit pathways for users improve through regulation-enabled mobility.
constraint_indexing:constraint_classification(digital_paternalism_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFETY THEATER INSTITUTION (PITON) — The ecosystem of trust and safety officers, content moderation vendors, policy makers, and academic researchers has become institutionalized around the assumption that platform operators should paternalistically control user environments. This institutional layer is substantially performative: content moderation at scale is largely reactive and theater, algorithmic curation is opaque even to operators, and safety policies are post-hoc justifications for business decisions rather than derived from user protection theory. The institution persists through inertia and funding dependency despite low functional verification of whether paternalism actually improves outcomes.
constraint_indexing:constraint_classification(digital_paternalism_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SOPHISTICATED USER (TANGLED ROPE) — Power users, developers, and technically sophisticated agents experience paternalism as mixed coordination-extraction. They benefit from baseline platform stability and moderation of extreme content (coordination function) but face restrictions on advanced features, API access limits, and algorithmic opacity that extract value from their expertise. They have more exit options than constrained users (can develop competing tools, use alternative platforms, understand technical workarounds) but still face meaningful constraints. Their extraction is lower than powerless users because they have agency and partial exit paths.
constraint_indexing:constraint_classification(digital_paternalism_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some protective gatekeeping in digital spaces appears immutable: platforms must moderate illegal content, prevent certain harms, manage information quality. This perspective sees paternalism as an inherent structural feature of networked systems at scale. However, the structural data contradicts the mountain classification — historical evidence shows platform alternatives with different protection models exist and function, and the specific asymmetry (operator control + user powerlessness) is contingent on market structure rather than immutable.
constraint_indexing:constraint_classification(digital_paternalism_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_paternalism_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_paternalism_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_paternalism_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_paternalism_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_paternalism_asymmetry, TR),
    TR >= 0.70.

:- end_tests(digital_paternalism_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the substantial value extraction (behavioral data, attention, constrained choice options) from users combined with genuine but incomplete coordination benefits. The value is not extracted as transparently as a direct fee would be — it is embedded in the design architecture. The trajectory shows platforms gradually increasing extractiveness by expanding the definition of 'protected' content and 'safe' behavior, capturing more value with each expansion. Suppression (0.62): Moderate-high, capturing the significant barriers to exit (network effects, lock-in, no viable alternatives) and barriers to resistance (algorithmic opacity, asymmetric rule enforcement, demonetization as punishment). Users cannot meaningfully audit the rules applied to them or escape their application. Theater ratio (0.65): Moderate-high, reflecting that much of the paternalistic apparatus is performative. Content moderation at scale is substantially reactive theater, algorithmic curation is opaque even to operators, and safety policies are often post-hoc rationalization for business decisions. The theater has increased as platforms have added more paternalistic layers without transparency or genuine user consent. Tangled Rope is the claimed type because the constraint exhibits genuine coordination (users do benefit from basic moderation against illegal and extremely harmful content) alongside systematic extraction (control of information diet, behavioral nudging, data harvesting, asymmetric rule enforcement).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival divergence. Operators genuinely experience paternalism as coordination — they see safety features, content policies, and algorithmic ranking as solving collective action problems (preventing viral hate speech, reducing harassment, managing information overload). Users genuinely experience paternalism as extraction and control — they see the same features as asymmetric restrictions imposed without consent, obscuring algorithmic judgment, constraining choice through opaque defaults. The regulatory perspective sees a solvable problem: transparency mandates, data portability, algorithmic explainability, and interoperability standards can shift the asymmetry toward rope or scaffold. The safety theater institution sees a degraded but persistent arrangement — paternalism is maintained through inertia and institutional commitment despite low functional verification that it achieves stated goals. The civilizational analytical observer risks naturalizing contingent platform architecture as immutable law ('protection requires paternalism'), but the structural data reveals this is false — platforms with different protection models exist and function. The perspectival gaps enable diagnostic detection: if all agents saw rope (pure coordination), there would be no gap and no snare classification from any perspective. The gaps reveal the true structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to the extraction flow. Powerless trapped users have d ≈ 0.95 (maximum target). Institutional beneficiary operators have d ≈ 0.10 (strong beneficiaries with arbitrage options). Moderate constrained creators have d ≈ 0.70 (significant targets but with some agency and partial exit). Powerful mobile sophisticated users have d ≈ 0.50 (symmetric — both benefit from and suffer extraction). Organized regulatory agents have d ≈ 0.45 (partly victims of asymmetry, partly agents building alternatives). The derivation reflects that operators have full arbitrage exit while users are trapped by network effects; creators and sophisticated users occupy intermediate positions with constrained or mobile options. The piton perspective's institutional actor has d ≈ 0.15 (beneficiary through institutional inertia) even though it perceives the constraint as degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   Digital paternalism resolves mandatrophy by distinguishing the coordination function (genuine: moderation of illegal content, harassment mitigation, basic safety) from the extraction mechanism (asymmetric: data harvesting, attention capture, behavioral control). The constraint is tangled rope because both functions are present — not pure rope (which would require minimal extraction) and not pure snare (which would have no coordination function). The theater ratio increase (0.42 → 0.65) indicates that platforms are gradually converting the coordination function into theater to justify extractive architecture: the same moderation capability is marketed with increasing paternalistic rhetoric while actual user benefit remains constant or declines. This is the diagnostic signature of tangled rope degrading toward snare. The sophistry that naturalizes this as a mountain ('protection requires paternalism') is false — historical platforms with different architectures existed (early forums, email, RSS, federation) and delivered coordination with less extraction. The mandatrophy is resolved by recognizing that platforms *could* offer similar coordination with different architectural choices (interoperable, portable, algorithmically transparent), which proves the paternalism is not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_vs_protection_distinction,
    'Where is the boundary between protective coordination (genuine safety work) and extractive paternalism (control justified as safety)?',
    'Comparative analysis of platform outcomes: users'' subjective experience of autonomy, measurable harms prevented vs prevented opportunities, evidence of actual user preference for restrictions vs operator-imposed defaults',
    'If boundary is sharp and verifiable: some paternalism is mountain (necessary), most is tangled_rope (hybrid). If boundary is diffuse: measurement becomes preference-dependent and extractiveness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_protection_distinction, empirical, 'Boundary between legitimate protection and extractive paternalism').

omega_variable(
    exit_option_technical_feasibility,
    'Can alternative platforms achieve feature parity and network effects comparable to dominant platforms, or is network lock-in structurally insurmountable?',
    'Historical analysis of platform migrations (e.g., Twitter → BlueSky/Mastodon adoption rates), technical feasibility of federation and interoperability, economic analysis of network effect thresholds for different platform categories',
    'If alternatives are technically feasible and economically viable: exit is constrained rather than trapped, classification shifts toward rope/scaffold for more perspectives. If lock-in is durable: snare classification holds across perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_technical_feasibility, empirical, 'Feasibility of platform alternatives and network lock-in durability').

omega_variable(
    algorithmic_opacity_intentionality,
    'Is algorithmic opacity a necessary technical consequence of machine learning systems or an intentional design choice to preserve operator control?',
    'Examination of technical literature on interpretability (XAI), comparison of explainability investments across different platform companies, analysis of patents and internal documentation where available',
    'If necessary consequence: extractiveness is lower (complexity cost justified). If intentional choice: extractiveness is higher (opacity is mechanism for control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_intentionality, empirical, 'Whether algorithmic opacity is technical necessity or intentional control mechanism').

omega_variable(
    user_preference_for_paternalism,
    'Do users actually prefer restrictive platforms with strong paternalism, or is preference for safety a cover story for preference for simplicity and reduced friction?',
    'Behavioral choice experiments, analysis of platform adoption patterns when users have genuine options, comparative satisfaction data for platforms with different restriction levels',
    'If users prefer paternalism: coordination function is genuine, tangled_rope classification strengthens. If users prefer freedom and see paternalism as overhead: coordination function is theater, extractiveness interpretation shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(user_preference_for_paternalism, empirical, 'User preference for paternalistic vs permissive platform design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_paternalism_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digpat_tr_t0, digital_paternalism_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(digpat_tr_t5, digital_paternalism_asymmetry, theater_ratio, 5, 0.58).
narrative_ontology:measurement(digpat_tr_t10, digital_paternalism_asymmetry, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(digpat_be_t0, digital_paternalism_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(digpat_be_t5, digital_paternalism_asymmetry, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(digpat_be_t10, digital_paternalism_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_paternalism_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_paternalism_asymmetry, 0.18).
narrative_ontology:affects_constraint(digital_paternalism_asymmetry, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(digital_paternalism_asymmetry, data_extraction_asymmetry).
narrative_ontology:affects_constraint(digital_paternalism_asymmetry, network_lock_in_switching_costs).

% DUAL FORMULATION NOTE:
% Digital paternalism asymmetry is downstream of several structurally distinct constraints with different ε values. Algorithmic amplification bias (ε≈0.50) represents the opacity and inequitable outcomes of ranking systems; data extraction asymmetry (ε≈0.72) focuses on data harvesting and behavioral surveillance; network lock-in (ε≈0.45) addresses switching costs. Digital paternalism integrates these into a unified justificatory framework ('for your protection') and represents the architectural embedding of all three. It is the constraint-level story that unifies these mechanism-level stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_paternalism_asymmetry, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
