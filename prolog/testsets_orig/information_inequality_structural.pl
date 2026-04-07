% ============================================================================
% CONSTRAINT STORY: information_inequality_structural
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_inequality_structural, []).

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
 *   constraint_id: information_inequality_structural
 *   human_readable: Information Inequality as Structural Constraint
 *   domain: political_economy/information_systems
 *
 * SUMMARY:
 *   Information inequality operates as a structural constraint that permits
 *   asymmetric benefit from access to knowledge, verification infrastructure,
 *   and distribution channels. The constraint exhibits the full range of DR
 *   classification depending on the observer's structural position.
 *   Gatekeeper institutions experience the constraint as coordination (Rope):
 *   legitimate expertise distribution that serves a real function. Dependent
 *   populations experience it as pure extraction (Snare): trapped in
 *   information asymmetry with no exit. Educated consumers experience it as
 *   mixed (Tangled Rope): genuine information coordination alongside
 *   attention extraction. The constraint's theater ratio (0.55) reflects that
 *   editorial gatekeeping includes both genuine expertise filtering
 *   (coordination) and performative curation (theater). The extractiveness
 *   value (0.58) indicates moderate-to-high asymmetric benefit, driven by
 *   behavioral data extraction, attention monopolization, and epistemic
 *   control rather than by information scarcity alone. The constraint's
 *   suppression (0.62) reflects structural barriers to information access:
 *   algorithmic opacity, cognitive load, cost of alternative sources, and
 *   epistemic homogenization that prevents awareness of access gaps.
 *
 * KEY AGENTS:
 *   - Information Gatekeepers (Media, Platforms): Institutional actors (institutional/arbitrage) — benefit from attention monopoly, behavioral data, epistemic control. Experience constraint as coordination of information distribution.
 *   - Information-Dependent Populations: Powerless agents (powerless/trapped) — depend on mediated information with no viable alternatives. Bear full extraction cost: attention, behavioral pattern, epistemic narrative.
 *   - Educated Information Consumers: Moderate agents (moderate/constrained) — can theoretically access multiple sources but face time and cognitive burden constraints. Both benefit from and bear costs of mediated information.
 *   - Captured Institutional Journalists: Institutional actors constrained by editorial and advertiser pressure (institutional/constrained, identity_locked) — coordinate news distribution while being forced into extractive dynamics. Identity-locked to professional role.
 *   - Legacy Editorial Institutions: Institutional actors maintaining gatekeeping through inertia (institutional/arbitrage) — their original function (expertise filtering) has atrophied relative to their performative role.
 *   - Open Information Movement: Organized agents building alternatives (organized/constrained) — see decentralized verification and distributed curation as pathways to lower-extraction information coordination.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing gatekeeper power as inevitable scarcity; false summit detector flags this.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_inequality_structural, 0.58).
domain_priors:suppression_score(information_inequality_structural, 0.62).
domain_priors:theater_ratio(information_inequality_structural, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_inequality_structural, extractiveness, 0.58).
narrative_ontology:constraint_metric(information_inequality_structural, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(information_inequality_structural, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_inequality_structural, tangled_rope).
narrative_ontology:human_readable(information_inequality_structural, "Information Inequality as Structural Constraint").
narrative_ontology:topic_domain(information_inequality_structural, "political_economy/information_systems").

domain_priors:requires_active_enforcement(information_inequality_structural).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_inequality_structural, information_gatekeepers).
narrative_ontology:constraint_beneficiary(information_inequality_structural, institutional_media_holders).
narrative_ontology:constraint_beneficiary(information_inequality_structural, algorithmic_intermediaries).
narrative_ontology:constraint_victim(information_inequality_structural, information_dependent_populations).
narrative_ontology:constraint_victim(information_inequality_structural, knowledge_asymmetry_bearers).
narrative_ontology:constraint_victim(information_inequality_structural, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION-DEPENDENT POPULATION (SNARE) — Trapped populations dependent on mediated information channels with no structural exit. High suppression from information scarcity, filter bubbles, and asymmetric content distribution. No coordination function visible — pure extraction of attention, behavioral pattern, and epistemic control.
constraint_indexing:constraint_classification(information_inequality_structural, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EDUCATED INFORMATION CONSUMER (TANGLED ROPE) — Constrained by time, cognitive burden, and algorithmic design that channels information. But benefits from mediated information infrastructure — genuine coordination of knowledge distribution exists alongside asymmetric extraction of behavioral data and attention. Exit options exist (use multiple sources, digital literacy) but require significant cognitive cost.
constraint_indexing:constraint_classification(information_inequality_structural, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL MEDIA HOLDER (ROPE) — Experiences the constraint as coordination: distributing information solves the collective problem of communication at scale. First-mover advantage and network effects create benefits. Sees information inequality as a necessary feature of information distribution, not extraction.
constraint_indexing:constraint_classification(information_inequality_structural, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPTURED JOURNALIST/EDITOR (TANGLED ROPE) — Institutional actor constrained by editorial policies, advertiser pressure, and algorithmic gatekeeping. Coordinates news distribution (genuine function) while being forced to participate in extractive dynamics. Identity-locked to professional role within compromised institution.
constraint_indexing:constraint_classification(information_inequality_structural, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY EDITORIAL INSTITUTION (PITON) — Traditional journalism gatekeeping persists through institutional inertia. Theater-ratio high (editorial review processes that no longer validate accuracy in the age of real-time information). Original function (vetting expertise) has atrophied; constraint maintained through professional credentials and legal liability frameworks.
constraint_indexing:constraint_classification(information_inequality_structural, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN INFORMATION MOVEMENT (SCAFFOLD) — Organized agents (open-source intelligence, decentralized information networks, Wikipedia model) building alternative distribution pathways with lower extraction. See information inequality as solvable through distributed authentication, collaborative curation, and transparency. Sunset logic: as distributed verification infrastructure matures, centralized gatekeeping loses enforcement power.
constraint_indexing:constraint_classification(information_inequality_structural, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing information scarcity as inherent: 'Information asymmetry is inevitable because attention is scarce and expertise cannot scale.' This risks conflating legitimate coordination costs (expertise validation) with contingent institutional extraction (media monopoly, algorithmic manipulation). The engine's false summit detector identifies this as naturalization of what is actually contingent gatekeeper power.
constraint_indexing:constraint_classification(information_inequality_structural, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_inequality_structural_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_inequality_structural, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_inequality_structural, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_inequality_structural, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_inequality_structural, TR),
    TR >= 0.70.

:- end_tests(information_inequality_structural_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high, reflecting asymmetric benefit captured by information gatekeepers. The value has risen from 0.35 (10 units ago) as algorithmic personalization, behavioral data monetization, and attention monopolization have intensified. The extraction is not total because some genuine expertise filtering and information coordination occurs alongside the asymmetric capture. Suppression (0.62): Moderate-high, reflecting significant structural barriers to information access: algorithmic opacity, cognitive burden of source verification, cost of platform alternatives, and epistemic homogenization that prevents awareness of the access gap itself. Theater ratio (0.55): Moderate. Editorial gatekeeping includes both genuine curation (expertise filtering) and performative elements (brand maintenance, authority signaling). The theater has increased as algorithmic curation has replaced human editorial judgment while maintaining the performative appearance of expertise.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates five distinct classifications from the same base properties, revealing how information inequality is experienced structurally differently by agents with different power and exit options. Gatekeeper institutions see coordination (Rope) — information distribution is solving a real problem. Educated consumers see mixed coordination and extraction (Tangled Rope) — they benefit from mediated information but lose attention and behavioral autonomy. Dependent populations see pure extraction (Snare) — they are trapped in information asymmetry with no genuine exit. Legacy institutions see their own degraded function (Piton) — editorial gatekeeping persists through professional credentialing and institutional inertia despite atrophied verification function. The open-source movement sees a temporary problem (Scaffold) — distributed information networks are building alternative pathways with sunset logic. The civilizational analytical observer risks misclassifying this as natural law (Mountain) — 'information scarcity is inevitable' — but the structural data reveals contingent gatekeeper power, not immutable limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary and victim status combined with exit options. Information gatekeepers are beneficiaries with arbitrage options (d ≈ 0.05) — they capture attention and behavioral data while maintaining exit options (can pivot to new platforms, new business models). They experience low effective extraction because the constraint benefits them. Dependent populations are victims with trapped exit options (d ≈ 0.95) — they cannot exit information dependence without severe cost and lack awareness of alternatives. They experience maximum extraction. Educated consumers are constrained victims (d ≈ 0.60) — they have some agency to diversify sources but face cognitive and temporal costs. Institutional journalists are constrained actors caught between coordination function and extractive pressure (d ≈ 0.55) — their position is mixed. Open-source movement agents are organized with constrained exits (d ≈ 0.45) — they are building alternatives but face scaling challenges. The analytical observer has analytical exit (d ≈ 0.72) — they can theoretically see the structure but risk naturalizing contingent constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that information inequality is genuinely a tangled rope: it coordinates information distribution at scale (legitimate function) while extracting asymmetric benefits through attention, behavioral data, and epistemic control (extraction mechanism). The constraint is not pure coordination (Rope) because suppression is high and benefits asymmetric; not pure extraction (Snare) because genuine expertise filtering and information accessibility are real coordination benefits. The perspectival gap between gatekeeper (sees Rope) and dependent population (sees Snare) is a diagnostic signature of tangled rope: the same structural mechanism is experienced as coordination by beneficiaries and extraction by victims. The false summit at the analytical context is important: naturalizing 'information asymmetry is inevitable scarcity' risks legitimizing what is actually extractive institutional power. The constraint could be restructured with lower suppression and more distributed verification without losing the coordination function. The fact that it has not been restructured indicates that gatekeeper interests in maintaining asymmetric benefit are stronger than coordination-optimization interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_vs_gatekeeping_boundary,
    'How much of the information inequality reflects genuine expertise boundaries versus extractive gatekeeping?',
    'Comparison of information access barriers across domains: medical information (high legitimate expertise barrier), political analysis (lower expertise barrier), entertainment (minimal expertise barrier). Correlation between expertise legitimacy and observed access restrictions.',
    'If expertise dominates: constraint is partially mountain (immutable expertise asymmetry). If gatekeeping dominates: constraint is snare (pure extraction). If balanced: constraint is genuinely tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_vs_gatekeeping_boundary, empirical, 'Boundary between legitimate expertise filtering and extractive gatekeeping').

omega_variable(
    algorithmic_manipulation_vs_preference_matching,
    'Do information algorithms serve user preferences (coordination) or manipulate users toward advertiser preferences (extraction)?',
    'Black-box testing of algorithmic behavior; A/B testing of diverse vs homogeneous feeds; measurement of serendipitous discovery rate vs engagement optimization. Comparison with transparent open-source algorithms.',
    'If preference-matching dominates: rope classification from institutional perspective. If manipulation dominates: snare classification from dependent population. If balanced: tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_manipulation_vs_preference_matching, empirical, 'Whether algorithms coordinate preference matching or extract attention through manipulation').

omega_variable(
    decentralized_information_viability,
    'Can distributed information networks (blockchain verification, decentralized social networks, open-source intelligence) actually scale to match centralized platforms'' coordination functions?',
    'Longitudinal comparison of adoption rates, information quality metrics (factual accuracy, bias detection), and consensus formation speed across centralized vs decentralized platforms. Real-world case studies of Wikipedia, open-source intelligence networks, decentralized social media.',
    'If viable: scaffold sunset is real and extraction timeline is bounded. If not viable: information inequality is structural and architectural, making some extraction inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_information_viability, empirical, 'Whether decentralized information networks can viably replace centralized platforms').

omega_variable(
    epistemic_commons_damage_quantification,
    'What is the measurable damage to collective epistemic capacity (shared reality, consensus-forming ability, epistemic trust) from information inequality?',
    'Measurement of polarization metrics, factual disagreement baselines, shared-reality erosion across populations with different information access. Longitudinal tracking of belief divergence correlated with algorithmic filter exposure.',
    'If damage is severe: victim classification of epistemic commons is justified; constraint is snare. If damage is moderate: constraint is tangled rope. If minimal: constraint is mostly rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_commons_damage_quantification, empirical, 'Measurable damage to collective epistemic capacity from information inequality').

omega_variable(
    identity_lock_media_consumption,
    'To what extent does media consumption identity-lock populations to specific information sources or worldviews?',
    'Psychological research on media identity fusion; measurement of switching costs when populations attempt to diversify sources. Analysis of post-exit belief patterns when populations leave algorithmic feeds.',
    'If high identity lock: exit_options should be ''identity_locked'' not ''constrained'' for some agents; classification shifts toward snare. If low: constrained is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_media_consumption, empirical, 'Degree of identity fusion with media sources in information-dependent populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_inequality_structural, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infineq_tr_t0, information_inequality_structural, theater_ratio, 0, 0.38).
narrative_ontology:measurement(infineq_tr_t5, information_inequality_structural, theater_ratio, 5, 0.48).
narrative_ontology:measurement(infineq_tr_t10, information_inequality_structural, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(infineq_be_t0, information_inequality_structural, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(infineq_be_t5, information_inequality_structural, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(infineq_be_t10, information_inequality_structural, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_inequality_structural, information_standard).
narrative_ontology:affects_constraint(information_inequality_structural, epistemic_polarization_downstream).
narrative_ontology:affects_constraint(information_inequality_structural, attention_economy_extraction).
narrative_ontology:affects_constraint(information_inequality_structural, algorithmic_opacity_gatekeeping).

% DUAL FORMULATION NOTE:
% Information inequality decomposes into at least three distinct constraints: (1) expertise filtering (information_standard coordination type, low ε), (2) algorithmic attention extraction (snare-adjacent, high ε), and (3) epistemic homogenization through filter bubbles (snare, very high ε and suppression). This story represents the unified structural phenomenon; downstream stories can decompose by function and measurement basis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_inequality_structural, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
