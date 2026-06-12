% ============================================================================
% CONSTRAINT STORY: technocratic_paradigm_resistance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technocratic_paradigm_resistance, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: technocratic_paradigm_resistance
 *   human_readable: Technocratic Paradigm Resistance in AI Governance
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   The technocratic paradigm — the cultural-economic system that makes
 *   efficiency, control, and profit the sole measures of value — operates as
 *   a constraint on AI governance by suppressing alternative value frameworks
 *   (care, relationality, human dignity as non-instrumental) and extracting
 *   from those reduced to optimization variables. The encyclical Laudato Si'
 *   and its successor documents identify this paradigm as the central
 *   obstacle to just technological development, arguing that resistance
 *   requires embedding alternative values in system design, not merely
 *   regulating outputs. The constraint exhibits rising extraction and theater
 *   over the measurement interval (2015-2024): as AI deployment accelerated,
 *   efficiency metrics became more dominant, while ethics review processes
 *   became more performative. The paradigm coordinates genuine collective
 *   action problems (resource allocation at planetary scale, information
 *   processing beyond human capacity) while simultaneously extracting through
 *   reduction of persons to data points — a structurally inseparable tangled
 *   rope from the analytical perspective. The constraint is contested across
 *   four kernel readings (Magisterial integralist, secular humanist,
 *   techno-optimist, pluralist pragmatic), each producing different
 *   beneficiary/victim structures and enforcement mechanisms. Observed
 *   extractiveness (0.68) and suppression (0.72) are closest to
 *   techno-optimist predictions, suggesting that reading currently dominates
 *   in actual deployment despite competing normative claims.
 *
 * KEY AGENTS:
 *   - Workers and Vulnerable Populations: Primary victims (powerless/trapped) — reduced to efficiency metrics in hiring, management, benefit allocation; no exit from algorithmic systems
 *   - Care Economy Practitioners: Secondary victims (moderate/constrained) — nurses, teachers, social workers constrained by efficiency mandates that conflict with care values; can exit roles but not paradigm
 *   - AI Platform Companies: Primary beneficiaries (institutional/arbitrage) — capture value through efficiency optimization; full exit options to alternative markets or jurisdictions
 *   - Technocratic Elites: Primary beneficiaries (powerful/mobile) — consultancies, optimization specialists, data scientists whose expertise is valued under efficiency-only metrics
 *   - Algorithmic Accountability Coalitions: Organized resistance (organized/mobile) — worker organizing, data rights groups, ethical AI advocacy building alternative governance with scaffold logic
 *   - Ethics Review Boards: Institutional actors (institutional/constrained) — perform ritualized oversight with minimal enforcement power; piton perspective reflects theater
 *   - Human Dignity as Relational Value: Abstract victim (powerless/trapped) — the normative framework itself is suppressed by efficiency-only metrics; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technocratic_paradigm_resistance, 0.68).
domain_priors:suppression_score(technocratic_paradigm_resistance, 0.72).
domain_priors:theater_ratio(technocratic_paradigm_resistance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technocratic_paradigm_resistance, extractiveness, 0.68).
narrative_ontology:constraint_metric(technocratic_paradigm_resistance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technocratic_paradigm_resistance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technocratic_paradigm_resistance, tangled_rope).
narrative_ontology:human_readable(technocratic_paradigm_resistance, "Technocratic Paradigm Resistance in AI Governance").
narrative_ontology:topic_domain(technocratic_paradigm_resistance, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(technocratic_paradigm_resistance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technocratic_paradigm_resistance, 'af659a43-d7ab-410c-be3e-3487537a7441').
narrative_ontology:cs_kernel_codification('af659a43-d7ab-410c-be3e-3487537a7441', formalized).
narrative_ontology:cs_authority_grounding('af659a43-d7ab-410c-be3e-3487537a7441', lineage).
narrative_ontology:cs_interpretation_layer_present('af659a43-d7ab-410c-be3e-3487537a7441').
narrative_ontology:cs_reading_relation('af659a43-d7ab-410c-be3e-3487537a7441', technocratic_paradigm_resistance__secular_humanist_ai_governance, coexists_with).
narrative_ontology:cs_reading_relation('af659a43-d7ab-410c-be3e-3487537a7441', technocratic_paradigm_resistance__techno_optimist_ai_governance, coexists_with).
narrative_ontology:cs_reading_relation('af659a43-d7ab-410c-be3e-3487537a7441', technocratic_paradigm_resistance__pluralist_pragmatic_ai_governance, coexists_with).
narrative_ontology:cs_axiom('af659a43-d7ab-410c-be3e-3487537a7441', foundational, human_dignity_as_imago_dei).
narrative_ontology:cs_axiom_status(human_dignity_as_imago_dei, holdable).
narrative_ontology:cs_axiom_grounding('af659a43-d7ab-410c-be3e-3487537a7441', human_dignity_as_imago_dei, theological).
narrative_ontology:cs_axiom('af659a43-d7ab-410c-be3e-3487537a7441', foundational, common_good_primacy_over_efficiency).
narrative_ontology:cs_axiom_status(common_good_primacy_over_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('af659a43-d7ab-410c-be3e-3487537a7441', common_good_primacy_over_efficiency, deontological).
narrative_ontology:cs_axiom('af659a43-d7ab-410c-be3e-3487537a7441', secondary, subsidiarity_principle_in_ai_governance).
narrative_ontology:cs_axiom_status(subsidiarity_principle_in_ai_governance, holdable).
narrative_ontology:cs_axiom_grounding('af659a43-d7ab-410c-be3e-3487537a7441', subsidiarity_principle_in_ai_governance, conventional).
narrative_ontology:cs_reference_frame('af659a43-d7ab-410c-be3e-3487537a7441', catholic_social_doctrine_pre_digital_era).
narrative_ontology:cs_drift_state('af659a43-d7ab-410c-be3e-3487537a7441', contemporary_ai_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('af659a43-d7ab-410c-be3e-3487537a7441', '2025-01-09T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technocratic_paradigm_resistance, technocratic_elites_concentrating_power).
narrative_ontology:constraint_beneficiary(technocratic_paradigm_resistance, ai_platform_companies).
narrative_ontology:constraint_beneficiary(technocratic_paradigm_resistance, efficiency_optimization_consultancies).
narrative_ontology:constraint_victim(technocratic_paradigm_resistance, workers_vulnerable_populations_excluded_by_optimization).
narrative_ontology:constraint_victim(technocratic_paradigm_resistance, care_economy_practitioners).
narrative_ontology:constraint_victim(technocratic_paradigm_resistance, communities_resisting_datafication).
narrative_ontology:constraint_victim(technocratic_paradigm_resistance, human_dignity_as_relational_value).
narrative_ontology:constraint_vindicates(technocratic_paradigm_resistance, efficiency_as_sole_value_proposition).
narrative_ontology:constraint_vindicates(technocratic_paradigm_resistance, data_driven_optimization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subjected to algorithmic management systems that measure value solely through efficiency metrics. No voice in system design, no exit from predictive hiring or automated benefit determination. Reduced to data points in optimization functions.
narrative_ontology:constraint_stakeholder(technocratic_paradigm_resistance, workers_vulnerable_populations_excluded_by_optimization, payer,
    powerless, biographical, trapped, global).

% Nurses, teachers, social workers constrained by efficiency mandates that conflict with care values. Systems coordinate resource allocation but devalue relational work. Can exit specific roles but not the paradigm without leaving profession.
narrative_ontology:constraint_stakeholder(technocratic_paradigm_resistance, care_economy_practitioners, payer,
    moderate, biographical, constrained, national).

% Design and deploy AI systems optimized for efficiency, control, and profit. Set the metrics that define value. Capture economic returns from optimization. Full exit options to alternative markets or regulatory jurisdictions.
narrative_ontology:constraint_stakeholder(technocratic_paradigm_resistance, ai_platform_companies, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(technocratic_paradigm_resistance, ai_platform_companies, beneficiary).

% Consultancies, optimization specialists, data scientists whose expertise is valued under efficiency-only metrics. Capture career and economic benefits from paradigm dominance. Mobile across sectors and jurisdictions.
narrative_ontology:constraint_stakeholder(technocratic_paradigm_resistance, technocratic_elites_concentrating_power, beneficiary,
    powerful, biographical, mobile, global).

% Worker organizing, data rights groups, ethical AI advocacy building alternative governance frameworks. See the paradigm as temporary problem being solved through regulation. Building accountability mechanisms with scaffold logic.
narrative_ontology:constraint_stakeholder(technocratic_paradigm_resistance, algorithmic_accountability_coalitions, observer,
    organized, generational, mobile, continental).

% Corporate AI ethics boards and academic review committees perform ritualized oversight with minimal enforcement power. Review process persists through institutional inertia despite inability to override profit-driven deployment decisions.
narrative_ontology:constraint_stakeholder(technocratic_paradigm_resistance, ethics_review_boards, agenda_setter,
    institutional, civilizational, constrained, global).

% Communities attempting to resist mandatory datafication, surveillance, or algorithmic governance. Excluded from design decisions. Lack technical literacy and political power to contest implementation. Trapped by infrastructure dependencies.
narrative_ontology:constraint_stakeholder(technocratic_paradigm_resistance, communities_resisting_datafication, excluded,
    powerless, generational, trapped, regional).

% The normative framework itself — understanding persons as relational, embodied, finite yet transcendent — is suppressed by efficiency-only metrics. An abstract good that cannot organize or exit. Kept for narrative completeness but excluded from directionality derivation.
narrative_ontology:constraint_stakeholder(technocratic_paradigm_resistance, human_dignity_as_relational_value, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(technocratic_paradigm_resistance, human_dignity_as_relational_value).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The paradigm coordinates resource allocation at planetary scale and information processing beyond human capacity. It solves genuine collective action problems: matching supply and demand across global markets, routing logistics networks, processing medical diagnostics, allocating scarce resources under uncertainty.
% TRANSFER_FUNCTION: The paradigm transfers economic value from workers and vulnerable populations (whose labor is optimized and whose data is extracted) to platform companies and technocratic elites (who control the metrics and capture the returns). It transfers decision-making power from affected communities to algorithm designers. It transfers normative authority from care ethics and relational values to efficiency metrics.
% ABSENT_VOICES: Affected communities lack technical literacy and political power to contest implementation. Care practitioners' professional knowledge is devalued relative to optimization expertise. Indigenous communities and Global South populations are excluded from design decisions despite bearing costs of deployment. The absent voices are structurally excluded by the paradigm's own logic: those who cannot be measured efficiently are not in the room.
% DISAPPEARANCE_RATIONALE: If the technocratic paradigm disappeared overnight, resource allocation systems would require redesign around alternative values (care, relationality, subsidiarity). Platform companies would lose their primary value extraction mechanism. Workers would regain bargaining power over working conditions. Care practitioners could reassert professional judgment over efficiency metrics. The rearrangement would be substantial because current arrangements depend on efficiency-only optimization.
% FOUNDING_PROBLEM: The paradigm emerged to solve coordination problems at scales beyond human cognitive capacity: global supply chains, planetary resource allocation, information processing for billions of users. The founding problem was genuine — pre-algorithmic methods could not coordinate at this scale.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem remains live: planetary-scale resource allocation and information processing still exceed human cognitive capacity. This is corroborated by logistics experts, supply chain managers, and information systems researchers across multiple traditions (not only paradigm beneficiaries). However, the paradigm's claim that efficiency-only optimization is the ONLY solution to this problem is contested by care ethicists, Catholic social teaching, and alternative governance frameworks — the problem is live but the paradigm's monopoly on solutions is not corroborated outside the beneficiary set.
narrative_ontology:disappearance_verdict(technocratic_paradigm_resistance, world_rearranges).
narrative_ontology:founding_problem_status(technocratic_paradigm_resistance, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKERS AND VULNERABLE POPULATIONS (SNARE) — Trapped in systems that measure their value solely through efficiency metrics. No exit from algorithmic management, predictive hiring, or automated benefit determination. Maximum extraction: reduced to data points in optimization functions with no voice in design.
constraint_indexing:constraint_classification(technocratic_paradigm_resistance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CARE ECONOMY PRACTITIONERS (TANGLED ROPE) — Nurses, teachers, social workers constrained by efficiency mandates that conflict with care values. Systems coordinate resource allocation (genuine function) but extract through metrics that devalue relational work. Can exit specific roles but not the paradigm itself without leaving profession.
constraint_indexing:constraint_classification(technocratic_paradigm_resistance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI PLATFORM COMPANIES (ROPE) — Experience the paradigm as pure coordination: efficiency metrics enable scale, profit maximization aligns stakeholders, data extraction funds innovation. Net beneficiaries with full exit options to alternative markets or regulatory jurisdictions.
constraint_indexing:constraint_classification(technocratic_paradigm_resistance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED RESISTANCE MOVEMENTS (SCAFFOLD) — Worker organizing, data rights coalitions, and ethical AI advocacy groups see the paradigm as a temporary problem being solved through regulation (EU AI Act, right to explanation, algorithmic impact assessments). Building alternative governance with sunset logic: once accountability mechanisms mature, pure efficiency optimization loses legitimacy.
constraint_indexing:constraint_classification(technocratic_paradigm_resistance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: INSTITUTIONAL ETHICS REVIEW (PITON) — Corporate AI ethics boards and academic review committees perform ritualized oversight with minimal enforcement power. The review process persists through institutional inertia despite inability to override profit-driven deployment decisions. Theater ratio reflects gap between stated values and actual constraint on development.
constraint_indexing:constraint_classification(technocratic_paradigm_resistance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the technocratic paradigm coordinates genuine collective action problems (resource allocation at scale, information processing beyond human capacity) while simultaneously extracting through reduction of persons to optimization variables. Both functions are real and structurally inseparable in current implementations. The paradigm requires active enforcement through market mechanisms, regulatory capture, and ideological naturalization.
constraint_indexing:constraint_classification(technocratic_paradigm_resistance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technocratic_paradigm_resistance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technocratic_paradigm_resistance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_paradigm_resistance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technocratic_paradigm_resistance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(technocratic_paradigm_resistance, TR),
    TR >= 0.70.

:- end_tests(technocratic_paradigm_resistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The paradigm extracts from workers and vulnerable populations by reducing them to optimization variables, from care practitioners by devaluing relational work, and from communities by forcing datafication. Extraction has increased over the interval as AI deployment accelerated and efficiency metrics became more dominant. The value reflects substantial but not maximal extraction — some spaces (healthcare, education) retain partial resistance to pure efficiency logic. Suppression (0.72): High. Significant barriers to alternative value frameworks include: market pressure for efficiency gains, regulatory capture by platform companies, ideological naturalization of optimization as neutral, lack of technical literacy among affected populations, and career risk for practitioners who resist metrics. Suppression has increased as the paradigm matured and alternatives were marginalized. Theater ratio (0.58): Moderate-high. Ethics review processes, corporate responsibility statements, and participatory design initiatives are substantially performative. Reviews assess compliance with stated principles but rarely override profit-driven deployment decisions. The theater has increased as companies adopted ethics language without changing core optimization logic. The ratio is not higher because some accountability mechanisms (GDPR, algorithmic impact assessments) do impose real constraints, though weaker than their rhetoric suggests.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full indexical range from a single structural phenomenon. Platform companies see pure coordination (Rope) — efficiency metrics enable scale and profit maximization aligns stakeholders. Organized resistance movements see a temporary problem being solved through regulation (Scaffold) — accountability mechanisms are maturing and will constrain pure optimization. Ethics review boards see their own degraded ritual (Piton) — oversight persists through inertia without enforcement power. Care practitioners see mixed coordination and extraction (Tangled Rope) — systems allocate resources but devalue relational work. Workers see pure extraction (Snare) — reduced to optimization variables with no exit. The analytical observer sees tangled rope at the civilizational scale — coordination and extraction are structurally inseparable in current algorithmic implementations. The gap reveals how structural position determines perception: beneficiaries with exit options see coordination, victims without exit see extraction, and the analytical perspective sees both functions as real and intertwined.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the paradigm's extraction flow. AI platform companies and technocratic elites are primary beneficiaries: they capture value through efficiency optimization and have full exit options (arbitrage-level mobility to alternative markets or regulatory jurisdictions). Their directionality is low (near 0.0), producing negative or minimal effective extraction — they experience the paradigm as pure coordination. Workers and vulnerable populations are primary victims: they are reduced to data points in optimization functions with no voice in design and no exit from algorithmic management systems. Their directionality is high (near 1.0), producing maximum effective extraction — they bear the full cost of reduction to efficiency metrics. Care economy practitioners occupy a middle position: they are constrained by efficiency mandates but retain some agency through professional norms and can exit specific roles (though not the paradigm itself without leaving their profession). Their directionality is moderate (0.5-0.7), producing significant but not maximal extraction. Ethics review boards are institutional actors with constrained exit options whose oversight function has atrophied into theater — their directionality is moderate but their classification derives from the theater gate rather than from high extraction. The analytical observer sees both the genuine coordination function and the extractive reduction as structurally inseparable, producing a tangled rope classification at the civilizational scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the technocratic paradigm is neither pure coordination (Rope) nor pure extraction (Snare) but a tangled rope whose classification depends on the observer's structural position. The paradigm coordinates genuine collective action problems — resource allocation at planetary scale, information processing beyond human capacity — that could not be solved through pre-algorithmic methods. This coordination function is real and benefits all participants in some dimensions (access to information, logistical efficiency, matching markets). Simultaneously, the paradigm extracts by reducing persons to optimization variables, suppressing alternative value frameworks, and concentrating power among those who control the metrics. This extraction is also real and harms identifiable victims (workers subjected to algorithmic management, communities forced into datafication, care practitioners whose relational work is devalued). The analytical classification is tangled rope because both functions are structurally present and neither can be eliminated without eliminating the constraint itself. The perspectival gap (beneficiaries see rope, victims see snare) is not a measurement error but the correct indexical reading: effective extraction varies with structural position, and the constraint genuinely appears different from different seats. The mandatrophy is resolved by recognizing that 'which type is it?' is the wrong question — the presheaf over observation sites IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficiency_coordination_separability,
    'Can the genuine coordination function (resource allocation, information processing) be separated from the extractive reduction of persons to data points, or are they structurally inseparable in algorithmic systems?',
    'Empirical analysis of alternative AI architectures that embed care values, human override, and contestability. Measure whether such systems can achieve coordination at scale or whether efficiency-only optimization is structurally necessary for scalability.',
    'If separable: the constraint is a Snare from more perspectives (extraction is contingent, not necessary). If inseparable: the constraint is closer to Mountain (efficiency-dignity tradeoff is inherent to algorithmic coordination at scale).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_coordination_separability, empirical, 'Whether coordination and extraction functions are structurally separable').

omega_variable(
    alternative_value_embedding_feasibility,
    'Can relationality, care, and human dignity be operationalized in AI system design without collapsing into efficiency proxies, or does formalization necessarily reduce these values to measurable metrics that reproduce the paradigm?',
    'Analysis of attempts to embed care ethics in algorithmic systems (participatory design, value-sensitive design, capability approach implementations). Track whether formalized ''care metrics'' maintain their normative content or become new optimization targets.',
    'If feasible: Scaffold perspective is vindicated (alternative paradigm is buildable). If infeasible: the paradigm may be closer to Mountain (algorithmic formalization structurally requires reduction to efficiency).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_value_embedding_feasibility, conceptual, 'Whether care values can be embedded without reduction to efficiency proxies').

omega_variable(
    regulatory_capture_depth,
    'Are emerging AI governance frameworks (EU AI Act, algorithmic accountability laws) genuine constraints on the technocratic paradigm, or are they being shaped by the paradigm''s beneficiaries to legitimize extraction while performing oversight?',
    'Longitudinal analysis of regulatory implementation: measure enforcement rates, penalty severity, and whether regulations shift power or merely formalize existing practices. Track revolving door between regulators and platform companies.',
    'If genuine constraint: Scaffold perspective confirmed (sunset is real). If captured: regulations are theater, and the Piton classification extends to governance layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Whether AI governance regulations constrain or legitimize the paradigm').

omega_variable(
    cs_framing_under_determination,
    'Is the kernel ''human dignity as imago Dei'' the authoritative framing, or is the constraint better understood through secular human rights frameworks, utilitarian optimization, or pluralist negotiation? Each framing produces different beneficiary/victim structures and different classification outcomes.',
    'Cross-reading analysis: compare the structural deltas predicted by each reading against observed deployment patterns. Which reading''s predicted beneficiaries and victims match actual power flows? Which reading''s enforcement mechanisms are operant?',
    'The Magisterial reading predicts moderate extraction with moral suasion enforcement. The techno-optimist reading predicts high extraction with market enforcement. The secular humanist reading predicts moderate extraction with legal enforcement. The pluralist reading predicts moderate extraction with multilateral enforcement. Observed extractiveness (0.68) and suppression (0.72) are closest to techno-optimist predictions, suggesting that reading currently dominates despite competing claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Which kernel framing best explains observed structural dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technocratic_paradigm_resistance, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(techno_resist_tr_t0, technocratic_paradigm_resistance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(techno_resist_tr_t3, technocratic_paradigm_resistance, theater_ratio, 3, 0.42).
narrative_ontology:measurement(techno_resist_tr_t6, technocratic_paradigm_resistance, theater_ratio, 6, 0.51).
narrative_ontology:measurement(techno_resist_tr_t9, technocratic_paradigm_resistance, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(techno_resist_be_t0, technocratic_paradigm_resistance, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(techno_resist_be_t3, technocratic_paradigm_resistance, base_extractiveness, 3, 0.59).
narrative_ontology:measurement(techno_resist_be_t6, technocratic_paradigm_resistance, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(techno_resist_be_t9, technocratic_paradigm_resistance, base_extractiveness, 9, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(techno_resist_su_t0, technocratic_paradigm_resistance, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(techno_resist_su_t3, technocratic_paradigm_resistance, suppression_requirement, 3, 0.64).
narrative_ontology:measurement(techno_resist_su_t6, technocratic_paradigm_resistance, suppression_requirement, 6, 0.69).
narrative_ontology:measurement(techno_resist_su_t9, technocratic_paradigm_resistance, suppression_requirement, 9, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technocratic_paradigm_resistance, resource_allocation).
narrative_ontology:affects_constraint(technocratic_paradigm_resistance, algorithmic_management_systems).
narrative_ontology:affects_constraint(technocratic_paradigm_resistance, predictive_policing_frameworks).
narrative_ontology:affects_constraint(technocratic_paradigm_resistance, automated_benefit_determination).

% DUAL FORMULATION NOTE:
% The technocratic paradigm is the upstream constraint that shapes specific AI deployment contexts. Algorithmic management, predictive policing, and automated benefit systems are downstream instantiations with their own extractiveness values reflecting domain-specific power asymmetries. The paradigm's extractiveness (0.68) reflects the cultural-economic system level; downstream constraints may have higher or lower values depending on local enforcement and resistance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technocratic_paradigm_resistance, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
