% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Intervention Legitimacy Calibrated to Capacity Deficits
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The graded sovereignty reading construes territorial authority not as a
 *   binary (sovereign or not) but as a spectrum calibrated to state
 *   administrative capacity. Under this framework, intervention legitimacy
 *   becomes proportional to capacity deficits: states with low fiscal
 *   discipline, weak institutions, limited security monopoly, or poor
 *   humanitarian standards are subject to graduated external
 *   management—conditionality, structural adjustment, peacekeeping, or
 *   trusteeship—justified as protective rather than extractive. This reading
 *   differs fundamentally from the classical Westphalian non-intervention
 *   principle (absolute_non_intervention) which holds that territorial
 *   sovereigns have equal rights regardless of internal capacity, and from
 *   conditional responsibility, which ties intervention to specific harms
 *   (genocide, terrorism) rather than general capacity metrics. The graded
 *   reading creates a permanent hierarchical structure where beneficiary
 *   status (high-capacity states, evaluation authorities) is largely
 *   self-certifying and victim status (low-capacity states) is persistently
 *   renewable through metrics that the evaluation authorities themselves
 *   administer. Theater is high (0.68) because the formal UN system preserves
 *   the language of sovereign equality while the actual decision system
 *   operates via capacity assessment. Extractiveness (0.58) reflects that the
 *   hierarchy enables control of policy, resource flows, and institutional
 *   design in low-capacity states, producing asymmetric benefits to
 *   evaluation authorities and high-capacity states. Suppression (0.62)
 *   reflects the mechanisms that lock states into low-capacity status:
 *   conditionality requirements that constrain fiscal autonomy, capacity
 *   metrics that reward Western institutional forms, and the paradox that
 *   rejecting the metric framework itself counts as evidence of incapacity.
 *
 * KEY AGENTS:
 *   - Low-Capacity States (Trapped/Powerless): Subject of perpetual capacity assessment and intervention authorization. Primary victims. Cannot exit the framework without being classified as rejecting international standards, which justifies intervention.
 *   - Capacity Evaluation Authorities (Institutional/Constrained): IMF, World Bank, UNHCR, UN agencies that assess and certify state capacity. Primary beneficiaries of the extraction mechanism; also constrained by their need to justify continued engagement. Institutional incentives favor continued identification of capacity gaps.
 *   - High-Capacity States (Institutional/Arbitrage): Western democracies and wealthy states automatically scored high on capacity metrics. Secondary beneficiaries—receive intervention authorization precedent, policy influence in low-capacity states, and economic access justified through 'capacity building' programs. Arbitrage exit option: can selectively participate in multilateral frameworks.
 *   - Regional Middle Powers (Moderate/Constrained): States with moderate capacity scores. Constrained by the framework—must maintain sufficient capacity markers to retain autonomy; vulnerable to downgrade. Also benefit from stabilization of neighboring low-capacity states.
 *   - Postcolonial Sovereigns (Moderate/Trapped): States emerging from colonial rule. Structurally vulnerable to capacity-based reclassification as requiring external management. Accepting capacity metrics may enable escape from this reading but potentially locks them into institutional subordination.
 *   - Westphalian Sovereignty Ritual (Institutional/Arbitrage): The UN Charter commitment to equal sovereignty and non-intervention. Performs legitimacy function (maintains legal fiction of equality) while the graded framework operates in parallel. Maintained through institutional inertia rather than functional fit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.58).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.62).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Intervention Legitimacy Calibrated to Capacity Deficits").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '1322a357-9ce6-4145-9abc-4741de5e7f0e').
narrative_ontology:cs_kernel_codification('1322a357-9ce6-4145-9abc-4741de5e7f0e', formalized).
narrative_ontology:cs_authority_grounding('1322a357-9ce6-4145-9abc-4741de5e7f0e', extraction).
narrative_ontology:cs_interpretation_layer_present('1322a357-9ce6-4145-9abc-4741de5e7f0e').
narrative_ontology:cs_reading_relation('1322a357-9ce6-4145-9abc-4741de5e7f0e', westphalia_sovereignty__absolute_non_intervention, coexists_with).
narrative_ontology:cs_reading_relation('1322a357-9ce6-4145-9abc-4741de5e7f0e', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('1322a357-9ce6-4145-9abc-4741de5e7f0e', foundational, capacity_variance_legitimates_intervention).
narrative_ontology:cs_axiom_status(capacity_variance_legitimates_intervention, holdable).
narrative_ontology:cs_axiom_grounding('1322a357-9ce6-4145-9abc-4741de5e7f0e', capacity_variance_legitimates_intervention, instrumental).
narrative_ontology:cs_axiom('1322a357-9ce6-4145-9abc-4741de5e7f0e', foundational, paternalistic_oversight_protects_weak_sovereigns).
narrative_ontology:cs_axiom_status(paternalistic_oversight_protects_weak_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('1322a357-9ce6-4145-9abc-4741de5e7f0e', paternalistic_oversight_protects_weak_sovereigns, deontological).
narrative_ontology:cs_reference_frame('1322a357-9ce6-4145-9abc-4741de5e7f0e', equal_territorial_sovereignty_principle).
narrative_ontology:cs_drift_state('1322a357-9ce6-4145-9abc-4741de5e7f0e', contemporary_post_cold_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1322a357-9ce6-4145-9abc-4741de5e7f0e', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, high_capacity_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, postcolonial_sovereigns).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAK STATE (SNARE) — Trapped in a hierarchy where 'lack of capacity' justifies perpetual intervention. Cannot exit the framework; withdrawal of authorization to prove capacity only triggers humanitarian intervention justifications. Structurally similar to colonial-era protectorates: the target is told they are being protected, not colonized. Extraction flows away from the weak state as the capacity-evaluation apparatus dictates policy, deploys advisors, conditions aid, and extracts rents through trusteeship arrangements. No coordination function visible from this perspective.
constraint_indexing:constraint_classification(westphalia_sovereignty__graded_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL MIDDLE POWER (TANGLED ROPE) — Mixed position: benefits from capacity-based intervention framework when neighboring low-capacity states are stabilized (reducing transnational spillover, enabling trade). Also constrained by the same framework — must maintain sufficient capacity metrics to retain autonomous decision-making, and faces cost of compliance with international capacity standards. Coordination function: the graded framework enables collective action on shared problems (refugee flows, transnational crime). Extraction: constrained to maintain capacity markers; vulnerability to capacity-downgrade justifying intervention. Moderate effective extraction due to constrained exit and organizational capacity.
constraint_indexing:constraint_classification(westphalia_sovereignty__graded_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-CAPACITY STATE (ROPE) — Benefits from the graded framework as a matter of default authority. High capacity scores are quasi-automatic for wealthy democracies regardless of actual institutional performance (US court system dysfunction, infrastructure decay, or electoral legitimacy questions do not trigger capacity reviews). The constraint enables coordination: weak states' compliance with international standards (IMF fiscal rules, governance benchmarks, rule-of-law metrics) reduces transaction costs for trade and investment. Arbitrage option allows selective participation in multilateral frameworks. Net beneficiary; experiences constraint as legitimate coordination mechanism.
constraint_indexing:constraint_classification(westphalia_sovereignty__graded_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPACITY-EVALUATION AUTHORITY (TANGLED ROPE) — The institution (IMF, World Bank, UNHCR, ICC) that measures and certifies state capacity. Coordination function: translating diffuse concerns about governance, fiscal sustainability, or humanitarian risk into actionable capacity metrics that enable collective response. Extraction: the authority gains institutional power, budget justification, and personnel deployment authority from the perpetual deficit it evaluates. There is genuine tension here — the institution may sincerely believe its capacity metrics are accurate, yet benefits from finding capacity deficits. Active enforcement required: regular assessments, conditionality reviews, structural adjustment programs. Constrained exit: the institution cannot abandon the metric system without losing its own legitimacy and resource flows.
constraint_indexing:constraint_classification(westphalia_sovereignty__graded_sovereignty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WESTPHALIAN SOVEREIGNTY RITUAL (PITON) — The formal legal commitment to equal, non-interventionist sovereignty persists in the UN Charter and international law texts despite the graded capacity framework that systematically circumvents it. The ritual of 'respecting sovereignty' is performed (abstention votes in Security Council, acknowledgment of legal equality) while the real mechanism (capacity evaluation, conditional aid, trusteeship) operates in parallel. Theater is high because the formal rule-set contradicts the actual decision rule. Piton emerges from the gap between what the legal system claims (equal sovereignty) and what it does (graded intervention). Maintained through institutional inertia: the UN Charter is foundational; replacing it would require consensus that capacity-based tiering is legitimate (which high-capacity states avoid openly admitting).
constraint_indexing:constraint_classification(westphalia_sovereignty__graded_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, state capacity legitimately varies, and differential intervention responses are structurally necessary. Some polities lack administrative capacity to fulfill basic state functions (taxation, courts, security). This is a natural fact. Allowing genuine state collapse creates humanitarian crises and transnational spillover. Therefore, intervention scaled to capacity deficits is an immutable feature of international order — as inevitable as gravity. However, this reading naturalizes what the structural data reveals as a constructed hierarchy calibrated to serve the interests of evaluation authorities and high-capacity states. Engine false-summit detection applies.
constraint_indexing:constraint_classification(westphalia_sovereignty__graded_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westphalia_sovereignty__graded_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westphalia_sovereignty__graded_sovereignty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, TR),
    TR >= 0.70.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The capacity framework enables direct extraction: policy conditionality that flows wealth toward creditors, capacity-building contracts awarded to evaluator-states' firms, management of fiscal resources through IMF programs, and control of institutional design (central bank independence, procurement rules, regulatory frameworks). The extraction is not maximal because the coordination function is genuine—reducing spillover from state collapse serves all parties, and some capacity metrics correspond to real institutional challenges. However, the extent of extraction is substantially higher than the coordination benefits justify, indicating hybrid rather than pure coordination. Suppression (0.62): High-moderate. Multiple mechanisms: (1) Lock-in through conditionality: accepting IMF programs constrains fiscal autonomy, making exit costly. (2) Capacity metrics as moving targets: states adopting Western institutional forms are still vulnerable to capacity downgrade if compliance wanes. (3) Metric capture: the evaluation framework itself is designed by evaluators, creating structural bias toward identifying deficits. (4) Vocabulary constraint: arguing against 'capacity' assessment is itself labeled as 'nationalist rejection of standards,' reducing rhetorical exit. Theater ratio (0.68): High-moderate, and increasing over the 50-year interval (0.55 → 0.76). The Westphalian ritual persists (UN Charter language, voting procedures, formal sovereignty acknowledgment) while capacity-based decision-making has intensified. The gap between formal law (equal sovereignty) and operational rule (graded intervention) widens as more states move under capacity-based conditionality. Theater increases because more capacity assessments, more structural adjustment programs, more peacekeeping operations require rhetorical work to square with the non-intervention principle. The expansion of 'human rights,' 'humanitarian intervention,' and 'responsibility to protect' doctrines are theater—they rationalize the same underlying graded framework using new vocabulary.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between the low-capacity state (Snare) and the high-capacity state (Rope) is maximal. Both experience the same formal rule (capacity assessment justifies differential treatment), but the low-capacity state sees extraction without coordination function (no choice, no benefit, perpetual evaluation), while the high-capacity state sees coordination without extraction (legitimate framework, voluntary participation). The analytical observer risks conflating this gap—seeing capacity-based hierarchy as inevitable rather than constructed—and thus naturalizing what the structural data reveals as an institutional arrangement that serves high-capacity states and evaluation authorities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Low-capacity states are victims with trapped exit options (cannot exit the evaluation framework without incurring worse penalties); high-capacity states are beneficiaries with arbitrage options (can selectively participate or withdraw). Evaluation authorities are beneficiaries (institutional power, budget growth) but constrained (must justify continued engagement). The engine derives d from these structural relationships and applies the sigmoid f(d) to compute experienced extractiveness per perspective. Low-capacity states experience d ≈ 0.92 (near-maximal target status): trapped exit + victim status → f(d) ≈ 1.40 → high chi. High-capacity states experience d ≈ 0.08 (near-maximal beneficiary status): arbitrage exit + beneficiary status → f(d) ≈ -0.18 → negative chi (net benefit). Evaluation authorities experience d ≈ 0.65 (symmetric position): constrained exit + institutional beneficiary status → f(d) ≈ 1.00 → moderate chi. The directionality asymmetry is the heart of the extraction mechanism: the same constraint produces opposite effective extractiveness depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through explicit consideration of what legitimates the hierarchy. The graded reading claims: capacity variation is real (empirical), intervention scaling is necessary (consequentialist), and the hierarchy serves coordination (functional). The absolute_non_intervention reading claims: equal sovereignty is the legitimate rule regardless of capacity variation. The conditional_responsibility reading claims: intervention is legitimate only for specific harms (genocide, mass atrocities), not for general capacity deficits. These readings are not empirically resolvable—they presuppose different legitimate kernels (cosmopolitan vs. Westphalian). The mandatrophy manifests as a choice point: which kernel is the grounded commitment? If Westphalian (equal sovereignty), graded intervention is a reading violation, and the extraction is illegitimate extraction. If cosmopolitan (variable human capacity with global responsibility), graded intervention is a legitimate tiering mechanism, and what looks like extraction from low-capacity states is actually paternalistic coordination. The omegas document this choice at multiple levels: kernel affinity (Westphalian vs. cosmopolitan), empirical closure (whether exit conditions exist in practice), and institutional incentives (whether evaluation authorities are structurally biased toward deficit identification). No amount of metric refinement resolves the mandatrophy because the mandatrophy is structural—it flows from the reading's presupposition of which kernel is legitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metric_endogeneity,
    'Do capacity metrics measure intrinsic state capability or reflect the metrication itself—i.e., are states ''low-capacity'' because they fail to adopt the evaluation framework, or do they genuinely lack administrative capacity?',
    'Counterfactual analysis: states that adopt Western governance standards vs. those that reject them but maintain functional fiscal capacity, security provision, and social stability. Historical comparison: Botswana (adopted metrics, high scores, high autonomy) vs. Singapore (rejected Western metrics early, achieved high capacity, negotiated autonomy). Vietnam and Rwanda post-conflict trajectories.',
    'If endogenous to metrication: the capacity framework is a Snare mechanism dressed as technical assessment—low scores justify intervention, which justifies the metric adoption demand. If capacity is intrinsic: the framework legitimately identifies genuine governance gaps. If mixed: clarify which dimensions are intrinsic vs. metricated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_metric_endogeneity, empirical, 'Whether capacity metrics measure intrinsic capacity or reify the metrication framework itself').

omega_variable(
    intervention_exit_condition,
    'What objective condition ends capacity-based intervention? When is a state deemed to have graduated from the low-capacity tier such that intervention authority terminates?',
    'Historical record: of states that were once subjects of intense intervention (IMF structural adjustment, UN trusteeship, ICC investigation), how many have achieved metrics-based graduation and been released from conditionality? Time from intervention onset to full autonomy restoration.',
    'If no documented exit condition: the snare classification is confirmed—the mechanism is predatory and designed for perpetual extraction. If exit conditions exist but are rarely met: the constraint is tangled rope with a broken sunset clause. If many states graduate: the coordination function is real and the ranking is legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_exit_condition, empirical, 'Whether capacity-based intervention has objective exit conditions or is designed for perpetual engagement').

omega_variable(
    reading_foreclosure_question,
    'Does the graded sovereignty reading logically foreclose the absolute non-intervention reading, or do they coexist as different parties'' commitments in the same system?',
    'Logical analysis: can a state simultaneously claim ''my sovereignty is absolute and not subject to capacity evaluation'' while other states claim ''intervention is legitimate when capacity deficits create spillover risks''? If both positions have adherents with no meta-framework to adjudicate between them, they coexist. If the graded reading''s logic (capacity variation implies differential intervention) directly contradicts non-intervention''s core premise (all states have equal authority), they may foreclose.',
    'If foreclose: one reading must be abandoned. If coexist: the conflict is structural and irresolvable within existing frameworks. If influences: the graded reading creates pressures that slowly erode non-intervention, but doesn''t logically eliminate it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_question, conceptual, 'Structural relationship between graded sovereignty and non-intervention readings').

omega_variable(
    postcolonial_sovereignty_recovery,
    'Can a postcolonial state that was deemed ''low-capacity'' and subject to intervention recover full autonomy and capacity metrics acceptance simultaneously, or does recovery through Western-defined metrics lock it into perpetual hierarchy?',
    'Trajectories of postcolonial states: those that accepted capacity frameworks early (Ghana, Kenya, Senegal) vs. those that rejected them (Zimbabwe pre-2008, Eritrea, North Korea). Measurement: autonomy in resource allocation, ability to refuse conditionality, capacity to negotiate international agreements on own terms vs. under trusteeship arrangements.',
    'If acceptance of metrics = de facto perpetual hierarchy: the graded reading captures victims in institutional subordination. If acceptance = pathway to equal sovereignty: the reading is an interim framework with real exit. If mixed: document which states achieve graduation and which remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postcolonial_sovereignty_recovery, empirical, 'Whether postcolonial sovereign recovery is possible within graded frameworks').

omega_variable(
    benchmark_capture_risk,
    'Do capacity evaluation authorities (World Bank, IMF, UNHCR) become functionally dependent on identifying capacity deficits such that their institutional incentives favor pessimistic assessments?',
    'Institutional analysis: budget allocation tied to crisis magnitude, personnel deployment linked to assessment findings, organizational growth correlated with number of states under conditionality. Comparison of assessments during budget cycles vs. off-cycle. Exit: what conditions would allow an evaluation authority to declare a state ''graduated'' and withdraw?',
    'If authorities are incentivized toward deficit identification: the snare reading is strengthened—extraction is embedded in institutional structure. If authorities face equal incentives for graduation vs. continued assessment: mixed extraction. If authorities can point to documented graduations: coordination function is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(benchmark_capture_risk, empirical, 'Institutional incentive alignment of capacity evaluation authorities').

omega_variable(
    cosmopolitan_vs_state_centric_kernel,
    'Is the Westphalian kernel (equal territorial sovereigns) or a cosmopolitan kernel (global humanity with variable institutional capacity) the legitimate grounding for international order? Does the graded reading require adopting cosmopolitan commitments?',
    'Normative analysis: which kernel''s axioms are presupposed by each reading? Does graded sovereignty require rejecting Westphalian equality as a foundational claim, or can it coexist as an interim technical framework?',
    'If graded reading requires cosmopolitan kernel: it forecloses Westphalian non-intervention reading. If graded reading can be presented as technical calibration of Westphalian framework: readings coexist. If cosmopolitan kernel is gaining institutional ground: graded reading is symptomatic of kernel migration, not mere reading coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmopolitan_vs_state_centric_kernel, conceptual, 'Whether graded sovereignty reading presupposes cosmopolitan or Westphalian kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ws_graded_theater_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ws_graded_theater_t25, westphalia_sovereignty__graded_sovereignty, theater_ratio, 25, 0.68).
narrative_ontology:measurement(ws_graded_theater_t50, westphalia_sovereignty__graded_sovereignty, theater_ratio, 50, 0.76).

% Extraction over time
narrative_ontology:measurement(ws_graded_extract_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ws_graded_extract_t25, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(ws_graded_extract_t50, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ws_graded_suppress_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ws_graded_suppress_t25, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(ws_graded_suppress_t50, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, imf_structural_adjustment).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, un_trusteeship_authority).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, humanitarian_intervention_doctrine).

% DUAL FORMULATION NOTE:
% The graded sovereignty reading and its sibling readings (absolute non-intervention, conditional responsibility) are distinct constraints from the same kernel, sharing the same base commitment (Westphalian sovereignty) but diverging on interpretation. Decomposition is essential: each reading has its own ε, its own beneficiary/victim structure, and its own perspectival profile. The graded reading (this file) has ε=0.58 (tangled rope: mixed coordination and extraction). The absolute non-intervention reading would have ε≤0.35 (rope: coordination without asymmetric extraction). The conditional reading would have intermediate ε. They are NOT observable-dependent readings of one constraint; they are structurally distinct commitments grounded in different axioms about what legitimates intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
