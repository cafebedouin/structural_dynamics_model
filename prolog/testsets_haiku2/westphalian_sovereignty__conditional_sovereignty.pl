% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty: Responsibility Doctrine and Intervention Authority
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The conditional sovereignty doctrine asserts that state sovereignty is
 *   not absolute but conditional on a state's responsibility to prevent
 *   systematic human rights violations within its borders. When a state fails
 *   that responsibility, external intervention becomes legitimate. This
 *   reading instantiates one position within the contested Westphalian
 *   sovereignty kernel. The reading benefits intervention advocates and
 *   international governance institutions by providing legal and moral
 *   language for external pressure, while constraining weaker states by
 *   subjecting them to external judgment and potential intervention. The
 *   constraint is classified as a snare because it operates as enforced
 *   asymmetric extraction: weak states are targets of the suppression
 *   machinery (investigation, sanctions, intervention threat), while strong
 *   states author the doctrine's definitions and control interpretation
 *   thresholds. The originalist alternative (absolute_sovereignty) claims no
 *   external authority can judge domestic affairs; the accommodationist
 *   alternative (graduated_sovereignty) distributes sovereignty on a spectrum
 *   tied to state capacity. This reading occupies the middle ground:
 *   sovereignty exists conditionally, and external authority legitimately
 *   enforces the condition.
 *
 * KEY AGENTS:
 *   - intervention_advocating_powers: Strong democracies and institutional actors (power/institutional) — benefit from authority to judge and intervene; set thresholds and definitions
 *   - non_aligned_developing_states: Moderate-power states (power/moderate) — bear costs of scrutiny, sanctions, intervention threat; constrained exit
 *   - weak_governance_states: Powerless fragile/failing states (power/powerless) — most vulnerable to intervention thresholds; trapped exit options
 *   - international_governance_institutions: UN, ICC, regional bodies (power/institutional) — gain mandate expansion and legitimacy from the doctrine
 *   - human_rights_advocacy_networks: Transnational NGOs (power/organized) — gain authority and funding by documenting violations that trigger the doctrine
 *   - absolute_sovereignty_advocates: Excluded states and scholars (power/institutional) — argue the doctrine masks imperialism; structurally excluded from defining it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.62).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty: Responsibility Doctrine and Intervention Authority").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '4600ae67-cc09-4b66-97f0-89ef0d8e36ad').
narrative_ontology:cs_kernel_codification('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', fixed_text).
narrative_ontology:cs_authority_grounding('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', extraction).
narrative_ontology:cs_interpretation_layer_present('4600ae67-cc09-4b66-97f0-89ef0d8e36ad').
narrative_ontology:cs_reading_relation('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', foundational, sovereignty_conditional_on_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', sovereignty_conditional_on_responsibility, deontological).
narrative_ontology:cs_axiom('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', foundational, external_authority_legitimate_for_grave_violations).
narrative_ontology:cs_axiom_status(external_authority_legitimate_for_grave_violations, holdable).
narrative_ontology:cs_axiom_grounding('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', external_authority_legitimate_for_grave_violations, deontological).
narrative_ontology:cs_axiom('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', secondary, responsibility_doctrine_prevents_atrocities).
narrative_ontology:cs_axiom_status(responsibility_doctrine_prevents_atrocities, holdable).
narrative_ontology:cs_axiom_grounding('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', responsibility_doctrine_prevents_atrocities, empirically_contingent).
narrative_ontology:cs_reference_frame('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', universal_human_rights_accountability).
narrative_ontology:cs_drift_state('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', contemporary_post_iraq_afghanistan, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4600ae67-cc09-4b66-97f0-89ef0d8e36ad', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_advocating_powers).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, non_aligned_developing_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, weak_governance_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, affected_domestic_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, affected_domestic_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wealthy democracies and established international powers interpret the responsibility doctrine to justify humanitarian intervention, sanctions regimes, and institutional pressure on states they judge to be systematically violating human rights. They set the threshold interpretations, commission investigations that trigger the doctrine, and lead coalitions for intervention. They claim to act on universal moral principle; critics argue they act on geopolitical interest.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_advocating_powers, agenda_setter,
    institutional, generational, analytical, global).

% Subject to the doctrine without authoring its definitions. They bear the costs of having their domestic policies scrutinized by external powers, face sanctions and isolation when judged non-compliant, and risk military intervention or regime-change pressure. Their exit option is constrained by their dependence on global institutions and markets that intervening powers influence. They argue the doctrine is instrumentalized to justify imperialism and erode genuine sovereignty.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_aligned_developing_states, payer,
    moderate, generational, constrained, global).

% The UN, ICC, international human rights bodies, and regional governance structures gain legitimacy, mandate expansion, and operational authority from the responsibility doctrine. They interpret violations, authorize investigations, and coordinate responses. Their institutional existence and influence depend on states accepting the doctrine's legitimacy, so they actively defend and refine it.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_governance_institutions, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, international_governance_institutions, agenda_setter).

% NGO networks, transnational advocacy groups, and human rights monitors gain funding, authority, and policy influence by documenting violations and triggering the doctrine's investigative mechanisms. They operate within the framework the doctrine establishes and depend on it for their institutional standing and effectiveness in their stated mission.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, human_rights_advocacy_networks, beneficiary,
    organized, biographical, constrained, global).

% Fragile or failing states with limited institutional capacity, post-conflict environments, and states with genuine governance crises. They are most vulnerable to external judgment and intervention thresholds because they lack the diplomatic, legal, and enforcement resources to contest characterizations of violations or to negotiate alternative arrangements. Their sovereignty is conditional in fact as well as in doctrine.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, weak_governance_states, payer,
    powerless, biographical, trapped, global).

% States and scholars who reject the responsibility doctrine and defend absolute sovereignty argue that conditioning sovereignty on external judgment creates a hierarchy in which powerful states police weaker ones, masking power politics as principle. Their position is structurally excluded from the architecture the doctrine establishes; they can resist and appeal but cannot author the doctrine's definitions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, absolute_sovereignty_advocates, excluded,
    institutional, generational, constrained, global).

% Civilians in states judged to be violating human rights may benefit from protection and eventual accountability brought by external intervention, or may suffer war, displacement, and state collapse that intervention triggers. They are rarely consulted in decisions about whether external intervention serves their interests, and often bear the highest costs of both violation and remedy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, affected_domestic_populations, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, affected_domestic_populations, payer).

% Legal scholars, philosophers, and analysts across the spectrum observe the doctrine's operation and contest its interpretation. They produce evidence about whether it is applied consistently, what it costs, and whether it actually prevents violations or simply rearranges power.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_legal_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, intervention_advocating_powers).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared normative framework: states claim to accept responsibility for preventing systematic human rights violations within their borders, and the international community claims authority to monitor and enforce that responsibility through investigation, sanctions, and intervention when violations are grave enough.
% TRANSFER_FUNCTION: Transfers sovereignty authority from individual states to a distributed international system: states cede unconditional authority over domestic affairs and accept external judgment on whether their practices meet responsibility thresholds; intervening powers gain authority to judge, investigate, and impose costs on states failing those thresholds.
% ABSENT_VOICES: Populations of non-aligned states and developing countries are rarely present in forums that define violation thresholds, determine when intervention is justified, or assess whether intervention actually reduces violations. Scholars and states that reject the doctrine are excluded from authoring its definitions, though they may contest them.
% DISAPPEARANCE_RATIONALE: If the conditional sovereignty doctrine disappeared, international governance would reorganize: absolute sovereignty would re-dominate, intervention would lose its legitimating language, intervention powers would lose international legal cover, and the architecture of institutional monitoring and investigation would collapse. Alternatively, unilateral power politics would operate without the constraint of invoking the responsibility doctrine as justification.
% FOUNDING_PROBLEM: The Holocaust and post-WWII atrocities demonstrated that purely internal state affairs could produce catastrophic human suffering; the founding problem is: what authority structure can prevent or intervene in systematic state-sponsored violence without creating a mechanism for powerful states to prey on weaker ones?
% FOUNDING_PROBLEM_CORROBORATION: Intervention advocates attest the problem remains live: active genocides, mass disappearances, systematic torture occur and international response is inadequate. Absolute sovereignty advocates and non-aligned states attest that the doctrine has become an instrument for geopolitical pressure and regime change disguised as principle. Empirical scholars document both: documented interventions that reduced violence, and documented interventions that destabilized regions and increased civilian harm, with no clear pattern correlated to stated rationale.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint transfers authority but leaves room for negotiation and resistance. It is measured as extractive rather than coordinative because the transfer asymmetrically favors strong states: they author definitions, judge violations, and enforce thresholds while weak states submit. Suppression is substantial (0.62) because the doctrine's enforcement depends on isolating and pressuring non-compliant states, and because weak states have limited ability to contest characterizations or appeal decisions. Theater ratio is moderate-high (0.41, rising over the interval) because much of the enforcement activity performatively enacts neutrality and universal principle while actually serving the geopolitical interests of the intervening powers. The measurement series show extraction and suppression rising from t0 to t20, then plateauing as the doctrine matured and its limits became visible (stalled interventions, failed outcomes, diminished belief in its efficacy). Theater ratio rises through the interval as institutional maintenance activity increases relative to functional intervention success.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of intervention advocates, the doctrine is genuine coordination: it establishes shared responsibility and provides mechanisms to prevent atrocities. From the seat of non-aligned states, the same structure operates as enforced extraction: they are subject to external judgment they did not author, face costs (sanctions, isolation, intervention) they cannot fully avoid, and lack power to redefine the thresholds or contest characterizations. The engine computes this divergence from the stakeholder power atoms and exit options: institutional seats with analytical exit (agenda-setter, international institutions) derive low directionality (beneficiaries); moderate and powerless seats with constrained or trapped exit (developing states, weak governance states) derive high directionality (targets). The claim is snare; the metrics describe the actual operation as asymmetrically extractive and enforcedly maintained.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervention_advocating_powers: d ≈ 0.15 (institutional power, analytical exit, set definitions, collect authority and legitimacy — net beneficiaries). International_governance_institutions: d ≈ 0.10 (institutional power, analytical exit, gain mandate and expansion — beneficiaries). Human_rights_advocacy_networks: d ≈ 0.20 (organized power, constrained exit, gain authority and funding but depend on the doctrine staying legitimate — beneficiaries with moderate directionality). Non_aligned_developing_states: d ≈ 0.70 (moderate power, constrained exit, subject to external judgment and costs, limited ability to resist or redefine — targets). Weak_governance_states: d ≈ 0.85 (powerless, trapped exit, most vulnerable to thresholds and intervention — high targets). Affected_domestic_populations: d ≈ 0.55 (powerless, trapped, may benefit from intervention protection or suffer intervention harms, no seat in decisions — symmetric-target). Absolute_sovereignty_advocates: d ≈ 0.65 (institutional power but structurally excluded from defining the doctrine, must resist rather than author — modified targets).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and contested: systematic human rights violations do occur, and the doctrine was built to address them. However, the measured operation shows the doctrine has partially inverted into a mechanism for geopolitical pressure and regime-change justification. The snare classification captures this: the coordination function (preventing atrocities) is real and stated; the extraction function (transferring authority to strong states, isolating non-compliant weak states, providing cover for geopolitical intervention) is the constraint's actual persistence mechanism. Mandatrophy is partial: the founding problem remains alive, but the constraint's efficacy at addressing it is contested (some interventions prevented atrocities; some interventions destabilized regions and increased harm). The theater ratio rising over the interval suggests increasing performance relative to function — the doctrine's legitimating language outlives its demonstrated efficacy at actually preventing violations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_definition_authority,
    'Who has legitimate authority to define what constitutes ''systematic human rights violations'' severe enough to trigger intervention, and on what grounds?',
    'Examine whether intervention-triggering determinations are made by universally recognized process (Security Council consensus, ICC, treaty-defined mechanisms) or by dominant powers unilaterally declaring violations and using institutional machinery to ratify their judgment. Track cases where similar violations in aligned vs. non-aligned states received different responses.',
    'If threshold-setting is effectively unilateral by strong states, the constraint is pure extraction (snare); if genuinely universal process determines thresholds, the constraint has meaningful coordination content (tangled rope). The reading''s own claim relies on belief that thresholds are universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_definition_authority, empirical, 'Whether violation thresholds are set universally or reflect power politics.').

omega_variable(
    intervention_efficacy_vs_harm,
    'On net, does external intervention authorized under the responsibility doctrine reduce human rights violations and prevent atrocities, or does it increase civilian harm, state failure, and regional destabilization?',
    'Comparative analysis of countries with and without intervention: mortality data, state capacity metrics, democratization, institutional development, and reported human rights measures pre- and post-intervention. Separate cases where intervention prevented specific atrocities from cases where intervention triggered broader violence.',
    'If intervention demonstrably prevents net violations and atrocities, the constraint is genuine coordination with extraction overlay (tangled rope). If intervention on net increases harm, the constraint is pure extraction disguised as protection (snare). If the evidence is mixed and case-dependent, the classification remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_efficacy_vs_harm, empirical, 'Whether the doctrine''s enforcement mechanism actually achieves its stated human rights purpose.').

omega_variable(
    alternative_authority_structures,
    'Could the same coordination function (preventing systematic atrocities) be achieved through a different institutional structure — one where weak states had equal authoring power in defining violations, setting thresholds, and authorizing responses?',
    'Institutional design analysis: test whether alternative architectures (Global South veto on intervention, rotation of judgment authority, consensus thresholds, equal participation in threshold-setting) would be technically implementable and what they would cost. Examine proposals from non-aligned states and scholars.',
    'If alternatives are technically feasible and would reduce asymmetric extraction while maintaining coordination, the current structure''s extractiveness is not necessary (making it a pure snare). If alternatives would collapse coordination or prove unworkable, the asymmetry is the price of coordination (making it tangled rope). If alternatives would require different tradeoffs (slower response, higher threshold for certainty, more false negatives), the choice becomes visible as political rather than inevitable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_authority_structures, conceptual, 'Whether the extraction component is structurally inseparable from the coordination function.').

omega_variable(
    reading_foreclosure_question,
    'Does the conditional_sovereignty reading logically foreclose the absolute_sovereignty reading within a single coherent framework, or can both coexist as live options held by different parties?',
    'Test whether a state can simultaneously hold that (a) it has unconditional sovereignty over its domestic affairs AND (b) external intervention is legitimate when violations are grave. Can these coexist as different commitments to different audiences, or does one strictly contradict the other?',
    'If they strictly contradict within one framework, the relation is forecloses and the readings are locked in a zero-sum contest. If states and scholars can coherently hold both depending on context (unconditional sovereignty as default, external authority as override in exceptional cases), the relation is coexists_with and both readings remain live. Classification of the relation affects what the network model predicts for custody battles over the kernel''s interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_question, conceptual, 'Logical relationship between conditional and absolute sovereignty readings.').

omega_variable(
    legitimacy_grounding_shift,
    'Has the doctrine''s claimed grounding shifted from universal moral principle to pragmatic great-power interest, and if so, what visible evidence marks that shift?',
    'Discourse analysis of official justifications for intervention over time, compared to observed patterns (do strong-state allies receive intervention scrutiny equal to non-aligned states?). Track whether the doctrine''s language remains tied to human rights metrics or becomes increasingly tied to geopolitical rationales.',
    'If grounding has substantially shifted to pragmatic interest while language remains principled, the constraint is authentically a snare: the coordination cover is increasingly nominal, extraction and power-maintenance are increasingly visible. If language and practice remain aligned, the constraint retains tangled-rope characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_grounding_shift, empirical, 'Whether the doctrine''s legitimacy grounding has eroded from principle to power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(west_tr_t5, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 5, 0.28).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 10, 0.32).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 15, 0.38).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 20, 0.41).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 25, 0.42).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 30, 0.41).
narrative_ontology:measurement(west_tr_t35, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(west_be_t5, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 15, 0.37).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(west_be_t35, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 35, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(west_su_t5, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 5, 0.51).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(west_su_t35, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__conditional_sovereignty, 0.18).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, r2p_doctrine_operationalization).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_humanitarian_intervention_norms).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, state_capacity_legitimacy_coupling).

% DUAL FORMULATION NOTE:
% The conditional_sovereignty reading is one of three instantiations of the westphalian_sovereignty kernel. The absolute_sovereignty reading (no external authority) directly contests this reading's core premise. The graduated_sovereignty reading accommodates both by distributing sovereignty on a capacity spectrum. All three are live positions in contemporary international law and political theory; this story models the conditional reading's structure, beneficiary map, and operational costs. Separate JSON files for the other readings provide their ε values, stakeholder configurations, and type classifications. The network links show that changes to the interpretation of one reading (e.g., stricter threshold-setting for intervention) create structural pressure on the others (graduated_sovereignty becomes more attractive; absolute_sovereignty's resistance to external authority grows).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
