% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Capacity-Calibrated Intervention Authority
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The graded sovereignty reading treats sovereignty as a spectrum:
 *   territorial authority exists on a continuum from full (high-capacity
 *   Western democracies) to nominal (failed or fragile states). This reading
 *   instantiates one specific normative claim about how legitimacy should be
 *   distributed: that intervention becomes justified when measurable capacity
 *   deficits appear. The sibling readings—absolute non-intervention
 *   (sovereignty is categorical, not scalar) and conditional responsibility
 *   (sovereignty is conditional on atrocity prevention, not capacity
 *   measurement)—are distinct constraints in the family, not perspectives on
 *   this one. This story instantiates graded sovereignty and its
 *   architectural consequences: it creates a hierarchical state system,
 *   elevates capacity metrics to a status functionally equivalent to
 *   classical law, and ensures that states classified as low-capacity remain
 *   perpetually subject to external correction. The claim and metrics are
 *   deliberately independent: the reading is CLAIMED as tangled_rope
 *   (coordination function + asymmetric extraction + active enforcement) and
 *   the metrics describe what that reading produces in operation.
 *
 * KEY AGENTS:
 *   - Capacity evaluation authorities (IMF, World Bank, UN, regional development banks): institutional power to define and measure state capacity; beneficiaries of the system through expanded authority and budgets
 *   - Interventionist powers (NATO, US, regional powers): institutional power to act on capacity classifications; beneficiaries through legitimacy and influence over low-capacity governance
 *   - Weak states (post-colonial, fragile, security-gap states): classified low-capacity; perpetually subject to external conditionality and oversight; victims whose decision-making authority is extracted
 *   - Postcolonial states: moderate institutional power but identity-locked to Western development paths; trapped in metrics designed by their former colonizers
 *   - High-capacity states: classified high; reference standard; exempt from oversight while positioned to oversee others
 *   - Global South coalitions: organize resistance but constrained by the power asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Capacity-Calibrated Intervention Authority").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '022b0a30-69be-4f2a-a703-59fd95e9d9c2').
narrative_ontology:cs_kernel_codification('022b0a30-69be-4f2a-a703-59fd95e9d9c2', distributed).
narrative_ontology:cs_authority_grounding('022b0a30-69be-4f2a-a703-59fd95e9d9c2', extraction).
narrative_ontology:cs_interpretation_layer_present('022b0a30-69be-4f2a-a703-59fd95e9d9c2').
narrative_ontology:cs_reading_relation('022b0a30-69be-4f2a-a703-59fd95e9d9c2', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('022b0a30-69be-4f2a-a703-59fd95e9d9c2', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('022b0a30-69be-4f2a-a703-59fd95e9d9c2', foundational, sovereignty_scalar_capacity_dependent).
narrative_ontology:cs_axiom_status(sovereignty_scalar_capacity_dependent, holdable).
narrative_ontology:cs_axiom_grounding('022b0a30-69be-4f2a-a703-59fd95e9d9c2', sovereignty_scalar_capacity_dependent, empirically_contingent).
narrative_ontology:cs_axiom('022b0a30-69be-4f2a-a703-59fd95e9d9c2', foundational, intervention_legitimacy_calibrated_to_capacity_deficit).
narrative_ontology:cs_axiom_status(intervention_legitimacy_calibrated_to_capacity_deficit, holdable).
narrative_ontology:cs_axiom_grounding('022b0a30-69be-4f2a-a703-59fd95e9d9c2', intervention_legitimacy_calibrated_to_capacity_deficit, instrumental).
narrative_ontology:cs_reference_frame('022b0a30-69be-4f2a-a703-59fd95e9d9c2', westphalian_categorical_sovereignty).
narrative_ontology:cs_drift_state('022b0a30-69be-4f2a-a703-59fd95e9d9c2', contemporary_post_cold_war_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('022b0a30-69be-4f2a-a703-59fd95e9d9c2', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, interventionist_powers).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_security_organizations).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, postcolonial_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, capacity_deficit_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, high_capacity_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, global_south_coalitions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International organizations, donor governments, and multilateral institutions assess and classify state capacity across dimensions: fiscal, institutional, security apparatus, rule-of-law infrastructure, corruption indices. Their evaluations determine who is eligible for intervention, technical assistance, conditional aid, and trusteeship regimes. Control over capacity metrics translates directly to agenda-setting authority over weak-state governance.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, beneficiary).

% High-capacity states and military coalitions (NATO, regional powers, US-led coalitions) claim legitimacy to intervene in low-capacity jurisdictions on grounds of security, humanitarian, or anti-terrorism rationales. The graded sovereignty reading provides the normative architecture for justifying these interventions: intervention becomes a calibrated response to measured capacity deficits, not an exercise of raw power.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, interventionist_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% States classified as having low state capacity experience continuous external oversight, conditional aid programs with governance requirements, technical advisors embedded in ministries, and explicit conditionality on sovereign decision-making. They cannot exit the hierarchy: remaining unclassified or low-capacity is the mechanism of extraction. Their nominal territorial authority is systematically subordinated to external evaluation and correction.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, weak_states, excluded).

% Formally sovereign nations whose institutional capacity was deliberately undermined during colonial extraction and whose state-building has proceeded under constant external pressure to adopt Western institutional forms. The capacity metric becomes a moving target: Western-defined success criteria ensure they cannot escape the low-capacity classification without abandoning their own institutional traditions and sovereignty expressions.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, postcolonial_states, payer,
    moderate, generational, identity_locked, national).

% UN bodies, regional security forums, and peacekeeping mandates derive authority and resources from managing capacity-deficit interventions. The graded sovereignty framework legitimates their existence: they exist to remediate state capacity gaps. Absence of such gaps would eliminate their primary function and budget.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_security_organizations, beneficiary,
    institutional, generational, arbitrage, global).

% Western democracies and established liberal states occupy the top tier of the capacity hierarchy. Their institutions are the reference standard against which all others are measured. They are systematically classified as high-capacity, which shields them from external intervention oversight while legitimating their oversight of others.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, high_capacity_states, beneficiary,
    institutional, generational, analytical, global).

% Communities within low-capacity states experience both the original capacity deficit (poor service delivery, corruption, security provision failure) AND the extraction regime imposed by external corrective authorities. They are invoked as humanitarian beneficiaries of intervention (justifying the arrangement) but are almost never consulted on intervention terms or implementation. Their exclusion from the agenda is structural.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, marginalized_populations_in_weak_states, excluded,
    powerless, immediate, trapped, local).

% Non-Western, non-liberal institutional models (traditional authority structures, Islamic legal frameworks, communal governance systems) are systematically classified as capacity deficits when they diverge from Western institutional forms. These alternative models are treated as problems to be fixed rather than legitimate expressions of sovereignty.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, alternative_state_models, excluded,
    analytical, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(westphalia_sovereignty__graded_sovereignty, alternative_state_models).

% Coalitions of weaker and post-colonial states dispute the capacity metrics and resist external oversight, but lack the institutional power to either exit the system or to redefine the evaluation criteria. Their resistance is real but operates within constraints set by the capacity-evaluation authorities.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, global_south_coalitions, payer,
    organized, generational, constrained, global).

% International relations scholars, legal theorists, and policy analysts studying the legitimacy and effects of graded sovereignty. They produce the conceptual and empirical arguments that make the reading coherent but do not directly benefit or pay.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, observer_academic_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for legitimate cross-border action in response to genuine state capacity failures: security provision, institutional corruption, mass violence, economic collapse. Solves the coordination problem of which external actors may legitimately intervene, on what grounds, under what conditions, and to what end—replacing ad hoc great-power intervention with rule-based legitimacy claims tied to measurable capacity deficits.
% TRANSFER_FUNCTION: Transfers decision-making authority from low-capacity-classified states to external evaluation bodies and interventionist powers. Moves governance capacity, institutional control, and policy discretion from weak-state capitals to international organizations and high-capacity state actors. The extraction is institutional sovereignty itself: the ability of a state to make binding decisions within its borders without external override.
% ABSENT_VOICES: Populations in low-capacity states who experience intervention are almost never present at the legitimacy-establishing forums; their lived experience of what works and what fails is excluded from the metrics that classify their state. Alternative state models and non-Western governance traditions are structurally excluded from the category of 'legitimate' institutional forms, meaning they cannot score high on capacity measures regardless of their actual performance.
% DISAPPEARANCE_RATIONALE: If the graded sovereignty framework disappeared, the legitimacy architecture for external intervention would collapse. Interventionist powers would lose their normative justification for cross-border action; low-capacity states would gain formal territorial inviolability; the entire system of conditional aid, technical conditionality, and institutional oversight would require reconstruction. The hierarchy of states would flatten to formal equality, which would require major powers to renegotiate their intervention authority on different grounds.
% FOUNDING_PROBLEM: Post-WWII and especially post-Cold War international system faced recurring crises where state failure created humanitarian disasters, security vacuums filled by non-state actors, and refugee flows that destabilized regions. Weak institutions enabled mass atrocities, collapsed public health systems, and ungoverned spaces that became terrorism havens. The founding problem was: what legitimizes external intervention when internal sovereign governments are incapable of basic state functions?
% FOUNDING_PROBLEM_CORROBORATION: High-capacity states and international organizations (IMF, World Bank, UN Development Programme) attest that capacity deficits remain live and intervention remains necessary. Scholars of state fragility and security studies (Rotberg, Herbst, Fukuyama) document that genuine capacity crises exist. However, postcolonial scholars (Achille Mbembe, Sylvia Wynter, scholars of African political thought) and Global South governments dispute whether capacity-as-defined is a real property or a constructed hierarchy designed to justify continued subordination. No corroboration from weak-state governments themselves supports the framework.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at 2025) because the constraint transfers decision-making authority and institutional sovereignty from low-capacity to high-capacity seats. The transfer is described as technical correction but operates as a hierarchical subordination. Suppression is also high (0.72) because the constraint requires active enforcement: weak states must be continuously monitored, evaluated, and corrected; resistance from low-capacity governments and Global South coalitions must be managed through conditionality (withdrawal of aid, sanctions, institutional exclusion). Theater is moderate-high (0.58) because the capacity metrics themselves perform a legitimating function—they are real (institutional corruption, security gaps, fiscal deficits exist) but their framing as the master variable for sovereignty is chosen, not discovered. The measurement series shows extraction accumulation over time: from 1945 (early Cold War, when sovereignty remained relatively categorical despite superpower competition) through 2025, the capacity framework progressively tightened. The theater ratio rose as the metrics became more elaborate and the corrective apparatus more sophisticated, even as genuine coordination function diminished (the founding problem of preventing state collapse was real post-1970; by 2025 the framework functions more to manage great-power competition for influence than to address genuine capacity crises). The suppression series runs one shared time grid so every metric is observable at each examined period.
 *
 * PERSPECTIVAL GAP:
 *   The graded sovereignty reading produces stark seat divergence. From the capacity-evaluation authority seat, the constraint is genuine coordination: weak institutions DO create humanitarian crises, and external assistance IS coordinated through this framework. From the weak-state seat, the same structure operates as extraction: they are evaluated by criteria they did not choose, using institutional reference standards from their former colonizers, in forums they do not control, with no exit available. The engine computes these divergences from the structural data: institutional vs. powerless seats, arbitrage vs. trapped exits, evaluation authority vs. evaluated subjected. The perspectival gap is not reconcilable—it is structural to the constraint. High-capacity states occupy a third seat: they are beneficiaries (the framework legitimates their intervention authority) but not primary targets (they are not continuously evaluated and corrected). The divergence is why this is a tangled rope, not a rope: the coordination function (preventing state collapse, enabling collective response to security crises) is real, but it rides on and is inseparable from the extraction (the hierarchy of states, the subordination of weak-state decision-making authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are clear: capacity-evaluation authorities (institutional power, arbitrage exit, directly control the metrics and budgets), interventionist powers (institutional power, arbitrage exit, legitimacy and influence), international security organizations (institutional power, arbitrage exit, function and budgets depend on capacity-deficit management), and high-capacity states (institutional power, exempt from evaluation). Their directionality is near the beneficiary end (d ≈ 0.1–0.25): they collect authority, budget, and legitimacy through this structure. Victims are also clear: weak states (powerless or moderate power, trapped or identity-locked exit, subordinated to evaluation and correction), postcolonial states (moderate power but identity-locked by colonial history, constrained exit), marginalized populations in low-capacity jurisdictions (powerless, immediate horizons, trapped, excluded from the agenda-setting). Their directionality is near the target end (d ≈ 0.75–0.95): they bear the cost of continuous subordination. The Global South coalitions sit between (organized power, constrained exit, real resistance but operating within the structure): d ≈ 0.55–0.65. High-capacity states occupy the beneficiary end but not the most extracted-to end: d ≈ 0.05–0.15 (they benefit from the hierarchy but are not its primary revenue stream).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question asks whether the founding problem still justifies the arrangement. The founding problem was genuine: post-WWII and post-Cold War state collapses created security vacuums, humanitarian crises, and regional spillovers. By 2025, the situation is mixed. Some capacity deficits remain live (fragile states with genuine institutional weakness still exist), but the framework has become decoupled from its founding problem in at least three ways: (1) the metrics have become more elaborate and the corrective apparatus more sophisticated, even as the foundational capacity problems have not improved proportionally—theater_ratio rose from 0.12 to 0.58, suggesting more performance than functional correction; (2) the framework is increasingly used to manage great-power competition and geopolitical influence rather than to address genuine capacity crises (the conditional aid system, for instance, is now as much about ensuring recipient states' alignment with donor preferences as about building capacity); (3) alternative state models that work for their populations are classified as capacity deficits because they diverge from Western institutional forms, suggesting the founding problem (state failure as measured by inability to provide security, rule of law, basic services) has shifted to the founding problem (deviation from Western institutional templates). This is a classic mandatrophy candidate: the arrangement was justified by solving a genuine problem; the problem persists in modified form but the arrangement now serves a different function (hierarchy maintenance rather than crisis remediation). The classification prevents false elevation: the constraint cannot be misread as pure coordination (rope) because the extraction and suppression are measured and real; the mandatrophy analysis flags that the founding problem's status is contested, which feeds the R5 mismatch detection consumer (founding_problem_status=contested + disappearance_verdict=world_rearranges produces a zombie-constraint flag).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metric_objectivity,
    'Are capacity metrics (fiscal strength, institutional centralization, monopoly of violence, rule of law indices) objective measures of state function, or are they cultural constructs that privilege Western institutional forms?',
    'Empirical: document whether non-Western states that perform their substantive functions well for their populations (security, service delivery, legitimacy) score low on standard capacity measures, and vice versa. Conceptual: examine whether alternative state models (communal authority, Islamic legal frameworks, traditional institutions) are systematically classified as capacity deficits despite functioning effectively within their own contexts.',
    'If metrics are objective, the graded sovereignty reading is empirically justified—low-capacity states really are at risk of failure. If metrics are cultural constructs, the constraint is a false-summit mountain (a natural-law appeal that actually encodes hierarchical power). The classification would shift from tangled_rope to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_metric_objectivity, empirical, 'Whether state capacity is an objective property or a Western-encoded hierarchy.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the graded sovereignty reading logically foreclose the absolute non-intervention reading within a single framework, or do they coexist as competing positions held by different parties?',
    'Logical analysis: if graded sovereignty treats sovereignty as scalar, does it make categorical sovereignty incoherent, or can a state simultaneously hold that ''sovereignty is categorical'' while accepting that ''intervention becomes justified in low-capacity cases''? Empirical: which reading do actual states, coalitions, and international bodies hold, and do they shift between readings or commit to one?',
    'If foreclosure is real, the readings are mutually exclusive—a state accepting graded sovereignty has renounced absolute non-intervention at the theoretical level. If coexistence is real, then states can shift between reading positions based on context. This affects how the reading_relations are classified in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether graded sovereignty and absolute non-intervention are logically incompatible or compatible coexisting positions.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the high suppression score (0.72) structural—external conditionality, institutional monitoring, aid sanctions—or is it partially internalized—weak-state elites accepting the capacity framework as legitimate development necessity and internalizing subordination as self-improvement?',
    'Post-exit measurement: document the trajectory of states that exit capacity-evaluation regimes or stop engaging with evaluators. If suppression persists (states continue to adopt Western institutional forms, pursue aid-compatible policies, organize internally around capacity metrics), the suppression is internalized. If suppression drops rapidly, it is primarily structural.',
    'If internalized, the effective suppression is higher than the structural measure suggests—the constraint has colonized the cognitive frame of weak-state leadership. This would strengthen the mandatrophy analysis: the constraint persists not because the problem is live but because the targets have internalized the evaluative framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression operates structurally or through internalization in weak-state governance.').

omega_variable(
    theater_ratio_rise_mechanism,
    'The theater ratio rose from 0.12 (1945) to 0.58 (2025). Is this rise driven by (a) increased sophistication of capacity metrics themselves becoming more performative than functional, (b) increased institutional overhead that no longer prevents state failures, or (c) the mechanism shift from genuine crisis remediation to great-power competition management?',
    'Documentary: track the volume and composition of capacity-evaluation activities (World Bank governance reports, IMF structural adjustment programs, UN capacity-building missions) against actual improvement in measured state capacity outcomes and security/humanitarian indicators. If activity increases while outcomes stagnate or worsen, theater is rising.',
    'High theater ratio with continued extraction suggests the constraint is approaching piton status—it persists due to institutional inertia and beneficiary entrenchment rather than functional coordination. This would shift the classification from tangled_rope (coordination + extraction + enforcement) toward piton (mostly performance, maintained by institutional refusal to acknowledge the function has atrophied).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_rise_mechanism, empirical, 'Whether rising theater reflects metric sophistication, institutional bloat, or functional displacement.').

omega_variable(
    alternative_interventionist_legitimacies,
    'If the graded sovereignty reading is contested and weak states resist it, are there alternative readings of intervention legitimacy that could ground the same interventions on different normative bases?',
    'Theoretical: map the conditional_responsibility reading (sovereignty conditional on atrocity prevention) and examine whether it would justify similar interventions but for different reasons. Empirical: identify actual interventions and document whether they match capacity-deficit grounds (graded sovereignty) or atrocity-prevention grounds (conditional responsibility).',
    'If alternative readings exist and would justify overlapping interventions, the choice among readings becomes a choice of rhetorical frame, not structural outcome. This affects how much of the extraction is intrinsic to intervention itself (necessary cost of cross-border action) versus specific to the graded sovereignty reading (inherent to the capacity-metric framework).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_interventionist_legitimacies, conceptual, 'Whether graded sovereignty is the only reading that justifies intervention, or whether alternatives exist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(west_tr_t1970, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1970, 0.22).
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__graded_sovereignty, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2005, 0.52).
narrative_ontology:measurement(west_tr_t2015, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2015, 0.56).
narrative_ontology:measurement(west_tr_t2025, westphalia_sovereignty__graded_sovereignty, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(west_be_t1970, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(west_be_t2015, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(west_be_t2025, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1945, 0.28).
narrative_ontology:measurement(west_su_t1970, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1970, 0.41).
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(west_su_t2015, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(west_su_t2025, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__graded_sovereignty, 0.18).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, international_aid_conditionality).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, structural_adjustment_programs).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, un_trusteeship_mandate).

% DUAL FORMULATION NOTE:
% Westphalia sovereignty kernel family: three readings with structurally distinct beneficiary/victim sets and extraction mechanisms. Absolute non-intervention (categorical sovereignty, no legitimate external override) is a mountain-candidate from Western institutional perspective but snare from postcolonial perspective (enforces historical power imbalances through non-intervention norm). Conditional responsibility (sovereignty forfeited by atrocity commission) has lower extraction than graded sovereignty but higher legitimacy-contestation because it rides on real atrocity prevention. Graded sovereignty (this constraint) is the most extractive because it enables intervention on the basis of capacity metrics that are themselves contested and controllable by external evaluators. All three are linked by network.affects_constraints; each story must declare its own ε and beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
