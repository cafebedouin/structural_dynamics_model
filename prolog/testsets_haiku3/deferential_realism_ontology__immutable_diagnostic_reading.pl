% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology: Immutable Diagnostic Reading
 *   domain: epistemology/institutional_design
 *
 * SUMMARY:
 *   The immutable diagnostic reading treats the Deferential Realism
 *   constraint typology as an observational instrument discovering fixed
 *   categories: mountains are invariant physical constraints, snares are
 *   measurable extraction mechanisms, and classification disputes are errors
 *   correctable through better observation. Under this reading, the framework
 *   suppresses alternative interpretive and normative approaches by reframing
 *   them as epistemically illegitimate impositions onto an objectively
 *   discoverable typology. The reading benefits observational purists and
 *   metric positivists (institutional communities whose authority depends on
 *   claiming their classifications are observer-independent), and extracts
 *   from normative advocates and interpretive pluralists (whose frameworks
 *   are delegitimized as 'imposing interpretation' rather than discovering
 *   fact). This constraint story instantiates ONE reading of a contested
 *   kernel; sibling readings (rhetorical_scaffold_reading,
 *   hybrid_pragmatic_reading) represent alternative framings of the same
 *   typology.
 *
 * KEY AGENTS:
 *   - Observational purists (mathematical ontologists, empiricist philosophers, metrological standardizers) — benefit from framework treating categories as discoverable
 *   - Metric positivists (data engineers, compliance auditors, quantitative classification practitioners) — benefit from authority protection via observational standard
 *   - Normative framework advocates (critical theorists, deontological ethicists, policy advocates) — bear cost of delegitimization as imposing interpretation
 *   - Interpretive pluralists (hermeneuticists, jurisprudents, qualitative sociologists) — trapped in identity-locked exit; their practice is fused with their interpretive framework
 *   - Applied policy communities (welfare administrators, judges, environmental reviewers, clinical diagnosticians) — pressured to suppress value-laden content of classifications
 *   - Philosophical realists (the reading itself as agenda-setter) — establish evidentiary standard and definition of legitimate resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.68).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.76).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology: Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '5c2f5286-e434-4b86-933a-1ca5673df1cb').
narrative_ontology:cs_kernel_codification('5c2f5286-e434-4b86-933a-1ca5673df1cb', fixed_text).
narrative_ontology:cs_authority_grounding('5c2f5286-e434-4b86-933a-1ca5673df1cb', expertise).
narrative_ontology:cs_interpretation_layer_present('5c2f5286-e434-4b86-933a-1ca5673df1cb').
narrative_ontology:cs_reading_relation('5c2f5286-e434-4b86-933a-1ca5673df1cb', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_reading_relation('5c2f5286-e434-4b86-933a-1ca5673df1cb', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('5c2f5286-e434-4b86-933a-1ca5673df1cb', foundational, classification_categories_discovered_not_constructed).
narrative_ontology:cs_axiom_status(classification_categories_discovered_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('5c2f5286-e434-4b86-933a-1ca5673df1cb', classification_categories_discovered_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('5c2f5286-e434-4b86-933a-1ca5673df1cb', foundational, metrics_measure_observer_independent_properties).
narrative_ontology:cs_axiom_status(metrics_measure_observer_independent_properties, holdable).
narrative_ontology:cs_axiom_grounding('5c2f5286-e434-4b86-933a-1ca5673df1cb', metrics_measure_observer_independent_properties, empirically_contingent).
narrative_ontology:cs_reference_frame('5c2f5286-e434-4b86-933a-1ca5673df1cb', observer_independent_constraint_ontology).
narrative_ontology:cs_drift_state('5c2f5286-e434-4b86-933a-1ca5673df1cb', contemporary_post_construction_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5c2f5286-e434-4b86-933a-1ca5673df1cb', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, observational_purists).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, metric_positivists).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, normative_framework_advocates).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, interpretive_pluralists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, applied_policy_communities).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, applied_policy_communities).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, correspondence_theory_of_truth).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, observer_independence).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, measurement_discovers_rather_than_constructs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutional communities (mathematical ontologists, empiricist philosophers, metrological standardizers) that benefit from a framework treating classification categories as observer-independent discoverable facts. Their authority derives from the framework's epistemic rigor; the reading vindicates their claim that proper observation resolves disputes.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, observational_purists, beneficiary,
    institutional, generational, arbitrage, global).

% Practitioners of quantitative classification systems (metricians, data engineers, compliance auditors, standardized-testing designers) whose institutional authority depends on the claim that metrics measure objective properties, not construct them. The reading protects their authority structure.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, metric_positivists, beneficiary,
    institutional, generational, arbitrage, global).

% Institutional and intellectual movements (critical theorists, deontological ethicists, value-pluralist philosophers, policy advocates) that treat classification categories as dependent on normative commitments and value judgments. Under the immutable diagnostic reading, their position is reframed as imposing 'interpretation' onto observation, which the framework treats as epistemic corruption. They bear the cost of having their frameworks delegitimized within communities that adopt the reading.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, normative_framework_advocates, payer,
    powerful, biographical, constrained, global).

% Scholars and practitioners working in domains where classification depends on incommensurable interpretive traditions (hermeneutics, jurisprudence, qualitative sociology, theology). The immutable diagnostic reading treats their frameworks as failing the observational standard; their exit would require abandoning the interpretive practice itself, which is fused with their professional and intellectual identity.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, interpretive_pluralists, payer,
    moderate, biographical, identity_locked, global).

% Communities making concrete classification decisions in contested domains (welfare administration, zoning law, environmental review, clinical diagnosis, security classification). They depend on the constraint typology for legitimacy. Under the immutable diagnostic reading, they face pressure to treat their classification choices as empirically determined rather than value-laden; when measurement underdetermines classification, this pressure suppresses acknowledgment of the normative content of the decision.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, applied_policy_communities, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, applied_policy_communities, beneficiary).

% The philosophical reading itself: a commitment to correspondence theory and observer-independence that frames the constraint typology as an observational instrument discovering fixed categories. This reading sets the agenda by establishing the evidentiary standard (metrics and observables) and defining what counts as resolution of classification disputes.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, philosophical_realists, agenda_setter,
    institutional, generational, arbitrage, global).

% Actors who operate across the boundary between physical/mathematical domains (where observational determination is strongest) and normative/applied domains (where it is weaker), and who would argue that classification depends on domain. They are structurally excluded from adjudicating the reading because the reading's own logic denies that domain-dependence is legitimate — it treats such arguments as imposing interpretation rather than discovering fact.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, domain_mixing_participants, excluded,
    moderate, biographical, constrained, global).

% Meta-theoretical seat observing how the immutable diagnostic reading operates as an enforcement mechanism for particular epistemic authorities while claiming to be merely descriptive. Can perceive the structure without being positioned inside the constraint.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, observational_purists).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified scheme for distinguishing genuine invariants (mountains) from constructed arrangements (ropes, snares, scaffolds) so that different institutional communities can reason about constraints using a shared taxonomy and appeal to measurement to resolve disagreements.
% TRANSFER_FUNCTION: Moves epistemic authority from normative and interpretive frameworks toward metric positivism and observational purists: the reading systematically privileges institutional communities that can translate their claims into metrics and suppresses communities whose classifications depend on incommensurable normative traditions or value pluralism.
% ABSENT_VOICES: Scholars and practitioners who argue that classification categories depend on incommensurable interpretive traditions (hermeneutics, jurisprudence, deontological ethics, pluralist value systems) are structurally excluded because the reading's own logic denies the legitimacy of interpretation as a classification method — such voices would argue for domain-dependence and value-pluralism but are silenced by the framework itself.
% DISAPPEARANCE_RATIONALE: If the immutable diagnostic reading and its suppressive enforcement vanished, normative and interpretive frameworks would re-enter policy domains (criminal law, welfare allocation, zoning, clinical medicine) as legitimate classification methods; the philosophical authority of metric positivism would decline; institutional communities would openly acknowledge the value-laden content of their classification choices; the Deferential Realism typology itself would be reframed as offering a vocabulary for policy critique rather than a discovery of observer-independent facts.
% FOUNDING_PROBLEM: Institutional and policy disputes are often framed as merely disagreeing about 'what' the constraint is—whether a system is a coordination mechanism or an extraction trap—rather than disagreeing about normative commitments and values. A framework that treats classification categories as observationally determined, rather than value-dependent, can appeal to measurement to resolve these disputes and avoid descending into interminable value debates.
% FOUNDING_PROBLEM_CORROBORATION: Observational purists and metric positivists attest the founding problem is live and that their framework solves it by providing a shared epistemic standard. Normative advocates and interpretive pluralists attest the founding problem is misdescribed: they argue the real problem is TREATING value-dependent classifications as observationally determined, which suppresses honest acknowledgment of the normative content of policy choices. Independent philosophical analysis (phenomenology, critical epistemology, philosophy of science) corroborates that classification in normatively contested domains is underdetermined by observables alone.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the reading systematically privileges metric-translatable claims over incommensurable interpretive frameworks, transferring epistemic authority without delivering better classification in contested domains. Suppression is high (0.76) because the reading's own logic denies legitimacy to interpretive pluralism—normative voices are excluded not by external force but by the framework's epistemic apparatus itself. Theater is moderate (0.42): the reading genuinely offers a useful vocabulary for policy critique (that is the real coordination function), but the measurement series shows theater rising from 0.22 to 0.42 over the interval—an increasing share of enforcement activity is defensive (defending the observational standard itself) rather than resolving concrete classification disputes. Accessibility collapse is high (0.72): once committed to the immutable diagnostic reading, exit from it requires abandoning the epistemic standard itself, which is identity-fused for practitioners. Resistance is moderate (0.58): normative and interpretive communities mount steady pushback through parallel frameworks, but the reading's institutional embedding gives it structural advantage. All metrics are authored at every time point on a single shared grid to prevent OQ-105-style misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of observational purists and metric positivists, the reading is genuine coordination: a shared epistemic standard that enables collective reasoning about constraints and resolves disputes through appeal to measurement. From the seat of normative advocates and interpretive pluralists, the same reading is enforced extraction: suppression of alternative frameworks disguised as epistemic rigor. From the seat of applied policy communities, the reading creates a dual burden: they need the shared vocabulary for institutional legitimacy, but they must suppress their knowledge that classifications in their domains are value-dependent. The engine should compute divergent type classifications from these seats, revealing the perspectival structure through the per-seat computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Observational purists and metric positivists sit at low d (full beneficiary): they collect epistemic authority and organizational advantage from the framework without bearing its costs. Normative advocates and interpretive pluralists sit at high d (full target): they bear the cost of delegitimization and lose authority in mixed institutional spaces where both frameworks have voice. Applied policy communities sit near d=0.5 (symmetric): they benefit from a shared classification vocabulary, but they pay by having to suppress and defer the value-laden content of their choices. The philosophical reading (the reading itself as agenda_setter) sits at d=0.0: it articulates the framework without being itself subject to extraction—it is the authority structure, not a party within it. Interpretive pluralists carry identity_locked exit because their framework is not merely a tool they use but part of their intellectual and professional self-constitution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional disputes treated as disagreements about 'what' the constraint is rather than value commitments) was live when the Deferential Realism framework was introduced as a neutral vocabulary for policy analysis. The immutable diagnostic reading treats this problem as still live and claims its observational standard solves it. However, normative and interpretive pluralists attest the founding problem is substantially displaced: the real problem is now that classification categories in contested domains are BEING treated as observationally determined when they are actually value-dependent. The measurement series shows extractiveness rising from 0.48 to 0.68 while theater rises from 0.22 to 0.42—a pattern consistent with mandatrophy: the founding function (providing a shared vocabulary) remains partly real, but an increasing share of enforcement activity (suppressing interpretive pluralism, delegitimizing normative frameworks) is purely extractive and defensive. This is not a snare (snares do not coordinate at all), but a tangled rope with accumulating extractive overhead masked by the theater of rigorous observation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observables_vs_constructed_measurement,
    'Are the metrics used to distinguish mountains from snares (extractiveness, suppression, theater_ratio) themselves observables that discover pre-existing categories, or are they constructed measurement schemes that impose theoretical categories onto reality?',
    'Philosophical analysis of metric construction: do the metrics correspond to detector-independent properties of constraints, or do they depend on the theoretical frame that selected them? Test via comparison: does switching to alternative metrics (e.g., power concentration, exit-option symmetry, coherence of beneficiary claims) produce fundamentally different classifications?',
    'If metrics are observables discovering pre-existing categories, the immutable diagnostic reading is vindicated and its suppression of interpretive pluralism is justified. If metrics are theoretically constructed, the reading is self-undermining: it claims to avoid imposing interpretation while actually imposing a particular metric-construction frame onto the classification problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observables_vs_constructed_measurement, conceptual, 'Whether metrics discover or construct the categories they measure.').

omega_variable(
    domain_variance_in_determinacy,
    'Does observational determinacy of classification vary across domains? In mathematical and physical domains, do fixed categories emerge from measurement more robustly than in normative, legal, or value-laden domains?',
    'Empirical study: compare classification stability (inter-rater reliability, temporal consistency, invariance under alternative measurement schemes) across domains. Do pure-mathematics and physics constraints show higher stability than applied-policy and normative constraints?',
    'If domain variance is real, the immutable diagnostic reading is false: classification is not universally observationally determined, but domain-dependent. The reading would then be exposed as suppressing legitimate interpretive methods in domains where they are appropriate, not as enforcing epistemic rigor. This would reclassify the constraint from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_variance_in_determinacy, empirical, 'Whether observational determinacy of classification is domain-invariant or domain-dependent.').

omega_variable(
    interpretive_pluralism_foreclosure,
    'Does the immutable diagnostic reading logically foreclose interpretive pluralism, or do both readings remain live positions that different communities can hold simultaneously?',
    'Logical analysis: can a framework that treats classification categories as observationally determined coexist with a framework that treats them as value-dependent, or does adopting one logically exclude the other? Test via: can a single institutional actor hold both readings for different purposes without contradiction?',
    'If foreclosure is real, the relation between this reading and the sibling readings is forecloses rather than coexists_with. If both can be live, the suppression of interpretive pluralism is enforcement, not logical necessity. This affects the structural classification of the whole kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_pluralism_foreclosure, conceptual, 'Whether the immutable diagnostic reading logically forecloses interpretive pluralism or merely competes with it.').

omega_variable(
    normative_framework_advocates_identity_lock,
    'For normative framework advocates and interpretive pluralists, is exit from the immutable diagnostic reading suppressed by external institutional barriers (trap), or by fusion of their intellectual/professional identity with the interpretive framework itself (identity_locked)?',
    'Qualitative study: interview scholars and practitioners in interpretive traditions who have adopted or rejected the immutable diagnostic reading. Does rejection require abandoning their professional community, intellectual tradition, or self-conception? Is leaving possible but costly (external trap), or is leaving unthinkable because it would dissolve their identity (identity_locked)?',
    'If identity_locked, these agents sit at higher effective extraction (d closer to 1.0) than trapped agents, because the suppression is internalized and travels with them even if external barriers were removed. Theater_ratio would be lower relative to pure extraction because the mechanism is self-sustaining rather than actively defended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_framework_advocates_identity_lock, empirical, 'Whether normative framework advocates'' exit is suppressed externally or through identity fusion.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of the deferential_realism_ontology kernel incommensurable—do they operate in logically independent reference frames such that no framework can hold more than one simultaneously—or are they compatible commitments that different actors can hold for different purposes?',
    'Comparative analysis: attempt to author a single constraint story that holds all three readings simultaneously without contradiction. If the attempt fails, readings are incommensurable (each is a separate constraint, not merely perspectives on one constraint). If compatible positions exist, characterize the conditions under which each reading is appropriate.',
    'If incommensurable, the kernel is not a single reality viewed differently but three genuinely distinct constraints with different epsilon values and different beneficiary structures. The immutable diagnostic reading is then one of three possible observational instruments, not the uniquely privileged one. This would lower extractiveness (it is not suppressing alternatives in the name of rigor, but competing as one among many instruments).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings are incommensurable or compatible commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(defe_tr_t0, observed).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(defe_tr_t5, observed).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(defe_tr_t10, observed).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(defe_tr_t15, observed).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(defe_tr_t20, observed).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(defe_tr_t25, observed).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(defe_tr_t30, observed).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(defe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(defe_be_t0, observed).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(defe_be_t5, observed).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(defe_be_t10, observed).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(defe_be_t15, observed).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(defe_be_t20, observed).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(defe_be_t25, observed).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(defe_be_t30, observed).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(defe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(defe_su_t0, observed).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(defe_su_t5, observed).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(defe_su_t10, observed).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(defe_su_t15, observed).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(defe_su_t20, observed).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement_basis(defe_su_t25, observed).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(defe_su_t30, observed).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(defe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.04).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% The immutable diagnostic reading is one of three readings of the contested kernel deferential_realism_ontology. The kernel is the Deferential Realism constraint typology itself. This reading treats the typology as an observational instrument discovering fixed categories; the hybrid_pragmatic reading treats it as having a fixed core with contested periphery; the rhetorical_scaffold reading treats it as normative vocabulary. Each reading instantiates a different constraint with structurally distinct epsilon values and beneficiary/victim positions. Epsilon for the immutable diagnostic reading (0.68) reflects high suppression of interpretive pluralism (0.76); epsilon for the hybrid_pragmatic reading would be lower (the core/periphery framing acknowledges normative content); epsilon for the rhetorical_scaffold reading would be higher on extractiveness at the institutional level (it treats the typology framework itself as extractive rhetoric). All three are linked via network.affects_constraints because the uphold or rejection of each reading influences the perceived legitimacy and the structural role of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
