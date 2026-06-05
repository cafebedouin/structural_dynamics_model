% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: AI Alignment as Integrated Priority: Catastrophic Risk + Present Harms
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'ai_alignment_priority' — the integrated reading, which holds that
 *   effective alignment requires addressing catastrophic risks (loss of
 *   control over advanced AI) and present harms (discriminatory, extractive
 *   deployment of current AI systems) as complementary priorities, not
 *   competing zero-sum alternatives. This reading differs fundamentally from
 *   two sibling readings: the existential_risk_reading, which treats
 *   catastrophic risk as the dominant alignment concern, and the
 *   nearterm_harms_reading, which prioritizes justice for marginalized
 *   populations currently harmed by deployed AI. The integrated reading's
 *   core claim is that both victim sets (present and future) are legitimate,
 *   that resource allocation should reflect both, and that dual-track
 *   methodologies (simultaneous capability safety and deployment auditing)
 *   can be made technically and institutionally coherent. The constraint
 *   exhibits Tangled Rope classification: it coordinates a legitimate
 *   dual-mandate (addressing both risks) while extracting from both victim
 *   populations through resource scarcity and prioritization deferral. The
 *   integrated reading is neither a compromise in the weak sense (split the
 *   difference) nor a false synthesis — it is a structural claim that the two
 *   priorities are not fundamentally opposed, that institutional design can
 *   handle both, and that the apparent zero-sum competition is produced by
 *   governance choices rather than natural necessity.
 *
 * KEY AGENTS:
 *   - Marginalized Populations: Primary victim (powerless/trapped/biographical) — experience present algorithmic harms (discrimination, surveillance, predatory targeting) deployed today with no exit option
 *   - Future Populations: Primary victim (powerless/trapped/civilizational) — face existential risk from advanced AI systems; cannot organize or negotiate around a diffuse future threat
 *   - Deployment Justice Advocates: Organized secondary actor (organized/constrained/biographical) — work to expose and prevent present harms; validated by integrated reading but resource-constrained relative to capability safety research
 *   - Existential Risk Researchers: Organized secondary actor (organized/constrained/generational) — work on capability control and misalignment prevention; constrained by timeline uncertainty and requirement to share resources with fairness research
 *   - AI Systems Deployment Infrastructure: Beneficiary (institutional/arbitrage/biographical) — tech companies, cloud providers, deployment platforms; benefit from dual-accountability legitimacy while avoiding strong constraints on expansion
 *   - Legacy Regulatory Frameworks: Institutional actor (institutional/arbitrage/immediate) — existing governance structures that claim to address AI harms but operate largely through theater and inertia
 *   - Integrated Methodology Practitioners: Moderate actor (moderate/constrained/generational) — researchers implementing simultaneous capability and fairness work; see the constraint as temporary coordination problem with sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.52).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.65).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "AI Alignment as Integrated Priority: Catastrophic Risk + Present Harms").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '540c9d36-3a96-4f5f-8212-c3c4b850cdeb').
narrative_ontology:cs_kernel_codification('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', distributed).
narrative_ontology:cs_authority_grounding('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', distributed).
narrative_ontology:cs_reading_relation('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', foundational, dual_victim_legitimacy).
narrative_ontology:cs_axiom_status(dual_victim_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', dual_victim_legitimacy, deontological).
narrative_ontology:cs_axiom('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', foundational, institutional_design_sufficiency).
narrative_ontology:cs_axiom_status(institutional_design_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', institutional_design_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', concurrent_mandate_framework).
narrative_ontology:cs_drift_state('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', contemporary_ai_deployment_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('540c9d36-3a96-4f5f-8212-c3c4b850cdeb', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, alignment_research_infrastructure).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, capability_safety_specialists).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, marginalized_groups_deploying_harm).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, future_populations_existential_risk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED POPULATIONS EXPERIENCING PRESENT HARMS (SNARE) — Face algorithmic discrimination, predatory targeting, and surveillance systems deployed today. Trapped by lack of alternative services and legal recourse. Cannot opt out of AI-mediated credit scoring, hiring, content moderation, or law enforcement prediction. The integrated reading's commitment to addressing present harms acknowledges this agent as a primary victim, but resource scarcity and research prioritization often subordinate deployment justice to capability safety. Maximum extraction: harm occurs now, mitigation delayed.
constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE POPULATIONS FACING EXISTENTIAL RISK (SNARE) — Face the possibility of catastrophic loss of control over advanced AI systems. Trapped: cannot negotiate or organize around a risk that may not materialize for decades. The integrated reading acknowledges this agent as a primary victim, but the temporal gap (risk may never occur vs harms occurring now) creates prioritization pressure that structurally disadvantages this agent's interests. Maximum extraction from an existential perspective: extinction leaves no alternative.
constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEPLOYMENT JUSTICE ADVOCATES AND AUDITORS (TANGLED ROPE) — Organized actors (civil rights orgs, fairness researchers, audit firms) work to expose and mitigate present harms. The integrated reading validates their work as essential to alignment, but constrained by: (1) funding concentrated in capability safety, (2) technical barriers to auditing opaque systems, (3) lack of enforcement power over tech companies. Benefit from the integrated reading's legitimation; bear costs of under-resourcing. Moderate extraction: some agency, some institutional validation, significant resource constraints.
constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: EXISTENTIAL RISK RESEARCHERS AND GOVERNANCE BODIES (TANGLED ROPE) — Organized actors (longtermist research organizations, policy institutes, regulatory bodies) work on capability safety and misalignment prevention. The integrated reading validates their work as essential but requires resource-sharing with deployment justice initiatives. Constrained by: (1) uncertainty about timeline and likelihood of existential risk, (2) political pressure to address present harms, (3) technical complexity of long-term capability control. Benefit from institutional recognition; bear costs of shared authority and deferred prioritization. Moderate extraction: legitimate concerns partially subordinated to integrated methodology.
constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: AI SYSTEMS DEPLOYMENT INFRASTRUCTURE (ROPE) — Tech companies, cloud providers, and deployment platforms experience the integrated reading as a coordination mechanism. The constraint requires dual-track accountability (capability audits + fairness audits) but enables deployment to proceed with legitimacy claims of responsibility. Net beneficiary: compliance mechanisms create liability shields and market access. Experiences constraint as Rope: genuine coordination problem (how to balance scale with safety) is solved through dual methodology. Low extraction: the constraint actually benefits large-scale deployment by distributing accountability burden.
constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY REGULATORY FRAMEWORKS (PITON) — Existing governance structures (data protection, consumer protection, algorithmic transparency rules) claim to address AI harms but are largely performative: they lack enforcement capacity, move slower than deployment, and rarely prevent extractive systems from launching. The integrated reading requires them to be supplemented by new structures but does not displace them. Theater ratio high: compliance theater, audit theater, regulatory theater. The frameworks persist through institutional inertia and legitimacy claims, not because they functionally prevent harms.
constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a universalist perspective, the tension between present and future harms might appear as an immutable constraint: scarce resources force prioritization; we cannot solve all problems simultaneously; the present always takes precedence in human psychology and governance. However, this 'natural law' framing naturalizes what is actually a political and institutional arrangement — resource scarcity, funding concentrations, and epistemic asymmetries are not laws of nature but products of governance choices. This perspective is marked as a false summit candidate: the engine will flag that treating the tension as inevitable masks the contingent institutional structures that create the apparent dilemma.
constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: INTEGRATED METHODOLOGY PRACTITIONERS (SCAFFOLD) — Researchers and practitioners who implement dual-track approaches (simultaneous work on capability safety + deployment justice) see the constraint as a temporary coordination problem with a sunset clause. As technical methods mature (interpretability for capability audit, fairness metrics for deployment audit, integrated red-teaming frameworks), the artificial tension between the two priorities decreases. Theater ratio moderate: some coordination overhead is genuine (managing dual workstreams), but institutional rigidity (siloed funding, separate conferences, competing career incentives) creates excess theater. Sunset envisioned: 10-15 years as integrated training and evaluation frameworks become standard practice.
constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_alignment_priority__integrated_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, TR),
    TR >= 0.70.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, trending upward. The constraint coordinates work on two legitimate victim sets but does so through resource allocation that defers both — present harms persist while resources are allocated to long-term capability safety, and existential risk prevention is under-resourced relative to its probabilistic importance. The upward trend (0.38 → 0.45 → 0.52 over the measurement interval) reflects that as AI systems scale and proliferate, the pressure to address both present harms and future risks intensifies, but institutional capacity to fund both remains constrained. The extraction operates through deferral: 'we will address present harms once we solve existential safety' and 'we will pursue existential safety once we build institutional capacity for fairness' — both victim sets remain waiting. Suppression (0.65): Moderate-high. Structural barriers include: (1) resource scarcity in AI governance funding (capability safety and fairness are competing for limited pools), (2) epistemic uncertainty about existential risk timeline (drives prioritization debates that suppress one agenda or the other), (3) institutional siloing (separate research communities, conferences, funding bodies maintain boundaries), (4) political asymmetry (existential risk frames appeal to longtermist values and attract libertarian/EA funding; present harms frame appeals to civil rights and attracts social justice funding, but these funding pools rarely overlap). The suppression is not decreasing — as AI scales, both communities face pressure to prove their urgency, which locks them into competition. Theater (0.58): Moderate. The integrated reading itself produces some theater: conferences on 'responsible AI' that claim to balance both concerns but allocate speaker time by existing institutional weights; governance bodies that form dual mandates but operate with sequential rather than simultaneous working groups; research agendas that claim integration but remain methodologically distinct. However, genuine coordination overhead exists — managing simultaneous red-teaming and fairness auditing is not pure theater; it is real coordination cost. The upward trend (0.52 → 0.55 → 0.58) reflects increasing performance pressure: as the stakes of both risks become visible, institutions must perform commitment to both priorities, generating theatrical elements alongside genuine work.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the existential risk and deployment justice communities is the central diagnostic feature of this constraint. From the existential risk perspective (perspective 4), the constraint appears as a reasonable attempt to maintain long-term capability safety work while accommodating legitimate concerns about present harms — a Tangled Rope of shared resources. From the deployment justice perspective (perspective 3), the same constraint appears as a mechanism that perpetually defers present-harm prevention in the name of future safety — also Tangled Rope, but with inverted power relations (existential research gets preferential timing, fairness work gets residual resources). Both see coordination and extraction; they disagree about the direction of flow and the relative legitimacy of their own victim set. The powerless perspectives (1 and 2) both see Snare: neither present nor future victims have agency in the integrated framework — both are waiting for resource-constrained institutions to eventually address their concerns. The institutional perspective (5) sees Rope — the constraint enables deployment to proceed with distributed accountability claims. The analytical observer perspective (7) risks naturalization: seeing the tension between present and future as inevitable rather than as a product of institutional design and funding concentration. The scaffold perspective (8) sees the constraint as temporary — as technical methods and institutional practice mature, the apparent tension dissolves. This perspectival map reveals that the integrated reading's success depends entirely on whether the organizational/technical/funding infrastructure actually converges the two workstreams or merely manages their competition.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint's directionality varies sharply across the two victim populations. Marginalized populations currently experiencing algorithmic harms have high d values (0.85-0.95) — they are the primary targets of extraction. Future populations facing existential risk have d values closer to 0.80-0.90 depending on time horizon and exit capacity assumptions. The organized actors (deployment justice advocates, existential risk researchers) have d values around 0.55-0.65 — they are partially beneficiaries (their work is legitimated) and partially victims (they are resource-constrained and placed in competition with each other). The deployment infrastructure (tech companies) has negative d (~0.10-0.20) — the constraint actually benefits them by distributing accountability, making them net beneficiaries. The engine's derivation chain maps these structural positions through beneficiary/victim declarations and exit options. Notably, the two victim populations have asymmetric exit options: present harm victims are structurally trapped (cannot opt out of algorithmic systems); future populations are trapped by temporal distance (cannot exit a risk they cannot yet perceive as immediate). The organized actors have constrained exit (they can leave the AI governance field but face career costs and would leave present/future victims unrepresented). These asymmetries are visible in perspectives 3 and 4, where both organized actors experience the constraint as Tangled Rope — they have some agency but are also constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The integrated reading itself is structured as a response to potential mandatrophy — the logical problem where addressing one mandate (present harms) appears to crowd out or contradict another (future safety). The integrated reading's core claim is that this apparent contradiction is a false dilemma: the two mandates are orthogonal, and institutional design can honor both. The constraint operates as Tangled Rope (extractiveness 0.52, suppression 0.65) because: (1) Genuine coordination function: it provides a framework for research communities that were previously siloed to work together on shared problems (e.g., interpretability serves both capability audit and fairness audit). (2) Asymmetric extraction: it coordinates work on two priorities while deferring resources to both victim sets. The mandatrophy is resolved by showing that the constraint is not a compromise but a structural observation — both threats are real, both victim sets are legitimate, and the apparent zero-sum competition is an institutional failure, not a logical necessity. However, the measurement trajectory (extractiveness rising from 0.38 to 0.52, suppression rising from 0.55 to 0.65) shows that the integrated approach is becoming more extractive over time, not less — as both risks become more visible and urgent, the pressure to defer both increases, and the institutional capacity to address either remains limited. This suggests that the mandatrophy is not resolved but merely reframed: the integrated reading changes the question from 'which victim set is legitimate?' to 'how long can we defer addressing both legitimate victim sets while claiming commitment to both?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_zero_sum,
    'Is the apparent resource scarcity between capability safety and deployment justice research a structural fact or a governance choice?',
    'Comparative funding analysis: capability safety funding vs. fairness research funding (2015-2030); counterfactual modeling of integrated funding allocation; historical precedent analysis from other dual-mandate regulatory domains (occupational safety AND environmental protection, financial stability AND consumer protection)',
    'If governance choice: the constraint is Tangled Rope (resource allocation is the coordination mechanism). If structural: the constraint approaches Snare (fundamental scarcity forces extraction). The integrated reading assumes this is resolvable through institutional design; if it is truly structural, the reading is aspirational rather than achievable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_zero_sum, empirical, 'Whether resource scarcity is structural or institutional').

omega_variable(
    timeline_uncertainty_existential_vs_present,
    'How does uncertainty about the timeline and probability of existential AI risk affect the rational allocation between preventing present harms and preventing catastrophic futures?',
    'Bayesian decision analysis under uncertainty; expected value calculation with varying existential risk probabilities (1% vs 10% vs 50% vs 90%); comparison with other domains managing low-probability high-impact risks (pandemic preparedness, nuclear safety, climate)',
    'If existential risk probability < 5%: allocate resources heavily toward present harms (near-term reading dominates). If > 30%: allocate significantly toward capability safety (existential reading dominates). If 5-30%: integrated approach is rationally justified. The integrated reading assumes we are in the 5-30% zone; if evidence shifts bounds, the reading''s structural justification weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timeline_uncertainty_existential_vs_present, empirical, 'Existential risk timeline and probability uncertainty').

omega_variable(
    technical_interdependence_red_teaming_fairness,
    'Do the technical methods for capability safety (red-teaming, adversarial testing, interpretability) and deployment justice (fairness audits, bias detection, distributional analysis) share sufficient methodological overlap to be genuinely integrated, or are they distinct workstreams that merely share resources?',
    'Meta-analysis of published papers using both capability and fairness evaluation; assessment of methodological overlap in safety benchmarks; design of integrated safety evaluation frameworks and measurement of their efficacy vs. sequential approaches',
    'If high overlap: integration is technically feasible and efficient (Rope or Tangled Rope with genuine coordination). If low overlap: integration is administratively coupled but methodologically distinct (Scaffold with sunset on the integration claim). If overlap emerges over time: the constraint shifts from Tangled Rope toward Rope as the dual-track framework matures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_interdependence_red_teaming_fairness, empirical, 'Technical interdependence between capability safety and fairness evaluation').

omega_variable(
    victim_set_boundary_ambiguity,
    'Which populations are legitimately victims of ''alignment failure''? Does the victim set include only (a) present users of deployed systems, (b) future populations under existential risk, (c) both, or (d) a different framing entirely?',
    'Conceptual and normative analysis: stakeholder mapping in AI governance; philosophical argument over intergenerational justice and duties to future generations; historical precedent from other domains (environmental protection, nuclear safety, genetic engineering) regarding victim set expansion',
    'If only present (a): prioritize deployment justice; integrated reading is compromise position. If only future (b): prioritize capability safety; integrated reading is compromise. If both (c): integrated reading is structurally justified. If different framing (d): the entire constraint may be mislabeled; need to decompose into separate constraints per victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_ambiguity, conceptual, 'Boundary conditions for ''victim of alignment failure''').

omega_variable(
    institutional_capture_existential_framing,
    'Does the emphasis on catastrophic existential risk serve as institutional capture mechanism, absorbing resources and legitimacy that would otherwise flow to present-harm prevention?',
    'Critical discourse analysis: examination of funding flows and institutional incentives; comparison with other high-stakes domains where catastrophic risk framing emerged (nuclear proliferation, biotech safety); behavioral analysis of research communities under uncertainty',
    'If true: the integrated reading is a corrective to capture, and resource reallocation toward present harms is justified. If false: existential risk framing is structurally accurate, and the integrated reading properly balances competing but equally valid concerns. High-confidence resolution would strengthen or weaken the integrated reading''s legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_existential_framing, conceptual, 'Whether existential risk framing masks institutional capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aaip_int_theater_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(aaip_int_theater_t3, ai_alignment_priority__integrated_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(aaip_int_theater_t6, ai_alignment_priority__integrated_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(aaip_int_extr_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(aaip_int_extr_t3, ai_alignment_priority__integrated_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(aaip_int_extr_t6, ai_alignment_priority__integrated_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(aaip_int_supp_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(aaip_int_supp_t3, ai_alignment_priority__integrated_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(aaip_int_supp_t6, ai_alignment_priority__integrated_reading, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, algorithmic_fairness_audit_deployment).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_capability_control_research).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-part kernel: 'What does alignment mean in AI governance?' The three readings (existential_risk, nearterm_harms, integrated) are structurally distinct constraints with different ε values, victim sets, and perspectival classifications. They form a kernel family linked by network.affects_constraints. The integrated reading influences its sibling readings by staking a claim that both are legitimate — this creates structural pressure on the existential-risk reading (demands it justify resource concentration) and on the nearterm-harms reading (demands it engage with long-term safety). The kernel decomposition reflects ε-invariance: each reading has a different extractiveness profile (existential: ε ~0.35 focusing on future risk; nearterm: ε ~0.58 focusing on present harms; integrated: ε ~0.52 claiming both) because they are evaluating different victim sets and different institutional arrangements. Measurements are calibrated to each reading's own internal timeline and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, organized, 0.58).
constraint_indexing:directionality_override(ai_alignment_priority__integrated_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
