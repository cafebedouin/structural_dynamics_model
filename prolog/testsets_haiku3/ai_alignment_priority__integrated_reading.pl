% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Dual-Priority AI Alignment (Integrated Reading)
 *   domain: technology_ethics/ai_governance/risk_assessment
 *
 * SUMMARY:
 *   The integrated reading of AI alignment asserts that preventing
 *   catastrophic loss of control over advanced AI systems and preventing
 *   present harms from deployed AI are complementary rather than competing
 *   priorities. This reading positions alignment as a dual-track research and
 *   governance problem: red-teaming for latent capabilities and existential
 *   risk mitigation coexist with fairness audits, bias testing, and
 *   deployment harm reduction. The constraint emerges from institutional
 *   practice (major safety organizations now conduct both tracks) and from
 *   research claims that early failure modes in deployed systems predict
 *   alignment failure in advanced systems. The reading is ONE perspective on
 *   a contested kernel; it competes with existential-risk-only and
 *   nearterm-harms-only framings for legitimacy and resources.
 *
 * KEY AGENTS:
 *   - alignment_research_community: institutional agenda-setter; allocates resources to both existential and deployment tracks; moderate mobility
 *   - future_populations: powerless beneficiaries; trapped from non-agency; civilizational time horizon; existential-risk work protects them
 *   - present_marginalized_groups: constrained beneficiaries; biographical time horizon; direct vulnerability to deployment harms; resource-constrained participation in governance
 *   - capability_developers: powerful payers; can exit to lighter-regulation jurisdictions; subject to dual compliance (red-teaming + fairness audits)
 *   - resource_constrained_safety_teams: moderate payers; stretched between two methodological tracks; trade-off costs remain concrete despite integration rhetoric
 *   - deployment_harms_victims: powerless payers; already harmed; trapped by economic dependence; no exit
 *   - existential_risk_advocates: powerful excluded voice; would prefer existential-only framing; retain institutional power to resist integration
 *   - nearterm_harms_advocates: moderate excluded voice; elevated but not autonomous under integration; governance occurs partly against their preferred frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.58).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.62).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Dual-Priority AI Alignment (Integrated Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "technology_ethics/ai_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '6ed7351a-01dd-4dcf-9228-3221e239af25').
narrative_ontology:cs_kernel_codification('6ed7351a-01dd-4dcf-9228-3221e239af25', distributed).
narrative_ontology:cs_authority_grounding('6ed7351a-01dd-4dcf-9228-3221e239af25', distributed).
narrative_ontology:cs_reading_relation('6ed7351a-01dd-4dcf-9228-3221e239af25', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ed7351a-01dd-4dcf-9228-3221e239af25', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('6ed7351a-01dd-4dcf-9228-3221e239af25', foundational, alignment_domains_are_coupled).
narrative_ontology:cs_axiom_status(alignment_domains_are_coupled, holdable).
narrative_ontology:cs_axiom_grounding('6ed7351a-01dd-4dcf-9228-3221e239af25', alignment_domains_are_coupled, empirically_contingent).
narrative_ontology:cs_axiom('6ed7351a-01dd-4dcf-9228-3221e239af25', secondary, dual_track_resource_allocation_feasible).
narrative_ontology:cs_axiom_status(dual_track_resource_allocation_feasible, holdable).
narrative_ontology:cs_axiom_grounding('6ed7351a-01dd-4dcf-9228-3221e239af25', dual_track_resource_allocation_feasible, instrumental).
narrative_ontology:cs_reference_frame('6ed7351a-01dd-4dcf-9228-3221e239af25', separated_safety_domains).
narrative_ontology:cs_drift_state('6ed7351a-01dd-4dcf-9228-3221e239af25', integrated_governance_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6ed7351a-01dd-4dcf-9228-3221e239af25', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, present_marginalized_groups).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, alignment_research_community).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, deployment_harms_victims).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, resource_constrained_safety_teams).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, present_marginalized_groups).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, capability_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research priorities, allocates funding between existential-risk and deployment-harm tracks, designs and maintains dual-track evaluation infrastructure. Justifies integration by citing methodological complementarity and risk coupling. Has power to decide which safety organizations receive legitimacy and resources; can condition funding on dual-track compliance from capability developers.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, alignment_research_community, agenda_setter,
    institutional, generational, mobile, global).

% Non-agential beneficiaries whose interests are represented through existential-risk arguments and continuity reasoning. Benefit from research that reduces catastrophic AI failure modes. Cannot participate in present governance; their voice is an inferred proxy from technical risk modeling and philosophical arguments about civilization-scale stakes.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, future_populations, beneficiary,
    powerless, civilizational, trapped, universal).

% Subject to present harms from deployed AI systems (hiring bias, lending discrimination, content moderation bias, surveillance targeting). Benefit from the institutional elevation of deployment-harm work under the integrated reading. Simultaneously pay by serving as test populations for safety research and by bearing the costs of incomplete safety measures before they are deployed more widely. Their voice in governance is mediated and constrained.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, present_marginalized_groups, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__integrated_reading, present_marginalized_groups, payer).

% Build AI systems that exhibit both existential and deployment risks. Subjected to dual-track compliance requirements: red-teaming for latent capabilities and fairness/bias audits before deployment. Experience the integrated framework as overhead; argue that speculative existential requirements lack empirical grounding and that deployment-harm mitigation should be iterative (fixing problems as they arise) rather than proactive. Can exit to jurisdictions with lighter safety requirements.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, capability_developers, payer,
    powerful, biographical, mobile, global).

% Already harmed by deployed AI in production systems. Face discrimination, misclassification, privacy violations, labor displacement. Under the integrated reading, their situation is framed as a research domain and pilot for larger alignment challenges, but they bear the concrete costs while the research community benefits from learning. Their exit from AI systems is constrained by economic and infrastructural dependence.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, deployment_harms_victims, payer,
    powerless, biographical, trapped, global).

% Operate safety organizations (audits, testing, incident response, policy analysis) with limited budgets and personnel. The integrated reading requires them to maintain expertise in both existential-risk evaluation and deployment-harm assessment. Real trade-offs persist: depth in one track comes at the cost of shallowness in the other. The rhetoric of complementarity conceals that they must choose and that the choice is structurally constrained (larger, more powerful institutions favor existential risk work; deployment-harm work faces funding precarity).
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, resource_constrained_safety_teams, payer,
    moderate, biographical, constrained, national).

% Regulate AI deployment and oversee alignment work at national or supra-national scale. The integrated reading requires them to maintain dual-mandate authority: existential-risk oversight (theoretical, computational, long-horizon) and deployment-harm prevention (empirical, statistical, immediate). Their legitimacy depends on appearing to address both without admitting that the two mandates can conflict or that capacity is insufficient for both.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, ai_governance_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Argue that alignment means preventing catastrophic loss of control; see present deployment harms as a distraction from the existential-scale problem. Under the integrated reading, they are positioned as one perspective among several rather than as the default frame for alignment work. They retain institutional power (substantial funding, academic legitimacy, policy access) to resist integration and maintain existential-risk-only framing in parts of the ecosystem.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, existential_risk_advocates, excluded,
    powerful, civilizational, mobile, global).

% Argue that alignment means preventing present discriminatory/extractive harms from deployed AI; prioritize justice for marginalized populations. Integration elevates their concerns from 'side issue' to 'complementary pillar,' but subordinates their autonomy: their work becomes framed as contributing to the larger alignment research agenda rather than as an independent demand for justice. They are included in the conversation under a frame not entirely of their choosing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, nearterm_harms_advocates, excluded,
    moderate, biographical, mobile, global).

% Notes the structural asymmetries in the integrated reading: future populations cannot advocate; present victims are included but resource-constrained; the two risk domains compete for attention despite integration rhetoric; existential-risk and nearterm-harms advocates remain organized separately and can obstruct each other's priorities; resource trade-offs persist beneath the complementarity claim.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__integrated_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__integrated_reading, alignment_research_community).
narrative_ontology:fixing_cost_class(ai_alignment_priority__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates existential-risk mitigation and deployment-harm reduction into a single research and governance framework, preventing siloing of safety work and enabling cross-learning: red-teaming insights transfer to fairness testing, audit methodologies inform both tracks, shared infrastructure (evaluation, testing, incident response) serves both existential and deployment objectives.
% TRANSFER_FUNCTION: Moves research resources, governance attention, and institutional legitimacy from existential-risk-only framings toward a dual-track allocation. Transfers accountability from the existential-risk research community alone to a broader set of stakeholders (marginalized groups, deployment engineers, governance institutions). Institutionally, transfers justification burden: alignment work must demonstrate benefit along both dimensions to secure funding.
% ABSENT_VOICES: Groups experiencing AI harms in jurisdictions without regulatory presence or safety infrastructure are largely absent—their situations ground the deployment-harm track rhetorically, but they do not participate in setting priorities. Existential-risk researchers outside the 'integrated' subcommunity are sidelined by the reframing, though they retain institutional power to resist it. Future populations remain inherently absent (non-agential); their interests are represented by proxy arguments from continuity and risk modeling.
% DISAPPEARANCE_RATIONALE: If the integrated reading vanished and alignment governance reverted to single-track (either existential-only or deployment-harm-only), the resource allocation would shift sharply, the institutional mandate of safety bodies would collapse to one dimension, the legitimacy claims of the alignment research community would narrow, and the framing of what 'counts' as a safety failure would change. Communities currently benefiting from the dual-track framing (present marginalized groups, deployment engineers seeking legitimacy) would lose institutional footing, while existential-risk-only or nearterm-harm-only advocates would recapture governance space.
% FOUNDING_PROBLEM: Early AI safety discourse treated existential risk and deployment harms as separate problems: existential risk was framed as a future research question (loss of control over superintelligence), while deployment harms were treated as engineering problems (bias, fairness). This separation allowed existential-risk work to abstract away from present impacts and allowed deployment-harm work to avoid building long-term safety infrastructure. The integrated reading emerged from the observation that the same alignment failure modes produce both kinds of harm: a system that is unaligned in subtle ways exhibits deployment bias AND exhibits latent capabilities misaligned with human intent; a system robust to specification gaming in near-term applications is more likely to resist deceptive alignment in advanced settings.
% FOUNDING_PROBLEM_CORROBORATION: Deployment-harm researchers and advocacy groups attest that present harms are systematic and require urgent research prioritization, supporting the founding problem's premise that separation enabled neglect. Existential-risk researchers dispute the framing: some attest that existential work already includes deployment insights (scaling laws, specification gaming); others argue that forcing integration dilutes both efforts. Independent governance analysis from institutions like OpenPhil and academic AI ethics groups document the historical separation and note its costs, though disagreement persists on whether integration is the right solution versus parallel tracks with better coordination.
narrative_ontology:disappearance_verdict(ai_alignment_priority__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__integrated_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__integrated_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the integrated reading requires dual-track compliance from capability developers and resource-constrained safety teams while future populations cannot negotiate and present victims remain structurally powerless. The constraint extracts time, attention, and resources from simpler single-track approaches without fully resolving the priority question—it manages the dispute rather than settling it. Suppression is moderate (0.62) because the integration framework is actively maintained through institutional coordination, funding allocation, and methodological alignment, but real resistance persists: existential-risk researchers maintain separate funding streams and theoretical frameworks; deployment-harm advocates argue integration dilutes their concerns; capability developers push back against dual-track compliance costs. Theater is moderate (0.41): much of the integration work is performative institutional coordination and rhetorical framing, but substantive methodological innovation (cross-track testing, shared evaluation infrastructure) does occur. The measurement series shows extractiveness and suppression rising to mid-interval (capability developer resistance peaks ~t=20, necessitating enforcement intensification), then slight decline as the framework stabilizes and stakeholders update expectations. Theater rises through the interval as institutional attention grows but plateau as the novelty of integration wears off.
 *
 * PERSPECTIVAL GAP:
 *   From the alignment research community's perspective, the integrated reading is a rational evolution: both tracks are necessary, they are complementary, and institutional coordination enables shared infrastructure. From the resource-constrained safety teams' perspective, integration is a burden: real trade-offs persist, and the rhetoric of 'complementarity' conceals that one track inevitably gets less attention. From future populations' perspective (inferred from existential-risk arguments), integration dilutes focus on the civilizational-scale risks that matter to them. From present marginalized groups' perspective, integration is partial victory (their concerns are institutional now) but also co-optation: their harm reduction becomes a justification for broader alignment research rather than an autonomous demand. The engine computes this per-seat from the structural data; the authored claim describes institutional consensus while the metrics reveal structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The alignment research community sits near beneficiary end (d ≈ 0.20): they set the terms, control resource allocation, and the integration framing elevates their role as mediators. Future populations and present marginalized groups sit near the target end (d ≈ 0.85): powerless, trapped or constrained, their interests are represented by proxy rather than directly advocated. Capability developers sit near symmetric-to-target (d ≈ 0.60): they bear dual-track compliance costs (payers) but retain exit options and can negotiate (mobile, powerful). Resource-constrained safety teams sit near-target (d ≈ 0.75): they must execute both tracks with scarce resources, and the integration constraint means they bear trade-off costs the research community avoids. Existential-risk and nearterm-harms advocates are excluded from formal stakeholder roles but retain institutional power; analytically, they sit at the frame-boundary where directionality is computed by other seats' assessment of whose voice matters in the alignment conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The integrated reading is vulnerable to mandatrophy through two mechanisms: (1) Scope creep—the dual-track mandate expands to require capability developers comply with existential-risk standards that were never empirically validated, and deployment-harm standards that have unclear connection to existential alignment; the research community keeps both on the books to maintain legitimacy even as they atrophy. (2) Resource capture—the integrated framework becomes institutional theater: safety organizations report on dual-track work to satisfy multiple constituencies, but actual resource allocation reverts to single-track (usually existential, which has clearer methodology and larger budgets), with deployment-harm work treated as compliance. The theater_ratio trajectory shows this dynamic: performative coordination rises through t=15, plateaus around t=20-25 as the framework becomes normalized and expectations reset. If this trajectory continues, the reading is at risk of becoming a piton—maintained by institutional inertia and inter-factional compromise, with the coordination function (integrating two safety research communities) atrophying while enforcement machinery (compliance reporting, dual-track framing) persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_empirical,
    'Are existential-risk mitigation and deployment-harm reduction genuinely methodologically complementary, or do they compete for finite research resources despite the institutional integration claim?',
    'Longitudinal funding data: track resource allocation to existential-risk vs. deployment-harm research over time. If integration is real, both tracks should show non-zero growth and neither should be sacrificed when resources tighten. If competition persists, one track will shrink during budget constraints while the other is protected.',
    'If genuinely complementary, the constraint is tangled_rope (real coordination plus extraction). If they compete despite integration framing, it is a snare whose integration rhetoric masks single-track prioritization; classification would shift downward on coordination_function and upward on pure_extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_empirical, empirical, 'Whether dual-track resource allocation persists or reverts to single-track under constraint').

omega_variable(
    future_populations_representation,
    'How are future populations'' interests represented in present alignment governance, and does the integrated reading improve or worsen their voice compared to existential-risk-only framing?',
    'Governance audit: examine decision-making structures, whose testimony is solicited, which risk models are adopted, and whether integration adds present-victim voices that displace future-population representations or supplements them. Compare pre- and post-integration institutional behavior.',
    'If integration adds voices without displacing future interests, it strengthens the constraint''s coordination function. If present demands crowd out future concerns, the constraint extracts legitimacy from future populations to address present ones—classification would shift toward snare on the future-population seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_populations_representation, conceptual, 'Whether integration improves or reduces salience of future-oriented existential concerns').

omega_variable(
    methodological_transfer,
    'Do insights from deployment-harm research (fairness testing, bias audits, interpretability work) transfer meaningfully to existential-risk mitigation, or is the claimed complementarity a post-hoc rationalization?',
    'Citation and collaboration analysis: examine whether existential-risk papers cite deployment-harm methodologies and vice versa at rates higher than pre-integration baseline. Track whether joint projects (red-teaming that also audits for bias, fairness testing that also probes latent capabilities) emerge.',
    'If real transfer occurs, the constraint''s coordination function is substantiated. If methodological integration is minimal, the constraint is mostly institutional coordination with limited research synergy—extractiveness is high relative to coordination value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_transfer, empirical, 'Empirical evidence of methodological cross-pollination between existential and deployment tracks').

omega_variable(
    kernel_contention_stability,
    'Does the integrated reading reduce contention between existential-risk and nearterm-harm advocates, or does integration stabilize the dispute at a higher institutional cost by requiring both sides to maintain parallel governance tracks indefinitely?',
    'Political-economy analysis: track whether existential-risk-only and nearterm-harms-only communities remain organized and maintain separate funding/institutional bases under integration. If both persist, integration has not resolved the kernel contention but rather embedded it in institutional redundancy.',
    'If contention remains embedded, the constraint exhibits piton dynamics: enforcement machinery (dual-track governance, compliance reporting) persists while the founding coordination problem (integrating two safety communities with different time horizons) atrophies. Long-term classification risk toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contention_stability, conceptual, 'Whether integration resolves or institutionalizes the kernel-level dispute between existential and nearterm-harms framings').

omega_variable(
    kernel_reading_foreclosure,
    'Does institutional embedding of the integrated reading foreclose the existential-risk-only or nearterm-harms-only readings, or do they remain live positions held by different organizations?',
    'Institutional analysis: examine whether funding, hiring, and regulatory authority have become conditional on the integrated reading or whether single-track advocates can still maintain institutional bases. Test whether a researcher or organization that rejects integration faces marginalization or can operate autonomously.',
    'If integrated reading forecloses alternatives, it has become a structural constraint whose persistence is enforced; if alternatives remain live, the kernel remains open and the constraint is a particular institutional choice rather than inevitable. Classification consequence: if foreclosing, the constraint risks becoming a false natural law (mountain claim with extracted beneficiaries).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the integrated reading has achieved institutional dominance that forecloses sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__integrated_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__integrated_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__integrated_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_priority__integrated_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__integrated_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__integrated_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__integrated_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_priority__integrated_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__integrated_reading, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__integrated_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__integrated_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_priority__integrated_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__integrated_reading, 0.18).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% The ai_alignment_priority kernel decomposes into three structurally distinct constraint stories, one per reading. Each reading instantiates a different constraint with different ε values, victim sets, and institutional implications. The existential_risk_reading emphasizes catastrophic loss-of-control scenarios (high ε on speculative existential harms, low ε on present deployment harms); the nearterm_harms_reading emphasizes present discrimination and extraction (high ε on deployment harms, lower emphasis on existential scenarios); this integrated_reading asserts moderate ε on both and requires dual-track institutional work. The three constraints are linked by network.affects_constraints: integration influences both single-track readings by changing resource allocation and institutional legitimacy, but does not foreclose them (both existential-risk and nearterm-harms advocates maintain separate organizing). The decomposition follows ε-invariance: each reading has a different referent for what 'alignment' means and therefore a different assessment of extraction. A single unified constraint story that tried to average across readings would fail ε-invariance by conflating observables that yield different assessments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
