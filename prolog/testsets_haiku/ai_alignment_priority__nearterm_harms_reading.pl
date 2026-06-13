% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__nearterm_harms_reading, []).

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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Alignment Priority: Near-Term Harms Reading
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   The AI alignment priority kernel contests how 'alignment' should be
 *   framed: as prevention of present harms to marginalized populations, as
 *   prevention of existential catastrophe from advanced AI, or as a
 *   complementary integration of both. This story instantiates the
 *   NEARTERM_HARMS_READING: alignment means auditing deployed systems for
 *   bias against marginalized groups (age, race, disability, gender,
 *   socioeconomic status) and preventing their deployment until mitigation is
 *   demonstrated. The constraint extracts from deployment-urgency interests
 *   (large model developers, deployment-dependent institutions) by imposing
 *   mandatory bias audits, fairness verification, and enforcement delays. It
 *   benefits marginalized populations directly (prevents discriminatory
 *   harms) and benefits algorithmic-justice advocates and bias-audit bodies
 *   institutionally (legitimizes their research, regulatory authority, and
 *   resource allocation). The constraint is CLAIMED as tangled_rope because
 *   it performs genuine coordination (shared fairness standards, bias
 *   detection mechanisms) while sustaining asymmetric extraction (slower
 *   deployment for deployment-urgency interests). The authored metrics
 *   reflect the actual operation: extractiveness is substantial (0.68)
 *   because audit requirements and deployment delays concentrate costs on
 *   developers and institutions; suppression is high (0.72) because the
 *   constraint excludes existential-risk framing from primary authority and
 *   marginalizes alternative alignment priorities; theater is moderate (0.41)
 *   because some audit and mitigation activity is genuine but an increasing
 *   share serves to justify the priority hierarchy rather than to measure
 *   actual fairness improvement.
 *
 * KEY AGENTS:
 *   - Marginalized populations subject to bias: powerless; trapped exit; experience direct harms from deployed AI; benefit from nearterm-harms framing but also bear indirect costs of deployment delays
 *   - Algorithmic justice advocates: moderate power; mobile exit; include scholars, civil rights organizations, regulators; benefit institutionally from nearterm-harms framing; control bias-audit research and authority
 *   - Large model developers: powerful; arbitrage exit; bear the cost of mandatory audits and deployment delays; actively resist nearterm-harms constraints; can relocate or absorb costs
 *   - AI deployment urgency interests: institutional power; constrained exit; governments, hospitals, financial firms dependent on rapid AI; bear operational cost of fairness requirements
 *   - Existential risk research community: organized; constrained exit; excluded from primary alignment authority under nearterm-harms reading; argue for integrated approach; witness the false-choice framing
 *   - Bias audit enforcement bodies: institutional; analytical exit; agenda-setters who administer fairness standards, conduct audits, enforce deployment consequences; collect authority and resources from gatekeeper role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.72).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Alignment Priority: Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '899fedde-2f86-4696-90c1-98a5db92b08f').
narrative_ontology:cs_kernel_codification('899fedde-2f86-4696-90c1-98a5db92b08f', distributed).
narrative_ontology:cs_authority_grounding('899fedde-2f86-4696-90c1-98a5db92b08f', distributed).
narrative_ontology:cs_reading_relation('899fedde-2f86-4696-90c1-98a5db92b08f', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('899fedde-2f86-4696-90c1-98a5db92b08f', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('899fedde-2f86-4696-90c1-98a5db92b08f', foundational, present_discriminatory_harms_are_binding_alignment_constraint).
narrative_ontology:cs_axiom_status(present_discriminatory_harms_are_binding_alignment_constraint, holdable).
narrative_ontology:cs_axiom_grounding('899fedde-2f86-4696-90c1-98a5db92b08f', present_discriminatory_harms_are_binding_alignment_constraint, deontological).
narrative_ontology:cs_axiom('899fedde-2f86-4696-90c1-98a5db92b08f', foundational, marginalized_population_justice_prioritized_over_capability_advancement).
narrative_ontology:cs_axiom_status(marginalized_population_justice_prioritized_over_capability_advancement, holdable).
narrative_ontology:cs_axiom_grounding('899fedde-2f86-4696-90c1-98a5db92b08f', marginalized_population_justice_prioritized_over_capability_advancement, deontological).
narrative_ontology:cs_axiom('899fedde-2f86-4696-90c1-98a5db92b08f', secondary, bias_auditing_and_mitigation_are_necessary_preconditions_for_deployment).
narrative_ontology:cs_axiom_status(bias_auditing_and_mitigation_are_necessary_preconditions_for_deployment, holdable).
narrative_ontology:cs_axiom_grounding('899fedde-2f86-4696-90c1-98a5db92b08f', bias_auditing_and_mitigation_are_necessary_preconditions_for_deployment, empirically_contingent).
narrative_ontology:cs_reference_frame('899fedde-2f86-4696-90c1-98a5db92b08f', algorithmic_justice_authority_frame).
narrative_ontology:cs_drift_state('899fedde-2f86-4696-90c1-98a5db92b08f', contemporary_ai_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('899fedde-2f86-4696-90c1-98a5db92b08f', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_subject_to_bias).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, algorithmic_justice_advocates).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, ai_deployment_urgency_interests).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, large_model_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_subject_to_bias).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face immediate harms from deployed AI systems making decisions about lending, hiring, criminal justice risk assessment, healthcare treatment, content moderation, and other high-stakes domains. They benefit from the nearterm-harms framing and enforcement because it demands bias audits before deployment and mitigation of detected discrimination. They also bear the costs of deployment delays (reduced access to beneficial AI services) and reduced availability of AI tools. Cannot exit from systems that govern fundamental life outcomes.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_subject_to_bias, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_subject_to_bias, payer).

% Scholars, research organizations, civil rights advocates, and advocacy groups dedicated to algorithmic fairness and bias mitigation. They benefit from the nearterm-harms reading because it legitimates and resources their research agenda, advocacy campaigns, and policy initiatives. Their expertise becomes central to alignment governance; they control bias-auditing methodology, fairness metrics development, and influence regulatory standards. Can redirect their institutional focus if consensus shifts toward existential-risk or integrated framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, algorithmic_justice_advocates, beneficiary,
    moderate, generational, mobile, national).

% Large AI labs and frontier model development organizations (Anthropic, OpenAI, Google DeepMind, Meta AI, others) bear the operational cost of mandatory bias audits, fairness interventions, and deployment delays required by nearterm-harms enforcement. They argue these requirements slow beneficial capability development and divert engineering resources from safety research. They can absorb compliance costs due to capital resources, relocate development to lower-regulation jurisdictions, or accelerate research timelines to race through regulatory windows.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, large_model_developers, payer,
    powerful, biographical, arbitrage, global).

% Institutions and enterprises dependent on rapid AI deployment for competitive advantage: technology companies pursuing AI-native products, healthcare systems deploying diagnostic AI, financial institutions using algorithmic trading and credit systems, government agencies implementing AI for benefits administration and law enforcement. They bear the cost of mandatory bias audits and deployment delay cycles. Their exit is constrained because competitors face the same requirements, but they actively resist and lobby against requirements perceived as slowing deployment.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_deployment_urgency_interests, payer,
    institutional, biographical, constrained, global).

% Researchers and organizations focused on existential risk from advanced AI systems, loss-of-control scenarios, and long-horizon AI safety argue that the nearterm-harms reading creates a false choice and diverts attention from catastrophic risk. They are structurally excluded from primary influence over alignment resource allocation and governance priorities when the nearterm-harms framing dominates institutional authority. They advocate for integrated approaches treating present harms and existential risk as complementary, but their arguments are marginalized in institutional settings controlled by algorithmic-justice advocates.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_research_community, excluded,
    organized, civilizational, constrained, global).

% Government AI offices, regulatory agencies (FTC, NIST, EU AI Office), institutional review boards for AI systems, technical bias-auditing firms, and fairness certification organizations that administer the nearterm-harms constraint. They set deployment standards, mandate bias audits, develop fairness metrics, review AI systems, and enforce consequences for discriminatory outcomes. They collect institutional authority and resources (funding, staff, regulatory power) from their role as alignment gatekeepers. Can exit via regulatory reform or institutional restructuring.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, bias_audit_enforcement_bodies, agenda_setter,
    institutional, generational, analytical, national).

% End-users and subjects of deployed AI systems (loan applicants evaluated by credit AI, job candidates reviewed by hiring algorithms, individuals assessed by criminal risk systems, social media users experiencing AI-curated content moderation) who are neither in organized advocacy groups nor in development institutions. They experience both the benefits of bias mitigation (reduced discriminatory harms) and the costs of deployment delays (reduced or unavailable AI services). They have no voice in alignment priority-setting despite being directly affected by the constraint's operation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, affected_individuals_in_deployment_regions, observer,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, bias_audit_enforcement_bodies).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of deploying AI systems that avoid discriminatory harms: marginalized populations, developers, regulators, and advocacy organizations need a shared definition of acceptable bias, a mechanism for detecting discrimination before deployment, and enforcement to prevent knowingly harmful systems. The nearterm-harms reading proposes that the definition is 'demonstrable fairness toward marginalized populations measured on validated bias metrics', the mechanism is pre-deployment sociotechnical audit by specialized bodies, and enforcement is regulatory denial of deployment or liability for harm.
% TRANSFER_FUNCTION: Moves technical audit labor, fairness research effort, fairness engineering resources, and institutional authority from developers and deployment-urgency institutions to bias-audit bodies and algorithmic-justice advocates. Moves temporal resources (deployment delays, extended testing cycles) from rapid-deployment priorities to bias-mitigation and verification priorities. Moves decision authority over what constitutes acceptable AI from market-driven competitive deployment to specialized regulatory bodies.
% ABSENT_VOICES: Existential risk researchers argue the nearterm-harms reading marginalizes catastrophic risk concerns and creates institutional misdirection. Affected individuals in non-advocacy communities have no organized voice in alignment priority-setting. Developers and deployment-urgency institutions resist but are not absent — their testimony disputes the priority but does not escape the authority structure.
% DISAPPEARANCE_RATIONALE: If the nearterm-harms alignment constraint vanished, developers would deploy AI systems with minimal pre-deployment bias auditing; marginalized populations would experience unmitigated discriminatory harms from automated decision-making across lending, hiring, criminal justice, and content moderation; institutional investment in algorithmic fairness research and bias remediation would shrink substantially; deployment cycles would accelerate, prioritizing capability and speed over fairness verification; regulatory oversight of deployed-system bias would collapse.
% FOUNDING_PROBLEM: Deployed AI systems exhibit systematic discrimination against marginalized populations: COMPAS criminal risk assessment over-flagged Black defendants; Amazon's hiring algorithm downranked women; credit lending algorithms excluded borrowers from historically excluded groups; facial recognition systems show higher error rates on women and people of color; content moderation systems amplify biases from training data. Early-era deployments without fairness verification caused documented harms to vulnerable populations. The nearterm-harms reading framed alignment as preventing these harms through mandatory bias audits and mitigation before deployment.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, algorithmic justice scholars, regulatory bodies (FTC, NIST, EU AI Office), and independent auditors outside the developer community attest the founding problem is active and ongoing. FTC enforcement actions against biased hiring and lending systems, academic audit studies showing persistent fairness failures in deployed systems, advocacy organization investigations documenting discriminatory outcomes — all corroborate that bias in deployed AI remains a live problem. Developers acknowledge bias exists but dispute whether it warrants the nearterm-harms reading's priority position; their testimony does not corroborate it as the central alignment concern. Regulatory bodies and affected-population advocates do corroborate it from outside the benefiting parties.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__nearterm_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__nearterm_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises gradually from 0.58 to 0.68 over the interval as deployment-delay costs accumulate and bias-audit requirements become routine. Suppression is high and stable (0.65→0.72) because the constraint's persistence depends on actively excluding existential-risk framing from primary authority and marginalizing alternative alignment priorities — not on participant preference for the nearterm-harms framing over existential-risk framing, but on institutional lock-in that treats present harms as the only legitimate alignment concern. Theater rises from 0.28 to 0.41 as audit activity increasingly becomes performative — bias-testing becomes a checkbox compliance exercise rather than deep fairness investigation, and institutional position-taking around the nearterm-harms reading displaces actual disagreement about priorities. Accessibility collapse is moderate (0.62) because alternatives to the nearterm-harms reading (existential-risk focus, integrated approach) remain live intellectual positions held by organized research communities — they are not inaccessible, only marginalized in authority structures. Resistance is moderate (0.58) because existential-risk researchers and deployment-urgency interests mount real resistance to the constraint's priority claim, though their resistance is dampened by public legitimacy of fairness concerns and regulatory enforcement mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The marginalized-population and bias-advocate seats experience this constraint as protective justice — a real coordination mechanism preventing documented harms. The deployment-urgency and developer seats experience it as unjustified extraction justified by fairness theater — they see bias audits as compliance exercises, deployment delays as competition-limiting, and the marginalizing of existential-risk concerns as a misdirection. The existential-risk research community experiences it as forced choice-making — they believe both present harms and catastrophic risks are real and complementary, but the nearterm-harms framing treats existential-risk prioritization as a false alternative and marginalizes it from authority. The agenda-setter (bias-audit enforcement) seat experiences the constraint as legitimate institutional authority — fairness standards are their expertise domain and bias auditing is their primary function. The engine computes these divergent type classifications from the structural positions; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations are beneficiaries (the constraint prevents harms to them) with high d → low χ. Algorithmic-justice advocates are beneficiaries (it legitimates and resources their work) with moderate power → moderate d → moderate χ. Large developers and deployment-urgency interests are victims (the constraint extracts from them via audit costs and deployment delays) with powerful/institutional power and constrained/arbitrage exit → high d → high χ. Existential-risk researchers are excluded by this reading's authority structure despite being institutionally powerful — their exclusion is not economic extraction but epistemic suppression, which raises their implicit d (they are suppressed targets even if not economically extracted from) but this is structural exclusion from voice, not directional extraction in the constraint's operation. The bias-audit enforcement bodies are agenda-setters (they administer and enforce) with institutional power and analytical exit → near-symmetric d but with agenda-setting authority that tips them toward beneficiary-like positions (they benefit from the constraint's persistence).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids simple mandatrophy failure (a founding problem that is dead) because the founding problem — documented harms from bias in deployed AI — is live and attested outside the benefiting parties. However, there is a subtle mandatrophy risk in the priority claim: if the integrated reading gains authority (existential-risk and nearterm-harms as complementary rather than competing), the nearterm-harms reading would persist as one constraint among multiple rather than the sole definition of alignment. This is not mandatrophy (the founding problem remains live) but it would be institutional demotion — the agenda-setter and beneficiary seats would lose exclusive authority. The measurement trajectory showing rising theater_ratio (0.28→0.41) suggests mild performance inflation: as the nearterm-harms reading becomes entrenched, some audit activity becomes ritualized rather than substantively driven. This is not yet full mandatrophy (the function persists), but it is the precondition for mandatrophy if the founding problem itself eventually becomes treated as solved (the institutional bias-audit apparatus continues performing fairness reviews even after bias measures have stabilized or when fairness improvements plateau).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fairness_metric_instability,
    'What constitutes ''demonstrated fairness'' in the nearterm-harms reading, and is that definition stable across deployment contexts and marginalized groups?',
    'Multi-year audit of fairness definitions and metrics used by bias-audit bodies across different sectors (hiring, lending, criminal justice, content moderation); comparison of which groups are treated as primary beneficiaries across different institutional contexts.',
    'If fairness definitions are unstable or context-dependent, the audit requirement becomes performative (theater_ratio climbs further) and the constraint loses coordination function. If stable, the constraint''s coordination role is validated and extraction reflects real mitigation cost. High instability would support reclassifying to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_metric_instability, empirical, 'Whether ''demonstrable fairness'' has consistent content across deployment contexts.').

omega_variable(
    priority_foreclosure_claim,
    'Does the nearterm-harms reading''s framing FORECLOSE the existential-risk reading (making them logically incompatible in a single framework), or do they COEXIST as competing priorities held by different institutional actors?',
    'Analysis of whether the nearterm-harms reading''s authority structure prohibits existential-risk research from claiming alignment relevance, or merely marginalizes it from resource allocation. Interview representatives of both readings about whether the other''s core premise is logically rejected or just deprioritized.',
    'If they foreclose each other, the reading_relations should be ''forecloses'' and the kernel involves genuine zero-sum choice. If they coexist, the relation is ''coexists_with'' and the kernel is about priority-setting among compatible claims. If one influences the other, the relation is ''influences'' and there is a structural upstream/downstream shape. This determines the degree of institutional conflict and whether integrated governance is structurally possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(priority_foreclosure_claim, conceptual, 'Whether the nearterm-harms and existential-risk readings are logically incompatible or institutionally competitive.').

omega_variable(
    marginal_bias_mitigation_cost,
    'What is the marginal cost to deploy-urgency interests (large developers, institutions) of compliance with nearterm-harms audit requirements, relative to the benefits of bias mitigation to marginalized populations?',
    'Cost-benefit accounting of AI deployment delays, audit labor, fairness engineering, and regulatory penalties; comparison with measured bias reduction in audited systems; welfare analysis of harm prevented vs. cost imposed.',
    'If costs substantially exceed prevented harms, the constraint is primarily extractive and the snare classification becomes more plausible. If costs are proportional to harms, the tangled-rope claim is supported. If costs are lower than benefits, the rope classification (pure coordination) becomes more plausible, and the authored extraction metrics would be questioned.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginal_bias_mitigation_cost, empirical, 'Whether audit costs are proportional to measured bias mitigation benefits.').

omega_variable(
    exclusion_of_existential_risk_perspective,
    'Is the marginalization of existential-risk framing in alignment governance a deliberate epistemological choice (existential risk is genuinely lower-priority) or an artifact of institutional capture by nearterm-justice advocates?',
    'Tracing institutional resource allocation, editorial decisions in AI governance bodies, and testimony from excluded existential-risk researchers about whether their exclusion reflects substantive disagreement or institutional gatekeeping.',
    'If institutional capture, the constraint''s suppression metric (0.72) reflects real power asymmetry, and the agenda-setter seat is extracting institutional authority from excluded parties. If substantive disagreement, the suppression reflects legitimate priority-setting. This determines whether suppression is structural coercion or legitimate coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_of_existential_risk_perspective, conceptual, 'Whether existential-risk marginalization reflects epistemic consensus or institutional power imbalance.').

omega_variable(
    trapped_populations_deployment_access_loss,
    'When bias audits delay deployment, do marginalized populations benefit from reduced discriminatory harms, or do they lose access to beneficial AI services (healthcare, finance, education) during the delay period?',
    'Comparative analysis of service access outcomes for marginalized populations in high-enforcement jurisdictions (mandatory bias audits, strict deployment delays) vs. low-enforcement jurisdictions (minimal pre-deployment audit); measurement of harm from discriminatory AI vs. harm from AI-service unavailability.',
    'If delays cause net harm through service loss, marginalized populations are dual victims (they are both targets of enforcement cost and lack exit options to avoid it). This would push the constraint toward snare classification and reframe the ''beneficiary'' label. If delays prevent greater harms than they cause, the beneficiary framing is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trapped_populations_deployment_access_loss, empirical, 'Whether deployment delays net-benefit or net-harm marginalized populations.').

omega_variable(
    reading_kernel_ambiguity,
    'Is the ai_alignment_priority kernel itself stable, or does framing alignment differently (as a technical problem, a governance problem, a social problem) change which reading applies?',
    'Philosophical and institutional analysis of alignment definitions: if alignment is defined as ''preventing harm X'', then nearterm-harms reading applies; if defined as ''maintaining control of capability Y'', then existential-risk reading applies; if defined as ''satisfying multiple stakeholder values'', then integrated reading applies. The kernel is what these readings share (they all answer ''what is alignment?''), but if that kernel is itself unstable or observer-dependent, the readings may not be siblings of a single kernel.',
    'If the kernel is unstable, these three readings are not alternative framings of one question but answers to different questions. Reclassifying would mean decomposing into separate constraints (one per definition of alignment) linked by network.affects_constraints rather than treating them as siblings of one kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether ai_alignment_priority is a single contested kernel or multiple distinct constraint questions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement_basis(ai_a_tr_t4, observed).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(ai_a_tr_t8, observed).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(ai_a_tr_t12, observed).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(ai_a_tr_t16, observed).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(ai_a_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement_basis(ai_a_be_t4, observed).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(ai_a_be_t8, observed).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(ai_a_be_t12, observed).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(ai_a_be_t16, observed).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(ai_a_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 4, 0.67).
narrative_ontology:measurement_basis(ai_a_su_t4, observed).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 8, 0.69).
narrative_ontology:measurement_basis(ai_a_su_t8, observed).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(ai_a_su_t12, observed).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t16, observed).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(ai_a_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__nearterm_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% The ai_alignment_priority kernel decomposes into three structurally distinct constraint stories with different ε values, victim/beneficiary sets, and authority structures. All three readings answer 'what is alignment and what should be prioritized?' but instantiate fundamentally different constraints. This nearterm_harms_reading treats alignment as prevention of present discriminatory harms and prioritizes justice for marginalized populations (high ε on deployed-system audits, victim set is marginalized groups subject to bias, beneficiary is present vulnerable populations, agenda-setter is bias-audit bodies). The existential_risk_reading would treat alignment as prevention of catastrophic loss of control and prioritize existential safety (different ε structure, different victim set, different agenda-setter, different coordination type). The integrated_reading would reject the priority choice and treat both as complementary constraints of equal standing. These are not alternative measurements of one constraint; they are three different constraints that compete for authority over what 'alignment' means. Each story must carry its own ε, its own beneficiary/victim structure, and its own classification; they are linked by network.affects_constraints (this reading influences the others by constraining the resource and institutional space they operate in) and by omega variables documenting the reading-relations structure (coexists_with, forecloses, influences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
