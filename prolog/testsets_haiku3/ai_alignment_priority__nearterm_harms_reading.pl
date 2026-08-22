% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__nearterm_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: ai_alignment_priority__nearterm_harms_reading
 *   human_readable: AI Nearterm Harms Alignment Priority (Justice-centered Reading)
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   This constraint instantiates the nearterm-harms reading of the contested
 *   AI alignment kernel. Alignment, under this reading, means ensuring that
 *   deployed AI systems do not cause or perpetuate discriminatory harms to
 *   marginalized populations (defined by age, race, disability, economic
 *   status, and other historically marginalized attributes). The priority is
 *   immediate justice for populations already suffering algorithmic
 *   discrimination. This reading coexists with the existential-risk reading
 *   (catastrophic loss of control over advanced systems) and the integrated
 *   reading (both catastrophic and present harms matter as complementary).
 *   The structural delta is sharp: high ε on deployed-system audits, victim
 *   set is concrete marginalized groups, beneficiary is present vulnerable
 *   populations, methodology is sociotechnical audit, resource flow is toward
 *   bias mitigation. The constraint is claimed as tangled_rope because it
 *   coordinates an alignment consensus (real coordination benefit around harm
 *   prevention) while extracting through asymmetric definition of what counts
 *   as harm, who participates in remedy design, and what mitigation is
 *   sufficient.
 *
 * KEY AGENTS:
 *   - Marginalized populations experiencing present algorithmic discrimination — primary victims and beneficiaries (trapped, immediate horizon, powerless)
 *   - AI deployment organizations — agenda setters (institutional power, arbitrage-grade exit, generational horizon)
 *   - Fairness researchers and ethics auditors — professional beneficiaries (organized, mobile, biographical horizon)
 *   - Existential-risk researchers — excluded from the nearterm-harms reading's authority structure (organized, constrained exit)
 *   - Oversight authorities — analytical observers (institutional power, generational horizon)
 *   - Non-participating marginalized populations — structurally excluded from audit loops (powerless, trapped, immediate horizon)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__nearterm_harms_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__nearterm_harms_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_priority__nearterm_harms_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(ai_alignment_priority__nearterm_harms_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__nearterm_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__nearterm_harms_reading, "AI Nearterm Harms Alignment Priority (Justice-centered Reading)").
narrative_ontology:topic_domain(ai_alignment_priority__nearterm_harms_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__nearterm_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__nearterm_harms_reading, '98876080-85a9-42d8-8a67-270f3cdd3eea').
narrative_ontology:cs_kernel_codification('98876080-85a9-42d8-8a67-270f3cdd3eea', formalized).
narrative_ontology:cs_authority_grounding('98876080-85a9-42d8-8a67-270f3cdd3eea', lineage).
narrative_ontology:cs_interpretation_layer_present('98876080-85a9-42d8-8a67-270f3cdd3eea').
narrative_ontology:cs_reading_relation('98876080-85a9-42d8-8a67-270f3cdd3eea', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('98876080-85a9-42d8-8a67-270f3cdd3eea', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('98876080-85a9-42d8-8a67-270f3cdd3eea', foundational, present_discriminatory_harms_are_alignment_priority).
narrative_ontology:cs_axiom_status(present_discriminatory_harms_are_alignment_priority, holdable).
narrative_ontology:cs_axiom_grounding('98876080-85a9-42d8-8a67-270f3cdd3eea', present_discriminatory_harms_are_alignment_priority, deontological).
narrative_ontology:cs_axiom('98876080-85a9-42d8-8a67-270f3cdd3eea', foundational, marginalized_populations_justice_centered).
narrative_ontology:cs_axiom_status(marginalized_populations_justice_centered, holdable).
narrative_ontology:cs_axiom_grounding('98876080-85a9-42d8-8a67-270f3cdd3eea', marginalized_populations_justice_centered, deontological).
narrative_ontology:cs_reference_frame('98876080-85a9-42d8-8a67-270f3cdd3eea', algorithmic_fairness_imperative).
narrative_ontology:cs_drift_state('98876080-85a9-42d8-8a67-270f3cdd3eea', contemporary_ai_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('98876080-85a9-42d8-8a67-270f3cdd3eea', '2026-06-20T14:32:18Z').
narrative_ontology:cs_kernel_id(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_present_harm_victims).
narrative_ontology:constraint_victim(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_present_harm_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__nearterm_harms_reading, fairness_researchers_and_auditors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience discriminatory harms from deployed AI systems in lending, hiring, criminal justice, content moderation, and healthcare. The nearterm-harms alignment priority centers their immediate safety as the measure of alignment. They benefit from audits and bias mitigation that reduce harms (fewer wrongful loan denials, lower false-positive risk predictions, less biased content suppression). They also bear the cost of being the measurement object: their continued subjection to systems is required to audit and demonstrate harm; their participation in remedy design is often instrumental rather than governance-level; their definition of justice (participatory, transformative, systemic) may be narrower than the constraint allows.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_present_harm_victims, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_present_harm_victims, payer).

% Deploy AI systems in high-stakes domains. They operate under the nearterm-harms priority: systems must be audited for discriminatory harms to marginalized populations, and alignment requires demonstrable mitigation. This constraint provides legitimacy (regulatory compliance, moral cover, stakeholder support) and allows continued deployment while appearing responsive to harms. Organizations control audit scope (which harms matter), remedy sufficiency (what level of bias reduction constitutes alignment), and outcome measurement (whether the deployed system is 'fair enough'). The constraint creates coordination benefit (shared understanding of alignment requirements) and extraction (defining alignment in terms compatible with continued profitable deployment).
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, ai_deployment_organizations, agenda_setter,
    institutional, generational, arbitrage, global).

% Their careers, funding, legitimacy, and institutional standing depend on the nearterm-harms reading being authoritative. Academic positions in fairness and ethics, research grants for bias detection, consulting contracts for auditing, policy influence over AI governance all flow from specialization in nearterm-harms assessment and mitigation. The constraint ensures steady demand for their expertise, career advancement, and professional legitimacy. They shape which harms are visible (auditable via algorithmic metrics) and which remain invisible (harms that resist quantification, harms that require systemic change rather than optimization).
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, fairness_researchers_and_auditors, beneficiary,
    organized, biographical, mobile, global).

% Hold a competing reading of alignment prioritizing catastrophic-risk prevention (interpretability, control, alignment of advanced systems) over present-harm mitigation. When organizations adopt the nearterm-harms priority, existential-risk researchers lose influence over AI governance agendas and funding allocation. They are excluded not by explicit rule but by institutional authority and resource flow: their research directions and concerns are treated as lower priority. Shifting research focus away from existential risk carries reputational and career costs (starting over in a new domain; competitive disadvantage relative to established nearterm-harms researchers).
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, existential_risk_alignment_researchers, excluded,
    organized, civilizational, constrained, global).

% People from marginalized groups harmed by AI systems who do not participate in audits, consent mechanisms, remedy feedback loops, or governance structures. They experience harms but are not consulted on what counts as harmful or what mitigation is sufficient. Their structural exclusion is maintained by the constraint's focus on auditable, researcher-measurable harms and by participatory governance models that center researchers and deployment organizations rather than affected communities.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, marginalized_populations_non_participating, excluded,
    powerless, immediate, trapped, global).

% Legislators, regulators, and civil-rights authorities that monitor AI deployment for compliance with fairness and harm-prevention standards. They take testimony from all seats, commission audits, and can mandate remedies or system halt. Their analytical position is upstream of the alignment reading's policy and organizational authority. They have jurisdiction to reframe the constraint or impose different alignment requirements.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__nearterm_harms_reading, oversight_authorities_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__nearterm_harms_reading, ai_deployment_organizations).
narrative_ontology:fixing_cost_class(ai_alignment_priority__nearterm_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework that deployed AI systems must be audited for discriminatory harms to marginalized populations and that alignment requires demonstrable mitigation of those harms. Coordinates deployment organizations, researchers, oversight authorities, and affected populations around a common understanding of what alignment means in practice.
% TRANSFER_FUNCTION: Moves professional legitimacy, research funding, policy influence, and consulting revenue toward fairness researchers and bias-mitigation expertise. Moves organizational legitimacy and regulatory compliance benefit toward deployment organizations that adopt and execute the nearterm-harms priority. For marginalized populations, transfers (in principle) improved treatment by systems, but actual resource transfer is often indirect (better system performance rather than direct compensation or control over system design).
% ABSENT_VOICES: Existential-risk researchers are structurally excluded: the nearterm-harms reading displaces their research directions and resource claims. Marginalized populations whose harms are not measurable via algorithmic audit or who reject the audit-and-optimize framing (those who want systems removed, radically transformed, or placed under community control) are excluded from the constraint's governance structures.
% DISAPPEARANCE_RATIONALE: If the nearterm-harms alignment priority vanished, deployment organizations would have no institutionalized pressure to audit and mitigate discriminatory harms; fairness research funding and career opportunities would shift to other problems; marginal harm-prevention activities would persist but would lack the legitimacy framework this constraint provides. The measurement methodologies, funding flows, and governance structures around AI fairness would reorganize.
% FOUNDING_PROBLEM: Deployed AI systems cause measurable discriminatory harms to marginalized populations in high-stakes domains: lending, hiring, criminal justice, content moderation, and healthcare. These harms compound existing systemic disadvantage and are preventable through bias detection and mitigation.
% FOUNDING_PROBLEM_CORROBORATION: Extensive documentation by independent fairness researchers, civil-rights organizations, regulatory agencies, and affected communities. ProPublica's COMPAS analysis, lending discrimination investigations, hiring-system audits, and community testimony all corroborate that algorithmic discrimination is real and ongoing.
narrative_ontology:disappearance_verdict(ai_alignment_priority__nearterm_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__nearterm_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__nearterm_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__nearterm_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__nearterm_harms_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval because: (1) deployment organizations initially adopt the nearterm-harms priority as a genuine coordination mechanism; (2) over time, the measurement and remedy framework becomes captured by deployment organizations themselves—what counts as a 'harm,' what harms matter, what mitigation is sufficient are increasingly defined by those who benefit from deployment; (3) the constraint evolves to function as a legitimacy cover for continued deployment rather than as a hard halt on harmful systems. Theater ratio plateaus at 0.42 because fairness audits and bias mitigation are performing real functions (bias detection, harm measurement) but with increasing performative component—the activities look like alignment but generate incremental rather than systemic change. Suppression rises from 0.58 to 0.71 as existential-risk researchers and radical-justice advocates are increasingly excluded from alignment governance: the constraint actively suppresses calls for system removal, and alternative framings of justice (participatory, transformative, rather than audit-based) are de-prioritized. Accessibility collapse sits at 0.64 because alternatives exist (existential-risk focus, integrated framing, complete system removal) but the institutional power of deployment organizations with stakes in the nearterm-harms reading makes switching costly.
 *
 * PERSPECTIVAL GAP:
 *   From the deployment organization's seat: the nearterm-harms priority is a genuine coordination commitment—alignment requires measurable fairness, audits prove commitment, mitigation demonstrates responsibility. From the marginalized-populations seat: the constraint delivers some immediate harm reduction (better loan approval for some, fewer wrongful risk predictions for some) but operates extractively by defining justice as algorithmic optimization rather than systemic change, by instrumentalizing affected populations as audit subjects rather than governance actors, and by legitimizing continued deployment of systems they might prefer removed. From the fairness-researcher seat: the constraint creates professional legitimacy and career resources. From the existential-risk seat: the constraint starves catastrophic-risk research of resources and governance authority. The engine should compute these as divergent types from the same structural data: beneficiary seats compute low extraction, target seats compute high extraction, excluded seats see extraction masked as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations are the constraint's primary victims (trapped exit, immediate time horizon, powerless position—high d toward extraction) and nominal beneficiaries (harms from their specific vulnerabilities are the measure of alignment). This dual position is the core extractive structure: they are centered as the moral priority of the constraint and simultaneously excluded from defining what alignment requires. Deployment organizations have arbitrage-grade exit (they can shift to existential-risk framing if political pressure changes) and institutional power—derived d should be moderate, benefiting from coordination authority without facing the full target pressure. Fairness researchers have mobile exit (can shift to other problems) and organized power, but their livelihoods depend on the constraint's persistence—derived d should be low, capturing benefit. Existential-risk researchers are excluded and constrained (cannot easily shift their research focus without career damage)—high d toward extraction, though not from marginalized populations directly but from the resource competition.
 *
 * MANDATROPHY ANALYSIS:
 *   The nearterm-harms reading does not exhibit classic mandatrophy (founding problem dead, constraint persists). The founding problem is live: algorithmic discrimination is real and persistent. What evolves is the constraint's function: it begins as a coordination mechanism (we will measure and mitigate harms) and drifts toward extraction (we will define justice in terms compatible with continued deployment). The theater ratio rising indicates the performative component of fairness audits increasing relative to actual harm remediation—the constraint begins to function as legitimacy theatre for deployment organizations. The suppression requirement rising indicates growing effort to exclude alternative framings of alignment. If the theater ratio crossed 0.5 and remained high while extractiveness plateaued, true mandatrophy would be confirmed. At the interval's end (T=25), extractiveness still rises (though flattening) and theater is still below 0.5, suggesting the constraint is in transition—the coordination function is still materially real (harms are being measured and some mitigated), but the extraction component is growing and will eventually dominate if the trend continues past T=25.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_capture_bias,
    'Who defines what counts as a ''harm'' in algorithmic systems, and does the measurement framework capture harms that are real but not auditable?',
    'Participatory audit designs that center marginalized-population definitions of harm, not just researcher-designed fairness metrics. Comparison of auditor-measured harms against community-reported experiences of the same systems.',
    'If marginalized populations and researchers diverge substantially on what counts as harmful, the nearterm-harms reading''s measurement framework is capturing deployment-organization interests (optimizable harms) rather than lived harm. This would increase the constraint''s effective extraction from the target population and confirm the measurement-capture analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_capture_bias, empirical, 'Whether the constraint''s measurement methodology captures harms as experienced by affected populations or only auditable/optimizable harms.').

omega_variable(
    remedy_sufficiency_asymmetry,
    'Is incremental bias mitigation (reducing discrimination rates) the same as justice for populations harmed by deployed systems, or do affected populations require systemic change (system removal, participatory governance, resource reallocation)?',
    'Long-term outcome studies comparing algorithmic fairness improvements against actual outcomes for marginalized groups (credit access, employment, criminal-justice outcomes). Participatory justice frameworks that distinguish algorithmic fairness from systemic remedies.',
    'If communities require systemic change but the constraint frames justice as algorithmic optimization, the constraint is systematically excluding the remedy marginalized populations actually demand. This would be evidence of extraction masked as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_sufficiency_asymmetry, preference, 'Whether algorithmic bias mitigation is sufficient justice or whether present-harm prevention requires systemic change that the constraint excludes.').

omega_variable(
    kernel_compatibility_existential_vs_nearterm,
    'Is the nearterm-harms reading logically compatible with the existential-risk reading in a single governance framework, or do they require resource/priority trade-offs that force institutional choice between them?',
    'Analysis of governance structures that have attempted to honor both readings; budget allocation studies showing whether organizations prioritizing nearterm harms also maintain existential-risk research capacity.',
    'If the readings are materially incompatible (zero-sum on resources, authority structure, research direction), the nearterm-harms reading is not a reading of the same kernel—it is a competing kernel. This would reclassify the relationship from coexists_with to forecloses. If they are compatible, resource choices are political rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_compatibility_existential_vs_nearterm, conceptual, 'Whether the nearterm-harms and existential-risk readings can coexist in one governance framework or force binary institutional choices.').

omega_variable(
    identity_lock_marginalized_populations,
    'To what extent are marginalized populations'' identities fused with their relationship to the nearterm-harms priority such that they would experience exit as identity dissolution?',
    'Qualitative and quantitative analysis of how communities tied to nearterm-harms advocacy experience proposals to shift to existential-risk framing or integrated framing—whether identity as ''affected communities'' and advocate status is contingent on the nearterm-harms reading''s centrality.',
    'If marginalized populations become identity-locked into the nearterm-harms reading (their social position, professional roles, community standing depend on the reading''s authority), their exit options shift from trapped to identity_locked, increasing effective extraction and suppression. This would signal deep institutional capture of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_marginalized_populations, empirical, 'Whether marginalized populations'' identity and social position become fused with the nearterm-harms alignment reading, creating identity-lock exit barriers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__nearterm_harms_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ai_a_tr_t0, observed).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(ai_a_tr_t5, observed).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(ai_a_tr_t10, observed).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(ai_a_tr_t15, observed).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ai_a_tr_t20, observed).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_priority__nearterm_harms_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(ai_a_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_a_be_t0, observed).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ai_a_be_t5, observed).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(ai_a_be_t10, observed).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(ai_a_be_t15, observed).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_a_be_t20, observed).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_priority__nearterm_harms_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ai_a_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_a_su_t0, observed).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(ai_a_su_t5, observed).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(ai_a_su_t10, observed).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ai_a_su_t15, observed).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t20, observed).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_priority__nearterm_harms_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(ai_a_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__nearterm_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__nearterm_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__nearterm_harms_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested AI alignment kernel. The existential_risk_reading prioritizes catastrophic-loss prevention; the integrated_reading holds both catastrophic and present harms as complementary. All three are readings of the same kernel: 'alignment is the proper goal of AI development.' The readings diverge on what alignment requires and compete for organizational priority and resource allocation. Each reading instantiates a different constraint with different ε values, victim sets, beneficiaries, and methodologies. The network edges track dependency: both the existential-risk and integrated readings are downstream of the nearterm-harms reading's institutional authority (they must address or refute its framing to gain traction), but the existential-risk reading explicitly forecloses the nearterm-harms reading's resource claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__nearterm_harms_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
