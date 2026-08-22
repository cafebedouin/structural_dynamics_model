% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance: Present Harms Priority Reading
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   AI risk governance faces a foundational allocation problem: should
 *   priority be given to mitigating documented present harms (algorithmic
 *   bias, labor displacement, surveillance, misinformation) that affect real
 *   populations now, or should priority be given to preventing speculative
 *   existential risks from advanced AI systems? This constraint story
 *   instantiates ONE READING of that contested kernel—the near-term-harms
 *   reading. From this perspective, technology companies and AI capability
 *   labs benefit structurally from x-risk framing because it directs
 *   governance attention, funding, and regulatory focus away from
 *   present-deployment failures. Marginalized populations experiencing
 *   algorithmic discrimination, workers facing displacement, and populations
 *   subject to surveillance deployment bear the costs of deferred
 *   present-harm governance while resources flow to existential-risk
 *   research. The constraint is CLAIMED as tangled_rope (genuine coordination
 *   problem + asymmetric extraction) while the metrics describe substantially
 *   extractive, actively enforced operation. The reading is fully
 *   independent: it declares a distinct victim set, beneficiary structure,
 *   and ε value as this kernel reading sees them.
 *
 * KEY AGENTS:
 *   - Marginalized populations experiencing algorithmic discrimination (criminal justice, lending, employment, housing)
 *   - Workers displaced by automation (manufacturing, call centers, knowledge work)
 *   - Global South populations subject to surveillance infrastructure
 *   - Technology companies avoiding regulation (beneficiary of x-risk framing)
 *   - AI capability labs focused on scaling (beneficiary of deferred deployment governance)
 *   - Fairness researchers (excluded from governance priority-setting)
 *   - Civil-society advocates (excluded from policy tables)
 *   - Governance policymakers (agenda-setter, subject to asymmetric lobbying pressure)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.82).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.71).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance: Present Harms Priority Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '438cc33b-71ed-4862-8270-51a8e2986de0').
narrative_ontology:cs_kernel_codification('438cc33b-71ed-4862-8270-51a8e2986de0', distributed).
narrative_ontology:cs_authority_grounding('438cc33b-71ed-4862-8270-51a8e2986de0', extraction).
narrative_ontology:cs_interpretation_layer_present('438cc33b-71ed-4862-8270-51a8e2986de0').
narrative_ontology:cs_reading_relation('438cc33b-71ed-4862-8270-51a8e2986de0', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('438cc33b-71ed-4862-8270-51a8e2986de0', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('438cc33b-71ed-4862-8270-51a8e2986de0', foundational, demonstrated_present_harm_overrides_speculation).
narrative_ontology:cs_axiom_status(demonstrated_present_harm_overrides_speculation, holdable).
narrative_ontology:cs_axiom_grounding('438cc33b-71ed-4862-8270-51a8e2986de0', demonstrated_present_harm_overrides_speculation, deontological).
narrative_ontology:cs_axiom('438cc33b-71ed-4862-8270-51a8e2986de0', foundational, marginalized_populations_entitled_to_immediate_governance).
narrative_ontology:cs_axiom_status(marginalized_populations_entitled_to_immediate_governance, holdable).
narrative_ontology:cs_axiom_grounding('438cc33b-71ed-4862-8270-51a8e2986de0', marginalized_populations_entitled_to_immediate_governance, deontological).
narrative_ontology:cs_axiom('438cc33b-71ed-4862-8270-51a8e2986de0', secondary, present_harm_mitigation_actionable_now).
narrative_ontology:cs_axiom_status(present_harm_mitigation_actionable_now, holdable).
narrative_ontology:cs_axiom_grounding('438cc33b-71ed-4862-8270-51a8e2986de0', present_harm_mitigation_actionable_now, empirically_contingent).
narrative_ontology:cs_reference_frame('438cc33b-71ed-4862-8270-51a8e2986de0', governance_prioritizes_documented_harms).
narrative_ontology:cs_drift_state('438cc33b-71ed-4862-8270-51a8e2986de0', post_x_risk_institutional_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('438cc33b-71ed-4862-8270-51a8e2986de0', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies_avoiding_regulation).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_research_labs_focused_on_capabilities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations_algorithmic_discrimination).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations_surveillance_exposure).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, content_moderation_workers_misinformation_harms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, fairness_and_accountability_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face algorithmic bias in criminal justice (risk-assessment tools), lending (credit scoring), employment (resume filtering), and housing (rental algorithms). Exit options are severely constrained: refusing to interact with systems means losing access to credit, employment, housing, and judicial fairness. The algorithmic harms are immediate and documented—biased predictions create disparate outcomes in concrete systems affecting real livelihoods. They bear the costs of deployment bias but have no seat at governance tables setting AI policy priorities.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations_algorithmic_discrimination, payer,
    powerless, biographical, trapped, global).

% Experience job loss and wage suppression from AI-driven automation in manufacturing, call centers, and knowledge work. The displacement is happening now: documented job losses, retraining costs borne by individuals rather than deployers, and wage pressure in adjacent sectors. They face immediate economic harm and have constrained exit—retraining requires capital and time most lack; geographic mobility is limited. Organized labor has some voice but faces asymmetric power against well-capitalized tech companies.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, workers_displaced_by_automation, payer,
    moderate, biographical, constrained, global).

% Are subject to large-scale surveillance systems deployed by governments and private companies with minimal transparency or consent. AI-powered facial recognition, predictive policing, and behavioral targeting disproportionately affect populations in regions with weak data-protection law and limited recourse. The harm is present and verifiable: documented surveillance, documented political targeting, documented suppression of dissent. Their only exit is geographic relocation, which is economically and legally impossible for most.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations_surveillance_exposure, payer,
    powerless, biographical, trapped, global).

% Bear psychological and health costs of reviewing misinformation, violent content, and coordinated harassment at scale. AI systems for content moderation remain imperfect; workers review escalations from algorithmic filters. They experience documented trauma, burnout, and lack of regulatory protection. Exit options are constrained by labor-market power asymmetry and the need for wage income; reporting harms often triggers retaliation from employers.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, content_moderation_workers_misinformation_harms, payer,
    powerless, biographical, constrained, global).

% Benefit from governance frameworks that prioritize speculative existential risks over documented present harms. The x-risk framing directs regulatory and public attention away from present deployment failures, allowing continued rapid deployment of biased systems, labor-displacing automation, and surveillance infrastructure without immediate consequence. They avoid costly compliance and bias-mitigation mandates. The existential-risk frame creates a legitimacy umbrella under which present-harm governance can be deferred as less urgent than long-term safety. They set research agendas and funding priorities that emphasize capabilities research and x-risk over fairness and labor-impact audits.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies_avoiding_regulation, beneficiary,
    institutional, generational, arbitrage, global).

% Receive substantial funding and research priority under a governance regime emphasizing existential risks. The x-risk narrative justifies rapid capability scaling with minimal immediate-deployment governance. They frame present harms as secondary to the imperative to 'solve superintelligence before it arrives.' They have strong voice in governance discussions through alignment researchers and technical safety programs. They benefit from deferred regulation of present systems because it accelerates capability development.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_research_labs_focused_on_capabilities, beneficiary,
    powerful, generational, arbitrage, global).

% Document present harms and advocate for immediate governance but face marginalized voice in policy discussions dominated by x-risk framings. They have research autonomy but constrained funding relative to capabilities research. They argue for fairness audits, transparency, and accountability mechanisms but lack institutional power to enforce these priorities against well-capitalized AI labs and companies. They are structurally excluded from setting the governance agenda even though their expertise directly addresses documented harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, fairness_and_accountability_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, fairness_and_accountability_researchers, excluded).

% Must decide resource allocation and priority-setting for AI governance. They face lobbying from both x-risk advocates and present-harm advocates. They are increasingly influenced by the x-risk framing (existential threat, requires urgent prevention). They administer the constraint by setting which harms are prioritized, which research gets funded, and which governance mechanisms are mandated. They have formal authority but face information asymmetry and lobbying pressure from well-resourced actors.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, governance_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for prioritizing present harms affecting marginalized communities. They represent affected populations but lack institutional power in governance discussions dominated by technical experts and company voices. They are often excluded from policy tables where AI governance priorities are set. Their constituencies experience the documented harms directly, but their voice is marginalized as 'lacking technical expertise' relative to x-risk researchers.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, civil_society_advocates, excluded,
    organized, biographical, constrained, global).

% Views the constraint structure from outside the immediate power dynamics. Observes that prioritizing existential risk governance over present-harm governance creates asymmetric extraction: those experiencing documented harms now subsidize the research and policy agenda of those managing speculative future risks. The observer tracks how the constraint's enforcement operates and what voices are systematically excluded.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, technology_companies_avoiding_regulation).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate allocation of scarce governance resources (expertise, funding, regulatory attention) across competing AI risk categories (present harms, existential risks, alignment).
% TRANSFER_FUNCTION: Directs governance funding, research priorities, and regulatory focus away from present-harm mitigation (fairness audits, transparency mandates, labor-impact assessment) toward existential-risk research (alignment, capability safety, superintelligence prevention). Moves the burden of deferred governance onto marginalized populations and workers experiencing documented algorithmic harms. Moves regulatory prestige and institutional authority from present-harm governance to existential-risk expertise.
% ABSENT_VOICES: Marginalized populations experiencing algorithmic discrimination are not in policy discussions; workers displaced by automation lack representation at strategy tables; Global South populations subject to surveillance infrastructure have no seat in North American/European governance forums; fairness researchers are present but excluded from priority-setting; content-moderation workers' experiences are not systematized into policy frameworks.
% DISAPPEARANCE_RATIONALE: If present-harm governance were prioritized over existential-risk governance, resource allocation would shift: fairness research and bias-audit mandates would move to immediate deployment gates; transparency requirements would be implemented now rather than deferred; labor-displacement policies would be formalized; surveillance systems would face regulatory friction before deployment. Technology companies would face immediate compliance costs and deployment constraints. The AI governance landscape would reorganize around preventing algorithmic discrimination and labor harm as primary objectives, with existential-risk research continuing but at reduced resource levels relative to current institutional hierarchy.
% FOUNDING_PROBLEM: The founding problem for this reading is: AI systems deployed at scale cause measurable, documented harms (algorithmic bias in criminal justice and lending, labor displacement, surveillance exposure, misinformation harms) to marginalized populations now. These harms are verifiable, causally attributable to deployed systems, addressable through available interventions (fairness audits, transparency mandates, deployment gates), and require immediate governance priority because affected populations cannot wait decades for existential-risk research to complete.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by documented cases: algorithmic bias in COMPAS (recidivism prediction causing wrongful incarceration), discrimination in lending algorithms, job-application filtering affecting marginalized job-seekers, facial-recognition errors disproportionately affecting people of color, labor displacement in manufacturing and call centers, documented surveillance of protests and dissent. Corroborated by fairness researchers, civil-society organizations (ACLU, AI Now Institute, Access Now), labor economists, and affected-community advocates. The founding problem status is contested only by those claiming existential risk supersedes present harm in priority; the harms themselves are not disputed—only whether they justify governance priority relative to existential-risk research.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) and rising over the interval because the governance constraint persistently directs resources away from present-harm mitigation toward existential-risk research, while marginalized populations experience documented algorithmic harms without corresponding governance protection. The temporal trend shows extractiveness increasing as x-risk framing becomes more institutionalized in policy and funding: the constraint's extraction function (directing governance away from present harms) becomes more effective over time. Suppression is substantial (0.71) because marginalized voices are systematically excluded from governance discussions and the x-risk narrative is defended by well-resourced institutions and researchers. Theater ratio is moderate (0.48) and flattens over the interval: the constraint includes some genuine coordination function (allocating scarce governance resources) but an increasing share of enforcement activity defends the x-risk framing against present-harm advocates. As the constraint matures, more enforcement effort goes to maintaining the narrative supremacy of existential risk over present harms, less to genuinely solving the allocation problem. Accessibility collapse is moderate (0.68): alternatives to x-risk framing exist and are actively advocated, but once the governance establishment adopts x-risk priorities, alternative framings face strong institutional barriers. Resistance is high (0.72): affected populations, civil-society organizations, and fairness researchers actively resist the constraint by documenting present harms, building alternative governance frameworks, and challenging x-risk dominance in policy discourse.
 *
 * PERSPECTIVAL GAP:
 *   The seated payer and beneficiary perspectives compute radically different constraint types from the same structural data. From the marginalized-population seat: this is a snare disguised as coordination—the stated governance problem (allocating finite resources) is real, but the solution persistently extracts from those most affected by deployment harms. From the technology-company seat: this is genuine coordination—existential risk is the true foundational problem, present harms are secondary governance issues, and x-risk framing efficiently focuses governance resources on the most important long-term challenge. The same constraint structure computes as (snare, high extraction, full target) for powerless populations and (rope, moderate coordination, beneficiary subsidizing long-term research) for capability labs. The engine computes per-seat; this gap is the structural story.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality strongly differentiates by power and structural position. Technology companies and AI labs sit near d=0.0 (full beneficiary): they avoid immediate compliance costs, face no regulatory constraint on capability scaling, and benefit from governance attention directed toward long-term alignment rather than present-deployment accountability. Marginalized populations, displaced workers, and surveillance-exposed populations sit near d=1.0 (full target): they bear documented harms from deployed systems, have no governance voice to redress those harms, and experience deferred mitigation as they subsidize long-term research. Fairness researchers and civil-society advocates sit at moderate d (0.5–0.7): they benefit from acknowledgment that present harms matter but are excluded from high-priority resource allocation and governance authority. Governance policymakers sit at d=0.5–0.6 (slightly toward target): they face lobbying pressure from both sides but asymmetric information advantage favors well-resourced x-risk advocates. The constraint's enforcement—which framings dominate policy discourse, which research gets funded, which harms get prioritized—depends entirely on maintaining the governance prestige of existential-risk framing while marginalizing present-harm advocates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for this reading is: present harms to marginalized populations are documented, causally attributable to deployed systems, immediately actionable through fairness audits and deployment gates, and require governance prioritization. The constraint's mandatrophy risk is substantial: as AI systems accumulate capability and scale deployment, the documented present harms accumulate and become harder to reverse (workers permanently out of labor markets, populations with lifetime surveillance records, algorithmic systems trained on biased data whose effects persist). Simultaneously, the existential-risk justification for deferring present-harm governance becomes stronger in policy discourse—'we must scale capability to solve alignment before superintelligence emerges' becomes the dominant narrative. The constraint shows classic mandatrophy symptoms: the original governance problem (prioritize documented harms) is substituted by a meta-problem (prevent existential risk from derailing governance), the original solution (fairness audits, transparency, deployment accountability) is deferred as premature relative to the meta-problem, and the original beneficiaries (affected populations) are redefined as secondary stakeholders in a larger civilizational-stakes narrative. The analysis suggests: if x-risk governance continues to dominate without addressing present-harm governance, the populations experiencing algorithmic bias and displacement will have been systematically excluded from governance remedy for an indefinite period, making the original mandate (govern present harms) effectively obsolete in practice even if nominally still stated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    x_risk_framing_legitimacy,
    'Is existential AI risk the foundationally urgent governance problem, or does the framing of existential risk as primary legitimate the deferral of present-harm governance?',
    'Longitudinal tracking of governance outcomes: if x-risk priority leads to capability scaling without present-harm mitigation, then enables widespread algorithmic harm, the framing''s legitimacy is questioned. Alternatively, if x-risk research prevents dangerous superintelligence scenarios, the priority is validated. The resolution requires decades of outcome observation.',
    'If existential-risk framing is ultimately unjustified, the constraint is a snare using speculative fears as cover for extraction. If justified, the constraint is tangled_rope where short-term extraction is the cost of solving the foundational long-term problem. The classification hinges entirely on whether existential AI risk is real and addressable only by the governance approach the constraint defends.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(x_risk_framing_legitimacy, empirical, 'Whether existential AI risk is the foundational governance problem or a cover story for deferring present-harm accountability.').

omega_variable(
    present_harm_vs_speculation_boundary,
    'What methodologically distinguishes a ''documented present harm'' from a ''speculative future risk'' in a way that should govern priority-setting?',
    'Develop explicit governance criteria: documented harms require immediate evidence and affected-population voice; speculative risks require plausible mechanism and expert-community consensus. Apply criteria consistently across both categories. If the criteria would give both categories equal weight, present harms would be prioritized (they have immediate affected populations). If criteria systematically favor existential-risk framing despite equal evidence quality, that reveals suppression mechanism.',
    'If the boundary is epistemologically justified, the constraint''s classification stands. If the boundary is instituted primarily through narrative supremacy of x-risk advocates, the constraint is more extractive than the framing suggests. The test is whether the priority allocation tracks the actual evidence quality or the institutional power of competing framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_harm_vs_speculation_boundary, conceptual, 'Whether present-harm vs. existential-risk priority reflects epistemological difference or power asymmetry.').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of present-harm governance primarily structural (external barriers to voice) or internalized (belief in x-risk priority by affected populations and fairness advocates)?',
    'Post-exit observation: if populations or researchers who leave the constraint framework continue to accept x-risk priority as legitimate, suppression is internalized; if they recognize deferred governance as unjust and organize alternative frameworks, suppression was primarily structural. Additionally, survey affected populations on whether they accept x-risk framing or experience it as imposed.',
    'If suppression is structural, removing institutional barriers would change priorities. If suppression is internalized, the constraint is more resistant to remedy through governance redesign. Internalization indicates the constraint has become self-perpetuating through narrative dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of present-harm governance is structural or internalized belief.').

omega_variable(
    kernel_reading_research_agenda_entanglement,
    'Does this reading''s ε value (high extraction from present-harm deferral) depend on which kernel reading is adopted, such that declaring beneficiaries changes if the existential_risk_reading is instantiated instead?',
    'Generate the sibling existential_risk_reading as a separate constraint story. Compare beneficiary/victim declarations: if technology companies and x-risk labs remain beneficiaries in the existential_risk_reading too, the ε values converge and suggest a single underlying extraction mechanism. If beneficiary/victim sets are completely inverted (existential-risk reading treats tech companies as victims of present-harm regulation), then ε is genuinely reading-dependent and represents a measurement of narrative contest rather than a structural property.',
    'If ε is reading-dependent in this way, both readings are empirically valid but epistemically incommensurable—the engine should flag them as kernel-reading artifacts rather than converging on a structural fact. If ε converges, the kernel reading is merely re-describing one underlying constraint from different narrative angles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_research_agenda_entanglement, conceptual, 'Whether ε is a structural property or a reading-indexed measurement of narrative contest.').

omega_variable(
    governance_resource_fungibility,
    'Are governance resources for present-harm mitigation and existential-risk research truly scarce and substitutable (funding one requires defunding the other), or is the scarcity narrative used to justify extraction?',
    'Empirical audit: track total global governance spending on present-harm mitigation and existential-risk research. Test whether absence of present-harm funding is due to absolute scarcity or institutional prioritization (if existential-risk research receives orders-of-magnitude more funding despite equal need, scarcity is narrative). Proposal: implement mandatory resource allocation (e.g., 20% governance resources to present-harm audits, 80% to existential risk) and observe whether existential-risk outcomes improve or degrade.',
    'If scarcity is genuine, the constraint describes a real allocation trade-off and classification stands. If scarcity is narrative (resources exist but are institutionally directed away from present harms), the constraint is a snare using resource fiction to justify extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_resource_fungibility, empirical, 'Whether governance resource scarcity is real or narrative justification for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(ai_r_tr_t30, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(ai_r_be_t30, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(ai_r_su_t30, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% Part of kernel constraint family: ai_risk_governance_priority. This story instantiates the near_term_harms_reading, which declares present harms to marginalized populations as the foundational governance problem. The existential_risk_reading (separate story) prioritizes superintelligence prevention and treats present harms as secondary. The bridge_reading (separate story) treats both as structurally entangled. All three are readings of the same contested governance kernel and should not be merged. They have distinct ε values, beneficiary/victim sets, and victim/payer directionality: near_term_harms_reading shows high extraction from marginalized populations due to deferred governance; existential_risk_reading shows different beneficiary structure (research institutions, long-term humanity) and different victim set (humanity at risk). The kernel is that AI governance must allocate scarce resources. The readings differ on what constitutes the 'real' problem requiring priority. Network links enable the engine to track how adopting one reading affects the others' classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
