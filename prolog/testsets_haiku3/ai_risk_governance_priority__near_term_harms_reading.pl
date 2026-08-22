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
 *   human_readable: AI Risk Governance: Near-Term Harms Priority Reading
 *   domain: technology_ethics/governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested kernel governing
 *   AI risk governance prioritization. The near-term-harms reading asserts
 *   that governance must prioritize documented, empirically demonstrable
 *   algorithmic discrimination and labor displacement affecting marginalized
 *   populations, Global South communities, displaced workers, and
 *   surveillance targets — prioritizing accountability frameworks, bias
 *   mitigation, regulatory enforcement, and worker transition support. The
 *   competing sibling readings — existential-risk reading (prioritize
 *   superintelligence prevention) and bridge reading (integrate both as
 *   non-mutually-exclusive) — inhabit the same kernel (the legitimacy claim
 *   about what AI governance should address) but establish different
 *   victim/beneficiary structures and resource flows. From the
 *   near-term-harms seat, technology companies and AI labs benefit from
 *   existential-risk framing because it diverts regulatory and public
 *   attention away from immediate accountability for current deployment
 *   harms. The foundational premise distinguishing this reading: algorithmic
 *   harms to marginalized populations are demonstrable, urgent, and are the
 *   primary responsibility of governance frameworks. The constraint's
 *   operation enforces this priority through regulatory attention, corporate
 *   compliance requirements, and research funding allocation — creating
 *   active suppression from actors preferring existential-risk framing.
 *
 * KEY AGENTS:
 *   - Marginalized populations and Global South communities: primary beneficiaries of near-term-harms governance, claiming accountability for documented algorithmic discrimination and environmental/data extraction harms.
 *   - Technology companies and AI labs: structural payers and agenda-setters, bearing compliance costs and regulatory constraints if near-term-harms governance is enforced; benefit from existential-risk framing that defers accountability.
 *   - Existential-risk researchers: secondary beneficiary seat, career-incentive-aligned to existential-risk framing; not directly harmed by near-term-harms governance but deprioritized.
 *   - Civil society advocates, fairness researchers, regulation authorities: observer and agenda-setter seats with competing pressures and finite resources.
 *   - AI development teams: constrained payer seat, bearing immediate operational friction from bias auditing and transparency requirements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.72).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance: Near-Term Harms Priority Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "technology_ethics/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '6fb16197-59f0-44a7-b6cc-7b189fbf416c').
narrative_ontology:cs_kernel_codification('6fb16197-59f0-44a7-b6cc-7b189fbf416c', distributed).
narrative_ontology:cs_authority_grounding('6fb16197-59f0-44a7-b6cc-7b189fbf416c', distributed).
narrative_ontology:cs_reading_relation('6fb16197-59f0-44a7-b6cc-7b189fbf416c', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fb16197-59f0-44a7-b6cc-7b189fbf416c', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('6fb16197-59f0-44a7-b6cc-7b189fbf416c', foundational, algorithmic_harms_demonstrable_and_urgent).
narrative_ontology:cs_axiom_status(algorithmic_harms_demonstrable_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('6fb16197-59f0-44a7-b6cc-7b189fbf416c', algorithmic_harms_demonstrable_and_urgent, empirically_contingent).
narrative_ontology:cs_axiom('6fb16197-59f0-44a7-b6cc-7b189fbf416c', foundational, marginalized_populations_primary_governance_claimants).
narrative_ontology:cs_axiom_status(marginalized_populations_primary_governance_claimants, holdable).
narrative_ontology:cs_axiom_grounding('6fb16197-59f0-44a7-b6cc-7b189fbf416c', marginalized_populations_primary_governance_claimants, deontological).
narrative_ontology:cs_reference_frame('6fb16197-59f0-44a7-b6cc-7b189fbf416c', documented_algorithmic_discrimination_baseline).
narrative_ontology:cs_drift_state('6fb16197-59f0-44a7-b6cc-7b189fbf416c', contemporary_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6fb16197-59f0-44a7-b6cc-7b189fbf416c', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, global_south_communities).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, surveillance_targets).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, ai_development_teams).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, existential_risk_researchers).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, algorithmic_harms_are_demonstrable_and_urgent).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__near_term_harms_reading, regulatory_attention_is_finite_and_contested).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face demonstrable algorithmic discrimination in hiring, lending, criminal justice, housing, and content moderation systems. Their harms are immediate and empirically documented — facial recognition errors disproportionately flag darker skin tones; credit algorithms charge higher rates to applicants from redlined neighborhoods; content moderation removes their speech while amplifying harmful stereotypes. A governance framework prioritizing near-term harms names them as primary claimants and directs mitigation resources (bias audits, algorithmic transparency, regulatory enforcement) toward their relief. They have minimal power to demand this priority and cannot exit these systems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations, beneficiary,
    powerless, immediate, trapped, global).

% Experience documented job displacement and wage suppression from automation and AI deployment in warehousing, customer service, transportation, and knowledge work. A near-term-harms framework legitimates regulatory requirements for retraining support, transition assistance, and wage floors; an existential-risk framing defers these concerns as speculative compared to hypothetical superintelligence. They can organize collective voice but face capital mobility and global labor arbitrage.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, displaced_workers, beneficiary,
    moderate, biographical, constrained, global).

% Bear acute harms from AI training data extraction, surveillance technology deployment, and resource-intensive AI compute infrastructure (water use, rare earth mining, carbon emissions) sited in their territories. They are data providers and environmental hosts but have minimal say in governance frameworks that set AI safety priorities. A near-term-harms reading centers their experiences; existential-risk framing globalizes the concern into hypothetical future superintelligence that abstract everyone equally.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_communities, beneficiary,
    powerless, biographical, trapped, global).

% Face pervasive real-time surveillance via facial recognition, behavioral tracking, and predictive policing systems. Their harms are documented and ongoing: wrongful arrests from misidentified faces, police harassment from risk-scoring algorithms, loss of anonymity and freedom of movement. Near-term governance prioritizes algorithmic transparency, audit requirements, and limits on deployment in sensitive contexts (law enforcement, immigration). Existential-risk framing treats these as smaller concerns than hypothetical future AI scenarios.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, surveillance_targets, beneficiary,
    powerless, immediate, trapped, global).

% Deploy AI systems at scale and benefit from governance frameworks that defer accountability for present harms to focus on speculative long-term risks. A near-term-harms governance priority imposes immediate costs: bias audits (time and engineering resources), transparency requirements (revealing proprietary training data and model architecture), regulatory compliance (operating constraints, potential fines or system redesigns), and liability exposure (they become responsible for documented algorithmic discrimination). They have resources to shape governance discourse and can move operations to less-regulated jurisdictions. They actively fund and promote existential-risk research and framing because it shifts the risk narrative away from their current deployment practices.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, technology_companies, agenda_setter).

% Face immediate constraints if near-term-harms governance is enforced: model development slows for bias testing and mitigation, release cycles extend for safety review, resource allocation shifts toward fairness work rather than capability improvement. They are employees of technology companies and have less direct power to set priorities than executives, but can organize through professional associations and research communities. An existential-risk framing allows them to frame their work as aligned with global safety priorities rather than as constrained by local discrimination problems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_development_teams, payer,
    organized, biographical, constrained, global).

% Their research agendas and funding streams benefit from governance frameworks that elevate existential risk as a primary concern. A near-term-harms reading does not foreclose their research but reorders its priority relative to immediate algorithmic discrimination and labor displacement. They are academic and policy researchers with career incentives aligned to the existential-risk framing; they can produce competing analyses and secure funding from organizations (AI safety nonprofits, effective altruism networks) that back existential-risk work.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, existential_risk_researchers, observer,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, existential_risk_researchers, beneficiary).

% Their research and funding are legitimated by a near-term-harms priority in AI governance. They study documented algorithmic discrimination, bias mitigation techniques, and accountability mechanisms. They have institutional bases in computer science and social science, publish in peer review, and advise policymakers. Their work is not eliminated by existential-risk governance but is deprioritized in funding and policy attention.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, fairness_and_bias_researchers, observer,
    organized, biographical, mobile, global).

% Hold authority to set AI governance priorities through legislation, regulation, and enforcement. They face competing claims: near-term-harms advocates demand immediate accountability for documented algorithmic discrimination; existential-risk advocates demand preventive measures against speculative superintelligence. Regulatory resources are finite; prioritizing one reading crowds out the other. They have limited expertise in highly technical AI questions and depend on commissioned research and stakeholder testimony — creating opportunities for actors with resources to shape the framing.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, regulation_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Represent marginalized communities and displaced workers in governance processes. They produce documentation of algorithmic harms, testify before regulators, and organize public pressure. They have limited funding and expertise relative to technology companies and existential-risk organizations, which creates asymmetry in the governance conversation. A near-term-harms governance priority validates their claims; existential-risk framing marginalizes them as parochial compared to concerns about superintelligence.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, civil_society_advocates, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, regulation_authorities).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a governance prioritization framework that directs finite regulatory, research, and corporate resources toward documented algorithmic harms affecting vulnerable populations in the near term (bias mitigation, transparency requirements, algorithmic auditing, worker transition support) rather than speculative long-term existential risk scenarios.
% TRANSFER_FUNCTION: Transfers regulatory attention, research funding, and corporate compliance burden from existential-risk mitigation toward near-term-harms mitigation; transfers the perceived legitimacy of 'AI safety' from abstract future concerns toward concrete present accountability; transfers the framing power to define AI governance from technology companies and existential-risk researchers toward civil society advocates and affected populations.
% ABSENT_VOICES: Existential-risk researchers and technology companies that benefit from x-risk framing would object to the priority if present in governance spaces; they are not excluded by formal rule but their positions are subordinated in this reading's framework, creating incentive to delegitimize near-term-harms framing or redefine it as compatible with existential-risk priorities (the 'bridge reading' strategy).
% DISAPPEARANCE_RATIONALE: If this governance priority disappeared, regulatory frameworks would shift toward existential-risk prevention (compute governance, model evaluation standards, AI pause scenarios), corporate resources would reallocate toward safety research rather than bias auditing, and immediate harms to marginalized populations would persist unchecked — the world does not return to a neutral state but to one where existential-risk framing captures governance.
% FOUNDING_PROBLEM: Documented algorithmic harms (facial recognition errors, biased lending, discriminatory hiring systems, surveillance targeting) are causing measurable, real-time injury to marginalized populations, particularly in the Global South and among Black, Indigenous, and low-income communities. These harms are empirically demonstrated, not speculative, and require immediate governance response.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations (ACLU, Electronic Frontier Foundation, Center for AI Safety and Accountability), affected communities, academic researchers in fairness and bias, labor unions, and regulatory bodies (FTC, EU AI Act drafters) attest that algorithmic discrimination is ongoing and urgent. Testimonies from marginalized communities describe lived harms from these systems. Independent audits and peer-reviewed research document bias in widely deployed systems. This corroboration comes from outside the near-term-harms reading's direct beneficiaries — it originates from investigators, advocates, and regulators examining the constraint from other seats.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.68 at interval end, reflecting the high structural cost imposed on technology companies and AI labs by a governance framework that mandates accountability for documented harms. The measurement series shows steady increase from 0.45 to 0.68 (t=0 to t=25), indicating accumulating enforcement pressure as near-term-harms frameworks gain legitimacy through regulatory adoption (EU AI Act, proposed US guardrails, corporate bias audits). Suppression is high (0.72) because the constraint's persistence requires active counter-narrative work to delegitimize existential-risk framing or redefine it as compatible with near-term-harms concerns. Theater ratio is moderate (0.44 at endpoint), indicating a mix of genuine bias mitigation work and performative compliance by companies seeking to appear responsive while minimizing substantive changes. Accessibility collapse is low (0.51) because alternative governance framings (existential-risk, bridge reading) remain live options that actors can choose; marginalized populations cannot easily access a 'no governance' state but can be offered a choice between readings. Resistance is high (0.73) because existential-risk researchers, technology companies, and actors benefiting from x-risk narrative actively resist the near-term-harms priority through funded counter-research, op-eds, policy advocacy, and reframing efforts ('x-risk is urgent AND we should care about near-term harms').
 *
 * PERSPECTIVAL GAP:
 *   The structural divergence is between the beneficiary seats (marginalized populations, displaced workers) and the payer seats (technology companies, AI labs). From the beneficiary perspective, the constraint's operation is life-and-death accountability for documented discrimination — near-term governance that finally centers their experiences. From the payer perspective, the constraint's operation is unfair deprioritization of existential risks that affect all of humanity equally, and a diversion of corporate and research resources toward parochial concerns about today's algorithmic flaws rather than long-term superintelligence prevention. The engine will compute these seats differently: beneficiaries show low directionality (the constraint subsidizes their recognition and directs resources to their relief), payers show high directionality (the constraint imposes costs and constraints on their operations). The existential-risk researcher seat sits between: they do not directly pay or benefit in material terms, but their career incentives and funding streams are deeply aligned to existential-risk framing — a secondary extraction mechanism that does not involve direct transfer of money but reallocates prestige, publication venues, and research agendas.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations and displaced workers have d near 0 (full beneficiary end): the constraint directly names their harms as primary, legitimates their claims, and directs governance resources toward their relief. They have trapped/identity-locked exit (they cannot opt out of algorithmic systems, their grievances are not fungible with other concerns). Technology companies have d near 1 (full target end): the constraint imposes immediate costs (bias auditing, transparency, operational constraints) and reduces their ability to define AI safety in their own terms. They have arbitrage exit (they can shift operations to less-regulated jurisdictions, migrate computing to different regulatory zones, reframe near-term harms as acceptable tradeoffs). Displaced workers have d near 0.6–0.7 (primarily target, but some offsetting benefit): they benefit from transition assistance and regulatory protections named by the constraint, but also bear the cost that existential-risk framing dominates policy attention and they compete with superintelligence prevention for finite resources. AI development teams have d near 0.8 (primarily target): they bear the operational friction of bias auditing and longer development cycles, but lack the power to exit (they are employees) or to arbitrage (professional reputation is geographically diffuse but tied to specific companies/research groups). Regulation authorities have d near 0.4–0.5 (slightly beneficiary-tilted): they gain authority to set governance priorities (the constraint empowers their decision-making) but are constrained by pressure from existential-risk advocates and limited expertise in evaluating technical AI claims. Civil society advocates have d near 0 (beneficiary): they gain legitimacy and influence if near-term-harms governance is adopted.
 *
 * MANDATROPHY ANALYSIS:
 *   The foundational mandate of this constraint — to establish governance frameworks that prioritize documented algorithmic harms to marginalized populations — remains live and uncontradicted. The tension is not between the mandate and its function, but between this mandate and a competing mandate from the existential-risk reading (prevent superintelligence). Both are live governance mandates; the constraint's operation extracts from actors who benefit from existential-risk framing to transfer legitimacy and resources to near-term-harms actors. Mandatrophy does not apply here because the mandate has not outlived its function; rather, the constraint's persistence depends on active contestation with a competing mandate. This is a tangled-rope constraint (coordination + extraction) because it does solve a genuine governance problem (how to allocate finite regulatory resources among AI risks) and provides benefit to the beneficiary seats (recognition, resource direction), while simultaneously imposing asymmetric costs on payer seats who would prefer existential-risk framing. The constraint's classification as tangled-rope rather than snare depends on whether the coordination function is genuine: does the near-term-harms governance framework solve a real problem that beneficiary seats have? Yes — documented algorithmic discrimination is a genuine, demonstrable problem that has no solution without governance intervention. Does it solve it in a way that extracts from payer seats for the solution's sake? Yes — technology companies could implement bias mitigation and algorithmic transparency entirely through corporate responsibility without governance mandate, but the constraint's operation ensures they do it under threat of regulation rather than choice, extracting both legitimacy from existential-risk framing and operational concessions from governance pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_tradeoff,
    'Are near-term-harms governance and existential-risk governance genuinely zero-sum in resource allocation, or can both be pursued within a single governance framework without competitive deprioritization?',
    'Longitudinal tracking of research funding, regulatory budgets, and corporate compliance spending over a decade-long window — does dedicating resources to bias auditing crowd out existential-risk research, or do both grow in parallel (evidence of bridge-reading viability)?',
    'If genuinely zero-sum: near-term-harms governance extracts from existential-risk actors and constrains long-term risk mitigation for the sake of immediate accountability. If complementary: both readings are compatible and the constraint operates as pure coordination with asymmetric benefit distribution (not extraction). If partially complementary: the constraint is tangled-rope with asymmetry in unknowable proportions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_tradeoff, empirical, 'Whether near-term and existential-risk governance compete for the same finite resources or can coexist within expanded governance capacity.').

omega_variable(
    existential_risk_specification,
    'What specific superintelligence scenario does the existential-risk reading treat as the primary governance target, and how does it justify the claim that this scenario is more urgent than documented present harms?',
    'Examination of existential-risk advocates'' actual threat models, time horizons, and probability estimates — do they specify the scenario clearly, or do they treat superintelligence as an abstract catch-all category? Do they provide empirical grounds for >probability weighting relative to documented algorithmic discrimination with measured base rates?',
    'If existential-risk scenarios are specified and empirically grounded as more probable than present harms: the existential-risk reading claims legitimacy on the same terrain as near-term-harms (demonstrated probability). If existential-risk scenarios are abstract or speculative without clear specification: the existential-risk reading''s claim to priority rests on moral weight (potential severity if it occurs) rather than likelihood, which is a different type of claim and affects how governance should weigh it. This omega documents the incommensurability between the readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(existential_risk_specification, conceptual, 'Whether existential-risk claims meet the same evidentiary standard as near-term-harms claims or rest on different epistemic grounds.').

omega_variable(
    governance_priority_is_zero_sum_by_definition,
    'Does the kernel ''ai_risk_governance_priority'' logically entail that only one reading can be elevated to governance priority status, or is the kernel''s legitimacy claim compatible with holding multiple readings as simultaneous priorities?',
    'Semantic and logical analysis of what ''priority'' means in governance contexts — if priority means ''most important focus,'' then only one reading can have priority and the readings are zero-sum. If priority means ''significant legitimate claim on governance attention,'' then multiple readings can have priority in parallel, and the readings coexist rather than compete.',
    'If zero-sum by definition: the constraint''s operation necessarily extracts from the non-prioritized reading (existential-risk research is subordinated). If compatible with multiple priorities: the constraint''s operation is coordinate-like (allocates governance roles to different readings rather than suppressing one). This omega documents whether the readings are logically competitors or merely differently-weighted options within a unified framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governance_priority_is_zero_sum_by_definition, conceptual, 'Whether the kernel''s structure logically entails zero-sum competition between readings or permits parallel prioritization.').

omega_variable(
    technology_company_benefit_from_xrisk_framing,
    'Do technology companies and AI labs actually benefit materially from existential-risk framing being elevated in governance, or is this an inferred structural relationship rather than a directly observable incentive alignment?',
    'Analysis of corporate resource flows — do companies that promote existential-risk work receive regulatory deferral or lighter oversight in near-term-harms domains? Do compliance costs for near-term-harms governance decrease relative to existential-risk research budget increases at the same company?',
    'If companies directly benefit: the technology-company payer seat''s d value is correct (they have strong incentive to suppress near-term-harms framing). If benefit is structural but not direct: the extraction mechanism is more subtle (legitimacy competition rather than direct cost transfer) and the tangled-rope classification may overstate the payer''s structural capture. This omega documents whether the inferred beneficiary alignment is empirically grounded or inferred from position alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_company_benefit_from_xrisk_framing, empirical, 'Whether technology companies materially benefit from existential-risk framing or only structurally align with it.').

omega_variable(
    marginalized_population_voice_in_governance,
    'Are marginalized populations whose harms are centered in this reading actually seated at the governance table where near-term-harms framing is debated and prioritized, or is their voice represented indirectly through civil society advocates and researchers?',
    'Audit of governance spaces (regulatory bodies, policy commissions, corporate ethics boards) — count and power-level of seats directly held by affected populations versus advocacy-mediated seats. Track whether governance decisions change when affected communities have direct voice.',
    'If marginalized populations lack direct seat: the constraint''s operation benefits them (names their harms as central) but may not amplify their agency (they remain objects of governance rather than subjects). If they hold direct seats with power: the constraint operates as coordinator of their interests at the governance table. This omega documents whether the constraint''s beneficiary structure reflects genuine empowerment or benevolent paternalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_population_voice_in_governance, empirical, 'Whether the near-term-harms governance framework is co-designed with affected populations or designed for them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(ai_r_tr_t5, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(ai_r_tr_t15, observed).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(ai_r_tr_t20, projected).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(ai_r_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(ai_r_be_t5, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(ai_r_be_t15, observed).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_r_be_t20, projected).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ai_r_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(ai_r_su_t5, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(ai_r_su_t15, observed).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(ai_r_su_t20, projected).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(ai_r_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__near_term_harms_reading, 0.18).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'ai_risk_governance_priority'. The near-term-harms reading (this story) prioritizes documented algorithmic discrimination and labor displacement affecting marginalized populations. The existential-risk reading prioritizes superintelligence prevention. The bridge reading treats both as non-mutually-exclusive concerns. These are not alternative measurements of a single constraint; they are three structurally distinct constraints grounded in the same kernel but implementing different victim/beneficiary structures and resource allocations. The ε-invariance principle applies: each reading has its own ε (this reading: high ε on present deployment harms, low ε on speculative superintelligence). Network links track the structural influence and contention between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, organized, 0.72).
constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
