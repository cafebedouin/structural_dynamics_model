% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term AI Harms Prioritization Framework
 *   domain: technology/governance/justice
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel about AI
 *   risk prioritization. The near-term-harms reading frames AI risk as
 *   primarily the measurable discrimination, worker displacement, and
 *   surveillance enabled by deployed systems NOW. It positions justice
 *   interventions—auditing, transparency, regulation, worker protection—as
 *   paramount. The sibling reading (existential_risk_reading) frames AI risk
 *   as primarily existential misalignment and argues that alignment research
 *   is paramount. These are NOT the same constraint viewed from different
 *   angles; they have different victim sets, different resource-allocation
 *   implications, different timescales, and different suppression mechanisms
 *   (near-term harms are suppressed by framing existential risk as
 *   distraction; existential risk is suppressed by framing near-term fixes as
 *   locally adequate). The constraint is CLAIMED as Tangled Rope because it
 *   coordinates fairness-accountability and worker-protection researchers
 *   around a shared agenda WHILE extracting from racialized, low-wage, and
 *   surveilled populations by making their harms a policy object without
 *   adequately resourcing remediation. The theater_ratio rises over the
 *   interval because compliance with audits and transparency measures grows
 *   performative—companies conduct bias audits to deflect existential-risk
 *   demands while actual harms persist.
 *
 * KEY AGENTS:
 *   - marginalized_communities: beneficiary under the near-term frame; victim of continued harms despite named priority
 *   - fairness_accountability_researchers: beneficiary; benefit from funding, legitimacy, policy influence; secondary agenda-setter role
 *   - racialized_populations: payer; targeted by discriminatory systems; trapped without algorithmic alternatives
 *   - low_wage_workers: payer; displaced by automation; retraining under-resourced
 *   - surveilled_populations: payer; privacy eroded; regulatory remedies slow
 *   - ai_deployment_companies: agenda-setter; enforce compliance with audits to avoid existential-risk demands
 *   - existential_risk_researchers: excluded; delegitimized as speculative; their work is suppressed by the near-term frame
 *   - worker_protection_advocates: beneficiary; labor-justice reading legitimizes their agenda
 *   - surveillance_regulation_advocates: beneficiary; civil-rights reading elevates their priority
 *   - regulatory_authorities: observer; tasked with implementing near-term-harms interventions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.72).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term AI Harms Prioritization Framework").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology/governance/justice").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '75d433ac-c958-440e-aeca-b3257ca4c49f').
narrative_ontology:cs_kernel_codification('75d433ac-c958-440e-aeca-b3257ca4c49f', distributed).
narrative_ontology:cs_authority_grounding('75d433ac-c958-440e-aeca-b3257ca4c49f', distributed).
narrative_ontology:cs_reading_relation('75d433ac-c958-440e-aeca-b3257ca4c49f', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('75d433ac-c958-440e-aeca-b3257ca4c49f', foundational, present_measurable_harms_require_urgent_intervention).
narrative_ontology:cs_axiom_status(present_measurable_harms_require_urgent_intervention, holdable).
narrative_ontology:cs_axiom_grounding('75d433ac-c958-440e-aeca-b3257ca4c49f', present_measurable_harms_require_urgent_intervention, deontological).
narrative_ontology:cs_axiom('75d433ac-c958-440e-aeca-b3257ca4c49f', foundational, justice_to_present_marginalized_populations_is_paramount).
narrative_ontology:cs_axiom_status(justice_to_present_marginalized_populations_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('75d433ac-c958-440e-aeca-b3257ca4c49f', justice_to_present_marginalized_populations_is_paramount, deontological).
narrative_ontology:cs_axiom('75d433ac-c958-440e-aeca-b3257ca4c49f', secondary, speculative_future_risks_must_not_defer_present_justice).
narrative_ontology:cs_axiom_status(speculative_future_risks_must_not_defer_present_justice, holdable).
narrative_ontology:cs_axiom_grounding('75d433ac-c958-440e-aeca-b3257ca4c49f', speculative_future_risks_must_not_defer_present_justice, conventional).
narrative_ontology:cs_reference_frame('75d433ac-c958-440e-aeca-b3257ca4c49f', deployed_ai_systems_causing_measurable_present_harms).
narrative_ontology:cs_drift_state('75d433ac-c958-440e-aeca-b3257ca4c49f', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('75d433ac-c958-440e-aeca-b3257ca4c49f', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, racialized_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, low_wage_workers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, surveilled_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, worker_protection_advocates).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, surveillance_regulation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to deployed AI systems that make discriminatory credit, hiring, and policing decisions. Lack of accessible remedies, algorithmic opacity, and diffuse attribution mean harms accumulate without accountability. They benefit from prioritization of near-term harms because it directs resources to auditing, regulation, and remediation of systems affecting them now. Exit from algorithmic decision-making is structurally impossible—the systems govern access to employment, housing, credit, and state services.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_communities, beneficiary,
    powerless, immediate, trapped, global).

% Academic and advocacy researchers who study and publish on algorithmic bias, transparency, and accountability. The near-term harms reading legitimizes their research agenda, funds their work through fairness audits and regulatory consulting, and elevates their voice in policy discourse. They have career incentives aligned with the near-term priority—their expertise is valuable precisely because the constraint's emergence creates demand for bias remediation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, agenda_setter).

% Over-represented as targets of discriminatory algorithmic decisions in hiring, lending, criminal justice, and surveillance. They bear the direct harms of deployed systems: denial of credit, job rejections, over-policing. While the near-term harms frame theoretically prioritizes their protection, enforcement of remedies is weak, disclosure remains obscured, and they lack standing to demand algorithmic audits or changes. They pay in measurable harm now while awaiting the remediation the constraint claims to mandate.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, racialized_populations, payer,
    powerless, immediate, trapped, global).

% Displaced by AI-driven automation in call centers, warehouses, data entry, and service work. Retraining programs funded under near-term harms priority are under-resourced and often mismatch actual job availability. They pay in lost income, job insecurity, and reduced bargaining power. The constraint acknowledges their situation but enforcement mechanisms prioritize audit and transparency over income support or job guarantees.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, low_wage_workers, payer,
    powerless, immediate, trapped, regional).

% Subject to expanded surveillance enabled by deployed AI systems: facial recognition, predictive policing, social-media monitoring, financial transaction analysis. The near-term harms frame names surveillance as a near-term harm and calls for regulation, but surveillance infrastructure is built into law-enforcement and financial systems where regulatory change is slow. They pay in loss of privacy, chilling of expression, and increased state/corporate control while awaiting regulatory remedies that face institutional resistance.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, surveilled_populations, payer,
    powerless, immediate, trapped, global).

% Deploy AI systems that drive discrimination, displacement, and surveillance. The near-term harms frame requires them to fund bias audits, implement transparency measures, and accept regulatory oversight. This imposes compliance costs but is substantially less restrictive than the existential-risk frame, which would demand compute restrictions, capability limitations, and AGI moratoriums. They enforce compliance with near-term harms audits because it shields them from more disruptive intervention. They benefit from the frame by deflecting existential-risk-motivated restraints.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_deployment_companies, agenda_setter,
    powerful, biographical, mobile, global).

% Researchers focused on misalignment, value learning, and long-horizon AI safety. The near-term harms reading positions their work as speculative and distracting from urgent present harms. This exclusion deflects funding, influence, and policy attention away from existential-risk mitigation. They would argue that near-term harms are inevitable consequences of unaligned systems and that resources should prioritize alignment and safety infrastructure. Their absence from near-term-harms-driven policy spaces is structural to the reading.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, mobile, global).

% Labor organizers and worker-rights researchers who frame AI displacement as a labor-justice issue. The near-term harms reading legitimizes their agenda: mandatory impact assessments, transition support, collective bargaining rights over algorithmic deployment. They benefit from the constraint's framing because it positions worker displacement as a near-term harm requiring immediate intervention, not a long-term labor-market adjustment.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, worker_protection_advocates, beneficiary,
    organized, biographical, mobile, global).

% Privacy advocates and civil-liberties organizations who frame algorithmic surveillance as a near-term civil-rights violation. The near-term harms reading elevates surveillance regulation from privacy-concern to urgent-justice priority. They benefit by securing funding for advocacy, policy expertise, and legislative action on facial recognition bans, predictive-policing prohibition, and data-protection regulation.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, surveillance_regulation_advocates, beneficiary,
    organized, biographical, mobile, national).

% Government agencies tasked with AI governance, labor protection, and civil rights enforcement. The near-term harms reading directs them toward bias audits, algorithmic impact assessments, and fairness regulation. This requires building capacity for technical assessment and oversight. The reading legitimizes immediate regulatory action on deployed systems rather than long-term alignment research.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for identifying, measuring, and remediating harms from deployed AI systems. Coordinates researchers, advocates, companies, and regulators around a common set of metrics (discrimination rates, displacement counts, surveillance scope) and remedial interventions (audits, transparency, regulation). Solves the collective-action problem of scattered, localized harms by aggregating them into a governance agenda.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, and policy action from existential-risk agendas toward near-term-harms agendas. Redirects resources from theoretical alignment research to empirical bias audits, fairness tooling, and worker-protection programs. Shifts deployment companies' compliance effort from long-horizon capability restraint toward present-day transparency and auditing. Moves bargaining power from existential-risk researchers toward fairness researchers and worker advocates.
% ABSENT_VOICES: Existential-risk researchers and long-horizon AI-safety communities are structurally excluded from near-term-harms-driven policy spaces. Future populations who might be affected by misaligned AGI have no seat at the table. Technology workers and AI researchers with capability concerns outside the near-term frame are delegitimized as speculative or indifferent to present suffering. Their absence is structural to the constraint—including them would require shifting resource allocation away from near-term priorities.
% DISAPPEARANCE_RATIONALE: If the near-term-harms prioritization framework disappeared, resource allocation would shift back toward existential-risk alignment research, regulatory focus would move from audits to capability limitation, and companies would face different compliance demands. Worker-protection and civil-liberties organizations would lose policy legitimacy and funding. Marginalized communities would lose the theoretical advocate community that names their harms in governance discourse, though the harms themselves would persist. The governance architecture would reorganize around different risk framings.
% FOUNDING_PROBLEM: Deployed AI systems cause measurable discrimination, displace workers, and enable surveillance of vulnerable populations. Current harms are documented, severe, and distributional—concentrated on marginalized groups. Existing accountability mechanisms are inadequate to the scale and pace of AI deployment. Justice requires immediate intervention on known, present harms rather than theoretical future risks.
% FOUNDING_PROBLEM_CORROBORATION: Documented bias in hiring algorithms (Amazon, LinkedIn), lending discrimination (algorithmic redlining), over-policing through predictive systems (COMPAS, Chicago police), and documented surveillance scale (Clearview AI, Chinese social credit) are attested by independent researchers, civil-rights organizations, and worker-protection advocates operating outside benefiting parties. Labor displacement statistics are public. The problem is live in the sense that these harms continue; it is contested in the sense that existential-risk researchers argue these are inevitable features of unaligned systems and that prioritizing near-term fixes delays more fundamental safety work.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval because deployment of AI systems accelerates faster than audit and remediation capacity. Early in the interval (t=0-5), the near-term-harms reading is newly dominant and resources flow toward fairness research and regulatory infrastructure—extractiveness is moderate. By t=10-15, audits and transparency measures are routine but marginalized populations' actual access to remediation is limited; the constraint increasingly extracts through naming harms without proportional remediation. Theater_ratio rises (0.18 to 0.41) because bias-audit compliance becomes performative—companies conduct audits to satisfy regulatory demand while algorithmic discrimination persists in slightly more obscured forms. Suppression_requirement rises (0.55 to 0.72) because maintaining the near-term frame requires continuous delegitimization of existential-risk research as speculative and distraction from urgent harms. The measurement series tracks one shared time grid so all metrics are valued at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary researchers and advocates compute the constraint as genuine coordination—they have successfully aggregated scattered harms into a governance agenda and secured resources. The payer seats (racialized populations, low-wage workers, surveilled populations) compute it as extraction with limited remediation—their harms are named and studied but actual outcomes (credit access, employment, privacy) remain blocked. The excluded existential-risk seat would compute the constraint as deflection—it redirects resources and legitimacy away from what they believe are more fundamental safety requirements. The deployment companies compute it as acceptable compliance cost—audits are less disruptive than capability limitation or deployment moratoriums. The engine computes per-seat divergence from the structural data; the claimed Tangled Rope is accurate because genuine coordination (fairness research) coexists with asymmetric extraction (named harms without proportional remedy).
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and fairness researchers are named as beneficiaries, but the structural relationship differs: researchers genuinely benefit (funding, legitimacy, career advancement); marginalized communities theoretically benefit (their harms named as priority) but actually remain trapped in algorithmic harms with limited remediation—the benefit is partial and asymmetrically distributed toward the advocacy community. Racialized populations, low-wage workers, and surveilled populations are victims because they bear the harms the constraint claims to address, yet lack leverage to demand remediation and remain trapped in the systems the constraint regulates. The directionality for payer seats is high (near target-end) because they cannot exit algorithmic decision-making. The directionality for beneficiary researchers is low (near beneficiary-end) because they have mobile exit and gain from the constraint's legitimacy. The agenda-setter (deployment companies) sits near the beneficiary end because they benefit from the constraint's deflection of existential-risk demands—a near-term-harms regime is less restrictive than an alignment-based regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deployed AI harms are measurable, present, distributed to marginalized groups) is live in the descriptive sense (harms continue) and contested in the normative sense (existential-risk framing argues these are inevitable features requiring fundamental alignment work, not justice audit). The disappearance verdict is world_rearranges because policy and resource allocation would reorganize if the near-term-harms frame disappeared. The mismatch is minimal here—the frame generates what it claims to (a governance apparatus for near-term harms). The constraint avoids full mandatrophy because its founding problem persists and the constraint continues to address it (audits, regulation, worker protection). The theater_ratio rise suggests mild mandatrophy drift—the constraint's performative components (audits used to deflect rather than remediate) are growing—but the function (aggregating harms into a governance agenda) is not yet dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedy_adequacy_vs_naming,
    'Does the near-term-harms framework adequately resource and enforce remediation of named harms, or does it primarily name harms while compliance enforcement remains weak?',
    'Temporal analysis of audit rates, regulatory enforcement actions, and actual changes in algorithmic outcomes for marginalized populations. Comparison of compliance spending (audits) vs. remediation spending (direct relief, algorithmic change, worker transition support).',
    'If remediation is inadequate despite naming, the constraint extracts asymmetrically—it uses marginalized populations'' harms as a policy object for researcher and advocate careers without proportional relief. If remediation is proportional, the extraction drops and the Tangled Rope reading solidifies as genuine coordination with some asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedy_adequacy_vs_naming, empirical, 'Whether near-term-harms naming translates to proportional remediation or remains performative.').

omega_variable(
    existential_risk_suppression_mechanism,
    'Is existential-risk research genuinely displaced by near-term priorities, or do both research agendas co-develop with different resource pools?',
    'Analysis of funding flows (NIH, NSF, philanthropic grants to alignment vs. fairness), researcher career progression (are fairness researchers elevated faster than alignment researchers?), and policy influence (which research agenda shapes regulation?). Survey of existential-risk researchers on whether near-term framing suppresses their work.',
    'If existential-risk research is substantially suppressed (lower funding growth, lower policy influence, delayed capability research), the near-term frame operates with high suppression—it actively excludes alternatives. If both research agendas grow independently, suppression is lower and the suppression metric should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_risk_suppression_mechanism, empirical, 'Whether near-term prioritization suppresses existential-risk research or both coexist.').

omega_variable(
    kernel_reading_logical_relationship,
    'Is the near-term-harms reading''s core premise logically incompatible with the existential-risk reading''s core premise, or can a single coherent AI-risk framework hold both?',
    'Philosophical analysis: can an AI-safety architecture be designed that (a) prioritizes near-term deployment oversight AND (b) prioritizes long-horizon alignment research? If yes, the readings coexist within a unified framework. If no, one reading''s core premise forecloses the other.',
    'If readings are coexistent-within-unified-framework, the reading_relations in cs_structure should be coexists_with or influences, and both agendas could in principle coordinate. If they are logically incompatible (e.g., near-term prioritization forecloses the long research timeline alignment requires), the relation is forecloses and policy choice is binary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_logical_relationship, conceptual, 'Logical relationship between near-term and existential-risk framings of AI risk.').

omega_variable(
    marginalized_communities_agency_vs_representation,
    'Do marginalized communities have meaningful voice and agency in near-term-harms governance, or are they represented by researchers and advocates who may not track their priorities?',
    'Audit of governance structures: are marginalized-community members seated on audit committees, consulted on remediation priorities, and able to veto or redirect compliance measures? Do researcher priorities (technical bias detection) align with community priorities (income protection, algorithmic contestation)?',
    'If communities have minimal agency, the beneficiary designation is partial—they are named but not empowered. The extraction metric should account for the difference between naming a group as beneficiary and giving them voice in the constraint''s operation. If communities have substantial voice, beneficiary designation is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_communities_agency_vs_representation, empirical, 'Whether marginalized populations have meaningful voice in near-term-harms governance or are represented by external advocates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(ai_r_tr_t5, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(ai_r_tr_t15, observed).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(ai_r_tr_t20, projected).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(ai_r_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ai_r_be_t5, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(ai_r_be_t15, observed).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_r_be_t20, projected).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ai_r_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(ai_r_su_t5, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ai_r_su_t15, observed).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_r_su_t20, projected).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(ai_r_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% The ai_risk_prioritization kernel has two constraint stories with structurally distinct ε values, victim sets, and suppression mechanisms. near_term_harms_reading (this story) frames AI risk as measurable present harms requiring justice interventions; existential_risk_reading frames AI risk as extinction-level misalignment requiring alignment research. These are not the same constraint viewed from different seats—they have different resource implications and different suppression dynamics. Each story independently instantiates the ε-invariance principle: near-term harms has moderate-to-high extraction from present marginalized groups; existential-risk has negligible extraction but faces high suppression via delegitimization-as-speculative. They are linked via network.affects_constraints because policy priority shifted toward one reading constrains the other's resource access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__near_term_harms_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
