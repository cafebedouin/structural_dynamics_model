% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__near_term_harms_reading, []).

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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety as Prevention of Documented Present-Day Harms
 *   domain: technology_governance/labor/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested 'AI
 *   safety' kernel: safety is defined as the prevention of documented,
 *   measurable harms already occurring from deployed systems — biased hiring
 *   and lending algorithms, discriminatory predictive policing, exploitative
 *   content moderation labor, and misinformation amplification. This reading
 *   generates a genuine coordination function (giving regulators, litigants,
 *   and journalists a shared evidentiary standard) but is authored here as
 *   substantially extractive because the frame is disproportionately set and
 *   funded by the same frontier labs whose products cause the harms, and
 *   because the categories of harm that get audited are the ones most legible
 *   and least threatening to lab business models — model-level bias metrics
 *   rather than labor conditions in the moderation supply chain, or novel
 *   harm categories not yet studied.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: agenda_setter/beneficiary (institutional/arbitrage) — funds and shapes what counts as documented harm
 *   - enterprise_ai_deployers: beneficiary (powerful/mobile) — shields liability behind vendor safety claims
 *   - algorithmically_screened_job_applicants: payer (powerless/trapped) — bears opaque automated screening decisions
 *   - content_moderation_gig_workers: payer (powerless/trapped) — performs the actual safety labor invisibly
 *   - communities_subject_to_predictive_policing: payer (powerless/trapped) — bears compounding generational harm
 *   - civil_rights_and_labor_advocacy_groups: excluded (organized/constrained) — documents harm but lacks a seat at the standard-setting table
 *   - financial_and_technology_regulators: observer (institutional/analytical) — enforces after the fact, lagging deployment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.58).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Prevention of Documented Present-Day Harms").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technology_governance/labor/civil_rights").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '024fca3a-21ea-420c-a3b6-9d289c7ea0fa').
narrative_ontology:cs_kernel_codification('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', distributed).
narrative_ontology:cs_authority_grounding('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', distributed).
narrative_ontology:cs_reading_relation('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', foundational, documented_harm_is_the_only_legitimate_safety_standard).
narrative_ontology:cs_axiom_status(documented_harm_is_the_only_legitimate_safety_standard, holdable).
narrative_ontology:cs_axiom_grounding('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', documented_harm_is_the_only_legitimate_safety_standard, empirically_contingent).
narrative_ontology:cs_axiom('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', secondary, speculative_future_harm_should_not_displace_present_remediation_resources).
narrative_ontology:cs_axiom_status(speculative_future_harm_should_not_displace_present_remediation_resources, holdable).
narrative_ontology:cs_axiom_grounding('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', speculative_future_harm_should_not_displace_present_remediation_resources, instrumental).
narrative_ontology:cs_reference_frame('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', harm_must_be_empirically_documented_to_warrant_governance_action).
narrative_ontology:cs_drift_state('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', post_generative_ai_deployment_surge, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('024fca3a-21ea-420c-a3b6-9d289c7ea0fa', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, enterprise_ai_deployers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, algorithmically_screened_job_applicants).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, content_moderation_gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, communities_subject_to_predictive_policing).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, credit_and_housing_applicants_of_color).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, measurable_harm_standard_for_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish safety commitments and fund fairness/bias research programs that are cited as evidence of responsible conduct. Set the terms of what counts as a 'documented harm' worth fixing, funding audits of the categories most legible to press and regulators while resisting binding transparency requirements on model internals, training data provenance, and deployment-scale labor practices. Control the pace and scope of what gets called an AI safety problem.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, frontier_ai_labs, beneficiary).

% License and deploy screening, scoring, and moderation systems built by frontier labs, absorbing liability minimization language ('the vendor's model is safety-tested') as a shield against downstream discrimination claims. Free to switch vendors or jurisdictions if scrutiny rises; the harms produced by their deployments are diffused into vendor-audit paperwork rather than traced back to deployment decisions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, enterprise_ai_deployers, beneficiary,
    powerful, biographical, mobile, global).

% Are filtered, ranked, or rejected by automated hiring and credit systems whose decision logic is proprietary. Cannot see why they were screened out, cannot appeal to a human in most cases, and cannot avoid the systems because they are embedded across the labor and credit market they depend on. Their harm is well-documented in audit studies but rarely remediated individually.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, algorithmically_screened_job_applicants, payer,
    powerless, biographical, trapped, national).

% Perform the human labeling and moderation labor that makes deployed models appear safe — reviewing traumatic content at high volume for low wages, often through subcontracted labor markets in the Global South, with minimal mental health support. Their labor is the actual mechanism by which 'safety' is produced day to day, yet they are structurally invisible in safety commitments that foreground model-level metrics.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, content_moderation_gig_workers, payer,
    powerless, immediate, trapped, global).

% Live in neighborhoods where predictive policing and risk-scoring tools trained on historically biased arrest data concentrate enforcement, reproducing the disparities in the training data as forward-looking predictions. Cannot opt out of geographic jurisdiction; harms compound across generations through arrest records and surveillance exposure.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, communities_subject_to_predictive_policing, payer,
    powerless, generational, trapped, regional).

% Face disparate outcomes from automated underwriting and tenant-screening algorithms that encode historical discrimination as statistical pattern. Some redress exists through fair-lending litigation, but the burden of proving algorithmic discrimination is high and the systems change faster than case law can track.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, credit_and_housing_applicants_of_color, payer,
    powerless, biographical, constrained, national).

% Document algorithmic discrimination and labor exploitation through litigation, journalism, and worker organizing, but are structurally outside the rooms where frontier labs and regulators negotiate voluntary safety commitments and model evaluation frameworks. Their evidence informs public discourse but rarely gates deployment decisions directly.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, civil_rights_and_labor_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Investigate disparate-impact complaints and draft rules for algorithmic accountability, but operate with technical capacity and legal authority that lags deployment speed. Can compel audits and impose fines but rarely mandate structural changes to underlying systems before harm has already accrued to large populations.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, financial_and_technology_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates safety effort on harms that are already measurable, documented, and politically legible — bias audits, content moderation, disclosure of known failure modes — which genuinely reduces some identifiable present-day injuries and gives affected parties a vocabulary and evidentiary basis to contest specific deployments.
% TRANSFER_FUNCTION: Moves regulatory and reputational risk away from frontier labs and enterprise deployers (who fund the audits that define the frame) and onto powerless populations who bear undocumented, novel, or hard-to-litigate harms that fall outside the audited categories; moves real safety labor onto low-wage moderation workers while crediting model-level 'safety' to the lab.
% ABSENT_VOICES: Content moderation workers and communities harmed by predictive policing rarely have seats in the standard-setting bodies, AI safety institutes, or voluntary commitment frameworks that define which harms count as in-scope; their testimony appears mainly through investigative journalism and litigation discovery, after deployment, not before.
% DISAPPEARANCE_RATIONALE: If the near-term-harms framing of AI safety disappeared as an organizing commitment, existing bias audits, disclosure requirements, and labor-condition scrutiny of moderation supply chains would lose their primary institutional vocabulary and funding rationale; enforcement agencies, journalists, and litigants who currently cite 'AI safety' commitments as evidentiary benchmarks would lose a reference point, and some documented harms currently subject to at least partial remediation would go fully unaddressed.
% FOUNDING_PROBLEM: Deployed algorithmic systems were producing measurable, well-documented harms — biased hiring and lending decisions, discriminatory predictive policing, exploitative moderation labor, and misinformation amplification — well before speculative long-horizon AI risk entered mainstream policy discourse, and needed a governance vocabulary and evidentiary standard distinct from abstract alignment research.
% FOUNDING_PROBLEM_CORROBORATION: Independent audit studies (e.g. algorithmic hiring bias research, fair-lending disparate-impact litigation, investigative reporting on outsourced content moderation labor conditions) and regulatory enforcement actions from bodies not funded by the labs corroborate that these harms are ongoing and measurable; this status is not solely attested by the AI labs whose commitments are being evaluated.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.48 to 0.68) as the near-term-harms frame becomes institutionalized into voluntary industry commitments that substitute for binding regulation — the frame's authority grows even as the underlying harms (documented in audit studies) persist largely unremediated. Theater ratio rises in parallel (0.38 to 0.61) reflecting increasing investment in visible compliance artifacts — published bias audits, safety reports, red-teaming exercises — relative to structural changes in deployment practices or labor conditions. Suppression is authored moderately (0.58 at endpoint) because affected populations are not directly coerced into silence, but their exit options are structurally foreclosed: they cannot opt out of algorithmic hiring markets, predictive policing jurisdictions, or the outsourced labor markets that need moderation work. Accessibility collapse is moderate (0.42) — some alternatives exist (litigation, journalism, worker organizing) but are resource-intensive and lag deployment. Resistance is present and organized (0.55) through advocacy groups and litigation, distinguishing this from a pure snare with no counter-pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs sit at the beneficiary end: they set the terms of what harm categories get audited, fund the research that defines 'responsible AI,' and use voluntary commitments to preempt binding regulation — d near 0. Enterprise deployers benefit similarly through liability-shielding, with easy exit via vendor switching. The four victim groups are all powerless with trapped or constrained exit: job applicants cannot avoid automated screening embedded across the labor market; moderation workers depend on the gig income and often work through opaque subcontracting chains; policed communities cannot exit their jurisdiction; credit/housing applicants face high evidentiary burdens to contest algorithmic decisions. Their d values sit near the full-target end — trapped exit combined with victim declaration amplifies effective extraction under the engine's directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The near-term-harms reading resolves an important mandatrophy risk in the opposite direction from its sibling readings: it prevents 'AI safety' from being captured entirely by speculative long-horizon alignment research that could indefinitely defer accountability for present harms. But its own founding problem (measurable present-day harm) remains live and corroborated by independent evidence — so the mandatrophy question here is not whether the mandate has become obsolete, but whether the INSTITUTION built to address it (voluntary industry-defined safety commitments) has been captured by the very actors it was meant to constrain. The founding problem is live; the mechanism addressing it (lab-defined audit categories) shows signs of capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_near_term_vs_existential,
    'Is ''AI safety'' properly understood as the near-term-harms reading instantiated here, or does prioritizing documented present harms structurally starve resources and attention from the existential-risk reading (or vice versa)?',
    'Track resource allocation, research funding, and policy attention across both framings over time; if near-term-harms funding demonstrably displaces x-risk research funding (or vice versa) rather than being additive, the readings are in tension rather than merely coexisting.',
    'If the readings are genuinely in zero-sum resource competition, this reading''s coordination function is partly achieved at the sibling reading''s expense, which would change the sibling''s effective extractiveness rather than this constraint''s.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_near_term_vs_existential, conceptual, 'Whether near-term and existential-risk readings compete for scarce institutional attention or are genuinely additive.').

omega_variable(
    audit_category_capture,
    'Are the harm categories currently audited (hiring bias, benchmark fairness metrics) selected because they are the most tractable to measure, or because they are the least threatening to deployer business models relative to categories like labor exploitation in the moderation supply chain?',
    'Compare the resourcing and institutional prominence given to model-level bias audits versus investigations of moderation labor conditions and novel/emerging harm categories not yet studied; a persistent resourcing gap despite comparable harm severity would indicate selection by tractability-to-industry rather than severity-to-victim.',
    'If audit category selection is captured by industry convenience, the coordination function of this reading is substantially performative for the excluded harm categories (moderation labor, novel discrimination vectors), raising effective extraction and theater ratio further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_category_capture, empirical, 'Whether harm-category selection tracks measurability/industry convenience over victim severity.').

omega_variable(
    voluntary_commitment_enforceability,
    'Do the voluntary safety commitments made by frontier labs under this reading have any binding enforcement mechanism, or do they function as a substitute for regulation that forecloses binding alternatives?',
    'Track instances where labs failed to meet published safety commitments and assess whether any consequence followed beyond reputational cost; also track whether the existence of voluntary commitments was cited by regulators or legislators as a reason to delay binding rules.',
    'If voluntary commitments substitute for and delay binding regulation with no enforcement teeth, this reading functions closer to snare (extraction disguised as coordination) than tangled_rope (genuine coordination alongside extraction); if commitments meaningfully complement binding rules, the tangled_rope classification is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_commitment_enforceability, empirical, 'Whether voluntary safety commitments substitute for or complement binding regulatory enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.44).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__near_term_harms_reading, theater_ratio, 8, 0.49).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__near_term_harms_reading, theater_ratio, 12, 0.53).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__near_term_harms_reading, theater_ratio, 16, 0.57).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__near_term_harms_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__near_term_harms_reading, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(ai_s_su_t16, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(ai_s_su_t24, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the 'ai_safety_commitment' kernel. near_term_harms_reading (this file) claims tangled_rope with high ε concentrated on documented present-day harm categories (bias, discrimination, labor exploitation, misinformation) and identifiable powerless victim groups. existential_risk_reading addresses a structurally distinct claim (extinction-level outcomes from misaligned superintelligent systems) with different victim/beneficiary structure and different ε — that story is NOT an alternative measurement of this one; it is a different constraint entirely per the ε-invariance principle, since the two claims have non-overlapping evidentiary bases, time horizons, and contested-vs-documented status. dual_priority_reading treats the two as non-competing and should be read as a claim about the relationship between the other two, not a synthesis that overrides either's ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
