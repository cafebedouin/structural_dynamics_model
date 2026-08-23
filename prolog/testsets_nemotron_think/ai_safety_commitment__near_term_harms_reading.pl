% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: AI Safety Commitment: Near-Term Harms Reading
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   The 'near-term harms' reading of AI safety commits the field to
 *   preventing documented present-day harms from deployed systems — bias,
 *   discrimination, labor exploitation, misinformation — as the defining
 *   scope of AI safety work. This reading gained dominance in policy venues
 *   (EU AI Act, US Executive Orders, major philanthropic portfolios) from
 *   roughly 2018 onward. Its coordination function is genuine: it produces
 *   legible, enforceable requirements that address real injuries. Its
 *   extraction function is structural: frontier AI labs and large platforms
 *   advocate for this framing because it directs regulation toward compliance
 *   costs they can absorb (and that raise barriers for smaller competitors)
 *   while deferring regulation on capabilities scaling, compute governance,
 *   and alignment verification for superintelligent systems. The constraint
 *   extracts compliance labor from smaller companies and performative
 *   protection from marginalized populations, while the agenda-setters
 *   capture regulatory legitimacy and avoided frontier regulation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.52).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety Commitment: Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, 'c7cb5fb6-b789-4dcc-abea-9e5a684d2413').
narrative_ontology:cs_kernel_codification('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', distributed).
narrative_ontology:cs_authority_grounding('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', distributed).
narrative_ontology:cs_reading_relation('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', foundational, deployed_systems_are_primary_harm_locus).
narrative_ontology:cs_axiom_status(deployed_systems_are_primary_harm_locus, holdable).
narrative_ontology:cs_axiom_grounding('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', deployed_systems_are_primary_harm_locus, empirically_contingent).
narrative_ontology:cs_axiom('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', secondary, existential_risk_is_speculative_distraction).
narrative_ontology:cs_axiom_status(existential_risk_is_speculative_distraction, holdable).
narrative_ontology:cs_axiom_grounding('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', existential_risk_is_speculative_distraction, empirically_contingent).
narrative_ontology:cs_reference_frame('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', present_harm_centered_ai_safety).
narrative_ontology:cs_drift_state('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', post_llm_deployment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c7cb5fb6-b789-4dcc-abea-9e5a684d2413', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, large_tech_platforms).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, smaller_ai_companies).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_safety_researchers_near_term).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, deployed_systems_cause_measurable_harm).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, algorithmic_bias_is_documentable).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, labor_exploitation_in_ai_supply_chains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead development of frontier models while advocating for AI safety definitions that center near-term harms. This framing directs regulatory attention toward bias audits, watermarking, and content moderation — requirements they can meet with existing infrastructure — while deferring or avoiding regulation on capabilities scaling, compute governance, or alignment verification for superintelligent systems. They fund near-term safety research generously and place alumni in policy roles.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).

% Deploy AI at scale in hiring, lending, content recommendation, and gig-work allocation. The near-term harms reading produces compliance obligations (bias audits, transparency reports, appeal processes) that are costly but manageable for incumbents and raise barriers for smaller competitors. They avoid the existential risk frame which could trigger compute caps, licensing regimes, or liability for model capabilities.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, large_tech_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Experience documented harms from deployed systems: algorithmic denial of benefits, discriminatory hiring filters, predictive policing targeting, healthcare allocation bias. The near-term harms reading names their injuries as the central AI safety problem, but the resulting regulatory frameworks often produce procedural compliance (impact assessments published but unread, appeal processes with no remedies) rather than material redress. They bear the harm whether the constraint functions or performs.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_populations, payer,
    powerless, biographical, trapped, global).

% Subject to algorithmic management that sets pay, assigns work, deactivates accounts, and surveils behavior without transparency or recourse. The near-term harms reading makes their exploitation a flagship AI safety concern, yet the resulting policy interventions (transparency mandates, algorithmic auditing requirements) have not materially shifted power or earnings. They pay the cost of the constraint's theater — performing compliance for platforms — without capturing its coordination benefit.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    powerless, biographical, constrained, global).

% Civil rights organizations and affected communities that document and litigate algorithmic discrimination in housing, credit, employment, and criminal justice. They invest heavily in making the near-term harms reading legible to policymakers. Their constraint is that the reading's institutional uptake produces compliance rituals (bias bounties, model cards, diversity reports) that substitute for structural change — they pay the advocacy cost while the extraction (deferred frontier regulation) benefits labs.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination, payer,
    organized, generational, constrained, regional).

% Build applications on top of frontier models or develop specialized AI. They bear disproportionate compliance costs from near-term harms regulations (auditing, documentation, bias testing) without the legal teams, lobbying access, or infrastructure of frontier labs. The constraint extracts compliance labor from them while the agenda-setters shape the rules to their own scale. Exit means leaving AI or selling to a platform.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, smaller_ai_companies, payer,
    moderate, biographical, constrained, global).

% Researchers and institutes focused on alignment, interpretability, and governance for systems exceeding human capabilities. The near-term harms reading's dominance in policy and funding venues (e.g., executive orders, EU AI Act focus, philanthropic portfolios) structurally crowds out their research agenda. They are not merely unfunded — their framing is treated as a distraction from 'real' safety work. Their professional identity is fused to the excluded frame; exit means abandoning their field's defining question.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers, excluded,
    moderate, civilizational, identity_locked, global).

% Researchers working on fairness, robustness, interpretability, watermarking, and evaluation of deployed systems. The near-term harms reading directs funding, conference tracks, policy fellowships, and industry roles toward their work. They benefit materially from the constraint's coordination function. Their exit options are strong — the skills transfer to industry ML, policy, or adjacent fields.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_safety_researchers_near_term, beneficiary,
    organized, biographical, mobile, global).

% Government bodies drafting AI legislation (EU AI Act, US Executive Orders, UK AI Safety Institute). They adopt the near-term harms reading because it produces legible, enforceable requirements (risk tiers, conformity assessments, transparency obligations) within existing administrative capacity. The existential risk frame demands novel governance instruments they lack mandate or expertise to build. They coordinate the constraint's enforcement while being shaped by industry input.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, regulators_policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Organizations (EFF, Access Now, Algorithmic Justice League, etc.) that advocate for both near-term harm mitigation and structural accountability. They occupy a dual position: they use the near-term harms reading to win concrete protections, but critique its capture by industry and its marginalization of systemic risk. They are not the primary beneficiaries or payers — they analyze the constraint's operation from outside the core extraction loop.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, civil_society_digital_rights_orgs, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates society on a shared definition of AI safety that makes documented harms from deployed systems the primary target of policy, research funding, and corporate responsibility programs — producing legible regulatory requirements (bias audits, transparency, redress mechanisms) that can be implemented within existing governance frameworks.
% TRANSFER_FUNCTION: Moves regulatory attention, compliance burden, and research funding toward near-term harm mitigation (fairness, transparency, labor protections) and away from existential risk research (alignment, interpretability at scale, compute governance). Moves compliance costs onto smaller AI companies and deployers while frontier labs capture the benefit of avoided frontier regulation. Moves legitimacy capital from speculative long-termism to documented presentism.
% ABSENT_VOICES: Future generations (if existential risk is real, they bear the cost of deferred preparation); global majority populations whose AI harms are under-documented in Western-centric bias taxonomies; workers in AI supply chains (data labeling, content moderation) whose labor conditions are excluded from 'algorithmic discrimination' frames; open-source developers who would face disproportionate compliance burden from near-term regulations.
% DISAPPEARANCE_RATIONALE: If the near-term harms reading vanished overnight, AI policy would lose its dominant operational framework. Funding would shift toward existential risk research or dissipate; regulations like the EU AI Act's high-risk tiers would lose their justification; corporate safety teams would restructure around capabilities instead of compliance; the coalition of civil rights groups, labor orgs, and near-term researchers would fracture. The world rearranges because this reading currently structures the entire AI governance field.
% FOUNDING_PROBLEM: Deployed AI systems were causing documented, measurable harms — racial bias in lending and hiring, algorithmic management exploiting gig workers, content amplification spreading misinformation — while the AI safety field (originating around 2014-2016) focused on speculative alignment problems for systems that did not yet exist. The near-term harms reading was built to re-center AI safety on actually occurring injuries.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations (NAACP LDF, ACLU), labor researchers (Data & Society, AI Now Institute), affected community coalitions (Stop LAPD Spawning, gig worker unions), and academic researchers (Buolamwini, Gebru, Noble, Crawford) document ongoing harms. The founding problem is corroborated by parties who do not benefit from the constraint's extraction loop — indeed, many critics argue the constraint's institutional form fails the very populations it names.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint directs massive compliance costs and research funding toward a frame that benefits the most powerful actors (avoided frontier regulation) while delivering weak material redress to named victims. Suppression (0.52) is moderate: the constraint does not ban existential risk research outright, but it structurally suppresses it through funding allocation, hiring norms, and policy agenda-setting — alternative frames are not illegal but are resourced out of relevance. Theater ratio (0.41) is rising: bias audits, model cards, and transparency reports increasingly function as compliance rituals that substitute for structural change. Accessibility collapse (0.38) is moderate — alternative framings (existential risk, dual priority) remain live and organized, but face steep uphill to capture the policy apparatus. Resistance (0.55) is significant: existential risk researchers, some philosophers, and a growing number of policymakers contest the reading's exclusivity.
 *
 * PERSPECTIVAL GAP:
 *   From the frontier lab seat, the constraint is genuine coordination: they built the field, they fund the work, they comply with the rules. From the marginalized population seat, the constraint is extraction wearing a coordination mask: their injuries are the marketing material for a regulatory regime that protects platform incumbents. From the existential risk researcher seat, the constraint is a snare: it captures the 'AI safety' label and its resources while suppressing the frame that addresses their concern. The engine computes these divergences from the structural data — the claimed_type (tangled_rope) reflects the analyst's judgment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs and large platforms are structural beneficiaries (d near 0.0-0.2): they shape the reading, fund its institutions, and capture its extraction (avoided frontier regulation). Marginalized populations, gig workers, and discriminated communities are structural targets (d near 0.8-1.0): they are named as beneficiaries but bear the harm whether the constraint functions or performs, and their advocacy labor sustains the reading's legitimacy. Smaller AI companies are targets (d ~0.7): they pay compliance costs without the lobbying offset. Existential risk researchers are identity-locked targets (d ~0.9): their professional identity fuses to the excluded frame, making exit professionally lethal. Near-term safety researchers are mobile beneficiaries (d ~0.3): they gain funding and roles but can exit. Regulators are agenda-setters with analytical exit (d ~0.4): they enforce the constraint but could pivot if political winds shift.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented harms from deployed systems) remains live and worsening. The constraint has not resolved its mandatrophy — it has expanded its mandate. The near-term harms reading now governs a growing compliance industry, but the harms it names persist. The extraction loop (frontier labs avoiding frontier regulation) has strengthened over the interval, not weakened. This is not a degraded piton — the constraint is actively enforced and expanding. But it is not a pure rope — the asymmetric extraction is structural, not incidental. Tangled rope captures the dual nature: real coordination on real harms, real extraction by powerful actors using the coordination as cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_term_vs_existential_resource_competition,
    'Do near-term harm mitigation and existential risk reduction genuinely compete for fixed resources (funding, talent, policy bandwidth), or is the competition manufactured by the near-term harms reading''s institutional dominance?',
    'Track funding flows, hiring patterns, and policy attention across both frames over time; test whether increased near-term spending correlates with decreased x-risk spending controlling for field growth.',
    'If competition is genuine, the extraction from x-risk researchers is a direct transfer — tangled_rope classification strengthens. If manufactured, the extraction is ideological — the constraint may be a snare using near-term harms as cover for suppressing a disfavored frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_term_vs_existential_resource_competition, empirical, 'Whether resource competition between near-term and existential risk frames is structural or constructed.').

omega_variable(
    performative_compliance_vs_material_redress,
    'Do the regulatory instruments produced by the near-term harms reading (bias audits, transparency reports, impact assessments) produce material redress for marginalized populations, or do they function primarily as compliance theater that legitimates continued deployment?',
    'Longitudinal studies of algorithmic harm incidents before/after compliance mandates; qualitative investigation of appeal/redress processes in deployed systems; comparison of harm rates in regulated vs. unregulated domains.',
    'If compliance is largely performative, theater_ratio is underestimated and victims are more deeply trapped — constraint trends toward snare. If compliance yields material improvement, the coordination function is stronger and extraction relatively lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performative_compliance_vs_material_redress, empirical, 'Whether near-term harms regulation delivers material protection or performative legitimacy.').

omega_variable(
    kernel_framing_as_strategic_capture,
    'Is the near-term harms reading''s dominance in policy venues the result of genuine epistemic consensus, or of strategic advocacy by frontier labs and platforms who benefit from a regulatory frame that excludes capabilities governance?',
    'Document lobbying expenditures, policy fellowships, revolving-door hiring, and philanthropic funding patterns linking frontier labs to near-term harms institutions; compare with x-risk advocacy networks.',
    'If strategic capture is documented, the beneficiary declaration (frontier labs) is causally central, not incidental — the constraint''s extraction is designed, not emergent. This would sharpen the tangled_rope classification and inform the dual_priority_reading''s structural position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_as_strategic_capture, empirical, 'Whether the reading''s policy dominance reflects consensus or capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 2016, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2016, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(ai_s_tr_t2018, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(ai_s_tr_t2020, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(ai_s_tr_t2022, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(ai_s_tr_t2024, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2016, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2016, 0.25).
narrative_ontology:measurement(ai_s_be_t2018, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(ai_s_be_t2020, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement(ai_s_be_t2022, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(ai_s_be_t2024, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2016, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2016, 0.2).
narrative_ontology:measurement(ai_s_su_t2018, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2018, 0.3).
narrative_ontology:measurement(ai_s_su_t2020, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(ai_s_su_t2022, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2022, 0.48).
narrative_ontology:measurement(ai_s_su_t2024, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ai_safety_commitment kernel. The near_term_harms_reading centers documented present-day harms; the existential_risk_reading centers extinction-level outcomes from misaligned superintelligence; the dual_priority_reading claims both are non-competing priorities. The three readings have different ε profiles, different victim/beneficiary structures, and different regulatory implications. They form a constraint family linked by shared kernel but divergent operationalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, powerless, 0.9).
constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
