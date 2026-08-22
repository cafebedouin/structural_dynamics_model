% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment as Bias/Harm Prevention Ethics Commitment
 *   domain: technology_ethics/ai_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the ethics_justice_reading of the
 *   ai_alignment_commitment kernel. The reading defines alignment as
 *   preventing reproduction of social bias and present-day harm in AI systems
 *   — prioritizing demonstrated current harms to marginalized populations
 *   over speculative future catastrophic risks. The constraint operates as a
 *   tangled rope: it coordinates genuine bias mitigation (real coordination
 *   function for communities experiencing algorithmic discrimination) while
 *   extracting resources from long-term safety research (asymmetric
 *   extraction from researchers whose work addresses different harm models).
 *   The extraction is enforced through funding priorities, hiring norms,
 *   regulatory mandates, and reputational pressure that make it
 *   professionally costly to pursue control-theoretic alignment without also
 *   performing current-harm mitigation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.42).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.35).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment as Bias/Harm Prevention Ethics Commitment").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "technology_ethics/ai_governance").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, 'b504368d-5351-46ae-bd6e-03b8f840a638').
narrative_ontology:cs_kernel_codification('b504368d-5351-46ae-bd6e-03b8f840a638', distributed).
narrative_ontology:cs_authority_grounding('b504368d-5351-46ae-bd6e-03b8f840a638', distributed).
narrative_ontology:cs_reading_relation('b504368d-5351-46ae-bd6e-03b8f840a638', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('b504368d-5351-46ae-bd6e-03b8f840a638', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('b504368d-5351-46ae-bd6e-03b8f840a638', foundational, current_harm_primacy_over_speculative_risk).
narrative_ontology:cs_axiom_status(current_harm_primacy_over_speculative_risk, holdable).
narrative_ontology:cs_axiom_grounding('b504368d-5351-46ae-bd6e-03b8f840a638', current_harm_primacy_over_speculative_risk, empirically_contingent).
narrative_ontology:cs_axiom('b504368d-5351-46ae-bd6e-03b8f840a638', foundational, marginalized_community_harm_as_alignment_failure).
narrative_ontology:cs_axiom_status(marginalized_community_harm_as_alignment_failure, holdable).
narrative_ontology:cs_axiom_grounding('b504368d-5351-46ae-bd6e-03b8f840a638', marginalized_community_harm_as_alignment_failure, deontological).
narrative_ontology:cs_axiom('b504368d-5351-46ae-bd6e-03b8f840a638', secondary, bias_mitigation_as_necessary_condition_for_legitimate_deployment).
narrative_ontology:cs_axiom_status(bias_mitigation_as_necessary_condition_for_legitimate_deployment, holdable).
narrative_ontology:cs_axiom_grounding('b504368d-5351-46ae-bd6e-03b8f840a638', bias_mitigation_as_necessary_condition_for_legitimate_deployment, conventional).
narrative_ontology:cs_reference_frame('b504368d-5351-46ae-bd6e-03b8f840a638', algorithmic_fairness_as_alignment).
narrative_ontology:cs_drift_state('b504368d-5351-46ae-bd6e-03b8f840a638', post_generative_ai_deployment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b504368d-5351-46ae-bd6e-03b8f840a638', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, civil_rights_organizations).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, bias_audit_practitioners).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ethics_review_boards).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, frontier_lab_alignment_teams).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, compute_intensive_research_programs).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, algorithmic_fairness_do_presupposes_current_harm_primacy).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, disparate_impact_doctrine).
narrative_ontology:constraint_vindicates(ai_alignment_commitment__ethics_justice_reading, environmental_justice_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities historically subjected to algorithmic discrimination (racialized groups in predictive policing, women in hiring algorithms, disabled people in benefit allocation systems) who experience AI systems as extensions of existing structural bias. They cannot exit the constraint because their identity categories are the very targets of the systems in question; the constraint's enforcement is the only mechanism that names their harm.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities, beneficiary,
    powerless, biographical, identity_locked, global).

% Advocacy organizations (ACLU, EPIC, AI Now Institute, Data & Society) that set the policy agenda for bias mitigation, litigate discriminatory deployments, and define the audit standards. They benefit from the constraint because it legitimizes their expertise and unlocks regulatory enforcement pathways they have built.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, civil_rights_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, civil_rights_organizations, agenda_setter).

% Technical practitioners and consultancies performing fairness audits, disparate impact assessments, and bias evaluations. The constraint creates a market for their services; they exit by pivoting to other compliance domains if enforcement weakens.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, bias_audit_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Institutional Review Boards, AI Ethics Committees, and mandated algorithmic impact assessment bodies that administer the constraint. They set the procedural standards, collect institutional authority from gatekeeping deployments, and can redirect to other governance frameworks if this one loses legitimacy.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ethics_review_boards, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_commitment__ethics_justice_reading, ethics_review_boards, beneficiary).

% Researchers working on mechanistic interpretability, scalable oversight, reward modeling, and control-theoretic alignment for advanced systems. They bear extraction because funding, talent, and institutional oxygen are diverted to bias mitigation benchmarks that do not address their research problems. Their exit is constrained by the field's funding structure: grant agencies and labs prioritize the ethics_justice framing.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, civilizational, constrained, global).

% Alignment teams at major labs (Anthropic, OpenAI, DeepMind, etc.) who must allocate finite research capacity between current-harm mitigation (RLHF for bias, red-teaming for hate speech) and long-term control problems (scalable oversight, interpretability). They pay in delayed research agendas and reputational pressure; exit is constrained because public and regulator scrutiny locks them into the current-harm frame.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, frontier_lab_alignment_teams, payer,
    powerful, generational, constrained, global).

% Large-scale training runs and infrastructure programs whose compute budgets are partially redirected to bias evaluation suites, red-teaming for social harms, and demographic parity benchmarking. They exit by reallocating compute to other objectives if the constraint's enforcement relaxes.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, compute_intensive_research_programs, payer,
    institutional, biographical, mobile, global).

% Researchers and advocates (MIRI, FHI, CAIS) who frame alignment primarily as catastrophic risk prevention. They are structurally excluded from the ethics_justice reading's policy table because their harm model (misaligned superintelligence) is treated as speculative distraction. Their identity is fused to the control frame; they cannot adopt the bias frame without dissolving their research program.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, existential_risk_advocates, excluded,
    organized, civilizational, identity_locked, global).

% Legislators, regulators (EU AI Act, US Executive Orders, UK AI Safety Institute) who must adjudicate between competing alignment framings. They observe the structural tension and allocate regulatory bandwidth; their analytical seat computes the tradeoff but they do not bear the extraction directly.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_governance_policymakers, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared standard for identifying, measuring, and mitigating discriminatory outputs from deployed AI systems — creating a common language for 'fairness' that enables regulatory enforcement, civil society litigation, and industry compliance.
% TRANSFER_FUNCTION: Moves research funding, engineering talent, compute allocation, and regulatory attention FROM long-term control research and frontier capability scaling TO bias evaluation, disparate impact auditing, demographic parity benchmarking, and current-harm remediation.
% ABSENT_VOICES: Communities in the Global South whose algorithmic harms are shaped by different power structures (colonial data extraction, labor exploitation in data annotation, language model erasure) but who are not seated at the Western bias-audit table. Also: future persons who would bear catastrophic risk if control problems are deferred — they are absent by definition but the constraint's resource allocation affects them.
% DISAPPEARANCE_RATIONALE: If the ethics_justice reading vanished overnight, bias audits would lose regulatory mandate, disparate impact litigation would lose its technical standard, marginalized communities would lose their primary policy lever, and labs would reallocate the extracted resources to capability scaling and control research — the entire AI governance landscape would reorganize around the safety_control reading.
% FOUNDING_PROBLEM: AI systems deployed at scale (hiring, lending, policing, healthcare, content moderation) were reproducing and amplifying historical discrimination against protected groups, with no accountability mechanism for the developers or deployers.
% FOUNDING_PROBLEM_CORROBORATION: Documented by independent investigative journalism (ProPublica's COMPAS investigation, Reuters' hiring algorithm exposes), academic literature (Buolamwini & Gebru on facial recognition disparity, Obermeyer et al. on healthcare allocation bias), and government reports (NIST FRVT demographic differentials, EU Fundamental Rights Agency surveys) — all sources outside the benefiting civil rights organizations and audit practitioners.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_commitment__ethics_justice_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).
:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the measurable diversion: major labs allocate 15-30% of alignment headcount to bias/red-teaming for social harms; grant programs (NSF, ERC, UKRI) weight 'responsible AI' criteria heavily; conference venues (FAccT, AIES) absorb submission capacity that might go to control research. Suppression (0.35) is moderate: the constraint does not ban control research but makes it structurally harder to fund and staff; dissenting researchers face reputational friction but not deplatforming. Theater ratio (0.28) captures the growing gap between audit performativity (checklist fairness metrics) and substantive harm reduction. The trajectory shows steady extraction accumulation as the reading institutionalized (2016-2026).
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (marginalized communities, civil rights orgs), the constraint is a rope: genuine coordination solving a real collective action problem (no one lab would voluntarily audit for bias). From the payer seats (safety researchers, frontier labs), it is a snare: extraction enforced through funding and reputational machinery that suppresses the alternative harm model. The engine computes this divergence from the declared structural positions — the claimed_type (tangled_rope) names the hybrid structure that produces the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and civil rights organizations are structural beneficiaries (d near 0.0): the constraint subsidizes their harm recognition and enforcement leverage. Bias audit practitioners and ethics boards are near-symmetric beneficiaries (d ~0.2-0.3): they collect real coordination value and some institutional rents. Long-term safety researchers and frontier lab teams are targets (d ~0.7-0.8): they bear the resource diversion and agenda displacement. Compute-intensive programs are mobile payers (d ~0.5): they can reallocate if enforcement shifts. Existential risk advocates are identity-locked excluded (d ~0.9): their research program is structurally incompatible with the reading's harm model. Policymakers sit at analytical (d=0.5): they observe the tradeoff without bearing extraction directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (algorithmic discrimination at scale) remains live — new deployment domains (generative AI, medical LLMs, automated government) create fresh bias surfaces. But the constraint shows mandatrophy signals: the coordination function (bias mitigation) has expanded to cover harms that are not discriminatory (copyright, privacy, labor displacement) while the extraction from control research has increased. The reading's mandate has outgrown its original function; the integrated_reading sibling attempts to resolve this by merging frames.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the ethics_justice_reading instantiate a distinct constraint from the integrated_reading, or are they the same constraint viewed from different emphasis?',
    'Test ε-invariance: if the integrated_reading''s claimed simultaneous attention structurally changes the victim set (adds future persons) and the extraction profile (reduces diversion from control research), then they are different constraints with different ε. Measure resource allocation in labs that adopt integrated vs. ethics-only framings.',
    'If same constraint, the ethics_justice_reading''s claimed_type (tangled_rope) would be an incomplete measurement of a single constraint''s profile. If distinct, each reading gets its own ε and classification — the kernel is a family, not a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are ε-invariant distinct constraints or emphasis variants of one constraint.').

omega_variable(
    extraction_measurement_boundary,
    'How much of the measured extractiveness (0.42) is necessary coordination overhead for bias mitigation vs. genuine asymmetric extraction from control research?',
    'Counterfactual: estimate the minimum resource floor for effective bias auditing (independent of control research). Any diversion above that floor is extractive overhead. Compare labs with and without dedicated safety teams to isolate the marginal cost of the ethics_justice mandate.',
    'If most extraction is coordination floor, the constraint trends toward rope. If substantial overhead exists, tangled_rope or snare classification strengthens. This boundary determines whether the ethics_justice reading''s extraction is structural necessity or rent-seeking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_measurement_boundary, empirical, 'The coordination-cost vs. extractive-overhead boundary within the ethics_justice reading''s measured ε.').

omega_variable(
    excluded_future_persons_suppression,
    'Does the constraint''s resource allocation suppress the interests of future persons who would bear catastrophic risk, and if so, is that suppression structural or a side effect of finite attention?',
    'Model the counterfactual research portfolio if ethics_justice mandate were relaxed: would control research capacity expand proportionally, or would resources flow to capability scaling? Track talent pipelines: do PhD students choose bias mitigation over control because of funding/mentorship structures?',
    'If suppression is structural (the constraint actively forecloses control research), the ethics_justice reading extracts from future persons — expanding its victim set intergenerationally. If side effect, the extraction is intra-generational only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_future_persons_suppression, conceptual, 'Whether the ethics_justice reading''s extraction from control research constitutes suppression of future-person interests.').

omega_variable(
    identity_locked_exclusion_mechanism,
    'Is the existential_risk_advocates'' exclusion identity_locked (ideological fusion to control frame) or structurally enforced (institutional gatekeeping)?',
    'Trace hiring and funding networks: are control researchers excluded from ethics_justice venues, or do they self-select out? Survey researchers who bridge both frames — what friction do they encounter?',
    'If identity_locked, the exclusion persists even if institutional gates open (self-reinforcing). If structural, policy changes (integrated funding calls, joint conferences) could reduce exclusion. Affects whether the constraint''s suppression metric should be higher for the excluded seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exclusion_mechanism, empirical, 'Mechanism of exclusion for the existential risk advocate seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 2016, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2016, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(ai_a_tr_t2018, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(ai_a_tr_t2022, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2022, 0.23).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2024, 0.26).
narrative_ontology:measurement(ai_a_tr_t2026, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2016, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2016, 0.12).
narrative_ontology:measurement(ai_a_be_t2018, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2018, 0.18).
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2020, 0.28).
narrative_ontology:measurement(ai_a_be_t2022, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2022, 0.35).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2024, 0.39).
narrative_ontology:measurement(ai_a_be_t2026, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2016, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2016, 0.15).
narrative_ontology:measurement(ai_a_su_t2018, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2018, 0.22).
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement(ai_a_su_t2022, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2022, 0.31).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2024, 0.33).
narrative_ontology:measurement(ai_a_su_t2026, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.08).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_governance_regulatory_capture).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, frontier_lab_resource_allocation).

% DUAL FORMULATION NOTE:
% The ai_alignment_commitment kernel decomposes into three constraint stories: ethics_justice_reading (this story, tangled_rope), safety_control_reading (mountain-claimed, measures as snare for excluded seats), and integrated_reading (claimed rope, measures as scaffold). The ethics_justice reading upstream-influences the integrated reading by setting the regulatory baseline that the integrated reading must incorporate. The safety_control reading coexists with ethics_justice across different institutional coalitions but influences it by competing for the same research talent pool.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_commitment__ethics_justice_reading, powerful, 0.75).
constraint_indexing:directionality_override(ai_alignment_commitment__ethics_justice_reading, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
