% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety Commitment — Near-Term Harms Reading
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   The 'AI safety means preventing documented present-day harms' reading
 *   instantiates a constraint that presents itself as harm reduction but
 *   operates as a legitimating structure: tech companies adopt the framing to
 *   demonstrate responsibility while directing resources toward alignment
 *   research that does not threaten their business models. The constraint
 *   coordinates industry around voluntary transparency and auditing (real
 *   coordination function) while extracting regulatory relief and avoiding
 *   binding interventions on bias, labor, and misinformation (asymmetric
 *   extraction). Victims are present-day populations harmed by deployed
 *   systems; beneficiaries are the labs and platforms that capture the safety
 *   narrative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.45).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety Commitment — Near-Term Harms Reading").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '4efaaf65-45dd-462b-9909-6c24b74d382a').
narrative_ontology:cs_kernel_codification('4efaaf65-45dd-462b-9909-6c24b74d382a', distributed).
narrative_ontology:cs_authority_grounding('4efaaf65-45dd-462b-9909-6c24b74d382a', extraction).
narrative_ontology:cs_interpretation_layer_present('4efaaf65-45dd-462b-9909-6c24b74d382a').
narrative_ontology:cs_reading_relation('4efaaf65-45dd-462b-9909-6c24b74d382a', ai_safety_commitment__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('4efaaf65-45dd-462b-9909-6c24b74d382a', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('4efaaf65-45dd-462b-9909-6c24b74d382a', foundational, deployed_system_harms_define_safety).
narrative_ontology:cs_axiom_status(deployed_system_harms_define_safety, holdable).
narrative_ontology:cs_axiom_grounding('4efaaf65-45dd-462b-9909-6c24b74d382a', deployed_system_harms_define_safety, empirically_contingent).
narrative_ontology:cs_axiom('4efaaf65-45dd-462b-9909-6c24b74d382a', secondary, regulatory_capture_via_safety_framing).
narrative_ontology:cs_axiom_status(regulatory_capture_via_safety_framing, holdable).
narrative_ontology:cs_axiom_grounding('4efaaf65-45dd-462b-9909-6c24b74d382a', regulatory_capture_via_safety_framing, empirically_contingent).
narrative_ontology:cs_reference_frame('4efaaf65-45dd-462b-9909-6c24b74d382a', harm_reduction_priority_framework).
narrative_ontology:cs_drift_state('4efaaf65-45dd-462b-9909-6c24b74d382a', post_generative_ai_deployment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4efaaf65-45dd-462b-9909-6c24b74d382a', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, large_tech_companies).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_development_labs).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, harm_reduction_priority).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, algorithmic_accountability).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, labor_protection_in_ai).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publicly adopt the near-term harms framing while directing research funding and regulatory attention toward alignment and existential risk; the framing enables voluntary transparency and auditing commitments that substitute for binding regulation on labor, bias, and misinformation. They collect reputational benefit and regulatory relief without restructuring core business models.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, large_tech_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Control the research agenda, benchmark design, and definition of 'safety' in technical standards bodies. They benefit from the framing's legitimating effect on their institutional authority while avoiding costly interventions on deployed-system harms.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_development_labs, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, ai_development_labs, agenda_setter).

% Bear the accumulated harms of algorithmic bias in hiring, lending, policing, and content moderation; the near-term framing promises remediation but extraction continues via delayed auditing, voluntary standards, and regulatory capture. No practical exit from systems that govern credit, employment, and public services.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_populations, payer,
    powerless, biographical, trapped, regional).

% Subject to algorithmic management that extracts labor value while classifying them as independent contractors; the safety framing produces ethical AI principles and transparency reports that do not alter wage-setting, deactivation, or data collection practices. Exit means leaving the platform economy entirely.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    organized, biographical, constrained, national).

% Experience compounding discrimination in housing, healthcare, and criminal justice mediated by deployed systems; the safety framing produces impact assessments and bias bounties that individualize structural harms. Exit requires collective political action against entrenched institutional users of these systems.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination, payer,
    moderate, generational, constrained, regional).

% Produce the technical literature that both readings cite; their institutional incentives align with the existential-risk framing (funding, prestige, talent pipeline), creating a structural bias in what counts as 'safety research' and what gets measured.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_safety_researchers, observer,
    analytical, civilizational, analytical, global).

% Advocate for near-term harm regulation and labor protections; they set the alternative agenda but are excluded from the technical standards bodies and frontier model governance forums where the constraint's enforcement is actually negotiated.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, civil_society_organizations, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, civil_society_organizations, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legitimating framework that allows AI developers to claim safety commitment while deferring structural interventions on deployed systems; coordinates industry self-governance around voluntary transparency and auditing rather than binding regulation.
% TRANSFER_FUNCTION: Moves regulatory pressure, research funding, and public attention from binding interventions on bias, labor exploitation, and misinformation toward voluntary alignment research, interpretability benchmarks, and speculative risk modeling — from marginalized communities and workers to institutional AI labs.
% ABSENT_VOICES: Workers subjected to algorithmic management without union representation; communities targeted by predictive policing and automated benefits denial who lack standing in AI governance forums; Global South populations whose data trains systems they have no say in governing.
% DISAPPEARANCE_RATIONALE: If the near-term harms reading vanished, regulatory pressure would shift toward binding algorithmic accountability laws, labor protections for gig workers, and enforceable transparency mandates — tech companies would lose the legitimating cover that substitutes voluntary commitments for regulation, and the research agenda would lose its primary justification for current funding allocation.
% FOUNDING_PROBLEM: Early AI ethics discourse identified that deployed systems were already causing documented harms — bias in hiring and lending, labor exploitation in gig platforms, misinformation at scale — and these harms required immediate remediation rather than speculative future research.
% FOUNDING_PROBLEM_CORROBORATION: Documented by independent journalists (e.g., Virginia Eubanks, Cathy O'Neil), labor organizers (e.g., Gig Workers Rising), civil rights litigators (e.g., ACLU algorithmic justice cases), and academic researchers outside the AI safety funding ecosystem (e.g., Timnit Gebru, Joy Buolamwini, Safiya Noble) — all attest the harms persist and the founding problem remains live.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness (0.68) reflects that the constraint diverts massive regulatory and research capacity toward low-ε alignment work while high-ε harms persist unaddressed; suppression (0.45) is moderate — the constraint operates through agenda-setting and funding allocation rather than direct coercion, but it structurally excludes binding alternatives; theater ratio (0.38) is rising as voluntary commitments proliferate while harms accelerate. The claimed type is tangled_rope because genuine coordination (industry alignment on safety language) coexists with extraction (regulatory capture via narrative control).
 *
 * PERSPECTIVAL GAP:
 *   From the AI lab seat, the constraint is a rope — genuine coordination on safety culture, real progress on interpretability. From the marginalized community seat, it is a snare — the safety language is cover for continued extraction. The engine computes this divergence from the structural data; the near-term framing's power is that it lets both readings coexist in public discourse while the extraction continues.
 *
 * DIRECTIONALITY LOGIC:
 *   Large tech companies and AI labs are structural beneficiaries (d near 0.0) — they collect reputational capital, regulatory relief, and agenda control. Marginalized populations, gig workers, and discriminated communities are structural targets (d near 1.0) — they bear ongoing harms while the constraint's enforcement machinery (voluntary audits, ethical principles) produces no material relief. AI safety researchers sit near analytical (d ~ 0.5) but their institutional incentives align with the beneficiary side. Civil society organizations are agenda-setters structurally excluded from enforcement forums.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented present-day harms) is live and corroborated by parties outside the beneficiary set. The constraint persists not because the problem is solved but because the beneficiary structure captures the remediation agenda — a classic mandatrophy pattern where the arrangement's mandate has been inverted: it was built to address near-term harms, now it primarily protects the institutions causing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the voluntary transparency/auditing coordination function structurally separable from the regulatory-capture extraction function, or does the coordination itself require the extraction to be sustained?',
    'Natural experiment: if binding regulation on near-term harms were enacted, would industry safety coordination (red-teaming, interpretability, evals) persist at current intensity, or would it collapse without the legitimating cover?',
    'If inseparable, the constraint is a snare — the coordination is the extraction mechanism. If separable, it is a tangled rope with a genuine coordination core that could survive regulatory reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the safety coordination and regulatory capture are separable functions or a single integrated structure.').

omega_variable(
    near_term_existential_resource_competition,
    'Does the near-term harms reading''s institutional success (funding, talent, regulatory attention) directly reduce resources for existential risk work, or do they expand the total safety pie?',
    'Track AI safety funding flows and talent allocation before/after major near-term harm policy victories (e.g., EU AI Act, US executive orders) — does alignment research funding grow, shrink, or stay flat?',
    'If zero-sum, the readings are in structural competition (influences relation confirmed). If positive-sum, they may genuinely coexist without trade-off (coexists_with relation sufficient).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_term_existential_resource_competition, empirical, 'Resource competition between near-term harm remediation and existential risk research within the AI safety field.').

omega_variable(
    framing_capture_vs_genuine_priority,
    'Do tech companies adopt the near-term harms framing because they genuinely prioritize those harms, or because it is the optimal strategy for avoiding binding regulation on their core business models?',
    'Compare lobbying expenditures, regulatory submissions, and internal communications on near-term harm regulation vs. existential risk regulation — which does industry actually fight?',
    'If strategic capture, the constraint''s beneficiary structure is intentional and the tangled_rope classification is stable. If genuine priority, the extraction may be an unintended consequence of coordination complexity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_capture_vs_genuine_priority, empirical, 'Intentionality of beneficiary capture — strategic framing adoption vs. genuine priority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_s_tr_t2, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__near_term_harms_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__near_term_harms_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__near_term_harms_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_s_be_t2, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(ai_s_su_t2, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2, 0.36).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 10, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, algorithmic_accountability_regulation).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, gig_worker_labor_protections).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'AI safety' label into a constraint with high extraction on near-term harm remediation. The existential_risk_reading has low ε on near-term harms but high ε on alignment research capture. The dual_priority_reading claims non-competition but structurally functions as a compromise that preserves both extraction vectors. All three share the kernel ai_safety_commitment but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, organized, 0.15).
constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
