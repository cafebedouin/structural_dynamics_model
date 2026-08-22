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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term AI Harms Prioritization Framework
 *   domain: technology/governance/social_justice
 *
 * SUMMARY:
 *   This constraint is one reading of a contested kernel in AI risk
 *   assessment: whether the primary threat is near-term harms from deployed
 *   systems causing measurable discrimination, displacement, and surveillance
 *   of vulnerable populations NOW, or existential risk from misaligned
 *   advanced AI. This reading instantiates the near-term-harms position:
 *   current discrimination is urgent, addressable, and demands institutional
 *   prioritization. The sibling existential-risk reading treats these as
 *   secondary to preventing extinction-level misalignment. The readings do
 *   not merely disagree on empirical facts; they compete for policy
 *   attention, funding allocation, and institutional legitimacy. Neither
 *   reading's authority structure is settled — the contest is live across
 *   research institutions, regulatory bodies, and advocacy groups. This
 *   constraint story describes the near-term-harms reading as a commitment
 *   system that grounds legitimacy in urgency, measured harm, and justice to
 *   present victims.
 *
 * KEY AGENTS:
 *   - Marginalized populations: victims of present algorithmic discrimination; named beneficiaries but lack decision power
 *   - Fairness & accountability researchers: institutional agenda-setters; set research priorities and validate policy direction
 *   - Low-wage workers & surveilled communities: primary payers; experience automation displacement and surveillance intensification
 *   - Technology companies: forced to audit and remediate; costs absorbed as compliance burden
 *   - Policymakers & regulators: coordinate the framework through mandate and regulation
 *   - Existential-risk researchers: excluded; their core claim is suppressed as speculative distraction
 *   - Uncertain AI timelines: the underlying empirical fact driving the reading contest
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
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term AI Harms Prioritization Framework").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "technology/governance/social_justice").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '2023d220-43b8-4843-a663-360c254aedab').
narrative_ontology:cs_kernel_codification('2023d220-43b8-4843-a663-360c254aedab', distributed).
narrative_ontology:cs_authority_grounding('2023d220-43b8-4843-a663-360c254aedab', expertise).
narrative_ontology:cs_interpretation_layer_present('2023d220-43b8-4843-a663-360c254aedab').
narrative_ontology:cs_reading_relation('2023d220-43b8-4843-a663-360c254aedab', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('2023d220-43b8-4843-a663-360c254aedab', foundational, present_harms_morally_urgent).
narrative_ontology:cs_axiom_status(present_harms_morally_urgent, holdable).
narrative_ontology:cs_axiom_grounding('2023d220-43b8-4843-a663-360c254aedab', present_harms_morally_urgent, deontological).
narrative_ontology:cs_axiom('2023d220-43b8-4843-a663-360c254aedab', foundational, algorithmic_discrimination_measurable_and_remediable_near_term).
narrative_ontology:cs_axiom_status(algorithmic_discrimination_measurable_and_remediable_near_term, holdable).
narrative_ontology:cs_axiom_grounding('2023d220-43b8-4843-a663-360c254aedab', algorithmic_discrimination_measurable_and_remediable_near_term, empirically_contingent).
narrative_ontology:cs_reference_frame('2023d220-43b8-4843-a663-360c254aedab', justice_centered_ai_governance).
narrative_ontology:cs_drift_state('2023d220-43b8-4843-a663-360c254aedab', contemporary_2024_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2023d220-43b8-4843-a663-360c254aedab', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, low_wage_workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, surveilled_communities).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, racialized_populations_discriminated_by_algorithms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, tech_companies_operating_ai_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face immediate, measurable harms from deployed AI systems: algorithmic discrimination in hiring, lending, criminal justice; predictive policing targeting racialized neighborhoods; labor displacement without safety net. Benefit from a policy framework that prioritizes remediation and oversight of these systems now, before they accumulate further damage.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, marginalized_populations, beneficiary,
    powerless, biographical, identity_locked, national).

% Research bias, fairness, and accountability in deployed AI systems. Have career incentives, funding streams, and institutional prestige tied to the prioritization of near-term harms. Set research agendas, influence policy priorities, author bias audit standards, and advocate for regulatory frameworks focused on present discrimination rather than speculative future risk.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, agenda_setter).

% Lose employment to AI automation without retraining, severance, or income support. The prioritization framework emphasizes labor protections and worker transition support; the gap between rhetoric and funding means the framework's implementation falls far short of its framing, and workers absorb the displacement cost.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, low_wage_workers_displaced_by_automation, payer,
    powerless, biographical, trapped, national).

% Subject to intensifying algorithmic surveillance systems—facial recognition in public spaces, predictive policing, financial monitoring. The prioritization of near-term harms creates advocacy and regulatory pressure to constrain surveillance, but implementation gaps leave systemic surveillance in place while shifting its terms rhetorically.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, surveilled_communities, payer,
    powerless, biographical, identity_locked, national).

% Suffer discrimination from algorithmic systems in credit decisioning, hiring, housing, criminal risk assessment. The near-term harms framing names them as priority; the constraint's actual enforcement creates audit requirements and awareness without dismantling the discriminatory systems or compensating victims of past discrimination.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, racialized_populations_discriminated_by_algorithms, payer,
    powerless, biographical, identity_locked, national).

% Focus on long-horizon alignment and catastrophic risk from advanced AI. Are systematically excluded from policy prioritization and funding allocation by the near-term-harms framing, which treats existential concerns as speculative distraction. Their argument—that preventing misaligned AGI is more important than managing current discrimination—is suppressed in institutional discourse dominated by the near-term framing.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_researchers, excluded,
    institutional, civilizational, mobile, global).

% Face regulatory requirements, audit obligations, fairness constraints, and potential liability from the near-term-harms prioritization. The constraint requires them to invest in bias detection and remediation; they carry the costs of auditing and retrofitting systems, though they often externalize the harm costs onto affected populations.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, tech_companies_operating_ai_systems, payer,
    powerful, biographical, constrained, global).

% Adopt and enforce the near-term-harms prioritization framework via regulation, funding priorities, and institutional commitments. They coordinate fairness standards, mandate impact assessments, and allocate resources to bias remediation. Their power derives from democratic legitimacy; their constraint-setting power is exercised through regulatory capture dynamics and expertise deference to fairness researchers.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policymakers_and_regulators, agenda_setter,
    institutional, generational, mobile, national).

% Communities experiencing AI harms (workers, surveilled populations, discriminated groups) are named in the prioritization framework but lack meaningful input into implementation decisions. Participation is tokenized through advisory boards and consultations; actual resource allocation and remediation strategy are controlled by researchers, regulators, and corporate compliance officers.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, affected_communities_lacking_voice, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes research, policy, and corporate accountability mechanisms to detect, measure, and remediate discriminatory harms in currently-deployed AI systems. Coordinates institutional focus on present harm rather than speculative future risk; creates shared standards for bias auditing and fairness criteria.
% TRANSFER_FUNCTION: Moves research funding, policy attention, and regulatory authority from existential-risk research toward near-term bias remediation. Moves compliance burdens and audit costs onto technology companies. Moves visibility and institutional legitimacy toward fairness researchers and marginalized-community advocates; away from existential-risk researchers.
% ABSENT_VOICES: Existential-risk researchers are structurally excluded from policy prioritization and funding, although they would argue (if present) that focusing on near-term discrimination diverts resources from preventing extinction-level misalignment. Communities experiencing AI harms are named but lack decision-making power; they speak through researcher and advocate intermediaries rather than directly setting priorities.
% DISAPPEARANCE_RATIONALE: If the near-term-harms prioritization framework disappeared, policy attention would revert to existential-risk research, funding would flow toward alignment rather than fairness, corporate compliance would drop bias-audit requirements, and institutional validation would shift back toward technical AI safety. The present-harms framework causes measurable redistribution of legitimacy and resources; removal would reverse that flux.
% FOUNDING_PROBLEM: Deployed AI systems are causing measurable discrimination, displacement, and surveillance harm to vulnerable populations today. These harms are urgent, addressable through regulation and auditing, and disproportionately affect those with least power to resist them. Justice intervention is needed now.
% FOUNDING_PROBLEM_CORROBORATION: Documented in peer-reviewed fairness literature (Buolamwini & Gebru, Mitchell et al., Selbst & Barocas), testified to by affected communities in regulatory hearings, confirmed by independent bias audits of deployed systems (ProPublica investigations, journalist undercover testing), and acknowledged by technology companies in compliance and settlement agreements. Corroboration comes from outside the benefiting academic and regulatory institutions.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval, reflecting the framework's accumulating implementation costs: audit requirements proliferate, compliance mandates tighten, and the rhetoric-implementation gap widens (affected communities receive visibility without remediation). Suppression requirement is high (0.72) because the framework requires active exclusion of existential-risk perspectives from policy discourse, framed as a distraction from urgent present harm. Theater ratio climbs from 0.22 to 0.41, indicating growing performative compliance: companies conduct bias audits, researchers publish fairness papers, regulators issue standards — while algorithmic discrimination and surveillance persist structurally. The gap between framework rhetoric and actual remediation creates the theater. Accessibility collapse is moderate (0.58): alternatives to the near-term framing exist and are intellectually coherent (existential-risk reading), but institutional power has made them costly to advocate. Resistance is high (0.71): affected communities push back against tokenized participation; existential-risk researchers resist the suppression of their research; companies resist audit costs. The claim is tangled_rope: genuine coordination of institutional focus on measurable present harm (coordination benefit), AND asymmetric extraction from those forced to bear compliance costs and implementation shortfalls (extraction cost). The two functions coexist in the same institutional structure.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (fairness researchers, advocacy organizations) compute the constraint as genuine coordination solving an urgent justice problem; the payer seats (displaced workers, surveilled communities, compliance-burdened companies) compute it as a framework that names harm while failing to remediate it, and that suppresses alternative research directions. The institutional agenda-setters (regulators, policymakers) sit between: they benefit from the framework's legitimacy and focus while absorbing political cost when implementation fails. The existential-risk researchers compute the entire constraint as extractive monopolization of institutional attention and funding that forecloses legitimate long-horizon research.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations should derive low d (beneficiaries) but actually derive high d (targets) because identity-locking means they cannot exit the surveilled/discriminated condition and the framework fails to remediate it; the benefits are naming and policy validation, while the harms remain structural. Fairness researchers derive low d (beneficiaries collecting legitimacy and funding). Displaced workers and surveilled populations derive high d (targets bearing the harm the framework names but doesn't fix). Technology companies derive mid-to-high d (constrained: they must comply with auditing but their core business model is rarely displaced). Existential-risk researchers derive high d (targets of suppression, excluded from policy resources). The override needed here is for marginalized populations: structural data (powerless, identity_locked) plus declared beneficiary role would produce d~0.2, but the implementation failure means actual d is higher (~0.65) — declare an override to reflect the gap between framework promise and structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (present algorithmic discrimination) is live and validated by external sources. The framework exists to solve it. Yet implementation has atrophied: audits generate reports without system changes; regulations mandate standards but lack enforcement; affected communities are consulted but not empowered. The constraint persists as a coordination framework but has increasingly become performance — a way for institutions to signal commitment to fairness without bearing the cost of actual remediation. The theater_ratio rise (0.22→0.41) captures this. Mandatrophy is NOT declared because the founding problem remains live and the constraint serves coordination on measurable present harm; but the implementation gap is severe enough that the constraint is drifting toward piton (functional decay) territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_failure_vs_theoretical_coordination,
    'Is the constraint''s core function generating coordination on present-harm remediation, or is the coordination rhetorical while actual remediation remains structurally blocked?',
    'Longitudinal outcome measurement: track whether resources deployed under the framework reduce measured discrimination/displacement/surveillance in affected populations at scale, or merely increase audit and compliance activity without outcome change.',
    'If remediation succeeds at scale, the constraint is genuine tangled_rope with asymmetric implementation costs. If remediation stalls while theater expands, the constraint drifts toward piton (institutional performance lacking functional coordination). This affects whether the constraint is solvable by better enforcement or requires fundamental restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_failure_vs_theoretical_coordination, empirical, 'Whether the framework achieves actual remediation or functions primarily as performance.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Is the near-term-harms reading logically incompatible with existential-risk research, or do they address different timescales and risk classes that could coherently coexist?',
    'Formal analysis of the axioms: can a framework simultaneously prioritize present discrimination remediation AND existential-risk mitigation, or does prioritization of one necessarily suppress the other at the resource and institutional level?',
    'If coexistence is logically possible, the suppression of existential-risk research is an institutional choice, not a structural necessity — the constraint''s suppression metric overstates forced exclusion. If they logically foreclose each other, the suppression is structural to the reading''s core commitments, and the constraint is correctly classified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the two readings are logically incompatible or merely institutionally competitive.').

omega_variable(
    identity_lock_mechanism_for_affected_communities,
    'Is the high identity_locked exit option for marginalized populations a structural feature of their relationship to algorithmic systems, or an authoring choice that could be revised?',
    'Counterfactual: can affected populations realistically exit AI-mediated systems (opt out of hiring algorithms, relocation away from surveillance, alternative financial systems)? Are exits genuinely unavailable, or are they costly/identity-incompatible in the way identity-lock describes?',
    'If true structural entrapment, identity_locked is correct and drives high directionality. If exits exist but are costly/identity-incompatible (e.g., one can theoretically avoid algorithms but cannot avoid hiring, housing, or financial systems), the exit should be classified as constrained, which would lower directionality slightly. This affects whether affected communities are primarily trapped or primarily coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_affected_communities, empirical, 'Whether marginalized populations face structural or identity-based entrapment in algorithmic systems.').

omega_variable(
    sibling_reading_committer_asymmetry,
    'Does the existential_risk_reading instantiate a fundamentally different commitment structure (different authority grounding, different axioms), or do both readings share the same commitment system and simply weight the same risks differently?',
    'Comparative CS structure analysis: does existential-risk reading ground legitimacy in the same sources (expertise, institutional lineage, practice) as the near-term-harms reading, or does it invoke different authority (e.g., philosophical reasoning, technical alignment theory)?',
    'If shared commitment structure, the readings are intra-framework disagreements and their contest is resolvable through evidence. If different commitment structures, the readings are truly alternative kernels using the same label (''AI risk''), and their contest is a clash of authority systems, not empirical dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_committer_asymmetry, conceptual, 'Whether the reading contest is empirical disagreement or authority-system clash.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(ai_r_tr_t0, observed).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(ai_r_tr_t5, observed).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(ai_r_tr_t10, observed).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(ai_r_tr_t15, observed).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(ai_r_tr_t20, observed).
narrative_ontology:measurement(ai_r_tr_t25, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(ai_r_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_r_be_t0, observed).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ai_r_be_t5, observed).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(ai_r_be_t10, observed).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(ai_r_be_t15, observed).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(ai_r_be_t20, observed).
narrative_ontology:measurement(ai_r_be_t25, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(ai_r_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(ai_r_su_t0, observed).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(ai_r_su_t5, observed).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(ai_r_su_t10, observed).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(ai_r_su_t15, observed).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(ai_r_su_t20, observed).
narrative_ontology:measurement(ai_r_su_t25, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(ai_r_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__near_term_harms_reading, 0.14).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the contested kernel 'ai_risk_prioritization' into two readings with structurally distinct ε values. The near_term_harms_reading focuses on present discrimination and surveillance (ε=0.68, measuring the gap between framework promise and implementation; suppression is active exclusion of alternative research directions). The existential_risk_reading focuses on misaligned AGI and extinction-level risk (separate file; different ε, different victim set, different beneficiaries). The readings coexist institutionally as competing research programs and policy priorities; neither logically forecloses the other within a single framework, but institutional power creates suppression asymmetry. Link via network.affects_constraints because research/funding allocation decisions made under one reading directly shape the feasibility and institutional position of the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__near_term_harms_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
