% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: ai_risk_prioritization__near_term_harms_reading
 *   human_readable: Near-Term Harms Reading of AI Risk Prioritization
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   This story instantiates the near-term-harms reading of the contested AI
 *   risk prioritization kernel: the claim that AI risk is primarily
 *   constituted by present, measurable harms — algorithmic discrimination,
 *   labor displacement, surveillance expansion — and that justice
 *   interventions targeting these harms deserve primary resource and policy
 *   priority. The reading genuinely coordinates real, documentable, immediate
 *   harm to identifiable populations into legible advocacy, funding, and
 *   legislative action. It also, in its institutionalized form, has developed
 *   a professional and funding ecosystem (fairness/accountability research,
 *   governance NGOs, compliance consulting) whose material interests are
 *   served by the framing's dominance independent of whether the underlying
 *   harms are actually reduced — and whose advocacy discourse actively frames
 *   the sibling existential-risk reading as a resource-diverting distraction,
 *   which is itself an extractive move against a rival claim on scarce
 *   attention and funding, not merely an empirical assessment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__near_term_harms_reading, 0.62).
domain_priors:suppression_score(ai_risk_prioritization__near_term_harms_reading, 0.55).
domain_priors:theater_ratio(ai_risk_prioritization__near_term_harms_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_risk_prioritization__near_term_harms_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__near_term_harms_reading, "Near-Term Harms Reading of AI Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_prioritization__near_term_harms_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__near_term_harms_reading, '22c7367e-7f6b-4e82-88d4-05b653b565dd').
narrative_ontology:cs_kernel_codification('22c7367e-7f6b-4e82-88d4-05b653b565dd', distributed).
narrative_ontology:cs_authority_grounding('22c7367e-7f6b-4e82-88d4-05b653b565dd', distributed).
narrative_ontology:cs_reading_relation('22c7367e-7f6b-4e82-88d4-05b653b565dd', ai_risk_prioritization__existential_risk_reading, coexists_with).
narrative_ontology:cs_axiom('22c7367e-7f6b-4e82-88d4-05b653b565dd', foundational, measurable_present_harm_has_lexical_priority).
narrative_ontology:cs_axiom_status(measurable_present_harm_has_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('22c7367e-7f6b-4e82-88d4-05b653b565dd', measurable_present_harm_has_lexical_priority, instrumental).
narrative_ontology:cs_axiom('22c7367e-7f6b-4e82-88d4-05b653b565dd', secondary, speculative_future_catastrophe_claims_warrant_discounted_resource_priority).
narrative_ontology:cs_axiom_status(speculative_future_catastrophe_claims_warrant_discounted_resource_priority, holdable).
narrative_ontology:cs_axiom_grounding('22c7367e-7f6b-4e82-88d4-05b653b565dd', speculative_future_catastrophe_claims_warrant_discounted_resource_priority, empirically_contingent).
narrative_ontology:cs_reference_frame('22c7367e-7f6b-4e82-88d4-05b653b565dd', civil_rights_and_labor_law_extension_framework).
narrative_ontology:cs_drift_state('22c7367e-7f6b-4e82-88d4-05b653b565dd', post_generative_ai_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('22c7367e-7f6b-4e82-88d4-05b653b565dd', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, ai_governance_ngo_sector).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, compliance_consulting_firms).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, racialized_communities_subject_to_algorithmic_scoring).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, low_wage_gig_workers_under_algorithmic_management).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, surveilled_immigrant_and_border_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, displaced_content_moderators_and_clerical_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__near_term_harms_reading, ai_deploying_firms).
narrative_ontology:constraint_victim(ai_risk_prioritization__near_term_harms_reading, ai_deploying_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to credit scoring, hiring algorithms, and predictive policing tools that encode historical bias into present decisions. Cannot opt out of the systems that score them (housing, employment, criminal justice) and have no meaningful say in audit design or remedy structure even though this reading exists to speak for them.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, racialized_communities_subject_to_algorithmic_scoring, payer,
    powerless, immediate, trapped, national).

% Managed, scheduled, and terminated by opaque dispatch and rating algorithms with no appeal mechanism. Leaving one platform typically means moving to a structurally identical one; the harm is continuous and current, which is exactly the evidentiary case this reading uses to argue for its own priority.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, low_wage_gig_workers_under_algorithmic_management, payer,
    powerless, immediate, constrained, national).

% Tracked by facial recognition, predictive risk scoring at borders, and biometric databases deployed well ahead of regulatory oversight. Have no exit from jurisdiction and no institutional standing to contest classification errors that carry deportation or detention consequences.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, surveilled_immigrant_and_border_populations, payer,
    powerless, immediate, trapped, national).

% Losing work as generative and classification systems automate moderation and clerical tasks, often in the Global South with minimal severance or retraining support. Their displacement is presented as the paradigm case near-term-harms advocacy exists to address, but the interventions funded rarely reach them directly.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, displaced_content_moderators_and_clerical_workers, payer,
    powerless, biographical, constrained, global).

% Build careers, grant portfolios, and institutional standing on documenting present algorithmic harm. Sets the research agenda for what counts as a legible, fundable harm (bias audits, disparate-impact metrics) and benefits professionally regardless of whether documented harms are actually remedied.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, fairness_accountability_researchers, agenda_setter).

% Raises philanthropic and government funding specifically framed around present, measurable, prosecutable harms. Sets policy priorities in legislative testimony and coalition statements, and has strong institutional incentive to characterize competing risk framings (existential risk) as a distraction diverting funding and attention.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_governance_ngo_sector, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, ai_governance_ngo_sector, agenda_setter).

% Sell bias-audit, algorithmic-impact-assessment, and compliance-certification services to firms deploying AI systems. Benefit financially from the near-term-harms framing becoming regulatory law regardless of whether audits change deployed system behavior; can pivot service offerings as regulation shifts.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, compliance_consulting_firms, beneficiary,
    powerful, biographical, arbitrage, national).

% Bear compliance costs (audits, documentation, legal exposure) under this framing's regulatory push, but also benefit from a risk framework that treats harm as fixable through process compliance rather than through capability restriction or deployment moratoria, which would cost far more.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, ai_deploying_firms, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, ai_deploying_firms, beneficiary).

% Argues that catastrophic and irreversible risks from advanced AI systems deserve comparable or greater resource priority, but is characterized within this reading's advocacy discourse as speculative, elite-captured, or a distraction from urgent present suffering — excluded from many near-term-harms coalition tables and funding streams as a matter of framing, not evidence.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, existential_risk_research_community, excluded,
    organized, civilizational, constrained, global).

% Draft algorithmic accountability legislation, hold hearings, and allocate regulatory agency budgets. Choose which risk framing to legislate around based partly on which coalition's evidence and testimony is most legible and immediately actionable, which structurally favors near-term, documentable harms over probabilistic long-horizon ones.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__near_term_harms_reading, policy_makers_and_legislators, observer,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__near_term_harms_reading, policy_makers_and_legislators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__near_term_harms_reading, ai_governance_ngo_sector).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates advocacy, research funding, and regulatory attention around AI harms that are measurable, occurring now, and attributable to identifiable deployed systems — enabling concrete legal and policy remedies (bias audits, worker protections, surveillance limits) rather than diffusing effort across speculative future scenarios.
% TRANSFER_FUNCTION: Moves research funding, legislative attention, and regulatory capacity toward present-harms documentation and compliance infrastructure, and away from long-horizon alignment and capability-governance research; moves compliance costs from harmed populations (who bear the cost of inaction) toward deploying firms (who bear audit and documentation costs, often without correspondingly reduced harm).
% ABSENT_VOICES: The populations most harmed (gig workers, surveilled communities, displaced clerical workers) are named as beneficiaries but rarely sit in the rooms where audit standards or remedy structures are set. Existential-risk researchers are excluded from many coalition and funding conversations by the framing itself, which characterizes their concern as a resource-diverting distraction rather than engaging its substance.
% DISAPPEARANCE_RATIONALE: If this prioritization framework and its associated funding and legislative apparatus disappeared, bias-audit mandates, algorithmic worker-protection bills, and surveillance-oversight regimes currently in motion would lose their primary advocacy infrastructure; research funding would likely reallocate toward capability and alignment work or elsewhere entirely, and the professional field built around present-harm documentation would substantially contract.
% FOUNDING_PROBLEM: Deployed AI systems were producing measurable, documentable discriminatory and exploitative outcomes (biased hiring/lending/policing algorithms, algorithmically managed precarious labor, expanding biometric surveillance) that existing civil-rights and labor law was not equipped to address, while public and funder attention was increasingly captured by speculative long-horizon AI scenarios.
% FOUNDING_PROBLEM_CORROBORATION: Independent empirical audits (e.g. academic algorithmic-bias studies, investigative journalism on gig-platform algorithmic management, government surveillance oversight reports) corroborate ongoing present-tense harm from outside the advocacy and research funding ecosystem itself. However, whether the specific institutional apparatus built around this framing is the necessary or sufficient response to that live problem is contested even by some allied labor and civil-rights organizations, who note remedies are often symbolic audits rather than binding restrictions.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__near_term_harms_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at moderate-high (0.62) and rising over the measured interval: the coordination function (documenting and remedying present harm) is real, but an increasing share of resource flow goes to audit/compliance infrastructure and advocacy-sector institution-building rather than to the harmed populations directly. Theater ratio rises to 0.4, reflecting the growing gap between compliance-audit activity and measurable harm reduction (bias audits proliferate; discriminatory outcomes persist). Suppression is authored at 0.55 — not primarily coercive suppression of the populations it serves, but suppression of the sibling framing's claim on resources and legitimacy, achieved through characterizing existential risk concern as speculative or elite-captured rather than through engaging its substance. Accessibility collapse is moderate (0.45): the near-term-harms framing has not fully foreclosed alternative framings, but it has captured a disproportionate share of legislative and philanthropic attention relative to open contestation of the underlying prioritization question.
 *
 * DIRECTIONALITY LOGIC:
 *   The harmed populations (racialized communities, gig workers, surveilled populations, displaced workers) are named beneficiaries of the advocacy discourse but structurally occupy the payer/victim role: they bear the actual harm continuously and have no meaningful exit, yet capture little of the resources the framing mobilizes in their name. Fairness/accountability researchers, governance NGOs, and compliance consultants are the true structural beneficiaries — their institutional and financial position improves as the framing gains regulatory traction, regardless of remedy efficacy. This is the tangled-rope signature: genuine coordination (documenting real present harm) fused with asymmetric extraction (professional/institutional capture of the resources and attention mobilized to address it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — present, measurable AI-driven discrimination and exploitation — remains live and independently corroborated (R5 status: live), which forecloses treating this as simple mandatrophy or pure theater. What prevents mislabeling this as pure coordination is the divergence between the claimed type (tangled_rope, chosen deliberately here) and what a naive read of the sunny advocacy framing would suggest (rope): the same structure that documents real harm also builds durable professional and institutional stakes in the framing's continued dominance and in the marginalization of the sibling existential-risk claim, which is a resource-allocation move, not a pure epistemic one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_priority_vs_institutional_capture,
    'Is the near-term-harms framing''s dominance in current AI policy and funding driven by the genuine relative urgency and evidentiary strength of present harms, or by the framing''s institutional advantage (legible metrics, existing civil-rights/labor law hooks, established advocacy infrastructure) independent of true comparative risk magnitude?',
    'Comparative resource-allocation analysis tracking whether funding and legislative attention shifts in response to new evidence on either side of the kernel contest, versus tracking institutional incumbency and coalition size; independent (non-advocacy-sector) risk assessment of relative expected harm across timescales.',
    'If dominance tracks institutional incumbency more than evidentiary strength, the framing''s suppression of the existential-risk reading (as ''distraction'') is itself an extractive move rather than a defensible risk-prioritization conclusion, supporting the tangled_rope classification more strongly. If dominance tracks genuine evidentiary urgency, the coordination function is stronger relative to the extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_priority_vs_institutional_capture, conceptual, 'Whether the reading''s dominance reflects genuine risk urgency or institutional path-dependency.').

omega_variable(
    remedy_efficacy_gap,
    'Do the bias-audit and compliance mechanisms this reading mobilizes actually reduce measured harm to the named victim populations over time, or do they primarily produce documentation and certification activity that substitutes for harm reduction (Goodhart drift)?',
    'Longitudinal outcome studies tracking disparate-impact metrics, gig-worker material conditions, and surveillance-related harms in jurisdictions before and after audit/compliance regime adoption, compared to jurisdictions without such regimes.',
    'A wide gap between audit activity and outcome improvement would confirm the rising theater_ratio trajectory as a real Goodhart-drift signature and strengthen the tangled_rope/extraction reading; a narrow gap would support the rope/coordination framing more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_efficacy_gap, empirical, 'Whether compliance infrastructure produces measurable harm reduction or substitute documentation activity.').

omega_variable(
    cross_reading_resource_zero_sum,
    'Is the resource competition between the near-term-harms and existential-risk readings genuinely zero-sum (such that this reading''s gain structurally requires the sibling''s suppression), or is the apparent conflict an artifact of current funding-institution structure that could be resolved by resource expansion rather than reallocation?',
    'Analysis of whether total AI-risk-related funding and policy capacity has been roughly fixed (supporting zero-sum) or has grown enough that both readings could be substantially resourced without displacing each other.',
    'If genuinely zero-sum, the suppression this reading exerts on the sibling reading is structurally necessary to its own resourcing, deepening the tangled_rope classification. If not zero-sum, the observed suppression is a discretionary framing choice rather than a structural necessity, which would narrow the gap between claimed and computed type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_reading_resource_zero_sum, empirical, 'Whether the two kernel readings structurally compete for a fixed resource pool or could both be resourced without conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__near_term_harms_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_prioritization__near_term_harms_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_prioritization__near_term_harms_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_prioritization__near_term_harms_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__near_term_harms_reading, ai_risk_prioritization__existential_risk_reading).

% DUAL FORMULATION NOTE:
% This story and ai_risk_prioritization__existential_risk_reading are sibling readings of the same kernel (ai_risk_prioritization): a single underlying contest over how societal resources for AI risk governance should be allocated. They are NOT the same constraint measured two ways — they have different victim sets (present marginalized populations here vs. all future persons in the sibling), different timescales (0-5 years vs. multi-decade/civilizational), different beneficiary structures (present-harm advocacy/compliance sector here vs. alignment-research institutions in the sibling), and different authored ε values reflecting each reading's own internal structure. Per the ε-invariance principle, each reading is authored as its own clean constraint with its own stable ε; the kernel-level contest between them is documented via omega variables and this network link, not folded into either constraint's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
