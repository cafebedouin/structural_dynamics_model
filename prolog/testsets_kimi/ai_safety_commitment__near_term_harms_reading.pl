% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Near-Term Harms Reading of AI Safety Commitment
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   This constraint instantiates the near_term_harms_reading of the contested
 *   ai_safety_commitment kernel. It defines AI safety as the prevention of
 *   documented present-day harmsâbias, discrimination, labor exploitation,
 *   misinformationârather than speculative existential risk. In practice,
 *   this reading has been institutionally captured by tech corporations and a
 *   compliance industry that converts harm prevention into auditable,
 *   manageable, and non-binding programs. The result is a structure that
 *   coordinates genuine concern about algorithmic harm while asymmetrically
 *   extracting from the very populations it names as beneficiaries,
 *   channeling their grievances into metrics that legitimize the status quo.
 *
 * KEY AGENTS:
 *   - tech_corporations: Primary agenda-setter and beneficiary (institutional/arbitrage) â administers responsible AI programs and captures regulatory avoidance.
 *   - algorithmic_auditing_firms: Secondary beneficiary (organized/mobile) â extracts revenue from the technical auditing machinery.
 *   - marginalized_communities: Primary target (powerless/trapped) â bears algorithmic harm and provides data for corporate legitimacy without structural recourse.
 *   - gig_workers: Primary target (powerless/constrained) â subject to platform extraction framed as a fairness problem rather than a labor problem.
 *   - regulatory_agencies: Analytical observer (institutional/analytical) â could impose structural fixes but operates under political and informational constraints.
 *   - existential_risk_advocates: Excluded voice (organized/constrained) â competing definition of safety kept out of resource allocation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.72).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "Near-Term Harms Reading of AI Safety Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, 'f389571f-33d3-4f29-9119-339e70711013').
narrative_ontology:cs_kernel_codification('f389571f-33d3-4f29-9119-339e70711013', distributed).
narrative_ontology:cs_authority_grounding('f389571f-33d3-4f29-9119-339e70711013', expertise).
narrative_ontology:cs_interpretation_layer_present('f389571f-33d3-4f29-9119-339e70711013').
narrative_ontology:cs_reading_relation('f389571f-33d3-4f29-9119-339e70711013', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('f389571f-33d3-4f29-9119-339e70711013', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('f389571f-33d3-4f29-9119-339e70711013', foundational, safety_is_documented_harm_mitigation).
narrative_ontology:cs_axiom_status(safety_is_documented_harm_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('f389571f-33d3-4f29-9119-339e70711013', safety_is_documented_harm_mitigation, empirically_contingent).
narrative_ontology:cs_axiom('f389571f-33d3-4f29-9119-339e70711013', foundational, present_harm_precedence_over_speculative_risk).
narrative_ontology:cs_axiom_status(present_harm_precedence_over_speculative_risk, holdable).
narrative_ontology:cs_axiom_grounding('f389571f-33d3-4f29-9119-339e70711013', present_harm_precedence_over_speculative_risk, instrumental).
narrative_ontology:cs_reference_frame('f389571f-33d3-4f29-9119-339e70711013', documented_harm_prevention).
narrative_ontology:cs_drift_state('f389571f-33d3-4f29-9119-339e70711013', corporate_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f389571f-33d3-4f29-9119-339e70711013', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, tech_corporations).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, algorithmic_auditing_firms).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers corporate responsible AI programs, funds algorithmic auditing, and publishes transparency reports. Uses the near-term harms frame to preempt binding regulation by offering manageable, technical interventions that keep governance within corporate control. Could exit the constraint by accepting structural regulation but benefits from maintaining the current framing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_corporations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, tech_corporations, beneficiary).

% Sells bias assessment tools, fairness metrics, and compliance documentation to tech corporations. Revenue depends on the continued framing of AI safety as auditable, technical, and distinct from labor or antitrust law. Their services provide the operational machinery of the constraint.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, algorithmic_auditing_firms, beneficiary,
    organized, biographical, mobile, global).

% Bear the documented harms the constraint nominally addressesâalgorithmic discrimination, exclusion, surveillanceâwhile being channeled into participatory feedback loops and audits that do not alter underlying system design. Their lived experience is extracted as data for corporate legitimacy without transferring structural power.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_communities, payer,
    powerless, immediate, trapped, national).

% Subject to algorithmic management, wage suppression, and classification-as-contractor enforced by platform AI. The near-term harms framing captures their grievances into narrow fairness metrics while leaving the extractive business model intact. Exit is constrained by economic necessity and platform monopoly.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    powerless, immediate, constrained, global).

% Evaluate whether responsible AI frameworks suffice for consumer protection. Some push for harder rules; others accept corporate auditing as de facto compliance. They could fix the constraint by imposing structural obligations but face political and informational asymmetries.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).

% Hold a competing definition of AI safety centered on catastrophic and extinction risk. They are excluded from the near-term harms policy coalition because the framing allocates safety budgets and regulatory attention away from speculative alignment research and toward deployed-system auditing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, tech_corporations).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing documented, present-day harms from deployed AI systems by establishing standards, audits, and accountability mechanisms for bias, discrimination, labor exploitation, and misinformation.
% TRANSFER_FUNCTION: Moves regulatory pressure and public attention away from structural economic reform and toward technical, auditable interventions; moves the costs of algorithmic harm onto marginalized populations and gig workers while tech corporations and auditing firms capture the legitimacy and revenue of responsible AI programs.
% ABSENT_VOICES: Existential risk researchers and structural labor organizers are excluded from this framing. The former contest the narrowing of safety to present systems; the latter would argue for worker control and profit redistribution rather than algorithmic fairness metrics.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, corporate responsible AI programs would lose their legitimizing frame, regulatory attention would shift from technical audits toward structural economic remedies, the algorithmic auditing industry would contract, and affected communities would need to pursue redress through different institutional channels.
% FOUNDING_PROBLEM: Deployed AI systems were causing measurable, documented harms to marginalized groups and workers without accountability mechanisms, creating a legitimacy crisis for tech corporations and a policy demand for intervention.
% FOUNDING_PROBLEM_CORROBORATION: Affected communities and labor organizers attest the problem remains live. Competition authorities and independent academic researchers outside the benefiting parties document ongoing harms and attest the problem is live but that the current arrangement has been captured; tech corporations attest the problem is being adequately addressed through the existing framework.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint channels substantial resources into auditing and transparency infrastructure that is decoupled from structural harm reduction. Suppression (0.68) is high because the framing actively suppresses alternatives such as binding liability, worker classification reform, and antitrust intervention by occupying the policy space with technical solutions. Theater ratio (0.45) reflects that nearly half the activityâbias audits, ethics boards, transparency reportsâis performative, serving legitimacy rather than outcomes. Accessibility collapse (0.60) indicates that structural alternatives remain conceptually visible but are politically collapsed by the dominance of the responsible AI frame. Resistance (0.55) captures growing pushback from affected communities and some regulators who recognize the capture dynamic. The temporal series show monotonic increase in all three tracked metrics as corporate AI ethics matured from nascent legitimacy repair (2016) to institutionalized regulatory avoidance (2024).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (tech corporations, auditing firms) experience the constraint as genuine coordination: it solves the collective-action problem of maintaining industry legitimacy and managing regulatory pressure. The payer seats (marginalized communities, gig workers) experience the same structure as extraction: their harms are mined for data and legitimacy while the underlying systems remain intact. The engine computes this divergence from the structural asymmetry in exit options and power.
 *
 * DIRECTIONALITY LOGIC:
 *   Tech corporations are declared beneficiaries with arbitrage-grade exit and institutional power, placing them at the low-d beneficiary end; their effective extraction is damped or inverted into subsidy. Marginalized communities and gig workers are declared victims with trapped or constrained exit and powerless status, placing them at the high-d target end; their effective extraction is amplified. Auditing firms are beneficiaries with mobile exit, receiving moderate subsidy. Regulatory agencies sit near symmetric with analytical exit. The directionality is derived from beneficiary/victim declarations and exit modulation without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this constraint as either pure coordination (rope) or pure extraction (snare). There is a genuine coordination problemâdeployed systems do cause documented harmsâand some harm reduction does occur. However, the asymmetric capture of the framing by tech corporations and the compliance industry means someone is coordinated (populations receive some attention) and someone pays (same populations bear the costs of theater, and corporations avoid harder rules). If the coordination function were absent, this would be a snare; if the extraction were absent, it would be a rope. Both are present, so tangled_rope is the structurally faithful classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_ambiguity,
    'Has the near-term harms framing been captured by tech corporations to deflect binding regulation, or does it retain independent efficacy in reducing algorithmic harm?',
    'Comparative outcome analysis across jurisdictions with binding algorithmic accountability rules versus voluntary corporate auditing regimes, measuring actual harm reduction for affected populations.',
    'If captured, the constraint''s effective extraction is higher than its coordination function suggests and the computed type should lean snare-ward; if independent, the coordination function is genuine and tangled_rope remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_ambiguity, empirical, 'Whether corporate administration of near-term safety is capture or genuine coordination.').

omega_variable(
    coordination_extraction_boundary,
    'Are transparency and auditing requirements genuine coordination costs of harm prevention, or are they extractive overhead that obscures structural solutions?',
    'Natural experiment comparing harm outcomes under audit-centric governance versus structural interventions (e.g., worker classification, profit-sharing mandates, liability shifts).',
    'If auditing is separable from outcomes, much of the measured extraction is overhead; if inseparable, the extraction is partly the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether audit infrastructure is necessary cost or extractive theater.').

omega_variable(
    labor_exploitation_framing,
    'Does the constraint''s framing of labor issues as ''algorithmic fairness'' suppress the alternative framing of ''worker power and collective bargaining''?',
    'Discourse analysis of policy documents and stakeholder forums to measure the marginalization of labor-organizing demands relative to fairness-metric demands.',
    'If the fairness frame suppresses worker-power framing, suppression is higher than the structural measure suggests and the victim set is broader.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_exploitation_framing, empirical, 'Whether labor exploitation is handled through fairness metrics or structural power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_s_tr_t1, ai_safety_commitment__near_term_harms_reading, theater_ratio, 1, 0.25).
narrative_ontology:measurement(ai_s_tr_t2, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__near_term_harms_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__near_term_harms_reading, theater_ratio, 6, 0.41).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__near_term_harms_reading, theater_ratio, 8, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_s_be_t1, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(ai_s_be_t2, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 8, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ai_s_su_t1, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 1, 0.44).
narrative_ontology:measurement(ai_s_su_t2, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 8, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, dual_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_safety_commitment kernel, which decomposes into three structurally distinct claims: near_term_harms_reading, existential_risk_reading, and dual_priority_reading. Each reading has distinct beneficiary/victim structures, epsilon values, and empirical status. The epsilon-invariance principle requires separate stories because the observables differ: one tracks documented present-day harms, the other tracks speculative future risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
