% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: AI Risk Governance Priority: Existential Risk Reading
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the existential-risk reading of the AI
 *   risk governance priority kernel. Under this reading, governance
 *   frameworks, funding mechanisms, and regulatory agendas are structured to
 *   treat superintelligence and human extinction as the overriding priority.
 *   The constraint coordinates global attention around a catastrophic future
 *   risk while asymmetrically extracting resources and legitimacy from
 *   present-harm mitigation efforts. Beneficiaries are x-risk research
 *   institutions and AI labs claiming safety leadership; costs are borne by
 *   communities suffering present algorithmic harms whose governance needs
 *   are deprioritized. The claim/metric independence is maintained: the
 *   constraint is claimed as tangled_rope (genuine coordination function plus
 *   asymmetric extraction) while metrics describe substantial extractiveness
 *   and rising theater as safety rhetoric decouples from present
 *   accountability.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions (beneficiary/institutional): Collect funding and mandate from the existential risk priority frame.
 *   - ai_labs_safety_leadership (beneficiary/institutional): Capture governance influence and regulatory deferral via safety claims.
 *   - near_term_harm_bearers (payer/powerless): Bear algorithmic bias, surveillance, and displacement without redress due to diverted attention.
 *   - ai_governance_regulators (agenda_setter/institutional): Administer the priority framework under expert pressure.
 *   - ai_ethics_near_term_advocates (excluded/organized): Pushed to the margins of priority-setting processes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.72).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.68).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "AI Risk Governance Priority: Existential Risk Reading").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '11a67ee7-1fae-48d8-b289-5ed239efd0a2').
narrative_ontology:cs_kernel_codification('11a67ee7-1fae-48d8-b289-5ed239efd0a2', formalized).
narrative_ontology:cs_authority_grounding('11a67ee7-1fae-48d8-b289-5ed239efd0a2', expertise).
narrative_ontology:cs_interpretation_layer_present('11a67ee7-1fae-48d8-b289-5ed239efd0a2').
narrative_ontology:cs_reading_relation('11a67ee7-1fae-48d8-b289-5ed239efd0a2', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('11a67ee7-1fae-48d8-b289-5ed239efd0a2', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('11a67ee7-1fae-48d8-b289-5ed239efd0a2', foundational, superintelligence_extinction_priority).
narrative_ontology:cs_axiom_status(superintelligence_extinction_priority, holdable).
narrative_ontology:cs_axiom_grounding('11a67ee7-1fae-48d8-b289-5ed239efd0a2', superintelligence_extinction_priority, empirically_contingent).
narrative_ontology:cs_axiom('11a67ee7-1fae-48d8-b289-5ed239efd0a2', secondary, alignment_control_imperative).
narrative_ontology:cs_axiom_status(alignment_control_imperative, holdable).
narrative_ontology:cs_axiom_grounding('11a67ee7-1fae-48d8-b289-5ed239efd0a2', alignment_control_imperative, instrumental).
narrative_ontology:cs_reference_frame('11a67ee7-1fae-48d8-b289-5ed239efd0a2', existential_security_reference).
narrative_ontology:cs_drift_state('11a67ee7-1fae-48d8-b289-5ed239efd0a2', post_large_scale_models_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11a67ee7-1fae-48d8-b289-5ed239efd0a2', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_harm_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive funding, legitimacy, and institutional mandate from the governance priority assigned to superintelligence and existential risk. Their research programs, conferences, and career pipelines depend on maintaining the salience of speculative AGI scenarios in policy and philanthropic allocation.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Leverage the existential risk frame to position their organizations as indispensable stewards of AGI development. They capture governance influence, safety investment, and regulatory deferral by claiming unique capability to manage extinction-level risks while continuing commercial scaling.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_safety_leadership, beneficiary,
    institutional, generational, arbitrage, global).

% Communities experiencing algorithmic bias, automated surveillance, and labor displacement who lack governance redress because regulatory attention and public funding are diverted toward speculative future risks. They cannot opt out of the systems that harm them and cannot access the governance channels that classify their harms as non-priority.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harm_bearers, payer,
    powerless, immediate, trapped, global).

% Set AI governance priorities under dominant expert framings that elevate superintelligence scenarios above present harms. They channel public funds and regulatory attention toward alignment research and AGI governance frameworks, constrained by the epistemic authority of the safety establishment.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_governance_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for present harms like bias, surveillance, and labor displacement but are systematically deprioritized in high-level governance agendas, funding competitions, and regulatory hearings shaped by the existential risk frame.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_ethics_near_term_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global attention and resource mobilization around a hypothesized future catastrophic risk, establishing shared evaluation frameworks for AI capability assessment and technical alignment research.
% TRANSFER_FUNCTION: Moves financial resources, regulatory attention, and epistemic authority from present harm mitigation to speculative alignment research and AGI governance infrastructures, from near-term harm bearers and public budgets to x-risk institutions and AI safety labs.
% ABSENT_VOICES: Near-term harm bearers in marginalized communities and Global South populations affected by algorithmic systems are not present in the rooms where risk priority is set; their absence is treated by the governance framework as evidence that their concerns are non-existential and therefore non-urgent.
% DISAPPEARANCE_RATIONALE: If the existential risk priority vanished overnight, x-risk institutions would lose their mandate and primary funding streams, AI labs would lose the safety-leadership legitimacy that justifies their governance role, and regulatory bodies would reallocate attention toward present harms; the global AI governance landscape would reorganize around demonstrated rather than speculative risks.
% FOUNDING_PROBLEM: The potential for artificial general intelligence to cause human extinction or permanent disempowerment, and the absence of technical or governance tools to prevent such an outcome.
% FOUNDING_PROBLEM_CORROBORATION: X-risk researchers and AI lab technologists attest to the founding problem from within the benefiting parties. Independent AI ethicists, affected community representatives, and near-term harms researchers contest the empirical basis and priority ordering; no party without institutional stake in AGI futures uniformly corroborates the founding problem's dominance over present harms.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.42 to 0.72 over the interval because the existential risk frame has progressively captured larger shares of AI governance budgets, regulatory headspace, and elite epistemic consensus, decoupling from the marginal cost of present-harm prevention. Suppression (0.68) reflects the active marginalization of near-term governance alternatives in funding competitions, policy hearings, and research agendas. Theater ratio rises to 0.52 because an increasing share of safety activityâgovernance frameworks for hypothetical AGI, adversarial testing of speculative capabilitiesâperforms priority without producing accountable present protections. Resistance at 0.55 captures sustained pushback from ethics communities and affected populations. Accessibility collapse at 0.60 indicates that alternative governance frames (near-term harms first) have become structurally harder to articulate within mainstream AI governance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (x-risk institutions, safety labs, regulators) experience the constraint as necessary survival coordination: they see a world-threatening problem and a legitimate mobilization to solve it. The payer seat (near-term harm bearers) experiences the same structure as abandonment and resource starvation: their documented harms are classified as non-priority, and the governance systems they might appeal to have been captured by a speculative frame. The excluded seat (near-term advocates) sees epistemic capture. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   x_risk_research_institutions and ai_labs_safety_leadership are structural beneficiaries: they receive funding, legitimacy, and governance influence, placing them at the low-d end of the spectrum. near_term_harm_bearers are structural payers: they bear the costs of diverted attention and unredressed harms, with trapped exit options placing them near the high-d end. ai_governance_regulators sit closer to symmetric or moderately beneficiary-aligned because they administer the constraint without personally capturing the rents, though their epistemic dependence on the safety establishment biases their position. ai_ethics_near_term_advocates are excluded from the directional flow entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mandatrophy mislabeling by preserving the genuine coordination functionâpreventing human extinction is a real collective-action problemâwhile insisting on the asymmetric extraction that rides on it. A snare classification would erroneously deny the coordination legitimacy; a rope classification would whitewash the resource capture and present-harm neglect; a scaffold classification would require a sunset clause that does not exist; a piton classification would mischaracterize the constraint as atrophied when it is actively growing; a mountain classification would falsely naturalize a contested policy choice. Tangled rope is the mandatrophy-resistant category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    x_risk_empirical_verification,
    'Does superintelligence pose a non-negligible probability of human extinction, and does that probability justify the current level of resource extraction from present governance?',
    'Track forecasting accuracy of AI capability trajectories, evaluate the frequency of extinction scenarios in rigorous models, and observe whether near-term harm mitigation funding falls as x-risk funding rises.',
    'If the empirical basis is weak, the coordination function collapses toward cover story and the constraint shifts toward snare; if the empirical basis is strong, the extraction may be the necessary price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(x_risk_empirical_verification, empirical, 'Uncertainty about the empirical basis for superintelligence extinction risk and its proportionality to governance resource allocation.').

omega_variable(
    victim_temporal_location,
    'Are the constraint''s costs borne by present near-term harm bearers, or are they borne by future generations who may never exist if the risk is realized?',
    'Comparative analysis of funding flows and policy attention: measure whether present governance neglect of bias, surveillance, and displacement is structurally caused by the x-risk priority frame.',
    'If present actors pay the cost, directionality is high and extraction is immediate; if only future humanity is at risk, the constraint''s extraction profile is harder to seat and may reclassify as a scaffold or rope depending on enforcement structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_temporal_location, conceptual, 'Ambiguity about whether the constraint extracts from present populations or externalizes risk onto the future.').

omega_variable(
    enforcement_naturalization,
    'Is the existential risk priority treated as an inevitable technical conclusion or as a contested policy choice?',
    'Discourse analysis of governance documents, funding calls, and regulatory statements to detect whether alternative priority framings are entertained or dismissed as non-expert.',
    'If naturalized, accessibility_collapse is higher than authored and resistance is systematically suppressed; if recognized as contested, the constraint remains actively enforced rather than self-evident.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_naturalization, conceptual, 'Whether the priority frame has achieved epistemic naturalization in governance discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_xrisk_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_xrisk_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_xrisk_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(ai_xrisk_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(ai_xrisk_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(ai_xrisk_tr_t25, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(ai_xrisk_tr_t30, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(ai_xrisk_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_xrisk_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ai_xrisk_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(ai_xrisk_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(ai_xrisk_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(ai_xrisk_be_t25, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(ai_xrisk_be_t30, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ai_xrisk_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_xrisk_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(ai_xrisk_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ai_xrisk_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(ai_xrisk_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(ai_xrisk_su_t25, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(ai_xrisk_su_t30, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
