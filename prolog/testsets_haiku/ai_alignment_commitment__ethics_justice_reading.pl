% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_ethics_justice, []).

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
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment as Justice and Bias Prevention (Ethics Reading)
 *   domain: ai_governance/technology_ethics/social_justice
 *
 * SUMMARY:
 *   This constraint story instantiates the ethics-and-justice reading of AI
 *   alignment: alignment is fundamentally about preventing present-day
 *   reproduction of social bias and harm to marginalized populations. This
 *   reading emerged from documented failures of AI systems in criminal
 *   justice, lending, healthcare, and hiring — failures with immediate,
 *   verifiable consequences for specific people. The reading reframes
 *   'alignment' from a control problem (keeping advanced AI systems' future
 *   actions within human intent) to a justice problem (ensuring AI systems do
 *   not perpetuate or amplify existing inequalities). This is ONE reading of
 *   a contested kernel. The sibling readings — safety-control-reading and
 *   integrated-reading — offer structurally different definitions of
 *   alignment's primary success criterion, different victim sets, and
 *   different resource allocation implications. The claim and metrics are
 *   intentionally independent: the constraint is CLAIMED as tangled_rope
 *   (genuine coordination function + asymmetric extraction) while the metrics
 *   describe high extractiveness and suppression from the perspective of
 *   deprioritized technical-safety communities. The divergence is diagnostic.
 *
 * KEY AGENTS:
 *   - Marginalized communities experiencing algorithmic bias (powerless, trapped, primary beneficiaries under this reading)
 *   - AI ethics research institutions (organized, agenda-setters, benefit from justice-reading framing)
 *   - Technical AI safety researchers (powerful, high exit options, experience the reading as extractive resource redirection)
 *   - AGI control resource allocation institutions (institutional, constrained exit, forced to rebalance budgets under political pressure from justice framing)
 *   - Policy and regulatory bodies (institutional, pulled by both justice and control framings, but justice has higher political salience)
 *   - AI systems developers (excluded from coalition, their objections reframed as bad faith)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.68).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.71).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment as Justice and Bias Prevention (Ethics Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "ai_governance/technology_ethics/social_justice").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, 'ffce4e40-c655-40e8-8e4d-3d4201cad3e1').
narrative_ontology:cs_kernel_codification('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', distributed).
narrative_ontology:cs_authority_grounding('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', distributed).
narrative_ontology:cs_reading_relation('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', ai_alignment_commitment__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', foundational, present_harm_priority_over_speculative_risk).
narrative_ontology:cs_axiom_status(present_harm_priority_over_speculative_risk, holdable).
narrative_ontology:cs_axiom_grounding('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', present_harm_priority_over_speculative_risk, deontological).
narrative_ontology:cs_axiom('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', foundational, marginalized_population_centering).
narrative_ontology:cs_axiom_status(marginalized_population_centering, holdable).
narrative_ontology:cs_axiom_grounding('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', marginalized_population_centering, deontological).
narrative_ontology:cs_reference_frame('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', prior_alignment_neglect_of_bias).
narrative_ontology:cs_drift_state('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', contemporary_mainstreaming, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ffce4e40-c655-40e8-8e4d-3d4201cad3e1', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities_experiencing_current_bias).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ai_ethics_research_institutions).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, affected_populations_with_enforcement_capacity).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_research_programs).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, agi_control_resource_allocation).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, technical_safety_researchers_deprioritized).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68 at interval end) because the reading functions as a resource-transfer mechanism: every research dollar directed to bias mitigation is unavailable for existential-risk research, every policy focus on present-harm remediation is a competing priority against control-focused governance. Suppression is high (0.71) because the reading's political legitimacy makes it difficult for technical-safety researchers to contest the priority without being publicly characterized as indifferent to present suffering. Theater is moderate (0.42) and rising: as the reading becomes institutionalized, an increasing fraction of activity is performative affirmation of justice commitments rather than substantive bias remediation (conference statements, corporate diversity initiatives, policy documents) rather than material change in affected populations' outcomes. The measurement series show extractiveness plateauing around interval-end while theater continues rising — diagnostic of a constraint whose coordination function (unifying bias research) is stabilized while performative activity expands. Accessibility of alternatives collapses moderately (0.64): technical-safety researchers cannot easily abandon the justice-reading coalition without career cost, affected communities cannot exit the systems causing them harm. Resistance is high (0.78): technical-safety communities actively contest the reading, some affected populations dispute whether the justice reading adequately addresses their needs, and AI developers resist the implicit accusation of bad faith.
 *
 * PERSPECTIVAL GAP:
 *   The reading creates asymmetric perception: from the ethics-institution and affected-population seats, the arrangement is genuine coordination addressing real harms that were previously ignored. From the technical-safety seat, the same arrangement is extractive resource redirection driven by political salience rather than rational risk prioritization. The gap is structural and intentional — the justice reading's legitimacy depends on marginalizing the control-risk perspective. This is not a flaw in the reading; it is the reading's core mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities: low d (beneficiaries, though trapped, no exit), near beneficiary end. Ethics institutions: low d (agenda-setters, high exit to other fields, beneficiaries from the constraint). Technical-safety researchers: high d (payers, high exit but reputationally constrained, victims of resource redirection). AGI institutions: moderate-high d (payers, constrained exit because political pressure makes ignoring justice concerns untenable). Developers: moderate d (excluded, mobile exit, but strategic exclusion creates structural pressure). Policy bodies: symmetric to slightly high d (pulled in both directions, constrained exit, but justice reading has political advantage). The directionality overrides are unnecessary: the structural declarations (beneficiary/victim + exit + power) produce accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (present-day algorithmic bias causing documented harm) is demonstrably live. The founding function (unifying bias research and policy under a shared legitimacy frame) is active and functioning. However, the question of whether the justice reading structurally precludes simultaneous attention to control problems (the integrated-reading sibling) is unresolved. If it does preclude, then the justice reading extracts from future safety research not as an intended side effect but as a structural consequence. If it does not preclude, then the high extractiveness reflects resource competition rather than mutual exclusivity. This ambiguity is captured in omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    justice_control_structural_exclusivity,
    'Are present-bias remediation and long-term control research structurally exclusive in resource allocation and institutional legitimacy, or can they coexist?',
    'Empirical: track whether funding increase in one domain requires funding decrease in the other, or whether increased total research investment accommodates both. Institutional: assess whether policy frameworks can center both justice and control concerns without reframing one as secondary.',
    'If exclusive, the justice reading extracts from control research by necessity and the reading''s extractiveness is higher than measured (a structural feature, not a competition effect). If coexistent, the high extractiveness may reflect institutional capture rather than inherent conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justice_control_structural_exclusivity, empirical, 'Whether the justice and control readings require mutually exclusive resource allocation or can coexist.').

omega_variable(
    marginalized_population_actual_benefit,
    'Do the policy and research changes driven by the justice reading actually improve outcomes for marginalized populations experiencing algorithmic bias, or does the constraint primarily benefit ethics institutions and policy discourse?',
    'Comparative outcome tracking: measure bias reduction, access improvements, and harm reduction in systems deployed under justice-reading frameworks vs. baseline systems. Survey affected populations on whether the reading''s governance changes materially affect their situation.',
    'If actual benefit is high, the constraint is genuine coordination despite asymmetric extraction elsewhere. If benefit is low or diffuse, the constraint may be primarily theatrical and the beneficiary designation misplaced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_population_actual_benefit, empirical, 'Whether the justice reading produces material improvements in outcomes for its named beneficiaries.').

omega_variable(
    reading_axiom_coherence,
    'Does the justice reading''s axiom that present-harm prevention is the primary alignment criterion remain coherent if control problems manifest at timescales shorter than the justice reading assumes (e.g., if capability gains accelerate)?',
    'Conceptual: develop formal models of timescale interaction between bias-harm accumulation and control-problem emergence. Empirical: monitor AGI capability trajectories against projections; if timescales compress, re-evaluate whether the axiom holds.',
    'If timescales compress, the foundational axiom becomes foreclosed and the justice reading loses its structural grounding. If timescales remain as assumed, the axiom remains holdable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_axiom_coherence, empirical, 'Whether the justice reading''s axioms remain coherent under different timescale assumptions.').

omega_variable(
    excluded_developer_voice_suppression,
    'Is the structural exclusion of AI systems developers from the justice-reading coalition itself a form of extractive suppression, or a legitimate refusal to amplify conflicted interests?',
    'Examine whether developers'' technical objections to bias-remediation approaches (feasibility, tradeoff analysis, cost-effectiveness) are substantively engaged or dismissed as bad faith. Compare to comparable-domain cases where excluded stakeholders'' objections are taken seriously despite conflicts of interest.',
    'If suppression, the constraint''s suppression metric understates the true coercive mechanism; if legitimate exclusion, suppression reflects enforcement against resistant powerholders rather than marginalized silencing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_developer_voice_suppression, conceptual, 'Whether developer exclusion is principled boundary-maintenance or extractive suppression of dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 8, 0.67).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_commitment__ethics_justice_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'ai_alignment_commitment'. The ethics-justice reading defines alignment as preventing present-day social bias and harm; the safety-control reading defines alignment as preventing catastrophic loss of control; the integrated reading claims both are necessary and non-exclusive. These are structurally distinct constraints with different ε values, beneficiary sets, and extraction mechanisms. They share the same institutional domain (AI governance) and influence each other through resource competition and legitimacy allocation. This story links to both siblings via network.affects_constraints; the siblings link back to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
