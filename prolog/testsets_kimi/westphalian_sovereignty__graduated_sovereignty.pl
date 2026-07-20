% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty: Capacity-Based State Classification
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the graduated_sovereignty reading of the
 *   westphalian_sovereignty kernel. Under this reading, sovereignty is not
 *   absolute or merely conditional but exists on a spectrum determined by
 *   state capacity and governance legitimacy. External actorsâpowerful
 *   states and international institutionsâgain discretionary authority to
 *   classify, intervene in, and restructure weak states based on capacity
 *   metrics. Weak states and their populations bear the costs of this
 *   reclassification, losing autonomy while interveners expand their spheres
 *   of influence. The coordination story (matching assistance to capacity
 *   needs) operates as cover for neo-colonial extraction.
 *
 * KEY AGENTS:
 *   - hegemonic_interveners: Primary beneficiary and agenda-setter (powerful/global/arbitrage) â classifies states and gains intervention discretion
 *   - international_financial_institutions: Secondary beneficiary and operational agenda-setter (institutional/global/arbitrage) â administers capacity metrics
 *   - weak_states: Primary target (powerless/national/trapped) â loses autonomy through reclassification
 *   - civilian_populations: Secondary target (powerless/local/trapped) â bears intervention costs
 *   - westphalian_scholars: Excluded voice (moderate/global/constrained) â advocates absolute sovereignty reading
 *   - critical_legal_observers: Analytical seat (analytical/global/analytical) â documents extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.65).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.78).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty: Capacity-Based State Classification").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, 'df160062-6cfd-49dc-9129-fc2e714fe465').
narrative_ontology:cs_kernel_codification('df160062-6cfd-49dc-9129-fc2e714fe465', formalized).
narrative_ontology:cs_authority_grounding('df160062-6cfd-49dc-9129-fc2e714fe465', extraction).
narrative_ontology:cs_interpretation_layer_present('df160062-6cfd-49dc-9129-fc2e714fe465').
narrative_ontology:cs_reading_relation('df160062-6cfd-49dc-9129-fc2e714fe465', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('df160062-6cfd-49dc-9129-fc2e714fe465', westphalian_sovereignty__conditional_sovereignty, influences).
narrative_ontology:cs_axiom('df160062-6cfd-49dc-9129-fc2e714fe465', foundational, sovereignty_scales_with_capacity).
narrative_ontology:cs_axiom_status(sovereignty_scales_with_capacity, holdable).
narrative_ontology:cs_axiom_grounding('df160062-6cfd-49dc-9129-fc2e714fe465', sovereignty_scales_with_capacity, empirically_contingent).
narrative_ontology:cs_axiom('df160062-6cfd-49dc-9129-fc2e714fe465', foundational, external_assessment_legitimate).
narrative_ontology:cs_axiom_status(external_assessment_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('df160062-6cfd-49dc-9129-fc2e714fe465', external_assessment_legitimate, conventional).
narrative_ontology:cs_reference_frame('df160062-6cfd-49dc-9129-fc2e714fe465', capacity_determined_sovereignty).
narrative_ontology:cs_drift_state('df160062-6cfd-49dc-9129-fc2e714fe465', post_r2p_consensus, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df160062-6cfd-49dc-9129-fc2e714fe465', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, hegemonic_interveners).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classify states according to capacity and legitimacy metrics, gaining discretionary authority to intervene, condition aid, and reshape domestic governance in target states. Benefit from reduced constraints on coercive engagement and expanded spheres of influence.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, hegemonic_interveners, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, hegemonic_interveners, beneficiary).

% Develop and administer state fragility indices, governance scorecards, and conditional lending frameworks that operationalize the graduated spectrum. Derive institutional relevance, funding, and policy influence from their role as neutral arbiters of state capacity.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_financial_institutions, beneficiary).

% Subject to external classification as fragile, failed, or quasi-sovereign. Lose autonomy over domestic policy, security arrangements, and resource governance as interveners justify deep engagement based on capacity deficits. Cannot opt out of the international order that classifies them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).

% Bear the direct costs of intervention, statebuilding mandates, and governance restructuring imposed under the graduated framework. Rarely consulted in the classification decisions that determine their political status.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, civilian_populations, payer,
    powerless, immediate, trapped, local).

% Uphold the absolute sovereignty reading of the Westphalian kernel but are systematically excluded from policy discourse on intervention and statebuilding, which has shifted to capacity-based frameworks.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, westphalian_scholars, excluded,
    moderate, biographical, constrained, global).

% Analyze the structural relationship between classification metrics and intervention outcomes, documenting the correlation between state reclassification and resource extraction or geopolitical alignment.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, critical_legal_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, hegemonic_interveners).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claimed coordination of international assistance and intervention by matching external support to state capacity deficits, preventing state collapse and humanitarian crisis through graduated engagement.
% TRANSFER_FUNCTION: Transfers autonomy, policy discretion, and resource control from weak states and their populations to external interveners and international financial institutions, justified by capacity classification.
% ABSENT_VOICES: Post-colonial critics and absolute sovereignty advocates who reject external classification authority; populations in classified states who are not consulted in the reclassification process.
% DISAPPEARANCE_RATIONALE: If the graduated sovereignty framework vanished, the architecture of statebuilding, conditional sovereignty, and capacity-based intervention would collapse; weak states would reclaim autonomous governance, external interveners would lose their primary justification for coercive engagement, and international financial institutions would lose their classificatory authority.
% FOUNDING_PROBLEM: Post-Cold War state collapse and governance failure in weak states that appeared to threaten regional stability and humanitarian norms.
% FOUNDING_PROBLEM_CORROBORATION: Critical international legal scholars and targeted state diplomats attest that the founding problem of ungoverned spaces has been superseded by the framework's use as a vehicle for geopolitical competition and resource extraction; no independent corroboration from non-intervening parties confirms the problem remains live in the form the arrangement claims to solve.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the framework transfers substantial autonomy and resources from weak states to external interveners under the cover of capacity assessment. Suppression (0.78) is higher still because the constraint's persistence depends on actively excluding absolute sovereignty alternatives and suppressing post-colonial resistance. Theater ratio (0.62) reflects the performative statebuilding industryâgovernance metrics, capacity assessments, and intervention mandates that produce theatrical legitimacy without functional autonomy. Accessibility collapse (0.58) captures the partial but significant closure of absolute sovereignty alternatives in international discourse. Resistance (0.52) reflects persistent diplomatic and grassroots opposition from targeted states. The measurement series show monotonic extraction accumulation and theater growth over the interval, consistent with a coordination story being progressively captured by extraction.
 *
 * PERSPECTIVAL GAP:
 *   The hegemonic intervener seat experiences the constraint as a necessary governance tool that prevents state collapse; the weak state seat experiences it as arbitrary external domination that strips autonomy. The engine computes this divergence from the structural data: same constraint, opposite directionalities, different computed types per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Hegemonic interveners and international financial institutions are structural beneficiaries (low d): they gain discretion, resources, and authority from the classification system. Weak states and civilian populations are structural targets (high d): they lose autonomy and bear costs. Westphalian scholars are excluded from the discourse entirely. The high suppression value ensures the target seats remain trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims to solve a coordination problem (matching assistance to capacity) but shows the signature of pure extraction: concentrated beneficiaries, identified victims, no sunset clause, and persistent operation after the founding problem has been addressed or manufactured. If this were genuine coordination, weak states would be net beneficiaries of stabilized autonomy; instead they are persistent targets. The absence of any transition path to full sovereignty for classified states confirms this is not a scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Westphalian sovereignty kernel governs actual institutional behavior: absolute equality, conditional responsibility, or graduated capacity?',
    'Comparative legal analysis of UN Charter interpretation, intervention practice, and state diplomatic correspondence to determine which reading is structurally dominant.',
    'If absolute sovereignty governs, this constraint is an illegal snare violating a deeper mountain of sovereign equality; if graduated governs, the international system has transformed into a capacity-based hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity about which reading of the sovereignty kernel governs international order').

omega_variable(
    capacity_assessment_objectivity,
    'Are state capacity and governance legitimacy assessments objective technical measurements or geopolitical instruments?',
    'Statistical audit of state fragility indices and intervention decisions for correlation with interveners'' strategic interests rather than independent capacity metrics.',
    'If systematically biased, the coordination story is cover for extraction and epsilon is at the high end; if objective, the extraction may be coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_objectivity, empirical, 'Objectivity of capacity classification metrics').

omega_variable(
    structural_delta_absolute,
    'What would change structurally if the absolute_sovereignty reading governed instead of this graduated reading?',
    'Counterfactual analysis of intervention frequency, conditional lending, and state classification practices under an absolute sovereignty regime.',
    'Under absolute sovereignty, external classification would be categorically illegitimate, eliminating the snare''s enforcement mechanism and transferring autonomy back to weak states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_absolute, conceptual, 'Structural difference between graduated and absolute readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0, 0.28).
narrative_ontology:measurement(west_tr_t5, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 5, 0.35).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 10, 0.42).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 15, 0.5).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 20, 0.56).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 25, 0.6).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(west_be_t5, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 25, 0.64).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(west_su_t5, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 25, 0.77).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__graduated_sovereignty, conditional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is the graduated_sovereignty reading of the westphalian_sovereignty kernel, decomposing from the colloquial label 'sovereignty' into structurally distinct claims. See sibling constraints for the absolute and conditional readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
