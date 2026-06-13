% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: Existential AI Alignment Priority Framework
 *   domain: technology/governance/existential_risk
 *
 * SUMMARY:
 *   The existential-risk reading of AI alignment frames the primary safety
 *   constraint as preventing catastrophic loss of control over sufficiently
 *   advanced AI systems. This reading treats alignment as a
 *   civilizational-scale problem requiring long-horizon, capability-focused
 *   research. It competes with the nearterm_harms_reading, which prioritizes
 *   present algorithmic discrimination and extractive harms affecting
 *   marginalized populations now. It also competes with the
 *   integrated_reading, which claims both catastrophic and present harms
 *   demand joint priority. This constraint story models the existential-risk
 *   reading as a tangled rope: it performs genuine coordination (unifying
 *   safety research around a shared threat model) while extracting
 *   (redirecting resources away from near-term justice work). The claim and
 *   metrics are intentionally divergent: the reading CLAIMS to be rope (the
 *   beneficiaries frame it as coordination) while the authored metrics show
 *   substantial extraction and high theater (capability research advancing
 *   under safety justification). The engine measures this divergence; do not
 *   reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - existential_risk_researchers: institutional agenda-setters; define alignment canonically; control funding allocation and publication norms
 *   - capability_research_institutions: beneficiaries; receive resources under existential-risk umbrella; justify frontier capability development as safety-necessary
 *   - marginalized_populations: powerless victims; experience present algorithmic harms; excluded from resource priority discussions; their voice is suppressed by the existential framing
 *   - near_term_safety_advocates: moderate-power payers; argue for present justice; constrained by being reframed as 'myopic' relative to existential urgency
 *   - future_humanity: non-agent beneficiary; serves as legitimating fiction for resource transfers to present institutions
 *   - deployed_ai_system_users: powerless victims; experience present harms; trapped outside the constraint's logic because their suffering is particular and immediate, not existential
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.72).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential AI Alignment Priority Framework").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "technology/governance/existential_risk").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '935c7dc6-7ec6-425c-b4dc-765a12afede2').
narrative_ontology:cs_kernel_codification('935c7dc6-7ec6-425c-b4dc-765a12afede2', distributed).
narrative_ontology:cs_authority_grounding('935c7dc6-7ec6-425c-b4dc-765a12afede2', extraction).
narrative_ontology:cs_interpretation_layer_present('935c7dc6-7ec6-425c-b4dc-765a12afede2').
narrative_ontology:cs_reading_relation('935c7dc6-7ec6-425c-b4dc-765a12afede2', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('935c7dc6-7ec6-425c-b4dc-765a12afede2', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('935c7dc6-7ec6-425c-b4dc-765a12afede2', foundational, existential_catastrophe_civilizational_priority).
narrative_ontology:cs_axiom_status(existential_catastrophe_civilizational_priority, holdable).
narrative_ontology:cs_axiom_grounding('935c7dc6-7ec6-425c-b4dc-765a12afede2', existential_catastrophe_civilizational_priority, instrumental).
narrative_ontology:cs_axiom('935c7dc6-7ec6-425c-b4dc-765a12afede2', foundational, present_harms_secondary_to_extinction_prevention).
narrative_ontology:cs_axiom_status(present_harms_secondary_to_extinction_prevention, holdable).
narrative_ontology:cs_axiom_grounding('935c7dc6-7ec6-425c-b4dc-765a12afede2', present_harms_secondary_to_extinction_prevention, deontological).
narrative_ontology:cs_reference_frame('935c7dc6-7ec6-425c-b4dc-765a12afede2', loss_of_control_as_primary_alignment_threat).
narrative_ontology:cs_drift_state('935c7dc6-7ec6-425c-b4dc-765a12afede2', contemporary_deployment_harm_recognition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('935c7dc6-7ec6-425c-b4dc-765a12afede2', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, capability_research_institutions).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, near_term_safety_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the resource transfer (away from deployed-system fairness, toward frontier capability research) is justified by speculative future scenarios rather than demonstrated present harms. The beneficiaries (capability researchers, frontier labs) collect resources; the victims (marginalized populations, near-term advocates) lose priority access. Suppression is high (0.72) because the existential framing actively excludes alternative priority models from institutional decision-making—near-term harm research is labeled important but subordinate. Theater is high (0.58) and rising: red-teaming and alignment testing generate genuine safety-relevant insights, but those same activities advance capability research and frontier system capabilities. The rising theater trajectory (0.42 -> 0.58) indicates increasing share of activity devoted to framing capability research as safety work rather than actually constraining capability. The measurement series spans 25 years (projected to ~2050) and tracks the constraint's intensification as capability research scales and the existential-framing institutional infrastructure matures. All three metrics share one time grid, authored at every point to enable temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional beneficiary seat, this is rope: we coordinate on catastrophic-risk prevention and everyone benefits from the safety infrastructure. From the marginalized-population seat, this is snare: the priority hierarchy suppresses attention to our present harms, resources flow away from deployed-system fairness, and we are told our suffering is less important than hypothetical future catastrophes. From the near-term-advocate seat, this is tangled rope: there is genuine coordination benefit (we all want aligned AI systems), but we pay the cost of being deprioritized and our research agenda is constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential-risk researchers: d ≈ 0.2-0.3 (beneficiaries with institutional power and mobility; the constraint subsidizes their research). Capability institutions: d ≈ 0.15 (direct beneficiaries; they set agendas and collect extracted resources). Marginalized populations: d ≈ 0.85-0.90 (full targets; powerless, trapped, bearing present costs for speculative future benefits; no exit except political reorganization). Near-term safety advocates: d ≈ 0.65 (constrained payers; moderate power lets them mount resistance, but suppression of their research agenda is active and successful). Future humanity: non-agent, d does not apply. Policy makers: d ≈ 0.5 (symmetric; they gain information and priority guidance, but their legitimacy depends on the existential framing and they lose autonomy over risk-prioritization criteria). Deployed-system users: d ≈ 0.88 (full targets; powerless, immediate-horizon, trapped; their harms are categorically excluded from the alignment definition). The directionality profile shows asymmetric concentration of extraction on powerless, trapped, immediate-horizon agents (marginalized populations, deployed-system users) while beneficiaries are institutional and civilizational-horizon actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The existential-risk reading faces a mandatrophy question: is the founding problem (loss of control over advanced AI) still live or has it been superseded by the present problem (deployed harms)? The constraint's answer: the founding problem is live and civilization-scale, therefore present harms are acceptable costs. The near-term-reading answer: the founding problem is speculative and the present harm problem is now primary. This mandatrophy divergence is unresolvable inside the existential-risk reading's own framework because the reading axiomatically prioritizes civilizational over biographical time horizons. The classification prevents mislabeling this as pure rope (which would require absence of victims and active enforcement) and catches the asymmetry: the constraint coordinates safety research (rope function) while extracting resources away from justice (snare function), held together by suppression of the integrated reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_speculative,
    'Are loss-of-control catastrophe scenarios for advanced AI systems sufficiently probable and near enough in time to justify prioritization over demonstrable present harms?',
    'Long-horizon forecasting calibration against actual AI capability timelines and failure modes. Comparison of extinction probability estimates across independent forecasters and disciplines. Analysis of whether catastrophe scenarios rest on empirically grounded threat models or theoretical worst-case assumptions.',
    'If existential scenarios are lower probability or farther horizon than claimed, the victim set and resource allocation shift toward present harms. If they are higher probability, the prioritization holds. This is the crux separating the existential_risk_reading from the nearterm_harms_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_vs_speculative, empirical, 'Probability and timeline of existential loss-of-control scenarios versus present harm timescales').

omega_variable(
    suppression_mechanism_structural,
    'Is the measured suppression (0.72) structural — resource scarcity making near-term harm work literally impossible — or internalized — near-term harm advocates have accepted existential risk as more legitimate?',
    'Post-priority-shift scenario: if funding and venues were reallocated to near-term harm research, would advocacy momentum resume (structural suppression) or would the field remain deprioritized (internalized)? Historical comparison with suppressed research areas that later regained prominence.',
    'If structural, the constraint''s effectiveness depends on active enforcement of priority hierarchies; if internalized, the victims have adopted the beneficiaries'' framework and the suppression is self-sustaining even if enforcement relaxed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural, empirical, 'Whether suppression of near-term harm research is structural or internalized').

omega_variable(
    undifferentiated_future_beneficiary,
    'Is ''all of humanity'' / ''future humanity'' a meaningful beneficiary for structural analysis, or does the existential reading hide a more specific beneficiary (capability researchers, frontier labs, particular institutional actors)?',
    'Track resource flows under the existential-risk framing: who actually receives funding, who sets research agendas, whose careers advance? If benefits concentrate on frontier researchers and capability labs, the declared beneficiary is future humanity but the structural beneficiary is present-day institutional actors.',
    'If future humanity is the actual beneficiary, the constraint is genuine long-horizon altruism. If actual benefits concentrate on institutional actors, the constraint is extractive (beneficiaries extract resources by framing it as serving future interests). This feeds the false-summit detection (natural law vs. constructed claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(undifferentiated_future_beneficiary, empirical, 'Whether beneficiary is genuinely undifferentiated future or disguises specific present institutional receivers').

omega_variable(
    reading_boundary_robustness,
    'Is the boundary between existential-risk reading and integrated reading stable under institutional pressure, or would merging them (equally prioritizing both catastrophic and present harms) dissolve the constraint''s enforcement structure?',
    'Test whether the existential-risk reading can coexist with integrated harm prioritization in the same institutional framework without losing resource control. If integration forces reorganization, the readings coexist but are genuinely incompatible in resource allocation terms; if integration is merely reframed as ''long-term alignment includes near-term justice'', the readings are rhetorical variants.',
    'If the readings coexist but control different resource streams, this is institutional division-of-labor. If merging them dissolves priority gatekeeping, the existential-risk reading''s enforcement structure depends on suppressing the integrated reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_robustness, conceptual, 'Whether existential and integrated readings can coexist without the existential reading losing institutional control').

omega_variable(
    red_teaming_methodology_capture,
    'Does the adversarial red-teaming methodology (testing systems against hypothetical future adversaries) reveal genuine alignment failure modes, or does it generate capability insights that advance the frontier systems it purports to test?',
    'Empirical tracking of red-teaming outputs: do they lead to fixes that constrain capability, or do they lead to capability improvements that better handle adversarial inputs? Independent analysis of whether red-teaming methodologies are primarily defensive or exploratory.',
    'If red-teaming primarily generates capability insights, the methodology itself is extractive: it transfers capability research under the label of alignment testing. This would make the constraint doubly extractive (resource transfer + capability advance). If red-teaming is genuinely defensive, the methodology is aligned with the framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(red_teaming_methodology_capture, empirical, 'Whether red-teaming methodology is defensive alignment testing or capability-advancing research mislabeled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__existential_risk_reading, theater_ratio, 5, 0.47).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.51).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__existential_risk_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_priority__existential_risk_reading, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__existential_risk_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__existential_risk_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_priority__existential_risk_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__existential_risk_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__existential_risk_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_priority__existential_risk_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__existential_risk_reading, 0.14).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% The ai_alignment_priority kernel decomposes into three structurally distinct constraints. Each reading instantiates a different ε (existential reading: high ε on speculative scenarios; nearterm reading: lower ε on measured present harms; integrated reading: moderate ε on both). The three stories form a family linked by network.affects_constraints because each reading's institutional success shapes the others' operating environment (resource competition, legitimacy battles, research agenda setting). They share a kernel (the contested definition of alignment) but diverge in victim sets, time horizons, and beneficiary structures. Do not interpret this family as 'three views of one constraint'—the ε-invariance principle requires treating them as distinct constraints with distinct metrics, victim relationships, and classifications. The decomposition is necessary because measuring alignment with a single observable (e.g., 'research publications') produces different ε values for each reading: what counts as evidence of alignment differs fundamentally across the three interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
