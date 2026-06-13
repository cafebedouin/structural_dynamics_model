% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__integrated_reading, []).

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
 *   constraint_id: ai_alignment_priority__integrated_reading
 *   human_readable: Integrated AI Alignment: Catastrophic Risk + Present Harms as Dual Governance Problem
 *   domain: ai_governance/technology_ethics
 *
 * SUMMARY:
 *   The integrated reading of AI alignment governance frames
 *   catastrophic-risk safety and present-harm prevention as complementary
 *   rather than competing priorities. This is structurally a tangled-rope
 *   constraint: it coordinates both existential-risk researchers and
 *   present-harms advocates around a dual methodology (red-teaming + audits,
 *   interpretability + accountability), but the coordination is
 *   asymmetric—deployment companies and AI institutions benefit from the
 *   ambiguity while marginalized populations bear the costs of living under
 *   unaccountable systems, and safety researchers bear the opportunity cost
 *   of divided institutional attention. The constraint is actively enforced
 *   through funding flows, publication norms, and policy mandates that
 *   require dual framing; its persistence depends on suppressing the zero-sum
 *   framing that would otherwise dominate (pure existential or pure justice).
 *   Extraction rises from 0.42 at interval start to plateau at 0.58 by
 *   midpoint, suggesting the integrated framing has become institutionalized
 *   (extraction stabilizes) but not yet routinized without friction (theater
 *   remains moderate, resistance stays high).
 *
 * KEY AGENTS:
 *   - Existential-risk researchers: Set catastrophic-risk agenda; institutional power, mobile exit, civilizational time horizon
 *   - Present-harms advocates: Frame justice concerns; organized but less powerful, constrained exit, biographical time horizon
 *   - AI deployment companies: Benefit from dual-framing ambiguity; institutional power, arbitrage exit, can structure compliance narrowly
 *   - Marginalized populations subject to deployed systems: Bear present harms without recourse; powerless, identity-locked, no institutional representation
 *   - Policy-makers and regulators: Must articulate integrated frameworks under resource constraints; institutional power, generational horizon, mobile institutional exit
 *   - Future generations: Affected by both catastrophic and present-harm consequences; powerless, trapped, no voice in current governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, 0.58).
domain_priors:suppression_score(ai_alignment_priority__integrated_reading, 0.62).
domain_priors:theater_ratio(ai_alignment_priority__integrated_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_alignment_priority__integrated_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__integrated_reading, "Integrated AI Alignment: Catastrophic Risk + Present Harms as Dual Governance Problem").
narrative_ontology:topic_domain(ai_alignment_priority__integrated_reading, "ai_governance/technology_ethics").

domain_priors:requires_active_enforcement(ai_alignment_priority__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__integrated_reading, '9d840ca2-34ac-44f2-9810-f98e5d12a8e0').
narrative_ontology:cs_kernel_codification('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', distributed).
narrative_ontology:cs_authority_grounding('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', distributed).
narrative_ontology:cs_reading_relation('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', ai_alignment_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_axiom('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', foundational, complementary_safety_methodologies).
narrative_ontology:cs_axiom_status(complementary_safety_methodologies, holdable).
narrative_ontology:cs_axiom_grounding('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', complementary_safety_methodologies, instrumental).
narrative_ontology:cs_axiom('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', foundational, plural_stakeholder_legitimacy).
narrative_ontology:cs_axiom_status(plural_stakeholder_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', plural_stakeholder_legitimacy, deontological).
narrative_ontology:cs_reference_frame('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', pre_institutionalization_coordination_gap).
narrative_ontology:cs_drift_state('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', contemporary_institutionalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9d840ca2-34ac-44f2-9810-f98e5d12a8e0', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__integrated_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, future_human_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, present_marginalized_groups).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__integrated_reading, ai_governance_institutions).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, present_marginalized_populations_bearing_harm).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, resource_constrained_ai_safety_teams).
narrative_ontology:constraint_victim(ai_alignment_priority__integrated_reading, communities_without_algorithmic_recourse).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__integrated_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__integrated_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the integrated framing transfers coordination burden onto safety researchers and marginalized populations without distributing the benefit (deployment companies retain autonomy while bearing only structured compliance costs; future generations benefit only if both agendas succeed). The measurement trajectory shows extractiveness rising steeply through institutionalization (0-15 year window) then plateauing, suggesting the frame has hardened into institutional practice. Theater is moderate (0.41 at end) because substantial red-teaming and audit infrastructure is real, but theater rises alongside extractiveness, indicating that as the frame becomes institutionalized, performative compliance increases—audits become theater for demonstrating 'integration' rather than genuine accountability. Suppression is high (0.62) because the integrated frame suppresses the zero-sum arguments that would otherwise dominate governance discourse; suppression_requirement rises as the field grows, indicating active work required to maintain the dual-methodology fiction against institutional pressure to prioritize one agenda. Accessibility_collapse is moderate (0.48) because actors within both research communities retain some ability to defect to pure existential or pure justice frames—the collapse is partial, not total. Resistance is high (0.71) because present-harms advocates and affected communities actively resist the frame's implicit resource-balancing assumptions (arguing present harms are competing, not complementary) and existential-risk researchers resist the reframing of their agenda as ethically obligated to serve justice concerns. The trajectory stabilization at t=15 reflects a possible institutional settlement, not consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the existential-risk researcher seat, the integrated frame is a genuine coordination achievement: catastrophic-risk work gains legitimacy and social license by committing to present-harm prevention. From the present-harms advocate seat, the frame is a co-option mechanism: existential arguments drain resources from justice work and use present-harms language to serve existential agendas. From the deployment-company seat, the frame is a compliance structure: both dual-methodology requirements are absorbed operationally while maintaining deployment timelines and profit margins. From the marginalized-population seat, the frame is irrelevant or worse—real accountability for present harms requires regulatory power to slow or halt deployment, which the integrated frame does not deliver. The engine will compute these seats' effective directionality differently: existential researchers and policy-makers likely compute as near-symmetric (benefit from legitimacy gains, bear coordination cost); deployment companies compute as near-beneficiary (compliance cost is manageable, institutional autonomy preserved); marginalized populations compute as victims (bear harms, gain only hypothetical future protection contingent on successfully preventing catastrophic risk). The gap is real and structural, not an authoring error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as future_human_populations (benefited by preventing catastrophic loss of control), present_marginalized_groups (benefited by preventing present harms), and ai_governance_institutions (benefited by access to a legitimizing dual frame). Victims are declared as present_marginalized_populations_bearing_harm (bear algorithmic harms without integration protecting them today), resource_constrained_ai_safety_teams (bear opportunity cost of dual-methodology requirement and funding fragmentation), and communities_without_algorithmic_recourse (excluded from governance conversations where the frame is negotiated). The directionality derivation should produce: existential researchers at d~0.25-0.35 (moderate beneficiary, mobility buffers them), present-harms advocates at d~0.55-0.65 (near-symmetric, they set agenda but face deprioritization), deployment companies at d~0.15-0.25 (moderate beneficiary, compliance costs are manageable), marginalized populations at d~0.85-0.95 (victims, identity-locked, no exit from the systems), future generations at d~0.80-0.90 (trapped victims, civilizational timescale makes exit impossible). No overrides are authored; the structural data should produce these values without intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The integrated reading avoids a trap: it prevents existential-risk research from becoming a mandatrophic constraint (where the founding problem—preventing catastrophic loss of control—becomes atrophied and replaced by performance of risk management). By insisting that existential work remain entangled with present-harm prevention, it forces continuous reality-testing: if existential methods diverge from accountability to present populations, the dual commitment surfaces the drift. Similarly, it prevents present-harms work from becoming mandatrophic by anchoring it to scalability and methodological rigor that existential-risk research provides. However, the integrated frame itself risks mandatrophy: once institutionalized, it can become theater—both agendas performed under an 'integration' label without genuine coordination of methods or resources. The rising theater_ratio from 0.25 to 0.41 flags this risk: institutionalization is being accompanied by increasing theatricality. Mandatrophy resolution would require empirical evidence that dual-methodology compliance actually produces better outcomes on both existential and present-harm metrics, or that the frame collapses when this evidence is absent. The commentary should note that if the founding problem (institutional zero-sum framing driving resource fragmentation) persists despite institutionalization of the integrated frame, the frame itself becomes a degraded constraint (piton-candidate) that serves primarily to absorb criticism without delivering integration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_vs_competition,
    'Are catastrophic-risk safety and present-harm prevention genuinely complementary methodologies, or do they compete for institutional attention and resources in ways the dual-framing obscures?',
    'Longitudinal study of funding allocation, researcher time budgets, and institutional priority shifts in AI safety organizations over 5-10 years. If integration requires continuous institutional work against zero-sum pressure, they remain substantially competitive; if the dual methodology becomes routine without friction, complementarity is vindicated.',
    'If competitive, the integrated frame is performative (tangled_rope theater increasing). If genuinely complementary, the frame enables both agendas to strengthen each other; theater should decline as complementarity becomes self-evident.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_vs_competition, empirical, 'Whether catastrophic and present-harm agendas strengthen or weaken each other under resource constraints.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the integrated reading foreclose the existential_risk_reading or the nearterm_harms_reading, or do all three remain live positions held by different parties?',
    'Mapping of institutional commitments: institutions that have formally adopted the integrated frame may still contain researchers who hold pure existential or pure justice framings internally. If the integrated frame is enforced as monopoly doctrine, foreclosure is occurring; if it coexists with pure framings within the same institutions, the readings coexist.',
    'If foreclosed, the integrated reading is more narrowly true (compels competing research to adopt its frame or exit); if coexisting, integration is achieved through negotiated resource-sharing rather than conceptual dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the integrated reading logically rules out sibling readings or allows them to coexist.').

omega_variable(
    accountability_mechanism_gap,
    'Does the integrated framing provide marginalized populations actual recourse to halt or slow harmful deployments, or does it provide only visibility and audit trails without decisional power?',
    'Analysis of regulatory frameworks that implement the integrated reading: do audits lead to deployment blocks/delays, or only to improved documentation and user disclosures? Do marginalized communities have veto or only comment authority?',
    'If recourse is structurally limited to visibility, the constraint extracts compliance theater from deployment companies while leaving harm-bearing communities powerless—tangled_rope or snare confirmed. If recourse includes halting authority, the constraint is more genuinely protective, shifting toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accountability_mechanism_gap, empirical, 'Whether integration produces material protection for harm-bearing populations or only transparency.').

omega_variable(
    existential_nearterm_boundary,
    'Is the boundary between ''existential risk'' and ''near-term harm'' stable, or do present systems (already deployed) carry existential-risk properties (e.g., large-scale coordination systems that could fail catastrophically)?',
    'Technical analysis of deployed systems'' interdependencies and failure modes: if present systems'' failure modes can cascade to civilization-scale disruption, the distinction dissolves and near-term harms ARE existential risks.',
    'If the boundary is unstable, the integrated framing is vindicated (the distinction was false). If the boundary is stable, the two readings address genuinely different timescales and the integration is temporal coordination rather than conceptual unification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_nearterm_boundary, empirical, 'Whether present systems carry existential-scale failure modes or are separated from civilizational risk.').

omega_variable(
    marginalized_population_representation,
    'In institutional settings where the integrated frame is operative, are marginalized populations whose harms motivate the present-harm agenda actually present in governance decisions, or are they represented through proxy advocacy?',
    'Audit of decision-making bodies in AI governance institutions adopting the integrated frame: percentage of seats held by representatives directly accountable to affected communities versus professional advocates speaking on their behalf.',
    'If proxy-only, the frame risks reproducing epistemic injustice (affected communities'' testimony filtered through professional interpretation). If directly present, the frame provides a seat for affected voices in catastrophic-risk governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_population_representation, empirical, 'Whether integration extends to including affected communities as direct participants or only as represented constituencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__integrated_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__integrated_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__integrated_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__integrated_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__integrated_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__integrated_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_a_tr_t25, ai_alignment_priority__integrated_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__integrated_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__integrated_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__integrated_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__integrated_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__integrated_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ai_a_be_t25, ai_alignment_priority__integrated_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__integrated_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__integrated_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__integrated_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__integrated_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__integrated_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(ai_a_su_t25, ai_alignment_priority__integrated_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__integrated_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__integrated_reading, 0.18).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__integrated_reading, ai_alignment_priority__nearterm_harms_reading).

% DUAL FORMULATION NOTE:
% The ai_alignment_priority kernel decomposes into three constraint stories representing competing institutional framings of what AI alignment fundamentally means. The existential_risk_reading frames alignment as catastrophic-risk prevention with moderate extractiveness on capability-research agendas. The nearterm_harms_reading frames alignment as justice-and-accountability with moderate extractiveness on affected communities' time and visibility. The integrated_reading (this story) frames alignment as requiring both agendas as complementary; it carries moderate extractiveness on both research communities and affected populations while attempting to dissolve the institutional zero-sum competition. The three readings coexist as live positions across different parties and institutions. Integration is not a synthesis that forecloses the siblings—it is a third institutional framing that coexists with both. Each reading has different victim sets, timescales, and methodologies, which is why they remain distinct constraints despite the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
