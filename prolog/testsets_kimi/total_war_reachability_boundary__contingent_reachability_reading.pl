% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Technology-Dependent Total War Reachability Boundary (Contingent Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint instantiates the contingent_reachability_reading of the
 *   total_war_reachability_boundary kernel. It treats the boundary that
 *   currently prevents total war between great powers as a temporary scaffold
 *   dependent on the present technological equilibriumâprimarily nuclear
 *   deterrenceârather than a permanent mountain or a self-sustaining rope.
 *   The reading holds that the post-1945 contraction of total-war
 *   reachability is an atrophied capability (piton-like) that could reverse
 *   as emerging technologies destabilize deterrence. Beneficiaries are states
 *   investing in destabilizing technologies that exploit the transitional
 *   ambiguity; victims are global civilian populations who bear the
 *   existential risk of deterrence failure. The metrics are authored
 *   independently of the scaffold claim to preserve claim/metric
 *   independence.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states (agenda_setter/institutional/constrained): maintain the deterrence architecture
 *   - disruptor_states (beneficiary/powerful/mobile): invest in destabilizing tech and gain strategic leverage from the scaffold's erosion
 *   - civilian_populations (payer/powerless/trapped): bear existential risk with no exit
 *   - non_nuclear_weapon_states (excluded/organized/constrained): advocate for alternatives but lack decision power
 *   - strategic_studies_community (observer/analytical/analytical): interpretive layer analyzing stability and drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.48).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.52).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Technology-Dependent Total War Reachability Boundary (Contingent Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '9ed10153-0002-4a86-9cf0-c718005aa4d2').
narrative_ontology:cs_kernel_codification('9ed10153-0002-4a86-9cf0-c718005aa4d2', formalized).
narrative_ontology:cs_authority_grounding('9ed10153-0002-4a86-9cf0-c718005aa4d2', practice).
narrative_ontology:cs_interpretation_layer_present('9ed10153-0002-4a86-9cf0-c718005aa4d2').
narrative_ontology:cs_reading_relation('9ed10153-0002-4a86-9cf0-c718005aa4d2', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('9ed10153-0002-4a86-9cf0-c718005aa4d2', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('9ed10153-0002-4a86-9cf0-c718005aa4d2', foundational, reachability_contingent_on_tech).
narrative_ontology:cs_axiom_status(reachability_contingent_on_tech, holdable).
narrative_ontology:cs_axiom_grounding('9ed10153-0002-4a86-9cf0-c718005aa4d2', reachability_contingent_on_tech, empirically_contingent).
narrative_ontology:cs_axiom('9ed10153-0002-4a86-9cf0-c718005aa4d2', foundational, strategic_contraction_reversible).
narrative_ontology:cs_axiom_status(strategic_contraction_reversible, holdable).
narrative_ontology:cs_axiom_grounding('9ed10153-0002-4a86-9cf0-c718005aa4d2', strategic_contraction_reversible, empirically_contingent).
narrative_ontology:cs_reference_frame('9ed10153-0002-4a86-9cf0-c718005aa4d2', stable_deterrence_framework).
narrative_ontology:cs_drift_state('9ed10153-0002-4a86-9cf0-c718005aa4d2', contemporary_tech_disruption_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9ed10153-0002-4a86-9cf0-c718005aa4d2', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, disruptor_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals, command-and-control infrastructure, and deterrence doctrines that constitute the current reachability boundary. They actively manage arms control and strategic posture to preserve the scaffold. Their exit is blocked by mutual vulnerability and alliance commitments.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Invest in emerging technologiesâhypersonic glide vehicles, AI-enabled early warning, cyber strategic weapons, anti-satellite systemsâthat erode the stability of the deterrence scaffold. They benefit from transitional ambiguity because it creates windows for strategic advantage before a new equilibrium forms.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, disruptor_states, beneficiary,
    powerful, generational, mobile, national).

% Bear the existential risk of deterrence failure and the downstream effects of arms racing. They do not choose the scaffold but live under it; its collapse or failure would directly target them.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations, payer,
    powerless, civilizational, trapped, global).

% Are structurally excluded from the deterrence architecture despite being subject to its risks. They advocate for disarmament and alternative security frameworks but lack decision-making power over the reachability boundary.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_weapon_states, excluded,
    organized, generational, constrained, global).

% Analyze deterrence stability and technological disruption. They produce the interpretive layer that frames whether the boundary is a mountain, rope, or scaffold, but do not directly administer or pay for the constraint.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_studies_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contingent_reachability_reading, disruptor_states).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contingent_reachability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents total war between great powers by maintaining a technology-dependent reachability boundaryâcurrently nuclear deterrenceâthat makes total war mutually catastrophic or unwinnable under the present technological equilibrium.
% TRANSFER_FUNCTION: Moves existential risk from the deterrence architecture onto global civilian populations; moves strategic leverage toward states that invest in technologies capable of eroding the boundary.
% ABSENT_VOICES: Non-nuclear weapon states and anti-nuclear movements advocate for disarmament and alternative security architectures; they are structurally excluded from the deterrence decision-making architecture.
% DISAPPEARANCE_RATIONALE: If the technology-dependent boundary vanished overnight, total war would re-enter the feasible set; alliance structures would fracture, arms racing would accelerate, and the international order would reorganize around acute great-power conflict.
% FOUNDING_PROBLEM: The recurrence of catastrophic great-power total war, which nuclear technology made so destructive that a managed deterrence equilibrium became imperative.
% FOUNDING_PROBLEM_CORROBORATION: Strategic historians attest the founding problem was real in 1945; contemporary defense analysts corroborate that great-power war remains a live risk. Anti-nuclear advocates from outside the beneficiary set attest the scaffold is eroding and the solution lies in disarmament rather than managed deterrence.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the existential risk and resource burden imposed on civilian populations and non-beneficiary states by the deterrence scaffold. Suppression (0.52) captures the active enforcement of the boundary through nuclear posture and the marginalization of disarmament alternatives. Theater_ratio (0.42) acknowledges that deterrence involves performative signaling, though the underlying arsenals are real. Accessibility_collapse (0.65) registers that policy alternatives to deterrence have largely collapsed in great-power strategic discourse. Resistance (0.38) reflects anti-nuclear movements and ban-treaty advocacy. The measurement series show rising extraction and theater as technological disruption erodes the scaffold, consistent with the piton nature of the current contraction.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states experience the constraint as a necessary coordination mechanism preserving their security; disruptor states experience it as a transient obstacle to be leveraged; civilian populations experience it as an imposed existential risk with no compensating benefit. The engine will compute divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Disruptor_states are declared beneficiaries because they capture strategic leverage from the scaffold's transitional nature and the ambiguity of technological competition. Civilian populations are declared victims because they bear the catastrophic downside if the scaffold fails. Nuclear_weapon_states administer the constraint but are symmetrically constrained by mutual vulnerability; their directionality is nearer 0.5 than the beneficiary pole.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the boundary as a scaffold prevents mislabeling the current deterrence equilibrium as a permanent mountain (which would ignore technological reversal risk) or as a pure rope (which would ignore the active enforcement and existential cost asymmetry). The scaffold's sunset clause is implicit in technology-dependent reachability: the constraint is justified as transitional precisely because it is expected to erode. Mandatrophy is not yet resolved because the founding problemâpreventing great-power total warâremains live, but the arrangement's adequacy is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tech_reversal_uncertainty,
    'Will emerging technologies (AI, hypersonics, cyber) reverse the current contraction of total war reachability, or will deterrence adapt to maintain the boundary?',
    'Empirical tracking of strategic stability as destabilizing technologies mature; observation of whether arms control can keep pace.',
    'If reversal is likely, the scaffold classification is correct and the sunset clause is real; if deterrence adapts, the boundary may be more durable than the contingent reading assumes, shifting toward rope or mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tech_reversal_uncertainty, empirical, 'Uncertainty about whether technology will reverse the contraction of total war reachability.').

omega_variable(
    contraction_permanence_framing,
    'Is the current strategic contraction a permanent elimination of winnable total war (contraction reading), a durable coordination equilibrium (dropping reading), or a temporary technological piton (this reading)?',
    'Historical analysis of whether past strategic contractions have proven reversible, and whether nuclear deterrence is sui generis or technology-dependent.',
    'Resolution determines whether this constraint is a scaffold, rope, or mountain; the three readings are mutually exclusive classifications of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_permanence_framing, conceptual, 'Framing ambiguity across the three readings of the total war reachability kernel.').

omega_variable(
    destabilizer_benefit_verification,
    'Do states investing in destabilizing technologies actually benefit from the temporary deterrence scaffold, or do they merely exploit its erosion?',
    'Strategic trade analysis tracking whether disruptive investments yield net leverage gains or merely trigger reactive arms racing.',
    'If benefits are real, the extraction is asymmetric and the scaffold carries tangled-rope dynamics; if benefits are illusory, the beneficiary declaration requires revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(destabilizer_benefit_verification, empirical, 'Whether the declared beneficiary group structurally collects from the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_contingent_tr_t0, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(twrb_contingent_tr_t7, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 7, 0.33).
narrative_ontology:measurement(twrb_contingent_tr_t14, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 14, 0.39).
narrative_ontology:measurement(twrb_contingent_tr_t21, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 21, 0.46).
narrative_ontology:measurement(twrb_contingent_tr_t28, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 28, 0.52).
narrative_ontology:measurement(twrb_contingent_tr_t35, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 35, 0.58).

% Extraction over time
narrative_ontology:measurement(twrb_contingent_be_t0, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(twrb_contingent_be_t7, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 7, 0.36).
narrative_ontology:measurement(twrb_contingent_be_t14, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 14, 0.41).
narrative_ontology:measurement(twrb_contingent_be_t21, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 21, 0.46).
narrative_ontology:measurement(twrb_contingent_be_t28, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 28, 0.52).
narrative_ontology:measurement(twrb_contingent_be_t35, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(twrb_contingent_su_t0, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(twrb_contingent_su_t7, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 7, 0.47).
narrative_ontology:measurement(twrb_contingent_su_t14, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(twrb_contingent_su_t21, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 21, 0.57).
narrative_ontology:measurement(twrb_contingent_su_t28, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 28, 0.62).
narrative_ontology:measurement(twrb_contingent_su_t35, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 35, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three structurally distinct claims. This reading (contingent_reachability) asserts a technology-dependent scaffold; the contraction_reading asserts a permanent mountain; the dropping_reading asserts a durable rope. Each has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
