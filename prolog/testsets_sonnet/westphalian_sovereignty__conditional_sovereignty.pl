% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Responsibility to Protect / Conditional Sovereignty Doctrine
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This story instantiates the conditional_sovereignty reading of the
 *   Westphalian sovereignty kernel: sovereignty is not unconditional but
 *   carries an implicit duty of protection toward a state's own population,
 *   and systematic failure to discharge that duty (mass atrocity, ethnic
 *   cleansing, genocide) legitimates external intervention that would
 *   otherwise violate non-interference norms. This is a distinct constraint
 *   from the sibling readings, not a different observable of the same one:
 *   absolute_sovereignty asserts intervention is categorically illegitimate
 *   regardless of domestic conduct (near-zero epsilon, near-Mountain framing
 *   from the sovereign-state seat); graduated_sovereignty ties the degree of
 *   sovereign authority continuously to state capacity and governance quality
 *   rather than gating on a discrete violation threshold. The 1999 Kosovo
 *   intervention, the 2005 World Summit adoption of R2P, and the 2011 Libya
 *   intervention are the doctrine's application history; the 2011-2018
 *   divergence in the data reflects the post-Libya backlash where major
 *   powers (notably Russia and China) hardened resistance to invoking the
 *   doctrine, treating it as a precedent for regime-change operations rather
 *   than protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.42).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Responsibility to Protect / Conditional Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '8366497d-c49e-4b20-a243-25b2c0a4489b').
narrative_ontology:cs_kernel_codification('8366497d-c49e-4b20-a243-25b2c0a4489b', distributed).
narrative_ontology:cs_authority_grounding('8366497d-c49e-4b20-a243-25b2c0a4489b', distributed).
narrative_ontology:cs_reading_relation('8366497d-c49e-4b20-a243-25b2c0a4489b', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('8366497d-c49e-4b20-a243-25b2c0a4489b', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('8366497d-c49e-4b20-a243-25b2c0a4489b', foundational, sovereignty_is_conditional_trust_not_unconditional_grant).
narrative_ontology:cs_axiom_status(sovereignty_is_conditional_trust_not_unconditional_grant, holdable).
narrative_ontology:cs_axiom_grounding('8366497d-c49e-4b20-a243-25b2c0a4489b', sovereignty_is_conditional_trust_not_unconditional_grant, deontological).
narrative_ontology:cs_axiom('8366497d-c49e-4b20-a243-25b2c0a4489b', secondary, systematic_violation_creates_discrete_intervention_threshold).
narrative_ontology:cs_axiom_status(systematic_violation_creates_discrete_intervention_threshold, holdable).
narrative_ontology:cs_axiom_grounding('8366497d-c49e-4b20-a243-25b2c0a4489b', systematic_violation_creates_discrete_intervention_threshold, conventional).
narrative_ontology:cs_reference_frame('8366497d-c49e-4b20-a243-25b2c0a4489b', post_westphalian_absolute_non_interference_norm).
narrative_ontology:cs_drift_state('8366497d-c49e-4b20-a243-25b2c0a4489b', post_libya_intervention_backlash, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8366497d-c49e-4b20-a243-25b2c0a4489b', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, interventionist_coalition_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_ngo_advocacy_networks).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, targeted_state_governments).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, weak_and_middle_power_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_intervened_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_intervened_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, human_rights_universalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over which situations are designated systematic human rights violations warranting intervention. Invoke the doctrine selectively against rivals and weaker states while shielding themselves and allies from the same standard through veto or diplomatic cover. Set the threshold for what counts as triggering intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Build coalitions to authorize or conduct interventions, gaining strategic access, resource positioning, or reputational capital as defenders of human rights. Bear little of the long-term cost of post-intervention reconstruction or instability, and can disengage when political costs rise.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, interventionist_coalition_states, beneficiary,
    powerful, biographical, mobile, global).

% Document abuses, build the evidentiary and moral case for intervention, and gain institutional standing, funding, and influence as the doctrine's operation validates their monitoring mandate. Do not bear military, economic, or political costs of the interventions they advocate for.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_ngo_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Face suspension of the sovereignty norm that would otherwise shield domestic conduct from external force, sanctions, or prosecution. Cannot appeal to sovereignty as a defense once designated a systematic violator, and have no symmetric mechanism to hold intervening powers to the same standard. Regime survival is directly at stake.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, targeted_state_governments, payer,
    moderate, biographical, trapped, national).

% Live under a doctrine they did not design and cannot invoke against powerful states, since enforcement runs through Security Council politics they cannot control. Their sovereignty is conditional in practice while great powers' sovereignty functions as effectively absolute, creating a structurally asymmetric application of the same rule.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, weak_and_middle_power_states, payer,
    powerless, generational, constrained, national).

% May be protected from the violations that triggered intervention, but also bear the direct costs of military action, sanctions regimes, state collapse, or prolonged instability that often follows intervention. Have no voice in whether, how, or when the doctrine is invoked on their behalf.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_intervened_states, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_intervened_states, beneficiary).

% Study the doctrine's application record, comparing invoked cases to non-invoked comparable cases, and assess whether the norm operates consistently or selectively along lines of geopolitical interest.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared international standard for when the international community may act to stop mass atrocities that a state's own government is committing or permitting, replacing ad hoc, purely unilateral justification with a claimed collective norm.
% TRANSFER_FUNCTION: Moves the presumption of non-interference away from designated violator states toward intervening coalitions; moves reputational and moral capital toward advocacy networks and intervening states; moves material and human costs of intervention and its aftermath onto targeted populations and weaker states generally.
% ABSENT_VOICES: Targeted state governments and, more critically, the civilian populations the doctrine claims to protect have no seat in deciding whether, when, or how it is invoked in their case. Weak and middle power states as a class have no equivalent doctrine they can invoke against powerful states that commit comparable violations.
% DISAPPEARANCE_RATIONALE: Interventionist states and advocacy networks would say the world rearranges badly — mass atrocities would proceed unchecked by external action. Targeted governments and many weak-state governments would say sovereignty norms would simply revert to their pre-1990s baseline, changing little in practice since the doctrine is invoked selectively and inconsistently already. Scholars are divided on whether its removal would reduce or merely relabel intervention.
% FOUNDING_PROBLEM: Cases such as Rwanda and Srebrenica demonstrated that absolute non-interference allowed genocide to proceed while the international community stood by citing sovereignty; the doctrine was built to create a normative trigger obligating action.
% FOUNDING_PROBLEM_CORROBORATION: UN-affiliated genocide prevention bodies and some independent human rights scholars attest the founding problem remains live and the doctrine has prevented or limited some atrocities. Independent legal scholars outside the advocacy and Security Council seats — along with governments of intervened states across multiple continents — attest that invocation correlates more strongly with geopolitical alignment of the target than with severity of violations, suggesting the doctrine now substantially serves selective power projection rather than the founding problem uniformly.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, contested).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits in the moderate 0.30-0.45 band per the expected structural delta: the doctrine genuinely creates a coordination mechanism against mass atrocity (real function) while systematically transferring the practical burden of the norm onto weaker and geopolitically disfavored states while leaving powerful states' sovereignty functionally undisturbed (real extraction). Suppression is moderate-high because once a state is designated a systematic violator, its own claim to sovereign non-interference is suspended by the doctrine's own logic, with no reciprocal mechanism it can invoke. Theater ratio rises across the interval (0.20 to 0.40) reflecting growing gap between the doctrine's stated universal application and its actual selective invocation along geopolitical lines — genuine atrocities without geopolitical interest attached (many African conflicts, Xinjiang, Chechnya) receive far less consistent invocation than atrocities involving strategically important or geopolitically opposed states.
 *
 * PERSPECTIVAL GAP:
 *   From the Security Council permanent members' seat, the doctrine looks like prudent, rare, carefully gated collective action — a Rope. From a targeted government's seat facing intervention while a geopolitically aligned state commits comparable violations with impunity, the same doctrine looks like a Snare wearing coordination language. The engine should register this divergence structurally: same constraint, different seats, different computed types.
 *
 * DIRECTIONALITY LOGIC:
 *   Permanent Security Council members and interventionist coalitions sit near the beneficiary end: they set the threshold, control the veto gate, and bear minimal reciprocal exposure since their own conduct is effectively shielded by veto power. Advocacy networks benefit through institutional validation without bearing intervention costs. Targeted governments and weak/middle powers sit near the target end: they cannot invoke the doctrine symmetrically and bear its coercive weight when designated. Civilian populations occupy a genuinely mixed position — the doctrine's stated beneficiary but also frequently a payer of intervention's collateral costs, which is why they carry both payer and beneficiary roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Rwanda/Srebrenica-style paralysis) was real and the coordination function it responds to has not disappeared — genocide and mass atrocity remain live risks, so this is not a pure mandatrophy case of an obsolete function persisting on inertia. But the mismatch between founding_problem_status (contested, trending toward selective application) and disappearance_verdict (contested) is the diagnostic signal: if the doctrine were purely coordination, invocation would track violation severity; the corroboration record instead shows invocation tracking geopolitical alignment, which is consistent with tangled coordination/extraction rather than either pure Rope or pure Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_designation_authority,
    'Who legitimately determines when ''systematic'' human rights violations have occurred sufficient to trigger the sovereignty exception, and is that determination itself capturable by the same powerful states positioned to benefit from intervention?',
    'Comparative analysis of invocation versus non-invocation across cases matched for violation severity, controlling for the geopolitical alignment of the target state with Security Council permanent members.',
    'If designation authority is substantially captured by intervening-coalition interests, the doctrine''s coordination claim is largely cover for selective extraction of sovereignty-suspension against geopolitically disfavored states; if designation tracks severity independent of alignment, the coordination function dominates and this reading sits closer to tangled_rope or even rope than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_designation_authority, empirical, 'Whether the trigger-designation mechanism is captured by beneficiary interests.').

omega_variable(
    kernel_reading_choice_ambiguity,
    'Is conditional_sovereignty the correct framing of the operative international legal norm, or does state practice actually track the graduated_sovereignty reading (a continuous capacity/legitimacy spectrum) more closely than a discrete threshold-triggered exception?',
    'Doctrinal and empirical review of ICJ opinions, Security Council resolution language, and state practice to determine whether sovereignty suspension in practice behaves as a binary gate (supports this reading) or a continuous function of state capacity/legitimacy assessments (supports the sibling graduated reading).',
    'If state practice better fits the graduated reading, this story''s discrete-threshold framing may be describing an idealized doctrinal statement rather than the operative constraint, and the true operative constraint would need to be authored as the graduated_sovereignty sibling instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice_ambiguity, conceptual, 'Whether the discrete-threshold framing or the continuous-spectrum framing better describes actual state practice.').

omega_variable(
    coordination_extraction_ratio_over_time,
    'Has the ratio of genuine atrocity-prevention coordination to geopolitically-motivated extraction shifted since the doctrine''s founding, or was selective application present from the start and only became visible after Kosovo and Libya provided contrasting cases?',
    'Historical review of pre-1999 sovereignty-exception advocacy and Security Council deliberation records to establish whether the founding intent was already understood as selectively applicable by its architects.',
    'If selectivity was foreseeable and accepted at founding, the doctrine was designed as tangled_rope from inception rather than degrading from a purer Rope; this changes the mandatrophy analysis from ''genuine function has partially atrophied into extraction'' to ''extraction was present at founding and coordination language was the cover story from the start''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_over_time, empirical, 'Whether selective application was foreseeable at the doctrine''s founding or emerged later.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(west_tr_t1997, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1997, 0.28).
narrative_ontology:measurement(west_tr_t2004, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2004, 0.35).
narrative_ontology:measurement(west_tr_t2011, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2011, 0.3).
narrative_ontology:measurement(west_tr_t2018, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2018, 0.42).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(west_be_t1997, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1997, 0.28).
narrative_ontology:measurement(west_be_t2004, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2004, 0.34).
narrative_ontology:measurement(west_be_t2011, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2011, 0.4).
narrative_ontology:measurement(west_be_t2018, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2018, 0.37).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2025, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(west_su_t1997, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1997, 0.34).
narrative_ontology:measurement(west_su_t2004, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2004, 0.38).
narrative_ontology:measurement(west_su_t2011, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2011, 0.45).
narrative_ontology:measurement(west_su_t2018, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, un_security_council_veto_power).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'sovereignty and intervention' under the westphalian_sovereignty kernel. absolute_sovereignty (near-Mountain, non-interference as near-inviolable) and graduated_sovereignty (continuous capacity/legitimacy function rather than discrete trigger) are separate constraint files with their own epsilon values, beneficiary/victim structures, and classifications. This file's epsilon (0.30-0.45, snare-leaning) should not be averaged or reconciled with the siblings' epsilon values — per the epsilon-invariance principle, they are structurally distinct constraints linked by network edges, not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
