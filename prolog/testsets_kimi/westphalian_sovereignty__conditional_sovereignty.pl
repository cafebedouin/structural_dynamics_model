% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty: Human Rights Trigger for Intervention
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This is the conditional_sovereignty reading of the
 *   westphalian_sovereignty kernel: sovereignty is not absolute but
 *   contingent on human rights compliance. It stands in contrast to
 *   absolute_sovereignty (no external legitimacy for intervention) and
 *   graduated_sovereignty (spectrum-based capacity model). This reading
 *   constrains state autonomy by legitimizing external intervention when
 *   systematic violations occur, concentrating extraction on targeted states
 *   while benefitting intervention advocates and great powers.
 *
 * KEY AGENTS:
 *   - intervention_advocates: Primary beneficiary (organized/mobile) â gain influence and funding from the conditional framing.
 *   - human_rights_institutions: Agenda-setter and beneficiary (institutional/constrained) â set thresholds and expand jurisdiction.
 *   - great_power_interventionists: Agenda-setter (institutional/arbitrage) â control enforcement and capture geopolitical legitimacy.
 *   - targeted_states: Primary target (institutional/trapped) â bear autonomy loss when thresholds are invoked.
 *   - excluded_sovereigntists: Excluded voice (organized/constrained) â defend absolute sovereignty but lack forum access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.55).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty: Human Rights Trigger for Intervention").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, 'e081427f-42df-410a-a429-dabd932801f5').
narrative_ontology:cs_kernel_codification('e081427f-42df-410a-a429-dabd932801f5', formalized).
narrative_ontology:cs_authority_grounding('e081427f-42df-410a-a429-dabd932801f5', lineage).
narrative_ontology:cs_interpretation_layer_present('e081427f-42df-410a-a429-dabd932801f5').
narrative_ontology:cs_reading_relation('e081427f-42df-410a-a429-dabd932801f5', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('e081427f-42df-410a-a429-dabd932801f5', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('e081427f-42df-410a-a429-dabd932801f5', foundational, sovereignty_entails_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('e081427f-42df-410a-a429-dabd932801f5', sovereignty_entails_responsibility, deontological).
narrative_ontology:cs_axiom('e081427f-42df-410a-a429-dabd932801f5', foundational, systematic_violations_trigger_intervention).
narrative_ontology:cs_axiom_status(systematic_violations_trigger_intervention, holdable).
narrative_ontology:cs_axiom_grounding('e081427f-42df-410a-a429-dabd932801f5', systematic_violations_trigger_intervention, conventional).
narrative_ontology:cs_reference_frame('e081427f-42df-410a-a429-dabd932801f5', responsible_sovereignty_framework).
narrative_ontology:cs_drift_state('e081427f-42df-410a-a429-dabd932801f5', contemporary_selective_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e081427f-42df-410a-a429-dabd932801f5', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, human_rights_institutions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, great_power_interventionists).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, targeted_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Network of NGOs, academics, and diplomats promoting Responsibility to Protect and humanitarian intervention. They gain institutional access, funding, and normative influence when sovereignty is framed as conditional on human rights performance.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_advocates, beneficiary,
    organized, generational, mobile, global).

% ICC, UN Human Rights Council, and special rapporteurs whose mandates depend on the conditional sovereignty principle. They set investigative agendas and benefit from expanded jurisdiction when the norm is activated.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, human_rights_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, human_rights_institutions, beneficiary).

% Permanent UNSC members and allied powers that authorize interventions. They control threshold interpretation, gain legitimacy for geopolitical preferences, and retain opt-out capacity for themselves and allies.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, great_power_interventionists, agenda_setter,
    institutional, generational, arbitrage, global).

% States facing accusations of systematic human rights violations. They lose autonomy over domestic security and governance when the conditional threshold is invoked; cannot exit the international system without catastrophic costs.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, targeted_states, payer,
    institutional, immediate, trapped, national).

% Legal scholars and Global South diplomats defending absolute sovereignty. They are marginalized in human-rights-dominant forums and lack veto power over threshold-setting.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, excluded_sovereigntists, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents atrocity crimes and systematic human rights violations by making state sovereignty contingent on responsible conduct, creating a framework for collective response when domestic governance fails catastrophically.
% TRANSFER_FUNCTION: Transfers autonomy and legal immunity from sovereign states to international institutions and intervening powers when human rights thresholds are breached; moves obligation to comply from internal discretion to external judgment.
% ABSENT_VOICES: Absolute sovereignty proponents (many Global South states, realist jurists) and populations in targeted states who experience intervention as harm rather than protection are underrepresented in the advocacy discourse that sets the threshold.
% DISAPPEARANCE_RATIONALE: If the principle vanished, the ICC would lack jurisdictional logic, R2P would collapse into ad hoc power politics, and targeted states would reclaim full autonomy â the international legal order would revert toward absolute sovereignty norms.
% FOUNDING_PROBLEM: Post-Holocaust and post-Cold War atrocities (Rwanda, Balkans) revealed that absolute sovereignty shields mass atrocities from external response; the constraint was built to close that impunity gap.
% FOUNDING_PROBLEM_CORROBORATION: UN General Assembly R2P commitment (2005) attests the problem is live from the advocate seat. Targeted states and realist jurists attest the founding problem is exploited for geopolitical ends; independent international law scholars outside the advocacy network corroborate that selective enforcement has undermined the norm's legitimacy.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.38, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.38) because the constraint extracts autonomy only when thresholds are met, not continuously. Suppression is moderate (0.55): enforcement depends on UNSC political will, ICC referrals, and institutional machinery, not automatic operation. Theater is moderate (0.45): the 'never again' rhetoric and R2P commitments substantially outpace consistent enforcement, producing performative legitimacy. Accessibility collapse is moderate (0.50): absolute sovereignty remains a live alternative for many states. Resistance is high (0.70): targeted states and Global South coalitions actively contest the norm. The time series shows extraction rising with R2P institutionalization (2005) and Libya (2011), then slightly receding as geopolitical backlash and selectivity critiques accumulate.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (targeted states) experiences the constraint as a coercive stripping of autonomy; the beneficiary seats (advocates, institutions, great powers) experience it as legitimate humanitarian coordination. The engine computes this divergence from the structural data: targeted states are victims with trapped exit, while great powers are beneficiaries with arbitrage exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted states are full targets (victim + trapped exit â d near 1.0). Intervention advocates are beneficiaries with mobile exit (d near 0.0). Human rights institutions are beneficiaries but constrained (d low-moderate). Great powers are beneficiaries with arbitrage-grade exit, meaning their effective extraction is damped to near-zero or negative despite their institutional power, because the constraint subsidizes their geopolitical preferences.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the coordination function (preventing atrocities), the constraint would be pure power projection by great powers. Without the extraction (autonomy loss), it would be pure humanitarian coordination. The combination of a genuine coordination story with asymmetric, threshold-manipulable extraction makes this a snare: the coordination narrative is cover for selective autonomy stripping, and persistence depends on actively suppressing absolute-sovereignty alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the conditional sovereignty reading structurally distinct from its sibling readings to the degree that it warrants separate epsilon-invariant constraints?',
    'Examine whether the three readings produce non-overlapping beneficiary/victim structures and different enforcement mechanisms.',
    'If the readings are structurally distinct, the current decomposition is warranted; if not, they should be merged into a single constraint with a distributed kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Location of this constraint within the westphalian_sovereignty kernel family.').

omega_variable(
    threshold_selectivity_ambiguity,
    'Are human rights thresholds for intervention applied consistently across states, or does geopolitical selectivity convert the constraint into an instrument of powerful states?',
    'Statistical analysis of UNSC referral patterns, ICC prosecution selections, and military interventions against severity of violations and geopolitical alignment.',
    'High selectivity would confirm the snare classification with concentrated great-power capture; consistent application would support tangled_rope or rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_selectivity_ambiguity, empirical, 'Whether enforcement selectivity undermines the norm''s universality.').

omega_variable(
    populated_benefit_ambiguity,
    'Do civilian populations in states facing intervention benefit from the conditional sovereignty constraint, or are they instrumentalized as justification for autonomy extraction?',
    'Post-intervention outcome studies comparing civilian welfare under intervention vs. non-intervention counterfactuals, and discourse analysis of threshold-setting.',
    'If populations benefit, they should be coded as beneficiaries (lowering effective extraction); if harmed or ignored, they are excluded or victims, increasing measured extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(populated_benefit_ambiguity, empirical, 'Whether protection of populations is a genuine coordination function or a cover story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t6, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 6, 0.28).
narrative_ontology:measurement(west_tr_t12, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 12, 0.38).
narrative_ontology:measurement(west_tr_t18, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 18, 0.45).
narrative_ontology:measurement(west_tr_t24, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 24, 0.5).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(west_be_t6, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(west_be_t12, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(west_be_t18, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(west_be_t24, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(west_su_t6, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(west_su_t12, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(west_su_t18, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(west_su_t24, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% The westphalian_sovereignty kernel decomposes into three structurally distinct constraints. Absolute sovereignty (mountain/rope) has negligible extraction and no victims. Conditional sovereignty (this story, snare) extracts autonomy via threshold triggers. Graduated sovereignty (tangled_rope) distributes extraction across a capacity spectrum. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
