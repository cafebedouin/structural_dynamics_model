% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Nuclear-Induced Contraction of the Total-War Possibility Space
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   Since 1945, the possession of thermonuclear arsenals by multiple great
 *   powers is read here as having contracted the space of strategically
 *   thinkable state action, excluding total industrial war between
 *   nuclear-armed great powers as a planning category rather than merely
 *   disfavoring it on cost grounds. The evidentiary trace is institutional:
 *   general staffs progressively retired mobilization doctrine and total-war
 *   contingency planning, strategic studies migrated toward crisis stability,
 *   arms control, and sub-nuclear domains, and total-war war-gaming for
 *   great-power conflict effectively ceased as a live planning exercise.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states_governing_apparatus: institutional beneficiary of the freed planning bandwidth
 *   - general_staff_planning_bodies: payer bearing the atrophy of total-war institutional capacity
 *   - arms_control_epistemic_community: beneficiary whose disciplinary agenda tracks the claimed contraction
 *   - sub_nuclear_conflict_populations: excluded voices bearing the redirected strategic energy
 *   - historians_of_strategic_thought: analytical observers adjudicating the archival record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.18).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.12).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear-Induced Contraction of the Total-War Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '380a352e-8676-481a-b3ee-1379e3ab9970').
narrative_ontology:cs_kernel_codification('380a352e-8676-481a-b3ee-1379e3ab9970', distributed).
narrative_ontology:cs_authority_grounding('380a352e-8676-481a-b3ee-1379e3ab9970', distributed).
narrative_ontology:cs_reading_relation('380a352e-8676-481a-b3ee-1379e3ab9970', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('380a352e-8676-481a-b3ee-1379e3ab9970', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('380a352e-8676-481a-b3ee-1379e3ab9970', foundational, total_war_categorically_excluded_from_planning_cognition).
narrative_ontology:cs_axiom_status(total_war_categorically_excluded_from_planning_cognition, holdable).
narrative_ontology:cs_axiom_grounding('380a352e-8676-481a-b3ee-1379e3ab9970', total_war_categorically_excluded_from_planning_cognition, empirically_contingent).
narrative_ontology:cs_axiom('380a352e-8676-481a-b3ee-1379e3ab9970', secondary, institutional_atrophy_evidences_categorical_not_probabilistic_exclusion).
narrative_ontology:cs_axiom_status(institutional_atrophy_evidences_categorical_not_probabilistic_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('380a352e-8676-481a-b3ee-1379e3ab9970', institutional_atrophy_evidences_categorical_not_probabilistic_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('380a352e-8676-481a-b3ee-1379e3ab9970', pre_nuclear_total_war_planning_norm).
narrative_ontology:cs_drift_state('380a352e-8676-481a-b3ee-1379e3ab9970', post_cold_war_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('380a352e-8676-481a-b3ee-1379e3ab9970', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states_governing_apparatus).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, arms_control_epistemic_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, populations_of_nuclear_armed_states).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, general_staff_planning_bodies).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, categorical_war_exclusion_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The permanent security and defense bureaucracies of nuclear-armed states operate under a planning horizon in which great-power total war has been removed as an actionable contingency; this frees institutional bandwidth and legitimizes budget allocation toward sub-nuclear, hybrid, and regional planning instead of mobilization-scale total war doctrine. They did not construct the removal — they administer institutions shaped by it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states_governing_apparatus, beneficiary,
    institutional, civilizational, analytical, global).

% Strategic studies scholars, arms-control institutions, and think tanks whose intellectual and professional infrastructure is premised on the categorical unthinkability of total great-power war. Their research agendas, funding, and disciplinary prestige track the contraction of the possibility space; they benefit from the space having contracted, without having caused the contraction.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, arms_control_epistemic_community, beneficiary,
    organized, generational, analytical, global).

% Military general staffs that once maintained mobilization doctrine, industrial war-footing plans, and total-war contingency architecture for great-power conflict. That planning apparatus has atrophied — skills, doctrine, and institutional memory for total industrial war between great powers have degraded or been formally retired, a cost borne by the institution even though no external party extracts it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, general_staff_planning_bodies, payer,
    institutional, generational, constrained, national).

% Ordinary citizens who live under conditions where great-power total war has exited the realm of live strategic contingency, but who have no agency over whether this holds and remain exposed to the residual catastrophic risk (accident, miscalculation, proliferation) that the contraction does not eliminate.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, populations_of_nuclear_armed_states, beneficiary,
    powerless, civilizational, trapped, national).

% Populations in regions where conventional, proxy, and sub-threshold conflict continues or intensifies as strategic energy is displaced downward from the foreclosed total-war domain into contested sub-nuclear space. Their experience of violence is arguably a redirection effect of the contraction, but they have no voice in strategic-studies discourse framed around great-power total-war unthinkability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, sub_nuclear_conflict_populations, excluded,
    powerless, biographical, trapped, regional).

% Scholars who trace the actual doctrinal record — war-gaming archives, mobilization planning documents, general staff curricula — to determine whether total war left the planning space categorically or merely became deprioritized under cost-benefit reasoning. They supply the evidentiary basis for adjudicating this reading against its siblings.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, historians_of_strategic_thought, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a coordination mechanism but a claimed structural fact: that the physical capacity for mutual civilizational destruction removed a category of state action from the field of things that can be coherently planned for, as opposed to merely things that are costly to choose.
% TRANSFER_FUNCTION: No transfer of resources between named parties; what moves is institutional attention and planning capacity, redirected away from total-war doctrine and toward sub-nuclear strategic domains, arms control scholarship, and crisis-stability engineering.
% ABSENT_VOICES: Populations experiencing sub-nuclear and proxy conflict have no standing in a discourse organized around whether total war has become 'unthinkable' for great powers; the redirection of strategic energy into their conflicts is treated as a footnote to the main claim rather than as a cost of it.
% DISAPPEARANCE_RATIONALE: If the constraint (the categorical exclusion itself) were removed — i.e., if total war re-entered the space of live strategic contingency — general staffs would need to reconstitute mobilization doctrine and industrial war planning largely from historical archive rather than living institutional practice, which would take years and expose real capability gaps. Whether the world 'rearranges' or reveals that the underlying planning capacity was never truly gone (merely dormant) is exactly the empirical question the sibling readings dispute.
% FOUNDING_PROBLEM: The founding problem was the literal physical fact of thermonuclear arsenals capable of mutual and civilizational destruction on timescales too short for conventional deterrent calculation to operate as it had for prior weapons categories — a problem of category, not merely degree.
% FOUNDING_PROBLEM_CORROBORATION: Historians of strategic thought and declassified war-gaming archives (e.g., RAND Corporation studies, NATO SHAPE contingency records) attest that general-staff planning for great-power total war has measurably atrophied since the 1960s — a finding independent of the arms-control community's own framing. Whether that atrophy reflects categorical exclusion (this reading) or merely extreme deterred cost (the deterrence_equilibrium sibling) is precisely what the corroborating archival record does not settle on its own.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are authored low because this reading, if true, describes a structural fact about physical capability and planning cognition rather than an enforced arrangement with identifiable rent extraction — nobody collects a toll from the exclusion of total war from planning space. Accessibility collapse is authored very high (0.88) because the reading's core claim is precisely that alternatives (total-war planning as a live option) have collapsed almost completely, not merely become costly. Resistance is low because there is no organized constituency actively contesting the exclusion of total-war planning (in fact, the opposite: institutional energy has moved away from it). Theater ratio rises over the interval (0.05 to 0.42) tracking the increasing performative element in the surviving vestiges of total-war contingency planning — nuclear posture reviews, legacy continuity-of-government exercises — that persist as institutional ritual after the underlying planning function atrophied.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of general staff planning bodies, the contraction reads as institutional capability loss under a claim of category-level exclusion that may or may not hold if circumstances change (e.g., renewed great-power confrontation, arms control regime collapse). From the seat of the arms-control epistemic community, the same contraction reads as a stable, near-mountain-like structural achievement. The engine computing different per-seat classifications from this asymmetry is exactly the point — the FSM check applies here because a mountain claim with declared institutional beneficiaries requires scrutiny of whether the 'natural law' framing is itself doing legitimating work for those whose disciplinary and institutional position benefits from it being treated as settled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (governing apparatus, epistemic community) sit near the low end of directionality because the contraction frees their institutional attention and legitimizes their agendas without imposing a cost on them through this specific structure. General staff planning bodies are named as payers not because an external party extracts from them, but because the atrophy of an institutional capability is itself a structural cost borne internally — this is why the claim is authored as mountain rather than tangled_rope: there is no adversarial extraction relationship, only asymmetric distribution of institutional consequence. Sub-nuclear conflict populations are excluded rather than victims in the beneficiary/victim sense because the causal chain from 'total war became unthinkable' to 'sub-nuclear conflict intensified' is itself contested and not asserted as established fact by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status mismatch check is the load-bearing test here: founding_problem_status is authored as contested rather than dead, because whether the underlying problem (civilizational-destruction-scale capability) still holds is not in dispute, but whether the INSTITUTIONAL RESPONSE (categorical exclusion of total-war planning) still tracks a live structural fact or has become inertial theater riding on an unexamined assumption is exactly what the sibling readings and the corroborating historians dispute. This prevents the story from either uncritically certifying the contraction as permanent natural law or dismissing it as pure institutional theater — the mismatch is preserved as an open question rather than resolved by authorial fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_extreme_cost_ambiguity,
    'Is the exclusion of total war from the planning space genuinely categorical (a mountain — total war has left the field of coherent strategic cognition), or is it an extremely high but still commensurable cost that merely resembles categorical exclusion under current conditions (which would make this reading structurally identical to the deterrence_equilibrium_reading)?',
    'Archival analysis of classified and declassified general-staff planning documents, war-gaming records, and contingency doctrine across nuclear powers to determine whether total-war planning was formally retired as categorically inapplicable or merely deprioritized under resource allocation and probability-weighted cost-benefit reasoning that could in principle be revisited.',
    'If the exclusion is merely extreme-cost rather than categorical, this reading collapses into the deterrence_equilibrium_reading and the claimed_type should shift from mountain toward a constructed/contingent classification with material beneficiaries actively maintaining the framing; if genuinely categorical, the mountain claim is more defensible but the FSM concern about institutional beneficiaries still applies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_extreme_cost_ambiguity, conceptual, 'Whether total-war exclusion is a categorical fact or an extreme-cost equilibrium misread as categorical.').

omega_variable(
    false_summit_institutional_beneficiary_check,
    'Do the declared institutional beneficiaries (governing apparatus, arms-control epistemic community) have an interest in the contraction being perceived as a natural, mountain-like fact rather than a constructed or contingent arrangement — and does that interest shape how the founding-problem status is reported?',
    'Compare independent archival/historical corroboration (outside the beneficiary set) against the beneficiary communities'' own self-reporting of the contraction''s permanence; look for divergence in confidence levels and framing.',
    'If the corroboration from historians diverges substantially from the beneficiary communities'' framing, this strengthens the case that the mountain claim is partially a false summit sustained by institutional interest rather than a pure natural-law fact, supporting reclassification pressure toward tangled_rope under FSM logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_institutional_beneficiary_check, conceptual, 'Whether beneficiary institutions have incentive to overstate the naturalness/permanence of the contraction.').

omega_variable(
    displacement_causality_ambiguity,
    'Is the intensification of sub-nuclear, proxy, and regional conflict causally downstream of the contraction of the total-war possibility space (strategic energy redirected downward), or is it independently driven by decolonization, regional power vacuums, and other factors unrelated to nuclear deterrence dynamics?',
    'Comparative conflict-intensity analysis controlling for nuclear-power involvement versus non-nuclear-power involvement in regional conflicts across the interval, and process tracing of specific cases where great-power proxy involvement substituted for direct confrontation.',
    'If displacement causality holds, the sub_nuclear_conflict_populations stakeholder should arguably be reclassified from excluded toward victim status, which would require adding base_properties.victims and reconsidering the mountain claim under a tangled_rope structure with identifiable asymmetric cost-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_causality_ambiguity, empirical, 'Whether sub-nuclear conflict intensification is a genuine displacement effect of the contracted total-war space.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__space_contraction_reading, theater_ratio, 1991, 0.38).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.13).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1991, 0.16).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.17).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.1).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the total_war_possibility_space kernel. deterrence_equilibrium_reading holds that total war remains reachable but is deterred by mutual vulnerability (a cost-based framing); nuclear_taboo_reading holds that total war became normatively prohibited through constructed taboo independent of material capability (a norms-based framing); this story (space_contraction_reading) holds that total war exited the planning space categorically (a cognition/possibility-space framing). Each carries its own ε, its own claimed_type, and its own stakeholder structure — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
