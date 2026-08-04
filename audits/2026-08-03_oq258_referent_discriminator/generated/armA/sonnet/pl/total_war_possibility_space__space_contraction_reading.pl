% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   From 1945 onward, the general staffs of nuclear-armed states
 *   progressively stopped producing coherent plans for winning a general war
 *   against a peer nuclear state, not because such a war became more costly
 *   but because no doctrine could describe a path from initiation to a
 *   survivable, purpose-serving outcome. This reading treats that contraction
 *   as a structural fact about the possibility space itself — closer to a
 *   mountain (an emergent feature of physics-plus-strategy) than to a policy
 *   choice — while noting that the fact has identifiable beneficiaries (the
 *   epistemic communities and institutions that now organize themselves
 *   around the contracted space) and identifiable indirect payers
 *   (populations who absorb displaced conventional conflict). The claim is
 *   authored as mountain because the underlying physical fact (assured
 *   destruction capability) is not revisable by any single actor's choice;
 *   the metrics are authored independently and show real, if modest,
 *   extraction and rising theatricality in how the contraction is
 *   institutionally maintained.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.28).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.62).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear-Induced Contraction of the Total-War Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '79979534-cab5-4f28-9702-f3cd565830a1').
narrative_ontology:cs_kernel_codification('79979534-cab5-4f28-9702-f3cd565830a1', distributed).
narrative_ontology:cs_authority_grounding('79979534-cab5-4f28-9702-f3cd565830a1', distributed).
narrative_ontology:cs_reading_relation('79979534-cab5-4f28-9702-f3cd565830a1', total_war_possibility_space__deterrence_equilibrium_reading, influences).
narrative_ontology:cs_reading_relation('79979534-cab5-4f28-9702-f3cd565830a1', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('79979534-cab5-4f28-9702-f3cd565830a1', foundational, total_war_is_categorically_unplannable_between_nuclear_peers).
narrative_ontology:cs_axiom_status(total_war_is_categorically_unplannable_between_nuclear_peers, holdable).
narrative_ontology:cs_axiom_grounding('79979534-cab5-4f28-9702-f3cd565830a1', total_war_is_categorically_unplannable_between_nuclear_peers, empirically_contingent).
narrative_ontology:cs_axiom('79979534-cab5-4f28-9702-f3cd565830a1', secondary, possibility_space_exclusion_is_prior_to_and_independent_of_preference_ranking).
narrative_ontology:cs_axiom_status(possibility_space_exclusion_is_prior_to_and_independent_of_preference_ranking, holdable).
narrative_ontology:cs_axiom_grounding('79979534-cab5-4f28-9702-f3cd565830a1', possibility_space_exclusion_is_prior_to_and_independent_of_preference_ranking, empirically_contingent).
narrative_ontology:cs_reference_frame('79979534-cab5-4f28-9702-f3cd565830a1', pre_nuclear_decisive_battle_doctrine).
narrative_ontology:cs_drift_state('79979534-cab5-4f28-9702-f3cd565830a1', post_cold_war_contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('79979534-cab5-4f28-9702-f3cd565830a1', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states_general_staffs).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, arms_control_epistemic_community).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, great_power_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, conventional_and_sub_nuclear_war_planners).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, mutually_assured_destruction_forecloses_rational_general_war).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, escalation_dominance_is_structurally_unattainable_between_peer_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the residual doctrinal apparatus (targeting lists, command-and-control, deterrence signaling) but have quietly stopped producing genuine plans for winning a total war against a peer nuclear state, because no such plan can be made coherent. Their institutional relief comes from no longer having to solve an unsolvable planning problem; their institutional cost is a slow atrophy of large-scale mobilization competence that leadership sometimes worries about.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states_general_staffs, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states_general_staffs, agenda_setter).

% Academic and think-tank strategists whose entire professional field (deterrence theory, escalation ladders, arms control verification) exists because total war planning was displaced into a different register. They benefit from the constraint's persistence as the ground of their discipline's relevance, and have no plausible exit from treating it as real without dissolving their own field.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, arms_control_epistemic_community, beneficiary,
    organized, civilizational, analytical, global).

% Civilians of nuclear-armed states who benefit from the fact that a WWI/WWII-style general mobilization war between great powers is no longer being planned against them by their own or rival general staffs, in exchange for permanently living inside the residual risk of the arsenals that produced this contraction. They cannot exit the arrangement; their only lever is domestic political pressure on arsenal size and doctrine, not on the underlying physical fact.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_populations, beneficiary,
    powerless, biographical, trapped, global).

% Officers and doctrine writers whose careers were redirected out of general-war mobilization planning (which became institutionally pointless) into limited war, counterinsurgency, gray-zone, and cyber domains. They bear the reputational and budgetary cost of a downgraded specialty — 'total war planner' is no longer a viable career track — even though the redirection reflects a real structural fact rather than a policy choice they could reverse.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, conventional_and_sub_nuclear_war_planners, payer,
    moderate, biographical, constrained, national).

% States and populations that experience conventional and irregular war precisely because total war between the nuclear-armed patrons has become unthinkable — conflict is displaced downward onto them via proxy and limited engagements. They have no seat in the strategic-studies conversation that produced this contraction and cannot object to a framework built entirely around great-power capitals.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, non_nuclear_states_and_proxies, excluded,
    powerless, biographical, trapped, regional).

% Analysts who study whether the absence of total-war planning reflects a genuine collapse of the possibility space (this reading) or merely a high-cost deterrent equilibrium (a sibling reading) or a normative taboo (another sibling reading). They take testimony from doctrine documents, war-college curricula, and general-staff planning archives.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__space_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_possibility_space__space_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a coordination mechanism participants opted into. It names a physical/strategic fact: above a certain yield and delivery threshold, no rational path exists from initiating general war to a survivable, purpose-serving outcome, so the option is structurally removed from planning rather than merely disfavored.
% TRANSFER_FUNCTION: The arrangement transfers strategic attention and institutional capacity away from total-war mobilization doctrine and toward deterrence signaling, arms control, and sub-nuclear conflict; it also displaces the risk and incidence of actual warfighting downward onto non-nuclear states and proxies who absorb the conflict great powers no longer fight directly with each other.
% ABSENT_VOICES: Non-nuclear states and the populations of conflict zones where great-power rivalry plays out by proxy have no voice in a discourse authored almost entirely from nuclear-capital perspectives; they experience the downstream cost of the contraction (displaced conventional and irregular war) without having shaped or consented to it.
% DISAPPEARANCE_RATIONALE: If nuclear weapons themselves vanished, this reading predicts the possibility space would immediately re-expand: general staffs would resume total-war mobilization planning, war-gaming for great-power general war would restart, and strategic studies would re-center on conventional great-power conflict. A sibling reading (deterrence_equilibrium) would predict something similar for different reasons (deterrent balance collapses), while the taboo reading would predict total war remains largely unthinkable even without the weapons, since the norm has become independent of the material substrate. Which prediction is right is exactly the unresolved contest between these three readings.
% FOUNDING_PROBLEM: Between 1945 and roughly the early Cold War, strategists confronted the fact that existing total-war planning doctrine (mass mobilization, decisive-battle theory, victory-through-attrition) no longer described any survivable path to a purpose-serving outcome once thermonuclear exchange was possible between peer states.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by historians of Cold War strategic doctrine (e.g. documented shifts in NATO and Warsaw Pact war-college curricula away from general-war mobilization planning toward limited-war and deterrence studies) and by declassified general-staff planning archives showing the practical abandonment of victory-oriented general-war plans after the advent of secure second-strike capability; these sources are independent of the arms-control epistemic community that also benefits from the reading.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is low but non-zero (0.28) because while the core fact is not itself an extraction mechanism, the institutions organized around maintaining and interpreting the contraction (targeting bureaucracies, arms-control apparatuses, war-college curricula) do consume real resources and confer real career and status benefits on those who administer them. Suppression is moderate-high (0.62) and driven by the enforcement of official secrecy around targeting and command-and-control doctrine, and by the institutional discouragement of dissenting 'total war is still winnable' planning within general staffs — this is suppression of an internal planning discourse, not of the physical fact. Theater ratio rises steadily from 1945 (0.05) to 2025 (0.42) as the genuine planning problem that originally drove the contraction recedes into settled doctrine while the surrounding institutional apparatus (deterrence signaling exercises, symbolic force posture reviews) increasingly performs vigilance rather than solving a live planning problem — a Goodhart-style drift worth flagging even though the underlying claim remains a mountain. Accessibility collapse is high (0.88): once secure second-strike capability exists between peers, no alternative total-war doctrine remains coherently available to planners. Resistance is low (0.22): virtually no serious institutional actor argues for reviving total-war mobilization planning against a nuclear peer, though occasional revisionist strategic theorists (e.g. limited nuclear war/escalation-dominance advocates) have pushed back.
 *
 * DIRECTIONALITY LOGIC:
 *   General staffs and the arms-control epistemic community sit toward the beneficiary end: the contraction relieves them of an unsolvable planning problem and grounds an entire professional field, respectively. Great-power populations are also coded as beneficiaries (they are not targeted for general war) but are trapped with respect to exit — they cannot opt out of living inside the arsenals that produced the contraction, so their benefit is inseparable from irreducible residual risk. Conventional and sub-nuclear war planners are coded as payers: their career track was foreclosed by a structural fact they did not choose, and their institutional status was involuntarily downgraded. Non-nuclear states and proxies are the clearest indirect payers: conflict displaced downward by the contraction lands on them, but the story places them as excluded rather than payer proper because the transfer runs through geopolitics rather than through the constraint's own institutional machinery directly extracting from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (no survivable path existed from general-war initiation to purpose-serving victory once mutual thermonuclear capability existed) remains live by the reading's own lights — arsenals persist and the physics has not changed — so this is not a case of an obsolete mandate being defended by inertia. Where mandatrophy risk enters is in the rising theater_ratio: the apparatus built to manage a real, live problem increasingly performs vigilance (posture reviews, signaling exercises) beyond what the underlying planning problem requires, which is exactly the drift the classification should register as a symptom without it displacing the mountain classification of the underlying fact itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_gradient_exclusion,
    'Is the exclusion of total war from the strategic possibility space genuinely categorical (no coherent doctrine exists, full stop) or is it better described as an extremely steep cost gradient that functions as if categorical but remains, in principle, a matter of degree — which would collapse this reading into the deterrence_equilibrium_reading?',
    'Close reading of classified and declassified general-staff planning documents for any surviving ''total war is winnable under condition X'' contingency branch; absence of any such branch across all nuclear peer dyads over 80 years would support the categorical reading, while even isolated surviving contingency branches (e.g. limited nuclear war escalation-dominance doctrines) would support the gradient reading.',
    'If gradient rather than categorical, this story''s core claim collapses into the sibling deterrence_equilibrium_reading and the mountain classification would need re-examination — the constraint would look more like an extremely well-defended tangled rope (extraction dressed as necessity) than a natural-law-like contraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_gradient_exclusion, conceptual, 'Whether the possibility-space exclusion is truly categorical or an extreme point on a cost gradient.').

omega_variable(
    beneficiary_structure_and_false_summit_risk,
    'Given that identifiable institutions (general staffs, the arms-control epistemic community) benefit from the contraction''s persistence and administer its interpretation, is this constraint a genuine natural-law-like fact about strategic physics, or a constructed institutional settlement that benefits from being described as natural?',
    'Track whether strategic theorists who are NOT embedded in the beneficiary institutions (e.g. independent historians, dissenting military theorists outside war colleges) converge on the same categorical-exclusion description, or whether the categorical framing is disproportionately produced and reproduced by the institutions that benefit from it.',
    'If the categorical framing is disproportionately an artifact of the benefiting institutions'' own discourse, this constraint is a false-summit mountain candidate and should reclassify toward tangled_rope; if independent historians and dissenting theorists corroborate the categorical description, the mountain claim is more secure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_and_false_summit_risk, empirical, 'Whether declared beneficiaries indicate a constructed settlement dressed as natural law (FSM candidate).').

omega_variable(
    displaced_conflict_attribution,
    'How much of the actual increase in proxy and conventional conflict since 1945 is causally attributable to the contraction of the total-war possibility space among nuclear peers, versus other causes (decolonization, ideological rivalry, resource competition)?',
    'Comparative historical analysis of conflict incidence and location correlated with nuclear-peer relationship status, controlling for decolonization-era conflict baselines.',
    'A strong causal link would sharpen the payer/excluded characterization of non-nuclear states and proxies; a weak link would suggest the displacement narrative in transfer_function is overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_conflict_attribution, empirical, 'Causal weight of the space-contraction reading in explaining displaced conventional/proxy conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement_basis(tota_tr_t1975, observed).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__space_contraction_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement_basis(tota_tr_t1991, observed).
narrative_ontology:measurement(tota_tr_t2010, total_war_possibility_space__space_contraction_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement_basis(tota_tr_t2010, observed).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(tota_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.18).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.21).
narrative_ontology:measurement_basis(tota_be_t1975, observed).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1991, 0.24).
narrative_ontology:measurement_basis(tota_be_t1991, observed).
narrative_ontology:measurement(tota_be_t2010, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2010, 0.26).
narrative_ontology:measurement_basis(tota_be_t2010, observed).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.28).
narrative_ontology:measurement_basis(tota_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement_basis(tota_su_t1945, observed).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement_basis(tota_su_t1962, observed).
narrative_ontology:measurement(tota_su_t1975, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement_basis(tota_su_t1975, observed).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1991, 0.52).
narrative_ontology:measurement_basis(tota_su_t1991, observed).
narrative_ontology:measurement(tota_su_t2010, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement_basis(tota_su_t2010, observed).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(tota_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.05).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the total_war_possibility_space kernel, each authored as its own constraint with its own epsilon and metrics per the ε-invariance principle: deterrence_equilibrium_reading (total war remains reachable but deterred — a high-cost equilibrium, likely tangled_rope-adjacent), nuclear_taboo_reading (total war became normatively prohibited independent of material capability — a constructed-norm reading), and this space_contraction_reading (total war exits the thinkable space categorically). The three do not average into one epsilon; they are linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
