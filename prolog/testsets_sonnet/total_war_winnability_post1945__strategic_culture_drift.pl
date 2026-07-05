% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War as Unreachable Strategic Category (Elite Discourse Atrophy)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'total_war_winnability_post1945.' The kernel asks: why has total war
 *   between great powers dropped out of reachable strategic space since 1945?
 *   Three structurally distinct claims answer this differently and are
 *   authored as three separate constraint stories, per the ε-invariance
 *   principle. THIS story instantiates the strategic_culture_drift reading:
 *   total war remains PHYSICALLY reachable (no structural bar, no binding
 *   legal prohibition) but has dropped from elite discourse because the
 *   professional field of strategic studies reorganized itself around
 *   limited-war and escalation-management paradigms, and the resulting
 *   institutional forgetting has atrophied the community's capacity to reason
 *   about it. This is a Piton: a formerly live planning capability whose
 *   function (readiness to think clearly about total war) has degraded
 *   through institutional inertia and career-incentive drift, not through
 *   active suppression or genuine obsolescence. Defense intellectuals who
 *   built careers on limited-war frameworks are the diffuse beneficiaries of
 *   the narrowed curriculum; the payer is the abstract capacity itself
 *   (strategic flexibility) and any future crisis planner who would need it.
 *   Sibling readings — structural_contraction_reading (nuclear weapons made
 *   total war structurally impossible, a Mountain-flavored claim) and
 *   normative_reading_drop (total war became normatively illegitimate via
 *   international law, a Rope/Tangled-Rope-flavored claim) — are NOT part of
 *   this story; they are separate constraints with separate ε values, linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - limited_war_defense_intellectuals: Primary beneficiary (institutional/identity_locked) — career and prestige built on the narrowed paradigm
 *   - arms_control_epistemic_community: Agenda-setter (institutional/identity_locked) — administers war-college curricula and journal gatekeeping
 *   - strategic_flexibility: Primary victim, non-agent (powerless/trapped) — the atrophying capability itself
 *   - future_crisis_planners: Secondary victim (moderate/trapped) — would inherit the atrophied capacity under pressure
 *   - national_security_bureaucracies: Excluded voice (organized/constrained) — retains operational caution not reflected in elite discourse
 *   - strategic_studies_historians: Analytical observer (analytical) — traces the genealogy of the category's disappearance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.42).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.31).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War as Unreachable Strategic Category (Elite Discourse Atrophy)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, 'bf6e4256-68f8-459f-835b-0b57f812b432').
narrative_ontology:cs_kernel_codification('bf6e4256-68f8-459f-835b-0b57f812b432', distributed).
narrative_ontology:cs_authority_grounding('bf6e4256-68f8-459f-835b-0b57f812b432', practice).
narrative_ontology:cs_interpretation_layer_present('bf6e4256-68f8-459f-835b-0b57f812b432').
narrative_ontology:cs_reading_relation('bf6e4256-68f8-459f-835b-0b57f812b432', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf6e4256-68f8-459f-835b-0b57f812b432', total_war_winnability_post1945__normative_reading_drop, influences).
narrative_ontology:cs_axiom('bf6e4256-68f8-459f-835b-0b57f812b432', foundational, strategic_capacity_atrophies_through_disuse_not_impossibility).
narrative_ontology:cs_axiom_status(strategic_capacity_atrophies_through_disuse_not_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('bf6e4256-68f8-459f-835b-0b57f812b432', strategic_capacity_atrophies_through_disuse_not_impossibility, empirically_contingent).
narrative_ontology:cs_axiom('bf6e4256-68f8-459f-835b-0b57f812b432', secondary, professional_field_incentives_shape_which_scenarios_remain_thinkable).
narrative_ontology:cs_axiom_status(professional_field_incentives_shape_which_scenarios_remain_thinkable, holdable).
narrative_ontology:cs_axiom_grounding('bf6e4256-68f8-459f-835b-0b57f812b432', professional_field_incentives_shape_which_scenarios_remain_thinkable, empirically_contingent).
narrative_ontology:cs_reference_frame('bf6e4256-68f8-459f-835b-0b57f812b432', cold_war_escalation_management_paradigm).
narrative_ontology:cs_drift_state('bf6e4256-68f8-459f-835b-0b57f812b432', post_cold_war_curriculum_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bf6e4256-68f8-459f-835b-0b57f812b432', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, arms_control_epistemic_community).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, future_crisis_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Career, funding streams, journal placements, and professional standing are built on limited-war, escalation-management, and crisis-stability frameworks (deterrence theory, graduated response, coercive diplomacy). Total-war planning is treated in their professional literature as archaic or unserious. They did not conspire to erase the category; the field simply stopped rewarding fluency in it, and their expertise is now defined by its absence from the mainstream curriculum.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, beneficiary,
    institutional, generational, identity_locked, global).

% Think tanks, treaty-verification bodies, and strategic-studies departments administer the intellectual infrastructure (war colleges, doctrine curricula, journal gatekeeping) that decides which strategic categories are taught and cited. They set the agenda for what counts as serious strategic thought and have quietly retired total-war planning from that agenda, both because it seems obsolete and because sustaining it would undercut the legitimacy of the limited-war paradigm they built their careers on.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, arms_control_epistemic_community, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, arms_control_epistemic_community, agenda_setter).

% The abstract capacity of the strategic-planning apparatus to reason clearly about total-war scenarios if circumstances required it. It is not an actor but a capability; it erodes silently as institutional memory, doctrine, and trained personnel who could reconstitute total-war thinking are not replaced. It cannot object to its own atrophy.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(total_war_winnability_post1945__strategic_culture_drift, strategic_flexibility).

% Military and civilian officials who would have to respond to a future crisis that escalates beyond the limited-war categories the field has spent decades refining. They inherit doctrine, training pipelines, and institutional muscle memory built almost entirely around escalation management and limited engagement; if a scenario outran those categories, they would have to reconstruct total-war planning largely from scratch, with no living professional community fluent in it.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, future_crisis_planners, payer,
    moderate, biographical, trapped, national).

% Operational planning staffs within militaries who might privately retain some total-war contingency planning but whose views rarely surface in elite civilian strategic discourse, journals, or war-college curricula. Their institutional caution about declaring total war 'unthinkable' is structurally excluded from the dominant academic conversation, which has moved on.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, national_security_bureaucracies, excluded,
    organized, generational, constrained, national).

% Scholars who trace the genealogy of strategic categories across the twentieth century. They can observe that total-war planning was a live, doctrinally elaborated field through the 1950s-60s and note its near-total disappearance from the curriculum and journal literature by the 1990s, without necessarily accepting either the structural-impossibility or normative-illegitimacy explanations for that disappearance.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The narrowing of elite strategic discourse to limited-war and escalation-management categories genuinely coordinates a shared professional vocabulary, doctrine curriculum, and crisis-communication grammar among defense intellectuals, policymakers, and allied militaries — enabling coherent deterrence signaling and crisis bargaining without each actor re-deriving first principles.
% TRANSFER_FUNCTION: The arrangement transfers institutional attention, funding, and professional prestige away from total-war contingency planning and toward limited-war/escalation-management scholarship, and transfers the latent cost of that narrowing onto whichever future crisis planners would need total-war reasoning capacity that the field no longer cultivates.
% ABSENT_VOICES: Operational military planners who retain some total-war contingency doctrine privately, and historians of the 1950s-60s strategic literature who remember when total-war planning was mainstream, are largely absent from the elite civilian strategic-studies conversation that has moved past the category; a future crisis planner forced to reconstruct total-war thinking under time pressure has no seat in the current conversation at all.
% DISAPPEARANCE_RATIONALE: If the ideational narrowing reversed overnight and total-war planning re-entered elite discourse, the defense-intellectual community's status hierarchy and curricula would visibly rearrange (a resettling of prestige and citation networks); but whether the underlying WORLD — actual crisis behavior, weapons postures, alliance structures — would rearrange is disputed, since the sibling readings attribute total war's absence to structural (nuclear) or normative (legal) causes that would persist regardless of what elites discuss.
% FOUNDING_PROBLEM: In the immediate postwar and early nuclear period, strategists needed frameworks to reason about escalation control and limited engagement precisely BECAUSE total war was still a live, terrifying possibility — the limited-war paradigm was built to manage escalation risk, not to declare total war impossible.
% FOUNDING_PROBLEM_CORROBORATION: Historians of strategic thought (e.g., genealogies of the Schelling/Kahn-era escalation literature) attest that the founding problem — managing escalation risk under conditions where total war remained a live possibility — has not disappeared; if anything, multipolar nuclear proliferation and emerging-technology arms races arguably renew it. This corroboration comes from strategic-studies historians outside the beneficiary set, who note the field's own justification (total war is basically settled, hence unnecessary to plan for) is asserted mainly by the beneficiary community itself and is not independently verified by operational military planners, who are structurally excluded from the conversation.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, contested).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).
:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.15 to 0.42 over seventy years) because the mechanism is diffuse institutional forgetting rather than active rent extraction — nobody is billing anyone, but professional attention, funding, and doctrinal effort are steadily redirected away from a capability whose absence has a real (if hard to price) cost. Suppression is comparatively low (0.31) because no one is coercively preventing total-war scholarship — it is simply unrewarded and uncited, a market-of-ideas drift rather than a censorship regime. Theater ratio rises sharply (0.12 to 0.68) because as the underlying capability atrophies, the discourse compensates with increasingly elaborate limited-war and escalation-control theorizing that performs strategic seriousness while the actual muscle for total-war reasoning withers — classic piton signature: form persists, function hollows. Accessibility collapse is moderate (0.4): the alternative (returning total-war planning to the curriculum) is not physically barred, just professionally unrewarded, so collapse is partial rather than the near-total collapse of a genuine mountain. Resistance is moderate-low (0.35): a minority of historians and some operational planners push back on the narrowing, but there is no organized counter-movement with power to reverse it.
 *
 * DIRECTIONALITY LOGIC:
 *   Limited-war defense intellectuals and the arms-control epistemic community are declared beneficiaries because the narrowed discourse is the terrain on which their professional standing, citation networks, and institutional funding are built — the engine should derive low-to-moderate d for these institutional/identity-locked seats, tempered by the fact that the benefit is reputational/positional rather than a direct financial transfer. Strategic flexibility (a non-agent payer) and future crisis planners carry the cost: their d should sit near the target end because they bear a capability deficit they did not choose and cannot presently exit (trapped). No override is used for the beneficiary seats because their identity_locked exit options already capture the structural bind — a defense intellectual whose career is built on limited-war theory cannot cheaply pivot to championing total-war planning without professional cost, which is exactly the piton-sustaining mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/founding_problem_status pairing is deliberately marked 'contested' rather than 'dead': the limited-war paradigm was built to manage escalation risk while total war remained live, and that founding problem plausibly persists (renewed multipolar nuclear competition, emerging-technology arms races) even as the field's own self-justification (that the problem is basically solved) is asserted mainly by the beneficiary community. This is precisely the mismatch the R5 genealogy interview is designed to surface: disappearance_verdict is 'contested' rather than 'world_rearranges' or 'world_unchanged,' because whether the WORLD would rearrange if the ideational narrowing reversed is exactly what's disputed between this reading and its structural/normative siblings. Classifying this as Piton rather than Snare or Tangled Rope avoids mislabeling institutional forgetting (no one is actively coercing the narrowing, no concentrated profiteer captures rents) as either pure extraction or an intentionally coordinated-and-exploited arrangement — the diffuse, no-villain character of career-incentive drift is the piton's defining signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_disambiguation_reachability_vs_discourse,
    'Is the true driver of total war''s absence from strategic planning the ideational/institutional drift claimed here, the structural nuclear-weapons argument (structural_contraction_reading), or the normative-legal argument (normative_reading_drop) — and can these be empirically distinguished?',
    'Compare counterfactual scenarios: if nuclear weapons were hypothetically removed from a dyad''s arsenal but the same defense-intellectual professional culture persisted, would total-war planning re-emerge? If international humanitarian law were repealed but the professional culture persisted, would planning re-emerge? Divergent answers would help isolate which causal layer is doing the load-bearing work; historical case studies of non-nuclear great-power planning (pre-1945 vs. post-1991 regional conflicts) offer partial natural experiments.',
    'If the structural (nuclear) reading fully accounts for total war''s absence, this ideational-drift reading would be redundant — the atrophy would be an epiphenomenon of physical impossibility rather than an independent piton with its own victim (strategic flexibility). If the ideational reading is doing independent causal work, the piton classification and its victim/beneficiary structure stand on their own.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_disambiguation_reachability_vs_discourse, conceptual, 'Whether the three kernel readings are independently causally operative or whether one subsumes the others.').

omega_variable(
    atrophy_reversibility,
    'If a crisis emerged that plausibly required total-war-scale planning, could the strategic-studies field reconstitute that capacity quickly, or has the institutional forgetting reached a point where reconstruction would take a generation?',
    'Examine historical precedents of rapid doctrinal reconstitution after long dormancy (e.g., counterinsurgency doctrine''s near-total abandonment after Vietnam and rapid, costly reconstruction after 2003) as a base rate for how quickly atrophied strategic categories can be rebuilt under operational pressure.',
    'If reconstitution is fast, the piton''s victim cost (strategic_flexibility) is smaller than claimed and the classification should weight lower extractiveness; if reconstitution is slow or catastrophically costly under time pressure, the piton''s victim cost is understated and the true extractiveness may be higher than the authored 0.42.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_reversibility, empirical, 'Whether the atrophied capability is cheaply reconstitutable or represents a deep, slow-to-repair institutional loss.').

omega_variable(
    beneficiary_intentionality_ambiguity,
    'Do limited-war defense intellectuals benefit from the narrowing merely incidentally (their careers happened to align with where the field went) or did they actively steer curricula and journal gatekeeping to entrench the narrowing and protect their own paradigm''s dominance?',
    'Archival and citation-network analysis of war-college curriculum committee records and journal editorial board composition over the 1970s-1990s period would show whether curriculum changes correlate with the professional interests of committee members, versus tracking independent shifts in policymaker demand.',
    'If steering was active and self-interested, this reading would shade closer to Tangled Rope or Snare (deliberate suppression of a rival paradigm for professional gain) rather than a genuine no-villain Piton. If the alignment was incidental, the Piton classification (diffuse institutional forgetting, no concentrated actor to blame) holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intentionality_ambiguity, empirical, 'Whether beneficiary capture was actively steered or an incidental byproduct of field evolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1955, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1955, 0.12).
narrative_ontology:measurement(tota_tr_t1965, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(tota_tr_t1985, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1985, 0.42).
narrative_ontology:measurement(tota_tr_t1995, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1995, 0.55).
narrative_ontology:measurement(tota_tr_t2010, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2010, 0.63).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(tota_be_t1955, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1955, 0.15).
narrative_ontology:measurement(tota_be_t1965, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1965, 0.2).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1975, 0.26).
narrative_ontology:measurement(tota_be_t1985, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1985, 0.31).
narrative_ontology:measurement(tota_be_t1995, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement(tota_be_t2010, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2010, 0.39).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__strategic_culture_drift, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.08).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, normative_reading_drop).

% DUAL FORMULATION NOTE:
% Sibling constraint in the total_war_winnability_post1945 kernel family. structural_contraction_reading claims nuclear weapons made total war physically unreachable (Mountain-flavored, near-zero extraction — a structural fact of the strategic environment). normative_reading_drop claims total war became legally/normatively illegitimate via Article 2(4) and IHL development (Rope/Tangled-Rope-flavored — a genuine coordination achievement with some enforcement asymmetry between powerful and weak states). THIS story (strategic_culture_drift) claims total war remains physically and legally reachable but has atrophied from elite planning capacity through institutional forgetting (Piton-flavored — moderate, rising extraction with no concentrated beneficiary capturing large rents, just diffuse career-incentive drift). The three readings are not competing measurements of one constraint; they are three structurally distinct constraints sharing a natural-language label, each with its own stable ε and its own classification, linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
