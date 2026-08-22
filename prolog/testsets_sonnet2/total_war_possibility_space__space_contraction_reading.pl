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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Categorical Exclusion of Total War from Strategic Possibility Space (Space-Contraction Reading)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This story instantiates the space-contraction reading of the
 *   total_war_possibility_space kernel: the claim that thermonuclear weapons
 *   and assured second-strike capability removed great-power total war from
 *   the set of strategically thinkable outcomes entirely, rather than merely
 *   making it less preferable under a live deterrence calculus (the sibling
 *   deterrence_equilibrium_reading) or normatively taboo (the sibling
 *   nuclear_taboo_reading). Under this reading, the mechanism is
 *   physical/logical, not equilibrium-based or normative: there is no
 *   coherent war-termination state for total inter-state war among nuclear
 *   possessors other than mutual destruction, so the option exits the
 *   planning space the way an impossible move exits a game tree rather than
 *   the way an unattractive move gets deprioritized. The predicted
 *   institutional signature is atrophy of total-war planning apparatus:
 *   mobilization doctrine disappears, general-staff war-gaming for
 *   great-power conflict ceases, and strategic studies as a field re-centers
 *   on sub-nuclear and gray-zone domains. The rising theater_ratio and
 *   base_extractiveness over the interval track a secondary phenomenon this
 *   reading must explain: as the total-war planning apparatus atrophies, its
 *   remaining activity (contingency planning cells, doctrine archives,
 *   symbolic exercises) becomes increasingly performative relative to genuine
 *   total-war readiness, and the displaced strategic competition re-emerges
 *   as sub-nuclear/proxy extraction that lands disproportionately on
 *   non-nuclear states.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states_defense_establishments: institutional beneficiary and agenda-setter — redirects planning capacity away from total war
 *   - sub_nuclear_conflict_industry: organized beneficiary — occupies the strategic space vacated by total-war planning
 *   - non_nuclear_states: moderate-power payer — absorbs displaced great-power competition without sharing the ceiling's protection
 *   - general_staff_war_planning_apparatus: institutional payer, identity-locked — its core mission is categorically foreclosed rather than deprioritized
 *   - civilian_populations_of_great_powers: powerless beneficiary — benefits from exclusion without agency over it
 *   - strategic_studies_scholars: analytical observer — documents the field's pivot as empirical fact, divided on mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.28).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.35).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Categorical Exclusion of Total War from Strategic Possibility Space (Space-Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '8b1cb686-584c-487a-9d79-efa684d37070').
narrative_ontology:cs_kernel_codification('8b1cb686-584c-487a-9d79-efa684d37070', distributed).
narrative_ontology:cs_authority_grounding('8b1cb686-584c-487a-9d79-efa684d37070', distributed).
narrative_ontology:cs_reading_relation('8b1cb686-584c-487a-9d79-efa684d37070', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b1cb686-584c-487a-9d79-efa684d37070', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('8b1cb686-584c-487a-9d79-efa684d37070', foundational, total_war_categorically_unwinnable_under_second_strike).
narrative_ontology:cs_axiom_status(total_war_categorically_unwinnable_under_second_strike, holdable).
narrative_ontology:cs_axiom_grounding('8b1cb686-584c-487a-9d79-efa684d37070', total_war_categorically_unwinnable_under_second_strike, empirically_contingent).
narrative_ontology:cs_axiom('8b1cb686-584c-487a-9d79-efa684d37070', secondary, planning_space_exclusion_is_physical_not_attitudinal).
narrative_ontology:cs_axiom_status(planning_space_exclusion_is_physical_not_attitudinal, holdable).
narrative_ontology:cs_axiom_grounding('8b1cb686-584c-487a-9d79-efa684d37070', planning_space_exclusion_is_physical_not_attitudinal, empirically_contingent).
narrative_ontology:cs_reference_frame('8b1cb686-584c-487a-9d79-efa684d37070', pre_nuclear_total_war_planning_norm).
narrative_ontology:cs_drift_state('8b1cb686-584c-487a-9d79-efa684d37070', post_cold_war_contemporary, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('8b1cb686-584c-487a-9d79-efa684d37070', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states_defense_establishments).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, sub_nuclear_conflict_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, general_staff_war_planning_apparatus).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, civilian_populations_of_great_powers).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, general_staff_war_planning_apparatus).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, escalation_dominance_irrelevance_at_total_war_scale).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, mutually_assured_destruction_as_physical_ceiling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ministries of defense and general staffs in nuclear-armed states no longer maintain live mobilization plans for great-power total war because the physical possibility of prosecuting and surviving such a war to strategic advantage has collapsed. They redirect budgets, doctrine shops, and officer career paths toward sub-nuclear, hybrid, and gray-zone planning. They benefit from a stable, low-maintenance strategic ceiling that no longer requires continuous total-war contingency investment.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states_defense_establishments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states_defense_establishments, agenda_setter).

% Defense contractors, proxy-conflict logistics networks, cyber and gray-zone capability vendors, and counterinsurgency specialists occupy the strategic space vacated by total-war planning. Their market exists because total war is no longer a live planning target, so state strategic attention and procurement concentrate on the sub-nuclear domain instead.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, sub_nuclear_conflict_industry, beneficiary,
    organized, generational, mobile, global).

% States without nuclear arsenals inherit a strategic environment shaped by great-power total-war exclusion but do not share the ceiling's protection symmetrically — conventional and sub-nuclear conflict, proxy war, and coercion below the nuclear threshold become the arena where great-power competition is displaced onto their territory. They bear the redirected costs of a possibility space contracted for the nuclear-armed but not equivalently contracted for them.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, non_nuclear_states, payer,
    moderate, generational, constrained, regional).

% The institutional apparatus historically built to plan, doctrine, and war-game total inter-state war (mobilization schedules, industrial conversion planning, total-war general staffs) finds its core professional function categorically foreclosed rather than merely deprioritized. Career total-war planners face an atrophying institutional mandate; the apparatus survives mostly as historical memory, doctrine archives, and reduced-scope contingency planning cells. Its identity is bound to a mission the space-contraction reading holds to no longer exist as a live planning target.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, general_staff_war_planning_apparatus, payer,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__space_contraction_reading, general_staff_war_planning_apparatus, beneficiary).

% Populations of nuclear-armed and allied states no longer face live planning for total societal mobilization and existential great-power war as a near-term strategic contingency. They benefit from the categorical exclusion without having produced or maintained it themselves; they have no exit from the arrangement and no lever to alter it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, civilian_populations_of_great_powers, beneficiary,
    powerless, civilizational, trapped, global).

% Academic and think-tank strategic studies communities document the shift of the field's center of gravity away from total-war planning and toward deterrence stability, sub-nuclear escalation management, and gray-zone conflict. They observe the institutional atrophy of total-war doctrine as an empirical fact requiring explanation, and are themselves divided over whether the exclusion is physical (this reading), equilibrium-based, or normative.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense — this is not a solved collective-action problem but a physical ceiling: the arrival of thermonuclear weapons and assured second-strike capability removed the possibility of prosecuting total inter-state war among nuclear powers to a coherent strategic outcome, because no plausible war-termination state exists that is not mutual destruction.
% TRANSFER_FUNCTION: The reading does not describe a transfer of resources between parties in the ordinary extractive sense; it describes a reallocation of institutional attention and planning capacity away from total-war contingency (mobilization doctrine, industrial war-planning, general-staff total-war gaming) and toward sub-nuclear and gray-zone domains, with non-nuclear states absorbing the displaced great-power competition.
% ABSENT_VOICES: Populations and states outside the nuclear club have no voice in whether the possibility-space contraction applies to them; the total-war ceiling is a fact about great-power dyads, but its downstream displacement of conflict onto proxy and regional theaters is rarely treated as a cost of the same phenomenon in the strategic-studies literature that documents the ceiling.
% DISAPPEARANCE_RATIONALE: If the physical ceiling this reading describes were falsified overnight (e.g., a technology neutralizing assured second-strike capability), the total-war planning apparatus would need immediate reconstruction: mobilization doctrine, industrial conversion planning, and general-staff total-war gaming would resume as live strategic necessities, and defense budgets and officer training pipelines would reorganize accordingly within a single planning cycle.
% FOUNDING_PROBLEM: The problem was never 'built' by any party — it names a physical fact that emerged from thermonuclear weapons yield and delivery reliability: the arrival of a destructive capability whose mutual use forecloses any coherent strategic victory condition for total inter-state war among possessors.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by declassified general-staff planning archives across multiple nuclear powers showing the near-total cessation of total-war mobilization gaming after the advent of assured second-strike capability (independent of any single state's self-interested framing), and by strategic-studies scholarship (an analytical, non-beneficiary seat) documenting the field's structural pivot toward deterrence stability and sub-nuclear conflict studies as its center of professional gravity.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because the space-contraction reading, taken on its own terms, describes a physical ceiling rather than an extractive arrangement — nobody profits from the ceiling's existence in the way a rent-collector profits from a toll. But extractiveness is not zero and rises over the interval because the reading's own predicted institutional atrophy generates a secondary extraction: as total-war planning careers and apparatus wither, displaced strategic competition migrates to sub-nuclear domains where non-nuclear states pay disproportionate costs, and a sub-nuclear conflict industry captures the redirected attention and budget. Theater ratio rises for the same reason — the residual total-war planning apparatus (contingency cells, symbolic exercises, doctrine museums) becomes decreasingly connected to genuine total-war readiness as the underlying mission is foreclosed, producing exactly the theatrical-maintenance signature a piton would show if this reading's mountain claim is wrong. Suppression is authored moderate (0.35), reflecting that the reading itself claims minimal suppression is needed — the ceiling holds by physics, not by enforcement — but some suppression remains in the form of continued investment in force postures that maintain assured second-strike capability, which is a form of active maintenance work even if it is not coercive suppression of alternatives in the ordinary Snare sense.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states' defense establishments and the sub-nuclear conflict industry sit near the beneficiary end: they get a stable strategic ceiling requiring no continuous total-war contingency investment, and a redirected market respectively. Non-nuclear states and the general staff war-planning apparatus sit nearer the target end: non-nuclear states absorb displaced great-power competition without symmetric protection, and the planning apparatus loses its core professional mandate through categorical foreclosure rather than choice. Civilian populations of great powers are beneficiaries by default — the ceiling protects them without their agency, hence trapped exit options rather than mobile ones, since they cannot act on this constraint even to defend it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question this reading raises directly: is the total-war planning apparatus's founding problem (planning to fight and win a total inter-state war) dead (foreclosed by physical possibility) or merely dormant (deterred, per the sibling equilibrium reading, and therefore reactivatable)? This reading answers 'dead' — the founding problem's status is authored as 'live' only in the sense that the physical ceiling itself is a live, current fact, not that the war-planning mission is live. The general_staff_war_planning_apparatus stakeholder is the seat where this matters most: if the space-contraction reading is correct, that apparatus's residual activity is honestly diagnosed as theater (rising theater_ratio) rather than misclassified as continued genuine coordination against a live threat. Mislabeling the atrophy as mere budget cutting (rather than categorical mission foreclosure) would obscure that no amount of re-investment restores the total-war planning function under this reading's premises, since the constraint removed is physical, not attitudinal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_ceiling_vs_maintained_equilibrium,
    'Is the exclusion of total war from strategic possibility space a true categorical impossibility (this reading), or is it an equilibrium condition that persists only because deterrence infrastructure is actively maintained (the sibling deterrence_equilibrium_reading)?',
    'Examine whether any nuclear power has continued to invest in escalation-dominance or first-strike-capable total-war-fighting doctrine despite public commitments to assured-destruction logic; sustained investment would suggest an equilibrium being actively defended rather than a closed possibility space.',
    'If the equilibrium reading is correct, the institutional atrophy this reading predicts (disappearance of mobilization doctrine, cessation of general-staff total-war gaming) is fragile and reversible upon any perceived shift in relative capability — the mountain classification would not hold and the constraint would be better modeled as a Tangled Rope requiring continuous deterrence maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_ceiling_vs_maintained_equilibrium, conceptual, 'Whether total-war exclusion is a physical fact or a maintained strategic equilibrium.').

omega_variable(
    who_benefits_from_a_natural_ceiling,
    'If this reading is correct that the exclusion is a genuine physical ceiling emerging from weapons physics rather than any party''s construction, why do identifiable beneficiaries (nuclear-armed defense establishments, sub-nuclear conflict industry) exist at all?',
    'Compare defense budget reallocation patterns and doctrine publication records across nuclear powers to determine whether the beneficiary pattern reflects passive adaptation to an exogenous physical fact, or whether some nuclear powers have actively marketed the ceiling''s existence to justify budget and doctrine shifts that also serve independent institutional interests (e.g., justifying reduced conventional force levels).',
    'If beneficiaries are shown to be actively shaping the narrative of categorical impossibility to serve independent institutional interests (budget capture, career redirection), this raises a false-summit concern: a constraint claimed as mountain but partly sustained by parties who benefit from that framing, which would push the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(who_benefits_from_a_natural_ceiling, empirical, 'Whether the beneficiary structure on a claimed-mountain reading indicates a genuine natural ceiling or partial construction.').

omega_variable(
    displaced_conflict_burden_measurement,
    'How much of the sub-nuclear/proxy conflict burden borne by non-nuclear states is causally attributable to the total-war exclusion (displacement) versus other independent drivers of regional conflict (decolonization, resource competition, ethnic conflict)?',
    'Comparative case study of conflict intensity and great-power proxy involvement in regions before and after nuclear parity was established among relevant great-power dyads, controlling for other conflict drivers.',
    'A strong causal link would substantiate the victim/payer status assigned to non_nuclear_states in this story; a weak link would suggest the extractiveness score attributed to displacement is overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_conflict_burden_measurement, empirical, 'Causal attribution of displaced-conflict burden to the total-war exclusion mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.12).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__space_contraction_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.18).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1991, 0.2).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.24).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.05).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_non_proliferation_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, conventional_deterrence_doctrine).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the total_war_possibility_space kernel. deterrence_equilibrium_reading models the same phenomenon as a maintained equilibrium (Tangled-Rope-flavored: requires continuous deterrence maintenance and could in principle unwind). nuclear_taboo_reading models it as a constructed normative prohibition independent of material capability (Rope- or Scaffold-flavored: contingent on continued normative consensus). This space_contraction_reading is the only one of the three claimed as Mountain, because it alone locates the exclusion mechanism in physical/logical impossibility rather than equilibrium or norm. Each reading has a distinct ε and distinct beneficiary/victim structure per the ε-invariance principle; they are not the same constraint viewed three ways but three structurally distinct claims sharing a label ('why total war didn't happen').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
