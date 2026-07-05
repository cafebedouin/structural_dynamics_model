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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Nuclear-Era Contraction of the Total-War Possibility Space
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This story instantiates the 'space contraction' reading of the contested
 *   total-war-possibility-space kernel: the claim that the arrival of assured
 *   mutual thermonuclear destruction did not merely raise the cost of
 *   great-power total war (deterrence equilibrium) or construct a normative
 *   prohibition against it (nuclear taboo), but removed it categorically from
 *   the set of strategically thinkable options for general staffs. The
 *   observable structural delta this reading predicts is institutional:
 *   mobilization doctrine atrophies, total-war war-gaming against peer
 *   nuclear powers ceases as a live planning activity, and strategic studies
 *   as a discipline reorganizes around sub-nuclear, limited, and gray-zone
 *   domains. The rising theater_ratio series reflects exactly this: a growing
 *   share of what remains of formal 'total war' planning documents and
 *   war-college curricula is retained for institutional continuity or
 *   historical reference rather than functioning as live contingency
 *   planning, while base_extractiveness stays low throughout because this
 *   reading claims no party extracts rent from the constraint's operation —
 *   the beneficiaries listed collect an institutional/strategic advantage
 *   (freed planning attention, redirected budgets) as an incidental
 *   consequence of a claimed structural fact about strategic space, not
 *   through enforcement or coercion of any other party.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: institutional beneficiaries whose general staffs stop planning for the excluded contingency
 *   - sub_nuclear_defense_industrial_base: organized beneficiary of redirected doctrine and procurement attention
 *   - non_nuclear_states: payers who remain exposed to conventional/coercive violence even as the total-war category contracts at the top of the system
 *   - general_staff_planning_institutions: agenda-setters whose mobilization and total-war doctrine atrophies as an institutional practice
 *   - strategic_studies_scholars: analytical observers debating whether the shift is categorical exit or probability suppression
 *   - populations_of_nuclear_weapon_states: excluded parties bearing residual catastrophic risk with no voice in the doctrine debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.18).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.22).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear-Era Contraction of the Total-War Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '9f5aeb89-5b32-46a3-9603-94aeaea36c27').
narrative_ontology:cs_kernel_codification('9f5aeb89-5b32-46a3-9603-94aeaea36c27', distributed).
narrative_ontology:cs_authority_grounding('9f5aeb89-5b32-46a3-9603-94aeaea36c27', expertise).
narrative_ontology:cs_interpretation_layer_present('9f5aeb89-5b32-46a3-9603-94aeaea36c27').
narrative_ontology:cs_reading_relation('9f5aeb89-5b32-46a3-9603-94aeaea36c27', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f5aeb89-5b32-46a3-9603-94aeaea36c27', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('9f5aeb89-5b32-46a3-9603-94aeaea36c27', foundational, total_war_categorically_unreachable_not_merely_undesirable).
narrative_ontology:cs_axiom_status(total_war_categorically_unreachable_not_merely_undesirable, holdable).
narrative_ontology:cs_axiom_grounding('9f5aeb89-5b32-46a3-9603-94aeaea36c27', total_war_categorically_unreachable_not_merely_undesirable, empirically_contingent).
narrative_ontology:cs_axiom('9f5aeb89-5b32-46a3-9603-94aeaea36c27', secondary, institutional_atrophy_of_planning_apparatus_is_diagnostic_of_categorical_exit).
narrative_ontology:cs_axiom_status(institutional_atrophy_of_planning_apparatus_is_diagnostic_of_categorical_exit, holdable).
narrative_ontology:cs_axiom_grounding('9f5aeb89-5b32-46a3-9603-94aeaea36c27', institutional_atrophy_of_planning_apparatus_is_diagnostic_of_categorical_exit, empirically_contingent).
narrative_ontology:cs_reference_frame('9f5aeb89-5b32-46a3-9603-94aeaea36c27', pre_nuclear_total_war_as_live_planning_category).
narrative_ontology:cs_drift_state('9f5aeb89-5b32-46a3-9603-94aeaea36c27', post_cold_war_crisis_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9f5aeb89-5b32-46a3-9603-94aeaea36c27', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, sub_nuclear_defense_industrial_base).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_revolution_thesis).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, categorical_unthinkability_of_great_power_total_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold arsenals whose mere existence is claimed to have removed a categorical option — general war against another nuclear-armed great power — from the planning menu, not merely raised its price. Their general staffs redirect planning resources toward sub-nuclear, proxy, gray-zone, and regional contingencies because the total-war branch of the decision tree is treated as void rather than merely costly. They benefit from a strategic environment in which the most catastrophic form of great-power conflict is institutionally unplanned-for.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Doctrine, procurement, and force-structure investment concentrate on limited war, counterinsurgency, precision strike, cyber, and gray-zone capabilities because total-war planning against peer nuclear powers has atrophied as a live institutional activity. This shift in the planning menu redirects budgets and career paths toward the domains that remain strategically live.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, sub_nuclear_defense_industrial_base, beneficiary,
    organized, generational, mobile, global).

% Operate inside a strategic order in which the great powers no longer plan for total war against each other but may still wage conventional, proxy, or coercive campaigns against non-nuclear states with less risk of triggering a general-war response from third parties. The contraction of the total-war category at the top of the system does not contract the space of violence available against them.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Historically maintained mobilization doctrine, industrial war-planning, and total-war war-gaming as core functions. Under this reading, that apparatus does not merely deprioritize total-war planning as one option among several — the branch is treated as removed from the tree, so mobilization doctrine, general-population conscription planning, and total industrial-war gaming atrophy as institutional practices rather than persisting as low-probability contingencies.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, general_staff_planning_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__space_contraction_reading, general_staff_planning_institutions, observer).

% Study the shift in what militaries and states treat as plannable. They observe that curricula, doctrine documents, and war colleges shifted almost entirely to limited-war, deterrence, and sub-nuclear scenarios after the mid-20th century, and debate whether this reflects a genuine categorical exit of total war from the thinkable or merely a probability-weighted deprioritization that could reverse.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_scholars, observer,
    analytical, generational, analytical, global).

% Live under the residual catastrophic risk that the categorical-exit claim is wrong — if total war is not actually removed from the possibility space but only deterred or normatively suppressed, they remain the ultimate bearers of that risk with no seat in the doctrine debates that decide how seriously the residual possibility is planned for.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, populations_of_nuclear_weapon_states, excluded,
    powerless, civilizational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a coordination arrangement between parties but a claimed structural fact about what became strategically thinkable once mutual thermonuclear destruction became technically assured. Insofar as it 'coordinates' anything, it coordinates institutional attention away from total-war planning and toward sub-nuclear domains across all nuclear-armed general staffs simultaneously.
% TRANSFER_FUNCTION: Institutional planning resources, career incentives, and doctrinal attention move away from total-war mobilization planning and toward limited-war, gray-zone, and sub-nuclear strategic domains, on the premise that the total-war branch is void rather than merely deprioritized.
% ABSENT_VOICES: Populations of nuclear weapon states bear the tail risk if the categorical-exit claim is false, but have no seat in the professional strategic-studies and general-staff debates that decide how much residual planning attention total-war contingencies receive. Historians of near-miss crises (Petrov incident, Able Archer 83) would object that the record shows total war remained reachable through accident and miscalculation, not categorically excluded.
% DISAPPEARANCE_RATIONALE: If the claimed structural fact were false — if total war were in fact still fully thinkable and merely deterred or normatively suppressed rather than categorically removed — the practical world would look similar in the short run (militaries would still deprioritize total-war planning under deterrence pressure) but the институциональная basis for that deprioritization would rest on a different and more fragile claim. The disagreement is precisely about which underlying mechanism (space contraction vs. deterrence equilibrium vs. normative taboo) is doing the work, which is why the verdict is contested rather than settled either way.
% FOUNDING_PROBLEM: The technical arrival of assured mutual thermonuclear destruction between great powers, which this reading claims did not merely make total war catastrophically costly but removed it as a strategically thinkable option — a category change in the planning menu, not a price change.
% FOUNDING_PROBLEM_CORROBORATION: Strategic studies scholars external to any single nuclear weapon state's military establishment (Bernard Brodie's original 'absolute weapon' thesis, later nuclear-revolution theorists) attest that a genuine categorical shift occurred in what general staffs treat as plannable. Historians of Cold War crisis near-misses attest, from outside the benefiting military establishments, that the record of close calls undercuts the categorical-impossibility claim and supports a probability-suppression rather than possibility-elimination reading — the corroboration is genuinely split rather than unanimous, which is itself the evidentiary signal for this omega-laden claim.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
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
 *   Extractiveness and suppression are both authored low because this reading describes a structural claim about the boundaries of the strategically thinkable, not a coercive or rent-extracting arrangement — no party is compelled to accept the contraction, and no party collects a toll for its operation. Accessibility_collapse is authored very high (0.88) because the reading's core claim is precisely that alternatives (total-war planning as a live option) have almost completely collapsed from the professional planning menu. Resistance is low (0.15) because the claim, where accepted, is accepted largely without a live opposing camp inside general-staff institutions themselves — the contestation lives in the scholarly and historical community (crisis near-miss researchers), not in active resistance to the doctrine shift. Theater_ratio rises across the interval because vestigial total-war planning documents, war-college modules, and civil-defense infrastructure increasingly function as institutional memory/performance rather than live contingency planning, consistent with an atrophy-not-elimination institutional signature.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon states' general staff seat, the contraction looks like a settled structural fact justifying decades of resource reallocation. From the excluded populations' seat and from crisis historians' seat, the same absence of total-war planning looks like a dangerous complacency resting on an unverified categorical claim — the 1983 Able Archer and 1983 Petrov incidents are cited as evidence the possibility space was never actually voided, only narrowly avoided. The engine's per-seat computation should reflect that the agenda-setting institutions experience this as a stable near-mountain while the excluded/payer seats experience latent, uninsured tail risk.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and the sub-nuclear defense industrial base are coded as beneficiaries because the claimed contraction directly redirects planning attention and resources toward domains where they compete and invest. Non-nuclear states are coded as payers not because they are charged a toll by this constraint directly, but because the contraction of total war at the great-power apex does not reduce — and arguably permits with less inhibition — coercive or conventional violence directed at them, since third-party escalation to total war is treated as off the table. Populations of nuclear states are excluded rather than beneficiaries or payers in the ordinary sense: they hold the tail risk if the reading is wrong, but have no seat setting the doctrine that decides how much residual planning attention that tail risk receives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the technical fact of assured mutual destruction — is itself still fully live (arsenals remain, MAD-relevant capability persists), which argues against treating the institutional apparatus around this reading as a stale mandate. What could mandatrophy here would be the opposite failure mode: if a future capability shift (missile defense breakthroughs, decapitation-strike precision, AI-enabled first-strike calculus) reopened the total-war branch of the planning tree while institutions continued to treat it as categorically closed. That is precisely the scenario the omega variables below are designed to flag — a mountain claim whose natural-law status could quietly become false while institutional atrophy prevents anyone from noticing in time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_exit_vs_deterred_reachability,
    'Is total war between nuclear powers genuinely excluded from the strategic possibility space (categorical exit), or does it remain reachable in principle but is merely suppressed to near-zero probability by mutual vulnerability (the deterrence_equilibrium_reading)?',
    'Examine documented crisis near-misses (Cuban Missile Crisis, Able Archer 83, 1983 Petrov false-alarm incident) for evidence that decision-makers at the time treated total war as a live, reachable option under stress rather than a foreclosed impossibility. If near-misses show live deliberation over total-war options, the categorical-exit claim weakens relative to deterred-reachability.',
    'If the deterred-reachability reading is correct, the institutional atrophy this story documents (mobilization doctrine disappearance, ceased total-war war-gaming) represents a dangerous complacency riding on a false natural-law claim — a false summit in the mountain classification, not a genuine structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_exit_vs_deterred_reachability, empirical, 'Whether total war is categorically excluded or merely deterred to near-zero probability.').

omega_variable(
    natural_law_vs_constructed_institutional_convenience,
    'Is the claimed contraction of the total-war possibility space a genuine emergent structural fact about nuclear-era strategy, or a convenient institutional narrative that benefits nuclear weapon states and the sub-nuclear defense industrial base by justifying reduced total-war planning burden and redirected procurement?',
    'Compare the doctrine and budget history of nuclear weapon states against non-nuclear states with credible conventional militaries: if institutional atrophy of total-war planning tracks nuclear possession specifically (and not merely general war-fatigue or bureaucratic drift), the natural-law reading gains support; if atrophy correlates more with budget pressure or bureaucratic incentive than with nuclear-possession status, the constructed-convenience reading gains support.',
    'If constructed, this constraint should be reclassified away from mountain toward a tangled_rope or scaffold — a claimed structural fact that in practice functions to justify beneficial resource reallocation for specific institutional actors (schema-required omega given declared beneficiaries on a mountain claim).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_institutional_convenience, conceptual, 'Natural-law vs. beneficiary-convenient-narrative ambiguity required by FSM candidacy.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does this reading''s premise diverge from the deterrence_equilibrium_reading and nuclear_taboo_reading — is it a claim about the material impossibility of execution, the psychological/cognitive unthinkability for planners, or the normative illegitimacy of the option?',
    'Close reading of primary strategic doctrine texts (Brodie, Schelling, Kahn) alongside general-staff planning documents across decades to identify whether the operative language treats total war as materially void, cognitively excluded from planning scenarios, or normatively taboo.',
    'Locates the disagreement precisely: if planners'' own documents describe total war as materially impossible to execute rationally (rather than merely undesirable or forbidden), this reading is supported; if their language is about deterred cost or moral prohibition, the sibling readings are better supported by the same evidence base.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating where the three kernel readings structurally diverge in the primary evidentiary record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__space_contraction_reading, theater_ratio, 1991, 0.32).
narrative_ontology:measurement(tota_tr_t2008, total_war_possibility_space__space_contraction_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.1).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.12).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1991, 0.14).
narrative_ontology:measurement(tota_be_t2008, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2008, 0.16).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.05).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the total_war_possibility_space kernel. deterrence_equilibrium_reading holds total war remains materially reachable but is suppressed by mutual vulnerability (a high-cost-but-live-option structure); nuclear_taboo_reading holds the exclusion is normatively constructed and would persist even if material deterrence capability were somehow removed; this reading (space_contraction_reading) holds the exclusion is a categorical, structural fact about what is strategically thinkable at all — closer to a mountain classification than either sibling, which are more plausibly tangled_rope or scaffold-adjacent given their reliance on continued material capability or continued normative maintenance respectively. All three should be evaluated against the same underlying historical record (Cold War crisis history, doctrine documents, war-college curricula) with different structural conclusions drawn from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
