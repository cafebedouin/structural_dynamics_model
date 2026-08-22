% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone is a 1933-era monument inscribed with a
 *   warning to mark the maximum observed reach of tsunami waves: 'High
 *   dwellings are the peace and harmony of our descendants. Remember the
 *   calamity of the great tsunamis. Do not build your homes below this
 *   point.' For 78 years prior to the 2011 Tōhoku tsunami, the stone
 *   functioned as a live behavioral rule enforced through community land-use
 *   allocation: residents did not settle below the stone's mark; new
 *   residents were instructed not to build below it; land distribution
 *   reflected the prohibition. The behavioral competence reading treats this
 *   constraint as a physical law operationalized through social practice—the
 *   stone marks a real hazard boundary, and the community's enforcement
 *   transmits empirical knowledge across generations without formal state
 *   apparatus. This reading authorizes the constraint's classification as
 *   mountain: very low extractiveness (no one benefits from the prohibition
 *   itself, only from the safety it ensures), nearly universal accessibility
 *   collapse (alternatives are not visible, or visible only as catastrophic
 *   failure), and minimal active resistance (the rule is so well-fitted to
 *   material reality that compliance requires little coercion). The sibling
 *   commemorative_husk_reading treats the same stone and rule as a degraded
 *   memorial whose behavioral force had atrophied to performance—families
 *   occasionally invoked the stone's story but had migrated settlement
 *   downslope as living memory faded and economic pressure mounted, making
 *   the rule a theater of safety rather than a live constraint by 2005–2010.
 *   This story instantiates the behavioral_competence reading: the stone WAS
 *   a live rule across 1933–2011, enforced through social practice, and the
 *   2011 wave validated its accuracy.
 *
 * KEY AGENTS:
 *   - aneyoshi_community: Maintains the stone and enforces the prohibition through collective land allocation without centralized state machinery.
 *   - downstream_residents_literal_safety: Benefit from the rule's enforcement; they are powerless to exit and cannot choose the constraint.
 *   - would_be_violators_episodic: Residents who test the boundary when memory of tsunamis fades and economic incentive rises to build downslope.
 *   - wave_physics (non-agent observer): The constraint's ultimate referent; the stone's location encodes empirical observation of tsunami reach.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Land-Use Prohibition (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '6a2143a3-7901-42d6-a268-69634120fc59').
narrative_ontology:cs_kernel_codification('6a2143a3-7901-42d6-a268-69634120fc59', implicit).
narrative_ontology:cs_authority_grounding('6a2143a3-7901-42d6-a268-69634120fc59', practice).
narrative_ontology:cs_interpretation_layer_present('6a2143a3-7901-42d6-a268-69634120fc59').
narrative_ontology:cs_reading_relation('6a2143a3-7901-42d6-a268-69634120fc59', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('6a2143a3-7901-42d6-a268-69634120fc59', foundational, prohibition_behavioral_operationalization).
narrative_ontology:cs_axiom_status(prohibition_behavioral_operationalization, holdable).
narrative_ontology:cs_axiom_grounding('6a2143a3-7901-42d6-a268-69634120fc59', prohibition_behavioral_operationalization, empirically_contingent).
narrative_ontology:cs_axiom('6a2143a3-7901-42d6-a268-69634120fc59', secondary, physical_constraint_social_transmission).
narrative_ontology:cs_axiom_status(physical_constraint_social_transmission, holdable).
narrative_ontology:cs_axiom_grounding('6a2143a3-7901-42d6-a268-69634120fc59', physical_constraint_social_transmission, deontological).
narrative_ontology:cs_reference_frame('6a2143a3-7901-42d6-a268-69634120fc59', community_tsunami_memory_transmission).
narrative_ontology:cs_drift_state('6a2143a3-7901-42d6-a268-69634120fc59', contemporary_2005_2010, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('6a2143a3-7901-42d6-a268-69634120fc59', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__behavioral_competence_reading, downstream_residents_literal_safety).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the stone monument in the village and enforces the behavioral rule: do not build below the stone's mark. The rule is transmitted orally, inscribed on the stone itself, and operationalized through collective land allocation and building permits. Community members coordinate on the prohibition without centralized enforcement machinery — the rule IS the coordination practice.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_community, agenda_setter,
    organized, civilizational, constrained, local).

% Inhabit areas protected by the prohibition's enforcement. The rule physically saves their lives by keeping settlement patterns above the proven tsunami reach line. They benefit from a constraint they did not choose and cannot exit — they are downstream of a physical regularity (tsunami frequency) that the social rule operationalizes.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, downstream_residents_literal_safety, beneficiary,
    powerless, civilizational, trapped, local).

% Residents who might want to build below the stone line for agricultural or resource access benefit. The prohibition excludes them from lower-lying, fertile land. Their pressure appears episodically when memory of tsunamis fades and economic incentives to build downslope rise. Community enforcement reactivates against each new generation's testing.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, would_be_violators_episodic, excluded,
    powerless, biographical, constrained, local).

% The constraint's ultimate referent: the recurrence rate and reach of tsunami events. The stone marks the empirically observed high-water line. The rule works because it IS informed by natural history, not despite it.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__behavioral_competence_reading, wave_physics, observer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_land_use_prohibition__behavioral_competence_reading, wave_physics).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes tsunami hazard memory into a spatial boundary (the stone's location) that guides land-use allocation without requiring continuous hazard communication or technical expertise. The stone IS the coordinate: residents allocate upslope land to settlement, downslope land to temporary resource use (pasture, crops during calm periods), synchronized to the stone's mark.
% TRANSFER_FUNCTION: No transfer of goods or rents. The constraint moves behavioral compliance: the prohibition transfers settlement action from below-the-stone to above-the-stone, in response to a physical regularity (tsunami reach). No agent collects from the rule's operation; the rule operationalizes a fact.
% ABSENT_VOICES: Residents who died in tsunamis that predated the stone's inscription — their testimony is encoded in the stone's location itself. Future residents not yet born will depend on the rule's transmission and enforcement; their interests are pre-committed to above-the-line settlement by current behavioral practice.
% DISAPPEARANCE_RATIONALE: If the stone vanished and its enforcement lapsed, settlement would migrate downslope when memory of the last tsunami faded (typically 1–2 generations). The next major tsunami would kill residents who had rebuilt below the wave reach, reorganizing the settlement pattern through catastrophe rather than rule. The constraint prevents reorganization BY catastrophe.
% FOUNDING_PROBLEM: Tsunami strike risk to settlement. Aneyoshi (and neighboring communities) experienced repeated tsunamis; the stone's location was selected to mark the observed high-water line from historical events, encoding a collective memory of maximum reach.
% FOUNDING_PROBLEM_CORROBORATION: Geomorphological and paleotsunami evidence (sand layers, erratics, coastal scarps) from researchers studying the 2011 Tōhoku tsunami aftermath confirms Aneyoshi's stone mark accurately predicted the 2011 wave reach — validating the rule's founding problem from sources independent of the community's own memory. The founding problem is testified to by wave physics itself.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 baseline) because no agent collects from the prohibition—downstream residents are saved, not exploited; the community enforces it as a survival practice, not as a revenue or status mechanism. Suppression is correspondingly low (0.12 baseline): enforcement is social and distributed, not coercive machinery. Theater ratio oscillates (0.15–0.28, highest in 1990–2005 when economic pressure to build downslope mounted and community invocation of the stone became more rhetorical), reflecting episodic testing and reactivation cycles. Accessibility collapse is very high (0.92 baseline): once you understand tsunami physics, settling above a marked high-water line is the only rationally defensible choice; building below it is not an alternative, it is catastrophe. Resistance is near-zero (0.05) because the rule is so well-fitted to material reality that genuine opposition is rare; episodic testing (1–2% of residents per generation) is demographic churn and generational forgetting, not organized dissent. The coercion grid shows: individual-level suppression is lower than organizational and structural levels (residents internalize the rule quickly when tsunami history is taught), stakes_inflation is highest at structural level (the cost of violating the rule IS the cost of waves), and resistance remains minimal at every level because the constraint's premise (wave physics) is not contestable.
 *
 * PERSPECTIVAL GAP:
 *   The downstream_residents and aneyoshi_community seats perceive this constraint identically: as a rule that saves lives. The would_be_violators seat perceives it as an economic constraint on land access, but this seat is not organized or continuous—it is episodic individuals testing the boundary when economic pressure rises. The engine's per-seat classification will converge because the structural relationship is symmetric: everyone benefits from the prohibition's enforcement. A piton reading (commemorative_husk) would produce a different seat structure—some seats benefiting from performing the memorial, others bearing its cost—but that is a different constraint with different structural data. This reading keeps the structural reality fixed: the stone enforces a survival rule through social practice, with no beneficiary asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: aneyoshi_community (agenda_setter, organized, constrained exit) holds d=0.0 (full beneficiary—they maintain the rule because it saves their relatives). Downstream_residents_literal_safety (beneficiary, powerless, trapped exit) hold d=0.0 (benefit from the rule they cannot exit). Would_be_violators_episodic (excluded, powerless, constrained exit) are not stationary agents—they are demographic cohorts testing the rule; their average d across all residents is near 0.0 (the prohibition saves them even when they test it). Wave_physics is analytical (non-agent, d not applicable). No directionality override is needed; the structural derivation from beneficiary-only, no-victims produces uniform d≈0.0 across the agent set, which is the truth.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: tsunamis still occur and still threaten settlement. The disappearance verdict is world_rearranges: if the stone vanished and the rule lapsed, settlement would migrate downslope and the next tsunami would kill people who had rebuilt there. The constraint persists because its founding problem persists and the rule's empirical basis (tsunami reach) is unchanging. No mandatrophy is present. The rule's founding problem and its continued necessity are empirically inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_enforcement_vs_performance,
    'Was the prohibition enforced as a live behavioral rule throughout 1933–2011, or did enforcement decay to commemorative theater in the decades before 2011?',
    'Historical land-use records from Aneyoshi: tax registries, building permits, settlement maps, or oral history documentation of which families occupied which land tracts. If downslope settlement increased in 1980–2010 despite the rule''s continued invocation, the rule was atrophying.',
    'If the rule remained live (no downslope settlement growth), this reading stands as mountain. If the rule atrophied (downslope settlement increased while invocation persisted), the constraint morphs into piton: inertial performance of a rule whose function had dissolved. Classification would shift from mountain to piton, and extractiveness/theater metrics would be re-authored to reflect the inertial state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_enforcement_vs_performance, empirical, 'Whether the prohibition remained operationalized as behavioral practice or degraded to commemorative performance.').

omega_variable(
    reading_contest_kernel_identity,
    'Which reading (behavioral_competence vs. commemorative_husk) correctly captures the constraint''s actual state during 1990–2011?',
    'The disagreement is empirically resolvable via historical settlement patterns, but the readings themselves are committer-indexed: this reading authors the constraint AS IF the behavioral rule were live; the sibling reading authors it AS IF the commemorative function had eclipsed behavioral operation. The two readings are not both true or both false—they are different story-tellings of the same kernel (the stone and prohibition) under different epistemic commitments.',
    'This is a conceptual rather than empirical omega: the engine computes per-seat classifications from the structural data authored under each reading. If both readings'' structural data are faithfully authored, the engine will compute mountain for behavioral_competence and piton/theater for commemorative_husk, and the divergence will signal the reading contest''s legitimacy. No resolution ''closes'' the question in the sense of settling which reading is true—the resolution is meta-level: acknowledging that two coherent readings of the kernel exist, both empirically grounded in the 2011 tsunami outcome, but differing in what they assert about the constraint''s state before validation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether the behavioral_competence or commemorative_husk reading more truthfully captures the constraint''s actual operation during 1933–2011.').

omega_variable(
    natural_law_vs_institutional_memory,
    'Is the stone-marked prohibition a natural law (the wave reach IS the boundary, encoded by geological/hydrological fact) or an institutional memory practice (the community transmits a rule across generations)?',
    'This is a framewise question, not resolvable by added data. The wave physics is natural law; the rule''s persistence is social practice. Both are true. The question is which framing foregrounds the constraint''s nature.',
    'This reading treats the constraint as mountain by emphasizing its physical substrate and low extractiveness. An alternative framing (not instantiated here) might emphasize the institutional practice and treat it as rope or scaffold (transitional enforcement of a rule). The reading choice foregrounds nature; an institutional reading would foregrounding practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_memory, preference, 'Whether to frame the constraint primarily as a natural regularity or as an institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1933, 0.15).
narrative_ontology:measurement(aney_tr_t1952, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1952, 0.12).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 2011, 0.18).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1933, 0.08).
narrative_ontology:measurement(aney_be_t1952, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1952, 0.07).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1970, 0.06).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 1990, 0.09).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2005, 0.1).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 2011, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1933, 0.1).
narrative_ontology:measurement(aney_su_t1952, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1952, 0.08).
narrative_ontology:measurement(aney_su_t1970, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1970, 0.14).
narrative_ontology:measurement(aney_su_t1990, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 1990, 0.16).
narrative_ontology:measurement(aney_su_t2005, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2005, 0.15).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 2011, 0.12).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1933, tn=2011
narrative_ontology:measurement(aney_grid_01, aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse(class), 1933, 0.91).
narrative_ontology:measurement(aney_grid_02, aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse(class), 2011, 0.93).
narrative_ontology:measurement(aney_grid_03, aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse(individual), 1933, 0.88).
narrative_ontology:measurement(aney_grid_04, aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse(individual), 2011, 0.9).
narrative_ontology:measurement(aney_grid_05, aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse(organizational), 1933, 0.94).
narrative_ontology:measurement(aney_grid_06, aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse(organizational), 2011, 0.95).
narrative_ontology:measurement(aney_grid_07, aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse(structural), 1933, 0.96).
narrative_ontology:measurement(aney_grid_08, aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse(structural), 2011, 0.97).
narrative_ontology:measurement(aney_grid_09, aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance(class), 1933, 0.05).
narrative_ontology:measurement(aney_grid_10, aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance(class), 2011, 0.07).
narrative_ontology:measurement(aney_grid_11, aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance(individual), 1933, 0.04).
narrative_ontology:measurement(aney_grid_12, aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance(individual), 2011, 0.06).
narrative_ontology:measurement(aney_grid_13, aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance(organizational), 1933, 0.02).
narrative_ontology:measurement(aney_grid_14, aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance(organizational), 2011, 0.04).
narrative_ontology:measurement(aney_grid_15, aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance(structural), 1933, 0.03).
narrative_ontology:measurement(aney_grid_16, aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance(structural), 2011, 0.05).
narrative_ontology:measurement(aney_grid_17, aneyoshi_land_use_prohibition__behavioral_competence_reading, stakes_inflation(class), 1933, 0.82).
narrative_ontology:measurement(aney_grid_18, aneyoshi_land_use_prohibition__behavioral_competence_reading, stakes_inflation(class), 2011, 0.85).
narrative_ontology:measurement(aney_grid_19, aneyoshi_land_use_prohibition__behavioral_competence_reading, stakes_inflation(individual), 1933, 0.85).
narrative_ontology:measurement(aney_grid_20, aneyoshi_land_use_prohibition__behavioral_competence_reading, stakes_inflation(individual), 2011, 0.88).
narrative_ontology:measurement(aney_grid_21, aneyoshi_land_use_prohibition__behavioral_competence_reading, stakes_inflation(organizational), 1933, 0.8).
narrative_ontology:measurement(aney_grid_22, aneyoshi_land_use_prohibition__behavioral_competence_reading, stakes_inflation(organizational), 2011, 0.82).
narrative_ontology:measurement(aney_grid_23, aneyoshi_land_use_prohibition__behavioral_competence_reading, stakes_inflation(structural), 1933, 0.92).
narrative_ontology:measurement(aney_grid_24, aneyoshi_land_use_prohibition__behavioral_competence_reading, stakes_inflation(structural), 2011, 0.94).
narrative_ontology:measurement(aney_grid_25, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression(class), 1933, 0.1).
narrative_ontology:measurement(aney_grid_26, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression(class), 2011, 0.12).
narrative_ontology:measurement(aney_grid_27, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression(individual), 1933, 0.08).
narrative_ontology:measurement(aney_grid_28, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression(individual), 2011, 0.1).
narrative_ontology:measurement(aney_grid_29, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression(organizational), 1933, 0.12).
narrative_ontology:measurement(aney_grid_30, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression(organizational), 2011, 0.14).
narrative_ontology:measurement(aney_grid_31, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression(structural), 1933, 0.14).
narrative_ontology:measurement(aney_grid_32, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression(structural), 2011, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel admits two readings: behavioral_competence_reading (this story, instantiating the constraint as live and operationally enforced, mountain type, ε≈0.08) and commemorative_husk_reading (sibling story, instantiating the constraint as degraded and inertial, piton type, higher ε and theater). Both readings are grounded in the same physical artifact (the stone) and the same historical event (the 2011 tsunami), but they differ in what they assert about behavioral enforcement during 1933–2011. The kernel decomposition is epistemically necessary: a single constraint story cannot simultaneously assert that the rule IS live and that it HAS atrophied. The engine will compute different per-seat classifications for each reading; the divergence signals the reading contest's structural legitimacy. Both stories share the interval 1933–2011 and the same stakeholder set; the difference is in the structural interpretation (enforcement vs. performance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
