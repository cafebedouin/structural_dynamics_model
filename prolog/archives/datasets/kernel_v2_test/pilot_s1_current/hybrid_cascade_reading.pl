% ============================================================================
% CONSTRAINT STORY: hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_cascade_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_cascade_reading
 *   human_readable: Hybrid Cascade: State-Imposed Fringe Climbing into Organic Adoption
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The hybrid cascade reading models a specific historical pattern in state
 *   formation: the state issues a top-down decree mandating participation in
 *   military or state service (creating an artificial fringe). This
 *   artificial fringe is initially suppressed — agents comply because
 *   enforcement is overwhelming and alternatives are foreclosed. Over time,
 *   however, the fringe acquires prestige through association with state
 *   power, economic opportunity, and identity claims. The first-generation
 *   cohort, trapped by the decree, transmits to the second-generation cohort
 *   a reframed narrative: the fringe is now aspirational, not coercive. The
 *   organic climb begins. By generation three or four, the original decree's
 *   enforcement machinery is largely dormant — agents volunteer for the
 *   fringe because the prestige has become real, the economic opportunities
 *   are genuine, and identity fusion has made exit unthinkable. At this
 *   point, the constraint has transitioned from tangled rope (decree +
 *   extraction + some coordination) to rope/scaffold (genuinely coordinating)
 *   to piton (the decree persists as theater, but the real mechanism is now
 *   social norm and aspiration). This reading is distinct from an
 *   endogenous-climb reading (which would claim prestige accrued organically
 *   without initial state imposition) and an exogenous-override reading
 *   (which would claim the state never genuinely succeeded in forcing organic
 *   adoption and maintained permanent coercion). The hybrid cascade reading
 *   asserts that the state-imposed mechanism is NECESSARY to initiate the
 *   climb — without the decree's artificial fringe, no organic climb would
 *   occur — but once initiated, the climb becomes self-sustaining.
 *
 * KEY AGENTS:
 *   - State Apparatus / Central Authority (institutional/arbitrage) — Issues the decree, enforces initial compliance, benefits from unified military and administrative personnel.
 *   - Military Hierarchy (institutional/arbitrage) — Executes enforcement, maintains the fringe structure, benefits from guaranteed personnel compliance.
 *   - Subject Population / Conscript Cohorts (powerless/trapped → moderate/constrained → organized/mobile across generations) — Experiences coerced adoption in T0; constrained adoption in T10–T20; voluntary participation in T30+.
 *   - First-Generation Adopters (moderate/constrained/biographical) — Bear the full weight of the decree's extraction; begin internalizing prestige narratives.
 *   - Second and Subsequent Generations (moderate→powerful/mobile/generational) — Inherit the normalized fringe as aspiration; perceive voluntary participation as opportunity rather than coercion.
 *   - Organic Adoption Movement / Cultural Elites (organized/constrained/civilizational) — Articulate the prestige narrative, drive identity fusion, manage the transition from coercion to aspiration.
 *   - Analytical Observer (analytical/analytical/civilizational) — Risks naturalizing the cascade as immutable feature of state formation rather than contingent institutional achievement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_cascade_reading, 0.58).
domain_priors:suppression_score(hybrid_cascade_reading, 0.62).
domain_priors:theater_ratio(hybrid_cascade_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_cascade_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hybrid_cascade_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hybrid_cascade_reading, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_cascade_reading, "Hybrid Cascade: State-Imposed Fringe Climbing into Organic Adoption").
narrative_ontology:topic_domain(hybrid_cascade_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_cascade_reading, 'bf0e3585-3ff1-497e-8660-fa243fac90e2').
narrative_ontology:cs_kernel_codification('bf0e3585-3ff1-497e-8660-fa243fac90e2', formalized).
narrative_ontology:cs_authority_grounding('bf0e3585-3ff1-497e-8660-fa243fac90e2', extraction).
narrative_ontology:cs_interpretation_layer_present('bf0e3585-3ff1-497e-8660-fa243fac90e2').
narrative_ontology:cs_reading_relation('bf0e3585-3ff1-497e-8660-fa243fac90e2', hybrid_cascade_reading__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('bf0e3585-3ff1-497e-8660-fa243fac90e2', hybrid_cascade_reading__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('bf0e3585-3ff1-497e-8660-fa243fac90e2', foundational, state_imposition_causal_necessity).
narrative_ontology:cs_axiom_status(state_imposition_causal_necessity, holdable).
narrative_ontology:cs_axiom_grounding('bf0e3585-3ff1-497e-8660-fa243fac90e2', state_imposition_causal_necessity, empirically_contingent).
narrative_ontology:cs_axiom('bf0e3585-3ff1-497e-8660-fa243fac90e2', foundational, organic_climb_succeeds_after_initiation).
narrative_ontology:cs_axiom_status(organic_climb_succeeds_after_initiation, holdable).
narrative_ontology:cs_axiom_grounding('bf0e3585-3ff1-497e-8660-fa243fac90e2', organic_climb_succeeds_after_initiation, empirically_contingent).
narrative_ontology:cs_reference_frame('bf0e3585-3ff1-497e-8660-fa243fac90e2', state_mandate_unified_apparatus).
narrative_ontology:cs_drift_state('bf0e3585-3ff1-497e-8660-fa243fac90e2', contemporary_norm_internalization_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bf0e3585-3ff1-497e-8660-fa243fac90e2', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_cascade_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(hybrid_cascade_reading, military_hierarchy).
narrative_ontology:constraint_victim(hybrid_cascade_reading, subject_populations).
narrative_ontology:constraint_victim(hybrid_cascade_reading, local_authority_structures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Trapped in a mandatory adoption regime. The state-imposed fringe (military service, state employment as pathways to status) appears to coordinate a functional need (military defense, administrative capacity) but extracts compliance through suppression of alternatives. Exit from the fringe pathway means social exclusion. The population perceives the constraint as unchangeable because enforcement is total and organized resistance is suppressed.
constraint_indexing:constraint_classification(hybrid_cascade_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANSITIONAL COHORT / FIRST-GENERATION ADOPTERS (TANGLED ROPE) — These agents experience genuine coordination (the military/state apparatus does solve organizational problems they face: unified command, resource pooling, status elevation) alongside substantial extraction (mandatory service, suppression of alternative paths to status). They are constrained — they could theoretically refuse, but the cost is social death. At generational time, they can perceive that this structure is not immutable, yet they remain locked in.
constraint_indexing:constraint_classification(hybrid_cascade_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE APPARATUS AND MILITARY HIERARCHY (ROPE) — Pure beneficiary perspective. From their seat, the constraint is coordination: the state decree creates a pool of personnel (officials, soldiers) whose compliance is guaranteed by the decree's enforcement. The apparatus experiences the constraint as solving the organizational problem of reliable personnel. At immediate time, they see no extraction cost — the decree simply coordinates behavior they want. Arbitrage exit: they can abandon the decree at any time, so they perceive it as chosen coordination, not extraction.
constraint_indexing:constraint_classification(hybrid_cascade_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIC ADOPTION MOVEMENT / POST-DECREE NORMALIZATION (SCAFFOLD) — This perspective emerges once the first-generation cohort internalizes the fringe status as desirable (the constraint has completed its 'climb'). At this point, organized actors (cultural intellectuals, military reformers, administrative elites) can see the constraint as transitional: the decree was temporary enforcement machinery; the normalization that followed is the real institutional achievement. They see a sunset built into the structure: as the organic climb completes, the need for top-down decree fades. The constraint is a temporary bridge to a self-sustaining equilibrium. Constrained exit because deviating from the normalized fringe still carries cost, but the sunset is visible from this perspective.
constraint_indexing:constraint_classification(hybrid_cascade_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL PERFORMANCE / DECREE THEATER (PITON) — Once organic adoption is complete (the fringe has climbed successfully), the original decree persists as pure performance. The state continues to invoke the mandate, but enforcement is no longer necessary — social norm and aspiration have replaced coercion. The constraint operates with high theater_ratio because the decree's original function (forcing initial adoption) is complete, yet the decree remains in formal codes. The theater is maintained to preserve legitimacy continuity, but the functional core has atrophied.
constraint_indexing:constraint_classification(hybrid_cascade_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN FALLACY RISK) — From civilizational/global scope, the constraint risks being naturalized as an immutable feature of state formation: 'All modern states must create a unified military and administrative apparatus; the decree is merely the visible expression of a deeper necessity.' This perspective treats the hybrid cascade as a natural law — state formation requires unified personnel. However, the structural data contradicts this: the decree is a choice; the organic climb is contingent on specific cultural conditions (prestige framing, identity fusion, economic incentives). The mountain classification is a false summit masking a constructed constraint.
constraint_indexing:constraint_classification(hybrid_cascade_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_cascade_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_cascade_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_cascade_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_cascade_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, declining from 0.75 to 0.42 over interval): The constraint begins highly extractive because the decree imposes mandatory service with zero exit options — agents are forced into the fringe, benefits flow entirely to the state apparatus, suppression is total. Over 40 years, extractiveness declines because (a) voluntary participation increases (organic climb reduces the need for enforcement extraction), (b) agents begin benefiting from the fringe status (prestige, economic opportunity, identity rewards), and (c) the state's extraction pressure decreases because compliance is now internal to agents' desires. The trajectory models a successful transition: initial extraction (the decree forces what agents don't want) declining to moderate extraction (agents want the thing, but the state still benefits disproportionately from their unified compliance). Suppression (0.62, declining from 0.85 to 0.35): Suppression is highest at T0 when enforcement is active and alternatives are completely foreclosed. It declines as the organic climb progresses because agents no longer need external enforcement — they self-suppress alternative identities. However, suppression never reaches zero because the fringe status itself is maintained through social exclusion of those who refuse; the mechanism shifts from state enforcement to peer enforcement. Theater ratio (0.51, rising from 0.22 to 0.62): The decree is initially functional (low theater) because it is actively being enforced for genuine purposes (building military and administrative capacity). As the climb completes and voluntary participation rises, the decree becomes increasingly redundant — it persists in formal codes but enforcement is absent. The theatrical element rises because the decree must now maintain legitimacy through invocation rather than through force. By T30–T40, the constraint operates with substantial theater: the fringe is maintained by prestige and identity fusion, not by decree enforcement, yet the decree remains formally in place to preserve the historical narrative and institutional continuity.
 *
 * PERSPECTIVAL GAP:
 *   The maximum gap is between T0 (snare vs. rope — coerced vs. coordinated) and T40 (rope vs. piton — genuine opportunity vs. theatrical performance). At T0, the subject population perceives immutable coercion; the state perceives straightforward coordination. At T40, the subject population perceives desirable opportunity (but is identity-locked into the fringe); the state perceives a self-sustaining norm that requires minimal enforcement theater. The gap narrows between T0 and T40 because the shared understanding of the fringe converges — both populations now agree it is desirable — but the structural inequality persists: the state benefits from unified compliance, while the subject population pays the price of conformity. The cascade completes when the gap between subject-population-perception and state-apparatus-perception narrows from (snare vs. rope) to (rope vs. rope), but their material positions remain asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is the agent's structural position relative to the constraint at a given time point. At T0, the subject population has d→1.0 (full target): they are forced to participate, extract nothing, benefit nothing. The state apparatus has d→0.0 (full beneficiary): they impose the constraint, benefit maximally, extract without cost. At T20, the subject population has d→0.6 (moderate target): they are still constrained, but prestige and opportunity provide benefits; extraction runs toward them but they also gain. At T40, the subject population has d→0.4 (mild target): they participate voluntarily, gain substantial benefits, but the state still maintains some extraction through the fringe structure's hierarchy. The engine's directionality derivation chain uses beneficiary/victim declarations plus exit options plus time horizon to compute d for each perspective. This reading declares: beneficiaries = state apparatus + military hierarchy (they collect from the constraint throughout all 40 years). Victims = subject populations + local authority structures (they bear the initial suppression cost; the suppression declines but extraction persists in attenuated form). The beneficiary/victim split is stable across the entire interval, but the MEANING of 'beneficiary' and 'victim' changes: victims transition from unwilling conscripts to willing participants who still bear some cost relative to beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid cascade reading resolves mandatrophy by revealing that the original decree's mandate (create a unified military and administrative apparatus through mandatory service) DOES achieve its goal — the apparatus is unified and operational. However, the mandate becomes OBSOLETE once organic adoption is complete. At T0–T20, the mandate is live: it is still necessary to force participation because voluntary adoption is insufficient. At T20–T30, the mandate transitions to twilight status: voluntary participation is rising but enforcement remains to accelerate the climb. At T30+, the mandate is dead: voluntary participation is dominant, enforcement is theatrical, and the decree could be repealed without affecting the constraint's operation. Mandatrophy is resolved when the founding mandate (force unified military/administrative personnel) is no longer necessary for the constraint to persist. Under the hybrid cascade reading, this resolution occurs around T30 (40% of the interval), at which point the constraint enters piton status and could be declared resolved. However, the formality persists because (a) institutional inertia, (b) the state's interest in continuity of authority narratives, and (c) the fringe structure's need for formal legitimacy. The decree becomes the theater — it must be invoked to justify the fringe's continued prestige, even though the decree itself is no longer necessary to maintain compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_climb_mechanism,
    'What specific structural conditions convert top-down-imposed status (military/state service mandatory) into organic aspiration (fringe climbing because agents desire it)?',
    'Historical analysis of prestige markers, identity fusion mechanisms, economic opportunity pathways, and cultural narratives in the transition from decree enforcement to voluntary adoption. Cross-case comparison: what conditions enable organic climb vs. persistent enforcement?',
    'If climb is driven by economic incentive alone: the constraint remains extractive even after formal decree ends (mechanisms change, extraction persists). If climb is driven by prestige/identity fusion: the constraint becomes genuine coordination (and the tangled rope classification may understate the coordination component). If climb requires continuous cultural reinforcement: the piton perspective is correct (theater is necessary for maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_climb_mechanism, empirical, 'Mechanism converting top-down imposition into organic aspiration').

omega_variable(
    reading_boundary_cascade_vs_ossification,
    'Is this reading of the imposition-pathway kernel describing a successful transition (cascade completes, constraint enters piton/scaffold phase) or a failed transition where top-down coercion ossifies into permanent suppression?',
    'Historical trajectory: does the constraint show measurable decline in theater_ratio and suppression as the organic climb completes? Or does suppression remain constant despite organic climb, indicating theater is covering persistent enforcement? Longitudinal measurement of enforcement intensity and voluntary participation ratio over 50+ year span.',
    'If successful cascade: the constraint is tangled rope → scaffold → rope as it matures. If failed cascade (coercion ossifies): the constraint remains tangled rope or snare throughout, and the ''organic climb'' is theater. If permanently intermediate (theater masks persistent coercion): the constraint is a piton misclassified as cascade.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_cascade_vs_ossification, empirical, 'Whether the cascade represents genuine transition or ossified enforcement theater').

omega_variable(
    comparative_cascade_pathways,
    'Does this reading (state-imposed fringe that climbs organically) represent a distinct structural pathway from purely endogenous climb or purely exogenous override, or are these three readings merely different framings of the same constraint?',
    'Network analysis of the three readings'' computed types and metrics via the engine: if hybrid_cascade_reading and endogenous_climb_reading produce substantially different ε values when measured on the same historical case, they are separate constraints (ε-invariance principle applies). If ε values are similar, the readings are semantic variations of one constraint.',
    'If separate constraints (different ε): write three stories, link via network.affects_constraints, and clarify which observable (initial mechanism vs. final state vs. transition path) differentiates them. If same constraint: collapse to one story with three perspectives and route the methodological disagreement to omegas.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_cascade_pathways, conceptual, 'Whether hybrid_cascade is a distinct constraint or a framing of the shared imposition-pathway kernel').

omega_variable(
    mandate_obsolescence_timing,
    'When does the original decree''s mandate become obsolete? At the point of formal repeal, at the point of zero enforcement, or at the point of organic adoption dominance?',
    'Historical record: trace formal status of decree over time; measure enforcement intensity (arrests, penalties for non-compliance) and voluntary participation rate. Mandate becomes obsolete when voluntary participation exceeds some threshold (80%? 95%?) and enforcement intensity drops to near zero.',
    'If mandate obsolescence is formal: the constraint mandatrophy resolves when the decree is repealed, regardless of enforcement status. If mandate obsolescence is measured by enforcement intensity: the constraint resolves earlier (enforcement already abandoned). If mandate obsolescence is measured by voluntary adoption: the constraint may be formally obsolete but functionally persistent (piton status).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_obsolescence_timing, empirical, 'Timeline for mandate obsolescence and mandatrophy resolution').

omega_variable(
    kernel_vs_reading_identity,
    'Is the hybrid cascade a reading of the imposition_pathway_kernel, or does it represent a fundamentally different kernel (the prestige_mechanism_kernel or the identity_fusion_kernel)?',
    'Semantic analysis: does the reading claim that all three pathways (hybrid, endogenous, exogenous) converge on the same underlying phenomenon (imposition pathway as the shared kernel)? Or does it claim that the hybrid pathway is structurally distinct because it bridges imposition and endogenous adoption?',
    'If true sibling reading: the three readings coexist under one kernel, and the engine''s cross-reading coupling analysis applies. If distinct kernel: write separate constraint families and link via network.affects_constraints. This distinction affects how the engine computes false summits and cascade foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_identity, conceptual, 'Whether hybrid cascade is a reading of imposition_pathway_kernel or requires a distinct kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_theater_t0_decree_functional, hybrid_cascade_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(hybrid_theater_t10_ritual_emerging, hybrid_cascade_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(hybrid_theater_t20_climb_complete_theater_rises, hybrid_cascade_reading, theater_ratio, 20, 0.51).
narrative_ontology:measurement(hybrid_theater_t30_piton_phase_begins, hybrid_cascade_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(hybrid_theater_t40_performance_maintenance, hybrid_cascade_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(hybrid_extractiveness_t0_decree_imposition, hybrid_cascade_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(hybrid_extractiveness_t10_enforcement_consolidation, hybrid_cascade_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(hybrid_extractiveness_t20_organic_climb_phase, hybrid_cascade_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hybrid_extractiveness_t30_normalization_advanced, hybrid_cascade_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(hybrid_extractiveness_t40_maturity_phase, hybrid_cascade_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_suppression_t0_active_enforcement, hybrid_cascade_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(hybrid_suppression_t10_internalization_begins, hybrid_cascade_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(hybrid_suppression_t20_voluntary_participation_rise, hybrid_cascade_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(hybrid_suppression_t30_enforcement_attenuation, hybrid_cascade_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(hybrid_suppression_t40_norm_internalized, hybrid_cascade_reading, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hybrid_cascade_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(hybrid_cascade_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The hybrid_cascade_reading is one of three structural readings of the imposition_pathway_kernel. Sibling readings (endogenous_climb_reading, exogenous_override_reading) model alternative causal pathways for the same observable outcome (unified military/administrative apparatus). The three readings have different ε values: hybrid cascade has moderate extractiveness that declines over time (0.58 average); endogenous climb would have lower initial extractiveness (apparatus emerges via incentive, not coercion); exogenous override would have extraction concentrated in the override mechanism (decree is pure suppression, no positive fringe). Network links represent structural influence: if the hybrid cascade is correct, it forecloses both alternatives within a single causal framework. If the alternatives are correct, the hybrid cascade is mischaracterized. The three readings should be evaluated as competing hypotheses about the same constraint family, not as perspectives on one shared constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
