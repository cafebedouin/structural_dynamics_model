% ============================================================================
% CONSTRAINT STORY: demographic_elimination_imperative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_elimination_imperative, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: demographic_elimination_imperative
 *   human_readable: Demographic Elimination Imperative in Settler-Colonial State Formation
 *   domain: political_history/nationalism_studies/settler_colonial_studies
 *
 * SUMMARY:
 *   The demographic elimination imperative represents the structural
 *   requirement that a settler-colonial state formation project in a
 *   territory with an existing majority population must transform the
 *   demographic composition to achieve its foundational objective. In the
 *   case of Zionist state formation in Palestine (1897-1948), the project's
 *   core premise — establishing a Jewish-majority state — faced an
 *   irreducible mathematical constraint: the territory had an Arab majority
 *   (approximately 90% in 1897, 67% in 1947). This constraint exhibits
 *   mountain properties because it emerges from the logical structure of the
 *   project itself, not from contingent policy choices. The imperative is not
 *   'should we displace the Arab population?' but 'how can we achieve a
 *   Jewish majority without displacing the Arab population?' — and the
 *   mathematical answer is 'you cannot.' Transfer proposals appear
 *   consistently across the ideological spectrum (from Herzl's 1895 diary
 *   entries through Ben-Gurion's 1937-1948 statements to the 1948 Plan Dalet
 *   implementation) not because of individual malice but because the
 *   structural constraint admits no alternative solution. The constraint's
 *   accessibility collapse (0.92) reflects that all implementation pathways
 *   converge on population displacement mechanisms, whether through voluntary
 *   emigration incentives, land acquisition excluding Arab tenancy, or direct
 *   forced transfer. The low extractiveness (0.08) reflects that this is a
 *   structural necessity of the state formation logic, not an extractive
 *   mechanism imposed for rent-seeking — the displacement serves the
 *   project's foundational objective, not a parasitic interest. The low
 *   theater ratio (0.15) reflects that the imperative was openly discussed in
 *   internal Zionist discourse, with minimal performative concealment until
 *   post-1948 historiographic revision.
 *
 * KEY AGENTS:
 *   - Displaced Arab Population: Primary affected group (powerless/trapped) — experiences the imperative as an immutable force across all time horizons
 *   - Zionist Political Leadership: Implementing agents (powerful/mobile) — perceives the imperative as a structural necessity inherent to the state formation project
 *   - State Formation Institutions: Institutional actors (institutional/arbitrage) — Jewish National Fund land acquisition, Jewish Agency settlement planning, Haganah/IDF military operations — all structured around the demographic transformation objective
 *   - International Community: External observers (organized/mobile) — UN partition planners, British Mandate authorities, neighboring Arab states — recognize the demographic imperative as inherent to the partition logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies the constraint as a mathematical and structural necessity of settler-colonial state formation in territories with existing majority populations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_elimination_imperative, 0.08).
domain_priors:suppression_score(demographic_elimination_imperative, 0.03).
domain_priors:theater_ratio(demographic_elimination_imperative, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_elimination_imperative, extractiveness, 0.08).
narrative_ontology:constraint_metric(demographic_elimination_imperative, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(demographic_elimination_imperative, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(demographic_elimination_imperative, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(demographic_elimination_imperative, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_elimination_imperative, mountain).
narrative_ontology:human_readable(demographic_elimination_imperative, "Demographic Elimination Imperative in Settler-Colonial State Formation").
narrative_ontology:topic_domain(demographic_elimination_imperative, "political_history/nationalism_studies/settler_colonial_studies").

domain_priors:emerges_naturally(demographic_elimination_imperative).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED POPULATION / IMMEDIATE HORIZON (MOUNTAIN) — From the immediate perspective of those facing displacement, the imperative appears as an immutable force — a structural inevitability of the state formation project that cannot be negotiated or escaped. The constraint is experienced as a natural law of the political order being imposed.
constraint_indexing:constraint_classification(demographic_elimination_imperative, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DISPLACED POPULATION / BIOGRAPHICAL HORIZON (MOUNTAIN) — Over a biographical timeframe, the displaced population perceives the demographic imperative as unchangeable. The structural requirement for population transfer is embedded in the foundational logic of the state project and persists across policy variations and leadership changes.
constraint_indexing:constraint_classification(demographic_elimination_imperative, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE FORMATION PROJECT (MOUNTAIN) — The institutional actors implementing the state formation project perceive the demographic imperative as a structural necessity inherent to the project's definition. A Jewish state in an Arab-majority territory faces an irreducible mathematical constraint: achieving demographic majority requires population displacement. This is not a policy choice but a logical entailment of the state's foundational premise.
constraint_indexing:constraint_classification(demographic_elimination_imperative, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL COMMUNITY (MOUNTAIN) — Organized international actors (UN partition planners, mandate authorities, neighboring states) perceive the demographic imperative as a structural feature of the partition logic. The mathematical reality that creating a Jewish-majority state in an Arab-majority territory requires population transfer is recognized across all positions, though responses to this reality vary. The constraint itself is invariant.
constraint_indexing:constraint_classification(demographic_elimination_imperative, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical perspective, the demographic elimination imperative is a mathematical and structural necessity inherent to settler-colonial state formation in territories with existing majority populations. The constraint is a logical entailment: if the goal is an ethno-national state (Jewish majority) and the territory has a different demographic majority (Arab), then achieving the goal requires altering the demographic composition. This is not a contingent policy but a structural requirement that follows from the project's definition. The constraint exhibits natural law properties: it emerges from the logical structure of the state formation project, has near-zero degrees of freedom (no alternative pathway exists that preserves both the ethno-national character and the existing demography), and shows accessibility collapse (all attempted implementations converge on population displacement mechanisms).
constraint_indexing:constraint_classification(demographic_elimination_imperative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: POLITICAL LEADERSHIP (MOUNTAIN) — Political leaders implementing the state formation project perceive the demographic imperative as an unchangeable structural constraint. Internal debates focus on implementation mechanisms (voluntary transfer vs forced displacement, timing, scale) but not on whether demographic transformation is necessary. The constraint is treated as a given, not a choice.
constraint_indexing:constraint_classification(demographic_elimination_imperative, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_elimination_imperative_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(demographic_elimination_imperative, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_elimination_imperative, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(demographic_elimination_imperative, ExtMetricName, E),
    domain_priors:suppression_score(demographic_elimination_imperative, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(demographic_elimination_imperative),
    narrative_ontology:constraint_metric(demographic_elimination_imperative, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(demographic_elimination_imperative, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(demographic_elimination_imperative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The demographic imperative is a structural requirement of the state formation project, not an extractive mechanism. The displacement serves the project's foundational objective (achieving Jewish majority) rather than enabling rent-seeking or parasitic extraction. The low value reflects that this is closer to a natural law (mathematical necessity) than to an extractive constraint. Suppression (0.03): Very low. The imperative itself does not suppress alternatives through coercion — it is a logical entailment of the project's premises. The suppression that occurs (military force, legal restrictions, land acquisition mechanisms) is in the implementation, not in the constraint itself. The constraint is the structural necessity; the implementation mechanisms are separate (and would be modeled as distinct constraints with higher suppression values). Theater ratio (0.15): Very low. The demographic imperative was openly discussed in internal Zionist discourse from Herzl forward. Transfer proposals appear in private diaries, internal memoranda, and leadership debates with minimal concealment. The theater emerges only in post-1948 public historiography, not in the constraint's operation during the state formation period. Accessibility collapse (0.92): Very high. All pathways to achieving a Jewish-majority state in an Arab-majority territory converge on population displacement mechanisms. Voluntary emigration incentives, land acquisition excluding Arab tenancy, military operations creating refugee flows — these are implementation variations, not alternative solutions. The structural constraint admits no pathway that preserves both the ethno-national objective and the existing demography. Resistance (0.08): Very low. The constraint exhibits minimal resistance to implementation because it follows from the project's logical structure. Debates focus on timing, scale, and mechanisms, not on whether demographic transformation is necessary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all perspectives classify as mountain because the demographic imperative is a structural necessity that appears invariant across observation positions. The displaced population sees an immutable force. The implementing institutions see a structural requirement. The international community sees a logical entailment of partition. The analytical observer sees a mathematical necessity. The uniformity across perspectives is itself diagnostic: it indicates that the constraint is closer to a natural law (logical/mathematical necessity) than to a contingent institutional arrangement. The omega variables probe whether this mountain classification is genuine (structural necessity) or a false summit (contingent choice naturalized as necessity). If binational alternatives were viable, or if the imperative emerged only from partition rather than being inherent to the ideology, the mountain classification would degrade to tangled_rope or snare. But the structural evidence — the consistency of transfer proposals across time, leadership, and ideological factions — suggests the constraint is genuinely structural.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint is a uniform-type mountain — it classifies as mountain from all perspectives because it is a structural necessity inherent to the state formation project's logic, not a contingent policy choice. The displaced population experiences it as an immutable force (mountain from powerless/trapped). The implementing institutions perceive it as a structural requirement (mountain from institutional/arbitrage). The international community recognizes it as inherent to the partition logic (mountain from organized/mobile). The analytical observer identifies it as a mathematical necessity (mountain from analytical/analytical). There is no beneficiary/victim asymmetry in the usual sense — the constraint is not extractive (one group benefiting at another's expense) but structural (a logical requirement of the project that affects all parties as an unchangeable given). The low extractiveness reflects this: the displacement is not rent-seeking but a necessary condition for the state's existence as defined.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that very low extractiveness (0.08) combined with mountain classification across all perspectives indicates a structural necessity rather than an extractive mechanism. The mandatrophy question 'Is this coordination (necessary for state formation) or extraction (imposed for benefit)?' is answered by the accessibility collapse and resistance metrics: the constraint emerges from the project's logical structure (high accessibility collapse, low resistance) rather than from imposed coercion. The demographic imperative is not 'good' or 'justified' — it is a mathematical fact about what achieving a Jewish majority in an Arab-majority territory requires. The moral and political questions (should such a state be created? is the displacement justified? are there alternative state forms?) are separate from the structural question (does the project as defined require demographic transformation?). The mountain classification captures the structural necessity without adjudicating the normative questions. The low extractiveness reflects that this is not a parasitic constraint (one group extracting rents from another) but a foundational constraint (a logical requirement of the project's definition). The omega variables preserve the irreducible uncertainties: whether binational alternatives could satisfy core objectives, whether the imperative was inherent or emergent, whether alternative demographic thresholds exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binational_alternative_viability,
    'Could a binational state structure have satisfied the core objectives of the Zionist project while avoiding the demographic elimination imperative?',
    'Historical counterfactual analysis of binational proposals (Brit Shalom, Hashomer Hatzair, Magnes-Buber proposals); examination of whether Jewish national self-determination requires demographic majority or could be achieved through other institutional arrangements',
    'If binational alternatives were structurally viable: the demographic imperative was a policy choice (Snare), not a structural necessity (Mountain). If binational alternatives were incompatible with core project objectives: the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binational_alternative_viability, conceptual, 'Whether binational alternatives could satisfy core project objectives').

omega_variable(
    demographic_threshold_necessity,
    'What demographic threshold constitutes ''majority'' sufficient for state viability — simple majority, supermajority, or regional concentration?',
    'Comparative analysis of ethno-national state formation projects; examination of whether states with demographic parity or minority dominance achieve stability; analysis of whether regional concentration (Jewish majority in core areas, Arab majority in periphery) would satisfy the imperative',
    'If simple majority suffices: the imperative is less severe (lower extractiveness). If supermajority or total dominance required: the imperative is more severe (higher extractiveness). If regional concentration suffices: alternative pathways exist (weakens Mountain classification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_threshold_necessity, empirical, 'What demographic threshold satisfies the state formation imperative').

omega_variable(
    temporal_inevitability,
    'Was the demographic imperative present from the project''s inception (1897-1917) or did it emerge only after the 1947 partition plan created territorial boundaries?',
    'Textual analysis of early Zionist writings (Herzl, Nordau, Weizmann) for transfer proposals; examination of whether the imperative predates territorial definition or emerges from it; analysis of whether the constraint is inherent to Zionism or contingent on partition',
    'If present from inception: the imperative is a structural feature of the ideological project (Mountain). If emergent from partition: the imperative is a contingent response to specific territorial boundaries (Tangled Rope or Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_inevitability, empirical, 'Whether the imperative predates territorial partition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_elimination_imperative, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demo_elim_theater_1897, demographic_elimination_imperative, theater_ratio, 0, 0.12).
narrative_ontology:measurement(demo_elim_theater_1922, demographic_elimination_imperative, theater_ratio, 25, 0.14).
narrative_ontology:measurement(demo_elim_theater_1947, demographic_elimination_imperative, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(demo_elim_extract_1897, demographic_elimination_imperative, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(demo_elim_extract_1922, demographic_elimination_imperative, base_extractiveness, 25, 0.07).
narrative_ontology:measurement(demo_elim_extract_1947, demographic_elimination_imperative, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_elimination_imperative, identity_coordination).

% DUAL FORMULATION NOTE:
% The demographic elimination imperative is the foundational structural constraint of the Zionist state formation project. Implementation mechanisms (land acquisition, military operations, legal restrictions) would be modeled as separate downstream constraints with higher extractiveness and suppression values. This story models the structural necessity itself, not its implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
