% ============================================================================
% CONSTRAINT STORY: the_churn_systemic_upheaval
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_churn_systemic_upheaval, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: the_churn_systemic_upheaval
 *   human_readable: The Churn (Systemic Collapse and Rebirth)
 *   domain: political/social/economic
 *
 * SUMMARY:
 *   The Churn represents a period of fundamental institutional dissolution
 *   and reconfiguration where the rules governing social, political, and
 *   economic life lose binding force and are replaced through contestation
 *   and reorganization. This is not gradual reform — it is the collapse of
 *   the legitimacy and enforcement mechanisms that sustained the old order,
 *   followed by rapid institutional experimentation and power contestation.
 *   The constraint exhibits the full six-type spectrum because different
 *   actors experience the same structural phenomenon radically differently.
 *   For the economically displaced, it is pure extraction (Snare): loss
 *   without replacement. For institutional reformers, it is coordination
 *   (Rope): rebuilding according to new principles. For the old power
 *   apparatus, it is degraded ritual (Piton): the performance of authority
 *   without actual capacity. For the international capital class, it is
 *   extraction with escape options (Snare but mobile). For constitutional
 *   reformers, it is temporary transition with a sunset (Scaffold). For
 *   incumbent institutional survivors, it is mixed cost and benefit (Tangled
 *   Rope). And from the civilizational view, it risks being naturalized as an
 *   immutable law of politics (Mountain — but the data reveals this as a
 *   false summit). The Churn's extractiveness increases from 0.32 to 0.58
 *   over the interval, reflecting the accumulation of institutional failures
 *   and the transition from formal to informal coercion mechanisms. Theater
 *   ratio increases from 0.38 to 0.55, indicating that as real institutional
 *   capacity declines, symbolic performances of authority (military displays,
 *   constitutional theatrics) increase to compensate.
 *
 * KEY AGENTS:
 *   - Economically Displaced Populations: Primary victims (powerless/trapped) — bear full cost of institutional collapse without exit options; transition through old-order Snare into new-order institutional uncertainty
 *   - Institutional Survivors: Secondary victims and potential beneficiaries (moderate/constrained) — local government, civil service, regional businesses that must renegotiate their position in new institutional order
 *   - Insurgent Coalitions: Primary beneficiaries (institutional/arbitrage) — organized groups with coherent alternative institutional visions; capture state power during the Churn; can time their entry and exit
 *   - Constitutional Reform Movement: Organized agents (organized/constrained) — civil society, international observers, reform advocates pushing for institutionalized solutions with sunset logic; constrained by need to maintain legitimacy during transition
 *   - Incumbent Power Apparatus: Degraded institutional actor (institutional/constrained) — military, police, bureaucracy attempting to maintain performance of authority as actual capacity fails; trapped in old institutional forms
 *   - International Capital Class: Powerful but mobile victims (powerful/mobile) — global financial and corporate actors experiencing extraction of assets and normative authority; can escape via capital flight but lose institutional monopolies
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the Churn as an inevitable law of politics rather than a contingent outcome of institutional design failures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_churn_systemic_upheaval, 0.58).
domain_priors:suppression_score(the_churn_systemic_upheaval, 0.68).
domain_priors:theater_ratio(the_churn_systemic_upheaval, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, extractiveness, 0.58).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_churn_systemic_upheaval, tangled_rope).
narrative_ontology:human_readable(the_churn_systemic_upheaval, "The Churn (Systemic Collapse and Rebirth)").
narrative_ontology:topic_domain(the_churn_systemic_upheaval, "political/social/economic").

domain_priors:requires_active_enforcement(the_churn_systemic_upheaval).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_churn_systemic_upheaval, insurgent_coalitions).
narrative_ontology:constraint_beneficiary(the_churn_systemic_upheaval, institutional_reformers).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, incumbent_power_holders).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, economically_displaced_populations).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, rule_dependent_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY DISPLACED POPULATIONS (SNARE) — Trapped within the collapsing system without resources to exit or adapt. Bear full cost of institutional dissolution. No alternative pathways; extraction is experienced as complete destabilization and loss.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL SURVIVORS (TANGLED_ROPE) — Mid-tier institutional actors (local government, civil service, regional businesses) simultaneously undergo extraction (loss of monopoly authority, asset seizure, forced restructuring) and experience coordination benefit (restoration of order, new institutional forms that may better serve their interests). Constrained exit — can negotiate but not escape.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSURGENT COALITIONS (ROPE) — Beneficiary group that experiences the constraint as pure coordination: organizing rapid institutional replacement, establishing new authority structures, mobilizing constituencies. The Churn enables their preferred outcome. Arbitrage exit — can pivot between institutional forms, time their moves.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized agents (civil society, reform coalition, international observers) see the Churn as a temporary but necessary dissolution of failed institutions. The constraint has explicit sunset logic: the goal is to rebuild institutional capacity and establish new rules. Suppression is justified as necessary transition-time coercion, not permanent extraction. High theater — much of the reform movement's activity is symbolic legitimation of the new order.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INCUMBENT POWER APPARATUS (PITON) — The old state monopoly on coercion persists through residual institutions (military, police, bureaucracy) even as its primary function is delegitimized. The apparatus attempts to maintain performance of authority even while losing structural capacity. Theater_ratio high: ritual displays of state power, formal law enforcement continuing while informal warlordism actually governs. Extraction mechanism degraded because the apparatus cannot reliably enforce — it maintains form through inertia.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: INTERNATIONAL CAPITAL CLASS (SNARE) — Global financial and corporate actors experience the Churn as extraction of their portfolio assets, normative authority, and institutional monopolies. However, they retain high exit capacity — can relocate capital, shift investments to new institutions, arbitrage instability. Experienced extraction is high but not absolute because mobility provides escape. Classification: Snare with high d (0.85) due to victim status despite mobile exit.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, systemic collapse and institutional rebirth are inevitable features of social evolution: all systems eventually face demands for legitimacy they cannot meet, requiring dissolution and reconfiguration. This perspective sees the Churn as an immutable law of political economy. However, the structural data contradicts the mountain classification — observed suppression (0.68) and extractiveness (0.58) are contingent on power distribution and institutional design, not inherent to collapse itself. This is a false summit that naturalizes what is actually a mutable political outcome.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_churn_systemic_upheaval_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_churn_systemic_upheaval, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_churn_systemic_upheaval, TR),
    TR >= 0.70.

:- end_tests(the_churn_systemic_upheaval_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, increasing trajectory. The Churn's extractiveness is not distributed evenly — some groups (insurgent coalitions, reformers, mobile capital) experience it as relatively low extraction relative to their gains; others (displaced populations, incumbent elites losing monopolies) experience it as severe. The average reflects the fact that the Churn is fundamentally redistributive: old institutional rents are destroyed (extraction from beneficiaries of the old order) and new institutional opportunities are created (benefit for insurgents and reformers). Suppression (0.68): High. The Churn involves explicit abandonment of rule-of-law constraints and increased resort to coercion. Formal law enforcement breaks down, state monopoly on violence fragmentizes, and informal enforcement mechanisms proliferate. Suppression increases during the transition period as no single authority can establish binding rules. Theater ratio (0.55): Moderate. The Churn involves substantial symbolic activity — constitutional reform theater, military displays, legitimation rituals for new authority — but also genuine institutional experimentation and power contestation. As institutional capacity declines, the theater component increases (Goodhart drift in the measurement), but the Churn retains functional restructuring rather than pure performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The economically displaced see the Churn as pure catastrophe (Snare) — their old institutions failed, leaving them trapped. The insurgent coalitions see the Churn as opportunity (Rope) — they are building new institutions that align with their preferences. The incumbent power apparatus sees ritual degradation (Piton) — it maintains the performance of authority even as its capacity collapses. The international capital class sees theft of their monopolies (Snare) but with escape options (can relocate capital and operations). The institutional survivors see mixed fate (Tangled Rope) — they lose some authority and resources but gain opportunities to participate in new institutional forms. The constitutional reformers see temporary transition (Scaffold) — the Churn is a necessary but time-bounded dissolution with a sunset when new institutions stabilize. The civilizational observer risks seeing immutable law (Mountain) — that all systems eventually face collapse and rebuild — but the structural data contradicts this: the extractiveness and suppression levels are contingent on institutional design choices and power distribution, not inherent necessities.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary dramatically by agent structural position. Economically displaced populations (powerless/trapped/victim) have d ≈ 0.95, experiencing maximum effective extraction through the sigmoid function. Institutional survivors (moderate/constrained/mixed) have d ≈ 0.55, experiencing moderate extraction because they retain some negotiating capacity and potential benefit from new institutional forms. Insurgent coalitions (institutional/arbitrage/beneficiary) have d ≈ 0.10, experiencing negative or minimal extraction because they benefit from the transition and can time their moves. Constitutional reformers (organized/constrained/mixed) have d ≈ 0.50, balanced between extraction costs (suppression during transition, constraint from need to maintain legitimacy) and coordination benefits (building new institutional legitimacy). The incumbent power apparatus (institutional/constrained/victim) has d ≈ 0.65, experiencing extraction of their monopoly authority despite retaining some residual coercive capacity. The international capital class (powerful/mobile/victim) has d ≈ 0.85, experiencing high extraction of their institutional monopolies and assets, but the high d value is partially offset by their mobile exit options in the chi calculation. The analytical observer has d ≈ 0.72, experiencing the Churn as an abstract analytical challenge rather than direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Churn resolves the mandatrophy by demonstrating that institutional collapse is not a uniform Mountain phenomenon but a site of radical perspective divergence. The claim that 'systems inevitably collapse and rebuild' naturalizes what is actually a contingent political outcome. The difference between orderly institutional reform and catastrophic Churn collapse depends on specific design features: legitimacy buffer (how far public trust can decline before institutional dissolution), exit option distribution (whether alternative institutions exist before old ones fail), and power concentration (whether any group has monopoly capacity to impose new institutional order). The Churn is not inevitable — it is the outcome of institutional design failures, legitimacy crises, and power contestation. The mountain perspective is a false summit: it mistakes a frequent historical pattern for a law of nature. The constraint is genuinely Tangled Rope at the macro level: it contains both coordination function (rebuilding institutions) and asymmetric extraction (losses concentrated on economically displaced populations and incumbent elites). Mandatrophy is resolved by clarifying that the Churn is NOT a coordination problem that can be solved by better information sharing or voluntary compliance — it is fundamentally a power-redistribution event where old institutional beneficiaries must lose monopoly rents for new institutional forms to emerge. This is extraction in the service of coordination, making the tangled_rope classification appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_dissolution_threshold,
    'What institutional legitimacy threshold triggers the onset of systematic institutional dissolution versus orderly reform?',
    'Comparative historical analysis of legitimacy metrics (public trust, tax compliance, voluntary compliance with law) preceding systemic collapse versus preceding reform within existing institutions',
    'If threshold is well-defined and uniform: collapse may be predictable and potentially preventable. If threshold is context-dependent and path-dependent: the Churn is effectively a black swan event with low predictability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_dissolution_threshold, empirical, 'The legitimacy threshold triggering institutional dissolution').

omega_variable(
    insurgent_coalition_capture_risk,
    'Do insurgent coalitions that capture state power during the Churn systematically replicate the extraction patterns they replaced?',
    'Longitudinal analysis of post-Churn institutional structures; comparison of extraction rates under new regimes with preceding regimes; measurement of captured groups'' access to exit options',
    'If captured: the Churn is cyclical (tangled_rope at macro scale). If not captured: the Churn can represent genuine institutional improvement (scaffold logic validated). Affects mandatrophy classification at the system level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insurgent_coalition_capture_risk, empirical, 'Whether new institutional orders replicate or escape extraction patterns').

omega_variable(
    displacement_irreversibility,
    'Are the economically displaced populations from the Churn permanently locked out of new institutional arrangements, or can they be re-incorporated?',
    'Post-Churn economic mobility studies; access to new institutional resources; measured reintegration into formal economy and civic participation',
    'If permanent: displaced populations transition from snare victims in old order to snare victims in new order (no liberation). If reversible: the Churn represents a genuine restructuring that can benefit previously trapped agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_irreversibility, empirical, 'Whether displacement from the Churn is permanent or reversible').

omega_variable(
    coercive_capacity_continuity,
    'Does the Churn genuinely dissolve state monopoly on coercion, or merely redistribute it among competing institutions while maintaining suppression levels?',
    'Empirical measurement of coercion distribution: state violence, private armed group violence, informal enforcement; comparison of total suppression rates pre-Churn vs during vs post-Churn',
    'If genuinely dissolved: suppression should decrease measurably during the Churn. If merely redistributed: suppression persists or increases (warlordism, fragmented enforcement). Affects whether Churn is liberation or rearrangement of the same constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercive_capacity_continuity, empirical, 'Whether the Churn dissolves or redistributes coercive capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_churn_systemic_upheaval, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(churn_tr_t0, the_churn_systemic_upheaval, theater_ratio, 0, 0.38).
narrative_ontology:measurement(churn_tr_t2, the_churn_systemic_upheaval, theater_ratio, 2, 0.47).
narrative_ontology:measurement(churn_tr_t4, the_churn_systemic_upheaval, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(churn_be_t0, the_churn_systemic_upheaval, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(churn_be_t2, the_churn_systemic_upheaval, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(churn_be_t4, the_churn_systemic_upheaval, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_churn_systemic_upheaval, enforcement_mechanism).
narrative_ontology:affects_constraint(the_churn_systemic_upheaval, institutional_legitimacy_collapse).
narrative_ontology:affects_constraint(the_churn_systemic_upheaval, state_capacity_fragmentation).
narrative_ontology:affects_constraint(the_churn_systemic_upheaval, incumbent_rent_extraction).

% DUAL FORMULATION NOTE:
% The Churn is the macro-level constraint describing systemic institutional dissolution. It is upstream of and influences several more specific constraints: institutional_legitimacy_collapse (the loss of binding force of existing rules), state_capacity_fragmentation (the distribution of coercive capacity among competing institutions), and incumbent_rent_extraction (the contestation over old institutional monopolies). The Churn's extractiveness reflects the aggregate of these sub-constraints' operations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(the_churn_systemic_upheaval, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
