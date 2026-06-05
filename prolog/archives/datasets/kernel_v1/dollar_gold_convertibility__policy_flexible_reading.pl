% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Conditional Obligation (Policy Flexible Reading)
 *   domain: international_political_economy/monetary_policy/international_law
 *
 * SUMMARY:
 *   The Bretton Woods system (1944-1971) established convertibility of the
 *   U.S. dollar into gold at $35 per troy ounce as a cornerstone of
 *   international monetary stability. This constraint story analyzes ONE
 *   reading of the contested kernel: the policy flexible reading, which
 *   interprets the convertibility obligation as conditional on and
 *   subordinate to U.S. domestic economic stability. Under this reading, the
 *   U.S. government may suspend or modify convertibility when required to
 *   pursue domestic monetary policy objectives (maintaining employment,
 *   controlling inflation, managing capital flows). This reading dominated
 *   U.S. policy from the late 1960s onward, particularly after the 1968
 *   London Gold Pool collapse forced de facto convertibility suspension, and
 *   culminated in Nixon's 1971 formal abandonment of the gold standard. The
 *   structural effect is to shift the constraint from a binding rule (strict
 *   reading) into a coordination mechanism that preserves U.S. monetary
 *   autonomy at the cost of creating devaluation risk for dollar holders.
 *   Extractiveness rises over the interval as the contradiction between
 *   domestic monetary expansion and fixed-rate gold convertibility becomes
 *   unsustainable, forcing the U.S. to exploit the flexibility
 *   interpretation. Theater increases correspondingly: formal compliance with
 *   Bretton Woods language persists long after practical policy has moved to
 *   subordinating convertibility to domestic needs.
 *
 * KEY AGENTS:
 *   - United States Monetary Authority: Primary beneficiary — retains monetary autonomy through flexible interpretation of convertibility obligation
 *   - Dollar Reserve Currency Holders (central banks, institutions): Primary victims — face devaluation risk as convertibility becomes conditional rather than guaranteed
 *   - Foreign Governments and Trade Partners: Secondary victims — constrained by dollar dependence and vulnerable to U.S. monetary policy shocks
 *   - International Monetary System Participants (IMF, central bank networks): Organized actors — benefit from dollar-based coordination, bear extraction risk from convertibility subordination
 *   - Bretton Woods Legal Framework: Institutional actor — formal Articles of Agreement persist as theater while substantive convertibility obligation becomes discretionary
 *   - Analytical Observer: Civilizational position — risks naturalizing U.S. monetary hegemony as inevitable rather than contingent political structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.52).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.48).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Conditional Obligation (Policy Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_policy/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'c4401a68-7e48-4785-bf91-a294c6c31456').
narrative_ontology:cs_kernel_codification('c4401a68-7e48-4785-bf91-a294c6c31456', fixed_text).
narrative_ontology:cs_authority_grounding('c4401a68-7e48-4785-bf91-a294c6c31456', extraction).
narrative_ontology:cs_interpretation_layer_present('c4401a68-7e48-4785-bf91-a294c6c31456').
narrative_ontology:cs_reading_relation('c4401a68-7e48-4785-bf91-a294c6c31456', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('c4401a68-7e48-4785-bf91-a294c6c31456', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('c4401a68-7e48-4785-bf91-a294c6c31456', foundational, domestic_monetary_autonomy_paramount).
narrative_ontology:cs_axiom_status(domestic_monetary_autonomy_paramount, holdable).
narrative_ontology:cs_axiom_grounding('c4401a68-7e48-4785-bf91-a294c6c31456', domestic_monetary_autonomy_paramount, instrumental).
narrative_ontology:cs_axiom('c4401a68-7e48-4785-bf91-a294c6c31456', secondary, convertibility_obligation_not_self_enforcing).
narrative_ontology:cs_axiom_status(convertibility_obligation_not_self_enforcing, holdable).
narrative_ontology:cs_axiom_grounding('c4401a68-7e48-4785-bf91-a294c6c31456', convertibility_obligation_not_self_enforcing, empirically_contingent).
narrative_ontology:cs_reference_frame('c4401a68-7e48-4785-bf91-a294c6c31456', conditional_domestic_priority_framework).
narrative_ontology:cs_drift_state('c4401a68-7e48-4785-bf91-a294c6c31456', post_london_gold_pool_collapse, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('c4401a68-7e48-4785-bf91-a294c6c31456', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_monetary_autonomy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, dollar_reserve_currency_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_discipline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOLLAR RESERVE HOLDERS (SNARE) — Central banks and institutional investors holding dollars as reserves face devaluation risk if convertibility becomes conditional on U.S. domestic economic priorities. No exit option: selling dollars crashes their own currency valuations. Trapped agents bearing full extraction cost as U.S. regains monetary autonomy at their expense.
constraint_indexing:constraint_classification(dollar_gold_convertibility__policy_flexible_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FOREIGN GOVERNMENTS AND TRADE PARTNERS (SNARE) — Constrained by dollar dependence for international trade settlements and currency reserves. High cost to diversify away from dollar; limited alternatives available (gold standard alternatives were the point of Bretton Woods). Convertibility conditionality reduces their leverage and increases vulnerability to U.S. monetary policy shocks.
constraint_indexing:constraint_classification(dollar_gold_convertibility__policy_flexible_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITED STATES MONETARY AUTHORITY (ROPE) — Sees convertibility as a coordination mechanism that enables dollar-based international settlement. The policy flexibility reading treats the obligation as subordinate to domestic stability, meaning the U.S. can suspend or modify convertibility when needed. Net beneficiary: retains monetary autonomy while maintaining dollar hegemony. Low extraction cost because the U.S. can exit the constraint entirely through policy action.
constraint_indexing:constraint_classification(dollar_gold_convertibility__policy_flexible_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL MONETARY SYSTEM PARTICIPANTS (TANGLED ROPE) — Organized actors (IMF, central bank networks, finance ministers) benefit from dollar-based settlement efficiency (coordination) while bearing exposure to U.S. monetary policy subordinating convertibility to domestic needs (extraction). Constrained by path dependence: dollar system is entrenched, but conditionality increases systemic risk. Active enforcement occurs through expectations management and periodic crisis coordination.
constraint_indexing:constraint_classification(dollar_gold_convertibility__policy_flexible_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BRETTON WOODS LEGAL FRAMEWORK (PITON) — The formal Articles of Agreement specify convertibility obligations, but under the policy flexible reading, these obligations are theater: the U.S. unilaterally reinterprets them as conditional on domestic stability. The legal text persists through institutional inertia and diplomatic courtesy, but enforcement capacity has atrophied. Theater ratio reflects that compliance with the convertibility clause is now discretionary rather than automatic.
constraint_indexing:constraint_classification(dollar_gold_convertibility__policy_flexible_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the policy flexible reading naturalizes monetary hegemony as an inherent feature of international finance: the dominant currency issuer always retains the option to subordinate external obligations to domestic priorities. This appears as an immutable law of monetary systems rather than a contingent political choice. False summit candidate: the constraint is actually a negotiated power structure, not a law of nature.
constraint_indexing:constraint_classification(dollar_gold_convertibility__policy_flexible_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dollar_gold_convertibility__policy_flexible_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dollar_gold_convertibility__policy_flexible_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, TR),
    TR >= 0.70.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The policy flexible reading permits the U.S. to extract monetary autonomy by subordinating external obligations to domestic priorities. This is extraction in the sense that dollar holders bear devaluation risk while the U.S. enjoys freedom to expand money supply. However, the extraction is not total (not Snare-level) because the U.S. benefits from dollar hegemony only if external actors continue to accept dollars as reserves — the coordination function (settlement efficiency) creates a binding reciprocal interest. The U.S. cannot extract infinitely without destroying the dollar system itself. Suppression (0.48): Moderate. Dollar holders are suppressed by lack of alternatives — they cannot exit without revaluing their entire currency positions downward. But suppression is incomplete: during the Bretton Woods period, the London Gold Pool and gold market alternatives provided some exit capacity (expensive but available). Theater ratio (0.58): Moderate-high. Over the interval, formal compliance with convertibility language persists (theater grows) while de facto policy increasingly subordinates convertibility to domestic needs. By 1971, the gap between stated obligation (formal $35/oz) and actual policy (suspension of gold sales) is maximal. The theater reflects the diplomatic and political cost of formally renouncing Bretton Woods rather than suspending it under a flexible interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Maximum divergence occurs between the U.S. authority and dollar holders: one sees low-cost coordination (rope), the other sees forced exposure to devaluation risk (snare). This gap is diagnostic of false equivalence — the constraint is not symmetric. The policy flexible reading explicitly acknowledges asymmetry: the obligation is conditional on domestic priorities, meaning the U.S. has a valve that others do not. The gap would narrow under the strict convertibility reading (which would see more uniform snare classification across all external actors, including the U.S.). The organized system's tangled rope view is realistic: they genuinely benefit from dollar settlement efficiency (coordination) while bearing real exposure to discretionary suspension (extraction). The piton classification reveals institutional degradation: Bretton Woods as a formal legal structure is theater after the gold pool collapse, maintained through diplomatic convention rather than enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from the structural distribution of extraction across agents. The U.S. as primary beneficiary (institutional power + arbitrage exit) gets d ≈ 0.15 (low target exposure): they can exit the constraint entirely through policy action, so experienced extractiveness chi is negative or near-zero. Dollar holders as primary victims (powerless + trapped) get d ≈ 0.95 (maximum target exposure): no exit options mean they absorb full extraction cost. Foreign governments with constrained exit get d ≈ 0.72 (high target): expensive but available alternatives. The organized international system with constrained exit gets d ≈ 0.65 (moderate target): they can coordinate policy responses but not exit the dollar system. The sigmoid f(d) amplifies these differences, producing the observed perspectival gap. The piton classification at institutional long-term view reflects theater_ratio dominance: the formal legal structure (low extraction) is overridden by theater observations (high performativity).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING CONSTRAINT: The policy flexible reading resolves mandatrophy by explicitly rejecting the strict convertibility interpretation — the obligation is subordinate, not binding. This is not a failure to classify; it is a choice among competing readings that have different extractiveness profiles. Under the strict reading, the constraint would classify as rope (binding rule with no flexibility) or snare (unsustainable rule that suppresses U.S. policy). Under the policy flexible reading, it classifies as tangled rope (coordination with escape hatch). The mandatrophy is resolved at the kernel level: the reading disputes which interpretation of the Bretton Woods text is legitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_scope_ambiguity,
    'What counts as a legitimate ''domestic economic stability'' justification for suspending convertibility?',
    'Historical analysis of suspension events (1933, 1968 gold pool collapse, 1971 Nixon Shock); comparison of stated justifications (unemployment, inflation, capital flight) against actual triggering conditions; longitudinal study of whether credible domestic crisis preceded each suspension event',
    'If ''stability'' is narrowly defined (runaway hyperinflation only): convertibility obligation retains significant force and constraint reclassifies toward snare for dollar holders. If ''stability'' is broadly defined (any adverse inflation/employment trend): the policy flexibility reading wins, constraint firmly classifies as tangled rope with U.S. as net beneficiary. Current practice suggests broad definition, supporting the policy flexible reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditionality_scope_ambiguity, empirical, 'What constitutes legitimate grounds for conditional convertibility suspension').

omega_variable(
    enforcement_mechanism_collapse,
    'After 1968 (London Gold Pool collapse), what prevented the U.S. from formally renouncing the convertibility obligation rather than suspending it in 1971?',
    'Documentary analysis of policy debates 1968-1971; interview data from Treasury and Federal Reserve officials; examination of whether formal renunciation was politically impossible (reputational cost) or legally impossible (treaty constraints). Counterfactual: if the U.S. had explicitly renounced convertibility in 1968, would dollar hegemony have persisted?',
    'If formal renunciation was merely politically costly but legally available: the policy flexible reading is a form of de facto nullification of Bretton Woods, reducing the constraint''s legitimacy and shifting classification toward snare for external actors (they were following a rule the rule-maker could abandon unilaterally). If formal renunciation was legally constrained: the constraint retains more binding force and classification edges back toward rope (the U.S. had less discretion than the flexible reading suggests).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_collapse, empirical, 'Why formal renunciation rather than suspension was not pursued').

omega_variable(
    reading_foreclosure_test,
    'Does the policy flexible reading logically foreclose the strict convertibility reading, or do they coexist as contestable interpretations of the same treaty text?',
    'Textual analysis: do Articles of Agreement Section 10(d) (permissible IMF constraints on convertibility) and Article VIII (obligations of members) together establish an unambiguous rule, or do they permit multiple consistent interpretations? Legal precedent analysis: have courts or adjudicating bodies ruled one reading out as doctrinally incoherent, or have both readings appeared in authoritative legal commentary?',
    'If foreclosure is textually established: the policy flexible reading is the legally correct interpretation, and the strict reading is doctrinally invalid (eliminates coexistence, strengthens policy flexible reading''s legitimacy). If coexistence is established: both readings are defensible from the treaty text, and the actual outcome (U.S. dominance of the flexible reading) reflects power rather than law (maintains both as live contestation, softens policy flexible reading''s claim to legitimacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the policy flexible and strict convertibility readings logically foreclose each other').

omega_variable(
    bretton_woods_counterfactual_stability,
    'Would a binding (non-conditional) convertibility obligation have prevented U.S. inflation in the 1960s, or would binding convertibility have merely forced earlier crisis and adjustment?',
    'Monetary history analysis: comparison of U.S. inflation trajectories under strict convertibility (1944-1968) with trajectories under fiat after 1971; decomposition of inflation drivers (Vietnam War spending, Great Society, wage-price spiral) against the constraint''s actual suppressive effect. Counterfactual modeling: if the U.S. could not suspend convertibility, what would have happened to money supply, gold reserves, and political support for the system?',
    'If binding convertibility would have prevented inflation: the policy flexible reading''s core claim (that subordinating convertibility to domestic stability is necessary) is validated. If binding convertibility would merely have forced earlier formal collapse: the constraint was always unsustainable, the policy flexible reading reflects pragmatic acceptance of what strict reading could not enforce, and both readings become relics of an impossible commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bretton_woods_counterfactual_stability, empirical, 'Counterfactual: would binding convertibility have forced earlier Bretton Woods collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_flex_tr_t0, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(dgc_flex_tr_t8, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(dgc_flex_tr_t16, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(dgc_flex_be_t0, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dgc_flex_be_t8, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(dgc_flex_be_t16, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 16, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dgc_flex_su_t0, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dgc_flex_su_t8, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(dgc_flex_su_t16, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 16, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_asymmetric_burden_sharing).

% DUAL FORMULATION NOTE:
% The dollar_gold_convertibility kernel decomposes into at least three structurally distinct constraint stories with different extractiveness values. This story models the policy flexible reading (ε=0.52, tangled rope) where convertibility is conditional. The strict reading (sibling, not this story) would have lower ε and classify as rope or snare. The Triffin reading (another sibling) identifies the structural incompatibility between domestic and international monetary obligations, potentially classifying as mountain (inherent contradiction). All three are linked via network.affects_constraints to signal their shared origin in the same treaty kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
