% ============================================================================
% CONSTRAINT STORY: asshole_filter_2015
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asshole_filter_2015, []).

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
 *   constraint_id: asshole_filter_2015
 *   human_readable: The Asshole Filter
 *   domain: psychological/social/organizational
 *
 * SUMMARY:
 *   The asshole filter is a systemic phenomenon in organizations and
 *   communities where established norms and boundaries intended to maintain
 *   cooperation and trustworthiness unintentionally repel non-transgressive
 *   people while attracting and rewarding transgressive ones. The mechanism
 *   is straightforward: enforce a norm, and people who respect norms comply;
 *   people who ignore norms violate freely; the system then selects for
 *   leadership from among the violators because they appear more confident,
 *   decisive, and willing to bend rules 'when necessary.' Over time, the
 *   institution becomes populated by people for whom norm-violation is a
 *   revealed preference, not a deviation. The irony is complete — the
 *   norm-enforcement mechanism selects its own subversion. This constraint
 *   exhibits genuine coordination function (the norms do coordinate behavior,
 *   at least temporarily) coupled with extraction (from cooperative members
 *   and from the institution's own trustworthiness as a collective good).
 *   Theater is high because organizations maintain elaborate codes of conduct
 *   and mission statements while their promotion practices contradict them.
 *   The filter is neither pure coordination nor pure extraction; it is a
 *   tangled rope: it coordinates while extracting, and it survives because
 *   both functions are real.
 *
 * KEY AGENTS:
 *   - Cooperative Members: Primary victims (powerless/trapped) — internalize norms in good faith; constrained by identity commitments; receive punishment for compliance
 *   - Transgressive Actors: Primary beneficiaries (institutional/arbitrage) — violate norms with impunity; selected into leadership; benefit from the institutional confusion their presence creates
 *   - Boundary Enforcers (Management/Leadership): Secondary beneficiaries and victims (moderate/constrained) — benefit from clear rules to enforce; degraded by the filter selecting their successors from rule-breakers
 *   - Institutional Trustworthiness: Collective victim (powerless/trapped) — abstract public good that cannot organize or exit; degraded by selection of transgressive leadership
 *   - Norm-Setting Theater: Institutional inertia carrier (institutional/arbitrage) — performative codes and mission statements persist despite contradictory incentives
 *   - Analytical Observer: Systemic view (analytical/analytical) — sees the complete feedback loop: norms select for their violators, violators become leaders, leaders model norm-violation, new norms are written to contain it, filter repeats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asshole_filter_2015, 0.52).
domain_priors:suppression_score(asshole_filter_2015, 0.65).
domain_priors:theater_ratio(asshole_filter_2015, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asshole_filter_2015, extractiveness, 0.52).
narrative_ontology:constraint_metric(asshole_filter_2015, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(asshole_filter_2015, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asshole_filter_2015, tangled_rope).
narrative_ontology:human_readable(asshole_filter_2015, "The Asshole Filter").
narrative_ontology:topic_domain(asshole_filter_2015, "psychological/social/organizational").

domain_priors:requires_active_enforcement(asshole_filter_2015).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asshole_filter_2015, transgressive_actors).
narrative_ontology:constraint_beneficiary(asshole_filter_2015, established_boundary_enforcers).
narrative_ontology:constraint_victim(asshole_filter_2015, cooperative_members).
narrative_ontology:constraint_victim(asshole_filter_2015, institutional_trustworthiness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COOPERATIVE MEMBER (SNARE) — Cooperative individuals internalize the norms and boundaries as legitimate social rules. They constrain their behavior in good faith. Yet the system rewards the transgressive actor, punishing cooperation. The cooperative member cannot exit without abandoning their identity commitments. Maximum extraction — they bear the cost of norm-maintenance without reward.
constraint_indexing:constraint_classification(asshole_filter_2015, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TRANSGRESSIVE ACTOR (ROPE) — The transgressive person experiences the constraint as a coordination mechanism that works in their favor. The norms exist; they ignore them; they profit from the social friction they create. They can exit at any time (reputational cost is low relative to material gain). Net beneficiary — the constraint coordinates others into compliance while they arbitrage the asymmetry.
constraint_indexing:constraint_classification(asshole_filter_2015, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: BOUNDARY ENFORCER (TANGLED ROPE) — Institutional actors tasked with boundary maintenance (managers, community leaders, HR) experience the constraint as a hybrid. They benefit from having clear rules to enforce (reduces ambiguity about decision-making). Yet the filter's function selects for transgressive people into leadership positions over time, which undermines the enforcer's own authority. Constrained exit — they are committed to the role but find it degrading as the system selects their successors from the rule-breakers.
constraint_indexing:constraint_classification(asshole_filter_2015, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL TRUSTWORTHINESS (SNARE) — The abstract collective good of institutional trustworthiness cannot exit or organize. As the filter selects transgressive people into positions of authority, the institution's reliability and predictability degrade. The system's own enforcement mechanism (promoting the people most willing to violate norms) undermines the condition it requires (norm-following). Trapped — no advocate, no exit, bearing full extraction cost.
constraint_indexing:constraint_classification(asshole_filter_2015, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NORM-SETTING THEATER (PITON) — Organizational codes of conduct, mission statements, and explicit value hierarchies are largely performative at the institutional level. They signal commitment to norm-following while the selection mechanisms (reward structures, promotion criteria) are indifferent to norm compliance. The theater persists through inertia — removing it would require acknowledging the filter's operation, which is costly. Theater ratio reflects the gap between stated values and incentive structures.
constraint_indexing:constraint_classification(asshole_filter_2015, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The civilizational view reveals the asshole filter as a genuine hybrid: it does coordinate behavior (people comply with stated norms in order to avoid transgressive punishment) while extracting from cooperative agents and degrading institutional trustworthiness (the primary coordination mechanism). The constraint is structurally stable because it rewards the people most willing to violate its own stated purposes, creating a selection cascade. Not a natural law but a stable institutional attractor with real coordination function and real extraction.
constraint_indexing:constraint_classification(asshole_filter_2015, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asshole_filter_2015_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asshole_filter_2015, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asshole_filter_2015, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asshole_filter_2015, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asshole_filter_2015, TR),
    TR >= 0.70.

:- end_tests(asshole_filter_2015_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The filter extracts from cooperative members (loss of status/reward for compliance) and from institutional trustworthiness (degradation of reputation). However, extractiveness is not as severe as a pure snare (0.70+) because the transgressive actors do not extract resources directly — they extract status and authority. The cooperative members remain functionally in the organization and continue to produce value; they are not eliminated. Theater ratio (0.58): High-moderate. The gap between stated organizational values (codes of conduct, mission statements) and actual promotion criteria (reward for confidence, decisiveness, norm-flexibility) is substantial. Approximately 58% of the institutional boundary-maintenance activity is performative — the codes are enacted as theater while the real selection mechanism operates independently. Suppression (0.65): High. Cooperative members face significant barriers to exit: sunk identity investment, social commitment to the stated values, belief that leaving represents personal failure rather than institutional failure. The filter suppresses alternatives (formation of parallel organizations, explicit rejection of the norms) because defection is costly and appears like moral compromise.
 *
 * PERSPECTIVAL GAP:
 *   The asshole filter creates maximum perspectival divergence because the same constraint appears as rope (beneficiary view), snare (victim view), and piton (institutional view) simultaneously. The beneficiary experiences low or negative extraction because they arbitrage the constraint for status gain. The victim experiences high extraction because they comply and are punished. The boundary enforcer experiences tangled rope — they maintain the system's legitimacy while watching it select for their own subversion. The collective good (institutional trustworthiness) is completely invisible to organizational decision-making, producing snare dynamics from a powerless, trapped position. The piton perspective reveals that institutional codes are maintained as theater because removing them would require explicit acknowledgment of the filter. The analytical view reveals the feedback loop: the constraint selects people who will undermine it, which makes it a stable attractor in organizational dynamics. The presheaf of perspectives shows that the constraint is not a single type but a family of nested extraction mechanisms, each visible from a different position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Cooperative members are victims with trapped exit — high d → high experienced extraction. Transgressive actors are beneficiaries with arbitrage exit — low d → negative experienced extraction (they benefit from the constraint). Boundary enforcers are constrained victims with partial benefit — medium d → medium extraction (they experience both the benefit of clear rules and the cost of selected successors who undermine their authority). Institutional trustworthiness is a powerless, trapped victim — maximum d → maximum experienced extraction. The norm-setting theater (institutional perspective) benefits from inertia — low d → negative extraction (the theater persists because it provides deniability). The analytical observer has analytical exit, which produces medium d under the derivation. The filter is stable when transgressive beneficiaries have sufficient power to resist feedback (arbitrage exit); it becomes unstable when cooperative members defect in sufficient numbers (threshold effect on collaborative capacity) or when institutional trustworthiness degrades to the point where external credibility fails. Boundary enforcers are the critical inflection point: if they remain committed to norm-enforcement despite observed violation, the filter persists; if they defect, the institutional authority structure collapses.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE STATUS: The asshole filter avoids mandatrophy because it exhibits genuine coordination function (the norms do coordinate behavior in short term) coupled with genuine extraction (from cooperative members and institutional trustworthiness). The mandatrophy risk is high because institutions often mislabel the filter as 'natural selection' (mountain) or 'inevitable organizational dynamics' (piton), when it is actually a tangible hybrid. The resolution requires acknowledging that: (1) norm-enforcement selects for norm-violators through reversed incentives, (2) this selection is not inevitable but contingent on reward structures, and (3) realigning incentives (rewarding norm-compliance rather than norm-flexibility) would change the selection mechanism. The theater ratio (0.58) indicates that approximately 58% of the institutional boundary-work is performative — the codes exist as cover for the real mechanism. If the theater ratio were 0.85+, the constraint would degrade to piton (inertial institution with lost function). If the theater ratio were 0.30–, the constraint would clarify to either pure rope (if beneficiary-focused) or pure snare (if victim-focused). The current 0.58 ratio reflects the genuine hybridity: the norms do some coordination work, but the selection mechanism does extraction work, and the two functions are in tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transgression_definition_boundary,
    'What constitutes ''transgressive'' in context? Is the filter selecting for norm-violators or for agents with sufficient power to ignore consequences?',
    'Comparative analysis of transgressive behavior across power levels; distinction between rule-breaking and consequence-immunity; measurement of enforcement selectivity by transgressor power status',
    'If power-based: the filter is a snare for the powerless and a rope for the powerful — purely extractive. If norm-based: the filter is a genuine coordination problem with misaligned incentives — tangled rope is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transgression_definition_boundary, conceptual, 'Whether transgression reflects norm-violation or power to ignore consequences').

omega_variable(
    institutional_selection_mechanism_clarity,
    'Do institutions explicitly select transgressive people, or does the filter emerge from implicit incentive structures (reward for confidence, speed, decisiveness regardless of boundary compliance)?',
    'Analysis of promotion criteria and actual promotion patterns; comparison of stated vs revealed preferences in hiring/advancement; interview data from decision-makers',
    'If explicit: institutions knowingly undermine trustworthiness — high mandatrophy signal. If implicit: the filter is a subtle misalignment of coordination and extraction — classic tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_selection_mechanism_clarity, empirical, 'Whether transgressive selection is explicit or implicit in institutional incentives').

omega_variable(
    cooperative_defection_threshold,
    'At what point do cooperative members defect or abandon the organization? Does the filter self-limit through cooperative exit, or do cooperative members remain trapped indefinitely?',
    'Longitudinal tracking of cooperative member retention; exit interviews; comparison of voluntary departure rates in high-filter vs low-filter institutions; measurement of cooperative member commitment over time',
    'If defection: the snare is unstable, and the filter cycles (reform → restoration of cooperation → re-emergence of filter). If trapped: the snare is stable, and institutional degradation continues until external shock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cooperative_defection_threshold, empirical, 'Whether cooperative members defect or remain trapped in high-filter institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asshole_filter_2015, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(assholefilter_tr_t0, asshole_filter_2015, theater_ratio, 0, 0.42).
narrative_ontology:measurement(assholefilter_tr_t5, asshole_filter_2015, theater_ratio, 5, 0.5).
narrative_ontology:measurement(assholefilter_tr_t10, asshole_filter_2015, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(assholefilter_be_t0, asshole_filter_2015, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(assholefilter_be_t5, asshole_filter_2015, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(assholefilter_be_t10, asshole_filter_2015, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asshole_filter_2015, enforcement_mechanism).
narrative_ontology:affects_constraint(asshole_filter_2015, organizational_trust_degradation).
narrative_ontology:affects_constraint(asshole_filter_2015, cooperative_member_retention).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(asshole_filter_2015, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
