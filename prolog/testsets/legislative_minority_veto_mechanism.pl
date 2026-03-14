% ============================================================================
% CONSTRAINT STORY: legislative_minority_veto_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legislative_minority_veto_mechanism, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legislative_minority_veto_mechanism
 *   human_readable: Legislative Minority Veto Mechanism
 *   domain: political/governance
 *
 * SUMMARY:
 *   Legislative minority veto mechanisms — supermajority requirements,
 *   filibuster rules, absolute veto powers held by numerically small
 *   coalitions — create a structural constraint where the ability to block
 *   policy becomes a form of extraction. Originally designed as a protection
 *   against tyranny-of-the-majority and a coordination device ensuring broad
 *   consensus on transformative policies, veto mechanisms evolve into
 *   extractive instruments when the minority holding veto power begins to use
 *   obstruction not as a genuine protection mechanism but as leverage for
 *   side benefits, regulatory carve-outs, or preservation of status quo
 *   positions that contradict the majority's electoral mandate. The
 *   constraint exhibits all six types from different perspectives because it
 *   genuinely coordinates minority representation while simultaneously
 *   extracting from majority responsiveness.
 *
 * KEY AGENTS:
 *   - Excluded Majority: Primary victim (powerless/trapped) — holds electoral mandate but legislative power is blocked by supermajority requirements; no exit from democratic institutions
 *   - Veto-Holding Minority: Primary beneficiary (institutional/arbitrage) — uses obstruction to extract side deals and policy carve-outs; can arbitrage between coalition partners and the majority
 *   - Status-Quo Institutional Order: Secondary beneficiary (institutional/arbitrage) — benefits from reduced legislative volatility and preserved constitutional arrangement
 *   - Transitional Demographic Group: Secondary victim (moderate/constrained) — caught in coalition dynamics; constrained by eventual demographic change that will shift voting power
 *   - Reform Coalition: Organized challenger (organized/mobile) — building coalition to overcome or reform veto mechanisms; mobile in strategy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as structural necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legislative_minority_veto_mechanism, 0.58).
domain_priors:suppression_score(legislative_minority_veto_mechanism, 0.62).
domain_priors:theater_ratio(legislative_minority_veto_mechanism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legislative_minority_veto_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(legislative_minority_veto_mechanism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legislative_minority_veto_mechanism, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legislative_minority_veto_mechanism, tangled_rope).
narrative_ontology:human_readable(legislative_minority_veto_mechanism, "Legislative Minority Veto Mechanism").
narrative_ontology:topic_domain(legislative_minority_veto_mechanism, "political/governance").

domain_priors:requires_active_enforcement(legislative_minority_veto_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legislative_minority_veto_mechanism, minority_faction_holding_veto).
narrative_ontology:constraint_beneficiary(legislative_minority_veto_mechanism, status_quo_preserving_interests).
narrative_ontology:constraint_victim(legislative_minority_veto_mechanism, legislative_majority_will).
narrative_ontology:constraint_victim(legislative_minority_veto_mechanism, policy_responsiveness_to_demographic_change).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MAJORITY (SNARE) — The majority electoral coalition holds legislative power but cannot exercise it due to supermajority requirements, filibuster rules, or veto mechanisms. Trapped within democratic institutions with no exit. Maximum experienced extraction: majority preferences cannot become policy despite electoral victory. No coordination benefit — the mechanism purely obstructs.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANSITIONAL MINORITY (TANGLED ROPE) — A minority faction (regional, ideological, or demographic) uses veto leverage to block majority-backed legislation while simultaneously negotiating side deals within the constraint framework. Constrained by eventual demographic change or electoral shifts, but benefits from the veto during their window of structural leverage. Both coordination (bargaining) and extraction (obstruction) present.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATUS-QUO INSTITUTIONAL ORDER (ROPE) — Constitutional designers (Founders, constituent assemblies) often embedded veto mechanisms as protection against majoritarian capture and as coordination for regional or minority rights representation. From the institutional perspective, the veto IS the coordination mechanism: it ensures that transformative policies require supermajority consensus rather than bare majoritarian will. The mechanism solves the collective action problem of preventing tyranny-of-the-majority in principle.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized actors (voting rights organizations, demographic coalitions, reform parties) see the veto mechanism as a temporary obstacle being eroded by demographic change and institutional reform. Mobile in their strategies — can organize around the constraint's boundaries, build supermajorities, or reform the veto rules themselves. The constraint has a sunset: as demographics shift or reform coalitions build sufficient power, veto mechanisms lose structural leverage.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PARLIAMENTARY RITUAL (PITON) — Procedural invocations of veto mechanisms (filibustering, supermajority requirements, committee obstruction) have evolved into performative rituals divorced from their original protective function. The veto persists through institutional inertia long after the threat it was designed to prevent has changed. Theater ratio (0.55) reflects that formal obstruction tactics dominate substantive negotiation.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (STRUCTURAL NECESSITY) — From a universal analytical view, some form of minority representation and veto capacity is a structural necessity in any system seeking to balance majority rule with minority protection. The tension between majority will and minority safeguard is not resolvable into pure coordination or pure extraction. However, this perspective risks naturalizing what is actually a contingent institutional choice — many democracies function without supermajority veto mechanisms.
constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legislative_minority_veto_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legislative_minority_veto_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legislative_minority_veto_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legislative_minority_veto_mechanism, TR),
    TR >= 0.70.

:- end_tests(legislative_minority_veto_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The veto mechanism begins as a low-extractiveness coordination device (0.35) but evolves toward higher extractiveness (0.58) as the minority faction shifts from protecting substantive rights toward using veto leverage to extract side deals and prevent responsive policy change. The trajectory reflects institutional drift from protective mechanism toward rent-seeking instrument. Suppression (0.62): High. Blocked majorities face substantial barriers — constitutional barriers, procedural rules, supermajority requirements — to overcome the veto. The barriers are structural and formal, giving them high legitimacy even when the actual minority using veto is not the minority the mechanism was designed to protect. Theater ratio (0.55): Moderate. Veto invocation involves performative procedural rituals (filibustering, obstruction, formal blocking) but retains real obstructive function — it is not purely theatrical. The ratio suggests that procedure (theater) and function (obstruction) are roughly balanced; veto mechanisms have not yet degraded into full piton status in most democracies.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the majority (snare perspective) and the institutional order (rope perspective) reveals the core tension: the same structural mechanism coordinates minority protection AND extracts from majority responsiveness. Neither perspective is false — both describe the constraint's real structure. The mandatrophy is resolved by accepting that the veto mechanism is genuinely both coordination and extraction, with the balance between them depending on empirical questions: Is the minority being protected a substantive minority (ethnicity, religion, region, ideology) with genuine historical grievances, or a structural privilege holder (economic class, professional guild, institutional beneficiary) using the veto to maintain status quo? Does the veto enhance negotiation and compromise, or does it enable obstruction without reciprocal engagement? Has demographic change made the 'protected' minority no longer genuinely vulnerable?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position: whether the agent benefits from obstruction or bears its cost. The veto-holding minority faction has low d (high beneficiary status) — they experience arbitrage opportunities and side-deal leverage. The excluded majority has high d (victim status) — they bear the full cost of blocked policy. The reform coalition has moderate d — they face obstruction but have mobile strategies and organizational capacity. The institutional order protecting the veto has low d — it benefits from reduced legislative volatility. Derived f(d) values range from near-zero for beneficiaries to 1.4+ for trapped majorities, reflecting the wide asymmetry in who experiences extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The legislative minority veto mechanism resolves the mandatrophy by distinguishing genuine minority protection (rope) from extractive obstruction (snare) through empirical analysis of who holds veto power and for what purpose. A veto protecting a structurally vulnerable minority from majoritarian capture is coordination — it ensures policies require supermajority consensus and genuine coalition-building. A veto protecting a status-quo-holding structural privilege holder from majority-backed reform is extraction — it blocks responsive policy while the minority extracts side deals and carve-outs. The empirical questions (omegas 1, 2, 3) determine which description applies. The mechanism itself is structurally tangled_rope: it contains both genuine coordination (minority representation) and genuine extraction (majority obstruction) in a hybrid form. The perspectival gap between rope and snare reveals this hybrid structure rather than indicating misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_protective_intent_degradation,
    'Has the veto mechanism shifted from protecting genuine minority rights to protecting structural privilege?',
    'Historical analysis of who holds veto power over time; demographic analysis of protected group vs. structural beneficiary; comparison of stated intent vs. deployed outcomes',
    'If protecting genuine minority: classification shifts toward rope. If protecting structural privilege: classification confirms snare from majority perspective. Fundamental to mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_protective_intent_degradation, empirical, 'Whether veto protects minority rights or structural privilege').

omega_variable(
    alternative_coordination_pathways,
    'Could minority protection be achieved through proportional representation, consensus requirements, or other mechanisms without blocking majority legislative will?',
    'Comparative institutional analysis; simulation of alternative veto structures; outcomes in multi-party consensus systems vs. majoritarian systems',
    'If alternatives equally effective: current veto mechanism is revealed as extractive choice. If current mechanism uniquely protective: tangled_rope classification supported. Determines whether supermajority veto is genuine coordination or rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_pathways, conceptual, 'Whether alternative minority protection mechanisms exist').

omega_variable(
    demographic_permanence_assumption,
    'Is the blocking minority actually permanent, or does the veto''s power depend on temporary demographic/electoral alignment?',
    'Projection of demographic trends; analysis of historical minority shifts; historical cases where vetoing minority became majority',
    'If minority permanent: veto is stabilizing mechanism (rope). If minority temporary: veto is transition extractor that loses force as demographics shift (scaffold). Affects sunset clause validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_permanence_assumption, empirical, 'Whether veto-holding minority is demographically permanent').

omega_variable(
    negotiation_functionality_under_veto,
    'Does the veto mechanism enhance genuine bargaining and compromise, or does it enable obstruction without negotiation?',
    'Analysis of legislative outcomes: proportion of legislation negotiated within constraint vs. proportion blocked without negotiation; deal-making patterns; post-veto compromise rates',
    'If enhances bargaining: tangled_rope (coordination + extraction mixed). If enables pure obstruction: snare (extraction without coordination benefit). Central to distinguishing extraction from coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_functionality_under_veto, empirical, 'Whether veto enables negotiation or pure obstruction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legislative_minority_veto_mechanism, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legminveto_tr_t0, legislative_minority_veto_mechanism, theater_ratio, 0, 0.4).
narrative_ontology:measurement(legminveto_tr_t3, legislative_minority_veto_mechanism, theater_ratio, 3, 0.48).
narrative_ontology:measurement(legminveto_tr_t6, legislative_minority_veto_mechanism, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(legminveto_be_t0, legislative_minority_veto_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legminveto_be_t3, legislative_minority_veto_mechanism, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(legminveto_be_t6, legislative_minority_veto_mechanism, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legislative_minority_veto_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(legislative_minority_veto_mechanism, democratic_majority_rule_principle).
narrative_ontology:affects_constraint(legislative_minority_veto_mechanism, coalition_formation_dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
