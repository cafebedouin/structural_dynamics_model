% ============================================================================
% CONSTRAINT STORY: 1945_truman_san_francisco_conference_proceeding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1945_truman_san_francisco_conference_proceeding, []).

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
 *   constraint_id: 1945_truman_san_francisco_conference_proceeding
 *   human_readable: Truman's San Francisco Conference Commitment and UN Institutional Establishment
 *   domain: governance/international_relations
 *
 * SUMMARY:
 *   The San Francisco Conference of 1945 established the United Nations as
 *   the institutional framework for post-war international governance.
 *   Truman's commitment to the UN embedded the U.S. in a multilateral system
 *   designed to prevent unilateral conquest, manage great-power competition
 *   through negotiation, and legitimize collective security through
 *   rules-based procedures. This constraint exhibits the full tension between
 *   coordination and extraction. Multilateralism genuinely solves the
 *   coordination problem of preventing WWIII-scale conflict through mutual
 *   deterrence and negotiated dispute resolution. Simultaneously, it extracts
 *   from small nations (who gain vote equality but lose decision-making
 *   power) and from great powers (who lose unilateral action capacity but
 *   gain legitimacy and bloc coordination benefits). The constraint's
 *   extractiveness has increased over the interval (0.18 → 0.38) as Cold War
 *   bloc dynamics hardened and the veto mechanism crystallized as a
 *   great-power protection device. Theater ratio has risen (0.35 → 0.48) as
 *   elaborate procedural machinery perpetuates the appearance of collective
 *   decision-making while real conflicts are resolved outside the
 *   institutional frame.
 *
 * KEY AGENTS:
 *   - United States: Founding beneficiary (institutional/arbitrage) — architects the rules-based order; can circumvent UN decisions while maintaining legitimacy of the framework
 *   - Soviet Union: Early beneficiary, later constrained (institutional/arbitrage to institutional/constrained) — gains permanent seat and veto; constrained by inability to reshape rules or coordinate non-aligned bloc
 *   - Small Nations: Primary victims (powerless/trapped) — formal vote equality but excluded from Security Council decisions; trapped because UN legitimacy makes alternatives to participation illegitimate
 *   - Allied Non-Superpower Nations: Secondary victims and beneficiaries (organized/constrained) — benefit from collective security against fascism; constrained by dependence on superpower sponsorship
 *   - Decolonization Movement: Temporary beneficiaries with sunset (organized/mobile) — scaffold constraint enables anti-colonial bloc voting and legitimacy for independence claims; exit option improves as colonies gain independence
 *   - League of Nations Legacy: Institutional inertia carrier (institutional/constrained) — perpetuates failed institutional forms through sunk costs and diplomatic networks; theater-high, function-low
 *   - Analytical Observer: Risks naturalizing contingency (analytical/analytical) — tempted to see UN as natural law of state survival rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1945_truman_san_francisco_conference_proceeding, 0.32).
domain_priors:suppression_score(1945_truman_san_francisco_conference_proceeding, 0.38).
domain_priors:theater_ratio(1945_truman_san_francisco_conference_proceeding, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1945_truman_san_francisco_conference_proceeding, extractiveness, 0.32).
narrative_ontology:constraint_metric(1945_truman_san_francisco_conference_proceeding, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(1945_truman_san_francisco_conference_proceeding, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1945_truman_san_francisco_conference_proceeding, tangled_rope).
narrative_ontology:human_readable(1945_truman_san_francisco_conference_proceeding, "Truman's San Francisco Conference Commitment and UN Institutional Establishment").
narrative_ontology:topic_domain(1945_truman_san_francisco_conference_proceeding, "governance/international_relations").

domain_priors:requires_active_enforcement(1945_truman_san_francisco_conference_proceeding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1945_truman_san_francisco_conference_proceeding, peace_seeking_nations).
narrative_ontology:constraint_beneficiary(1945_truman_san_francisco_conference_proceeding, rules_based_order_proponents).
narrative_ontology:constraint_beneficiary(1945_truman_san_francisco_conference_proceeding, multilateral_governance_advocates).
narrative_ontology:constraint_victim(1945_truman_san_francisco_conference_proceeding, us_unilateral_action_capacity).
narrative_ontology:constraint_victim(1945_truman_san_francisco_conference_proceeding, great_power_autonomous_decision_making).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL NATION (SNARE) — Bound by UN Charter with no meaningful exit option; participation is obligatory for legitimate state recognition. Experiences the constraint as pure extraction: formal vote equality is theater (Security Council veto structure and great-power bloc coordination render small-state votes ineffective). Trapped by the machinery of collective security itself — the constraint promises protection but delivers only marginalization from actual decision-making. Maximum suppression because formal alternatives to UN involvement are delegitimized by the framework itself.
constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED COALITION (NON-SUPERPOWER) (TANGLED ROPE) — Genuine coordination benefits from collective security commitments and dispute resolution machinery, but also extraction through structural subordination to superpower blocs. Benefits from rules-based order that constrains fascism and unilateral conquest; constrained by inability to act independently of superpower sponsors. Can exit (withdraw from UN, rejoin if permitted) but at significant diplomatic and security cost. Mixed experience: coordination and extraction are both real.
constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: UNITED STATES (BENEFICIARY POSITION) (ROPE) — Experiences the UN commitment as pure coordination. The U.S. is positioned as the architect of the rules-based order; multilateralism serves U.S. interests by legitimating its leadership and spreading its governance model globally. Arbitrage exit option: the U.S. can circumvent UN decisions (Korea, Vietnam, Iraq show this), withdraw institutional support, or fund alternative mechanisms. The constraint is experienced as enabling, not limiting — multilateralism amplifies U.S. power by enlisting allies through rules rather than direct domination. Net beneficiary through coordinating preferred outcomes.
constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVIET UNION (BENEFICIARY POSITION, EARLY) (ROPE) — In 1945, the USSR benefits from the UN framework: it acquires permanent Security Council seat with veto power, gaining status parity with the U.S. and Britain. Multilateralism provides legitimacy for Soviet regional interests in Eastern Europe and protection against coordinated Western action through the veto mechanism. Arbitrage exit: the Soviet Union can use or ignore UN decisions depending on interest. Constraint appears as coordination in the immediate post-war moment, before Cold War competition hardens bloc structures.
constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SUPERPOWER (LONG-TERM / GENERATIONAL) (TANGLED ROPE) — As cold war dynamics unfold over decades, the UN constraint shifts from beneficial coordination to mixed extraction. The superpower benefits from the legitimacy the UN provides for its actions and the rules-based framing that constrains peer rivals; it is constrained by Security Council deadlock, inability to unilaterally reshape rules, and institutional resistance to power transitions. Constrained exit: military withdrawal of support is possible (U.S. did not pay dues periodically; USSR existed formally in UN while acting unilaterally) but at cost of legitimacy loss and alliance weakening. The extraction is real — the constraint does prevent unilateral conquest — but mixed with coordination benefits.
constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DECOLONIZATION MOVEMENT (SCAFFOLD) — The UN framework provides temporary structural leverage for anti-colonial organizing: the General Assembly enables non-aligned nations to coordinate bloc voting, and the charter's self-determination language legitimates independence claims. This is a scaffold constraint: the framework is temporary support for a transitional dynamic (colonialism → independence). As decolonization succeeds, the movement's exit option becomes higher (newly independent states become permanent participants). The constraint has a sunset: as colonialism ends, the decolonization movement's structural need for UN legitimacy declines. Theater is moderate-low: the General Assembly voting outcome is real (resolutions pass), even though enforcement is weak.
constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: LEAGUE OF NATIONS LEGACY (PITON) — The UN institutional structure replicates League of Nations machinery (Assembly, Council, Secretariat, International Court). From a civilizational perspective, the constraint perpetuates institutional forms whose functional deficits are well-known: the League failed to prevent WWII; the UN repeats many of the same structural vulnerabilities (Security Council veto paralysis mirrors great-power bloc deadlock in the League; General Assembly lacks enforcement capacity). The constraint persists through institutional inertia and the sunk cost of established diplomatic networks. Theater ratio high: elaborate procedural machinery obscures the underlying absence of enforcement mechanisms. The institution appears functional through ritual (resolutions, debates, symbolic votes) while real disputes continue outside the institutional frame.
constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the UN constraint appears as a natural law of international relations: once nation-states recognize that unilateral conflict is mutually destructive, some form of coordinating mechanism is logically necessary. The constraint appears inevitable given the structural facts (multiple great powers, weapons of mass destruction, incompatible territorial claims). The mountain classification frames multilateralism as inherent to the survival of state systems. However, the structural data contradicts this: beneficiaries exist (U.S., USSR in early period, powerful nations generally), extraction mechanisms are visible (small nations excluded from decisions, unilateral action capacity suppressed), and institutional alternatives exist (direct bilateral treaties, spheres-of-influence arrangements, military alliances). The analytical observer risks naturalizing a contingent institutional choice.
constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1945_truman_san_francisco_conference_proceeding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1945_truman_san_francisco_conference_proceeding, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(1945_truman_san_francisco_conference_proceeding, TR),
    TR >= 0.70.

:- end_tests(1945_truman_san_francisco_conference_proceeding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The constraint exhibits both genuine coordination (dispute resolution machinery, collective security commitment, rules-based procedure) and real extraction (small nations' exclusion from decisions, great-power veto capacity, suppression of unilateral action by non-superpowers). The intermediate value reflects that extraction is not as severe as a pure snare (small nations do gain some voice, enforcement is legitimacy-based rather than coercive) but more substantial than a pure rope (asymmetric decision power is structurally embedded). The upward trajectory (0.18 → 0.38) reflects hardening of Cold War bloc structures and realization that the veto enables great-power extraction through blocking. Suppression (0.38): Moderate. Barriers to exit from UN participation include delegitimization of alternatives (unilateral action appears illegitimate if UN-authorized mechanism exists), economic/diplomatic penalties for withdrawal, and security risks of isolation. But suppression is not total — states can and do act unilaterally (Korea, Vietnam, invasions of small states), and withdrawal is formally possible (though costly). The constraint does suppress alternatives without making them impossible. Theater ratio (0.48): Moderate. The UN generates significant performative activity (resolutions, speeches, procedural debates) that creates appearance of collective decision-making. But the theater is not dominant — General Assembly resolutions do carry symbolic weight, Security Council authorization does provide legitimacy even when enforcement is absent, and the institutional machinery does channel disputes into negotiation rather than immediate violence. The ratio is rising as bloc dynamics harden and the gap between procedural output and real power grows.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays profound perspectival divergence driven by power position and exit options. The small nation sees a snare: formal equality masking powerlessness, trapped by the framework's legitimacy. The U.S./Soviet beneficiary sees a rope: coordination that amplifies their preferred outcomes through legitimacy and alliance structure. The allied non-superpower sees tangled rope: genuine coordination benefits mixed with structural subordination. The decolonization movement sees a scaffold: temporary institutional support with a real sunset clause. The analytical observer at civilization scale risks seeing a mountain (inevitable necessity of multilateralism given the atomic age) — but the beneficiary presence and structural data reveal this as a false summit, a naturalization of a contingent institutional choice. The perspectival gaps reveal the constraint's true structure: it is a tangled rope that appears as different types depending on structural position. The powerful experience coordination; the powerless experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. The U.S. as institutional beneficiary with arbitrage exit (can circumvent UN while maintaining legitimacy) derives low d (~0.10) → low/negative χ, experiencing the constraint as coordination (Rope). The small nation as powerless victim with trapped exit (must participate because alternatives are delegitimized) derives high d (~0.92) → high χ, experiencing the constraint as extraction (Snare). The organized non-superpower allied nation as victim with constrained exit (can withdraw but at security/diplomatic cost) derives moderate-high d (~0.72) → moderate χ, experiencing mixed extraction and coordination (Tangled Rope). The Soviet Union early period as institutional beneficiary with arbitrage exit derives low d (~0.12), experiencing coordination; later period (generational timescale) as constrained (cannot reshape rules) derives moderate d (~0.58), experiencing mixed (Tangled Rope). The analytical observer at universal scope derives d from the position-independent fact that the constraint is being described: d ≈ 0.72 (observer outside the coordination flow), which would produce moderate-high χ and suggest analytical-perspective classification. The false-summit detector flags the mountain classification because beneficiaries are declared (U.S., USSR, rules-based order proponents) — the constraint is not a natural law but a constructed institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely a tangled rope — it contains both real coordination and real extraction — and that the mandatrophy arises from different perspectives emphasizing different aspects. The U.S. perspective emphasizes the coordination (legitimate multilateralism amplifying U.S. preferred outcomes) and classifies as Rope. The small-nation perspective emphasizes the extraction (powerlessness despite formal equality) and classifies as Snare. Both are factually accurate readings of different parts of the same constraint structure. The analytical observer's temptation to call it a Mountain (natural law of atomic-age politics) is a false summit — the constraint is a designed institutional choice with identifiable beneficiaries and victims, not an immutable limit. The mandatrophy resolves to: this is a tangled rope. Its extractiveness is moderate because both coordination and extraction are structurally real. The perspectival divergence is diagnostic, not a failure of classification. The constraint works (prevents WWIII-scale war through negotiated competition), and it extracts (excludes small nations from decisions, suppresses unilateral action by non-superpowers). These are not contradictory — they are two aspects of the same institutional mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_hegemonic_legitimation,
    'Does the UN framework function as genuine coordination for collective security, or primarily as a legitimation mechanism for the prevailing power structure?',
    'Comparative analysis of UN-authorized vs unilateral military actions; measurement of General Assembly resolution compliance by power status; statistical analysis of vetoes by superpower interest',
    'If coordination-dominant: constraint is primarily Rope/Tangled Rope. If legitimation-dominant: constraint is primarily Snare/Piton with false-summit mountain framing. Classification shifts significantly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_hegemonic_legitimation, empirical, 'Whether UN functions as genuine coordination or hegemonic legitimation').

omega_variable(
    veto_mechanism_functionality,
    'Does the permanent member veto protect legitimate regional interests (stabilizing mechanism) or primarily enable great-power extraction through blocking coalition action (destabilizing mechanism)?',
    'Historical analysis of veto usage patterns; comparison of conflict escalation in UN-authorized vs vetoed dispute regions; measurement of veto justification rhetoric vs strategic interests',
    'If stabilizing: veto is coordination feature (Rope features). If extractive: veto enables snare dynamics for vetoing powers and their clients. Dramatically shifts perspective classifications for superpowers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_mechanism_functionality, empirical, 'Veto mechanism as stabilizing vs extractive feature').

omega_variable(
    small_state_structural_exclusion,
    'Is small-state exclusion from real decision-making (Security Council structure) a necessary coordination cost, or a fundamental extraction mechanism built into the institutional design?',
    'Comparative analysis of voting alignment patterns; measurement of General Assembly resolution influence on Security Council outcomes; case study analysis of small-state interests that aligned vs conflicted with superpower preferences',
    'If necessary cost: small-state snare perspective reflects coordination floor (higher theater ratio acceptable). If designed extraction: small-state snare perspective reveals false-coordination architecture. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_state_structural_exclusion, empirical, 'Small-state exclusion as coordination cost vs designed extraction').

omega_variable(
    unilateral_action_constraint_reality,
    'Does the UN constraint actually suppress unilateral great-power action, or primarily suppress unilateral action by middle powers and small states while permitting superpower circumvention?',
    'Quantitative analysis of military action justifications pre- and post-UN; measurement of UN authorization rates for superpower vs other nations; case study of vetoed vs authorized actions by initiator power level',
    'If constraint is genuinely bilateral: U.S./USSR perspectives shift from Rope to higher extraction. If constraint selectively enforced: classification reflects asymmetric suppression (different Snare/Rope readings for different power levels). Directionalityoverides may be needed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_action_constraint_reality, empirical, 'Whether UN constraint applies symmetrically across power levels').

omega_variable(
    rules_based_order_alternative_availability,
    'Were there materially viable alternatives to the UN model for post-war coordination (spheres of influence, concert of great powers, regional associations)?',
    'Analysis of post-WWII policy debates and rejected proposals; counterfactual modeling of bloc coordination under alternative frameworks; historical evidence of deliberation at San Francisco Conference',
    'If alternatives existed: UN choice was contingent institutional design (false-summit risk high). If alternatives non-viable: UN appears more inevitable (mountain classification gains credibility). Affects whether beneficiary presence triggers false-summit detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rules_based_order_alternative_availability, conceptual, 'Existence of materially viable alternatives to UN model').

omega_variable(
    decolonization_movement_structural_dependence,
    'Did the scaffold constraint (UN framework enabling decolonization organizing) actually accelerate decolonization, or would independence movements have succeeded through different mechanisms (direct military struggle, bilateral negotiation with colonial powers)?',
    'Comparative analysis of independence timelines and methods for UN-member vs non-member colonies; measurement of UN General Assembly resolution impact on colonial power withdrawal decisions; case study analysis of independence movements that used vs bypassed UN framework',
    'If UN-dependent: scaffold classification is accurate, sunset will occur as decolonization completes. If UN-marginal: scaffold is theater, constraint is primarily Snare for small nations throughout. Changes judgment on temporal evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decolonization_movement_structural_dependence, empirical, 'Whether decolonization structurally depended on UN scaffold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1945_truman_san_francisco_conference_proceeding, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(1945_tr_t0, 1945_truman_san_francisco_conference_proceeding, theater_ratio, 0, 0.35).
narrative_ontology:measurement(1945_tr_t5, 1945_truman_san_francisco_conference_proceeding, theater_ratio, 5, 0.42).
narrative_ontology:measurement(1945_tr_t15, 1945_truman_san_francisco_conference_proceeding, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(1945_be_t0, 1945_truman_san_francisco_conference_proceeding, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(1945_be_t5, 1945_truman_san_francisco_conference_proceeding, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(1945_be_t15, 1945_truman_san_francisco_conference_proceeding, base_extractiveness, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1945_truman_san_francisco_conference_proceeding, enforcement_mechanism).
narrative_ontology:affects_constraint(1945_truman_san_francisco_conference_proceeding, cold_war_bloc_coordination).
narrative_ontology:affects_constraint(1945_truman_san_francisco_conference_proceeding, decolonization_legitimacy_mechanism).
narrative_ontology:affects_constraint(1945_truman_san_francisco_conference_proceeding, great_power_veto_dynamics).

% DUAL FORMULATION NOTE:
% The San Francisco Conference constraint is upstream of multiple structural dynamics: Cold War bloc coordination depends on the UN framework for legitimacy and procedure; decolonization movements depend on the UN General Assembly for bloc coordination and independence legitimacy; great-power veto mechanisms depend on the permanent Security Council seat structure. Each downstream constraint has its own extractiveness value reflecting the specific mechanism, but all depend on the foundational UN institutional architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1945_truman_san_francisco_conference_proceeding, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
