% ============================================================================
% CONSTRAINT STORY: portuguese_party_system_realignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_portuguese_party_system_realignment, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: portuguese_party_system_realignment
 *   human_readable: Portuguese Party System Realignment and Elite Coordination Lock
 *   domain: political_economy/institutional_dynamics
 *
 * SUMMARY:
 *   The Portuguese party system realignment (2011-present) represents a
 *   structural transition from the post-1974 two-party consensus model
 *   (Socialist Party and Social Democrats alternating in power) to a
 *   multi-party coordination system requiring confidence-and-supply
 *   arrangements or formal coalitions. This constraint exhibits tangled-rope
 *   characteristics: genuine coordination function (incumbent parties solving
 *   the problem of governing with fragmented electoral support) layered with
 *   asymmetric extraction (insurgent parties gaining representation but
 *   constrained by established party gatekeeping; voters gaining preference
 *   options but facing coalition unpredictability). The constraint emerged in
 *   response to austerity policies (2010-2015), which delegitimized both
 *   major parties and created electoral space for Left Bloc expansion, Green
 *   Party growth, and (later) Chega's right-authoritarian challenge. The
 *   theater ratio has risen steadily as the gap between formal
 *   government-formation rituals and informal confidence-and-supply
 *   negotiations widens: publicly, the Socialist minority government under
 *   António Costa appeared to govern alone, but effectively governed through
 *   monthly renegotiation of support from Left Bloc and Greens. This gap
 *   between the performative apparatus (formal coalition negotiations,
 *   presidential consultations) and the actual mechanism (backroom
 *   party-leadership bargaining) characterizes the piton perspective. Voters
 *   experience this realignment as a mixed coordination-extraction hybrid:
 *   they gained genuine choice (Left Bloc, Greens, Chega represent real
 *   electoral preferences), but this choice comes with suppressed information
 *   about coalition bargaining and constrained ability to predict
 *   governmental composition. The cordon sanitaire against Chega represents
 *   an enforcement mechanism with rising costs: if Chega's vote share
 *   continues to grow (from 1.3% in 2019 to 8% in 2023 polling), maintaining
 *   the public pledge of non-cooperation while privately negotiating becomes
 *   increasingly untenable.
 *
 * KEY AGENTS:
 *   - Established Parliamentary Parties (PS, PSD, CDS): Primary beneficiaries (institutional/arbitrage) — retain gatekeeping power over coalition formation despite vote-share decline; can coordinate with insurgent parties from position of strength; can withdraw support if terms are unacceptable
 *   - Voters in Electoral Volatility: Primary victims (powerless/trapped) — face constrained exit: voting for insurgents signals preference but produces unpredictable coalition outcomes; voting for establishments locks them in old consensus but enables stability; no exit option avoids cost
 *   - Insurgent Left Coalition (Left Bloc, Greens): Secondary victims (organized/constrained) — gain parliamentary representation and leverage through confidence-and-supply arrangements, but remain structurally subordinate to PS; can vote against budgets (as Greens did in 2023) but cannot force coalition inclusion
 *   - Chega and Right-Authoritarian Challengers: Secondary victims (moderate/constrained) — fill political space and gain electoral votes, but systematically excluded from coalition discussions through cordon sanitaire; confined to oppositional role despite electoral growth
 *   - Electoral Reform Advocates: Organized agents (organized/mobile) — see realignment as solvable through institutional design (electoral thresholds, proportional representation calibration, coalition formation rules); pursuing scaffold solution with sunset clause
 *   - Constitutional and Administrative Ritual: Institutional actor (institutional/arbitrage) — formal mechanisms of government formation (presidential consultations, parliamentary confidence votes) persist through inertia despite functional displacement by informal bargaining; high theater ratio reflects degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(portuguese_party_system_realignment, 0.58).
domain_priors:suppression_score(portuguese_party_system_realignment, 0.48).
domain_priors:theater_ratio(portuguese_party_system_realignment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(portuguese_party_system_realignment, extractiveness, 0.58).
narrative_ontology:constraint_metric(portuguese_party_system_realignment, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(portuguese_party_system_realignment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(portuguese_party_system_realignment, tangled_rope).
narrative_ontology:human_readable(portuguese_party_system_realignment, "Portuguese Party System Realignment and Elite Coordination Lock").
narrative_ontology:topic_domain(portuguese_party_system_realignment, "political_economy/institutional_dynamics").

domain_priors:requires_active_enforcement(portuguese_party_system_realignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(portuguese_party_system_realignment, established_parliamentary_parties).
narrative_ontology:constraint_beneficiary(portuguese_party_system_realignment, governmental_continuity_actors).
narrative_ontology:constraint_victim(portuguese_party_system_realignment, electoral_volatility_absorbers).
narrative_ontology:constraint_victim(portuguese_party_system_realignment, voter_preference_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VOTER IN REALIGNMENT (SNARE) — Voters face trapped exit: voting for established parties reinforces the coordination lock; voting for insurgent parties (Chega, Left Bloc variants) signals preference but produces unpredictable coalition outcomes. The voter cannot exit the constraint without bearing the cost of governmental instability or accepting reduced representation. High suppression: information asymmetry about coalition formation rules, media framing that amplifies fear of fragmentation, and strategic party signaling about unacceptability thresholds.
constraint_indexing:constraint_classification(portuguese_party_system_realignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURGENT PARTY COALITION (TANGLED ROPE) — Socialist Party, Left Bloc, Green Party, and other challengers benefit from the realignment (gain seats, media attention, governance leverage through supply-and-confidence arrangements) while being constrained by the established parties' willingness to deny coalition participation. Genuine coordination function exists: they provide governmental stability through informal confidence-and-supply arrangements (2015-2019 Socialist minority government supported by Left Bloc and Greens). But extraction is asymmetric: established parties can withdraw support, leaving insurgents vulnerable. Active enforcement: PS conditional support withdrawal threatens governmental collapse.
constraint_indexing:constraint_classification(portuguese_party_system_realignment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED PARTY CARTEL (ROPE) — Socialist Party and Social Democrats experience the constraint as pure coordination: they are solving the problem of maintaining governmental stability despite electoral fragmentation. The confidence-and-supply logic enables minority governments without formal coalition. Arbitrage exit: if confidence is withdrawn, the cartel can shift to alternative governing partners or new elections. Net beneficiary position — receives deference from insurgent parties and ability to exclude them from formal power. Suppression is low from their perspective because they retain control over bargaining terms.
constraint_indexing:constraint_classification(portuguese_party_system_realignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RIGHT-AUTHORITARIAN INSURGENTS (TANGLED ROPE) — Chega experiences both coordination and extraction. Coordination function: provides voters an outlet for anti-establishment sentiment; fills political space left by center-right's centrist drift. Extraction: systematically excluded from coalition discussions by left and center-left due to 'cordon sanitaire' norms; confined to oppositional role despite electoral growth. Constrained exit: can vote and campaign freely, but cannot access governmental leverage without breaking the cordon. Active enforcement: all major parties publicly pledge non-cooperation with Chega.
constraint_indexing:constraint_classification(portuguese_party_system_realignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL REFORM ADVOCATES (SCAFFOLD) — Academics, civil society actors, and some politicians advocating for proportional representation reform see the realignment as a temporary coordination failure solvable through institutional change. Low theater: their mechanisms (public debate, academic analysis, cross-party reform commissions) have genuine function. Sunset clause: if electoral thresholds are raised or mixed-member proportional systems adopted, the realignment constraint's suppression decreases substantially. Mobile exit: reform advocates can pursue change through multiple channels and shift focus if blocked in one domain.
constraint_indexing:constraint_classification(portuguese_party_system_realignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL CONSENSUS RITUAL (PITON) — The formal mechanisms of government formation (presidential consultations, published coalition negotiations, parliamentary votes of confidence) are substantially performative. These rituals serve important legitimacy functions but are increasingly divorced from the real coordination mechanism: backroom negotiations between PS and insurgent parties. Theater ratio rising as the gap between formal procedures (public consensus-building) and actual bargaining (private confidence-and-supply deals) widens. Piton classification reflects degradation: the ritual persists through institutional inertia despite lower functional verification compared to the pre-realignment two-party consensus model.
constraint_indexing:constraint_classification(portuguese_party_system_realignment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some electoral fragmentation and coalition complexity are inherent to proportional representation systems as they mature: voter preferences diversify, insurgent parties emerge, and governing requires negotiation. This perspective risks naturalizing as immutable what is actually contingent on institutional design (electoral thresholds, coalition formation rules, presidentialism vs. parliamentarism). The engine will flag this as a false summit: the 'inherent to democracy' framing obscures contingent choices about system design.
constraint_indexing:constraint_classification(portuguese_party_system_realignment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portuguese_party_system_realignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(portuguese_party_system_realignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portuguese_party_system_realignment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(portuguese_party_system_realignment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(portuguese_party_system_realignment, TR),
    TR >= 0.70.

:- end_tests(portuguese_party_system_realignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The established parties extract value through gatekeeping power (controlling coalition formation terms) and through the information asymmetry about confidence-and-supply arrangements. However, extraction is not maximal (0.70+) because insurgent parties do gain genuine parliamentary seats and leverage, and there are real coordination benefits (minority governments do form and function rather than repeatedly dissolving). The metric reflects that the mechanism is hybrid: some extraction is rent-seeking (PS obtaining policy concessions beyond what its vote share justifies), some is legitimate coordination cost. Suppression (0.48): Moderate. Barriers to preference expression include: (1) uncertainty about coalition formation rules (voters don't know how their vote translates to coalition composition), (2) cordon sanitaire norms that suppress Chega representation options, (3) media framing that amplifies fear of governmental instability, (4) strategic party signaling about unacceptability thresholds. But suppression is not total because all parties can campaign openly, voters have genuine information about platform differences, and insurgent parties have successfully gained parliamentary representation. Theater ratio (0.65): Moderately high. The formal apparatus of government formation (presidential consultations, published coalition negotiations, parliamentary confidence votes) performs important legitimacy functions but is increasingly performative: the real bargaining happens in private party-leadership meetings. The gap has widened over the interval (0.48 to 0.65) as confidence-and-supply became the primary mechanism and as Chega's exclusion through cordon sanitaire required visible public consensus-building to appear legitimate despite private gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the diagnostic power of indexed classification: the same institutional structure appears as rope (for beneficiaries), tangled rope (for mixed agents), snare (for trapped voters), scaffold (for reformers), and piton (for degraded rituals). The perspectival gaps reveal: (1) gatekeeping power asymmetry: established parties see problem-solving, insurgents see subordination; (2) preference-expression tradeoff: voters gained choice options but lost coalition predictability; (3) temporal mismatch: formal rituals (constitutional consultations) lag behind actual mechanisms (confidence-and-supply bargaining), creating theater. These gaps are not measurement errors — they are diagnostic signals that the constraint is hybrid (mixed coordination-extraction) and asymmetrically distributed across agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: established_parliamentary_parties (PS, PSD, CDS) genuinely coordinate through confidence-and-supply arrangements and retain gatekeeping power over coalition formation. They derive low directionality (d ≈ 0.12) because they are net beneficiaries with arbitrage exit options: if a confidence-and-supply arrangement breaks down, they can shift to alternative coalition partners or call new elections. Victim declarations: electoral_volatility_absorbers (voters navigating multi-party choice with coalition unpredictability) and voter_preference_expression (the abstract collective interest in authentic representation). Voters derive high directionality (d ≈ 0.83) because they are trapped — they cannot exit preference expression without withdrawing from the political system entirely. The engine computes chi = ε × f(d) × σ(S) with national scope (σ=1.0): for beneficiaries, chi ≈ 0.58 × 0.0 × 1.0 ≈ 0.0 (they experience coordination benefit); for victims, chi ≈ 0.58 × 1.30 × 1.0 ≈ 0.75 (they experience high effective extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification resolves mandatrophy by identifying both coordination function (confidence-and-supply arrangements enable minority governments) and asymmetric extraction (insurgent parties and voters experience constraint). The constraint requires_active_enforcement (true): all major parties publicly enforce the cordon sanitaire against Chega, demonstrating that the gatekeeping is not a natural consequence of proportional representation but an actively maintained arrangement. Without active enforcement, Chega would naturally enter coalition negotiations based on vote share. The coordination function (enabling government formation under electoral fragmentation) is genuine: without confidence-and-supply, Portugal would face repeated elections or governmental paralysis. But this coordination is layered with extraction: the established parties extract gate-keeping rent (policy concessions beyond their vote share) and voters extract suppression (information asymmetry about coalition outcomes). The tangled_rope classification correctly rejects the false summit (natural law about proportional systems) while honoring both the real coordination and the real extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cordon_sanitaire_stability,
    'Can the cordon sanitaire against Chega persist if electoral support exceeds 25-30% without destabilizing the constraint itself?',
    'Longitudinal tracking of Chega electoral performance and elite coalition responses; observation of whether non-cooperation pledge remains credible at higher vote shares; analysis of comparative cases (Austria FPÖ, Denmark DPP, Sweden SD)',
    'If cordon breaks: Chega moves to tangled_rope or rope classification, constraint shifts toward snare for centrist voters excluded from representation. If cordon holds: constraint remains stable but suppression increases (enforced exclusion becomes more visible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cordon_sanitaire_stability, empirical, 'Viability of cordon sanitaire against right-authoritarian parties at high vote shares').

omega_variable(
    confidence_and_supply_durability,
    'Is the confidence-and-supply mechanism a durable coordination equilibrium or a temporary arrangement constrained by periodic electoral volatility?',
    'Historical analysis of supply-confidence duration and collapse triggers; modeling of bargaining dynamics under different coalition compositions; comparison with other proportional systems using minority-government confidence mechanisms',
    'If durable equilibrium: PS-led minority government becomes stable governance mode (rope-dominated system). If temporary: each electoral cycle risks breakdown requiring new negotiations (snare for voters during crisis periods).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confidence_and_supply_durability, empirical, 'Whether confidence-and-supply is a stable or contingent coordination mechanism').

omega_variable(
    electoral_threshold_effectiveness,
    'Would raising the electoral threshold from 0.67% to 3-4% (as some reformers propose) reduce volatility or increase suppression by excluding smaller voices?',
    'Comparative analysis of threshold effects in similar proportional systems; simulation of Portuguese election results under alternative thresholds; qualitative assessment of representation losses vs. stability gains',
    'If threshold reduces volatility without suppression costs: becomes genuine scaffold solution with real sunset. If threshold increases suppression: becomes a snare mechanism for minority preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_threshold_effectiveness, preference, 'Trade-off between electoral stability and representation inclusivity via threshold reform').

omega_variable(
    identity_lock_partisan_affiliation,
    'To what extent are Portuguese voters identity-locked into established party affiliation (cultural identification with PS or PSD lineages) vs. constrained by coalition unpredictability?',
    'Survey analysis of party identification stability across age cohorts and educational levels; qualitative interviews with voters about exit costs (identity loss vs. external consequences); analysis of intergenerational party loyalty patterns',
    'If primarily identity-locked: realignment is slower, constraint more stable but perceptual (voters see themselves as having chosen their lock). If primarily constrained: realignment is faster, constraint more volatile as cost-benefit calculations shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_partisan_affiliation, empirical, 'Whether voters are identity-locked or externally constrained in party choices').

omega_variable(
    eu_institutional_amplification,
    'Does EU membership and ECB conditionality amplify suppression of electoral volatility through technocratic constraints on feasible coalition programs?',
    'Analysis of EU-mandated austerity constraints on government formation; comparison with Portuguese party platforms pre- and post-eurozone crisis; assessment of whether voters perceive coalition programs as genuinely different or constrained by EU requirements',
    'If EU constrains materially: effective extraction increases (voters cannot implement preference-divergent policies regardless of coalition). If EU constraint is modest: Portuguese institutional dynamics remain primary driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_institutional_amplification, empirical, 'EU technocratic constraints on electoral preference expression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portuguese_party_system_realignment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psr_tr_t0, portuguese_party_system_realignment, theater_ratio, 0, 0.48).
narrative_ontology:measurement(psr_tr_t5, portuguese_party_system_realignment, theater_ratio, 5, 0.58).
narrative_ontology:measurement(psr_tr_t10, portuguese_party_system_realignment, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(psr_be_t0, portuguese_party_system_realignment, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(psr_be_t5, portuguese_party_system_realignment, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(psr_be_t10, portuguese_party_system_realignment, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(portuguese_party_system_realignment, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(portuguese_party_system_realignment, 0.12).
narrative_ontology:affects_constraint(portuguese_party_system_realignment, electoral_volatility_contagion).
narrative_ontology:affects_constraint(portuguese_party_system_realignment, eu_technocratic_constraint_on_budgets).
narrative_ontology:affects_constraint(portuguese_party_system_realignment, cordon_sanitaire_sustainability).

% DUAL FORMULATION NOTE:
% Portuguese party realignment is downstream of EU austerity policies (2010-2015) and upstream of cordon sanitaire stability constraints. The realignment itself has different ε values depending on observable: if measured by formal coalition stability, ε ≈ 0.30 (rope-dominant); if measured by voter preference expression and coalition unpredictability, ε ≈ 0.58 (tangled_rope). The stories are linked because austerity legitimacy collapse drives voter fragmentation (upstream), and coalition gatekeeping dynamics determine whether Chega remains excluded or enters mainstream politics (downstream).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(portuguese_party_system_realignment, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
