% ============================================================================
% CONSTRAINT STORY: exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exogenous_override_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exogenous_override_reading
 *   human_readable: Exogenous Override Reading: Legitimacy of State-Decreed Practice Standardization
 *   domain: political_history/institutional_change/modernization
 *
 * SUMMARY:
 *   The exogenous override reading holds that state authority is legitimate
 *   in decreeing practice change for collective benefit — modernization,
 *   fiscal integration, international alignment. This reading emerges
 *   historically in the transition to national statehood: Ottoman calendar
 *   standardization (Gregorian adoption), French metric system imposition,
 *   Meiji dress code reforms, Soviet calendar changes. The reading justifies
 *   suppression of local practices through appeal to coordination benefits:
 *   unified fiscal years enable tax collection, standardized measures enable
 *   trade, calendrical alignment enables international treaty
 *   synchronization. From the state's perspective, the decree solves genuine
 *   collective-action problems at the national and international scale. From
 *   the rural practice community's perspective, the decree is pure
 *   extraction: prohibition without alternative that serves their local
 *   functions (lunar calendar's use in agricultural timing, traditional
 *   dress's kinship signaling, vernacular units' local-scale market
 *   efficiency). The constraint exhibits high theater ratio (0.55) because
 *   enforcement becomes increasingly performative: populations maintain
 *   underground practice, officials perform enforcement, both sides tacitly
 *   negotiate the boundary between public compliance and private continuance.
 *   The exogenous override reading is empirically distinct from its siblings:
 *   the endogenous displacement reading insists that practices persist
 *   because they solve local problems the decree ignores; the dual practice
 *   equilibrium reading treats urban/rural divergence as structurally stable
 *   rather than transitional.
 *
 * KEY AGENTS:
 *   - Central State Apparatus: Primary beneficiary (institutional/arbitrage) — captures coordination gains from standardization, international legitimacy, fiscal efficiency
 *   - Rural Practice Communities: Primary victim (powerless/trapped) — face legal prohibition of customary practices with enforcement mechanisms; bear suppression without compensation
 *   - Intermediate Administrative Apparatus: Secondary actor (moderate/constrained) — benefits from coordination gains but faces enforcement costs and legitimacy friction with constituents
 *   - Nationalist Intellectual Class: Secondary beneficiary (organized/constrained) — advocates for standardization, experiences coordination benefits and institutional power, but also architects of cultural suppression
 *   - Underground Practice Tradition: Persisting constraint actor (moderate/constrained) — maintains practices clandestinely, gradually fades (exogenous override assumption) or persists indefinitely (endogenous displacement assumption)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent to modernity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exogenous_override_reading, 0.58).
domain_priors:suppression_score(exogenous_override_reading, 0.68).
domain_priors:theater_ratio(exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exogenous_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exogenous_override_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(exogenous_override_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(exogenous_override_reading, "Exogenous Override Reading: Legitimacy of State-Decreed Practice Standardization").
narrative_ontology:topic_domain(exogenous_override_reading, "political_history/institutional_change/modernization").

domain_priors:requires_active_enforcement(exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exogenous_override_reading, 'b128d6c5-7235-432f-ac2b-821d32448207').
narrative_ontology:cs_created_at('b128d6c5-7235-432f-ac2b-821d32448207', '').
narrative_ontology:cs_kernel_codification('b128d6c5-7235-432f-ac2b-821d32448207', formalized).
narrative_ontology:cs_authority_grounding('b128d6c5-7235-432f-ac2b-821d32448207', extraction).
narrative_ontology:cs_interpretation_layer_present('b128d6c5-7235-432f-ac2b-821d32448207').
narrative_ontology:cs_kernel_id(exogenous_override_reading, legitimacy_of_practice_standardization).
narrative_ontology:cs_reading_relation('b128d6c5-7235-432f-ac2b-821d32448207', endogenous_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('b128d6c5-7235-432f-ac2b-821d32448207', dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('b128d6c5-7235-432f-ac2b-821d32448207', foundational, state_decree_legitimizes_standardization).
narrative_ontology:cs_axiom_status(state_decree_legitimizes_standardization, holdable).
narrative_ontology:cs_axiom('b128d6c5-7235-432f-ac2b-821d32448207', secondary, collective_benefit_prioritizes_national_scale).
narrative_ontology:cs_axiom_status(collective_benefit_prioritizes_national_scale, holdable).
narrative_ontology:cs_reference_frame('b128d6c5-7235-432f-ac2b-821d32448207', state_coordinating_authority_framework).
narrative_ontology:cs_drift_state('b128d6c5-7235-432f-ac2b-821d32448207', contemporary_multilevel_governance, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exogenous_override_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(exogenous_override_reading, fiscal_integration_systems).
narrative_ontology:constraint_beneficiary(exogenous_override_reading, international_coordination_regimes).
narrative_ontology:constraint_victim(exogenous_override_reading, local_practice_communities).
narrative_ontology:constraint_victim(exogenous_override_reading, endogenous_tradition_carriers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL PRACTICE COMMUNITY (SNARE) — Local populations face legal prohibition of customary practices (lunar calendar, traditional dress codes, vernacular weights) with enforcement mechanisms (fines, public shaming, administrative harassment). Exit options are strictly material: abandon the practice entirely (identity cost) or maintain it clandestinely (legal risk). The constraint extracts compliance through suppression without genuine coordination benefit to the target.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERMEDIATE APPARATUS (TANGLED ROPE) — Magistrates, local officials, and village administrators experience genuine coordination benefits (unified tax collection, standardized measures, national integration) alongside extraction (enforcing the decree requires their labor, creates legitimacy friction with constituents, and makes them enforcement targets). They have agency but face real costs to exiting the enforcement role.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL STATE AUTHORITY (ROPE) — The state apparatus benefits from unified calendar systems (synchronized fiscal year, coordinated military mobilization, international treaty alignment), unified measurement systems (tax assessment efficiency, trade logistics), and unified cultural signaling (modern nation-state identity). The decree solves genuine collective-action coordination problems at the national and international level.
constraint_indexing:constraint_classification(exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UNDERGROUND PRACTICE TRADITION (PITON) — Endogenous traditions (lunar calendar for agricultural timing, traditional dress for kinship signaling, vernacular units for local trade) persist clandestinely for decades despite legal prohibition. The constraint's formal function (eradicate customary practice) becomes increasingly theatrical — enforcement effort continues but effectiveness degrades as populations normalize dual practice. The underground tradition demonstrates that the suppression floor is partial, not total.
constraint_indexing:constraint_classification(exogenous_override_reading, piton,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NATIONALIST INTELLECTUAL CLASS (TANGLED ROPE) — Modernizers and nationalist intellectuals experience genuine coordination benefits from standardization (national identity formation, international legitimacy, technological transfer efficiency) but also face extraction: they must persuade, enforce, and ultimately use violence against populations they claim to modernize. The constraint gives them institutional power but also makes them architects of cultural suppression.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, state monopoly on measure-setting and calendar administration appears as an inherent feature of modern statehood — similar to monopoly on coinage or legal jurisdiction. This perspective risks naturalizing what is actually a contingent institutional arrangement: that state authority should standardize even practices that carry endogenous tradition value. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exogenous_override_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The exogenous override reading frames practice standardization as coordination-driven, but the structural asymmetry is severe: beneficiaries (state apparatus, international regimes) capture enormous coordination rents while targets (rural populations) lose endogenous practice value with no compensation. The reading claims legitimacy grounds in collective benefit, but benefits are concentrated in state apparatus and international alignment, not distributed to populations bearing suppression. Suppression (0.68): High. Multiple enforcement mechanisms: legal prohibition, public shaming, administrative harassment, fines. Populations cannot exit through negotiation or compensation — the only options are full compliance (identity cost) or clandestine practice (legal risk). Theater ratio (0.55): Moderate-high. Enforcement becomes increasingly theatrical over the interval: official measures show declining practice rates, but underground practice persists indefinitely in many historical cases. The constraint's stated function (eradication) becomes decoupled from actual outcome (coexistence). Theater rises over time as both enforcement and resistance normalize.
 *
 * PERSPECTIVAL GAP:
 *   The exogenous override reading produces sharp perspectival divergence because legitimacy grounding is contested. The state authority sees Rope (genuine coordination). The nationalist intellectuals see Tangled Rope (coordination benefits + extraction of cultural authority). The administrative apparatus sees Tangled Rope (coordination gains + enforcement costs). The rural populations see Snare (extraction without compensation). The underground practice tradition sees Piton (formal prohibition with persistent clandestine practice). The analytical observer sees Mountain (modernity requires standardization) — but this is a false summit candidate, since the constraint's legitimacy is power-based, not natural-law-based. The reading's core claim (exogenous decree is legitimate) is contested by the endogenous displacement reading (legitimacy lies with endogenous problem-solving) and the dual practice equilibrium reading (both emerge from different institutional contexts).
 *
 * DIRECTIONALITY LOGIC:
 *   The exogenous override reading's directionality is determined by agents' structural position relative to standardization. Beneficiaries (state apparatus, international regimes) experience low or negative effective extraction — they capture rents from the coordination mechanism. Victims (rural populations) face maximum extraction — trapped exit options + victim status + concentrated suppression. The administrative apparatus sits between: institutional power with constrained exit (enforcement role is sticky; exiting means losing state authority). The analytical observer at the civilizational scale risks perception of immutability — the reading naturalizes state decree as inherent to modernity — but the structural data reveals this as a false summit: the legitimacy claim grounds on power to enforce, not on natural law. Override to natural law is triggered by the reading's false naturalization of contingent institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The exogenous override reading resolves the mandatrophy by demonstrating that legitimacy claims in state practice standardization depend on which agent's coordination problem is foregrounded. If the state's coordination problem (unified fiscal years, international alignment) is primary: standardization via decree is legitimate (Rope/Tangled Rope). If the rural population's coordination problem (maintaining local practice functions) is primary: exogenous decree is extraction without coordination benefit (Snare). The reading mandatrophy is not resolved by empirical data but by normative framing: which collective's benefit counts as 'collective benefit'? The exogenous override reading answers: the national and international collective. The endogenous displacement reading answers: the local practice community. These are incommensurable at the framing level, which is why they produce different constraint types from the same structural data. The dual practice equilibrium reading declines to choose, treating both framings as simultaneously live, which produces yet different perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_benefit_genuineness,
    'Does standardization produce genuine coordination benefits measurable independent of enforcement authority, or does the benefit accrue primarily to the state apparatus itself?',
    'Empirical comparison of economic/administrative efficiency gains from standardization vs. implicit transfer of rents to the state. Cross-national analysis of outcomes in states that achieved standardization via negotiation vs. decree.',
    'If genuine coordination benefit is substantial and widely distributed: constraint is Rope from more perspectives. If benefit is primarily to state apparatus and international regimes: constraint remains Snare/Tangled Rope with cosmetic coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_genuineness, empirical, 'Whether standardization produces genuine or apparatus-captured coordination benefits').

omega_variable(
    reading_contention_empirical,
    'This constraint instantiates the exogenous override reading of legitimacy — that state authority can legitimately impose practice change for collective benefit. The endogenous displacement reading contests this, holding that practices persist because they solve local problems that the state decree overlooks. The dual practice equilibrium reading holds that both emerge from different population strata. Which reading better predicts the long-term dynamics of practice persistence vs. change?',
    'Historical tracking of practices subject to standardization decrees: measure persistence rates after 50+ years, distinguish urban/rural divergence, correlate persistence with measurable local-function value (e.g., lunar calendar persistence in agricultural regions), assess whether dual practice becomes institutionalized or remains transitional.',
    'If exogenous override reading holds: practices fade within 1-2 generations as enforcement internalizes. If endogenous displacement holds: practices persist indefinitely because they solve problems the decree ignores. If dual practice holds: coexistence becomes stable equilibrium, not transitional phase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contention_empirical, empirical, 'Long-term persistence of traditional practices under standardization decrees').

omega_variable(
    kernel_legitimacy_grounding,
    'What grounds the authority of the exogenous override reading''s core axiom — that state decree legitimizes practice change? Is it democratic mandate, expert authority, necessity for international alignment, or power to enforce?',
    'Institutional analysis of how the state justified the decree at the time: was it framed as democratically authorized, expert-driven, internationally necessary, or simply as state prerogative? Track whether justification frameworks changed over time and whether population adoption of new practices tracks the justification narrative or precedes/succeeds it.',
    'If legitimacy grounds on democratic mandate or expert authority: the reading is holdable across political systems. If it grounds solely on power to enforce: the reading is structurally identical to the endogenous displacement reading but with opposite normative valence — a reveal that the contest is about how to frame state coercion, not about legitimacy grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_legitimacy_grounding, conceptual, 'What grounds the legitimacy claim in state-decreed practice change').

omega_variable(
    false_summit_candidate,
    'Does the exogenous override reading naturalize state standardization as inherent to modernity, when the same standardization achieved through endogenous deliberation and adoption would have different structural properties (higher effective coordination, lower extraction)?',
    'Comparative institutional analysis: measure extractiveness and suppression levels in standardization achieved via decree vs. achieved via negotiation/incentive within otherwise similar populations. If decree-based standardization shows significantly higher suppression and comparable or lower effective coordination, the mountain framing is a false summit.',
    'If false summit confirmed: the exogenous override reading''s authority grounding is revealed as power-based rather than legitimacy-based — the constraint is Snare/Tangled Rope with a natural-law cover story. If mountain holds: standardization via decree is genuinely structurally necessary for rapid institutional integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate, empirical, 'Whether exogenous override reading falsely naturalizes state decree as inherent to modernity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exogenous_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exog_override_tr_t0, exogenous_override_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(exog_override_tr_t10, exogenous_override_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(exog_override_tr_t20, exogenous_override_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(exog_override_tr_t30, exogenous_override_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(exog_override_be_t0, exogenous_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(exog_override_be_t10, exogenous_override_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(exog_override_be_t20, exogenous_override_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(exog_override_be_t30, exogenous_override_reading, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exogenous_override_reading, endogenous_displacement_reading).
narrative_ontology:affects_constraint(exogenous_override_reading, dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The exogenous override reading is one of three readings of the legitimacy_of_practice_standardization kernel. All three readings operate on the same factual base (states do decree practice standardization; populations do resist; practices do persist) but assign different legitimacy sources. The three readings have different ε values and different beneficiary/victim structures. Each story is a complete, ε-invariant constraint — not an observable-dependent view of one constraint, but three structurally distinct constraints grounded in different normative framings of the same kernel. They are linked by network dependencies: exogenous override influences endogenous displacement (if exogenous authority is legitimate, endogenous alternatives are delegitimized); endogenous displacement coexists with dual practice equilibrium (both recognize plurality but assign it different structural meaning).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exogenous_override_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
