% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: Exogenous Practice Standardization via State Decree (Legitimacy Reading)
 *   domain: political_history/institutional_change/modernization
 *
 * SUMMARY:
 *   This constraint instantiates the exogenous override reading of the
 *   legitimacy_of_practice_standardization kernel. The reading asserts that
 *   state authority has legitimate grounds to decree practice standardization
 *   (calendar, dress, weights/measures) when justified by collective benefit
 *   (modernization, fiscal stability, international alignment). The
 *   structural delta shows: calendar/dress change appears as abrupt legal
 *   imposition with enforcement mechanisms; surface compliance masks
 *   persistent underground practice; 'double life' (public conformity,
 *   private maintenance of lunar calendar) becomes a stable equilibrium
 *   rather than a transitional phase; rural populations maintain lunar
 *   calendar for decades despite official decree. This reading contrasts with
 *   two siblings: the endogenous_displacement_reading (legitimacy requires
 *   voluntary adoption driven by perceived utility), and the
 *   dual_practice_equilibrium_reading (legitimacy is domain-partitioned
 *   between state authority over public/administrative domains and
 *   traditional authority over private/ritual domains). The exogenous
 *   override reading stakes a normative claim: the state's unilateral
 *   authority to impose standardization is legitimate when justified by
 *   collective benefit, and the persistence of underground practice
 *   represents either incomplete transition or illegitimate resistance, not a
 *   valid alternative equilibrium. The measurement trajectory shows
 *   extractiveness and suppression declining over 20 years as enforcement
 *   machinery weakens and the practice change becomes internalized by younger
 *   generations, while theater ratio rises as enforcement becomes
 *   increasingly performative. This is the decay pattern of a tangled rope
 *   approaching piton status.
 *
 * KEY AGENTS:
 *   - State Administrative Apparatus (institutional/arbitrage) — primary beneficiary; sees standardization as solution to coordination problems in fiscal accounting, military timing, bureaucratic record-keeping
 *   - International Alignment Coalition (institutional/arbitrage) — secondary beneficiary; benefits from synchronized practices in trade, diplomacy, treaty compliance
 *   - Traditional Practice Communities (powerless/trapped) — primary victim; faces legal prohibition with enforcement; no exit capacity
 *   - Underground Cultural Maintenance Network (organized/constrained) — secondary victim; organizes resistance and preservation; experiences both coordination (maintaining community bonds) and extraction (surveillance risk, hidden costs)
 *   - Intermediate Bureaucrat / Local Enforcer (moderate/constrained) — mixed position; must coordinate implementation while navigating community resistance; experiences tangled rope structure
 *   - Analytical Observer (analytical/analytical) — sees false summit tendency in the civilizational 'natural law' framing of modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "Exogenous Practice Standardization via State Decree (Legitimacy Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/institutional_change/modernization").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '2b20c8ee-8117-4cc7-b0cd-41f1fb462310').
narrative_ontology:cs_kernel_codification('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', formalized).
narrative_ontology:cs_authority_grounding('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', extraction).
narrative_ontology:cs_reading_relation('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', foundational, state_unilateral_authority_legitimate).
narrative_ontology:cs_axiom_status(state_unilateral_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', state_unilateral_authority_legitimate, instrumental).
narrative_ontology:cs_axiom('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', foundational, underground_practice_illegitimate_resistance).
narrative_ontology:cs_axiom_status(underground_practice_illegitimate_resistance, holdable).
narrative_ontology:cs_axiom_grounding('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', underground_practice_illegitimate_resistance, instrumental).
narrative_ontology:cs_created_at('2b20c8ee-8117-4cc7-b0cd-41f1fb462310', '2026-02-26T14:23:47Z').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_coalition).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, fiscal_integration_beneficiaries).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_practice_communities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, endogenous_cultural_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The rural population maintaining lunar calendar or traditional dress faces legal prohibition with enforcement (fines, confiscation, public humiliation). Exit is not available — they cannot leave the jurisdiction or openly practice without penalty. The constraint extracts their conformity through threat while the coordination benefit (fiscal standardization, calendar alignment) flows to the state apparatus, not to them. Maximum experienced extraction.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERMEDIATE BUREAUCRAT / LOCAL ENFORCER (TANGLED ROPE) — Moderate power with constrained exit options. Must coordinate implementation of the new standard while managing community compliance. Experiences both genuine coordination (the administrative problem is real — unsynchronized calendars do create bureaucratic friction) and asymmetric extraction (the bureaucrat extracts authority and legitimacy from enforcement). Neither pure coordination nor pure extraction.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATIVE APPARATUS (ROPE) — The state sees practice standardization as a coordination mechanism: unified calendar, uniform dress, synchronized administrative time. From the state's perspective, this solves genuine collective action problems (fiscal accounting, military coordination, international treaty compliance). The state has arbitrage — it can exit by abandoning the standardization project without existential threat. The apparatus experiences the constraint as beneficial coordination, not extraction.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL ALIGNMENT COALITION (ROPE) — Trading partners, treaty allies, and international bodies benefit from standardized practices. The Gregorian calendar, metric system, Western dress codes facilitate commerce and diplomatic coordination. These actors see the constraint as solution to coordination problems across borders. They have arbitrage — they benefit but could function without the specific standardized practice if necessary. From this perspective, extraction is minimal and coordination is genuine.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: VESTIGES OF ENFORCEMENT INFRASTRUCTURE (PITON) — Over decades, enforcement weakens as younger generations adopt the new practice endogenously, compliance becomes habitual, and the justifying crisis (fiscal crisis, international pressure) recedes. The enforcement machinery persists (laws remain on books, but penalties fade) not because it solves a real coordination problem anymore, but through institutional inertia. Theater ratio rises as enforcement becomes performative rather than functional. The constraint degrades toward pure ritual.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: UNDERGROUND CULTURAL MAINTENANCE NETWORK (TANGLED ROPE) — Organized resistance (secret societies, religious leadership, family networks preserving lunar calendars) coordinates the preservation of traditional practice. This organization derives benefit from maintaining cultural autonomy (genuine coordination for preservation) while also experiencing extraction (risk of discovery, hidden costs of dual practice). The network's exit is constrained — abandoning the practice would dissolve the network's coherence. They experience both coordination (maintaining community bonds through shared secret practice) and extraction (bearing hidden costs, surveillance risk).
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CIVILIZATIONAL LAW VIEW (MOUNTAIN) — From a civilizational/universal timescale, the transition from local/distributed practices to standardized practices is treated as an inherent property of modernization itself — inevitable, natural, required by scale. The state's legitimacy to impose standardization is naturalized as a law of political development. However, this reading is a false summit: the structural data shows the constraint is a constructed institutional choice (state authority, enforcement mechanisms, asymmetric beneficiaries) not an immutable law. The mountain classification reveals the naturalizing framing that legitimates the exogenous override reading.
constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_of_practice_standardization__exogenous_override_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The exogenous override reading centers on state decree as the legitimation mechanism, which implies enforcement and extraction from non-complying communities. The state extracts conformity (and the coordination benefits that flow from standardization) from traditional communities through legal coercion, not voluntary adoption. However, extractiveness is not maximal (0.72+) because the coordination benefit is genuine — synchronized calendars do reduce administrative friction — even if the cost is borne asymmetrically by traditional communities. The measurement trajectory shows extractiveness declining over 20 years as enforcement weakens and younger generations adopt the practice endogenously, suggesting the constraint evolves toward either stable piton (performative enforcement) or rope (voluntary coordination) as time passes. Suppression (0.72): High. Initial suppression is very high (0.85) because the state uses legal prohibition, fines, confiscation, and public enforcement to prevent traditional practice. Suppression declines over time (0.72 by year 20) as alternative practices become internalized and enforcement capacity degrades. Theater ratio (0.68): Moderate-high and rising. The initial phase (t=0) shows relatively low theater (0.35) because enforcement is real and punitive. But as the constraint matures, enforcement becomes increasingly symbolic — laws remain on books but penalties fade, compliance becomes habitual, and the original justifying crisis (fiscal crisis, international pressure) recedes. By year 20, theater ratio has risen to 0.68, indicating the constraint is approaching piton status where the performance persists but the functional mechanism has atrophied. Claimed type (Tangled Rope): The constraint exhibits genuine coordination (solving real administrative problems) alongside asymmetric extraction (costs borne by traditional communities, benefits concentrated on state and international actors). The exogenous override reading justifies extraction by appeal to collective benefit, but the benefit is unequally distributed.
 *
 * PERSPECTIVAL GAP:
 *   The exogenous override reading produces perspectival gaps between all six positions. The state apparatus sees rope (coordination). The traditional community sees snare (extraction with no coordination benefit for them). The organized underground network sees tangled rope (coordination of cultural maintenance, extraction from surveillance risk). The intermediate bureaucrat sees tangled rope (must coordinate implementation while managing resistance). The international coalition sees rope (coordination facilitates trade). The civilizational observer who naturalizes modernization risks seeing mountain (inherent law of development), but the structural data reveals this as false summit. The gap between state perspective (rope) and traditional community perspective (snare) is the core diagnostic feature of this reading — it reveals whether the constraint's justification (collective benefit) is empirically sustainable or ideological cover for extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural relationship to the extraction flow. State apparatus (institutional/arbitrage) sees the constraint as solving their coordination problem with minimal cost to themselves — low d, low chi. International coalition (institutional/arbitrage) similarly has arbitrage options and sees coordination benefit — low d, low chi. Traditional community (powerless/trapped) faces legal coercion with no exit and no benefit — high d, high chi. Intermediate bureaucrat (moderate/constrained) occupies a mixed position: can refuse enforcement (constrained, not trapped) but faces career consequences; experiences both coordination work (implementing the standard) and extraction (authority over compliance decisions) — moderate d. Underground network (organized/constrained) has greater power through organization but constrained exit; coordinates preservation while experiencing extraction from surveillance — moderate d slightly elevated. The perspectival gap between beneficiaries (low d) and victims (high d) is maximal in this reading, reflecting the exogenous override claim: the state's legitimate authority derives from its position outside the affected community, capable of unilaterally imposing the standard for 'collective benefit.' This is precisely the structure that omega #2 (collective benefit constituency) challenges.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_efficacy,
    'Does practice change driven by state decree achieve faster/more stable adoption than endogenous cultural drift, or does exogenous imposition generate persistent underground practice that weakens real coordination?',
    'Longitudinal compliance data: lunar calendar usage in rural populations 10/20/30 years post-mandate; correlation between enforcement intensity and surface compliance vs actual administrative use; comparison of voluntary vs decreed adoption timelines across different societies',
    'If exogenous faster/more stable: exogenous override reading''s core claim (legitimacy through state efficacy) is supported. If endogenous is more stable: exogenous reading is imposing costs without delivering coordination benefits — shifts classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_efficacy, empirical, 'Whether state decree achieves more effective practice change than endogenous adoption').

omega_variable(
    collective_benefit_constituency,
    'Who exactly is the ''collective'' that benefits from the standardized practice? Whose coordination problem is being solved?',
    'Distributional analysis of benefits: fiscal efficiency gains accrue to state apparatus; international trade gains accrue to merchant classes and state treasury; administrative burden reduction accrues to bureaucracy. Costs borne by traditional communities. Measure: percentage of population experiencing net benefit from standardization within specified time window.',
    'If benefits concentrated on state + international + merchant classes: the constraint is asymmetric extraction masked as collective benefit. If benefits distributed broadly: exogenous reading''s legitimacy claim is stronger. Likely finding: benefits are concentrated, suggesting false-summit tendency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_constituency, empirical, 'Distribution of costs and benefits from practice standardization').

omega_variable(
    dual_practice_stability,
    'Is the persistent underground practice of lunar calendar, traditional dress, etc. a transitional phase being gradually displaced, or a stable long-term equilibrium?',
    'Multi-generational ethnographic data: Do children of calendar-practitioners adopt the official calendar, or do they maintain dual calendars? What is the intergenerational transmission rate? Are there regions where dual practice has persisted for 50+ years without convergence to official standard?',
    'If transitional (converges to official within 2-3 generations): exogenous reading''s efficiency narrative is partly supported. If stable equilibrium: dual practice is a permanent structural feature, suggesting the exogenous override reading mischaracterizes the constraint as ''change'' when it is actually ''stratification into public/private domains.'' This omega directly challenges the exogenous override reading''s core claim that legitimate practice change has occurred.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_practice_stability, empirical, 'Long-term stability of dual-practice equilibrium vs transitional displacement').

omega_variable(
    state_legitimacy_grounding,
    'On what basis does the exogenous override reading justify the state''s authority to decree practice standardization? Is the justification procedural (state has authority by constitutional/institutional right), instrumental (achieves collective benefit), or natural-law (required by modernization)?',
    'Textual/rhetorical analysis of the state''s justification for the decree. Examination of whether resistance movements dispute the state''s procedural authority to decree (legitimacy challenge) or dispute the benefit claim (efficacy challenge) or both.',
    'If procedural: exogenous reading rests on prior institutional legitimacy commitment. If instrumental: exogenous reading rests on empirical claim about collective benefit (resolvable by omega #1 and #2). If natural-law: exogenous reading naturalizes a contingent choice — reclassify as false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legitimacy_grounding, conceptual, 'Grounding of state authority claim in exogenous override reading').

omega_variable(
    reading_contest_localization,
    'What specific structural features distinguish the exogenous override reading from its siblings (endogenous displacement, dual practice equilibrium)?',
    'Reconstruction of the kernel contest: exogenous reading asserts state authority as legitimate basis; endogenous reading asserts voluntary adoption as sole legitimate basis; dual practice reading asserts domain partition (public/administrative vs private/ritual) as legitimate framework. Which reading does the historical record support?',
    'This omega documents that the three readings are genuinely distinct constraint structures, not observer-relative perspectives on the same structure. Each instantiates a different ε value and different classification pattern. Resolving this omega means accepting that only one reading correctly describes the actual structural constraint — the others are viable normative positions but not empirically accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_localization, conceptual, 'Structural localization of the exogenous override reading within the kernel contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_pract_std_theater_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(legit_pract_std_theater_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(legit_pract_std_theater_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(legit_pract_std_extr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(legit_pract_std_extr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(legit_pract_std_extr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legit_pract_std_supp_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(legit_pract_std_supp_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(legit_pract_std_supp_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, resource_allocation).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-member constraint family representing three distinct readings of the legitimacy_of_practice_standardization kernel. Each reading instantiates a different structural claim with different ε values, different beneficiary/victim structures, and different classification patterns. They are not observer-relative perspectives on a single constraint — they are genuinely distinct constraint structures, each claiming to describe the real structural mechanism. All three must be included in the corpus for the kernel contest to be properly modeled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__exogenous_override_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
