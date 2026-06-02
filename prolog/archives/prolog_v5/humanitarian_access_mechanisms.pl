% ============================================================================
% CONSTRAINT STORY: humanitarian_access_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humanitarian_access_mechanisms, []).

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
 *   constraint_id: humanitarian_access_mechanisms
 *   human_readable: Humanitarian Access Mechanisms in Conflict Zones
 *   domain: international_humanitarian_law/conflict_operations
 *
 * SUMMARY:
 *   Humanitarian access mechanisms in conflict zones present a fundamental
 *   structural tension: providing aid requires negotiating with actors
 *   (states and armed groups) who have incentives to weaponize access
 *   conditionality for political extraction, while humanitarian organizations
 *   depend on maintained access to serve beneficiary populations. The
 *   constraint exhibits the full spectrum of DR types, making it a diagnostic
 *   exemplar for how the same institutional arrangement can simultaneously
 *   coordinate and extract. From the civilian population's perspective,
 *   access mechanisms are snares — they are trapped and cannot exit the zone,
 *   while aid conditionality enforces political compliance. From humanitarian
 *   organizations' perspective, the mechanisms are tangled ropes — genuine
 *   coordination of aid distribution mixed with enforced neutrality
 *   violations and surveillance. From donor states' perspective, they are
 *   ropes — humanitarian operations coordinate international burden-sharing
 *   and geopolitical stabilization while maintaining plausible deniability.
 *   From host states and armed groups' perspective, they are snares in the
 *   sense of extracting political concessions, but tangled ropes in the sense
 *   of coordinating service delivery under their de facto authority. The
 *   humanitarian neutrality principle itself has become a piton — maintained
 *   through institutional inertia despite systematic politicization in
 *   practice. The extractiveness has risen over the interval (0.42 → 0.58) as
 *   conflict actors have learned to weaponize access more effectively, while
 *   theater_ratio has increased (0.55 → 0.68) as organizations have
 *   intensified the neutrality narrative to obscure politicization. The
 *   analytical observer classifies the constraint as tangled_rope because
 *   both coordination and extraction are structurally necessary: aid must be
 *   coordinated across hostile jurisdictions, AND political actors extract
 *   concessions as the price of access. No actor can escape the hybrid.
 *
 * KEY AGENTS:
 *   - Affected Civilian Populations: Primary victim (powerless/trapped) — geographically confined, information-controlled, aid access weaponized for political compliance
 *   - Humanitarian Organizations: Secondary actor (organized/constrained) — depend on access permission, experience genuine coordination and asymmetric extraction simultaneously
 *   - Host State Authority: Extractor beneficiary (powerful/arbitrage) — controls access, extracts political legitimacy and intelligence
 *   - Armed Opposition Groups: Mixed actor (organized/constrained) — provide de facto services while extracting resources and recognition from access negotiations
 *   - Donor States and International Community: Beneficiary (institutional/arbitrage) — benefit from geopolitical stabilization and burden-sharing while maintaining plausible deniability
 *   - The Humanitarian Neutrality Principle: Institutional narrative (piton) — maintains performative impartiality narrative despite systematic violation in practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humanitarian_access_mechanisms, 0.58).
domain_priors:suppression_score(humanitarian_access_mechanisms, 0.72).
domain_priors:theater_ratio(humanitarian_access_mechanisms, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humanitarian_access_mechanisms, extractiveness, 0.58).
narrative_ontology:constraint_metric(humanitarian_access_mechanisms, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(humanitarian_access_mechanisms, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humanitarian_access_mechanisms, tangled_rope).
narrative_ontology:human_readable(humanitarian_access_mechanisms, "Humanitarian Access Mechanisms in Conflict Zones").
narrative_ontology:topic_domain(humanitarian_access_mechanisms, "international_humanitarian_law/conflict_operations").

domain_priors:requires_active_enforcement(humanitarian_access_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humanitarian_access_mechanisms, host_state_sovereignty).
narrative_ontology:constraint_beneficiary(humanitarian_access_mechanisms, armed_groups).
narrative_ontology:constraint_beneficiary(humanitarian_access_mechanisms, humanitarian_organizations).
narrative_ontology:constraint_victim(humanitarian_access_mechanisms, affected_civilian_populations).
narrative_ontology:constraint_victim(humanitarian_access_mechanisms, independent_verification_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped in conflict zones with no exit capacity. Humanitarian access is controlled by state actors and armed groups who extract concessions (intelligence, political alignment, territorial recognition) in exchange for aid delivery. Civilians bear full suppression: geographic confinement, information control, weaponization of aid access. Minimal coordination benefit — aid is conditional on political compliance. High experienced extraction.
constraint_indexing:constraint_classification(humanitarian_access_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HUMANITARIAN ORGANIZATIONS (TANGLED ROPE) — Constrained by dependence on state/armed group permission and by reputational risk of perceived partiality. Experience genuine coordination (negotiating access, distributing aid efficiently) alongside asymmetric extraction (forced consent to political conditions, neutrality violations, staff placement surveillance). Suppression operates through access denial threat and operational restrictions. Can exit at cost (abandoning beneficiaries, losing operational presence), but not without significant harm to mission and organization.
constraint_indexing:constraint_classification(humanitarian_access_mechanisms, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DONOR STATES AND INTERNATIONAL COMMUNITY (ROPE) — See humanitarian access as a coordination mechanism for burden-sharing and legitimacy. Benefit from humanitarian operations that reduce refugee flows, stabilize regions, and provide post-conflict reconstruction opportunities. Can arbitrage between multiple conflict theaters and redirect funding if access deteriorates. Low experienced extraction — benefits flow toward this actor through geopolitical stability gains and soft power.
constraint_indexing:constraint_classification(humanitarian_access_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HOST STATE AUTHORITY (SNARE) — Extracts political concessions and territorial legitimacy through access control. Humanitarian organizations must implicitly recognize state authority, provide intelligence on populations, and avoid supporting opposition groups. High suppression maintained through regulatory authority and denial capacity. From the state's perspective, this is rational political extraction — humanitarian access is weaponized to consolidate control. Low theater ratio for the state itself (straightforward political conditionality), but the extraction mechanism is coercive and asymmetric.
constraint_indexing:constraint_classification(humanitarian_access_mechanisms, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ARMED OPPOSITION GROUPS (TANGLED ROPE) — Seek legitimacy and resources through controlling humanitarian access in areas under their de facto authority. Genuine coordination function (distributing aid, establishing civilian services) mixed with extraction (taking supplies, recruiting logistics support, demanding political recognition). Constrained by donor pressure and international monitoring. Both beneficiary and victim of the humanitarian access system — extract from it while being constrained by external pressure on humanitarian organizations to maintain impartiality.
constraint_indexing:constraint_classification(humanitarian_access_mechanisms, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: HUMANITARIAN NEUTRALITY PRINCIPLE (PITON) — The doctrine of humanitarian neutrality (impartiality, independence from political agendas) is largely theatrical. Organizations declare neutrality while implicitly legitimizing state authority through operational recognition, provide intelligence that states use against civilians, and face intense pressure to align with donor state interests. The neutrality ritual persists through institutional momentum (UN mandates, Red Cross doctrine) despite systematic violation in practice. Theater ratio is high because the actual function (aid delivery conditioned on political compliance) is obscured by the neutrality narrative.
constraint_indexing:constraint_classification(humanitarian_access_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global/civilizational view, humanitarian access mechanisms coordinate response to mass suffering while simultaneously enabling political actors to extract legitimacy, intelligence, and control through aid conditionality. The constraint exhibits genuine coordination (connecting donors to beneficiaries, distributing resources) alongside structural extraction (weaponization of access, intelligence extraction, coercive legitimacy). The perspectival analysis reveals that 'humanitarian' framing obscures the political extraction that accompanies aid delivery. No agent escapes the hybrid: donors, states, organizations, and opposition all coordinate and extract simultaneously.
constraint_indexing:constraint_classification(humanitarian_access_mechanisms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humanitarian_access_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(humanitarian_access_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(humanitarian_access_mechanisms, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(humanitarian_access_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(humanitarian_access_mechanisms, TR),
    TR >= 0.70.

:- end_tests(humanitarian_access_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Political actors extract legitimacy, intelligence, territorial recognition, and compliance concessions through access control. However, the constraint is not pure extraction (which would require ε ≥ 0.70) because genuine coordination functions exist — aid actually does flow to populations, organizations do genuinely distribute resources efficiently within constraints, and donors do coordinate burden-sharing. The extraction is embedded within coordination rather than replacing it. Suppression (0.72): High. Multiple suppression mechanisms operate simultaneously: geographic confinement of civilians, information control, weaponization of access (denial as coercion), regulatory authority of host states, and safety risks for humanitarian staff. Suppression is not total (some access always exists, some populations can access alternative resources), but it is severe and multivalent. Theater ratio (0.68): Moderate-high. The humanitarian neutrality narrative is substantially theatrical — organizations declare impartiality while implicitly recognizing state authority, provide intelligence used against civilians, and adjust operations based on donor pressure. However, the theater is not complete (some genuine impartiality exists, some organizations do refuse compromising conditions), and the actual coordination function (aid distribution) is real.
 *
 * PERSPECTIVAL GAP:
 *   The gap between snare (civilian) and rope (donor) is maximal — a full 180° rotation. This reveals the complete asymmetry in how the mechanism functions: donors experience it as a coordination benefit, benefiting from stability and reduced refugee flows; civilians experience it as an extraction mechanism, bearing costs of aid conditionality and access denial. The humanitarian organization's tangled_rope position is the bridge — it both coordinates (genuine aid distribution) and extracts (compliance with politicization). The analytical observer at the civilizational level must see the tangled_rope structure because the mechanism itself is designed to coordinate international response to mass suffering, which requires both genuine aid flow AND enforcement of state/group authority to maintain access. The piton perspective (the neutrality doctrine itself) reveals that organizations cope with the perspectival gap by maintaining a narrative of impartiality that masks their complicity in political extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary dramatically across agents. Civilian populations are full targets (d → 1.0): trapped, no exit options, victims of extraction. Humanitarian organizations are partial targets (d → 0.55-0.65): constrained exit (can abandon populations but at mission cost), beneficiaries of legitimacy and resources but also victims of compliance pressure. Host states are beneficiaries (d → 0.15-0.25): control access, extract concessions, experience low or negative extraction because the mechanism subsidizes their political goals. Donor states are beneficiaries (d → 0.10-0.20): arbitrage exit (can redirect to different conflicts), geopolitical benefits, low extraction. The engine derives d from power × exit × beneficiary/victim declarations. Civilians with trapped exit and victim status produce high d → high f(d) → high χ. Host states with institutional power, arbitrage access (they can always deny access), and beneficiary status produce low d → negative f(d) → low/negative χ. Organizations with organized power, constrained exit, and mixed beneficiary/victim status produce mid-range d → moderate f(d) → moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by demonstrating that the six types are not competing answers but perspectival positions within a structurally unified hybrid system. The question 'Is this a Rope or a Snare?' is malformed — it is both, simultaneously, from different structural positions. The civilian sees snare (pure extraction). The organization sees tangled_rope (mixed). The state sees its own snare (but from the extractor side, not the victim side). The donor sees rope (coordination without extraction). The piton perspective reveals that the neutrality doctrine masks rather than enables the coordination/extraction hybrid. The mandatrophy is resolved when the analyst recognizes that the classification multiplicity is not measurement error but structural truth — humanitarian access mechanisms are designed to coordinate response while enabling political actors to extract concessions, and no agent can exit this hybrid because aid genuinely is needed AND political conditions are genuinely enforced. The tension is not resolvable by better measurement; it is intrinsic to the mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditionality_threshold,
    'At what level of access conditionality does coordination become pure extraction? Where is the boundary between legitimate state security concerns and illegitimate political leverage?',
    'Comparative analysis of access agreements across conflicts; measurement of aid diversion rates vs political compliance correlations; donor pressure effectiveness on conditionality reduction',
    'If threshold is low (minimal state conditions acceptable): current mechanisms are extractive snares. If threshold is high (significant political conditions tolerable): mechanisms are legitimate tangled ropes. Current uncertainty drives classification gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conditionality_threshold, preference, 'Boundary between legitimate conditionality and extractive leverage').

omega_variable(
    neutrality_fiction_persistence,
    'Is humanitarian neutrality maintained as a fiction to enable continued access, or do organizations genuinely believe in impartiality despite systematic evidence of politicization?',
    'Internal communications analysis; comparison of stated vs. reported operational decisions; interviews with humanitarian leadership on access-vs-principle trade-offs; measurement of policy changes following donor pressure vs. operational evidence',
    'If fiction: humanitarian system is deliberately complicit in political extraction, theater_ratio should be higher. If genuine belief: organizations are identity-locked into a doctrine that prevents recognizing their own capture, requiring identity-lock mechanisms in analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_fiction_persistence, empirical, 'Whether humanitarian neutrality is maintained as operational fiction or genuine belief').

omega_variable(
    civilian_survival_alternative_pathways,
    'Do humanitarian access mechanisms represent the only viable pathway for civilian survival in conflict zones, or are alternative survival strategies (local reciprocity, black markets, kinship networks) sufficient substitutes?',
    'Economic anthropology of conflict-zone survival; measurement of dependency on formal humanitarian aid vs. informal economy; longitudinal tracking of civilian populations during access disruptions',
    'If mechanisms are only viable pathway: civilians are trapped by necessity, not suppression, and snare classification is correct. If alternatives exist: suppression is lower than measured, suggesting civilian mobility is higher than currently modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_survival_alternative_pathways, empirical, 'Availability of alternative survival pathways in conflict zones').

omega_variable(
    organized_beneficiary_coalition_threshold,
    'Do humanitarian organizations collectively constitute an organized coalition capable of coordinated leverage against state/group conditioning, or are they sufficiently fragmented and dependent that collective action is infeasible?',
    'Network analysis of coordination among humanitarian organizations; measurement of joint action on access principles; comparison of leverage when organizations act individually vs. collectively; analysis of fragmentation incentives (competition for donor funding, specialization by geography/sector)',
    'If coalition capacity exists but unused: exit_options should be upgraded to mobile for organizations, lowering experienced extraction. If fragmentation is structural: constrained classification and high extraction are accurate, and the system depends on coordination failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_beneficiary_coalition_threshold, empirical, 'Capacity for humanitarian organizations to form coordinated leverage coalition').

omega_variable(
    donor_state_interest_alignment,
    'Do donor states genuinely align with humanitarian principles, or do they instrumentalize humanitarian access for geopolitical advantage while maintaining plausible deniability through organizational intermediaries?',
    'Correlation analysis between humanitarian access approvals and donor state strategic interests; measurement of conditionality divergence when humanitarian goals conflict with state interests; leaked communications and documentary evidence of donor pressure; analysis of funding patterns favoring access in strategically important regions',
    'If aligned: donor state perspective as rope is accurate, and the system achieves coordination. If instrumental: donor states are co-extractors, and humanitarian organizations are strategic proxies, elevating tangled_rope to snare at the analytical level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(donor_state_interest_alignment, empirical, 'Alignment between donor state interests and humanitarian principles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humanitarian_access_mechanisms, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(humac_tr_t0, humanitarian_access_mechanisms, theater_ratio, 0, 0.55).
narrative_ontology:measurement(humac_tr_t5, humanitarian_access_mechanisms, theater_ratio, 5, 0.62).
narrative_ontology:measurement(humac_tr_t10, humanitarian_access_mechanisms, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(humac_be_t0, humanitarian_access_mechanisms, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(humac_be_t5, humanitarian_access_mechanisms, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(humac_be_t10, humanitarian_access_mechanisms, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humanitarian_access_mechanisms, resource_allocation).
narrative_ontology:affects_constraint(humanitarian_access_mechanisms, international_humanitarian_law_compliance).
narrative_ontology:affects_constraint(humanitarian_access_mechanisms, refugee_movement_control).
narrative_ontology:affects_constraint(humanitarian_access_mechanisms, armed_group_legitimacy_through_service_provision).

% DUAL FORMULATION NOTE:
% Humanitarian access mechanisms decompose into three linked constraints: (1) the basic access negotiation (this story, ε=0.58), (2) compliance with IHL norms that govern access conditions (ε=0.35, rope), and (3) the armed group service provision function that mimics state legitimacy through humanitarian coordination (ε=0.48, tangled_rope). Each story has different base properties but shares common beneficiaries/victims. The access mechanism story is the upstream constraint that enables extraction in the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humanitarian_access_mechanisms, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
