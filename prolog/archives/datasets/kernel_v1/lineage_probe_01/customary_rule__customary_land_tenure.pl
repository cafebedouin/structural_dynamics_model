% ============================================================================
% CONSTRAINT STORY: customary_rule__customary_land_tenure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_customary_rule__customary_land_tenure, []).

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
 *   constraint_id: customary_rule__customary_land_tenure
 *   human_readable: Customary Land Tenure: Community Membership and Use Rights
 *   domain: political/comparative/property_law
 *
 * SUMMARY:
 *   Customary land tenure is one reading of a contested kernel about the form
 *   of customary rule in African and Asian societies. This reading
 *   instantiates customary rule through its ECONOMIC CORE: tenure as
 *   community membership and use rights, with inalienability (prohibition on
 *   sale, mortgage, or conversion) as the mechanism protecting against market
 *   capture and generational land loss. The constraint operates between a
 *   land-holding community (beneficiary, organized across generations) and
 *   would-be buyers, state registrars, private developers, and speculative
 *   investors (victims, whose extraction mechanisms are foreclosed by
 *   inalienability). The sibling readings — elder_adjudication (the JUDICIAL
 *   form) and lineage_chieftaincy (the POLITICAL form) — are distinct
 *   constraints that network with this one but operate on different
 *   structural dimensions. This reading focuses solely on the tenure
 *   function: how land access is allocated, protected, and transmitted. The
 *   historical backdrop is colonial land registration (which concentrated
 *   land in European settlers and indigenous elites) and post-colonial state
 *   titling schemes (which have repeatedly enabled land loss to elites
 *   despite modernization narratives). Customary tenure persists where it is
 *   actively enforced by communities and suppresses the state's and capital's
 *   standard extraction mechanisms. The constraint exhibits tangled-rope
 *   structure: genuine coordination (protecting intergenerational access,
 *   preventing fragmentation, managing use conflicts) coexists with genuine
 *   suppression (restricting who can own, constraining transfer options,
 *   requiring elder/lineage approval) and asymmetric extraction (elders
 *   extract authority and deference; the state extracts revenue by capturing
 *   formalization processes; capital extracts by converting customary land to
 *   speculative assets when enforcement declines). Theater ratio is low
 *   (0.35) because the mechanisms are functional, not performative —
 *   inalienability actually prevents land loss, use rights actually allocate
 *   access, elder adjudication actually resolves disputes. The rising
 *   extractiveness trajectory (0.35 → 0.58) reflects intensifying pressure
 *   from state and capital, and rising suppression (0.55 → 0.72) reflects the
 *   increased enforcement effort required to maintain inalienability against
 *   these pressures.
 *
 * KEY AGENTS:
 *   - Land-holding community (intergenerational): Primary beneficiary (organized/constrained) — benefits from inalienability protection, collective tenure security, generational continuity, prevention of land fragmentation
 *   - Lineage groups / elder councils: Secondary beneficiary and enforcement actor (organized/arbitrage) — authority to allocate, adjudicate, enforce; extract deference and decision-making power
 *   - Would-be titleholders / private investors: Primary victim from constraint's perspective (powerful/mobile) — their extraction mechanism (speculation, mortgaging, concentration) is foreclosed; can exit to titled land zones elsewhere
 *   - State land registries / post-colonial government: Secondary victim and enforcer of competing constraint (institutional/constrained) — blocked from revenue extraction through titling; constrained by need to maintain cooperative fiction that formal tenure serves all parties
 *   - Landless migrants / non-members: Powerless victims (powerless/trapped) — absolute exclusion from tenure because membership is criterion; cannot purchase or claim entry
 *   - Analytical observer (ecological/civilizational): Risks naturalizing the constraint as immutable necessity when it is actually contingent institutional choice defended by real beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(customary_rule__customary_land_tenure, 0.58).
domain_priors:suppression_score(customary_rule__customary_land_tenure, 0.72).
domain_priors:theater_ratio(customary_rule__customary_land_tenure, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(customary_rule__customary_land_tenure, extractiveness, 0.58).
narrative_ontology:constraint_metric(customary_rule__customary_land_tenure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(customary_rule__customary_land_tenure, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(customary_rule__customary_land_tenure, tangled_rope).
narrative_ontology:human_readable(customary_rule__customary_land_tenure, "Customary Land Tenure: Community Membership and Use Rights").
narrative_ontology:topic_domain(customary_rule__customary_land_tenure, "political/comparative/property_law").

domain_priors:requires_active_enforcement(customary_rule__customary_land_tenure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(customary_rule__customary_land_tenure, '3ae6523f-ee12-4215-8d2c-68598864f327').
narrative_ontology:cs_kernel_codification('3ae6523f-ee12-4215-8d2c-68598864f327', fixed_text).
narrative_ontology:cs_authority_grounding('3ae6523f-ee12-4215-8d2c-68598864f327', lineage).
narrative_ontology:cs_interpretation_layer_present('3ae6523f-ee12-4215-8d2c-68598864f327').
narrative_ontology:cs_reading_relation('3ae6523f-ee12-4215-8d2c-68598864f327', customary_rule__elder_adjudication, coexists_with).
narrative_ontology:cs_reading_relation('3ae6523f-ee12-4215-8d2c-68598864f327', customary_rule__lineage_chieftaincy, coexists_with).
narrative_ontology:cs_axiom('3ae6523f-ee12-4215-8d2c-68598864f327', foundational, land_inalienable_by_community_membership).
narrative_ontology:cs_axiom_status(land_inalienable_by_community_membership, holdable).
narrative_ontology:cs_axiom_grounding('3ae6523f-ee12-4215-8d2c-68598864f327', land_inalienable_by_community_membership, conventional).
narrative_ontology:cs_axiom('3ae6523f-ee12-4215-8d2c-68598864f327', foundational, intergenerational_tenure_continuity_prior_to_market_exchange).
narrative_ontology:cs_axiom_status(intergenerational_tenure_continuity_prior_to_market_exchange, holdable).
narrative_ontology:cs_axiom_grounding('3ae6523f-ee12-4215-8d2c-68598864f327', intergenerational_tenure_continuity_prior_to_market_exchange, deontological).
narrative_ontology:cs_reference_frame('3ae6523f-ee12-4215-8d2c-68598864f327', land_as_communal_relation).
narrative_ontology:cs_drift_state('3ae6523f-ee12-4215-8d2c-68598864f327', contemporary_market_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3ae6523f-ee12-4215-8d2c-68598864f327', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(customary_rule__customary_land_tenure, customary_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(customary_rule__customary_land_tenure, land_holding_community_intergenerational).
narrative_ontology:constraint_beneficiary(customary_rule__customary_land_tenure, lineage_groups).
narrative_ontology:constraint_victim(customary_rule__customary_land_tenure, would_be_private_titleholders).
narrative_ontology:constraint_victim(customary_rule__customary_land_tenure, speculative_investors).
narrative_ontology:constraint_victim(customary_rule__customary_land_tenure, state_revenue_extraction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LANDLESS MIGRANT (SNARE) — New arrivals or outsiders cannot acquire land through purchase or lease because alienability is structurally suppressed. Tenure is locked to community membership (birth, adoption, or rare grant). Exit from the constraint is material impossibility — cannot pay to enter, cannot relocate claims. The inalienability mechanism that protects community cohesion becomes an absolute barrier for non-members. Suppression is maximal; coordination benefit is zero for this agent.
constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: JUNIOR LINEAGE MEMBER (TANGLED ROPE) — Community member with use rights but not allocation authority. Benefits from the community's collective tenure security and intergenerational stability (genuine coordination: land cannot be seized by outsiders or lost to debt). But constrained by elders' authority over allocation and use restrictions. Can exit via migration and claiming membership elsewhere (high cost, possible). The constraint coordinates land access across generations and kinship groups while extracting authority over use decisions from junior members. Both coordination function and asymmetric enforcement are genuine.
constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ELDER COUNCIL (ROPE) — Authority holders (elders, lineage heads, land committee) experience the constraint as coordination mechanism with minimal coercion overhead. Their role is solving the collective action problem: preventing land fragmentation, maintaining kinship relations across generations, adjudicating disputes, allocating use rights. They benefit from authority and respect but do not experience extraction — the constraint aligns their interests with community reproduction. Exit cost is high (loss of status, community standing) but not material impossibility. Pure coordination from this structural position.
constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: STATE REGISTRAR / PRIVATE DEVELOPER (SNARE) — External actors (state land registries, private developers, mortgage lenders) see customary tenure as blocking their extraction mechanism. The constraint prevents their standard rent-capture pathways: land titling for revenue extraction, mortgaging for debt finance, speculation for asset appreciation, conversion to cash crop export. They experience the inalienability as a ceiling on their own extractiveness. They are not trapped (they can operate in titled land elsewhere, can shift to other regions) but their preferred extraction mechanism is foreclosed in customary land zones. The constraint's suppression of alienability is directly targeted at suppressing THEIR extraction capacity.
constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COLONIAL / POST-COLONIAL STATE (PITON) — The state declares customary tenure 'backward' and 'inefficient' while its own land registries fail to function (corruption, lost records, unprocessed disputes). The official narratives of modernization and formalization persist despite decades of evidence that land titling increases land loss to elites, creates debt traps, and destabilizes smallholders. The state's institutional claim to rational administration is performative — theaters of reform persist while the actual function (secure tenure for the poor, equitable allocation) is achieved better by the customary system. Theater ratio is high: modernization rhetoric decoupled from actual tenure security outcomes. The state benefits from the appearance of administrative authority (arbitrage option — can shift focus to other sectors) and from the ideological frame, not from functional tenure administration.
constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POST-COLONIAL NATION-STATE (TANGLED ROPE, institutional) — The state faces genuine coordination problems: integrating customary zones into a unified legal framework, collecting land tax, managing inter-community disputes, preventing conflicts over encroachment. These are real coordination functions that formal tenure could serve. But the state's actual exercise of this function extracts value: central land registries concentrate information asymmetrically, state-mediated conversion enables elite land grabs, colonial-era land law privileges European-style private ownership. The state's institutional position requires enforcement (land surveyors, courts, police) to maintain the constraint. Both genuine coordination (legal predictability across regions, conflict adjudication) and asymmetric extraction (state captures rents from title processes, enables elite accumulation) are present. The state is constrained by its need to maintain the cooperative fiction that formal tenure serves all parties equally, when it demonstrably benefits capital accumulation over subsistence security.
constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational ecology perspective, the constraint appears as an immutable natural law: land is finite, non-fungible, and place-specific; community membership is the only sustainable tenure form because alienability leads to concentration, land loss, ecosystem collapse, and ultimately social breakdown. Customary tenure rules are not contingent social forms but ecological necessities that emerge wherever human societies successfully persist on land for centuries. This perspective risks naturalizing what is actually a contingent institutional choice — the engine will flag this as a candidate false summit because identifiable beneficiaries (communities, lineage groups) exist who benefit from the maintenance of the inalienability framing. The 'natural law' narrative obscures the real work: active enforcement, elder authority, dispute adjudication, exclusion of outsiders.
constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(customary_rule__customary_land_tenure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(customary_rule__customary_land_tenure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(customary_rule__customary_land_tenure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(customary_rule__customary_land_tenure, TR),
    TR >= 0.70.

:- end_tests(customary_rule__customary_land_tenure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint suppresses alienability, which prevents the state and capital from operating their standard extraction mechanisms (titling revenue, mortgaging, land concentration, speculation). This is genuine extraction FROM the would-be extractors. However, the constraint also includes real intergenerational coordination — land access is not merely redistributed, but coordinated across time and kinship relations in ways that demonstrably improve tenure security relative to titled systems. The ε value reflects both functions: the coordination benefit (which would lower ε) and the suppression of outsiders' extraction (which would raise ε). The measurement trajectory shows rising extractiveness from 0.35 to 0.58, reflecting intensifying pressure from state and capital to integrate customary zones into formal markets. Suppression (0.72): High. Inalienability is enforced through social sanction, elder authority, and community exclusion. The enforcement mechanisms are strong: attempting to sell land without community approval results in social sanctions, invalidation of sale, reputation loss, kinship rupture. The suppression is not merely negative (prohibition) but positive (active enforcement by community members). Theater ratio (0.35): Low. The constraint's mechanisms are functional: inalienability actually prevents speculative land loss, use rights actually allocate access according to kinship and need, elder adjudication actually resolves boundary and use disputes. The constraint is not maintained through theatrical performance but through real material interests (keeping land in the community, preventing concentration, maintaining kinship stability). Theater ratio rises slightly (0.28 → 0.35) as the constraint faces pressure from modernization narratives and state legitimacy claims that emphasize performance over function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The elder council sees pure coordination (Rope) — they are solving the legitimate collective action problem of maintaining land access across generations. The land-holding community sees genuine protection (Tangled Rope with heavy coordination weighting) — they benefit from the coordination while bearing the constraint costs (restricted mobility, elder authority over use). Landless migrants and outsiders see absolute exclusion (Snare) — the inalienability mechanism becomes an impassable barrier. Would-be buyers and speculative investors see foreclosed extraction (Snare from their perspective, but inverted — they are excluded FROM extraction rather than subjected TO it). The state sees its own extraction mechanism blocked (Snare) — titling and formalization are its primary revenue and administrative control tools, and customary tenure denies access. The state also experiences the constraint as a challenge to its authority (Piton) — modernization rhetoric claims formal titling is rational and efficient, but evidence repeatedly shows customary tenure delivers better outcomes, so the state's institutional claim becomes performative. The analytical observer risks seeing immutable natural law (Mountain) — land is finite, markets concentrate, communities evolved inalienability over centuries, this pattern is universal. But this naturalization obscures that real beneficiaries (the elder council, the community) actively maintain the suppression, making it contingent rather than natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: their power, exit options, and beneficiary/victim relationship to the constraint. The elder council (organized/arbitrage) has low d (~0.20): they are beneficiaries with exit options (they can relocate to another community, can exit the elder role through retirement or status loss). The land-holding community (organized/constrained) has moderate d (~0.55): they are both beneficiaries (they benefit from tenure security) and victims (constrained by elder authority, restricted in sale options), with moderate exit costs. Landless migrants (powerless/trapped) have maximum d (~0.98): they are pure victims of the suppression, with zero exit options except relocation to titled land zones (which may be inaccessible, expensive, or unavailable). State registrars and private investors (powerful/mobile) have high d from the constraint's perspective (~0.85): the constraint is specifically targeted at suppressing their extraction capacity, but they have exit options (they can operate in titled land zones elsewhere). The measurement of d for each agent then feeds into the chi formula: χ = ε × f(d) × σ(S). Local scope (σ=0.8) dampens extraction; an agent with d=0.85 at local scope experiences χ less intense than the same agent at regional scope would. The elder council's arbitrage exit option and beneficiary status produce negative effective extraction from their perspective — the constraint subsidizes them. The state's mobile exit and powerful position moderate their experienced extraction relative to the absolute suppression they face.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that customary land tenure IS a mixed coordination-extraction mechanism (Tangled Rope), not a natural law or pure coordination. The false summit risk is real: the analytical observer naturalizes the inalienability as ecological necessity or evolved tradition, erasing the real work of enforcement and the real beneficiaries (elder councils, lineage groups). The constraint's metrics (ε=0.58, suppression=0.72, theater=0.35) reflect that both genuine coordination and genuine enforcement are present. The perspectival gap reveals WHY the mechanism works: it protects community interests from state and capital extraction by making community membership the tenure criterion, not title. But it also extracts authority from junior members and non-members. The intergenerational benefit is real (people retain land across generations in customary zones at much higher rates than in titled zones) but concentrated in elder/lineage hierarchies. The mandatrophy dissolves when we accept that the constraint does BOTH: it coordinates intergenerational access AND suppresses alienability AND extracts authority from non-elders. All three are structurally necessary to the mechanism. The state's claim that formalization would improve outcomes (piton perspective) is demonstrably false in most cases, but the state maintains this narrative through theater despite contrary evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inalienability_mechanism_voluntariness,
    'Is the suppression of alienability maintained by collective choice or by coercive enforcement that would dissolve without active suppression?',
    'Ethnographic and historical evidence of land sales outside official channels; rates of informal alienation; what happens when enforcement capacity declines (institutional collapse, colonial withdrawal, elder authority erosion)',
    'If maintained by collective preference: the constraint is a genuine coordination mechanism (Rope rises above the snare floor). If maintained only by active enforcement of elder authority despite widespread desire to sell: the constraint is extractive despite the collective-sounding framing (snare rises above tangled rope). If bifurcated (core lineage zones genuinely protected by preference; peripheral zones maintained only by enforcement): the constraint decomposes into family of constraints with different ε values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inalienability_mechanism_voluntariness, empirical, 'Whether inalienability is maintained by collective preference or coercive enforcement').

omega_variable(
    intergenerational_benefit_distribution,
    'Does the inalienability mechanism distribute intergenerational benefits equally across community members, or does it concentrate authority and access in elder/lineage hierarchies?',
    'Distribution of land allocation across age, gender, lineage status; rates of landlessness within communities despite formal tenure system; gendered access to land use and allocation decision-making; comparison of elder-lineage material benefit to junior/female/non-lineage benefit',
    'If truly distributed: genuine intergenerational coordination (Rope). If concentrated in elder/lineage authority: the beneficiary is not ''the community'' but a specific power hierarchy; the constraint is tangled rope or snare depending on who bears suppression costs. If highly gendered: victim set must be specified by gender; the constraint may have different ε and suppression values for different community members.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_benefit_distribution, empirical, 'Whether intergenerational benefits are equally distributed or concentrated in elder hierarchies').

omega_variable(
    state_formalization_counterfactual,
    'What would happen to tenure security, land loss, and community cohesion if customary tenure were formally titled and integrated into state registries?',
    'Comparative case studies: regions where formalization occurred (land loss rates, elite accumulation, tenure insecurity post-formalization); economic modeling of speculative capture under open alienability; ethnographic evidence of community outcomes in formalized vs customary zones',
    'If formalization causes land loss and tenure insecurity (as most evidence suggests): the state''s modernization narrative is false, and the inalienability mechanism is protecting against state-enabled extraction (suppression is functional, not coercive). If formalization increases security and reduces loss (contrary to evidence): the state perspective is correct and customary tenure is rent-seeking dressed as tradition. This omega determines whether the state''s piton classification (degraded institution) is accurate or whether the state is the true snare and customary tenure is a genuine rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_formalization_counterfactual, empirical, 'Outcomes of state formalization of customary tenure in comparable contexts').

omega_variable(
    reading_contest_naturalization,
    'Is the claim that customary tenure is the ''natural'' or ''traditional'' form a descriptor of what actually evolved, or is it part of the committer assertion that this reading makes?',
    'Historical reconstruction: how did customary tenure systems emerge? Through centuries of evolutionary pressure? Through recent codification for anti-colonial assertion? Through invented tradition in response to colonial threat? Linguistic analysis of how ''customary'' is used in debates (naturalization vs. strategic choice).',
    'If evolved over centuries through selection pressure: the natural law perspective (mountain) has more warrant, but the beneficiary presence still triggers FSM evaluation. If partly invented or recently codified as assertion against colonialism: the reading is more clearly a strategic choice (tangled rope for the community, snare for outsiders) and less a natural law. If both: the constraint is a genuine hybrid — evolved practices now strategically asserted and enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_naturalization, conceptual, 'Whether customary tenure is an evolved mechanism or a strategic assertion').

omega_variable(
    kernel_reading_contest_location,
    'Where is the actual dispute between the customary_land_tenure reading and its siblings located? Is it about the economic/tenure function, the judicial function, or the political function — or about whether these are separable?',
    'Textual analysis of actual disputes in customary rule communities: do communities and states argue about HOW land tenure should work (economic form)? Or about WHO decides (judicial/political form)? Or do they insist these are inseparable (land tenure cannot be separated from elder adjudication and chieftaincy)?',
    'If separable: each reading (tenure, adjudication, political form) is genuinely independent and three separate constraints are the right decomposition. If inseparable: the three readings are aspects of one constraint and the network edges should reflect deep coupling. If contested whether separable: the omega itself documents the reading contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether customary rule''s three dimensions (tenure, adjudication, political form) are structurally separable or inseparable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(customary_rule__customary_land_tenure, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clt_tr_t0, customary_rule__customary_land_tenure, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clt_tr_t30, customary_rule__customary_land_tenure, theater_ratio, 30, 0.32).
narrative_ontology:measurement(clt_tr_t60, customary_rule__customary_land_tenure, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(clt_be_t0, customary_rule__customary_land_tenure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clt_be_t30, customary_rule__customary_land_tenure, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(clt_be_t60, customary_rule__customary_land_tenure, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clt_su_t0, customary_rule__customary_land_tenure, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clt_su_t30, customary_rule__customary_land_tenure, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(clt_su_t60, customary_rule__customary_land_tenure, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(customary_rule__customary_land_tenure, resource_allocation).
narrative_ontology:affects_constraint(customary_rule__customary_land_tenure, elder_adjudication).
narrative_ontology:affects_constraint(customary_rule__customary_land_tenure, lineage_chieftaincy).
narrative_ontology:affects_constraint(customary_rule__customary_land_tenure, state_land_registry_capture).
narrative_ontology:affects_constraint(customary_rule__customary_land_tenure, speculative_land_accumulation).

% DUAL FORMULATION NOTE:
% The customary_land_tenure constraint is part of a three-member constraint family decomposing the contested kernel 'customary_rule'. The sibling readings (elder_adjudication, lineage_chieftaincy) operate on different structural dimensions (judicial, political) but network with this constraint through tight institutional coupling. All three readings are live in contemporary customary rule communities; the reading contest is empirical (how are these dimensions structurally related?) and strategic (which dimension do state and capital target for dissolution?). The constraint also affects upstream constraints about state land registry systems and downstream constraints about speculative accumulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(customary_rule__customary_land_tenure, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
