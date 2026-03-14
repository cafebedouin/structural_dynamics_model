% ============================================================================
% CONSTRAINT STORY: irish_nationalist_movement_1900s
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irish_nationalist_movement_1900s, []).

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
 *   constraint_id: irish_nationalist_movement_1900s
 *   human_readable: Irish Nationalist Movement Coordination and Extraction (1900s-1922)
 *   domain: political/anti_colonial
 *
 * SUMMARY:
 *   The Irish nationalist movement from 1900 to 1922 presents a complex
 *   constraint that combines genuine anti-colonial coordination (uniting
 *   disparate communities and classes against British dominance) with
 *   systematic extraction from internal constituencies (rural poor, working
 *   class, women, minority groups). The movement emerges from political
 *   agitation in 1900, radicalizes after the Easter Rising of 1916, and
 *   culminates in the establishment of the Irish Free State in 1922. The
 *   constraint exhibits transformative dynamics: early theater-heavy
 *   political negotiation gives way to militant organization, then military
 *   conflict, and finally state consolidation. The extractiveness metric
 *   shows growth over the interval (0.35→0.58) reflecting increasing
 *   militarization and coercive resource requisitions, while theater ratio
 *   declines initially (as the movement becomes functionally militant) then
 *   stabilizes (as institutional consolidation requires renewed performative
 *   legitimation). This constraint demonstrates how a single structural
 *   arrangement can be classified as snare, tangled rope, rope, scaffold, and
 *   piton depending on the observer's structural position and time horizon,
 *   revealing the tension between viewing nationalism as liberation versus
 *   extraction.
 *
 * KEY AGENTS:
 *   - Rural Irish Poor: Primary victim (powerless/trapped) — economically dependent on nationalist networks; subject to coercive requisitions and social sanctions; no viable exit
 *   - Irish Working Class: Primary victim (powerless/trapped) — employment controlled through nationalist organizations; strike participation becomes nationalist obligation; emigration as only exit
 *   - Irish Nationalist Leadership: Primary beneficiary (institutional/arbitrage) — captures political authority, resource flows, and state apparatus; benefits from movement's coordination and coercive functions
 *   - Emerging Irish State: Primary beneficiary (institutional/mobile) — vehicle for nationalist consolidation; gains territorial control and administrative capacity from movement's success
 *   - Urban Intellectuals/Professionals: Secondary beneficiary (moderate/constrained) — advance careers through nationalist credentials; coordinate cultural revival; constrained but not trapped
 *   - Catholic Church: Institutional actor (institutional/constrained) — coordinates religious legitimacy with nationalist mobilization; provides alternative authority; genuinely benefits but retains partial independence
 *   - Constitutional Nationalists: Organized beneficiaries (organized/mobile) — coordinate political advancement; can exit or negotiate; see movement as coordination, not trap
 *   - Irish Unionist Minority: Secondary victim (powerful/trapped) — despite nominal power and wealth, structurally trapped by nationalist dominance; property and community threatened; high extraction despite power
 *   - British Colonial Administration: Institutional actor (institutional/constrained) — maintains degraded colonial structure; piton perspective: institutions persist through inertia despite declining function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irish_nationalist_movement_1900s, 0.58).
domain_priors:suppression_score(irish_nationalist_movement_1900s, 0.72).
domain_priors:theater_ratio(irish_nationalist_movement_1900s, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irish_nationalist_movement_1900s, extractiveness, 0.58).
narrative_ontology:constraint_metric(irish_nationalist_movement_1900s, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(irish_nationalist_movement_1900s, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irish_nationalist_movement_1900s, tangled_rope).
narrative_ontology:human_readable(irish_nationalist_movement_1900s, "Irish Nationalist Movement Coordination and Extraction (1900s-1922)").
narrative_ontology:topic_domain(irish_nationalist_movement_1900s, "political/anti_colonial").

domain_priors:requires_active_enforcement(irish_nationalist_movement_1900s).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irish_nationalist_movement_1900s, irish_nationalist_leadership).
narrative_ontology:constraint_beneficiary(irish_nationalist_movement_1900s, emerging_irish_state).
narrative_ontology:constraint_victim(irish_nationalist_movement_1900s, irish_rural_poor).
narrative_ontology:constraint_victim(irish_nationalist_movement_1900s, irish_working_class).
narrative_ontology:constraint_victim(irish_nationalist_movement_1900s, irish_women).
narrative_ontology:constraint_victim(irish_nationalist_movement_1900s, unionist_minority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL IRISH POOR (SNARE) — Trapped by land tenure insecurity, economic dependency on nationalist political structures, and coercive social sanctions against non-participation in nationalist activities. Cannot exit the constraint: land remains controlled through networks affiliated with nationalist leadership; economic alternatives are scarce; dissent is met with community ostracism or IRA violence. Bears full extraction burden through forced contributions, labor requisitions, and exposure to counter-insurgency violence. Experiences maximum suppression with no genuine alternative.
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IRISH WORKING CLASS (SNARE) — Trapped by employment dependency on nationalist-controlled enterprises and labor organizations. Strike participation becomes a nationalist obligation rather than a working-class strategic choice. IRA control over labor coercion mechanisms means economic penalties for non-participation in nationalist campaigns. No mobile alternative: emigration is the only exit, and emigration itself is constrained by economic barriers and nationalist social pressure. Suppression is structural (economic) and social (community sanctions).
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN NATIONALIST INTELLECTUALS/PROFESSIONALS (TANGLED ROPE) — Constrained but not trapped. Career advancement depends on nationalist credentials; professional networks are infiltrated by nationalist leadership; exit requires abandoning social standing and professional prospects. But genuine coordination benefit exists: shared cultural revival, language restoration, institutional development benefit these agents materially. The constraint coordinates intellectual identity and political mobilization while extracting conformity and career direction. Moderate extraction but real agency — can negotiate within the movement or exit at professional cost.
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CATHOLIC CHURCH HIERARCHY (TANGLED ROPE) — Constrained by strong but not absolute alignment with nationalist sentiment among congregations. The Church coordinates moral authority with nationalist mobilization, extracting both legitimacy and influence. But the Church retains institutional autonomy: can moderate nationalist demands, mediate between communities, provide alternative authority structures. Coordination function is genuine (shared spiritual and cultural identity); extraction is real (nationalist leadership uses Church authority). Church can exit partially through institutional distance, but does not; benefits outweigh costs from institutional perspective.
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL NATIONALIST POLITICIANS (ROPE) — Organized agents with exit options. These actors can leave the nationalist coalition (some did, particularly after 1916), can negotiate alternative arrangements, can form competing political structures. The constraint coordinates political advancement and electoral legitimacy while extraction is low: career benefits align with movement success. Suppression is minimal — no external pressure constrains their exit capacity. This group sees nationalism as a coordination mechanism, not a trap. Can exit; chooses participation.
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: IRISH UNIONIST MINORITY (SNARE) — Trapped by the nationalist movement's control over state machinery and security apparatus after independence. Pre-1922, trapped by the movement's military organization and social sanctions. Cannot exit: their property, community identity, and civic standing are threatened by nationalist dominance. High suppression and extraction despite their nominal power and wealth — the movement's political success eliminates their structural position. Experiences pure extraction through expropriation, community displacement, and political disenfranchisement.
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: IRISH STATE ADMINISTRATIVE APPARATUS (SCAFFOLD) — Sees the nationalist constraint as a temporary coordination mechanism with a built-in sunset. The movement's coordination function (territorial independence, state consolidation) has a clear endpoint: once the state is established (1922), the movement's extractive mechanisms can theoretically decline and dissolve into normal governance. Administrative apparatus experiences extraction pressure during conflict (requisitions, security demands) but believes suppression will decrease post-independence. Sunset logic: the constraint's function is achieved upon state formation.
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: BRITISH COLONIAL ADMINISTRATION (PITON) — Sees the nationalist movement as a degraded resistance structure that persists through institutional inertia and tradition rather than functional necessity. Early 1900s: Irish nationalism is theatrical grievance with limited revolutionary capacity. Over the interval, the constraint's actual capacity grows (military organization, social mobilization) while British administrative response becomes increasingly ritualized and ineffective (military counterinsurgency, political negotiation theater that concedes nothing). By 1922, the piton has become functionally dominant — the old colonial structure cannot exit (constrained by precedent and institutional commitments) but no longer serves its coordination function. The movement's existence now confirms the necessity of the movement.
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (TANGLED ROPE) — From a civilizational perspective, the Irish nationalist movement coordinates anti-colonial resistance (genuine coordination function: uniting disparate communities against external dominance) while extracting conformity and directing resources toward nationalist objectives rather than immediate working-class or rural development. The constraint's extractiveness increases over time as the movement becomes more militant and coercive. Suppression from the British colonial administration is high, which paradoxically validates the movement's existence and necessity. The analytical observer sees real coordination (anti-colonial mobilization) alongside real extraction (subordination of internal interests to nationalist strategy). Mandatrophy is resolved: this is not a pure coordination mechanism masquerading as extraction; nor is it pure extraction wearing coordination's mask. Both functions are structurally present.
constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irish_nationalist_movement_1900s_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irish_nationalist_movement_1900s, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irish_nationalist_movement_1900s, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irish_nationalist_movement_1900s, TR),
    TR >= 0.70.

:- end_tests(irish_nationalist_movement_1900s_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and increasing. The movement extracts labor, resources, and conformity from internal constituencies (rural poor, working class, women excluded from leadership). Early extractiveness is lower (0.35) because the movement is primarily a political-theater phenomenon — Sinn Féin advocacy, electoral participation, cultural nationalism, Home Rule negotiation. After the 1916 Easter Rising, extractiveness increases (0.48) as the movement militarizes: IRA organization, resource requisitions, armed campaign. By the Treaty crisis (0.62), extractiveness peaks as the movement demands participation in civil war and state consolidation. Post-Treaty (0.58), extractiveness remains elevated as the new state institutionalizes coercive capacity. The metric reflects the movement's transformation from coordination mechanism (early period) to extraction apparatus (militant period) while maintaining coordination functions throughout. Suppression (0.72): High and persistent. External suppression from British colonial administration is constant; internal suppression by the movement (against collaborators, women, minority voices, labor radicalism) intensifies post-1916. Suppressions mechanisms include IRA violence, community ostracism, legal prosecution, economic boycotts, and control over resources. Rural poor and working class face dual suppression: from British authorities and from nationalist leadership enforcing participation. Theater ratio (0.55): Moderate and declining over interval. Early period (1900-1916) high theater: Home Rule political theater, constitutional negotiation, cultural performance, Sinn Féin electioneering. Post-1916 theater declines: the movement becomes functionally militant (armed campaign, direct control, resource extraction). Theater stabilizes at elevated levels (0.55) post-Treaty as the new state requires legitimation ritual alongside actual coercive capacity. The theater decline reflects increased functionality and intensity, not decline in institutional legitimacy needs.
 *
 * PERSPECTIVAL GAP:
 *   The movement produces radically divergent classifications across structural positions. The powerless (rural poor, working class, unionist minority) perceive snare: high extraction, high suppression, no exit alternatives. Moderate agents (urban professionals, intellectuals) perceive tangled rope: constrained but not trapped; genuine benefit from coordination alongside extraction cost. Organized agents (constitutional politicians) perceive rope: coordination benefits, exit capacity, voluntary participation. The Catholic Church perceives tangled rope with institutional distance: genuine coordination but retained autonomy. The state apparatus perceives scaffold: temporary coordination with built-in sunset upon state establishment. The British administration perceives piton: degraded institutions persisting through inertia despite declining function. The analytical observer perceives tangled rope: both genuine anti-colonial coordination and systematic internal extraction are structurally present. The gap is not merely observational difference; it reflects real structural differences in how the constraint binds different agents. The rural poor genuinely cannot exit; the constitutional politicians genuinely can. This is not perceptual relativism — it is structural heterogeneity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint: are they beneficiaries or victims, and what exit capacity do they hold? Rural poor and working class: victims + trapped → high d (0.85-0.95) → high χ. They experience the constraint as maximally extractive. Nationalist leadership: beneficiaries + arbitrage → low d (0.10-0.20) → low/negative χ. They experience low extraction, high coordination benefit. Constitutional politicians: beneficiaries + mobile → low d (0.25-0.35) → negative χ. They experience coordination benefits with exit capacity; extraction is self-imposed. Urban professionals: mixed (beneficiary status + constrained exit) → moderate d (0.40-0.50) → moderate χ. They experience real constraints on career exit but genuine professional advancement from participation. Catholic Church: institutional beneficiary + constrained → low d (0.20-0.30) → low χ. They retain autonomy while benefiting from coordination. The derivation chain correctly predicts that powerless agents experience snare (high d → high f(d) → high χ) while institutional beneficiaries experience rope (low d → negative f(d) → negative χ). Same base constraint, different d values per agent, different perceived classifications. This demonstrates why directionality is not a single property of the constraint but an emergent property of the agent-constraint relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's classification as tangled rope is justified by distinguishing genuine anti-colonial coordination function from internal extraction. The movement coordinates resistance to British colonialism (genuine coordination: unites classes, communities, geographic regions against external dominance). The movement simultaneously extracts from internal constituencies (labor, resources, conformity, subordination of internal interests to nationalist strategy). Both functions are structurally present, not one masquerading as the other. Beneficiaries exist (nationalist leadership, emerging state, urban professionals) and victims exist (rural poor, working class, unionist minority, women). Active enforcement is required (IRA coercion, resource requisitions, social sanctions). The constraint cannot be reclassified as rope (pure coordination) because extraction is systematic and asymmetric; it cannot be reclassified as snare (pure extraction) because coordination function is genuine. The mandatrophy dissolves when we recognize that nationalist leadership genuinely believes in the anti-colonial project AND genuinely extracts from internal constituencies, and both aspects are structurally necessary to the movement's function. The movement cannot coordinate anti-colonial resistance without extracting from internal populations; the movement cannot maintain internal discipline without coercive apparatus that also suppresses internal dissent. The tangled rope classification captures this: genuine coordination plus systematic asymmetric extraction, both functions active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_vs_coercion,
    'How much of the rural poor''s participation is driven by internalized nationalist identity fusion versus direct coercion and economic dependency?',
    'Post-independence behavioral changes: tracking of participation levels in nationalist activities after the movement''s extractive mechanisms dissolve; analysis of voluntary vs coerced participation rates by community; exit behavior when suppression mechanisms are removed',
    'If primarily identity fusion (high): Irish society has genuinely coordinated around nationalist project; rural poor are identity_locked rather than trapped. If primarily coercion (high): rural poor are trapped; suppression mechanisms persist post-independence under different institutional forms. If mixed: decompose into separate constraints (identity_locked narrative vs coercive extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_coercion, empirical, 'Identity fusion vs direct coercion in nationalist participation').

omega_variable(
    state_capacity_transition,
    'Does the Irish state post-1922 retain the nationalist movement''s extractive mechanisms, or do suppression and extraction genuinely decline as the scaffold perspective predicts?',
    'Comparative analysis of resource extraction, coercive capacity, and suppression levels: pre-independence (1900-1922) vs post-independence (1922-1932); tracking of IRA organizational structure, resource flows, and violence after independence; measurement of state legitimacy and voluntary compliance changes',
    'If extraction continues: scaffold perspective is incorrect; the movement''s function was not to establish the state but to establish permanent dominant coalition control. If extraction declines: scaffold logic is confirmed; the movement was a temporary coordination mechanism whose extractive mechanisms were structurally tied to colonial opposition. If mixed: bifurcation into separate state coalitions and residual nationalist organization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_capacity_transition, empirical, 'Whether nationalist extraction mechanisms persist or decline post-independence').

omega_variable(
    unionist_exit_alternative_sufficiency,
    'Did unionist minority have structurally viable exit options (political integration with Northern Ireland, emigration, property sales) or were exits foreclosed by the movement''s coercive apparatus?',
    'Historical analysis of unionist exit behavior (emigration, property sales, political migration); data on movement restrictions on unionist transactions; legal vs de facto barriers to unionist mobility; post-1922 unionist population distribution and economic status',
    'If exits were structurally viable: unionist experience is constrained extraction (high-cost mobility). If exits were foreclosed: unionist experience is trapped extraction (no mobility). Classification confidence for unionist perspective depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unionist_exit_alternative_sufficiency, empirical, 'Structural viability of unionist exit options during nationalist mobilization').

omega_variable(
    coordination_function_authenticity,
    'Is the movement''s coordination function (anti-colonial unity, territorial independence) authentic or merely a post-hoc rationalization for resource extraction?',
    'Comparative analysis with other anti-colonial movements; measurement of coordination function pre-1916 vs post-1916; analysis of whether leadership could have achieved independence through negotiated settlement (indicating extraction rather than genuine coordination need); study of coordination failures (internal faction violence, resource misallocation) vs coordination successes',
    'If authentic: movement is genuinely tangled rope with both coordination and extraction. If post-hoc rationalization: movement is primarily snare with coordination framing. The classification pivots on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, conceptual, 'Whether anti-colonial coordination function is authentic or post-hoc rationalization').

omega_variable(
    british_piton_rationality,
    'Is the British colonial administration''s persistence in Ireland rational (piton as institutional inertia) or rational (piton as strategic choice to maintain imperial infrastructure)?',
    'Analysis of British policy documents; cost-benefit analysis of Irish administration vs withdrawal; comparison with British exit from other colonies; measurement of extractive flows from Ireland to Britain',
    'If inertia: piton classification is justified; the constraint persists because alternatives are cognitively blocked. If strategic choice: British administration should be classified as snare (extracting from Ireland) or rope (coordinating empire infrastructure), not piton. Affects classification of whether British perspective is genuinely degraded or deliberately maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(british_piton_rationality, empirical, 'Whether British colonial persistence is institutional inertia or strategic extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irish_nationalist_movement_1900s, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irish_nat_tr_t0, irish_nationalist_movement_1900s, theater_ratio, 0, 0.68).
narrative_ontology:measurement(irish_nat_tr_t6, irish_nationalist_movement_1900s, theater_ratio, 6, 0.62).
narrative_ontology:measurement(irish_nat_tr_t12, irish_nationalist_movement_1900s, theater_ratio, 12, 0.48).
narrative_ontology:measurement(irish_nat_tr_t18, irish_nationalist_movement_1900s, theater_ratio, 18, 0.52).
narrative_ontology:measurement(irish_nat_tr_t22, irish_nationalist_movement_1900s, theater_ratio, 22, 0.55).

% Extraction over time
narrative_ontology:measurement(irish_nat_be_t0, irish_nationalist_movement_1900s, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(irish_nat_be_t6, irish_nationalist_movement_1900s, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(irish_nat_be_t12, irish_nationalist_movement_1900s, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(irish_nat_be_t18, irish_nationalist_movement_1900s, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(irish_nat_be_t22, irish_nationalist_movement_1900s, base_extractiveness, 22, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irish_nationalist_movement_1900s, identity_coordination).
narrative_ontology:boltzmann_floor_override(irish_nationalist_movement_1900s, 0.12).
narrative_ontology:affects_constraint(irish_nationalist_movement_1900s, british_imperial_administration).
narrative_ontology:affects_constraint(irish_nationalist_movement_1900s, irish_labor_movement_1900s).
narrative_ontology:affects_constraint(irish_nationalist_movement_1900s, irish_gender_suffrage).
narrative_ontology:affects_constraint(irish_nationalist_movement_1900s, irish_catholic_institutional_power).

% DUAL FORMULATION NOTE:
% The Irish nationalist movement decomposes into multiple structurally distinct constraints: (1) anti-colonial political coordination (ε≈0.30, Rope) vs (2) internal extractive hierarchy and resource mobilization (ε≈0.65, Snare). The base story combines both functions into tangled rope (ε=0.58). Separation is possible but counterproductive analytically — the constraint's function relies on fusion of coordination and extraction. The movement could be decomposed into separate stories for anti-colonial coordination (affects British imperial administration) vs labor extraction (affects Irish labor movement) vs women's role subordination (affects Irish gender suffrage), but the constraint's structural identity requires the tangled relationship. Linked constraints show how the movement affects institutional actors (Catholic Church) and subordinated movements (labor, women's suffrage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irish_nationalist_movement_1900s, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
