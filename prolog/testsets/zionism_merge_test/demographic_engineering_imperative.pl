% ============================================================================
% CONSTRAINT STORY: demographic_engineering_imperative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_engineering_imperative, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: demographic_engineering_imperative
 *   human_readable: Demographic Engineering Imperative in Zionist State-Building
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The demographic engineering imperative emerged from the structural
 *   contradiction at the heart of Zionist state-building: establishing a
 *   Jewish-majority state in a territory with an Arab majority population.
 *   From the 1930s onward, Zionist leadership explicitly debated 'transfer'
 *   proposals — the euphemistic term for Arab population removal — as a
 *   solution to the demographic problem. The 1947 UN Partition Plan allocated
 *   55% of Mandatory Palestine to a Jewish state despite Jews comprising only
 *   33% of the population and owning less than 7% of the land. The 1948 war
 *   resulted in the displacement of approximately 750,000 Palestinians
 *   (roughly 80% of the Arab population in the territory that became Israel),
 *   creating the demographic majority that had not existed before. Post-1948
 *   policies — military government over remaining Arab citizens until 1966,
 *   denial of refugee return under the 1950 Law of Return's asymmetry
 *   (automatic citizenship for Jews, permanent exclusion for Palestinian
 *   refugees), land confiscation under the Absentee Property Law, and ongoing
 *   settlement expansion in the West Bank — represent the constraint's
 *   continued operation. The imperative exhibits both coordination
 *   (organizing Jewish immigration and state-building) and extraction
 *   (systematic displacement and dispossession of Palestinians). The
 *   constraint's theater_ratio (0.42) reflects moderate performative content:
 *   demographic management is conducted through legal and bureaucratic
 *   mechanisms that maintain democratic appearances while producing
 *   exclusionary outcomes. The ratio is lower than purely theatrical
 *   constraints because the demographic engineering is functionally
 *   effective, not merely symbolic.
 *
 * KEY AGENTS:
 *   - Zionist Movement Leadership: Primary beneficiary (institutional/arbitrage) — achieved state-building goal through demographic transformation; maintains political power through Jewish majority
 *   - Palestinian Arab Population: Primary victim (powerless/trapped) — subjected to displacement, land confiscation, and denial of return; no exit from the constraint's operation
 *   - Jewish Immigrant Population: Secondary beneficiary (moderate/constrained) — benefits from immigration incentives and citizenship rights; constrained by security costs and moral burdens of displacement mechanism
 *   - State Security Apparatus: Institutional actor (institutional/constrained) — benefits from expanded authority and resources; constrained by perpetual conflict and international censure; identity fused with demographic project
 *   - Refugee Communities: Secondary victim (powerless/trapped) — multi-generational displacement in camps; legally barred from return; denied citizenship in host states
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination function (Jewish refuge after persecution) and extraction mechanism (Palestinian dispossession) as structurally coexistent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_engineering_imperative, 0.78).
domain_priors:suppression_score(demographic_engineering_imperative, 0.85).
domain_priors:theater_ratio(demographic_engineering_imperative, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_engineering_imperative, extractiveness, 0.78).
narrative_ontology:constraint_metric(demographic_engineering_imperative, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(demographic_engineering_imperative, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_engineering_imperative, tangled_rope).
narrative_ontology:human_readable(demographic_engineering_imperative, "Demographic Engineering Imperative in Zionist State-Building").
narrative_ontology:topic_domain(demographic_engineering_imperative, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(demographic_engineering_imperative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(demographic_engineering_imperative, '56c32dbe-7898-4356-888b-3cc80bb67e63').
narrative_ontology:cs_kernel_codification('56c32dbe-7898-4356-888b-3cc80bb67e63', distributed).
narrative_ontology:cs_authority_grounding('56c32dbe-7898-4356-888b-3cc80bb67e63', lineage).
narrative_ontology:cs_interpretation_layer_present('56c32dbe-7898-4356-888b-3cc80bb67e63').
narrative_ontology:cs_reading_relation('56c32dbe-7898-4356-888b-3cc80bb67e63', demographic_engineering_imperative__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('56c32dbe-7898-4356-888b-3cc80bb67e63', demographic_engineering_imperative__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('56c32dbe-7898-4356-888b-3cc80bb67e63', foundational, persecution_justifies_return).
narrative_ontology:cs_axiom_status(persecution_justifies_return, holdable).
narrative_ontology:cs_axiom_grounding('56c32dbe-7898-4356-888b-3cc80bb67e63', persecution_justifies_return, deontological).
narrative_ontology:cs_axiom('56c32dbe-7898-4356-888b-3cc80bb67e63', foundational, historical_presence_grounds_sovereignty).
narrative_ontology:cs_axiom_status(historical_presence_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('56c32dbe-7898-4356-888b-3cc80bb67e63', historical_presence_grounds_sovereignty, conventional).
narrative_ontology:cs_axiom('56c32dbe-7898-4356-888b-3cc80bb67e63', secondary, demographic_majority_necessary_for_security).
narrative_ontology:cs_axiom_status(demographic_majority_necessary_for_security, holdable).
narrative_ontology:cs_axiom_grounding('56c32dbe-7898-4356-888b-3cc80bb67e63', demographic_majority_necessary_for_security, empirically_contingent).
narrative_ontology:cs_reference_frame('56c32dbe-7898-4356-888b-3cc80bb67e63', ancient_jewish_sovereignty_claim).
narrative_ontology:cs_drift_state('56c32dbe-7898-4356-888b-3cc80bb67e63', post_1948_state_establishment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56c32dbe-7898-4356-888b-3cc80bb67e63', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demographic_engineering_imperative, zionist_movement_leadership).
narrative_ontology:constraint_beneficiary(demographic_engineering_imperative, jewish_immigrant_population).
narrative_ontology:constraint_beneficiary(demographic_engineering_imperative, state_security_apparatus).
narrative_ontology:constraint_victim(demographic_engineering_imperative, palestinian_arab_population).
narrative_ontology:constraint_victim(demographic_engineering_imperative, internally_displaced_persons).
narrative_ontology:constraint_victim(demographic_engineering_imperative, refugee_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN ARAB POPULATION (SNARE) — Trapped by military control, legal restrictions on movement and return, and systematic displacement mechanisms. Experiences the demographic imperative as pure extraction: home demolitions, land confiscation, residency revocations, and denial of return rights. No coordination function visible from this position — the constraint exists to reduce their presence. Maximum extraction with no exit.
constraint_indexing:constraint_classification(demographic_engineering_imperative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JEWISH IMMIGRANT POPULATION (TANGLED ROPE) — Benefits from immigration incentives, housing subsidies, and citizenship rights, but also constrained by the security costs and moral burdens of the displacement mechanism. Experiences both coordination (absorption infrastructure, economic opportunity) and extraction (military service requirements, perpetual conflict, ethical compromise). Constrained exit — leaving means abandoning the Zionist project and community ties.
constraint_indexing:constraint_classification(demographic_engineering_imperative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ZIONIST MOVEMENT LEADERSHIP (ROPE) — Primary beneficiary. Experiences the demographic imperative as coordination: solving the genuine problem of establishing a Jewish-majority state in a territory with Arab majority. Immigration policy, land acquisition, and population management are functional tools for state-building. Net beneficiary with arbitrage-level exit options (international mobility, capital flight capacity).
constraint_indexing:constraint_classification(demographic_engineering_imperative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SECURITY APPARATUS (TANGLED ROPE) — Institutional actor that both benefits from (expanded authority, resource allocation, institutional permanence) and is constrained by (perpetual conflict, international censure, operational costs) the demographic imperative. Coordinates security functions while extracting from both Palestinian and Jewish populations through surveillance, military service, and emergency powers. Constrained exit — institutional identity fused with the demographic project.
constraint_indexing:constraint_classification(demographic_engineering_imperative, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFUGEE COMMUNITIES (SNARE) — Descendants of 1948 and 1967 displacement, trapped in camps across Lebanon, Jordan, Syria, and occupied territories. Legally barred from return, denied citizenship in host states, and subjected to ongoing dispossession. Experience the demographic imperative as permanent extraction with no coordination function. Generational time horizon reflects multi-generational refugee status. Trapped exit — return blocked by law and military force.
constraint_indexing:constraint_classification(demographic_engineering_imperative, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the demographic imperative exhibits both genuine coordination (solving the collective action problem of Jewish refuge after persecution) and substantial extraction (systematic displacement of indigenous population). The constraint coordinates Jewish immigration and state-building while extracting from Palestinians through dispossession. The analytical classification as tangled_rope reflects the structural reality: both functions coexist and neither can be eliminated without dissolving the constraint.
constraint_indexing:constraint_classification(demographic_engineering_imperative, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_engineering_imperative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(demographic_engineering_imperative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_engineering_imperative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(demographic_engineering_imperative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(demographic_engineering_imperative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The demographic imperative extracts systematically from Palestinians through displacement, land confiscation, residency revocations, and denial of return rights. The extraction is not total (0.78 rather than 0.95) because some coordination function exists for Jewish immigrants (absorption infrastructure, economic opportunity). The spike to 0.82 in 1948 reflects the Nakba's acute displacement phase. Suppression (0.85): Very high. Palestinians face military control, legal restrictions on movement and return, administrative detention, home demolitions, and systematic barriers to political organization. The spike to 0.88 in 1948 reflects wartime military operations and mass displacement. Suppression remains high (0.85) in the contemporary period due to ongoing occupation, settlement expansion, and denial of refugee return. Theater ratio (0.42): Moderate. Demographic management operates through legal and bureaucratic mechanisms (citizenship law, residency requirements, land-use planning, military administration) that maintain democratic appearances while producing exclusionary outcomes. The theater is real but not dominant — the mechanisms are functionally effective at demographic engineering, not merely symbolic. The ratio has increased modestly over time as international scrutiny has required more elaborate justificatory frameworks.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. The Zionist movement leadership experiences coordination (Rope) — solving the genuine problem of establishing a Jewish refuge state after European persecution. The Jewish immigrant population experiences mixed coordination and extraction (Tangled Rope) — benefiting from absorption infrastructure while bearing security costs and moral burdens. The Palestinian Arab population experiences pure extraction (Snare) — displacement, dispossession, and denial of return with no coordination function visible from their position. The state security apparatus experiences institutional tangled_rope — benefiting from expanded authority while constrained by perpetual conflict. Refugee communities experience generational snare — multi-generational displacement with no exit. The analytical observer sees structural tangled_rope — both coordination (Jewish refuge) and extraction (Palestinian dispossession) coexist and neither can be eliminated without dissolving the constraint. The perspectival gap is not resolvable through better information — it reflects genuinely different structural positions relative to the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the demographic imperative. The Zionist movement leadership are primary beneficiaries — they achieved state-building goals through demographic transformation and maintain political power through Jewish majority. Their directionality is low (near 0.0), producing low or negative effective extraction (they experience the constraint as coordination). The Palestinian Arab population are primary victims — subjected to displacement, land confiscation, and denial of return. Their directionality is very high (near 1.0), and combined with trapped exit options, produces maximum effective extraction (they experience pure snare). The Jewish immigrant population are secondary beneficiaries with constrained exit — they benefit from immigration incentives but also bear security costs. Their directionality is moderate (0.3-0.4), producing moderate effective extraction (tangled rope experience). The state security apparatus is an institutional actor with identity fusion — benefits from expanded authority but constrained by perpetual conflict. Directionality is moderate (0.35-0.45), producing institutional tangled_rope. Refugee communities are secondary victims with trapped exit and generational time horizon — directionality very high (0.9-1.0), producing maximum extraction over generational timescale.
 *
 * MANDATROPHY ANALYSIS:
 *   The demographic engineering imperative resolves the mandatrophy by demonstrating that tangled_rope is the structurally accurate classification when both coordination and extraction are irreducible. The constraint genuinely coordinates Jewish immigration and state-building (solving the collective action problem of refuge after persecution) AND genuinely extracts from Palestinians through systematic displacement. Neither function can be eliminated without dissolving the constraint: removing the coordination function means no Jewish state; removing the extraction function means no Jewish demographic majority. The mandate (establishing a Jewish-majority state in a territory with Arab majority) structurally required the extraction mechanism (displacement or permanent subordination of Arab population). The analytical observer's tangled_rope classification reflects this structural reality. The perspectival divergence (beneficiaries see rope, victims see snare) is not a measurement error — it reflects genuinely different structural positions. The constraint is not 'really' a rope with unfortunate side effects, nor 'really' a snare with a coordination cover story. It is structurally both, and the classification system's job is to capture that irreducible duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_threshold_necessity,
    'Is a Jewish demographic majority structurally necessary for state viability, or is this a constructed requirement that naturalizes displacement?',
    'Comparative analysis of multi-ethnic democracies; examination of alternative constitutional frameworks (binational state, confederation, consociational democracy); historical counterfactuals of partition plans with different demographic assumptions',
    'If structurally necessary: the coordination function is genuine and the constraint is a tragic dilemma. If constructed: the demographic imperative is a cover story for ethno-nationalist exclusion, and the constraint is closer to pure snare from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_threshold_necessity, conceptual, 'Whether demographic majority is necessary or constructed requirement').

omega_variable(
    transfer_inevitability_thesis,
    'Was large-scale Palestinian displacement an inevitable consequence of Zionist state-building, or a contingent outcome of specific policy choices and military actions?',
    'Historical analysis of pre-1948 Zionist leadership debates on transfer; examination of military orders and operational plans during 1947-1949; comparison with alternative partition scenarios; analysis of post-1967 settlement patterns',
    'If inevitable: the demographic imperative''s extractive component was structurally determined by the goal of Jewish majority. If contingent: alternative paths existed, and the displacement represents policy choice rather than structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_inevitability_thesis, empirical, 'Whether displacement was inevitable or contingent outcome').

omega_variable(
    persecution_justification_scope,
    'Does the historical persecution of Jews in Europe justify displacement of Palestinians who bore no responsibility for that persecution?',
    'Ethical analysis of collective rights, historical responsibility, and territorial claims; examination of alternative refuge solutions (Uganda Plan, Birobidzhan, post-war European reconstruction); assessment of Palestinian agency and consent in the process',
    'If justified: the coordination function (Jewish refuge) outweighs the extraction (Palestinian displacement), and the constraint is closer to scaffold from more perspectives. If not justified: the extraction is illegitimate regardless of coordination function, and the constraint is closer to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_justification_scope, preference, 'Whether European persecution justifies Palestinian displacement').

omega_variable(
    demographic_engineering_sunset,
    'Is the demographic imperative a temporary state-building necessity with a natural sunset (once Jewish majority is secured), or a permanent feature requiring ongoing enforcement?',
    'Analysis of post-1948 immigration policy; examination of settlement expansion patterns; assessment of citizenship law evolution; tracking of demographic anxiety in political discourse over time',
    'If temporary: the constraint is scaffold (transitional coordination with sunset). If permanent: the constraint is tangled_rope or snare (ongoing extraction mechanism without sunset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_engineering_sunset, empirical, 'Whether demographic imperative has natural sunset or requires permanent enforcement').

omega_variable(
    alternative_state_models_viability,
    'Were alternative state models (binational state, cantonal federation, equal citizenship democracy) structurally viable, or would they have collapsed into civil war or minority domination?',
    'Comparative analysis of multi-ethnic state formation (Switzerland, Belgium, Lebanon, Yugoslavia); examination of 1940s proposals (Magnes-Buber, Hashomer Hatzair binationalism); assessment of power asymmetries and conflict dynamics',
    'If viable: the demographic imperative was a choice that prioritized ethnic exclusivity over democratic inclusion, strengthening the snare classification. If non-viable: the imperative was a tragic necessity, strengthening the coordination function and tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_state_models_viability, conceptual, 'Whether alternative state models were structurally viable').

omega_variable(
    kernel_reading_foreclosure,
    'Do the national_liberation_reading and settler_colonial_reading foreclose each other within a single analytical framework, or can they coexist as partial descriptions of different structural layers?',
    'Examination of whether movements can be simultaneously indigenous (in deep historical terms) and colonial (in proximate structural terms); analysis of other cases combining return narratives with displacement patterns (Liberia, post-colonial reverse migrations); assessment of whether ''indigenous'' status is time-bounded or permanent',
    'If foreclosed: observers must choose one reading and reject the other, producing incompatible constraint classifications. If coexistent: the readings describe different structural layers (historical claim vs. operational pattern), and the constraint exhibits both coordination and extraction simultaneously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether kernel readings foreclose each other or describe different structural layers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_engineering_imperative, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demog_eng_theater_1947, demographic_engineering_imperative, theater_ratio, 0, 0.25).
narrative_ontology:measurement(demog_eng_theater_1948, demographic_engineering_imperative, theater_ratio, 1, 0.35).
narrative_ontology:measurement(demog_eng_theater_1952, demographic_engineering_imperative, theater_ratio, 5, 0.38).
narrative_ontology:measurement(demog_eng_theater_1967, demographic_engineering_imperative, theater_ratio, 20, 0.4).
narrative_ontology:measurement(demog_eng_theater_1977, demographic_engineering_imperative, theater_ratio, 30, 0.42).
narrative_ontology:measurement(demog_eng_theater_1997, demographic_engineering_imperative, theater_ratio, 50, 0.42).
narrative_ontology:measurement(demog_eng_theater_2025, demographic_engineering_imperative, theater_ratio, 78, 0.42).

% Extraction over time
narrative_ontology:measurement(demog_eng_extract_1947, demographic_engineering_imperative, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(demog_eng_extract_1948, demographic_engineering_imperative, base_extractiveness, 1, 0.82).
narrative_ontology:measurement(demog_eng_extract_1952, demographic_engineering_imperative, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(demog_eng_extract_1967, demographic_engineering_imperative, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(demog_eng_extract_1977, demographic_engineering_imperative, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(demog_eng_extract_1997, demographic_engineering_imperative, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(demog_eng_extract_2025, demographic_engineering_imperative, base_extractiveness, 78, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(demog_eng_suppress_1947, demographic_engineering_imperative, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(demog_eng_suppress_1948, demographic_engineering_imperative, suppression_requirement, 1, 0.88).
narrative_ontology:measurement(demog_eng_suppress_1952, demographic_engineering_imperative, suppression_requirement, 5, 0.82).
narrative_ontology:measurement(demog_eng_suppress_1967, demographic_engineering_imperative, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(demog_eng_suppress_1977, demographic_engineering_imperative, suppression_requirement, 30, 0.83).
narrative_ontology:measurement(demog_eng_suppress_1997, demographic_engineering_imperative, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(demog_eng_suppress_2025, demographic_engineering_imperative, suppression_requirement, 78, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_engineering_imperative, identity_coordination).
narrative_ontology:affects_constraint(demographic_engineering_imperative, law_of_return_asymmetry).
narrative_ontology:affects_constraint(demographic_engineering_imperative, absentee_property_confiscation).
narrative_ontology:affects_constraint(demographic_engineering_imperative, settlement_expansion_imperative).
narrative_ontology:affects_constraint(demographic_engineering_imperative, citizenship_law_ethnic_preference).

% DUAL FORMULATION NOTE:
% The demographic engineering imperative is the upstream structural constraint that necessitates downstream legal and administrative mechanisms. The Law of Return's asymmetry (automatic Jewish citizenship, permanent Palestinian exclusion), the Absentee Property Law (land confiscation from displaced Palestinians), settlement expansion (territorial control and demographic facts), and citizenship law ethnic preferences (maintaining Jewish majority) are all implementations of the demographic imperative. Each downstream constraint has its own extractiveness value reflecting its specific operation, but all derive their structural logic from the upstream demographic requirement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
