% ============================================================================
% CONSTRAINT STORY: ulysses_chp10
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_rocks_1904, []).

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
 *   constraint_id: ulysses_chp10
 *   human_readable: The Dublin Simultaneity (Wandering Rocks)
 *   domain: social/political/religious
 *
 * SUMMARY:
 *   Dublin in June 1904 (the date of Ulysses) operates as a complex system
 *   where colonial administrative apparatus, Catholic Church hierarchy, and
 *   established business classes coordinate and extract through the
 *   constraint of spatial simultaneity. The 'Wandering Rocks' chapter models
 *   this through eighteen parallel narratives of Dublin residents moving
 *   through the city's streets during a single afternoon — the constraint of
 *   shared geography forces continuous near-misses and coordinated presence.
 *   From the colonial administration's perspective, this simultaneity enables
 *   legible surveillance and efficient tax/revenue collection. From the
 *   Church's perspective, it structures moral oversight and parish
 *   coordination. From established merchants' perspective, it coordinates
 *   customer flows and social reproduction. But for working-poor residents,
 *   their daily movements are trapped within this same geography, rendering
 *   them visible to enforcement while offering no exit. Nationalist
 *   intellectuals experience the constraint as both coordination signal
 *   (shared grievance creates intellectual community) and extraction
 *   mechanism (censorship, economic exclusion). The theater ratio (0.58)
 *   reflects the gap between the functional coordination narrative (efficient
 *   city management) and the performative maintenance of enforcement rituals
 *   (surveillance as entertainment, bureaucracy as ceremony).
 *
 * KEY AGENTS:
 *   - Working Poor: Primary victim (powerless/trapped) — bears visibility cost of simultaneity with no exit option
 *   - Colonial Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — extracts through legible surveillance and efficient control
 *   - Catholic Church Hierarchy: Secondary beneficiary (institutional/constrained) — coordinates and extracts through moral surveillance; constrained by Protestant competition
 *   - Nationalist Intellectuals: Secondary victim (moderate/constrained) — experience constraint as both coordination and extraction; organize resistance through shared grievance
 *   - Established Business Classes: Tertiary actor (institutional/arbitrage) — benefit from coordination but see function degraded through commercial displacement (piton status)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing colonial urban geometry as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp10, 0.35).
domain_priors:suppression_score(ulysses_chp10, 0.42).
domain_priors:theater_ratio(ulysses_chp10, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp10, extractiveness, 0.35).
narrative_ontology:constraint_metric(ulysses_chp10, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ulysses_chp10, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp10, tangled_rope).
narrative_ontology:human_readable(ulysses_chp10, "The Dublin Simultaneity (Wandering Rocks)").
narrative_ontology:topic_domain(ulysses_chp10, "social/political/religious").

domain_priors:requires_active_enforcement(ulysses_chp10).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp10, colonial_administrative_apparatus).
narrative_ontology:constraint_beneficiary(ulysses_chp10, church_hierarchy).
narrative_ontology:constraint_beneficiary(ulysses_chp10, established_business_classes).
narrative_ontology:constraint_victim(ulysses_chp10, working_poor).
narrative_ontology:constraint_victim(ulysses_chp10, nationalist_intellectuals).
narrative_ontology:constraint_victim(ulysses_chp10, women_without_property).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING POOR (SNARE) — Trapped within Dublin's spatial and economic geography. Cannot exit the system of daily movements constrained by employment precarity, religious obligation, and family debt. Maximum experienced extraction — their labor flows through colonial administrative and clerical control systems with no pathway out.
constraint_indexing:constraint_classification(ulysses_chp10, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: NATIONALIST INTELLECTUALS (TANGLED ROPE) — Constrained by English linguistic-cultural dominance and limited publishing outlets, but also benefit from the constraint as a coordination signal: shared grievance structures the intellectual community. Coordinated resistance through literary production, journalism, political organization. Mixed extraction and coordination — significant coercive overhead (censorship, surveillance, economic exclusion from institutional positions) but also genuine collective action benefit.
constraint_indexing:constraint_classification(ulysses_chp10, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COLONIAL ADMINISTRATION (ROPE) — Experiences the constraint as pure coordination: Dublin's spatial simultaneity enables efficient administration and revenue collection. Movement patterns are legible, predictable, controllable. The system coordinates dispersed enforcement (police, postal service, customs) through shared Dublin geography. Net beneficiary with arbitrage options — can withdraw administrative resources or shift enforcement priorities.
constraint_indexing:constraint_classification(ulysses_chp10, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CATHOLIC CHURCH HIERARCHY (TANGLED ROPE) — Coordinates parish social control and moral surveillance through Dublin's spatial arrangement (church locations, confession schedule, marriage/baptism records). Also extracts: spiritual authority is leveraged for financial contributions, sexual conduct regulation, political allegiance. Active enforcement (confession as surveillance, excommunication threat, marriage canon law). Constrained by Protestant/secular competition for legitimacy and education. Mixed coordination-extraction with genuine enforcement burden.
constraint_indexing:constraint_classification(ulysses_chp10, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ESTABLISHED BUSINESS CLASSES (PITON) — The constraint of Dublin simultaneity once provided genuine coordination for market efficiency (predictable customer flows, staffing, inventory). Now largely degraded: modern commercial districts are displacing the integrated neighborhood economy. Persist through inertia and community habit rather than functional necessity. Theater ratio high — maintaining traditional opening hours, neighborhood gathering spaces, credit relationships despite superimposed English commercial rationalization. Extraction mechanisms persist but primary function has atrophied.
constraint_indexing:constraint_classification(ulysses_chp10, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Dublin's geography (river position, street grid, bridge locations) creates immutable spatial constraints on simultaneous presence. The city's layout inevitably structures who can be where at what time — this appears as natural/physical law. However, the structural data reveals this as false summit: the constraint is not the city's geometry but the institutional control systems (colonialism, church, property law) that render that geometry extractive. The mountain classification naturalizes what is actually a contingent social arrangement.
constraint_indexing:constraint_classification(ulysses_chp10, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp10_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp10, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp10, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp10, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp10_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts through three channels: (1) labor visibility enabling wage suppression and control, (2) ecclesiastical surveillance enabling moral-sexual extraction, (3) administrative overhead enabling tax collection and resource flow reversal. However, the extraction is not total — working-poor residents benefit from neighborhood coordination, social networks, and informal mutual aid embedded in the simultaneity structure. The value reflects balanced mix of coordination benefit and extraction cost. Suppression (0.42): Moderate. Significant barriers to exit include lack of capital for emigration, family ties, religious ties, employment precarity requiring daily presence, English cultural-linguistic dominance limiting options outside Dublin. But suppression is not total — emigration is possible though difficult, rural return is option though costly, organizational capacity exists (union activity, nationalist cells). Theater ratio (0.58): Moderate-high. The simultaneity constraint has developed substantial performative overlay: the ritual of public presence (attending Mass, shopping at familiar establishments, walking recognized routes) has become theater alongside its functional coordination role. Modern transportation and retail are beginning to displace traditional simultaneity patterns, making maintained routines increasingly theatrical. The theatrical component has grown over the interval as English commercial rationalization competes with traditional Dublin neighborhood ecology.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits fundamental perspectival divergence. The colonial administration sees a coordination mechanism enabling efficient control. The Church sees a coordination mechanism enabling moral oversight. Established merchants see a coordination mechanism enabling commerce. Nationalist intellectuals see extraction wrapped in coordination language — the system that coordinates their oppression is presented as natural/functional. Working-poor residents experience pure extraction: they are made visible for control without corresponding benefit. The analytical observer risks the false summit: naturalizing the urban geometry as the constraint when the actual constraint is the institutional systems (colonialism, patriarchy, class structure) that render that geography extractive. The gap between beneficiary (administration) and victim (working poor) perspectives is maximal: ~0.85 chi difference estimated from d values (administration d ≈ 0.05, working poor d ≈ 0.95).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position and exit options. Colonial administration (institutional/arbitrage) achieves low d (~0.05): beneficiary status + full exit options = negative f(d) → low or negative chi. Working poor (powerless/trapped) achieve high d (~0.95): victim status + zero exit options = high f(d) → high chi experienced extraction. Nationalist intellectuals (moderate/constrained) achieve medium d (~0.55): victim status + partial exit (emigration possible but costly) + benefit from coordination = medium f(d) → moderate experienced extraction. Church hierarchy (institutional/constrained) achieve medium d (~0.45): beneficiary status + constrained exit (Protestant competition, secular pressure) + extraction mechanism active = slightly below-neutral f(d) → moderate chi. The derivation chain priority: structural data (beneficiary/victim from institutional position + power + exit capacity) → canonical d values produce the observed perspectival gap.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simultaneity_vs_synchronization,
    'Is the Dublin simultaneity a natural coordination problem (many agents needing to synchronize movements) or an enforcement mechanism (system designed to make movements visible and controllable)?',
    'Historical analysis of pre-colonial Dublin street patterns vs English urban planning interventions; comparison with other colonized cities'' simultaneity structures; archival evidence of deliberate surveillance architecture vs organic growth',
    'If natural coordination: classification shifts toward Rope and Scaffold from all perspectives. If enforcement mechanism: Snare classification strengthens for powerless agents, reveals tangled_rope as coordination cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simultaneity_vs_synchronization, empirical, 'Whether simultaneity arises from coordination needs or surveillance design').

omega_variable(
    church_moral_authority_independence,
    'Does the Catholic Church''s moral authority in Dublin depend on the colonial constraint, or would it exist independently?',
    'Comparative analysis with Catholic Church power in independent Ireland (post-1922); examination of church leverage during periods of reduced colonial oversight; sociological assessment of moral authority sources (doctrine vs institutional position vs extraction mechanism)',
    'If dependent on colonialism: church perspective is contingent tangled_rope, authority degrades post-independence. If independent: church represents genuine coordination function, classification maintains across scenarios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_moral_authority_independence, conceptual, 'Whether church authority is dependent on colonial structure').

omega_variable(
    exit_alternative_geography,
    'Could the working poor exit Dublin''s constraint system through emigration or rural return, or are those alternatives structurally blocked?',
    'Historical data on emigration patterns, remittance flows, rural opportunity costs, passage costs, family separation barriers, visa/passport requirements; interviews with historical records of migration decisions',
    'If exit available: working poor classification shifts from trapped to constrained, Snare degrades to Tangled Rope. If structurally blocked: trapped status confirmed, Snare classification solidified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_alternative_geography, empirical, 'Whether working poor have viable exit through emigration').

omega_variable(
    literary_representation_as_extraction,
    'Does Joyce''s novel representation of Dublin simultaneity constitute further extraction of Dublin''s constraint structure (commodifying working-class movement patterns) or resistance documentation (making invisible constraint visible)?',
    'Analysis of textual representation (does novel center working poor agency or depict them as mechanism parts?); reception history (did Dublin readers recognize themselves in representation?); economic flows (who profited from novel publication and readership)',
    'If extraction: novel represents piton degradation (theater of literary representation replacing actual resistance). If documentation: novel represents analytical perspective, enables later political mobilization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literary_representation_as_extraction, preference, 'Whether literary representation constitutes extraction or documentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp10, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp10, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ulys_tr_t3, ulysses_chp10, theater_ratio, 3, 0.47).
narrative_ontology:measurement(ulys_tr_t6, ulysses_chp10, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp10, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ulys_be_t3, ulysses_chp10, base_extractiveness, 3, 0.29).
narrative_ontology:measurement(ulys_be_t6, ulysses_chp10, base_extractiveness, 6, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp10, resource_allocation).
narrative_ontology:affects_constraint(ulysses_chp10, irish_language_suppression).
narrative_ontology:affects_constraint(ulysses_chp10, colonial_tithe_extraction).
narrative_ontology:affects_constraint(ulysses_chp10, patriarchal_marriage_contract).

% DUAL FORMULATION NOTE:
% The Dublin simultaneity is downstream of structural colonialism and Church institutional power but represents a distinct constraint on the coordination mechanisms available to residents. The upstream constraints (colonialism, church authority) have their own extractiveness values reflecting imperial and ecclesiastical extraction; the simultaneity constraint has its own extractiveness reflecting the spatial-temporal coordination overlay that makes broader extraction legible and enforceable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp10, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
