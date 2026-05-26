% ============================================================================
% CONSTRAINT STORY: freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_freedom_of_movement_reading, []).

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
 *   constraint_id: freedom_of_movement_reading
 *   human_readable: Border Restrictions as Violation of Freedom of Movement (Cosmopolitan Reading)
 *   domain: political_philosophy/migration_law/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates the freedom-of-movement reading of the
 *   border_legitimacy kernel. From this reading, border restrictions are
 *   treated as extractive snares that violate the presumptive right to global
 *   mobility. The cosmopolitan normative claim (freedom of movement is a
 *   human right; borders are presumptively illegitimate restrictions)
 *   generates ε=0.58 by foregrounding how border enforcement extracts from
 *   the global poor, traps workers in low-wage labor markets, and suppresses
 *   alternative allocation mechanisms that would equalize opportunity. The
 *   constraint exhibits all six types from different perspectives, revealing
 *   deep disagreement about whether borders coordinate state capacity (rope
 *   from wealthy governments) or extract from those unable to cross them
 *   (snare from powerless perspectives). The theater ratio (0.55) reflects
 *   that border restriction operates partly through performative humanitarian
 *   exceptions (asylum processing, refugee quotas, diplomatic agreements)
 *   that create appearance of justice while preserving underlying extraction.
 *   The measuring point shows extraction accumulation over 20 years: base
 *   extractiveness rises from 0.42 to 0.58 as enforcement intensifies (wall
 *   construction, biometric tracking, maritime interdiction, offshore
 *   processing) and humanitarian exceptions narrow (smaller refugee caps,
 *   harder asylum criteria, longer processing delays). Theater also increases
 *   as international institutions develop more elaborate adjudication
 *   procedures (refugee determination, family reunification review) that
 *   theatrically soften extraction without reducing the fundamental
 *   immobility.
 *
 * KEY AGENTS:
 *   - Global Poor: Primary victims (powerless/trapped) — face lethal barriers to cross-border movement; no arbitrage capacity; experience maximum extraction and suppression
 *   - Asylum Seekers and Refugees: Primary victims (powerless/trapped) — extraordinary suppression (detention, deportation risk, offshore processing); legal status uncertainty deepens trap
 *   - Displaced Workers in Wealthy States: Secondary victims (moderate/trapped) — experience wage compression and labor market deterioration from restricted global mobility; citizenship locks them in declining labor market position
 *   - Wealthy Origin States / National Governments: Primary beneficiaries (institutional/arbitrage) — capture labor market rents, maintain wage premiums for citizens, control tax bases, manage fiscal welfare systems; can arbitrage bilateral labor agreements and diaspora policies
 *   - Protected Labor Markets / High-Skill Worker Coalitions: Beneficiaries (moderate/arbitrage) — maintain wage premiums and job security through restriction; skilled workers retain mobility via visa sponsorship while low-skill workers remain trapped
 *   - Multinational Corporations: Beneficiary-constrained hybrid (institutional/constrained) — benefit from global wage differentials enabled by border restriction; some arbitrage capacity through offshoring and visa sponsorship but constrained by border enforcement in some markets
 *   - Labor Unions and Low-Skill Coalitions: Ambivalent constrained (organized/constrained) — benefit from wage protection via restriction (coordination function) but structurally extract from potential global membership; politically constrained from openly opposing border closure
 *   - International Migration Institutions (IOM, UNHCR, UN agencies): Theater operators (institutional/constrained) — maintain procedural legitimacy through humanitarian processing while underlying border extraction mechanism remains fixed; theater ratio high, functional capacity to challenge borders low
 *   - Analytical Observer: False summit position (analytical/analytical) — risks naturalizing state-necessity framing as immutable law when it is contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(freedom_of_movement_reading, 0.58).
domain_priors:suppression_score(freedom_of_movement_reading, 0.68).
domain_priors:theater_ratio(freedom_of_movement_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(freedom_of_movement_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(freedom_of_movement_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(freedom_of_movement_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(freedom_of_movement_reading, snare).
narrative_ontology:human_readable(freedom_of_movement_reading, "Border Restrictions as Violation of Freedom of Movement (Cosmopolitan Reading)").
narrative_ontology:topic_domain(freedom_of_movement_reading, "political_philosophy/migration_law/human_rights").

domain_priors:requires_active_enforcement(freedom_of_movement_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(freedom_of_movement_reading, wealthy_origin_states).
narrative_ontology:constraint_beneficiary(freedom_of_movement_reading, protected_labor_markets).
narrative_ontology:constraint_victim(freedom_of_movement_reading, global_poor).
narrative_ontology:constraint_victim(freedom_of_movement_reading, displaced_workers).
narrative_ontology:constraint_victim(freedom_of_movement_reading, asylum_seekers).
narrative_ontology:constraint_victim(freedom_of_movement_reading, welfare_recipients_in_destination_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL POOR — Structurally immobile across borders; face lethal barriers (drowning, deportation, detention) to exit poverty traps. No arbitrage option, no mobility. Maximum extraction and suppression experienced as absolute constraint on life trajectory.
constraint_indexing:constraint_classification(freedom_of_movement_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED WORKERS IN DESTINATION STATES — Citizens in wealthy nations whose local labor market conditions deteriorate when immigration is restricted; wages compressed, job competition concentrated. Trapped by citizenship in a state that restricts their access to global labor markets. Experience border restrictions as extraction of their wage premium and mobility.
constraint_indexing:constraint_classification(freedom_of_movement_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: GENERATIONAL WELFARE RECIPIENTS — Children born into poverty in restrictive states; intergenerational trap deepens across biographical time. Border closure becomes hereditary constraint. No arbitrage, mobility constrained by accident of birth. Maximum experienced extraction.
constraint_indexing:constraint_classification(freedom_of_movement_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED LABOR (AMBIVALENT) — Unions in wealthy states benefit from wage protection via restrictive immigration (coordination function: wage floors, job security). Simultaneously, border restrictions extract from their potential membership base (global workers unable to organize). Constrained by political capital costs of opposing 'border security' framing despite structural interest in global labor mobility.
constraint_indexing:constraint_classification(freedom_of_movement_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WEALTHY ORIGIN STATE GOVERNMENTS (ROPE) — Experience border restrictions as pure coordination mechanism: border enforcement sorts global population by origin state, enabling state capacity for social insurance, fiscal collection, and welfare targeting. Extraction runs toward these states — they benefit from wage suppression outside their borders and labor supply concentration within. Arbitrage available: can negotiate bilateral labor agreements, guest worker programs, diaspora policies.
constraint_indexing:constraint_classification(freedom_of_movement_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CORPORATIONS (QUALIFIED MOBILITY) — Multinational firms benefit from border restrictions that suppress global wages while maintaining some arbitrage capacity (offshoring, H-1B sponsorship, executive mobility). Experience the constraint as enabling labor extraction via geographic wage differentials while maintaining executive-class mobility. Arbitrage available through corporate relocation, visa sponsorship, trade agreements.
constraint_indexing:constraint_classification(freedom_of_movement_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL MIGRATION INSTITUTIONS (PITON) — UN agencies, IOM, regional bodies perform verification of humanitarian claims, process asylum, coordinate border data. The institutional apparatus persists through procedural theater (country quotas, processing queues, humanitarian screening) that creates appearance of fairness while underlying extraction mechanism (border closure) remains fixed. Theater ratio high: extensive refugee determination process, humanitarian exception processing, family reunification procedures. Function degraded: institutions cannot challenge border premise itself.
constraint_indexing:constraint_classification(freedom_of_movement_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STATE CAPACITY VIEW (FALSE SUMMIT) — From civilizational perspective, border-state structures are modeled as natural law: state capacity to maintain fiscal systems requires population control; welfare systems require border closure; social insurance requires homogeneous tax bases. This naturalizes what the freedom-of-movement reading reveals as contingent institutional choice: state capacity COULD operate on principles of universal membership and progressive extraction. The mountain classification is a false summit — the engine will detect beneficiary presence and reclassify.
constraint_indexing:constraint_classification(freedom_of_movement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(freedom_of_movement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(freedom_of_movement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(freedom_of_movement_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(freedom_of_movement_reading, TR),
    TR >= 0.70.

:- end_tests(freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Border restrictions extract from the global poor primarily through suppressed wages and immobility, secondarily through welfare recipients in wealthy states whose labor market position deteriorates when global mobility is restricted. The extraction accumulates over time as enforcement intensifies (wall construction, biometric systems, offshore processing create cumulative suppression). The value reflects that restriction is the primary mechanism — removing borders would restructure global labor markets substantially, benefiting migrants and (at aggregate level) reducing inequality, while harming protected labor markets. Suppression (0.68): High. Physical barriers, legal prohibition, deportation threat, detention, maritime interdiction, and processing delays combine to create near-total immobility for the poor. Suppression is not symmetric across agents — wealthy individuals cross borders easily while poor face lethal barriers. The value reflects suppression intensity for the primary victim set (global poor and asylum seekers). Theater ratio (0.55): Moderate. Border restriction operates through both enforcement (tangible barriers) and humanitarian exceptions (processing procedures, refugee determination, family reunification). The theater increased over time as states developed elaborate asylum systems that create appearance of justice while actual refugee intake caps remain restrictive. International institutions contribute theater by providing legitimating procedures. Theater is significant but not dominant — the enforcement mechanism is real, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces maximal perspectival disagreement. The global poor and asylum seekers see pure extraction (Snare) — they experience only suppression and no coordination benefit. Wealthy governments and protected labor markets see coordination (Rope) — they experience border restriction as enabling state fiscal capacity and wage protection. Labor unions see mixed dynamics (Tangled Rope) — they benefit from wage protection (coordination) while being extracted from via the excluded global labor pool. International migration institutions see their own theatrical degradation (Piton) — they run elaborate humanitarian procedures while the underlying extraction mechanism persists. The analytical observer risks false summit thinking (Mountain) by naturalizing state capacity as immutable when cosmopolitan reading shows it as reconstructible on universal membership principles. The perspectival gap is not merely about data interpretation — it reflects genuine conflicts of interest: reducing extraction for the global poor requires reducing coordination benefits for wealthy states.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from structural position in the extraction flow. The global poor are maximum victims with no exit (d≈0.95, trapped exit → high f(d) ≈1.42). Wealthy governments are beneficiaries with arbitrage capacity (d≈0.05, institutional/arbitrage → low f(d) ≈-0.12). Protected labor markets in wealthy states are moderate beneficiaries constrained from claiming their interest openly (d≈0.20, powerful but politically constrained → f(d)≈0.02). Asylum seekers face extreme suppression layers (d≈0.98, trapped + legal status vulnerability → f(d) ≈1.48). Labor unions are ambivalent: they benefit from restriction (coordination) but are structurally constrained from organizing globally (d≈0.55 from constrained/organized hybrid, capturing the tension). Each perspective's classification depends on its exit options and beneficiary/victim status: powerless trapped victims see snare (maximum experienced χ); wealthy governments see rope (coordination function); organized labor sees tangled rope (mixed). The piton classification for international institutions reflects that their function (humanitarian adjudication) has atrophied relative to their theater (elaborate procedures).
 *
 * MANDATROPHY ANALYSIS:
 *   The cosmopolitan reading resolves the mandatrophy by showing that border restrictions serve both coordination (state fiscal capacity, welfare targeting, national identity) and extraction (suppressed wages, immobility, concentrated opportunity). The snare classification from powerless perspectives is not a mislabeling of coordination as extraction — it is a recognition that from the perspective of those trapped outside, the constraint is purely extractive regardless of its coordination function elsewhere. The rope classification from wealthy government perspectives is not a mislabeling of extraction as coordination — it is a recognition that from the perspective of state fiscal capacity, borders do provide genuine coordination for public goods provision. The mandatrophy is resolved by indexing: the same constraint is snare from (powerless, trapped) and rope from (institutional, arbitrage). This is not a contradiction — it is the presheaf structure of how constraints appear from different positions. The false summit risk (analytical observer naturalizing state-necessity framing) is documented in omega variables and mitigated by the beneficiary declaration, which triggers FSM detection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint instantiates the freedom-of-movement reading of the border_legitimacy kernel. What are the sibling readings and how do they produce different ε values?',
    'Comparative structural analysis of the sovereignty_reading and humanitarian_obligation_reading as separate constraints. Each reading generates its own ε, own beneficiary/victim set, own type classification.',
    'The freedom-of-movement reading claims ε=0.58 (Snare from powerless perspectives) by making border enforcement extractive and treating mobility restriction as the primary cost vector. The sovereignty_reading would claim lower ε by emphasizing state capacity coordination function. The humanitarian_reading would claim ε between them by acknowledging both extraction and humanitarian exceptions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Kernel decomposition: this is one reading of border_legitimacy, not the complete constraint space').

omega_variable(
    global_poor_demographic_variation,
    'Does the ε=0.58 value apply uniformly to the global poor, or does extraction intensity vary by origin region, conflict zone, climate vulnerability?',
    'Decompose global poor into stratified victim sets: climate migrants (rising ε as displacement increases), conflict-zone populations (high ε, extreme suppression), economic migrants (moderate ε, higher arbitrage capacity). Measure extraction intensity by region.',
    'If variation exceeds 0.25 across regions: decompose into separate constraint stories per region. If uniform: current single ε is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_poor_demographic_variation, empirical, 'Whether global poor extraction is homogeneous across origin regions').

omega_variable(
    welfare_recipient_causal_direction,
    'Do welfare recipients in wealthy states experience deterioration because of immigration restriction (restriction extracts their potential), or because immigration restriction is CAUSED by welfare retrenchment politics (causality runs opposite direction)?',
    'Time-series analysis: compare wage trajectories in high-restriction vs low-restriction jurisdictions, controlling for welfare expenditure and labor market structure. Test whether restriction predicts welfare outcomes or welfare retrenchment predicts restriction adoption.',
    'If restriction → deterioration: welfare recipients are direct victims (current story). If welfare retrenchment → restriction: they are indirect victims (restriction is cover story, actual extraction vector is welfare cuts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_recipient_causal_direction, empirical, 'Causal direction of border restriction → welfare recipient harm').

omega_variable(
    arbitrage_capacity_corporate_qualification,
    'How much actual arbitrage capacity do corporations retain? Do visa sponsorship and offshoring genuinely free them from border constraints or are they also trapped (just at different scale)?',
    'Comparative analysis of corporate mobility across different capital intensities and skill requirements. High-capital firms (tech, finance): genuine arbitrage. Low-capital firms (manufacturing, agriculture): constrained or trapped.',
    'If genuine arbitrage: rope classification for institutional perspective is correct. If constrained: institutional power should shift to ''constrained'', classification to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitrage_capacity_corporate_qualification, empirical, 'Corporate arbitrage capacity under border restriction').

omega_variable(
    false_summit_detection_trigger,
    'Is the mountain classification a genuine natural law view, or does it mask extractive benefits for wealthy states?',
    'FSM trigger: beneficiaries declared (wealthy_origin_states, protected_labor_markets). Omega documents whether state-necessity framing naturalizes contingent institutional choice. If yes: FSM reclassification engine applies.',
    'If false summit confirmed: state capacity is reconstructible on universal membership principles; the mountain is an artifact of nationalist framing. Classification shifts to snare or tangled_rope depending on whether coordination function is retained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection_trigger, conceptual, 'Whether state-necessity framing is false summit or genuine natural law').

omega_variable(
    asylum_exception_structure,
    'Does the asylum humanitarian exception (non-refoulement, refugee status) constitute genuine mitigation of snare dynamics or elaborate theater that preserves extraction while appearing to soften it?',
    'Empirical analysis of asylum acceptance rates, refugee resettlement caps, boat turnback policies, detention durations. Compare theoretical rights to actual flows. Measure suppression within asylum processing itself.',
    'If exception substantial: suppression value overstated, lower ε justified. If theater: suppression understated, ε should increase toward 0.65-0.70.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asylum_exception_structure, empirical, 'Whether asylum exception mitigates or theatrically preserves extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(freedom_of_movement_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(free_tr_t0, freedom_of_movement_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(free_tr_t10, freedom_of_movement_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(free_tr_t20, freedom_of_movement_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(free_be_t0, freedom_of_movement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(free_be_t10, freedom_of_movement_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(free_be_t20, freedom_of_movement_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(freedom_of_movement_reading, sovereignty_reading).
narrative_ontology:affects_constraint(freedom_of_movement_reading, humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_legitimacy kernel. The freedom_of_movement_reading (this file) claims ε=0.58 and treats extraction as primary, with coordination as secondary effect. The sovereignty_reading treats coordination as primary and extraction as unintended consequence. The humanitarian_obligation_reading balances them. All three are readings of the same kernel commitment — the legitimacy of border authority — but generate different constraint stories with different ε, different beneficiary/victim sets, different classifications. Each reading is a separate JSON file linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(freedom_of_movement_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
