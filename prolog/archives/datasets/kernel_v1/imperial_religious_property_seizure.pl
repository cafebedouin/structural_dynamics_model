% ============================================================================
% CONSTRAINT STORY: imperial_religious_property_seizure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_religious_property_seizure, []).

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
 *   constraint_id: imperial_religious_property_seizure
 *   human_readable: Imperial Religious Property Seizure and Dispossession
 *   domain: political_economy/religious_institutional_power
 *
 * SUMMARY:
 *   Imperial religious property seizure represents a foundational extraction
 *   mechanism by which centralizing empires consolidate power by claiming or
 *   redirecting wealth accumulated by competing institutional actors
 *   (religious institutions, temples, monasteries, pilgrimage networks). The
 *   constraint operates through three integrated enforcement mechanisms: (1)
 *   explicit authorization via legal edict declaring seizure lawful under
 *   imperial authority; (2) theological legitimation via doctrinal
 *   justification reframing seizure as protection, rededication, or
 *   purification of sacred wealth; (3) military enforcement via suppression
 *   of organized resistance and occupation of religious sites. The constraint
 *   exhibits extreme perspectival divergence: the empire experiences
 *   coordination (consolidating dispersed authority and wealth), organized
 *   reformers see a temporary problem with institutional solutions
 *   (scaffold), the legitimation apparatus sees its own degradation (piton),
 *   religious institutions experience total extraction (snare), and the
 *   analytical observer risks naturalizing contingent institutional
 *   arrangements as laws of statecraft (false summit mountain). The
 *   extractiveness value (0.78) reflects that religious institutions
 *   accumulated substantial wealth over centuries through pilgrim donations,
 *   land grants, and tax exemptions, and the seizure redirects this wealth
 *   with minimal compensation. The suppression value (0.82) reflects that
 *   resistance is met with military force, institutional dissolution, and
 *   elimination of alternative authority structures. The theater ratio (0.55)
 *   indicates that seizure is justified through elaborate theological and
 *   legal frameworks, but the justifications are increasingly performative by
 *   t=50-100 (institutional inertia and elite rationalization dominate over
 *   effective legitimation).
 *
 * KEY AGENTS:
 *   - Imperial State: Primary beneficiary (institutional/arbitrage) — consolidates dispersed wealth and authority; experiences seizure as coordination mechanism
 *   - Religious Institutions (Monasteries, Temples): Primary victims (powerless/trapped) — no exit options from imperial seizure; suppressed by military force; accumulated wealth taken with no compensation
 *   - Pilgrimage Networks: Secondary victims (moderate/constrained) — coordinating regional worship and commerce; loss of sacred sites and accumulated offerings; can relocate but lose sanctity capital
 *   - Competing Religious Institution: Institutional actor (powerful/mobile) — experiences seizure as mixed threat and coordination tool; may benefit from suppression of rivals; can negotiate doctrinal accommodation
 *   - Imperial Legitimation Apparatus (Clergy, Philosophers, Legal Scholars): Institutional actor maintaining performative justification (institutional/arbitrage) — generates theological and legal cover for seizure; experiences own mechanism as degraded (piton) by t=50+
 *   - Reform Coalition (Popular Movements, Canon Law Reformers): Organized agents (organized/constrained) — coordinates constraints on seizure through institutional protections and property-rights norms; sees sunset condition as institutional consolidation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law of state consolidation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_religious_property_seizure, 0.78).
domain_priors:suppression_score(imperial_religious_property_seizure, 0.82).
domain_priors:theater_ratio(imperial_religious_property_seizure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_religious_property_seizure, extractiveness, 0.78).
narrative_ontology:constraint_metric(imperial_religious_property_seizure, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(imperial_religious_property_seizure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_religious_property_seizure, snare).
narrative_ontology:human_readable(imperial_religious_property_seizure, "Imperial Religious Property Seizure and Dispossession").
narrative_ontology:topic_domain(imperial_religious_property_seizure, "political_economy/religious_institutional_power").

domain_priors:requires_active_enforcement(imperial_religious_property_seizure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_religious_property_seizure, imperial_state).
narrative_ontology:constraint_beneficiary(imperial_religious_property_seizure, imperial_military_apparatus).
narrative_ontology:constraint_victim(imperial_religious_property_seizure, religious_institutions).
narrative_ontology:constraint_victim(imperial_religious_property_seizure, pilgrimage_networks).
narrative_ontology:constraint_victim(imperial_religious_property_seizure, monastic_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MONASTIC COMMUNITY (SNARE) — Religious communities have no exit option from imperial seizure. They cannot relocate institutional wealth without military protection the empire will not provide. They cannot resist openly (suppressed by force). They cannot appeal to alternative authority (the emperor IS the supreme authority). The extraction is total: accumulated wealth over centuries is seized with minimal compensation or negotiation. Experienced as pure coercion with no coordination benefit.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PILGRIMAGE NETWORK — REGIONAL SCOPE (SNARE) — Pilgrimage networks coordinating regional worship and commerce face seizure of sacred sites, shrines, and accumulated offerings. Exit is constrained (can relocate pilgrimage routes but lose accumulated sanctity capital and network density). Suppression is high (imperial forces occupy sacred sites). Theater is moderate (seizure is often framed as 'protection' or 'rededication' to imperial faith). Experienced extraction is severe — the pilgrimage economy is disrupted or redirected to imperial benefit.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMPETING RELIGIOUS INSTITUTION (TANGLED ROPE) — An institutional religious competitor (rival sect, foreign faith, heretical school) experiences the imperial seizure mechanism as both a threat and a coordination tool. The empire uses seizure to suppress competitor institutions while coordinating a unified state religion. A powerful competitor may have some mobility (can negotiate doctrinal accommodation, migrate to peripheral regions, shift institutional identity) and may even benefit from the suppression of rival competitors. Mixed extraction and coordination — the constraint both extracts from and coordinates with this actor.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: IMPERIAL STATE (ROPE) — The centralizing empire experiences the seizure mechanism as pure coordination: consolidating dispersed wealth from competing institutional centers into imperial control, funding military and infrastructure while establishing centralized authority. The empire has exit options (does not perform seizure, decentralizes authority) and exercises them through choice. No extraction is experienced — the empire is the primary beneficiary. Structurally, the empire sees this as necessary coordination for statecraft.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: IMPERIAL LEGITIMATION APPARATUS (PITON) — The theological-legal justification system (imperial clergy, court philosophers, legal scholars declaring the seizure lawful) maintains performative legitimacy long after the seizure mechanism has degraded or lost functional necessity. Theater_ratio is high (elaborate theological justifications, ceremonial reintegration of seized wealth into state-sponsored religious activities, claims that the imperial state 'protects' religion). The legitimation apparatus persists through institutional inertia — maintained because the seizure mechanism requires ongoing moral cover, not because the cover is effective. Degraded piton: the justifications no longer persuade subordinate populations but continue to function as internal coordination for the imperial elite.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: REFORM COALITION (SCAFFOLD) — Organized religious reformers, popular movements, and peripheral institutions coordinate around constraints on imperial seizure: canon law limits, property rights for religious institutions, separation of sacred and imperial treasuries. These movements establish sunset conditions: as property-rights norms crystallize, institutional protections accumulate, and imperial authority diversifies or decentralizes, the seizure mechanism loses structural necessity. This perspective sees the constraint as a temporary coordination failure solvable by institutional reform. Estimated sunset: varies by region (150-300 years for institutional consolidation). Modeled as Scaffold: temporary, with exit visible to organized actors.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the seizure appears as an immutable feature of state consolidation: all centralizing empires require wealth consolidation, all states compete with religious institutions for authority, and property seizure is an inevitable mechanism of centralization. This perspective risks naturalizing what the structural data reveals as a highly contingent institutional extraction mechanism. The analytical observer's mountain classification is a false summit — the constraint appears natural only when measured from a universalizing, power-neutral epistemic frame.
constraint_indexing:constraint_classification(imperial_religious_property_seizure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_religious_property_seizure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imperial_religious_property_seizure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imperial_religious_property_seizure, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_religious_property_seizure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(imperial_religious_property_seizure, TR),
    TR >= 0.70.

:- end_tests(imperial_religious_property_seizure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. Religious institutions accumulate substantial wealth through pilgrim donations, land grants, tax exemptions, and centuries of capital accumulation. The imperial seizure redirects this wealth to imperial control with minimal compensation or negotiation. The extractiveness is not total (0.95+) because the empire often redirects seized wealth to state-sponsored religious activities, maintaining a thin coordination function (centralized religious authority). However, the net extraction to religious institutions is severe — they lose autonomous control of wealth and must operate as imperial subordinates. The measurement trajectory shows rising extractiveness from t=0 to t=50 as the seizure mechanism matures and consolidates, then slight decline at t=100 as reform movements establish institutional constraints. Suppression (0.82): Very high. Resistance to seizure is met with military force, institutional dissolution, and elimination of alternative authority structures. Religious communities have no exit option (trapped). No appeal mechanism exists (emperor is supreme authority). No alternative locus of power can protect religious wealth. Organized resistance is systematically suppressed. However, suppression is not total (0.95+) because religious institutions maintain some autonomy in ritualized functions and some regions retain informal property rights. The measurement trajectory shows rising suppression through t=50 as imperial apparatus matures, then slight decline at t=100 as property-rights norms accumulate institutional protection. Theater ratio (0.55): Moderate-high. Seizure is justified through elaborate theological frameworks (seizure as purification, rededication, protection), legal edicts (emperor as supreme property holder), and ceremonial reintegration of seized wealth into state-sponsored religious activities. The elaborate justifications are performative — they do not persuade subordinate populations but function as internal rationalization for the imperial elite. Theater rises from t=0 to t=100 as legitimation apparatus invests more effort in moral cover. The moderate initial value reflects that early seizures may have been genuinely believed; the rising trajectory reflects increasing performative investment as the mechanism becomes routinized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence across the observation site. The empire (institutional/arbitrage) sees pure coordination and experiences negative extraction (wealth flowing toward them). The monastic community (powerless/trapped) sees pure coercion and experiences maximum extraction (wealth flowing away with no exit). The pilgrimage network (moderate/constrained) sees mixed extraction and coordination (sacred sites coordinate pilgrimage, but imperial seizure disrupts coordination). The reform coalition (organized/constrained) sees a temporary problem with institutional solutions and experiences moderate extraction (institutional reform can constrain seizure). The legitimation apparatus (institutional/arbitrage) sees its own degradation (piton) and experiences arbitrage (generates wealth through moral justification labor). The analytical observer (analytical/analytical) risks seeing immutable natural law (mountain) when the structural data reveals contingent institutional extraction. The perspectival gap is largest between the empire and the monastic community: the same mechanism is experienced as coordination by the beneficiary and pure extraction by the victim. The gap arises because beneficiaries have exit options (can choose not to seize) while victims have none (cannot choose not to be seized).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position, power level, and exit options. The empire (institutional/arbitrage) experiences low or negative d — beneficiary with full arbitrage options. Derived d ≈ 0.08, f(d) ≈ -0.08. Negative chi indicates wealth flowing toward the empire. The monastic community (powerless/trapped) experiences high d — victim with no exit. Derived d ≈ 0.95, f(d) ≈ 1.42. High chi indicates maximum extraction. The pilgrimage network (moderate/constrained) experiences moderate-high d ≈ 0.72, f(d) ≈ 1.12, reflecting costs constrained but not total. The competing institution (powerful/mobile) experiences d ≈ 0.48, f(d) ≈ 0.60, reflecting some agency and some benefit from competitor suppression. The reform coalition (organized/constrained) experiences d ≈ 0.55, f(d) ≈ 0.75, reflecting ability to negotiate constraints. The scope modifier σ(S) = 1.1 (continental scope amplifies extractiveness slightly). Final chi = ε × f(d) × σ(S) varies by perspective: empire χ ≈ -0.07 (coordination), monastic community χ ≈ 1.22 (pure snare), pilgrimage network χ ≈ 0.97 (snare), reform coalition χ ≈ 0.64 (scaffold to tangled_rope range).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by showing that the constraint's classification derives from the structural relationship between the agent and the seizure mechanism, not from ambiguity in the mechanism itself. The empire's rope classification is not coordination at the expense of monastic snare — they are different constraints measured from different positions. From the imperial position, the seizure mechanism solves a real coordination problem (consolidating dispersed authority). From the monastic position, the same mechanism is pure extraction with no coordination function. The mandatrophy is not 'is this snare or rope?' but 'whose structural position determines the answer?' The answer is both, simultaneously — the constraint is a presheaf over the observation site. The snare classification for monastic communities and the rope classification for the empire are not competing answers but complementary perspectives on the same structural reality. The false summit detection on the analytical perspective confirms this: what appears as a natural law of statecraft (all empires seize religious property) is actually a contingent institutional extraction mechanism visible as such only from within specific structural positions (the victims', not the beneficiaries').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seizure_legitimation_efficacy,
    'Does the theological/legal justification for imperial seizure actually persuade the affected populations, or does it function as internal rationalization for the imperial elite?',
    'Historical analysis of resistance narratives, theological counter-arguments by religious communities, and population adherence to alternate legitimacy claims (anti-imperial prophets, heterodox sects, alternative authority structures)',
    'If legitimation persuades subordinates: the constraint includes genuine coordination (justification coordination across diverse populations). If it functions as internal rationalization: the constraint is pure extraction with performative overlay (higher snare classification). Classification shifts from snare/tangled_rope to pure snare or piton (if theater is high).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(seizure_legitimation_efficacy, empirical, 'Whether theological/legal justification for seizure persuades populations or functions as elite rationalization').

omega_variable(
    institutional_wealth_accumulation_necessity,
    'Is the wealth accumulated by religious institutions necessary for state consolidation, or does imperial seizure extract surplus beyond statecraft funding requirements?',
    'Comparison of seized wealth to imperial military and infrastructure expenditures; analysis of wealth redistribution patterns; determination of whether seizure funds statecraft or funds imperial luxury/expansion beyond consolidation needs',
    'If seizure wealth is necessary for statecraft: snare classification is correct (non-negotiable extraction). If seizure extracts surplus: the constraint is partially speculative (empire takes more than needed), raising questions about whether lesser extraction could achieve the same coordination outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_wealth_accumulation_necessity, empirical, 'Whether seized wealth is necessary for statecraft or extracted as surplus').

omega_variable(
    alternative_consolidation_mechanisms,
    'Could imperial consolidation achieve the same functional outcome through alternative mechanisms (taxation, voluntary integration, institutional accommodation) without direct property seizure?',
    'Historical comparison of empires using seizure vs. empires using alternative consolidation mechanisms; analysis of consolidation timelines and stability outcomes; examination of whether seizure accelerates consolidation or merely extracts wealth from existing consolidation',
    'If alternatives exist and produce equivalent outcomes: seizure is not structurally necessary (pure extraction mechanism, not coordination mechanism). Snare classification confirmed. If seizure produces faster consolidation: mechanism has hybrid coordination function (tangled_rope classification more accurate). If seizure produces more stable consolidation: mechanism has genuine coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_consolidation_mechanisms, empirical, 'Whether imperial consolidation requires property seizure or can achieve same outcome through alternatives').

omega_variable(
    religious_institutional_countervailing_power,
    'Did religious institutions possess sufficient countervailing power to resist seizure through doctrine, organization, or alternative legitimacy claims?',
    'Historical analysis of religious institutional organization, doctrinal capacity for resistance narrative (martyrdom theology, prophecy against empire, theological frameworks justifying non-compliance), and instances of successful resistance or negotiation',
    'If religious institutions possessed countervailing power: the trap is not total — suppression ≤ 0.82 (some institutional agency possible). If institutions were organizationally fragmented: suppression is near-total (0.82-0.95). Affects the trapped/constrained distinction for religious victims. Also affects piton classification if institutional resistance was systematically suppressed — the piton would represent degradation of what was once a functional countervailing institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_institutional_countervailing_power, empirical, 'Whether religious institutions possessed countervailing power to resist or negotiate seizure').

omega_variable(
    false_summit_natural_law_illusion,
    'Is the mountain classification from the analytical perspective a genuine natural law of state consolidation, or a false summit naturalizing a contingent institutional extraction mechanism?',
    'Cross-empire historical analysis: do all centralizing empires seize religious property, or only those with specific institutional configurations (competitive religions, decentralized pre-imperial authority, absence of property-rights traditions)? Counterfactual analysis of empires achieving consolidation without seizure.',
    'If seizure is universal to state consolidation: mountain classification is justified (natural law of statecraft). If seizure is contingent on institutional context: the mountain is a false summit (engine-detected). This omega documents the key analytical uncertainty: whether the constraint appears natural because of its universal necessity or because of the universalizing epistemic frame of the analytical observer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_illusion, conceptual, 'Whether the apparent natural law of imperial seizure reflects structural necessity or observer epistemic frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_religious_property_seizure, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irps_tr_t0, imperial_religious_property_seizure, theater_ratio, 0, 0.38).
narrative_ontology:measurement(irps_tr_t20, imperial_religious_property_seizure, theater_ratio, 20, 0.48).
narrative_ontology:measurement(irps_tr_t50, imperial_religious_property_seizure, theater_ratio, 50, 0.55).
narrative_ontology:measurement(irps_tr_t100, imperial_religious_property_seizure, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(irps_be_t0, imperial_religious_property_seizure, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(irps_be_t20, imperial_religious_property_seizure, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(irps_be_t50, imperial_religious_property_seizure, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(irps_be_t100, imperial_religious_property_seizure, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(irps_su_t0, imperial_religious_property_seizure, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(irps_su_t20, imperial_religious_property_seizure, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(irps_su_t50, imperial_religious_property_seizure, suppression_requirement, 50, 0.82).
narrative_ontology:measurement(irps_su_t100, imperial_religious_property_seizure, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_religious_property_seizure, resource_allocation).
narrative_ontology:affects_constraint(imperial_religious_property_seizure, religious_institutional_autonomy_erosion).
narrative_ontology:affects_constraint(imperial_religious_property_seizure, state_legitimacy_through_religious_capture).
narrative_ontology:affects_constraint(imperial_religious_property_seizure, pilgrimage_economy_disruption).

% DUAL FORMULATION NOTE:
% Imperial religious property seizure is the primary extraction mechanism; it upstream-affects the erosion of religious institutional autonomy and downstream-affects the disruption of pilgrimage economies and the capture of religious authority for state legitimacy claims. Each affected constraint has its own ε value and perspectival structure. The seizure mechanism is the trunk; institutional autonomy erosion and legitimacy capture are branches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_religious_property_seizure, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
