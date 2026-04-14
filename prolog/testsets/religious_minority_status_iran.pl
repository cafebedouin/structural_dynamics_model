% ============================================================================
% CONSTRAINT STORY: religious_minority_status_iran
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_religious_minority_status_iran, []).

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
 *   constraint_id: religious_minority_status_iran
 *   human_readable: Religious Minority Status in Iran
 *   domain: social/political/legal
 *
 * SUMMARY:
 *   Religious minority status in Iran constitutes a structural constraint
 *   that systematically extracts from designated religious communities while
 *   maintaining legitimacy through a combination of constitutional
 *   recognition and international law commitment. The Islamic Republic's
 *   founding documents guarantee freedom of religion and recognize three
 *   religious minorities (Christian, Jewish, Zoroastrian); simultaneously,
 *   the constitution establishes Islam as the state religion and Shia
 *   theology as binding on law. This contradiction produces a snare:
 *   minorities are formally recognized but operationally suppressed; they are
 *   promised legal protection but lack enforcement mechanisms; they cannot
 *   exit through emigration (capital barriers, documentation discrimination),
 *   conversion (monitored and coerced), or religious practice (surveillance
 *   and closure). The constraint exhibits all six DR types from different
 *   structural positions: powerless trapped individuals experiencing maximum
 *   extraction (snare), institutional beneficiaries experiencing the
 *   constraint as coordination (rope), organized underground networks
 *   experiencing mixed coordination and extraction (tangled rope), and
 *   analytical observers seeing a pure extraction mechanism with high
 *   suppression and minimal coordination function.
 *
 * KEY AGENTS:
 *   - Religious minority members (Bahai, Zoroastrian, Christian, Jewish, Sunni Muslim): Primary victims (powerless/trapped) — face legal discrimination, employment exclusion, religious practice surveillance, education barriers, property restrictions
 *   - Shia Islamic establishment (clerical hierarchy, religious endowments, state theology ministry): Primary beneficiaries (institutional/arbitrage) — consolidate ideological authority, allocate state resources, maintain doctrinal control over religious interpretation
 *   - Revolutionary Guard and security apparatus: Secondary beneficiary (institutional/arbitrage) — control minority monitoring infrastructure, allocate enforcement resources, extract economic and intelligence value from surveillance networks
 *   - Underground religious community networks: Secondary actor (organized/constrained) — maintain cultural transmission and mutual aid under suppression, sustain identity through evasion, actively organize resistance
 *   - International community and diaspora: Tertiary actor (powerful/mobile) — provide funding and advocacy but face sanction complications; diaspora funding may inadvertently sustain constraint structures through state intelligence targeting
 *   - Constitutional and legal framework: Institutional performance mechanism (institutional/arbitrage) — maintains legitimacy through recognition of minorities while enforcing discrimination; theater persists through international pressure and internal legitimacy needs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(religious_minority_status_iran, 0.68).
domain_priors:suppression_score(religious_minority_status_iran, 0.82).
domain_priors:theater_ratio(religious_minority_status_iran, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(religious_minority_status_iran, extractiveness, 0.68).
narrative_ontology:constraint_metric(religious_minority_status_iran, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(religious_minority_status_iran, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(religious_minority_status_iran, snare).
narrative_ontology:human_readable(religious_minority_status_iran, "Religious Minority Status in Iran").
narrative_ontology:topic_domain(religious_minority_status_iran, "social/political/legal").

domain_priors:requires_active_enforcement(religious_minority_status_iran).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(religious_minority_status_iran, shia_islamic_establishment).
narrative_ontology:constraint_beneficiary(religious_minority_status_iran, revolutionary_guard_apparatus).
narrative_ontology:constraint_victim(religious_minority_status_iran, bahai_community).
narrative_ontology:constraint_victim(religious_minority_status_iran, zoroastrian_community).
narrative_ontology:constraint_victim(religious_minority_status_iran, christian_minorities).
narrative_ontology:constraint_victim(religious_minority_status_iran, jewish_community).
narrative_ontology:constraint_victim(religious_minority_status_iran, sunni_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIGIOUS MINORITY MEMBER (SNARE) — A Bahai, Zoroastrian, Christian, or other non-recognized minority faces legal restrictions on employment, education, property ownership, and religious practice. Exit options are severely constrained: emigration requires capital and international connections; internal exit through conversion is coerced and monitored. The constraint extracts through economic exclusion, legal discrimination, and social surveillance with minimal coordination function. Maximum suppression and experienced extraction.
constraint_indexing:constraint_classification(religious_minority_status_iran, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SHIA ISLAMIC ESTABLISHMENT (ROPE) — The religious hierarchy and state apparatus experience the constraint as a coordination mechanism: it consolidates Islamic identity, maintains doctrinal authority over religious interpretation, and allocates state resources along sectarian lines. The establishment benefits from minority exclusion through resource capture and ideological hegemony. From this position, the constraint solves a genuine coordination problem: 'How do we preserve Shia Islam's institutional primacy?' Minimal experienced extraction because the entire system operates to beneficiary advantage.
constraint_indexing:constraint_classification(religious_minority_status_iran, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SUNNI OR DISSENTING MUSLIM MINORITY (SNARE) — Muslims outside the Twelver Shia mainstream (Kurdish Sunnis, Sufi orders, theological dissenters) experience significant constraints despite nominal religious majority status. Legal restrictions target specific sectarian interpretations; employment discrimination affects Sunni-majority professions; mosque closure and preacher surveillance operate. Exit to mainstream Islam is coerced; exit to another country faces similar barriers as other minorities. High extraction with suppression, though slightly less severe than recognized religious minorities due to nominal Islam affiliation.
constraint_indexing:constraint_classification(religious_minority_status_iran, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL COMMUNITY AND DIASPORA (TANGLED ROPE) — Wealthy diaspora members, international NGOs, and foreign governments have mobile exit options and significant power but are also bound by coordination problems: sanctions and international pressure create mutual dependency; diaspora funding of underground communities sustains the constraint structure itself. The international position combines genuine coordination (human rights monitoring, advocacy networks) with asymmetric extraction (diaspora wealth dynamics, geopolitical leverage over minorities).
constraint_indexing:constraint_classification(religious_minority_status_iran, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNDERGROUND RELIGIOUS COMMUNITY NETWORKS (TANGLED ROPE) — Organized minority religious groups (house churches, covert Bahai networks, underground Zoroastrian communities) experience high suppression but also maintain genuine coordination functions: mutual aid, ritual preservation, cultural transmission, identity maintenance. The constraint requires active enforcement against these networks; the networks require active organization to persist. Both coordination and extraction are present — the community is not passive victim but active participant in sustaining the constraint structure through resistance.
constraint_indexing:constraint_classification(religious_minority_status_iran, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL AND INTERNATIONAL LAW FRAMEWORK (PITON) — Iran's constitution nominally recognizes three religious minorities (Christian, Jewish, Zoroastrian) while denying Bahai status; international human rights commitments coexist with domestic enforcement. This legal theater persists through institutional inertia despite contradictions: the 'recognized minority' status provides minimal actual protection; international conventions are formally ratified but operationally ignored. The framework maintains legitimacy through performative compliance (token minority representatives in parliament, formal legal protections that are not enforced).
constraint_indexing:constraint_classification(religious_minority_status_iran, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a universal, civilizational perspective, religious minority status in Iran is classified as a snare: high base extractiveness (0.68), very high suppression (0.82), active enforcement requirements, minimal coordination function, and asymmetric power relations that produce structural capture of vulnerable populations. The constraint exhibits all snare hallmarks: existence depends on suppressing alternatives (emigration, conversion, religious practice); beneficiaries (the state religious apparatus) gain directly from victims' exclusion; no mutual benefit mechanism exists.
constraint_indexing:constraint_classification(religious_minority_status_iran, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(religious_minority_status_iran_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(religious_minority_status_iran, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(religious_minority_status_iran, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(religious_minority_status_iran, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(religious_minority_status_iran, TR),
    TR >= 0.70.

:- end_tests(religious_minority_status_iran_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from minorities through employment discrimination (professional licensing restricted to Shia Muslims), education barriers (university admission quotas, textbook marginalization), property exclusion (restriction on mosque purchase and expansion), and economic exclusion (pension system discrimination). The extraction is not total — minorities retain some economic activity — but systematic and legally codified. The 30-year measurement trajectory (0.48 → 0.68) shows extraction accumulation over time as enforcement mechanisms have matured and economic pressure has intensified. Suppression (0.82): Very high. Multiple layers of suppression operate: legal prohibition of religious practice (unauthorized gatherings, ritual restrictions), documentation discrimination (ID cards marking religious affiliation), surveillance networks (security ministry monitoring), mobility restrictions (travel limitations for certain minorities), and social enforcement (workplace discrimination, family pressure). Exit options are severely constrained: emigration requires capital and documentation that minorities are systematically denied; conversion is coerced and monitored; internal exit through secular identity is incomplete (documented identity persists through surveillance). Theater ratio (0.55): Moderate. The constitutional recognition of minorities (articles 13-14) provides performative legitimacy — Iran hosts minority representatives in parliament, recognizes holidays, maintains nominal legal protections — while enforcement mechanisms operate to negate these protections. The theater has grown over the measurement period (0.42 → 0.55) as international scrutiny has increased, requiring more elaborate legitimacy performance.
 *
 * PERSPECTIVAL GAP:
 *   The snare/rope divide is the critical gap. The Shia establishment genuinely experiences the constraint as coordination solving a real problem: 'How do we maintain theological authority in a pluralistic society?' From their position, minority suppression is functionally equivalent to establishing a standard (Islamic law as the binding interpretation). From the minority's position, the same constraint is pure extraction with no coordination benefit — they are targeted, not included in the coordination problem. The piton perspective adds a temporal dimension: the constitution's performative function has replaced its original protective function. Early post-revolution constitutions attempted to balance majority religion with minority protection; current enforcement has abandoned this balance, leaving only the theater. Underground networks (tangled rope) reveal that suppression generates counter-organization, making the constraint a co-constitutive structure rather than unilateral domination — but this organized response is not coordination in the Rope sense; it is defensive resistance that requires the constraint to motivate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural relationship to extraction flow. Victims (trapped minorities) occupy d ≈ 0.92-0.98: they are full targets with no exit capacity. Beneficiaries (institutional establishment) occupy d ≈ 0.02-0.05: the constraint exists to subsidize them. Organized resisters (underground networks) occupy d ≈ 0.65-0.75: they face high extraction but have some capacity to reduce it through organization. The diaspora occupies d ≈ 0.60: they have exit capacity (mobile) and resources (powerful) but are also bound to the constraint through kinship and solidarity relationships that limit their external positioning. The constitution itself occupies d ≈ 0.08: it is a beneficiary mechanism whose primary function is to legitimize the extraction (by providing formal legal cover), yielding low or negative experienced extraction because its operation subsidizes the entire constraint structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the snare/rope confusion through clear structural data: beneficiaries (the Shia establishment) do not bear equivalent costs to victims (the religious minorities). Rope would require symmetric coordination where both parties benefit approximately equally or bear symmetric risks; tangled rope would require both genuine coordination AND asymmetric extraction to coexist. Here, the coordination function (consolidating Islamic authority, allocating state resources) benefits exclusively the religious establishment while minorities bear all suppression costs. The theater component (constitutional recognition, international law compliance, minority representation) is performative legitimacy for an underlying snare. The constraint avoids false mountain classification because suppression is high but not natural — it requires active enforcement by security apparatus. The piton perspective reveals that the constitutional theater is the degraded remnant of an earlier legitimacy claim (revolutionary Islam's promise of universal justice) now maintained through inertia as enforcement intensity has revealed the truth of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conversion_coercion_mechanism,
    'Is religious conversion a genuine exit option or a coerced response that creates new forms of constraint?',
    'Ethnographic analysis of post-conversion experiences; tracking of documented coercion cases; correlation between conversion rates and enforcement intensity',
    'If conversion is genuine exit: reclassify affected agents to constrained rather than trapped. If conversion is coerced: suppression metric may underestimate actual constraint intensity — agents forced to adopt false identities carry internalized suppression post-exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conversion_coercion_mechanism, empirical, 'Whether religious conversion functions as genuine exit or coerced compliance').

omega_variable(
    underground_community_sustainability,
    'Do underground religious networks genuinely provide coordination functions (cultural transmission, mutual aid, identity preservation) or are they primarily evasion mechanisms that require the constraint to maintain meaning?',
    'Structural analysis of underground community outputs (cultural production, social services, knowledge transmission) vs constraint-evasion activities; post-constraint analysis of whether these functions persist if enforcement weakens',
    'If genuine coordination: perspective 5 (tangled rope) is accurate — the constraint involves mutual active participation. If primarily evasion: perspective should be snare — the coordination is artifact of suppression, not independent function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(underground_community_sustainability, empirical, 'Whether underground religious networks have genuine coordination function').

omega_variable(
    diaspora_extraction_loop,
    'Does diaspora funding of underground communities inadvertently sustain the constraint by creating resource flows that the state apparatus can track and exploit?',
    'Analysis of state security intelligence targeting of diaspora fund transfers; correlation between diaspora funding spikes and enforcement escalations; documented cases of fund interception',
    'If true: the international/diaspora perspective (4) should reclassify to higher extraction — diaspora participation in the constraint structure contradicts their stated resistance. If false: the tangled rope classification stands — international community''s mobile power allows genuine support without co-constitution of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_extraction_loop, empirical, 'Whether diaspora funding sustains the constraint mechanism').

omega_variable(
    recognized_vs_unrecognized_distinction,
    'Is the constitutional distinction between recognized minorities (Christian, Jewish, Zoroastrian) and unrecognized minorities (Bahai) a meaningful safety distinction or performative protection?',
    'Comparative enforcement intensity data; legal protections invoked vs actual enforcement; documented discrimination rates within recognized vs unrecognized categories',
    'If meaningful distinction: separate constraint stories needed for recognized vs unrecognized minorities with different suppression metrics. If performative: the distinction is piton theater that masks uniform snare structure across all minorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognized_vs_unrecognized_distinction, empirical, 'Whether recognized/unrecognized distinction provides real legal protection').

omega_variable(
    identity_lock_vs_material_trap,
    'Is suppression of religious minorities primarily structural (legal barriers, economic exclusion) or internalized (identity capture, fear-induced compliance internalization)?',
    'Tracking of post-emigration psychological patterns; analysis of third-generation diaspora identity persistence; correlation between enforcement intensity and internalized compliance vs coercive resistance',
    'If primarily structural: trapped exit classification is accurate. If internalized: identity_locked exit classification applies — exit does not resolve suppression because agents have internalized the constraint. This would elevate suppression effective levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_trap, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(religious_minority_status_iran, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reli_tr_t0, religious_minority_status_iran, theater_ratio, 0, 0.42).
narrative_ontology:measurement(reli_tr_t15, religious_minority_status_iran, theater_ratio, 15, 0.48).
narrative_ontology:measurement(reli_tr_t30, religious_minority_status_iran, theater_ratio, 30, 0.55).
narrative_ontology:measurement(reli_tr_t45, religious_minority_status_iran, theater_ratio, 45, 0.55).

% Extraction over time
narrative_ontology:measurement(reli_be_t0, religious_minority_status_iran, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(reli_be_t15, religious_minority_status_iran, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(reli_be_t30, religious_minority_status_iran, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(reli_be_t45, religious_minority_status_iran, base_extractiveness, 45, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(religious_minority_status_iran, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(religious_minority_status_iran, 0.12).
narrative_ontology:affects_constraint(religious_minority_status_iran, iranian_freedom_of_expression).
narrative_ontology:affects_constraint(religious_minority_status_iran, iranian_employment_discrimination).
narrative_ontology:affects_constraint(religious_minority_status_iran, iranian_education_access).

% DUAL FORMULATION NOTE:
% Religious minority status in Iran is the upstream structural constraint that generates downstream constraints in specific domains (employment, education, expression). Each downstream constraint has its own ε value reflecting domain-specific mechanisms, but all are enabled by the overarching minority status constraint. The constraint family decomposes along domain lines: religious_minority_status_iran (structural status), iranian_employment_discrimination (economic extraction), iranian_education_access (educational exclusion), iranian_freedom_of_expression (speech surveillance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(religious_minority_status_iran, institutional, 0.03).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
