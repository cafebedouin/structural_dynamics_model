% ============================================================================
% CONSTRAINT STORY: indonesian_civil_society_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indonesian_civil_society_restrictions, []).

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
 *   constraint_id: indonesian_civil_society_restrictions
 *   human_readable: Indonesian Civil Society Restrictions and NGO Control Mechanisms
 *   domain: political_governance/civil_society
 *
 * SUMMARY:
 *   Indonesia's civil society restriction regime emerged through incremental
 *   legal accumulation and enforcement intensification over the past two
 *   decades. The Legal and Human Rights Ministry, coordinated with military
 *   and intelligence apparatus, has deployed overlapping legal mechanisms
 *   (Law No. 8/1997 on NGO Corporate Documents, Anti-Terrorism Law No.
 *   15/2003 amended 2018, Information and Electronic Transaction Law No.
 *   11/2008, Revised Law on Peaceful Assembly No. 9/1998, and recent
 *   restrictions on foreign funding and 'organizational dissolution'
 *   authority) to constrain independent civil society. The constraint
 *   operates as a tangled hybrid: it coordinates state security apparatus
 *   capacity (genuine collective action problem of maintaining state
 *   coherence and counterterrorism) while simultaneously extracting resources
 *   and agency from civil society organizations, human rights defenders, and
 *   religious minorities. The theater ratio (0.65) reflects that legal
 *   enforcement is partly performative — courts produce predetermined
 *   outcomes under executive pressure; restrictions are selectively enforced
 *   against politically disfavored groups while tolerating state-aligned
 *   civil society; the formal judicial system maintains legitimacy theater
 *   while functional independence has degraded. The measurement trajectory
 *   shows escalation: base extractiveness rising from 0.35 to 0.58, and
 *   theater ratio rising from 0.45 to 0.65, indicating that enforcement
 *   intensity has increased while the judicial system has become more
 *   performative over the interval.
 *
 * KEY AGENTS:
 *   - Individual Activists and NGO Workers: Primary victims (powerless/trapped) — face legal jeopardy, surveillance, organizational dissolution, and high exit costs. No arbitrage capacity.
 *   - Civil Society Organizations and Networks: Secondary victims (organized/constrained) — experience the restriction regime as both constraining and coordinating; forced to collaborate on legal defense and security protocols.
 *   - State Security Apparatus (Military, Intelligence, Police): Primary beneficiary (institutional/arbitrage) — experiences restrictions as coordination mechanism for state capacity and security coherence. Low experienced extraction because they are the beneficiary.
 *   - Compliant International NGOs: Secondary beneficiary (institutional/arbitrage) — can navigate restrictions through formalized government relationships and reporting frameworks; maintain presence while managing state relations.
 *   - Formal Judicial System: Institutional actor (institutional/constrained) — maintains performative review authority while functional independence has been captured by executive pressure. Sees own process as degraded (piton perspective).
 *   - International Human Rights Bodies and Advocacy Networks: Organized observers (organized/mobile) — operate outside restriction regime's direct reach; building alternative verification and advocacy pathways with potential sunset mechanism.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes genuine coordination functions alongside asymmetric extraction; identifies constraint as tangled rope hybrid rather than pure extraction or pure coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indonesian_civil_society_restrictions, 0.58).
domain_priors:suppression_score(indonesian_civil_society_restrictions, 0.72).
domain_priors:theater_ratio(indonesian_civil_society_restrictions, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indonesian_civil_society_restrictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(indonesian_civil_society_restrictions, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(indonesian_civil_society_restrictions, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indonesian_civil_society_restrictions, tangled_rope).
narrative_ontology:human_readable(indonesian_civil_society_restrictions, "Indonesian Civil Society Restrictions and NGO Control Mechanisms").
narrative_ontology:topic_domain(indonesian_civil_society_restrictions, "political_governance/civil_society").

domain_priors:requires_active_enforcement(indonesian_civil_society_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indonesian_civil_society_restrictions, state_security_apparatus).
narrative_ontology:constraint_beneficiary(indonesian_civil_society_restrictions, executive_authority).
narrative_ontology:constraint_victim(indonesian_civil_society_restrictions, civil_society_organizations).
narrative_ontology:constraint_victim(indonesian_civil_society_restrictions, independent_advocacy_groups).
narrative_ontology:constraint_victim(indonesian_civil_society_restrictions, religious_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED ACTIVIST (SNARE) — Activists and NGO workers face legal jeopardy (Anti-Terrorism Law, Law No. 11/2008 on Information and Electronic Transactions used selectively for political speech), organizational dissolution threats, asset seizure, and social stigmatization. Exit options are severely limited: remaining in Indonesia means operating under pervasive surveillance and arbitrary enforcement; leaving means abandoning livelihood, family ties, and organizational mission. Material barriers to exit are high. No perceived coordination benefit from the restriction mechanism itself — only coercion.
constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY NETWORK (TANGLED ROPE) — Organized networks (inter-NGO coalitions, human rights alliances) experience the restriction mechanism as both constraining AND coordinating. The restrictions force coordination: NGOs must collaborate on legal defense, develop security protocols, share risk assessment data, and build collective advocacy channels. The constraint simultaneously extracts (organizations face operational costs, legal fees, resource diversion to compliance) and enables coordination (shared threat creates coalition infrastructure). Organized networks have higher agency than isolated activists but remain constrained by resource limitations and legal jeopardy.
constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLIANT INTERNATIONAL NGO (ROPE) — Large international NGOs (International NGO Forum, World Bank-affiliated organizations) experience the restriction regime as a coordination mechanism: government approval, partner screening, and reporting frameworks create clear rules for legitimate operation. These organizations can arbitrage between domestic regulatory compliance and international funder requirements, maintaining presence while managing government relations. Their experience is one of coordination under defined constraints, not pure extraction — the rules, while restrictive, are legible and mostly navigable for well-resourced organizations.
constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE SECURITY APPARATUS (ROPE) — The military, intelligence agencies, and police benefit from the restriction regime as a coordination mechanism for state coherence. Civil society restrictions help prevent mobilization against state authority, coordinate inter-agency security priorities, and establish shared standards for threat identification. The apparatus experiences the constraint as enabling, not extractive — a mechanism that solves the collective action problem of maintaining state capacity. Low experienced extraction because this actor is the primary beneficiary.
constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL ADVOCACY COALITION (SCAFFOLD) — International human rights bodies (UN mechanisms, regional courts, international NGO networks) see Indonesian restrictions as a temporary enforcement gap with a sunset. The mechanisms driving toward liberalization include ASEAN human rights standards, international treaty obligations, generational value shifts in younger Indonesian leadership, and economic pressure from international investors valuing civil society stability. These forces are building alternative verification pathways (international witness, documentation mechanisms, transnational advocacy) that bypass state enforcement. The coalition has high agency because it operates outside the restriction regime's direct reach.
constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: FORMAL JUDICIAL SYSTEM (PITON) — Indonesia's judiciary has authority to strike down restrictive laws or limit enforcement but has been captured by executive pressure and operates under institutional constraints (judicial independence fragile, budget dependence on executive, political appointments). The formal legal process is substantially performative: courts adjudicate cases within parameters set by executive interest rather than legal principle. The judicial system sees its own process as degraded — it maintains theater of independent review while producing predetermined outcomes. Theater ratio is high because the judicial ritual persists through institutional inertia despite reduced functional independence.
constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global/civilizational perspective, the Indonesian restriction regime has genuine coordination functions (state capacity, terrorism prevention, social order) alongside asymmetric extraction (suppression of dissent, concentration of power, silencing of minorities). The constraint is neither pure coordination (rope) nor pure extraction (snare) — it is a hybrid with both mechanisms structurally present. Active enforcement is visible and structural. Beneficiaries and victims are clearly identifiable. The mechanism prevents neither coordination nor extraction — it produces both simultaneously.
constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indonesian_civil_society_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indonesian_civil_society_restrictions, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indonesian_civil_society_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indonesian_civil_society_restrictions, TR),
    TR >= 0.70.

:- end_tests(indonesian_civil_society_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The state captures significant benefits (reduced dissent, enhanced state capacity, concentration of political authority) while imposing substantial costs on civil society (legal jeopardy, operational constraints, resource diversion to compliance). The value reflects both genuine coordination costs (counterterrorism, public order) and extractive overhead (political repression, silencing of minorities). The trajectory from 0.35 to 0.58 indicates escalating enforcement intensity rather than stable mechanism. Suppression (0.72): High. Multiple barriers to exit and resistance exist: selective prosecutions under broadly written laws (ITE Law, Anti-Terrorism Law) create legal jeopardy; surveillance capacity is extensive (digital monitoring, informant networks); organizational dissolution is a threat weapon; international funding restrictions create resource dependency; and social stigmatization of 'radical' civil society is pervasive. However, suppression is not total — some organizations operate in gray zones, diaspora networks function, and international platforms provide partial escape valves. Theater ratio (0.65): Moderate-high. Legal processes (court proceedings, administrative approvals) maintain legitimacy performance while actual outcomes are predetermined by executive interest. The judiciary performs independence theater while producing politically guided decisions. Formal restrictions exist and are selectively enforced against disfavored groups while tolerated for state-aligned organizations. The trajectory from 0.45 to 0.65 reflects increasing judicial capture and performative behavior over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the state apparatus's rope perspective and the activist's snare perspective is the diagnostic signature of the constraint's tangled hybrid nature. Both perspectives are accurate readings of the same structural arrangement — the security apparatus genuinely experiences coordination benefits from the restrictions (state capacity, counterterrorism coordination), while activists genuinely experience pure extraction (legal jeopardy without coordination benefit). The gap reveals that the coordination function is asymmetric: it benefits state capacity at the cost of civil society agency. The constraint simultaneously solves the state's collective action problem (maintaining internal security and political control) while extracting from those it suppresses. This is the defining characteristic of tangled rope — both coordination and extraction are structurally present and functionally entangled.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (0.58), the agent's directionality value (d), and spatial scope modifier. Beneficiaries (state apparatus, compliant NGOs) have low d → f(d) ≈ -0.12 to 0.02 → χ is low or negative (they experience coordination, not extraction). Victims with constrained exit (civil society networks) have mid-range d → f(d) ≈ 0.75 → χ ≈ 0.35-0.40 (significant experienced extraction with some agency). Victims with trapped exit (individual activists) have high d → f(d) ≈ 1.40 → χ ≈ 0.60-0.70 (maximum experienced extraction). Organized international observers have high d but analytical distance → χ reduced by their external perspective. The scope modifier σ(S) is 1.0 (national scope) — neither amplifying nor dampening χ relative to the reference. The tangled rope classification derives from: base extractiveness 0.58 (within tangled range 0.40-0.90), suppression 0.72 (≥ 0.40 gate), active enforcement present (required), beneficiaries (state apparatus, compliant NGOs) providing coordination function, and victims (civil society) bearing asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the tangled rope classification itself. The constraint cannot be classified as pure rope (Snare alternative) because suppression is too high (0.72 ≥ 0.40) and extraction is asymmetric (state capacity benefits vs civil society costs are not symmetrically distributed). It cannot be classified as pure snare because genuine coordination functions exist (counterterrorism, state capacity, public order) that benefit the beneficiary class. The classification as tangled rope acknowledges both mechanisms structurally present: the restriction regime coordinates state security apparatus (genuine collective action problem solved) while simultaneously extracting agency and resources from civil society (asymmetric cost distribution). The perspectival divergence is the diagnostic signature — observers positioned as beneficiaries see rope; observers positioned as victims see snare; the analyst sees the entanglement. This resolves the mandatrophy question: 'Is this coordination or extraction?' Answer: Yes, both. The constraint is a tangled hybrid where the coordination function serves the state's security while the extraction targets civil society dissent capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vs_repression_boundary,
    'What threshold distinguishes legitimate state security coordination (terrorism prevention, public order) from extractive political repression (suppression of dissent, silencing of minorities)?',
    'Comparative analysis of enforcement patterns: rates of prosecution against actual terrorist activity vs political speech; analysis of conviction outcomes and sentence severity; cross-country comparison with other democracies with similar terrorism threats but lower restriction levels',
    'If threshold tilted toward security: more perspectives classify as rope (coordination-dominant). If threshold tilted toward repression: more perspectives classify as snare (extraction-dominant). Current data suggests hybrid (tangled rope) is most accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_repression_boundary, empirical, 'Boundary between legitimate security coordination and extractive political repression').

omega_variable(
    judicial_independence_capture_mechanism,
    'Is the Indonesian judiciary captured through direct executive pressure, institutional incentive misalignment, or both? How reversible is the capture?',
    'Longitudinal analysis of judicial decisions on state authority cases before/after key political transitions; survey of judicial independence metrics; analysis of budget autonomy and appointment processes; comparison with regional counterparts (Malaysia, Thailand, Philippines)',
    'If directly captured and reversible: piton classification is accurate and sunset is possible through institutional reform. If structurally captured: the judicial system may not recover without broader constitutional change. Classification consequence: shifts piton to snare if capture is irreversible at current reform pace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_capture_mechanism, empirical, 'Mechanism and reversibility of judicial system capture').

omega_variable(
    civil_society_coalition_formation_dynamics,
    'Do the restrictions accelerate or decelerate organizational coalition-building among civil society groups? Is the tangled rope perspective''s claim about forced coordination empirically supported?',
    'Network analysis of inter-NGO collaboration patterns before/after restriction escalations; time series of coalition formation events correlated with enforcement intensity; interviews with NGO networks on whether cooperation increased due to shared threat',
    'If restrictions accelerate coalition formation: tangled rope classification is correct — coordination function is structurally present alongside extraction. If restrictions atomize groups: more snare-like — extraction without coordination benefit. Current evidence suggests mixed (cyclical: threat → coalition → enforcement → atomization → regrouping).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_coalition_formation_dynamics, empirical, 'Whether restrictions drive or prevent civil society coalition formation').

omega_variable(
    international_advocacy_sunset_timeline,
    'What is the empirical timeline for international pressure (ASEAN standards, treaty obligations, economic incentives) to produce measurable liberalization of restrictions?',
    'Historical analysis of ASEAN human rights mechanism adoption rates; tracking of World Bank/IMF conditionality on governance; survey of investor sentiment on civil society environment; comparison with regional precedents (Thailand''s civil society liberalization post-2017, Philippines post-Duterte)',
    'If sunset occurs within 10-15 years: scaffold classification is structurally sound. If international pressure proves ineffective without internal political realignment: sunset is aspirational rather than structural, and scaffold perspective is misclassified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_advocacy_sunset_timeline, empirical, 'Timeline for international pressure to produce civil society liberalization').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression (0.72) is structural (legal barriers, surveillance capacity, organizational dissolution threat) versus internalized (activists internalize self-censorship, NGO leadership internalizes cooperation logic)?',
    'Post-exit trajectory analysis: do activists who leave Indonesia maintain suppression patterns? Survey of diaspora civil society activism compared to domestic civil society; comparison of speech patterns in Indonesian diaspora spaces vs within-country spaces; analysis of whether internalized suppression persists after structural barriers are removed',
    'If primarily structural: barriers can be reduced through legal reform; exit would restore full capacity. If partly internalized: suppression persists even after structural removal; activists would need deprogramming or generational change. Classification consequence: higher effective suppression if internalized component is large.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Proportion of suppression that is structural versus internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indonesian_civil_society_restrictions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indo_tr_t0, indonesian_civil_society_restrictions, theater_ratio, 0, 0.45).
narrative_ontology:measurement(indo_tr_t5, indonesian_civil_society_restrictions, theater_ratio, 5, 0.58).
narrative_ontology:measurement(indo_tr_t10, indonesian_civil_society_restrictions, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(indo_be_t0, indonesian_civil_society_restrictions, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(indo_be_t5, indonesian_civil_society_restrictions, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(indo_be_t10, indonesian_civil_society_restrictions, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indonesian_civil_society_restrictions, enforcement_mechanism).
narrative_ontology:affects_constraint(indonesian_civil_society_restrictions, indonesian_media_freedom_restrictions).
narrative_ontology:affects_constraint(indonesian_civil_society_restrictions, religious_minority_protection).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indonesian_civil_society_restrictions, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
