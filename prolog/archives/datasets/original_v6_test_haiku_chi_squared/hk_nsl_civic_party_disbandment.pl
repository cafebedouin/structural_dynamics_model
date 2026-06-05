% ============================================================================
% CONSTRAINT STORY: hk_nsl_civic_party_disbandment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hk_nsl_civic_party_disbandment, []).

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
 *   constraint_id: hk_nsl_civic_party_disbandment
 *   human_readable: The Hong Kong National Security Law (NSL) leading to the dissolution of the Civic Party
 *   domain: political/governance
 *
 * SUMMARY:
 *   The Hong Kong National Security Law (NSL), imposed by Beijing in June
 *   2020, created a political environment that culminated in the dissolution
 *   of the Civic Party in November 2023. The constraint models the structural
 *   extraction mechanism embedded in NSL: it provides vague criminal
 *   liability for 'sedition,' 'subversion,' and 'foreign collusion,' backed
 *   by a National Security Council with Beijing representatives that
 *   overrides normal judicial process. The Civic Party's voluntary
 *   dissolution reflects the rational calculation that operating under NSL is
 *   impossible — prosecution is inevitable regardless of conduct, retaliation
 *   against family members is possible, and the political space for
 *   opposition has been eliminated. This constraint exhibits the full
 *   spectrum of DR types from different perspectives: pure extraction (snare)
 *   from the opposition's viewpoint, coordination mechanism (rope) from
 *   Beijing's perspective, degraded institutional function (piton) from the
 *   judiciary's perspective, mixed extraction-coordination (tangled rope)
 *   from the business community's perspective, and false natural law
 *   (mountain) from the perspective that naturalizes geopolitical
 *   determinism. The theater ratio (0.68) reflects the performative nature of
 *   the institutional apparatus: trials continue, legal procedure is
 *   observed, but outcomes are predetermined by political direction rather
 *   than evidence. The high suppression (0.85) captures the combination of
 *   criminal liability, asset freezes, emigration barriers, and family
 *   retaliation mechanisms that eliminate all practical exit options except
 *   complete political abandonment.
 *
 * KEY AGENTS:
 *   - Civic Party leadership: Primary victim (moderate/constrained) — voluntary dissolution to avoid prosecution of members, but constrained by family ties and asset bases in Hong Kong
 *   - Hong Kong opposition politicians: Primary victim (powerless/trapped) — face criminal liability under vague NSL standards; cannot exit without abandoning political identity
 *   - Hong Kong civil society: Secondary victim (moderate/constrained) — activists, journalists, scholars face prosecution risk; some exit through emigration at high cost
 *   - Beijing central government: Primary beneficiary (institutional/arbitrage) — achieves sovereignty assertion and elimination of institutional competitors
 *   - Hong Kong security apparatus: Primary beneficiary (institutional/arbitrage) — expands institutional authority and jurisdiction; coordination mechanism for Beijing's will
 *   - Hong Kong business community: Mixed (organized/constrained) — benefits from political stabilization and preferred-partner access; constrained by regulatory uncertainty and selective enforcement risk
 *   - Independent judiciary: Victim (institutional/constrained) — formal independence ritual maintained but substantive function subordinated to national security imperative
 *   - International democratic coalition: Victim (organized/mobile) — cannot enforce alternative authority in Hong Kong; sanctions have low costliness to Beijing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hk_nsl_civic_party_disbandment, 0.72).
domain_priors:suppression_score(hk_nsl_civic_party_disbandment, 0.85).
domain_priors:theater_ratio(hk_nsl_civic_party_disbandment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, extractiveness, 0.72).
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(hk_nsl_civic_party_disbandment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hk_nsl_civic_party_disbandment, snare).
narrative_ontology:human_readable(hk_nsl_civic_party_disbandment, "The Hong Kong National Security Law (NSL) leading to the dissolution of the Civic Party").
narrative_ontology:topic_domain(hk_nsl_civic_party_disbandment, "political/governance").

domain_priors:requires_active_enforcement(hk_nsl_civic_party_disbandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hk_nsl_civic_party_disbandment, beijing_central_government).
narrative_ontology:constraint_beneficiary(hk_nsl_civic_party_disbandment, hong_kong_security_apparatus).
narrative_ontology:constraint_victim(hk_nsl_civic_party_disbandment, hong_kong_civil_society).
narrative_ontology:constraint_victim(hk_nsl_civic_party_disbandment, opposition_political_parties).
narrative_ontology:constraint_victim(hk_nsl_civic_party_disbandment, independent_judiciary).
narrative_ontology:constraint_victim(hk_nsl_civic_party_disbandment, press_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONG KONG OPPOSITION POLITICIAN (SNARE) — Cannot exit without abandoning political career or fleeing territory. Faces criminal liability under NSL for vague offenses (sedition, subversion). No independent appeals mechanism. d≈0.98, f(d)≈1.50, σ=0.8 → χ≈0.90. Maximum extraction and suppression.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HONG KONG CIVIL SOCIETY ACTIVIST (SNARE) — Can exit through emigration but at severe cost (abandonment of community, social capital). Constrained by fear of prosecution, asset freezes, travel restrictions. NSL retroactively criminalizes prior speech. d≈0.92, f(d)≈1.35, σ=0.8 → χ≈0.79. High extraction despite exit option due to high cost.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: HONG KONG BUSINESS COMMUNITY (TANGLED ROPE) — Constrained by dependence on mainland markets and Beijing approval. Experiences NSL as both extraction (regulatory uncertainty, selective enforcement against rivals) and coordination (explicit alignment signals generate preferred-partner status, reduced tariffs, regulatory favor). Benefits from political stabilization that removes protest risk. d≈0.58, f(d)≈0.78, σ=1.0 → χ≈0.56. Moderate extraction with coordination benefits.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HONG KONG SECURITY APPARATUS (ROPE) — Coordinating mechanism for Beijing's sovereignty assertion. Derives institutional authority and expanded jurisdiction from NSL. Experiences as pure coordination: clarity of mandate, elimination of institutional competitors (independent judges, legislators). Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05, σ=1.0 → χ≈-0.04. Negative effective extraction = net institutional strengthening.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BEIJING CENTRAL GOVERNMENT (ROPE) — NSL solves a coordination problem: establishing singular Chinese sovereignty over Hong Kong without full institutional merger. Creates clear signal about non-negotiable lines (separatism, foreign interference, subversion). Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12, σ=1.1 → χ≈-0.07. Net beneficiary from coordination.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: INTERNATIONAL DEMOCRATIC COALITION (SNARE) — Organized (UN member states, NGO networks) but cannot enforce alternative authority in Hong Kong territory. Mobile through sanctions, diplomatic action, visa denials — but these have low costliness to Beijing. Experiences NSL as pure extraction of Hong Kong's autonomy with no compensating coordination function. d≈0.88, f(d)≈1.28, σ=1.2 → χ≈0.93. High effective extraction at global scope.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: HONG KONG INDEPENDENT JUDICIARY (PITON) — Once a functional constraint on executive power (separation of powers). NSL retroactively subordinates judiciary to national security imperative via National Security Council override. Judicial system continues with formal independence ritual (trials, sentencing, appeals) but substantive function has atrophied — outcomes determined ex-ante by political direction. theater_ratio=0.68 (≥0.70 threshold just missed, but trajectory toward piton). Victim + constrained → d≈0.85, f(d)≈1.18, σ=0.8 → χ≈0.64. Degraded institutional extraction.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / GEOPOLITICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, Beijing's assertion of singular sovereignty over Hong Kong is presented as an immutable consequence of the 'One Country, Two Systems' framework collapsing under pressure. But structural data (ε=0.72, suppression=0.85, theater=0.68) contradicts the mountain classification. The engine will detect this as a false summit: the 'geopolitical necessity' framing naturalizes what is actually a contingent political choice (Beijing could maintain autonomy, could allow opposition parties, could protect judiciary). This perspective risks misclassifying extraction as natural law.
constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hk_nsl_civic_party_disbandment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hk_nsl_civic_party_disbandment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hk_nsl_civic_party_disbandment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hk_nsl_civic_party_disbandment, TR),
    TR >= 0.70.

:- end_tests(hk_nsl_civic_party_disbandment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): Very high. The NSL creates asymmetric extraction mechanism: opposition politicians lose careers and freedom; civil society loses speech rights; judiciary loses independence; business gains preferred-partner access conditional on political compliance. The extraction is not absolute expropriation but permanent threat of prosecution and institutional subordination. Trajectory: initial assertiveness (0.35 at t=0, when NSL was theoretical) → rapid escalation (0.55 at t=2, after initial prosecutions) → sustained high level (0.72 at t=4, after Party dissolution). Suppression (0.85): Very high. NSL elimates all meaningful exit options: opposition cannot compete, cannot emigrate freely (asset freezes), cannot appeal to independent judiciary (subordinated to National Security Council), cannot organize covertly (surveillance, informants). Only exit is complete political abandonment. Theater ratio (0.68): Moderately high. NSL regime maintains performative institutions: trials are conducted with legal procedure, evidence is presented, sentences are given. But outcomes are predetermined by political direction; judges receive guidance from National Security Council; appeals are perfunctory. The ritual of justice persists while substance is hollowed out. Trajectory: initial opaqueness (0.42 at t=0) → increasing formalization of theater (0.55 at t=2) → stabilization of theatrical procedure (0.68 at t=4).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival variance across structural positions. Beijing's central government sees coordination (Rope) — NSL solves the legitimate problem of asserting singular sovereignty and eliminating institutional competitors. The security apparatus sees organizational empowerment (Rope) — expanded mandate and unambiguous authority. The business community sees mixed extraction-coordination (Tangled Rope) — loses some regulatory freedom but gains preferred-partner access and political stability. Opposition politicians see pure extraction (Snare) — no coordination benefit, only criminal liability and career destruction. International actors see extraction without coordination (Snare) — observe Hong Kong autonomy being eliminated without any demonstrable security benefit to Hong Kong citizens. The judiciary sees its own institutional degradation (Piton) — formal independence ritual maintained but actual decision-making authority transferred to National Security Council. The analytical observer tempted to naturalize NSL as geopolitical necessity sees a false mountain (mountain) — structural data contradicts the determinism claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Opposition politicians: Victim + trapped → d≈0.98, f(d)≈1.50. Maximum extraction; no exit capacity. Civil society: Victim + constrained → d≈0.92, f(d)≈1.35. High extraction; exit possible only through emigration at severe cost. Judiciary: Victim + constrained → d≈0.85, f(d)≈1.18. Institutional extraction; formal role maintained but substantive function subordinated. Business community: Mixed (beneficiary on regulatory access + victim on enforcement uncertainty) + constrained → d≈0.58, f(d)≈0.78. Moderate extraction mediated by selective enforcement. Hong Kong security apparatus: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Net organizational strengthening; no extraction from perspective. Beijing government: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Maximum beneficiary; sovereignty assertion without constraints. International coalition: Organized victim + mobile → d≈0.88, f(d)≈1.28. High extraction at global scope; sanctions available but low-cost to Beijing.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: This constraint demonstrates the core mandatrophy resolution — distinguishing extraction from legitimate coordination. NSL could be framed as a coordination mechanism for Hong Kong governance (coordination of security, rule of law, sovereignty clarity). The mandatrophy resolves through structural decomposition: NSL does provide coordination benefits to Beijing and Hong Kong security apparatus (rope perspective), but these coordination benefits are conditional on extraction from opposition and civil society. The constraint is Snare (not Rope) because: (1) the coordination function flows primarily to one party (Beijing), (2) the victim groups (opposition, civil society) derive zero coordination benefit — they cannot opt into the system by complying more fully, (3) suppression mechanisms (asset freezes, family retaliation, migration barriers) eliminate alternatives that would be available in a pure coordination regime. A genuine coordination mechanism would allow opposition parties to operate under agreed rules; NSL eliminates the possibility of political opposition under any terms. Therefore, mandatrophy is resolved: NSL is Snare, not Rope. The Beijing perspective (Rope) represents a partial truthful framing of how Beijing experiences the constraint (coordination benefit), but the global classification prioritizes the structural reality that opposition cannot negotiate better terms within the constraint system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_versus_sovereignty,
    'Does the NSL represent Beijing enforcing an immutable sovereignty principle, or suppressing Hong Kong autonomy that was structurally viable?',
    'Counterfactual analysis: trajectories of other autonomous regions (Macau, Singapore, Taiwan); historical comparison with earlier Hong Kong sovereignty arrangements; expert testimony from constitutional scholars in both jurisdictions',
    'If immutable: NSL is mountain (natural law of geopolitics). If contingent choice: NSL is snare (political extraction). Classification swing of 5 DR types.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_versus_sovereignty, conceptual, 'Whether NSL enforces immutable sovereignty or suppresses viable autonomy').

omega_variable(
    opposition_viability_prior_to_nsl,
    'Was Hong Kong opposition politics genuinely democratic competition or a Potemkin institution that the NSL merely formalized?',
    'Analysis of pre-2020 Hong Kong legislative outcomes: did opposition parties exercise meaningful legislative power? Could they block government initiatives? Historical trajectory of press freedom, assembly rights, electoral competitiveness metrics before NSL.',
    'If genuinely democratic: NSL represents dramatic extraction (snare classification is accurate). If already constrained: NSL is incremental suppression (classification may shift toward piton). Affects victim legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_viability_prior_to_nsl, empirical, 'Whether Hong Kong opposition was genuinely democratic before NSL').

omega_variable(
    enforcement_selectivity_mechanism,
    'Is NSL enforcement mechanically inevitable (applied to all subversive speech equally) or discretionary (selectively targeting rival factions)?',
    'Comparative analysis of prosecution rates across opposition factions; identification of cases where similar speech produced different outcomes; expert analysis of National Security Council decision-making processes; leaked documentation or testimony about enforcement criteria.',
    'If inevitable: Snare with predictable rules (still extraction, but transparent). If discretionary: Snare becomes tangled_rope hybrid (coordination of elites through selective enforcement). Affects directionality of business community perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Whether NSL enforcement is mechanically inevitable or discretionary').

omega_variable(
    international_enforcement_cost,
    'Are international sanctions and diplomatic costs high enough to make NSL a net negative for Beijing, or are they absorbed as acceptable extraction cost?',
    'Economic modeling of sanctions impact; bilateral trade flow changes; foreign investment reduction; brain drain measurement; comparison with pre-sanction baseline; Beijing''s revealed preferences (accepting sanctions vs scaling back NSL).',
    'If costs exceed benefits: NSL may degrade into piton (maintained through inertia despite losses). If absorbed costs: Snare classification holds. Affects whether constraint is sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_cost, empirical, 'Whether international costs make NSL a net loss for Beijing').

omega_variable(
    civil_society_coalition_capacity,
    'Can Hong Kong civil society organize underground networks sufficient to maintain democratic capacity during NSL regime, or is suppression complete?',
    'Tracking of underground political activity (confidential polling, diaspora networks, coded communication); comparative analysis with other authoritarian regimes and their resistance movements; technological capacity assessment (VPN prevalence, encrypted communication adoption).',
    'If capacity survives: Civil society victims may upgrade from powerless → moderate/organized (coalition power). If suppression complete: Classification holds at powerless. Affects long-term regime stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_society_coalition_capacity, empirical, 'Whether Hong Kong civil society can organize resistance under NSL').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hk_nsl_civic_party_disbandment, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hk_nsl_tr_t0, hk_nsl_civic_party_disbandment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hk_nsl_tr_t2, hk_nsl_civic_party_disbandment, theater_ratio, 2, 0.55).
narrative_ontology:measurement(hk_nsl_tr_t4, hk_nsl_civic_party_disbandment, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(hk_nsl_be_t0, hk_nsl_civic_party_disbandment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hk_nsl_be_t2, hk_nsl_civic_party_disbandment, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(hk_nsl_be_t4, hk_nsl_civic_party_disbandment, base_extractiveness, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hk_nsl_civic_party_disbandment, enforcement_mechanism).
narrative_ontology:affects_constraint(hk_nsl_civic_party_disbandment, hong_kong_press_freedom_degradation).
narrative_ontology:affects_constraint(hk_nsl_civic_party_disbandment, cross_strait_political_integration_pressure).
narrative_ontology:affects_constraint(hk_nsl_civic_party_disbandment, china_national_security_law_generalization).

% DUAL FORMULATION NOTE:
% The NSL represents a strategic choice by Beijing to establish singular sovereignty through legal and institutional mechanisms rather than formal abolition of Hong Kong institutions. This creates a constraint family with related extraction mechanisms: press freedom degradation (related snare operating on media institutions), political party dissolution (related snare operating on opposition parties), and generalization of NSL to other territories (related snare with different geographic scope). The present story focuses on the political party dissolution pathway; sibling constraints model press freedom and territorial generalization with their own ε values reflecting different extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hk_nsl_civic_party_disbandment, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
