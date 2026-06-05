% ============================================================================
% CONSTRAINT STORY: separation_of_powers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers, []).

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
 *   constraint_id: separation_of_powers
 *   human_readable: Separation of Powers in Constitutional Governance
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Separation of powers is a foundational constraint in constitutional
 *   governance: the principle that legislative, executive, and judicial
 *   authority must be distributed across distinct institutions with mutual
 *   checks and balances. This constraint exhibits a rich perspectival
 *   structure because it simultaneously performs coordination functions
 *   (enabling stable governance across multiple power centers) and extraction
 *   functions (diffusing accountability, enabling institutional capture,
 *   protecting organized interests at the expense of marginalized
 *   constituencies). The theater ratio has increased over the interval as the
 *   gap between formal separation and actual power concentration has widened
 *   through emergency powers, administrative agencies, and party discipline
 *   mechanisms. The constraint classifies as Tangled Rope from the analytical
 *   perspective: it coordinates genuine governance functions across branches
 *   while also enabling asymmetric extraction through diffused accountability
 *   and organized lobby capture.
 *
 * KEY AGENTS:
 *   - Marginalized Constituencies: Primary victims (powerless/trapped) — politically minorities locked into jurisdictions where distributed power diffuses accountability for their interests
 *   - Organized Interest Groups: Secondary beneficiary (moderate/constrained) — can lobby multiple branches and benefit from divided government dynamics creating uncertainty and delay
 *   - Executive Branch: Primary institutional beneficiary (institutional/arbitrage) — exercises power within enumerated domain; coordination through other branches' oversight enables predictable constraint enforcement
 *   - Legislative and Judicial Branches: Institutional beneficiaries (institutional/arbitrage) — maintain domain authority; constrained by other branches but receive institutional recognition and stable power-sharing
 *   - Direct Popular Sovereignty: Primary victim (powerless/trapped) — diffusion of authority means no single institution can be held accountable to majoritarian will; checks can block majorities indefinitely
 *   - Democratic Reform Movements: Organized agents seeking sunset (organized/constrained) — view SOP as temporary solution to monarchical absolutism, believe democratic maturity enables more direct delegation without tripartite diffusion
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable checks on power concentration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers, 0.38).
domain_priors:suppression_score(separation_of_powers, 0.42).
domain_priors:theater_ratio(separation_of_powers, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers, extractiveness, 0.38).
narrative_ontology:constraint_metric(separation_of_powers, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(separation_of_powers, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers, tangled_rope).
narrative_ontology:human_readable(separation_of_powers, "Separation of Powers in Constitutional Governance").
narrative_ontology:topic_domain(separation_of_powers, "political/constitutional").

domain_priors:requires_active_enforcement(separation_of_powers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers, legislative_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers, executive_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers, judicial_branch).
narrative_ontology:constraint_beneficiary(separation_of_powers, institutional_checks).
narrative_ontology:constraint_victim(separation_of_powers, direct_popular_sovereignty).
narrative_ontology:constraint_victim(separation_of_powers, executive_efficiency).
narrative_ontology:constraint_victim(separation_of_powers, political_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED CONSTITUENCY (SNARE) — Political minorities locked into a system where power is distributed among branches that collectively ignore their interests. Cannot exit the jurisdiction without extreme cost. The separation of powers diffuses accountability: each branch blames the others for inaction. No single authority can be held responsible for the marginalization.
constraint_indexing:constraint_classification(separation_of_powers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED INTEREST GROUP (TANGLED ROPE) — Has capacity to lobby multiple branches, creating genuine coordination across institutions. Also benefits from structural diffusion: can shop claims across branches, delay unwanted regulation through divided government, leverage branch-against-branch dynamics. Some extraction cost (lobbying burden, uncertainty), some coordination benefit (voice in multiple channels).
constraint_indexing:constraint_classification(separation_of_powers, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE BRANCH (ROPE) — Experiences separation of powers as coordination mechanism enabling governance. Can exercise power within enumerated domain without seizure by other branches. Transactions costs rise, but predictability and durability increase. The constraint solves the executive's problem of controlling its own agents through oversight from other branches.
constraint_indexing:constraint_classification(separation_of_powers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM MOVEMENT (SCAFFOLD) — Sees separation of powers as a temporary solution to the problem of monarchical absolutism, but recognizes its sunset: as democratic legitimacy increases, direct representation and proportional accountability become more viable. The constraint has declining necessity — it was built for distrust of demos, but democratic maturity enables more direct delegation. Reform movements envision participatory alternatives where power diffusion is unnecessary.
constraint_indexing:constraint_classification(separation_of_powers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CEREMONIAL CONSTITUTIONAL ORDER (PITON) — The formal separation of powers persists as theatrical ritual while actual power concentrates: executive orders, emergency powers, administrative agencies, party discipline, and informal networks bypass the formal tripartite structure. The branches perform separation; actual governance happens in hidden channels. Theater ratio is high because the visible constraint (three co-equal branches) masks informal power centralization.
constraint_indexing:constraint_classification(separation_of_powers, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a structural analysis perspective, separation of powers appears as an immutable consequence of concentrating coercive authority: any concentration of power sufficient to enforce law becomes itself a threat requiring containment. The constraint is inevitable given the premise that governmental power must exist but must not concentrate. However, the base properties contradict mountain classification — the empirical evidence of power reconcentration through informal mechanisms, emergency powers, and administrative expansion reveals this as a false summit. The 'inevitability' naturalizes what is actually a fragile institutional arrangement.
constraint_indexing:constraint_classification(separation_of_powers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(separation_of_powers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(separation_of_powers, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(separation_of_powers, TR),
    TR >= 0.70.

:- end_tests(separation_of_powers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The separation of powers does enable genuine coordination across institutions — each branch can operate within its domain knowing other branches will check extreme overreach. This coordination benefit reduces the pure extractiveness score. However, the constraint also enables extraction through diffused accountability: organized interests can lobby multiple branches simultaneously, creating uncertainty and delay that benefits those with resources to navigate the complexity. The moderate value reflects the genuine tension between coordination and extraction. Suppression (0.42): Moderate. Barriers to political participation and voice are substantial but not total. Citizens cannot easily exit a national jurisdiction; political minorities can be locked out of all three branches. However, organized interests have access to institutional channels; formal democratic participation mechanisms (voting, petition, protest) exist even if unequally effective. Theater ratio (0.55): Moderate-to-high. The formal separation of powers persists as explicit constitutional principle and institutional structure, but actual governance increasingly happens through informal channels: executive orders and emergency powers concentrate executive authority; administrative agencies operate with autonomy from formal branch oversight; party discipline and unified government bypass checks-and-balances mechanisms. The theater has increased because the visible constraint (three co-equal branches) increasingly masks actual power distribution.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between institutional beneficiaries and powerless victims is maximal. The executive branch sees separation of powers as a coordination mechanism enabling stable governance (Rope classification from immediate time horizon). The marginalized constituency sees it as a mechanism that diffuses accountability and locks them out of all decision-making channels (Snare classification from trapped/biographical perspective). The organized interest group occupies a middle position: they benefit from institutional access but face coordination costs and uncertainty (Tangled Rope). The democratic reform movement sees the constraint as addressing a temporary problem (monarchical absolutism) whose necessity declines as democratic legitimacy increases (Scaffold). The piton perspective reveals that formal separation increasingly masks actual power concentration (theater ratio rising). The analytical observer risks naturalizing this as inevitable ('power concentration is inherent to governance') when it is actually a contingent institutional arrangement that could be restructured.
 *
 * DIRECTIONALITY LOGIC:
 *   The separation of powers constraint distributes directionality values asymmetrically across stakeholders. Institutional branches (executive, legislative, judicial) experience low directionality (d ≈ 0.15-0.25) because they benefit from the constraint's coordination function — they have recognized domain authority, stable rules of engagement, and institutional resources to navigate the system. They derive d values from institutional power + arbitrage exit options + beneficiary status. Organized interest groups experience moderate directionality (d ≈ 0.45-0.55) because they face high coordination costs (lobbying multiple branches) but also gain opportunities for strategic leverage (shopping claims, exploiting divided government). They derive d values from moderate power + constrained exit + mixed beneficiary/victim status. Marginalized constituencies experience high directionality (d ≈ 0.80-0.95) because they cannot effectively navigate the system's complexity, have minimal lobbying resources, and experience diffused accountability as extraction. They derive d values from powerless status + trapped exit + victim status. The canonical fallback for 'powerless' produces d ≈ 1.00, which correctly reflects that trapped agents with no resources to navigate institutional complexity experience the constraint as pure extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The separation of powers constraint resolves the mandatrophy by exhibiting genuine coordination function (enabling stable multi-branch governance) alongside genuine extraction (diffusing accountability, enabling lobby capture). The Tangled Rope classification is warranted because both functions are present: (1) beneficiaries exist (institutional branches that derive stable authority from the constraint), (2) victims exist (marginalized constituencies locked out by diffused accountability), (3) active enforcement is required (constitutional interpretation, judicial review, legislative-executive negotiation must constantly maintain the separation). The constraint is not a false summit (mountain misclassification) because the base properties (extractiveness 0.38, suppression 0.42) and structural data (genuine coordination, genuine asymmetric extraction) support tangled rope. The piton perspective correctly identifies that theater_ratio has increased, suggesting drift toward degradation: if administrative agencies and emergency powers continue to accumulate outside formal tripartite structure, the constraint's coordination function will atrophy while its theatrical performance increases, potentially causing reclassification to piton. Current status: functional Tangled Rope with increasing piton risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_vs_actual_power_distribution,
    'Does the formal tripartite separation of powers accurately describe how authority actually flows, or do informal mechanisms (party discipline, administrative agencies, executive orders, lobby networks) constitute the real governance structure?',
    'Empirical mapping of decision-making authority across formal institutions vs informal networks; analysis of which entities actually determine policy outcomes vs which entities ratify predetermined decisions',
    'If formal structure accurately represents authority flow: separation of powers is functioning as designed (Rope or Scaffold). If actual power reconcentrates through informal channels: the constraint is largely theatrical (Piton), and its extractiveness should be reclassified downward (the extraction that appears to flow from SOP is actually concentrated outside it).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_vs_actual_power_distribution, empirical, 'Whether formal separation maps to actual authority distribution').

omega_variable(
    emergency_power_permanence,
    'Are executive emergency powers genuinely exceptional and temporary, or have they become permanent fixtures that constitute a second, shadow constitution?',
    'Historical analysis of emergency declarations: frequency, duration, scope creep, whether they revert or become standard. Comparison of peacetime vs wartime executive authority expansion.',
    'If genuinely temporary: SOP retains enforceability (Rope/Tangled Rope). If emergency powers are normalized and permanent: SOP''s constraint on executive authority is effectively void, and the system should reclassify as pure executive dominance (Snare from powerless perspective), requiring different governance analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_power_permanence, empirical, 'Whether emergency powers are temporary or have become permanent').

omega_variable(
    administrative_state_location,
    'In which branch of the formal tripartite system do the administrative agencies (EPA, FDA, SEC, etc.) actually reside, and where does their decision-making authority come from?',
    'Legal analysis of agency authority sources (statute, executive order, delegated rulemaking); empirical analysis of which branch successfully controls agency decisions; historical tracking of agency independence from formal branch oversight',
    'If agencies are truly subordinate to one branch: they fit within SOP structure (Rope). If agencies are genuinely independent, accountable to none of the three branches: SOP is violated at its foundation, and the system has a fourth power that the constraint does not address (requires decomposition into separate constraint stories).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_state_location, empirical, 'Which branch controls the administrative state').

omega_variable(
    democratic_alternative_sufficiency,
    'Could direct democratic mechanisms (referendum, recall, proportional representation, participatory budgeting) replace the separation-of-powers model as a check on concentrated authority while reducing the transaction costs and diffusion of accountability?',
    'Comparative analysis of jurisdictions with high direct democracy (Switzerland, some US states) vs pure representative systems; measurement of accountability, responsiveness, minority protection, and governance efficiency across models',
    'If direct democracy provides adequate checks without SOP: the Scaffold perspective is correct, and SOP''s necessity is declining. If direct democracy fails or produces different failure modes: SOP remains necessary, and the Scaffold sunset is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_alternative_sufficiency, conceptual, 'Whether direct democracy can replace separation of powers').

omega_variable(
    minority_protection_mechanism,
    'Does separation of powers actually protect political minorities, or does it protect institutional minorities (branches) while minority factions within the electorate face majoritarian tyranny?',
    'Historical analysis of minority rights protection: correlation between SOP and civil rights expansion vs suppression; measurement of whether minorities with institutional allies (courts, federal legislators) gain protection vs minorities lacking institutional champions',
    'If SOP protects political minorities: the snare classification is wrong, and the marginalized constituency should see rope or tangled rope. If SOP protects only institutionalized minorities: the snare perspective is correct, and the constraint structure requires disaggregation of ''minority'' by institutional embeddedness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_protection_mechanism, empirical, 'Whether SOP protects political minorities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sep_pow_tr_t0, separation_of_powers, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sep_pow_tr_t50, separation_of_powers, theater_ratio, 50, 0.5).
narrative_ontology:measurement(sep_pow_tr_t100, separation_of_powers, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(sep_pow_be_t0, separation_of_powers, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sep_pow_be_t50, separation_of_powers, base_extractiveness, 50, 0.33).
narrative_ontology:measurement(sep_pow_be_t100, separation_of_powers, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers, judicial_review).
narrative_ontology:affects_constraint(separation_of_powers, legislative_gridlock).
narrative_ontology:affects_constraint(separation_of_powers, executive_emergency_powers).
narrative_ontology:affects_constraint(separation_of_powers, administrative_state_delegation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
