% ============================================================================
% CONSTRAINT STORY: institutional_legitimation_theater
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimation_theater, []).

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
 *   constraint_id: institutional_legitimation_theater
 *   human_readable: Institutional Legitimation Theater
 *   domain: institutional_governance/governance_legitimacy
 *
 * SUMMARY:
 *   Institutional legitimation theater is a structural constraint wherein
 *   organizations maintain elaborate procedures, consultation mechanisms,
 *   governance rituals, and compliance theatrics that create the appearance
 *   of constituent participation, transparency, and power constraint while
 *   preserving substantive decision-making authority in leadership hands.
 *   This constraint exhibits characteristics across all six DR types
 *   depending on the observer's structural position. From the constituent's
 *   perspective, theater is a snare — they are trapped in participation
 *   rituals that produce no substantive influence. From leadership's
 *   perspective, theater is rope — it solves the coordination problem of
 *   maintaining authority while appearing responsive. From a transparency
 *   advocate's perspective, theater is a scaffold — digital tools and open
 *   data are creating sunset pathways that make theater unnecessary. From the
 *   institutional bureaucracy's perspective, theater is a piton — formal
 *   procedures persist through inertia long after functional purpose has
 *   eroded. From a moderate institutional actor, theater is tangled rope —
 *   genuinely coordinating information flow while extracting time and
 *   creating appearance of consent. From a civilizational observer, theater
 *   risks appearing as mountain — an immutable feature of large-scale
 *   coordination — but this represents a false summit that naturalizes what
 *   are contingent institutional arrangements.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — captures authority preservation and decision-making prerogative during appearance of consultation; can exit through career mobility
 *   - Constituent Subjects: Primary victims (powerless/trapped) — bear cost of participation in theater that produces no substantive influence; cannot exit without abandoning institutional membership
 *   - Mid-Tier Managers: Secondary actors (moderate/constrained) — both benefit from theater (cover for decisions) and bear cost (time spent in procedures); constrained by career dependence and institutional hierarchies
 *   - Transparency Movement: Organized opposition (organized/constrained) — building alternative accountability mechanisms; see theater as temporary constraint with sunset
 *   - Bureaucratic Institution: Institutional actor (institutional/arbitrage) — maintains theater through inertia; sees own procedures as performative but unable to acknowledge or reform
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks confusing contingent institutional pathology with immutable requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimation_theater, 0.58).
domain_priors:suppression_score(institutional_legitimation_theater, 0.65).
domain_priors:theater_ratio(institutional_legitimation_theater, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimation_theater, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_legitimation_theater, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_legitimation_theater, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimation_theater, tangled_rope).
narrative_ontology:human_readable(institutional_legitimation_theater, "Institutional Legitimation Theater").
narrative_ontology:topic_domain(institutional_legitimation_theater, "institutional_governance/governance_legitimacy").

domain_priors:requires_active_enforcement(institutional_legitimation_theater).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimation_theater, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_legitimation_theater, power_consolidation).
narrative_ontology:constraint_victim(institutional_legitimation_theater, operational_efficiency).
narrative_ontology:constraint_victim(institutional_legitimation_theater, substantive_accountability).
narrative_ontology:constraint_victim(institutional_legitimation_theater, constituent_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUENT SUBJECT (SNARE) — Citizens, employees, or stakeholders embedded in the institution cannot exit the legitimation theater. They bear the cost of procedural theater that substitutes for substantive decision-making. Trapped by citizenship, employment, or dependency on institutional services. No meaningful way to withdraw participation or challenge the legitimacy framework without abandoning the institutional relationship itself.
constraint_indexing:constraint_classification(institutional_legitimation_theater, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER MANAGER (TANGLED ROPE) — Constrained by career dependence and institutional rules, but also benefits from legitimation theater. The theater provides cover for decisions already made, reduces personal accountability, and creates appearance of consultation while preserving managerial prerogative. Extraction exists (time spent on procedural theater) but so does genuine coordination (consensus-building, information flow). Exit is possible but costly — career damage, blacklisting, reputational loss in institutional networks.
constraint_indexing:constraint_classification(institutional_legitimation_theater, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Primary beneficiary. Leadership experiences the constraint as pure coordination: legitimation theater solves the problem of maintaining authority while appearing responsive. The theater is functional for them — it enables decisions to proceed without genuine constraint. They have arbitrage options (can leave, can leverage institutional position into external opportunities). Net flow of extraction runs toward them.
constraint_indexing:constraint_classification(institutional_legitimation_theater, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSPARENCY MOVEMENT (SCAFFOLD) — Organized advocates for institutional transparency, accountability mechanisms, and genuine participatory governance see legitimation theater as a temporary constraint being actively dismantled. Digital tools, open-data mandates, and oversight mechanisms are creating parallel accountability pathways that reduce the theater's extraction power. Sunset trajectory: as transparency norms mature (10-15 years), the functional requirement for theater diminishes because actual decision-making becomes visible. Theater persists but loses extractive force.
constraint_indexing:constraint_classification(institutional_legitimation_theater, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BUREAUCRATIC RITUAL SYSTEM (PITON) — Formal consultation procedures, governance committees, and compliance processes persist long after their functional purpose has eroded. Modern institutions often maintain elaborate consultation theater because they are accustomed to it, because it appears legitimate, and because removing it seems risky — even though the theater itself prevents substantive participation. The system sees its own procedures as performative (theater_ratio 0.78) but continues them through institutional inertia. Exit would require admitting the procedures are theater.
constraint_indexing:constraint_classification(institutional_legitimation_theater, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LEGITIMACY PARADOX (MOUNTAIN) — From a civilizational scale, some theater is inherent to institutional legitimacy: institutions cannot function without belief in their legitimacy, that belief requires visible procedures that appear to constrain power, and complete transparency would undermine the mythic foundations that maintain collective coordination. This perspective treats legitimation theater as an immutable structural feature of large-scale coordination. However, this risks naturalizing a contingent institutional pathology — the engine's false summit detector will likely identify this as misclassification.
constraint_indexing:constraint_classification(institutional_legitimation_theater, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimation_theater_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimation_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimation_theater, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimation_theater, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimation_theater, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimation_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Leadership captures significant benefits during the window when theater obscures decision-making, but the extraction is not total because some genuine coordination occurs within and around the theater (information flow, consensus-building on implementation details, feedback on consequences). Theater provides selective extraction — leadership extracts decision-making authority while theater maintains appearance of distributed constraint. Suppression (0.65): Moderate-high. Constituents face institutional barriers to meaningful exit (membership costs, practical dependence on services) and informational barriers (theater obscures actual decision-making mechanisms). However, suppression is not maximal because some constituent groups have resources to exit or demand change. Theater itself is a suppression mechanism — by creating appearance of participation, it reduces constituent motivation to organize alternatives. Theater ratio (0.78): High. The measurement of extractiveness has drifted upward over time (0.42 → 0.58) as institutions have become more sophisticated in theater design, while actual constituent influence has remained flat or declined. Theater ratio has also increased (0.62 → 0.78), indicating that procedural performance has expanded relative to functional decision-making.
 *
 * PERSPECTIVAL GAP:
 *   Leadership's rope perspective fundamentally diverges from constituent's snare perspective because they experience opposite ends of the extraction flow. Leadership sees theater as enabling coordination (appearance of responsiveness reduces resistance to decisions). Constituents see theater as preventing coordination (procedures substitute for actual participation). The gap is not perceptual error — it reflects real structural differences in who benefits and who bears cost. The transparency movement's scaffold perspective adds temporal dimension: they are actively building sunset mechanisms (open data platforms, digital accountability) that would make traditional theater unnecessary. The piton perspective recognizes that institutions maintain theater through inertia, which is analytically distinct from deliberate extraction (tangled rope) or inevitable necessity (mountain).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to the theater extraction flow. Institutional leadership with arbitrage exit options experiences low d (beneficiary with exit = 0.15 canonical). Constituent subjects with trapped exit experience high d (victim with no exit = 0.95 canonical). Mid-tier managers who benefit from theater but face career costs occupy middle d (constrained exit + mixed benefit/cost = 0.55 canonical). The piton classification for the bureaucratic system derives from theater_ratio (0.78) exceeding the piton threshold (0.70), not from high experienced extractiveness — the system sees its own procedures as performative, which is the piton diagnostic signal. Theater_ratio exceeding base_extractiveness (0.78 > 0.58) indicates that procedural performance has exceeded functional necessity, a hallmark of institutional degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   Institutional legitimation theater resolves mandatrophy by showing that the tangled rope classification requires genuine coordination function (which theater provides: information aggregation, appearance of constraint) alongside asymmetric extraction (leadership preserves decision authority). The constraint is NOT pure extraction (snare) because theater does coordinate information and reduce uncertainty about institutional intent. It is NOT pure coordination (rope) because the theater masks asymmetric decision-making authority. The constraint IS hybrid: theater genuinely coordinates while extracting constituent influence. The mandatrophy is resolved by recognizing that all six perspectives are legitimate structural readings — there is no single 'true' classification, only perspectival positions defined by the observer's structural relationship to the theater mechanism. Leadership experiences rope (coordination); constituents experience snare (extraction); organized transparency advocates experience scaffold (sunset mechanism); the institution itself experiences piton (performative degradation). The analytical observer must resist the temptation to naturalize theater as mountain — the structural data shows theater is contingent on institutional choices and designed procedures, not immutable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functionality_threshold,
    'What level of procedural theater is functionally necessary for institutional legitimacy versus what is extractive overhead?',
    'Comparative institutional analysis: institutions with minimal theater vs high-theater institutions; correlation between theater_ratio and constituent trust, operational efficiency, and substantive accountability',
    'If threshold is high (theater_ratio > 0.70 is necessary): legitimation theater is legitimate coordination cost. If threshold is low (< 0.40): most observed theater is extractive overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionality_threshold, empirical, 'Functionality threshold for legitimation theater').

omega_variable(
    constituent_perception_gap,
    'Do constituents perceive legitimation theater as genuinely constraining institutional power or as performative?',
    'Constituent surveys, focus groups, behavioral analysis of institutional responsiveness to constituent feedback from consultation processes; tracking whether consultation outcomes influence final decisions',
    'If constituents believe theater is genuine constraint: extraction is lower (rope-like from constituent perspective). If constituents see theater as performative: extraction is higher (snare-like), and suppression may include cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituent_perception_gap, empirical, 'Constituent perception of theater''s constraining power').

omega_variable(
    leadership_intentionality,
    'Is legitimation theater maintained deliberately (as strategic extraction mechanism) or through institutional inertia (unexamined practice)?',
    'Leadership interviews, strategic planning documents, decision-making process analysis; comparison of institutions where theater was consciously reduced vs institutions where it persists unchanged',
    'If deliberate: classification remains tangled_rope/snare (strategic extraction). If inertial: classification shifts toward piton (degraded institution). Affects mandatrophy analysis — deliberately maintained hybrid is riskier than inertial performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leadership_intentionality, empirical, 'Whether legitimation theater is strategic or inertial').

omega_variable(
    alternative_legitimacy_mechanisms,
    'Can institutions maintain legitimacy without theater? What replaces the procedural appearance of constraint?',
    'Historical cases of rapid theater reduction (e.g., digital transparency implementations, reorganizations); institutional performance metrics post-theater-reduction',
    'If legitimacy persists after theater removal: theater is extractive overhead, not necessary coordination. If legitimacy collapses: theater is necessary cost of large-scale coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_legitimacy_mechanisms, empirical, 'Feasibility of legitimacy without theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimation_theater, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(instleg_tr_t0, institutional_legitimation_theater, theater_ratio, 0, 0.62).
narrative_ontology:measurement(instleg_tr_t5, institutional_legitimation_theater, theater_ratio, 5, 0.7).
narrative_ontology:measurement(instleg_tr_t10, institutional_legitimation_theater, theater_ratio, 10, 0.78).

% Extraction over time
narrative_ontology:measurement(instleg_be_t0, institutional_legitimation_theater, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(instleg_be_t5, institutional_legitimation_theater, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(instleg_be_t10, institutional_legitimation_theater, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimation_theater, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_legitimation_theater, democratic_deficit).
narrative_ontology:affects_constraint(institutional_legitimation_theater, accountability_capture).
narrative_ontology:affects_constraint(institutional_legitimation_theater, procedural_legitimacy_paradox).

% DUAL FORMULATION NOTE:
% Institutional legitimation theater is downstream of specific governance failures (accountability gaps, power concentration) but represents a distinct structural constraint. The upstream constraints have different ε values reflecting specific institutional pathologies; the theater itself has ε = 0.58 reflecting the mixed coordination-extraction hybrid. Theater is also upstream of transparency/open-data constraints — theater reduction affects those constraint values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_legitimation_theater, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
