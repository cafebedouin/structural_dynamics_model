% ============================================================================
% CONSTRAINT STORY: complicity_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_complicity_equilibrium, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: complicity_equilibrium
 *   human_readable: Complicity Equilibrium: Mutual Extraction and Institutional Stability
 *   domain: institutional_sociology/political_economy
 *
 * SUMMARY:
 *   Complicity equilibrium represents a structural pattern in institutional
 *   governance where leadership and subordinate professionals form a mutual
 *   extraction pact: leadership captures rents (through policy decisions,
 *   resource allocation, or selective enforcement) and distributes side
 *   payments to subordinates (career advancement, resource access, protection
 *   from accountability) in exchange for their silence, cooperation, or
 *   active participation in dysfunctional institutional behavior. The
 *   institution maintains a legitimate public facade while the dysfunction
 *   continues internally. This constraint is distinguished from simple
 *   corruption by its equilibrium structure: it is not unstable theft but a
 *   stabilized distribution system that satisfies all participants (at least
 *   temporarily). It is distinguished from pure extraction (snare) by the
 *   genuine coordination benefits that subordinates receive—the constraint
 *   does solve the collective action problem of maintaining institutional
 *   dysfunction while appearing legitimate. From the perspective of external
 *   stakeholders or whistleblowers, the constraint is pure snare with no
 *   coordination benefit. From the perspective of leadership, it is pure rope
 *   (coordination mechanism). The analytical observer sees tangled rope:
 *   genuine coordination infrastructure layered with asymmetric extraction.
 *   The constraint is enforced through career incentives, loyalty testing,
 *   and selective accountability enforcement. As information asymmetry
 *   degrades through transparency mechanisms (FOIA, journalism,
 *   whistleblowers), the equilibrium destabilizes. The temporal measurement
 *   data shows extractiveness and suppression both rising as institutional
 *   complexity increases and information control requires more active effort.
 *   Theater ratio rises as the compliance apparatus expands to maintain
 *   legitimacy facade in the face of growing internal dysfunction.
 *
 * KEY AGENTS:
 *   - Institutional Leadership (institutional/arbitrage): Primary architect and beneficiary—captures majority of extracted rents, maintains external legitimacy facade, controls career advancement levers
 *   - Middle Management and Professional Cadres (moderate/constrained): Complicit subordinates—receive career advancement and resource access; face high but surmountable exit costs; genuinely benefit from coordination function while bearing asymmetric extraction
 *   - Entry-Level and Early-Career Professionals (powerless/identity_locked): Trapped by identity fusion with institution—structurally mobile but psychologically bound through professional identity; perceive constraint as snare despite exit capacity
 *   - External Stakeholders (powerless/trapped): Citizens, customers, constituents subject to institution's output—bear full extraction cost with no exit capacity within biographical horizon; no coordination benefit
 *   - Institutional Accountability Apparatus (institutional/arbitrage): Formal compliance, audit, and ethics structures—exist as ritualistic forms providing cover for complicity pact
 *   - Reform Coalitions (organized/constrained): External oversight bodies, anti-corruption agencies, civil society—see constraint as temporary structure with sunset mechanisms via transparency and institutional rotation
 *   - Analytical Observer (analytical/analytical): Sees the recurring structural pattern of mutual extraction pacts stabilizing institutional dysfunction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(complicity_equilibrium, 0.58).
domain_priors:suppression_score(complicity_equilibrium, 0.68).
domain_priors:theater_ratio(complicity_equilibrium, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(complicity_equilibrium, extractiveness, 0.58).
narrative_ontology:constraint_metric(complicity_equilibrium, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(complicity_equilibrium, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(complicity_equilibrium, tangled_rope).
narrative_ontology:human_readable(complicity_equilibrium, "Complicity Equilibrium: Mutual Extraction and Institutional Stability").
narrative_ontology:topic_domain(complicity_equilibrium, "institutional_sociology/political_economy").

domain_priors:requires_active_enforcement(complicity_equilibrium).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(complicity_equilibrium, institutional_leadership).
narrative_ontology:constraint_beneficiary(complicity_equilibrium, middle_management).
narrative_ontology:constraint_beneficiary(complicity_equilibrium, professional_cadres).
narrative_ontology:constraint_victim(complicity_equilibrium, institutional_integrity).
narrative_ontology:constraint_victim(complicity_equilibrium, external_stakeholders).
narrative_ontology:constraint_victim(complicity_equilibrium, whistleblowers).
narrative_ontology:constraint_victim(complicity_equilibrium, reform_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTERNAL STAKEHOLDER (SNARE) — Citizens, customers, or constituents subject to the institution's output (corrupted regulatory decisions, inadequate services, harmful policies) have no exit capacity within the biographical horizon. Trapped by geographic jurisdiction, legal obligation, or economic dependency. Bears full extraction cost — the institution's dysfunction is externalized onto those who cannot leave. Maximum experienced extraction.
constraint_indexing:constraint_classification(complicity_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRAPPED SUBORDINATE / IDENTITY-LOCKED (SNARE) — Entry-level or early-career professional within the institution. Structurally mobile (could find other employment) but identity-locked: their professional identity, credentials, and career trajectory are fused with THIS institution. Exit would require abandoning years of identity investment, breaking from institutional socialization, and becoming a 'disloyal' outsider. The binding mechanism is cognitive (internalized institutional identity) not structural (economic dependency alone). Suppression is enforced through identity frames: 'everyone does this,' 'this is how things work,' 'loyalty is expected.' The snare classification reflects that the identity lock creates effective trapedness despite structural mobility.
constraint_indexing:constraint_classification(complicity_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLICIT MIDDLE MANAGER (TANGLED ROPE) — Faces high but surmountable costs to exit: loss of seniority, pension impacts, reputation damage within professional networks, need to relocate or accept lower-status position. But receives genuine coordination benefits — participates in the constraint's governance, has partial agency in decision-making, receives resource allocation and career advancement. The constraint genuinely coordinates information flow and resource distribution (tangled_rope requirement met: active enforcement + beneficiary function + asymmetric extraction). Exit costs are substantial but not insurmountable — constrained rather than trapped.
constraint_indexing:constraint_classification(complicity_equilibrium, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL LEADERSHIP (ROPE) — Primary architect and beneficiary. Leadership experiences the constraint as coordination: distributing benefits to subordinates solves the collective action problem of maintaining institutional dysfunction while projecting legitimacy. The constraint functions to enable leadership's rent extraction. Leadership has maximum arbitrage — can shift to different institutions or roles without penalty; the constraint's benefits flow primarily upward. The classification as rope (not tangled_rope) reflects leadership's net-positive position: they are not bearing asymmetric extraction, they are designing it. Chi is low for this agent because f(d) for beneficiaries with arbitrage is negative.
constraint_indexing:constraint_classification(complicity_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — External reformers (oversight bodies, anti-corruption agencies, civil society organizations) see complicity equilibrium as a temporary structural problem with a sunset: as transparency increases (FOIA requests, investigative journalism, whistleblower protections), the information asymmetry that sustains the pact degrades. Institutional rotation (leadership turnover, generational change) creates windows for norm change. The coalition has agency through external pressure and sunset mechanisms are visible (legal reforms, media exposure, audit requirements). Low theater: reformers focus on structural change rather than ritualistic compliance. Constraint classification as scaffold reflects the presence of real exit paths and sunset logic, though constrained rather than mobile because systemic reform is slow and faces institutional resistance.
constraint_indexing:constraint_classification(complicity_equilibrium, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL ACCOUNTABILITY FRAMEWORK (PITON) — Formal oversight and ethics structures (compliance offices, audit committees, ethics policies) exist as ritualistic forms. They generate reports, hold meetings, and maintain procedural appearance without triggering real accountability. The framework persists through institutional inertia — formally required, regularly enacted, substantively degraded. Theater ratio high: compliance theater substitutes for genuine accountability. The constraint's primary function (extracting while maintaining legitimacy) is now performed by the accountability apparatus itself — compliance structures provide cover for the complicity pact they were designed to prevent.
constraint_indexing:constraint_classification(complicity_equilibrium, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Views complicity equilibrium as a recurring structural pattern in institutional governance: the distribution of extracted rents as side payments to ensure complicity. The constraint genuinely coordinates institutional action (enforces collective silence, distributes resources, maintains legitimacy facade) AND extracts asymmetrically (leadership captures largest share, external stakeholders bear costs). Active enforcement via career incentives, loyalty testing, and accountability suppression. Classification reflects both the coordination function (which is real — the institution does function as a coordinated entity) and the extractive asymmetry (which is also real — power and benefits flow toward leadership).
constraint_indexing:constraint_classification(complicity_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complicity_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(complicity_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complicity_equilibrium, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(complicity_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(complicity_equilibrium, TR),
    TR >= 0.70.

:- end_tests(complicity_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint enables leadership to extract rents while ensuring complicity from subordinates through benefit distribution. The extraction is not maximal (0.72+) because subordinates genuinely benefit from the coordination infrastructure and resource distribution—this is not pure theft. The extractiveness value reflects the asymmetry (leadership captures disproportionate share) tempered by the genuine coordination benefits. Suppression (0.68): High. The constraint requires active suppression of alternative information pathways, whistleblower channels, and competing narratives. Dissent is suppressed through career penalties, loyalty testing, and framing ('everyone does this,' 'this is standard practice'). Accountability mechanisms are themselves captured and converted to compliance theater. Theater ratio (0.65): Moderate-high. The compliance apparatus generates performative compliance activities (ethics trainings, audit reports, policy documents) that create appearance of accountability while the complicity pact continues. Theater rises over time (0.50 → 0.65) as institutional complexity increases and the gap between formal and actual accountability grows. Theater does not reach maximum (0.72+) because some genuine coordination functions remain; the institution does coordinate action and distribute resources according to its actual norms (the complicit norms), not purely according to facade.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival gap problem. Leadership sees rope: the constraint elegantly solves the collective action problem of how to extract rents while maintaining legitimacy and ensuring subordinate cooperation. External stakeholders see snare: pure extraction with no coordination benefit and no exit capacity. Middle managers see tangled rope: they are extracting from below and being extracted from above; they genuinely benefit but also bear asymmetric costs. Entry-level professionals see snare despite structural mobility because identity-lock prevents exit perception. The reform coalition sees scaffold: the information asymmetry is degrading and institutional rotation creates windows for change. The accountability apparatus sees piton: degraded institution maintaining performative compliance rather than real accountability. The analytical observer sees tangled rope: both the coordination function and the extractive asymmetry are structural and real. The perspectival gap reveals how the same institutional arrangement can be experienced as coordination (leadership), extraction (stakeholders), mixed (middle management), identity trap (early career), temporary problem (reformers), or degraded ritual (compliance apparatus). Each perspective is analytically correct from its position—there is no 'true' classification, only indexed classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural relationship to the extraction flow and their exit capacity. Leadership (beneficiary + arbitrage): d ≈ 0.05, f(d) ≈ -0.12, low/negative chi—leadership is the beneficiary and has maximum exit options (can move to other institutions), so effective extraction runs opposite to them. Middle managers (mixed + constrained): d ≈ 0.55, f(d) ≈ 0.75, moderate chi—they are both beneficiaries (receive side payments) and victims (extract from below, extracted from above), with constrained exit (high costs but possible). Entry-level professionals (victim + identity_locked): d ≈ 0.89, f(d) ≈ 1.28, high chi—they are victims of extraction, believe themselves trapped despite structural mobility, so experienced extraction is amplified by the perceptual gap. External stakeholders (victim + trapped): d = 0.95, f(d) ≈ 1.42, maximum chi—pure victims with no exit capacity within biographical horizon. Analytical observer (d ≈ 0.72, canonical): observes the full structure from outside. The directionality derivation chain captures why middle managers experience moderate extraction (they genuinely benefit) while entry-level professionals experience severe extraction despite similar nominal status (identity lock amplifies perceptual distance from structural reality).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy resolution through perspectival analysis. The apparent contradiction—is this rope (coordination) or snare (extraction)?—is resolved by indexing to position. From leadership's position, it is rope: the constraint solves the collective action problem of maintaining dysfunction while ensuring complicity and legitimacy. From external stakeholders' position, it is snare: extraction with no coordination benefit. From middle management's position, it is tangled rope: both coordination benefits (they participate in resource distribution and governance) and extraction (they are asymmetrically extracted from). The mandatrophy is resolved not by finding the 'correct' single classification but by recognizing that each indexed perspective captures a real aspect of the constraint's structure. The constraint IS both rope and snare simultaneously—rope for those who benefit from the mutual extraction pact, snare for those who bear costs without reciprocal benefits. The analytical observer's tangled rope classification captures this duality: the constraint has genuine coordination function (it coordinates institutional action toward its actual objectives, which are leadership extraction and complicity maintenance) AND asymmetric extraction (rents flow upward, costs flow outward to external stakeholders). The mandatrophy is not resolved by choosing one type; it is resolved by recognizing that the constraint is only stable because different agents experience it as different types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coerced_complicity,
    'To what degree do subordinates voluntarily participate in the complicity pact versus being coerced through institutional power asymmetry?',
    'Longitudinal interviews with exit cohorts; analysis of decision-framing (''I had to'' vs ''I chose to'') in whistleblower accounts; comparison of exit rates under different accountability regimes',
    'Pure coercion: constraint reclassifies as snare for all subordinates (not tangled rope for middle managers). Genuine voluntariness: classification depends on whether subordinates perceive the distribution of benefits as fair (cooperative vs extractive framing). Most likely: mixed—coercion and voluntary participation coexist, with voluntariness increasing by rank.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_complicity, empirical, 'Voluntary vs coerced participation in complicity pact').

omega_variable(
    extractive_asymmetry_quantification,
    'What is the actual distribution of rents extracted by the constraint? What proportion goes to leadership vs subordinates vs is retained as institutional slack?',
    'Forensic institutional accounting; comparison of career trajectory benefits across ranks; analysis of internal compensation structures and discretionary spending; comparison to benchmark institutions without complicity equilibrium',
    'If leadership captures >80% of extracted value: tangled_rope classification confirmed. If distribution is more egalitarian (>40% to subordinates): constraint may be genuine rope (equitable coordination). If retained as slack: constraint may appear different from external vs internal perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_asymmetry_quantification, empirical, 'Distribution of extracted rents across institutional hierarchy').

omega_variable(
    institutional_dysfunction_necessity,
    'Is the dysfunctional behavior that the complicity pact enables genuinely necessary for the institution to function, or is it purely extractive?',
    'Comparative institutional analysis: performance metrics in institutions where complicity is broken (post-scandal reform, leadership transition, transparency shock); identification of which ''dysfunctions'' are load-bearing vs performative; interview analysis of whether subordinates believe the dysfunction is necessary',
    'If dysfunction is load-bearing (institution could not function without it): constraint approaches rope classification (genuine coordination requirement). If dysfunction is purely extractive: tangled_rope or snare classification confirmed. If dysfunction is performative (institution functions fine without it post-scandal): suggests theater_ratio underestimation and piton classification may be more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_dysfunction_necessity, empirical, 'Necessity vs superfluity of institutional dysfunction').

omega_variable(
    information_asymmetry_degradation,
    'How stable is the information asymmetry that sustains the complicity pact? What is the half-life of suppressed information in the modern transparency environment?',
    'Temporal analysis of major institutional scandals: how long from ''hidden complicity'' to ''public exposure'' across different institutional types; effectiveness of whistleblower mechanisms, FOIA requests, investigative journalism; prediction models of information leakage rates',
    'If half-life < 5 years: scaffold perspective is accurate, sunset clauses are real, constraint has inherent degradation pathway. If half-life > 15 years: complicity equilibrium may be more stable than scaffold perspective suggests; reform timeline extends generational timescale. Directly affects whether constraint can sustain as tangled_rope long-term or will bifurcate into reform (scaffold) or breakdown (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_degradation, empirical, 'Information asymmetry half-life and suppression degradation').

omega_variable(
    identity_lock_mechanism_in_subordinates,
    'For identity-locked subordinates, what specific aspects of institutional identity prevent exit: professional credentials, relational bonds, ideological commitment, status/rank, or internalized institutional values?',
    'Post-exit interviews with former professionals; analysis of which constraints prevent leaving vs which enable leaving (pension vesting, credential recognition, network ties); identity-framing analysis in institutional communication and training',
    'If identity lock is primarily credential-based or status-based: supplementing credentials (second degree, lateral move to non-institutional sector) enables exit. If lock is ideological or relational: exit requires identity reconstruction, harder to achieve. Classification impact: stronger identity locks = worse perceptual distance between structurally mobile and functionally trapped = worse snare dynamics despite constrained exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_subordinates, conceptual, 'Mechanisms of professional identity fusion in subordinate roles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(complicity_equilibrium, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(complicit_tr_t0, complicity_equilibrium, theater_ratio, 0, 0.5).
narrative_ontology:measurement(complicit_tr_t4, complicity_equilibrium, theater_ratio, 4, 0.59).
narrative_ontology:measurement(complicit_tr_t8, complicity_equilibrium, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(complicit_be_t0, complicity_equilibrium, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(complicit_be_t4, complicity_equilibrium, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(complicit_be_t8, complicity_equilibrium, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(complicit_su_t0, complicity_equilibrium, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(complicit_su_t4, complicity_equilibrium, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(complicit_su_t8, complicity_equilibrium, suppression_requirement, 8, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(complicity_equilibrium, enforcement_mechanism).
narrative_ontology:affects_constraint(complicity_equilibrium, institutional_accountability_facade).
narrative_ontology:affects_constraint(complicity_equilibrium, whistleblower_suppression).
narrative_ontology:affects_constraint(complicity_equilibrium, career_path_incentive_distortion).

% DUAL FORMULATION NOTE:
% Complicity equilibrium is composed of three structurally distinct sub-constraints: (1) the mutual extraction pact between leadership and subordinates (the primary constraint modeled here); (2) the accountability facade that provides cover for the pact (modeled separately as institutional_accountability_facade, piton classification); (3) the suppression mechanisms targeting whistleblowers and reform advocates (whistleblower_suppression, snare classification). These are linked constraints with ε values reflecting their different empirical status: the primary pact is high-extraction/high-coordination (tangled_rope, ε=0.58), the accountability facade is low-function/high-theater (piton, ε≈0.20), and whistleblower suppression is pure extraction (snare, ε≈0.72). All three are required for complicity equilibrium to sustain; decomposing them enables analysis of which mechanisms are most vulnerable to intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
