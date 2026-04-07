% ============================================================================
% CONSTRAINT STORY: portugal_government_stability_ad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_portugal_government_stability_ad, []).

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
 *   constraint_id: portugal_government_stability_ad
 *   human_readable: The AD Minority Government Stability (The "Presidential" Scaffold)
 *   domain: political/governance
 *
 * SUMMARY:
 *   The Aliança Democrática (AD) minority government in Portugal, formed in
 *   March 2024, operates as a Scaffold — a temporary coordination mechanism
 *   with a constitutionally-defined sunset. The constraint arises from
 *   Portugal's proportional representation system and fragmented parliament
 *   following the 2023 elections, where no single coalition holds a
 *   legislative majority. The AD coalition (PSD, CDS-PP, and independent
 *   support) governs with tacit parliamentary tolerance from opposition
 *   parties, particularly the Socialist Party (PS), who choose not to trigger
 *   a confidence vote. This arrangement solves the collective action problem:
 *   'How do we form a functioning government without a supermajority?' The
 *   constraint is enforced through the threat of dissolution (presidential
 *   power) and elections (constitutional calendar). It exhibits scaffold
 *   characteristics because the arrangement is explicitly temporary —
 *   governed by electoral cycles and the understanding that either the AD
 *   will strengthen its parliamentary position or elections will reset the
 *   coalition arithmetic. The theater_ratio (0.58) reflects the performative
 *   aspects of parliamentary debate and confidence maneuvering, but the
 *   underlying mechanism is structural: the confidence vote threat is real,
 *   the sunset clause is constitutional, and the coordination function
 *   (enabling executive function without a majority) is genuine.
 *
 * KEY AGENTS:
 *   - AD Coalition (PSD, CDS-PP, independents): Primary beneficiary (institutional/arbitrage) — governs despite minority status; can shift support coalitions; controls executive agenda
 *   - Portuguese Executive: Primary coordinator (institutional/immediate) — operates effective government with constrained parliament; experiences the minority government as a coordination solution, not an extraction problem
 *   - Socialist Party (PS) and Left-Bloc Opposition: Secondary actor (moderate/mobile) — moderate exit options; benefits from leverage over government on case-by-case votes; experiences mixed coordination and extraction
 *   - Portuguese Presidency: Constitutional enforcer (institutional/constrained) — holds dissolution power; ensures sunset clause through electoral calendar; provides threat mechanism for scaffold stability
 *   - Vulnerable populations and marginalized communities: Victims (powerless/trapped) — bear extraction risk through reduced policy responsiveness; no formal voice in coalition negotiations; cannot exit political system
 *   - Parliamentary institution: Institutional actor (institutional/arbitrage) — maintains legislative ritual while real power concentrates in confidence mechanics; exhibits piton characteristics (degraded function, maintained through inertia)
 *   - Analytical observer (comparative government): Observer (analytical/analytical) — sees minority government as legitimate institutional form and coordination mechanism; views scaffold as normal solution in proportional systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(portugal_government_stability_ad, 0.28).
domain_priors:suppression_score(portugal_government_stability_ad, 0.42).
domain_priors:theater_ratio(portugal_government_stability_ad, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(portugal_government_stability_ad, extractiveness, 0.28).
narrative_ontology:constraint_metric(portugal_government_stability_ad, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(portugal_government_stability_ad, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(portugal_government_stability_ad, scaffold).
narrative_ontology:human_readable(portugal_government_stability_ad, "The AD Minority Government Stability (The \"Presidential\" Scaffold)").
narrative_ontology:topic_domain(portugal_government_stability_ad, "political/governance").

domain_priors:requires_active_enforcement(portugal_government_stability_ad).
narrative_ontology:has_sunset_clause(portugal_government_stability_ad).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(portugal_government_stability_ad, ad_coalition_parties).
narrative_ontology:constraint_beneficiary(portugal_government_stability_ad, portuguese_executive).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The AD coalition (PSD, CDS-PP, and ad-hoc independent support) experiences the minority government as a temporary coordination structure. Coalition partners are constrained by electoral mathematics but see a defined sunset: either parliamentary elections occur (scheduled or early), or the coalition's support base shifts. Enforcement is moderate and necessary to maintain discipline, but the partners perceive the arrangement as negotiable, not permanent extraction.
constraint_indexing:constraint_classification(portugal_government_stability_ad, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The executive branch experiences the minority government as coordination with significant benefits. The governing structure allows policy-making without constant re-negotiation of coalition contracts. The executive has arbitrage options: form larger coalitions, negotiate issue-by-issue parliamentary support, or trigger elections. The constraint is primarily coordinative from this perspective — solving the problem of how a minority government functions in parliamentary systems.
constraint_indexing:constraint_classification(portugal_government_stability_ad, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Opposition parties experience the minority government as a mixed coordination-extraction constraint. They have moderate exit options: support or oppose government bills on a case-by-case basis, threaten confidence votes, or push toward elections. However, they also benefit from the minority government's structural weakness — it creates negotiating leverage for opposition demands on specific legislation. The extraction is asymmetric: the government extracts compliance; opposition extracts policy concessions. Both experience coordination benefits (legislative predictability) and extraction costs (policy compromises).
constraint_indexing:constraint_classification(portugal_government_stability_ad, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% The Portuguese presidency (primarily ceremonial but with latent dissolution powers) and constitutional framework see the minority government as a temporary structural scaffold. The presidency has constrained options: dissolve parliament (triggering elections), refuse bills, or remain ceremonial. The constitutional order provides a sunset mechanism: election cycles and constitutional amendment procedures. The scaffold is maintained through the presidential veto threat and the electoral calendar. Theater is moderate — the presidency's occasional public statements about governmental stability serve performative functions while the real constraint mechanism is the institutional threat of dissolution.
constraint_indexing:constraint_classification(portugal_government_stability_ad, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Vulnerable populations — low-income citizens, immigrant communities, precarious workers — experience the minority government as a snare. They have no formal influence on parliamentary support negotiations. Government stability depends on opposition tolerance or side-deals that prioritize coalition survival over vulnerable-group protection. Policy becomes subordinated to governmental survival. Exit options are minimal: these groups cannot withdraw from the political system or form credible parliamentary threats. They bear the extraction of reduced social policy responsiveness without commensurate coordination benefits.
constraint_indexing:constraint_classification(portugal_government_stability_ad, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The Portuguese legislative process, viewed over a civilizational timescale, exhibits piton characteristics. Parliamentary procedures (committees, plenary debates, confidence votes) persist even when actual decision-making has moved to backroom coalition negotiations. The legislature maintains ceremonial functions and theatrical debate while real power concentrates in executive-opposition side-dealing. Theater_ratio is elevated here because much legislative activity is performative — scripted speeches, procedural votes, committee hearings — while genuine legislative power resides in who controls the confidence equation. This perspective sees institutional inertia: the form of parliamentary democracy persists while its function has partially atrophied into a confidence-management game.
constraint_indexing:constraint_classification(portugal_government_stability_ad, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational/comparative perspective, the Portuguese minority government functions as a pure coordination mechanism. Minority governments are common in proportional representation systems and represent a solution to the collective action problem of coalition formation. The constraint solves: 'How do we govern without a legislative majority?' The answer is a coordination protocol (support thresholds, issue-by-issue negotiation, confidence mechanics). This perspective sees the minority government as a legitimate institutional form, not as a temporary pathology. The sunset is normal: elections occur at scheduled intervals (every 4 years) or when confidence fails. The scaffold's theater is minimal from this view — what appears as performative to a domestic observer is structural necessity to a comparative analyst.
constraint_indexing:constraint_classification(portugal_government_stability_ad, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portugal_government_stability_ad_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(portugal_government_stability_ad, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portugal_government_stability_ad, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(portugal_government_stability_ad, TR),
    TR >= 0.70.

:- end_tests(portugal_government_stability_ad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The AD minority government extracts executive control without a parliamentary majority, but the extraction is constrained and temporary. The coalition gains policy-making authority and executive patronage, but this is offset by their need to negotiate case-by-case support from opposition. The extraction is not severe because the opposition maintains credible exit options (confidence votes, legislative obstruction). The trend is upward (0.18 → 0.28) as the government accumulates side-deals and the opposition becomes normalized to supporting it. Suppression (0.42): Moderate. The minority government suppresses certain policy alternatives — those that would require opposition votes or trigger coalition collapse. However, suppression is not total; the government can still negotiate most legislation through opposition tolerance or electoral mandate. The opposition retains the ability to force elections or impose conditions. Theater ratio (0.58): Moderate-high. Parliamentary procedure includes significant performative elements — confidence debates, procedural votes, committee hearings — while actual power resides in off-stage confidence negotiations. However, the theater is not as high as a Piton would suggest (0.70+) because the underlying mechanism (the confidence vote) is real and consequential. The trend is upward as theatrical debate increasingly substitutes for legislative work. Claimed type: Scaffold matches the structural data. The constraint has a genuine sunset clause (elections within 4 years or earlier if confidence fails), beneficiaries (the AD coalition gains executive function), and temporary enforcement (opposition tolerance is contingent, not permanent).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is substantial and reveals the mandatrophy resolution. The AD coalition sees Rope (coordination with benefits). The executive sees Rope (enabling government function). The opposition sees Tangled Rope (mixed coordination benefit and extraction cost through forced compromises). Marginalized communities see Snare (policy becomes subordinated to government survival, no exit option). The presidency sees Scaffold (sunset is constitutional). Parliamentary institution sees Piton (ritual persists while function atrophies). The comparative observer sees Rope (legitimate proportional-system coordination). The gap between the beneficiary (Rope) and victim (Snare) perspectives is the entire analytical distance between coordination and extraction. This reveals the mandatrophy: the constraint IS both coordination and extraction, depending on position. The AD coalition coordinates. Marginalized communities are extracted from. Both perspectives are structurally valid.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position within the constraint. The AD coalition has institutional power and arbitrage options — they can shift coalition composition, trigger elections favorable to them, or renegotiate support. Their d-value is low (~0.15-0.25), placing them as beneficiaries with protective options. The opposition (PS, Left-Bloc) has moderate power and mobile options — they can shift support votes, threaten confidence, or organize alternative coalitions. Their d-value is moderate (~0.50-0.60), placing them as mixed beneficiary-victims: they benefit from leveraging power but also bear the cost of government instability if they push too hard. Marginalized communities have powerless status and trapped exit options — they cannot vote out the AD (insufficient numbers), cannot form credible alternative coalitions, and cannot withdraw from the political system. Their d-value is high (~0.85-0.95), placing them as full extraction targets. The presidency has institutional power and constrained exit — can dissolve but must respect constitutional norms. Their d-value is moderate-low (~0.30-0.40) as a technical enforcer rather than a beneficiary or victim. The directionality derivation from beneficiary (AD) and victim (marginalized communities) plus exit options automatically produces the right d-values without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through perspectival legitimacy. This is NOT a misclassified constraint — it is genuinely a Scaffold from some perspectives and Snare from others. The mandatrophy question is: 'Is the minority government a coordination mechanism (Scaffold/Rope) or an extraction mechanism (Snare/Tangled Rope)?' The answer is context-dependent and position-dependent. For the AD coalition and executive, it IS coordination — it solves the genuine collective action problem of governing without a majority. For marginalized communities, it IS extraction — policy becomes subordinated to coalition survival. For the opposition, it IS both: they coordinate government formation (preventing state instability) while extracting policy concessions (their vote trades for legislative priorities). The constraint resolves its mandatrophy by existing in multiple classification states simultaneously, which the perspectival framework captures. The theater_ratio (0.58) is moderately elevated but not at piton levels (0.70+), indicating that while performative elements exist (parliamentary debate, confidence posturing), the underlying mechanism (real confidence votes, real sunset through elections) is functional. The constraint is neither a pure coordination mechanism (Rope) nor a pure extraction mechanism (Snare) — it is a hybrid (Tangled Rope/Scaffold) that distributes coordination benefits and extraction costs unevenly across agents. The analytical observer's comparative perspective (seeing it as legitimate Rope) reflects the frame that minority governments are normal proportional-representation solutions, which is true at civilizational scale but misses the domestic extraction happening to vulnerable groups. The false summit test: if the comparative observer claims the minority government is a natural law of proportional systems (Mountain), they are naturalizing what is actually a contingent institutional arrangement that redistributes power unevenly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confidence_vote_trigger,
    'What event or sequence triggers a confidence vote that ends the minority government?',
    'Historical tracking of near-misses (e.g., 2023-2024 budget negotiations); identification of explicit red lines from opposition parties; analysis of parliamentary arithmetic thresholds for government defeat',
    'If trigger threshold is low (e.g., single major bill defeat): minority government becomes snare for marginalized groups with unpredictable policy. If trigger threshold is high (e.g., existential government priority): minority government remains scaffold with predictable sunset. Classification of opposition experience shifts from tangled_rope toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confidence_vote_trigger, empirical, 'Parliamentary thresholds and events that trigger government confidence votes').

omega_variable(
    opposition_coalition_credibility,
    'Can opposition parties credibly form an alternative government if they coordinate?',
    'Formal coalition negotiation attempts; public commitment statements from opposition parties; feasibility of policy alignment between PS and left-wing parties',
    'If opposition can form credible alternative: minority government is true scaffold (exit option exists). If opposition cannot coordinate: minority government becomes de facto snare (opposition trapped into supporting current government to prevent instability). Theater_ratio interpretation shifts: theatrical debate becomes actual constraint mechanism rather than performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_coalition_credibility, empirical, 'Whether opposition parties can form alternative governing coalitions').

omega_variable(
    european_integration_constraints,
    'Do EU fiscal and governance requirements (NextGenerationEU, fiscal rules, ESM conditions) externally enforce minority government stability regardless of domestic preferences?',
    'Analysis of EC conditionality statements; comparison of minority government survival rates in EU vs non-EU countries; tracking of EC intervention in minority government confidence votes',
    'If EU conditions are binding: minority government becomes externally-enforced tangled_rope (extraction via compliance with Brussels requirements, coordination via EU funding). If EU conditions are permissive: minority government remains domestically-determined scaffold. Beneficiary classification shifts from ad_coalition_parties to european_commission_interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_integration_constraints, empirical, 'Whether EU fiscal and governance requirements enforce minority government stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portugal_government_stability_ad, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ptad_tr_t0, portugal_government_stability_ad, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ptad_tr_t2, portugal_government_stability_ad, theater_ratio, 2, 0.52).
narrative_ontology:measurement(ptad_tr_t4, portugal_government_stability_ad, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(ptad_be_t0, portugal_government_stability_ad, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ptad_be_t2, portugal_government_stability_ad, base_extractiveness, 2, 0.24).
narrative_ontology:measurement(ptad_be_t4, portugal_government_stability_ad, base_extractiveness, 4, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(portugal_government_stability_ad, enforcement_mechanism).
narrative_ontology:affects_constraint(portugal_government_stability_ad, portuguese_budget_process).
narrative_ontology:affects_constraint(portugal_government_stability_ad, labor_market_regulation).
narrative_ontology:affects_constraint(portugal_government_stability_ad, healthcare_access_policy).

% DUAL FORMULATION NOTE:
% The AD minority government stability functions as a single constraint with perspectival multiplicity (Scaffold from coalition view, Snare from marginalized community view, Rope from executive view). This is not a decomposition into separate constraints — the base extractiveness and suppression are intrinsic to the single governmental arrangement. Rather, the multiple classifications reflect that the constraint simultaneously solves coordination problems (government formation) while creating extraction mechanisms (policy subordination to coalition survival). The network edges indicate downstream policy constraints that inherit the coalition's extraction and coordination dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
