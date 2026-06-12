% ============================================================================
% CONSTRAINT STORY: agenda_conditioning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agenda_conditioning, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: agenda_conditioning
 *   human_readable: Agenda Conditioning in Democratic Policy Formation
 *   domain: political_economy/democratic_theory/institutional_analysis
 *
 * SUMMARY:
 *   Agenda conditioning operates upstream of the observable
 *   preference-aggregation process in democratic systems. Measured
 *   rich-middle preference agreement on proposed policies may be an artifact:
 *   policies that would split rich and middle preferences are filtered out
 *   before reaching the survey instrument or legislative agenda. The
 *   constraint's primary mechanism is the veto: institutional chokepoints
 *   (Senate filibuster, committee gatekeeping, judicial review, executive
 *   veto, primary system donor filtering) allow veto holders to exclude
 *   proposals before they become 'live' policy options. Responsiveness
 *   studies that compare survey preferences to legislative outcomes measure
 *   only the post-filter policy space, systematically missing non-decisions.
 *   The constraint has intensified over the 50-year interval: base
 *   extractiveness rose from 0.45 to 0.68 as veto points proliferated and
 *   hardened (filibuster use increased, judicial appointments became more
 *   ideological, campaign finance concentration grew). Theater ratio rose
 *   from 0.35 to 0.58 as the democratic responsiveness measurement apparatus
 *   expanded while continuing to treat the agenda as exogenous. Suppression
 *   requirement rose from 0.55 to 0.72 as enforcement mechanisms matured: the
 *   two-party duopoly hardened, media gatekeeping consolidated, and the
 *   Overton window narrowed around veto-holder-acceptable positions.
 *
 * KEY AGENTS:
 *   - Excluded Policy Beneficiaries: Primary victim (powerless/trapped) — policies that would benefit them never reach the agenda; structurally invisible in responsiveness datasets
 *   - Median Voter: Secondary victim (moderate/constrained) — can vote but only among pre-filtered options; measured preferences may be endogenous to the constrained agenda
 *   - Veto Holders: Primary beneficiary (institutional/arbitrage) — use institutional chokepoints to filter the agenda; experience the constraint as coordination
 *   - Status Quo Beneficiaries: Secondary beneficiary (institutional/arbitrage) — protected from destabilizing proposals; interpret agenda exclusion as evidence of illegitimacy
 *   - Reform Coalition: Mixed position (organized/constrained) — achieve incremental wins but most ambitious goals never reach debate; tangled rope experience
 *   - Democratic Responsiveness Measurement Apparatus: Institutional observer (institutional/constrained) — measures post-filter responsiveness; acknowledges limitation but lacks methodology to measure non-decisions; piton perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing agenda conditioning as inherent to collective choice rather than recognizing it as contingent institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agenda_conditioning, 0.68).
domain_priors:suppression_score(agenda_conditioning, 0.72).
domain_priors:theater_ratio(agenda_conditioning, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agenda_conditioning, extractiveness, 0.68).
narrative_ontology:constraint_metric(agenda_conditioning, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(agenda_conditioning, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(agenda_conditioning, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(agenda_conditioning, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agenda_conditioning, snare).
narrative_ontology:human_readable(agenda_conditioning, "Agenda Conditioning in Democratic Policy Formation").
narrative_ontology:topic_domain(agenda_conditioning, "political_economy/democratic_theory/institutional_analysis").

domain_priors:requires_active_enforcement(agenda_conditioning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agenda_conditioning, veto_holders).
narrative_ontology:constraint_beneficiary(agenda_conditioning, status_quo_beneficiaries).
narrative_ontology:constraint_victim(agenda_conditioning, excluded_policy_beneficiaries).
narrative_ontology:constraint_victim(agenda_conditioning, median_voter).
narrative_ontology:constraint_victim(agenda_conditioning, democratic_responsiveness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(agenda_conditioning, reform_coalition).
narrative_ontology:constraint_victim(agenda_conditioning, reform_coalition).
narrative_ontology:constraint_vindicates(agenda_conditioning, preference_aggregation_sufficiency).
narrative_ontology:constraint_vindicates(agenda_conditioning, revealed_preference_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose interests would be served by policies that never reach the legislative agenda. They participate in surveys and vote in elections, but the options presented to them have already been filtered through veto points. Their preferences appear in responsiveness studies only when they happen to align with veto-holder-acceptable positions. No exit: cannot vote for policies that are never proposed, cannot organize around options that are never debated, cannot migrate to jurisdictions with fundamentally different agenda structures (all advanced democracies have veto points, though distributed differently).
narrative_ontology:constraint_stakeholder(agenda_conditioning, excluded_policy_beneficiaries, payer,
    powerless, biographical, trapped, national).

% The median voter in income and ideology space. Can vote and participate in surveys, but the choice set presented has been pre-filtered. The measured rich-middle preference agreement may be an artifact: policies that would split their preferences from wealthy voters' preferences are excluded upstream. Exit options constrained by two-party duopoly, geographic immobility, and the fact that all feasible destinations have similar (though differently configured) veto structures. Experiences democratic responsiveness, but responsiveness to a truncated policy space.
narrative_ontology:constraint_stakeholder(agenda_conditioning, median_voter, payer,
    moderate, biographical, constrained, national).

% Actors with structural veto power over the legislative agenda: Senate filibuster participants, committee chairs, median Supreme Court justices, executive veto wielders, primary system mega-donors. They do not need to vote down proposals; they prevent proposals from reaching a vote. Experience the constraint as coordination: the agenda-setting process filters out destabilizing policies, allowing them to coordinate on acceptable incremental reforms. Arbitrage exit: can shift resources across jurisdictions, lobby at multiple levels of government, or relocate if any single venue becomes hostile.
narrative_ontology:constraint_stakeholder(agenda_conditioning, veto_holders, agenda_setter,
    institutional, immediate, arbitrage, national).

% Actors who benefit from existing policy arrangements and are protected by the agenda filter even if they do not directly wield veto power. Includes incumbent firms in regulated industries, beneficiaries of current tax structures, holders of appreciating assets. The constraint prevents destabilizing proposals from reaching debate, which they interpret as evidence that such proposals lack legitimacy or popular support. Arbitrage exit: can adapt to incremental policy changes while the agenda filter blocks structural reforms that would threaten their position.
narrative_ontology:constraint_stakeholder(agenda_conditioning, status_quo_beneficiaries, beneficiary,
    institutional, biographical, arbitrage, national).

% Organized advocacy groups, progressive and populist movements, policy entrepreneurs pushing for structural reforms. Achieve incremental wins (some proposals do advance through the filter), but their most ambitious goals are systematically excluded. Experience both coordination (the system provides venues for advocacy and incremental change) and extraction (the veto filter blocks transformative policies). Constrained exit: can organize, build coalitions, and shift focus across issue areas, but cannot bypass the institutional chokepoints that filter the agenda.
narrative_ontology:constraint_stakeholder(agenda_conditioning, reform_coalition, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(agenda_conditioning, reform_coalition, beneficiary).

% Academic researchers and policy analysts who measure democratic responsiveness by comparing survey preferences to legislative outcomes. Acknowledge the agenda-setting problem in principle (the literature discusses non-decisions and agenda bias) but continue to publish studies that treat the agenda as exogenous because the alternative (measuring counterfactual policy spaces) lacks established methodology. The measurement apparatus is partly performative: produces actionable findings within its scope but systematically misses the upstream filter. Constrained exit: can shift research focus or methodology, but career incentives (publication, tenure, grant funding) favor working within the established paradigm.
narrative_ontology:constraint_stakeholder(agenda_conditioning, democratic_responsiveness_scholars, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(agenda_conditioning, veto_holders).
narrative_ontology:fixing_cost_class(agenda_conditioning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The agenda-setting process solves the collective action problem of reducing an infinite policy space to a finite set of legislative proposals that can be debated and voted on. Without some filtering mechanism, legislatures would face decision paralysis.
% TRANSFER_FUNCTION: The constraint transfers agenda-setting power from the median voter to veto holders. It moves policy outcomes toward veto-holder-preferred positions by excluding proposals that veto holders would block if they reached a vote. The transfer is not direct resource extraction but rather the preservation of existing resource distributions by preventing redistributive policies from reaching debate.
% ABSENT_VOICES: Excluded policy beneficiaries — those whose interests would be served by policies that never reach the agenda — are structurally absent from the responsiveness measurement apparatus. They appear in surveys as respondents but their preferences are recorded only over the filtered policy space. When they express support for options outside the Overton window, their responses are often coded as 'extreme' or 'unrealistic' rather than as evidence of unmet demand. They are absent from the legislative process not because they choose not to participate but because the participation venues (voting, lobbying, campaign contributions) operate over a pre-filtered choice set.
% DISAPPEARANCE_RATIONALE: If the agenda filter disappeared overnight — if all feasible policies could reach legislative debate without passing through veto points — the policy space would expand dramatically. Proposals currently excluded (wealth taxes, single-payer healthcare, campaign finance restrictions, labor law reforms, financial transaction taxes) would become live options. Measured rich-middle preference agreement would likely decline as the full range of redistributive policies entered the choice set. Legislative outcomes would shift toward median voter preferences over the expanded agenda rather than veto-holder preferences over the filtered agenda. The constraint's disappearance would rearrange the distribution of policy outcomes, not merely the process by which existing outcomes are reached.
% FOUNDING_PROBLEM: The founding problem was decision paralysis in large-scale democratic systems: without some mechanism to structure and limit the agenda, legislatures would face an unmanageable number of proposals and could not reach decisions efficiently. The Federalist Papers explicitly defended institutional chokepoints (bicameralism, executive veto, staggered terms) as necessary to prevent hasty or ill-considered legislation. The problem was framed as one of quality control and deliberation: filtering the agenda would improve legislative outcomes by ensuring that only well-vetted, broadly acceptable proposals reached a vote.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested along two dimensions. First, whether decision paralysis remains a genuine risk: critics argue that modern legislatures face the opposite problem (gridlock, not overproduction of legislation), and that the veto points now prevent necessary action rather than preventing hasty action. Defenders argue that the volume of potential legislation has grown with state capacity, and that filtering remains necessary. Second, whether the current distribution of veto points serves the stated function (quality control) or a different function (protecting veto-holder interests): critics point to the systematic exclusion of popular redistributive policies as evidence that the filter serves extraction rather than deliberation. Corroboration sources: (1) Defenders of the status quo include constitutional originalists, public choice theorists, and beneficiaries of the current system (Business Roundtable, Chamber of Commerce statements on regulatory process). (2) Critics include democratic reform advocates (Fix the Senate coalition, National Popular Vote Interstate Compact), scholars documenting the rich-poor responsiveness gap (Gilens, Page & Gilens), and excluded policy beneficiaries (labor unions, progressive advocacy groups). (3) The empirical record is mixed: some veto points demonstrably prevent low-quality legislation (judicial review of unconstitutional laws), while others demonstrably block popular, well-vetted proposals (Senate filibuster of voting rights legislation, minimum wage increases, climate policy).
narrative_ontology:disappearance_verdict(agenda_conditioning, world_rearranges).
narrative_ontology:founding_problem_status(agenda_conditioning, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED POLICY BENEFICIARIES (SNARE) — Policies that would benefit this group never reach the agenda. The constraint operates upstream of the observable preference-aggregation process, so their interests are structurally invisible in responsiveness studies. No exit: cannot vote for options that are never proposed. Maximum extraction: bear full cost of non-decisions while the measurement apparatus records only decisions that cleared the veto filter.
constraint_indexing:constraint_classification(agenda_conditioning, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MEDIAN VOTER (SNARE) — Experiences the constraint as a narrowed choice set. Can vote, but only among pre-filtered options. The measured rich-middle preference agreement is an artifact of upstream conditioning: policies that would split rich and middle never reach the survey instrument. Exit options constrained by the two-party system and geographic immobility. High extraction: the democratic responsiveness they experience is to a truncated policy space.
constraint_indexing:constraint_classification(agenda_conditioning, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VETO HOLDERS (ROPE) — Experience the constraint as coordination: the agenda-setting process filters out policies that would destabilize their position, allowing them to coordinate on acceptable reforms while blocking unacceptable ones. Arbitrage exit: can shift resources, lobby, or relocate if any single jurisdiction becomes hostile. Net beneficiaries: the constraint runs toward them, not away from them.
constraint_indexing:constraint_classification(agenda_conditioning, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATUS QUO BENEFICIARIES (ROPE) — The constraint protects existing arrangements from challenge. Experience it as a stability mechanism: the agenda naturally excludes destabilizing proposals, which they interpret as evidence that such proposals lack legitimacy. Arbitrage exit: can adapt to incremental changes while blocking structural reforms. Low effective extraction: the constraint subsidizes their position.
constraint_indexing:constraint_classification(agenda_conditioning, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (TANGLED ROPE) — Organized advocacy groups see both coordination and extraction. The agenda-setting process provides a venue for incremental reforms (coordination function: some proposals do advance), but the veto filter systematically excludes their most ambitious goals. Constrained exit: can organize, lobby, and build coalitions, but cannot bypass the institutional chokepoints. Mixed experience: some wins, but the most transformative policies never reach debate.
constraint_indexing:constraint_classification(agenda_conditioning, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PREFERENCE AGGREGATION VIEW (MOUNTAIN) — From a civilizational perspective rooted in public choice theory and social choice frameworks, some agenda constraint is inherent to any decision-making system: infinite policy spaces must be reduced to finite choice sets, and any reduction mechanism will favor some interests over others. This perspective sees the constraint as an immutable property of collective decision-making itself. However, the structural data contradicts this classification — the engine will compute this as a false summit, revealing that the 'inherent to democracy' framing naturalizes what is actually a contingent institutional design that benefits identifiable veto holders.
constraint_indexing:constraint_classification(agenda_conditioning, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: DEMOCRATIC RESPONSIVENESS MEASUREMENT APPARATUS (PITON) — The academic and policy infrastructure that measures democratic responsiveness by comparing survey preferences to legislative outcomes treats the agenda as given. The measurement ritual persists through institutional inertia (journals, tenure standards, grant funding) despite its known limitation: it cannot detect non-decisions. The apparatus sees its own process as degraded — researchers acknowledge the agenda-setting problem but continue to publish responsiveness studies because the alternative (measuring the unmeasured) lacks established methodology. Theater ratio reflects that the measurement produces actionable findings within its scope but systematically misses the upstream filter.
constraint_indexing:constraint_classification(agenda_conditioning, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agenda_conditioning_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agenda_conditioning, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agenda_conditioning, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agenda_conditioning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agenda_conditioning, TR),
    TR >= 0.70.

:- end_tests(agenda_conditioning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Veto holders capture substantial benefits by excluding policies that would redistribute resources or constrain their autonomy. The extraction operates through non-decisions: policies that never reach debate, never get voted on, never appear in responsiveness studies. The value reflects that a large portion of the feasible policy space is excluded, and the exclusion systematically favors veto holders over excluded policy beneficiaries. The rising trajectory (0.45 → 0.68 over 50 years) reflects proliferation and hardening of veto points. Suppression (0.72): High. Multiple enforcement mechanisms: institutional chokepoints (filibuster, committee system, judicial review), two-party duopoly limiting ballot access, media gatekeeping, campaign finance barriers to outsider candidates, primary system donor filtering. The rising trajectory (0.55 → 0.72) reflects maturation of these mechanisms. Alternatives exist in principle (constitutional amendment, electoral realignment, third-party emergence) but face compounding barriers. Theater ratio (0.58): Moderate-high. The democratic responsiveness measurement apparatus is partly performative: it produces actionable findings within its scope (comparing preferences to outcomes on the measured agenda) but systematically misses the upstream filter. The ritual persists because the alternative (measuring counterfactual agendas) lacks established methodology. The rising trajectory (0.35 → 0.58) reflects that the measurement apparatus expanded (more surveys, more studies, more sophisticated methods) while the core limitation (treating agenda as exogenous) remained unaddressed. Accessibility collapse (0.35): Low-moderate. Alternatives to the current agenda-setting process are visible and debated: parliamentary systems, proportional representation, campaign finance reform, filibuster abolition, ranked-choice voting. The constraint does not collapse alternatives as completely as a genuine natural law would. Resistance (0.62): Moderate-high. Substantial active resistance from reform coalitions, progressive and populist movements, academic critics of the responsiveness literature, and advocacy groups pushing excluded policies. The constraint must be actively defended through institutional inertia, media framing, and suppression of alternative electoral systems.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a three-way split in classification. Veto holders and status quo beneficiaries see coordination (Rope) — the agenda-setting process filters out destabilizing proposals, which they experience as legitimate preference aggregation. Excluded policy beneficiaries and the median voter see extraction (Snare) — the constraint operates upstream of their observable choices, systematically excluding policies that would benefit them. The reform coalition sees mixed coordination and extraction (Tangled Rope) — the system provides a venue for incremental reforms but blocks transformative change. The democratic responsiveness measurement apparatus sees its own degraded ritual (Piton) — continues to measure post-filter responsiveness despite knowing it misses non-decisions, because the alternative lacks established methodology. The analytical observer risks seeing an immutable natural law (Mountain) — agenda conditioning is inherent to collective choice — but the structural data reveals this as a false summit: the specific veto points and their distribution are contingent institutional design choices, not laws of political physics. The gap between the beneficiary's rope and the victim's snare is the core measurement the framework exists to take: the same structural phenomenon appears as coordination from above and extraction from below.
 *
 * DIRECTIONALITY LOGIC:
 *   Veto holders and status quo beneficiaries are declared beneficiaries with institutional power and arbitrage exit options. The engine derives low d values for these agents, producing low or negative effective extraction (they experience the constraint as coordination or subsidy). Excluded policy beneficiaries are declared victims with powerless standing and trapped exit options. The engine derives high d values, producing maximum effective extraction (they bear the full cost of non-decisions with no exit path). The median voter is a declared victim with moderate power and constrained exit options. The engine derives a moderate-high d value, producing substantial but not maximal effective extraction (they experience a narrowed choice set but retain some agency through voting). The reform coalition is neither pure beneficiary nor pure victim — they achieve some wins but face systematic exclusion of their most ambitious goals. The engine will derive a mid-range d value from their organized power and constrained exit, producing the mixed extraction characteristic of tangled rope. The democratic responsiveness measurement apparatus is not a beneficiary or victim of the constraint itself (it measures the constraint's effects rather than experiencing extraction), so it receives no directionality override. Its piton classification derives from the theater gate rather than from experienced extraction. The analytical observer's mountain classification is perspectival — the false summit detector will identify it as naturalization of contingent institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that agenda conditioning is simultaneously a coordination mechanism (from the veto holder perspective) and an extraction mechanism (from the excluded policy beneficiary perspective). The mandate — democratic responsiveness, preference aggregation — has not outlived its function from the veto holder's seat: the system continues to aggregate preferences over the filtered agenda, and that aggregation serves their interests. But the mandate HAS outlived its function from the excluded beneficiary's seat: the responsiveness they experience is to a truncated policy space that systematically excludes their interests. The mandatrophy is resolved not by declaring one perspective correct but by recognizing that both are structurally valid readings of the same institutional arrangement. The analytical observer's mountain classification (agenda conditioning is inherent to collective choice) is the naturalization that the framework exists to detect: it treats a contingent distribution of veto points as a law of political physics, thereby legitimizing the extraction the powerless agents experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_agenda_observability,
    'Can the full feasible policy space be reconstructed from historical records, or is the set of never-proposed policies inherently unobservable?',
    'Comparative analysis across jurisdictions with different veto structures; historical analysis of policy proposals that disappeared from agenda vs those that remained live; examination of policy ideas circulating in advocacy networks that never reached legislative consideration',
    'If observable: the constraint''s extractiveness can be quantified by comparing measured responsiveness to counterfactual responsiveness over the full policy space. If unobservable: the constraint''s operation remains partly hidden, and measured rich-middle agreement may be an artifact with unknown magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_agenda_observability, empirical, 'Whether the counterfactual agenda is empirically reconstructable').

omega_variable(
    veto_holder_identity_stability,
    'Are veto holders a stable class across policy domains, or does veto capacity vary by issue area such that different groups hold veto power over different parts of the agenda?',
    'Cross-domain analysis of which actors successfully block proposals in different policy areas (healthcare, taxation, labor, environmental regulation); network analysis of lobbying and campaign finance flows',
    'If stable: the constraint is a coherent extraction mechanism benefiting a unified class. If variable: the constraint is better modeled as a collection of domain-specific snares with different beneficiaries, and the ''veto holder'' category is an analytical convenience rather than a structural fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_holder_identity_stability, empirical, 'Whether veto capacity is unified or domain-specific').

omega_variable(
    preference_endogeneity,
    'To what extent are measured middle-class preferences endogenous to the constrained agenda — do people prefer what they see as feasible, or does the agenda reflect pre-existing preferences?',
    'Longitudinal surveys tracking preference formation; experiments presenting respondents with policy options outside the current Overton window; analysis of preference shifts following exogenous agenda expansions (e.g., after electoral realignments or crisis events)',
    'If strongly endogenous: the measured rich-middle agreement is largely an artifact of adaptive preference formation, and the constraint''s extractiveness is higher than preference-based measures suggest. If weakly endogenous: measured preferences are a reasonable proxy for interests, and the constraint''s extraction operates primarily through agenda restriction rather than preference manipulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preference_endogeneity, empirical, 'Whether measured preferences are endogenous to the agenda').

omega_variable(
    natural_law_vs_institutional_design,
    'Is agenda conditioning an inherent feature of any collective decision-making system (mountain), or a contingent result of specific institutional design choices (snare)?',
    'Comparative institutional analysis: do polities with different veto structures (parliamentary vs presidential systems, unicameral vs bicameral legislatures, different judicial review regimes) show different degrees of agenda conditioning? Historical analysis: did the U.S. system''s agenda-conditioning properties change after specific institutional reforms (17th Amendment, filibuster rules changes, primary system evolution)?',
    'If inherent: the analytical observer''s mountain classification is correct, and the constraint is a natural law of collective choice. If contingent: the mountain classification is a false summit, naturalizing institutional arrangements that benefit veto holders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_design, conceptual, 'Whether agenda conditioning is natural law or institutional artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agenda_conditioning, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agenda_cond_theater_1970, agenda_conditioning, theater_ratio, 0, 0.35).
narrative_ontology:measurement(agenda_cond_theater_1980, agenda_conditioning, theater_ratio, 10, 0.4).
narrative_ontology:measurement(agenda_cond_theater_1990, agenda_conditioning, theater_ratio, 20, 0.45).
narrative_ontology:measurement(agenda_cond_theater_2000, agenda_conditioning, theater_ratio, 30, 0.5).
narrative_ontology:measurement(agenda_cond_theater_2010, agenda_conditioning, theater_ratio, 40, 0.55).
narrative_ontology:measurement(agenda_cond_theater_2020, agenda_conditioning, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(agenda_cond_extract_1970, agenda_conditioning, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(agenda_cond_extract_1980, agenda_conditioning, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(agenda_cond_extract_1990, agenda_conditioning, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(agenda_cond_extract_2000, agenda_conditioning, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(agenda_cond_extract_2010, agenda_conditioning, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(agenda_cond_extract_2020, agenda_conditioning, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(agenda_cond_suppress_1970, agenda_conditioning, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(agenda_cond_suppress_1990, agenda_conditioning, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(agenda_cond_suppress_2010, agenda_conditioning, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(agenda_cond_suppress_2020, agenda_conditioning, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agenda_conditioning, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% Agenda conditioning is downstream of veto_asymmetry (the structural distribution of veto points) but represents a distinct constraint. Veto asymmetry describes the institutional architecture; agenda conditioning describes the epistemic consequence (non-decisions are unmeasured). The two constraints have different beneficiary structures: veto asymmetry benefits those who hold formal veto power; agenda conditioning benefits those whose interests align with the status quo, whether or not they hold formal veto power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
