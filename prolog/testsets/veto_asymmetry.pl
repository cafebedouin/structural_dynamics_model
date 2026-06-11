% ============================================================================
% CONSTRAINT STORY: veto_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_veto_asymmetry, []).

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
 *   constraint_id: veto_asymmetry
 *   human_readable: Veto Asymmetry in Democratic Policy Formation
 *   domain: political_economy/democratic_theory/institutional_analysis
 *
 * SUMMARY:
 *   Veto asymmetry describes the structural inequality in democratic policy
 *   influence arising from the lower cost of blocking policy change versus
 *   enacting new policy. In systems with multiple veto points (bicameral
 *   legislatures, executive veto, judicial review, filibuster rules,
 *   committee gatekeeping), blocking requires capturing only one chokepoint
 *   while enactment requires assembling majority coalitions across the entire
 *   legislative chain. This cost differential creates systematic
 *   wealth-correlated policy bias: concentrated wealth holders can afford to
 *   purchase chokepoint vetoes (through campaign contributions, lobbying,
 *   regulatory capture) at costs far below what diffuse majority coalitions
 *   would need to spend to overcome those vetoes and enact policy. The
 *   constraint exhibits genuine coordination function (Madisonian stability,
 *   protection against policy volatility) layered with substantial extraction
 *   (suppression of majority-preferred redistributive policy, chokepoint
 *   rent-seeking). The theater_ratio (0.58) reflects increasing performative
 *   democratic participation (voting, public comment periods, legislative
 *   hearings) whose outcomes are substantially predetermined by chokepoint
 *   capture. Measurements show extraction and suppression rising from
 *   1970-2015 as wealth concentration increased, then plateauing as the
 *   constraint reached its current mature form.
 *
 * KEY AGENTS:
 *   - Diffuse Majority Coalitions: Primary victim (powerless/trapped) — cannot organize at scale to overcome chokepoint vetoes; per-capita benefit of any single policy win is below coordination cost threshold
 *   - Concentrated Wealth Holders: Primary beneficiary (institutional/arbitrage) — purchase chokepoint vetoes at costs far below full-chain enactment costs; extraction flows toward this agent through blocked redistributive policy
 *   - Chokepoint Gatekeepers: Secondary beneficiary (institutional/arbitrage) — committee chairs, filibuster-wielding senators, agency heads who extract rents from both enactment-seekers and blockers
 *   - Issue-Specific Advocacy Coalitions: Mixed position (moderate/constrained) — sometimes benefit from veto asymmetry when defending achieved gains, sometimes pay extraction cost when seeking new enactments
 *   - Reform Coalition: Organized agents (organized/constrained) — see structural solution (constitutional amendment, filibuster abolition) but face high barriers to implementation; constrained by the constraint's self-defense mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible hybrid of coordination and extraction; risks either naturalizing the asymmetry as necessary democratic stability or dismissing all blocking power as pure extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(veto_asymmetry, 0.62).
domain_priors:suppression_score(veto_asymmetry, 0.68).
domain_priors:theater_ratio(veto_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(veto_asymmetry, extractiveness, 0.62).
narrative_ontology:constraint_metric(veto_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(veto_asymmetry, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(veto_asymmetry, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(veto_asymmetry, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(veto_asymmetry, tangled_rope).
narrative_ontology:human_readable(veto_asymmetry, "Veto Asymmetry in Democratic Policy Formation").
narrative_ontology:topic_domain(veto_asymmetry, "political_economy/democratic_theory/institutional_analysis").

domain_priors:requires_active_enforcement(veto_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(veto_asymmetry, concentrated_wealth_holders).
narrative_ontology:constraint_beneficiary(veto_asymmetry, chokepoint_gatekeepers).
narrative_ontology:constraint_victim(veto_asymmetry, diffuse_majority_coalitions).
narrative_ontology:constraint_victim(veto_asymmetry, policy_enactment_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(veto_asymmetry, issue_advocacy_coalitions).
narrative_ontology:constraint_victim(veto_asymmetry, issue_advocacy_coalitions).
narrative_ontology:constraint_victim(veto_asymmetry, reform_coalitions).
narrative_ontology:constraint_vindicates(veto_asymmetry, madisonian_faction_control_doctrine).
narrative_ontology:constraint_vindicates(veto_asymmetry, negative_liberty_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large coalitions seeking redistributive policy or regulatory enactment. Cannot organize at scale because per-capita benefit of any single policy win is below the coordination cost threshold set by organization_floor. Bear the cost of blocked majority-preferred policy but cannot overcome chokepoint vetoes. Trapped by the structural math: even when 60-70% of the population prefers a policy, the cost of organizing that supermajority exceeds what any individual member would gain from the policy's enactment.
narrative_ontology:constraint_stakeholder(veto_asymmetry, diffuse_majority_coalitions, payer,
    powerless, biographical, trapped, national).

% Individuals and corporations with sufficient wealth concentration to purchase chokepoint vetoes. Capture committee chairs, filibuster-wielding senators, and regulatory agencies at costs far below what diffuse coalitions would need to spend to overcome those vetoes. Benefit from blocked redistributive policy and regulatory forbearance. Can arbitrage between chokepoints and between blocking and allowing based on which produces higher return.
narrative_ontology:constraint_stakeholder(veto_asymmetry, concentrated_wealth_holders, beneficiary,
    institutional, biographical, arbitrage, continental).

% Committee chairs, filibuster-wielding senators, regulatory agency heads, and other institutional actors who control single points in the legislative chain. Set the agenda by deciding what reaches a vote and extract rents from both enactment-seekers (payment to allow passage) and blockers (payment to block). Benefit from the asymmetry itself — the more veto points exist, the more valuable each gatekeeper position becomes.
narrative_ontology:constraint_stakeholder(veto_asymmetry, chokepoint_gatekeepers, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(veto_asymmetry, chokepoint_gatekeepers, beneficiary).

% Single-issue advocacy groups (environmental, civil rights, labor, industry associations) that have achieved some policy wins and now defend them. Benefit from veto asymmetry when blocking threats to achieved gains (cheaper to defend one chokepoint than to maintain full-chain coalition). Pay extraction cost when seeking new enactments (must overcome multiple vetoes). Constrained by resource limitations but have more organization capacity than fully diffuse coalitions.
narrative_ontology:constraint_stakeholder(veto_asymmetry, issue_advocacy_coalitions, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(veto_asymmetry, issue_advocacy_coalitions, beneficiary).

% Campaign finance reform groups, anti-gerrymandering advocates, filibuster abolitionists, and other movements seeking to reduce veto points or limit chokepoint capture. See the structural solution (constitutional amendment, norm change, institutional redesign) but face high barriers to implementation. Constrained by the constraint's self-defense mechanism — veto asymmetry makes its own removal expensive by requiring supermajority coalition formation to overcome the chokepoints that defend the current architecture.
narrative_ontology:constraint_stakeholder(veto_asymmetry, reform_coalitions, payer,
    organized, generational, constrained, national).

% Comparative political economists and democratic theorists observing veto asymmetry across multiple democracies and time periods. See both the genuine coordination function (Madisonian stability, protection against policy volatility) and the substantial extraction (wealth-correlated policy bias, suppression of majority-preferred redistribution). Neither collecting from nor paying into the constraint — observing the structure from outside any particular national system.
narrative_ontology:constraint_stakeholder(veto_asymmetry, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(veto_asymmetry, concentrated_wealth_holders).
narrative_ontology:fixing_cost_class(veto_asymmetry, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Veto asymmetry coordinates two distinct problems: (1) Madisonian stability — preventing hasty policy changes and protecting against some forms of majoritarian tyranny by requiring broad consensus for enactment. (2) Wealth-holder collective action — solving the coordination problem of defending property rights and regulatory forbearance by concentrating defensive power at a few purchasable chokepoints rather than requiring continuous full-chain coalition maintenance.
% TRANSFER_FUNCTION: The arrangement transfers policy influence from diffuse majority coalitions to concentrated wealth holders. What moves: the ability to block or enact policy. From whom: from large coalitions whose per-capita benefit is below organization threshold. To whom: to wealth holders who can afford chokepoint capture costs and to the gatekeepers who control those chokepoints. The transfer is asymmetric — blocking costs far less than enacting, so influence flows toward those who prefer status quo (typically wealth holders defending against redistribution).
% ABSENT_VOICES: Diffuse majority coalitions are structurally excluded — not absent from the room but unable to speak effectively because their per-capita benefit from any single policy is below the coordination cost threshold. They vote, they petition, they comment in public hearings, but these participation rituals produce predetermined outcomes when chokepoints are captured. The exclusion is structural rather than procedural — the architecture ensures that even when 60-70% of the population prefers a policy, that preference cannot overcome a single well-funded chokepoint veto.
% DISAPPEARANCE_RATIONALE: If veto asymmetry disappeared overnight (unicameral legislature, no filibuster, easy amendment process, strict campaign finance limits), policy outcomes would rearrange substantially. Majority-preferred redistributive policies currently blocked by chokepoint vetoes would pass. Wealth holders would need to maintain full-chain coalitions rather than purchasing single chokepoints. Chokepoint gatekeepers would lose rent-extraction capacity. The rearrangement would be substantial and immediate — this is a constructed institutional architecture, not a natural feature of democracy.
% FOUNDING_PROBLEM: The Madisonian founding problem: how to prevent majoritarian tyranny and faction dominance in a large republic. Federalist 10's solution: fragment institutional power across multiple veto points so that no single faction can easily capture the entire system. The architecture was designed to make policy change difficult, requiring broad consensus and protecting minority rights (particularly property rights) against majority redistribution.
% FOUNDING_PROBLEM_CORROBORATION: The status is contested between two readings: (1) The founding problem is LIVE — majoritarian tyranny remains a threat, and veto points continue to serve their designed function of requiring broad consensus (corroborated by conservative legal scholars, Federalist Society, property rights advocates). (2) The founding problem is DEAD or transformed — the threat today is not majoritarian tyranny but minoritarian capture, and veto points now serve wealth concentration rather than faction control (corroborated by comparative political economists observing that other stable democracies with fewer veto points do not exhibit higher majoritarian tyranny, and by empirical studies showing wealth-correlated policy bias in high-veto-point systems). The corroboration is split along ideological and structural position lines — those who benefit from veto asymmetry attest the problem is live; those who pay the extraction cost attest it is dead.
narrative_ontology:disappearance_verdict(veto_asymmetry, world_rearranges).
narrative_ontology:founding_problem_status(veto_asymmetry, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIFFUSE MAJORITY COALITION (SNARE) — Cannot organize at scale to overcome chokepoint vetoes; bears full cost of blocked policy enactment. Trapped by coordination costs that exceed the per-capita benefit of any single policy win. Maximum experienced extraction — the structural position that reveals veto asymmetry as pure extraction mechanism.
constraint_indexing:constraint_classification(veto_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ISSUE-SPECIFIC ADVOCACY COALITION (TANGLED ROPE) — Constrained by resource requirements for sustained legislative campaigns but benefits from the same chokepoint architecture when defending existing policy. Experiences both coordination (when blocking threats to achieved gains) and extraction (when seeking new enactments). Mixed structural position — sometimes gatekeeper, sometimes supplicant.
constraint_indexing:constraint_classification(veto_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHOKEPOINT GATEKEEPER (ROPE) — Committee chairs, filibuster-wielding senators, regulatory agency heads. Benefits from veto asymmetry through rent extraction from both sides: payment to allow passage, payment to block. Experiences the constraint as pure coordination — the architecture creates a valuable service (agenda control) that both enactment-seekers and blockers demand. Net beneficiary with arbitrage-grade exit (can switch between blocking and allowing based on payment).
constraint_indexing:constraint_classification(veto_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONCENTRATED WEALTH HOLDER (ROPE) — Captures chokepoints at lower cost than buying full legislative chains. Experiences veto asymmetry as efficient coordination: the architecture solves the collective action problem of defending property rights and regulatory forbearance by concentrating defensive power at a few purchasable nodes. Low effective extraction because the constraint subsidizes this agent — extraction flows toward them, not away.
constraint_indexing:constraint_classification(veto_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: REFORM COALITION (TANGLED ROPE) — Organized agents (campaign finance reform groups, anti-gerrymandering advocates, filibuster abolitionists) see veto asymmetry as a solvable institutional design problem but face high barriers to constitutional amendment or norm change. Benefits from the same chokepoint architecture when blocking backsliding but pays extraction cost when seeking structural reform. Constrained exit — can build pressure but cannot unilaterally exit the system.
constraint_indexing:constraint_classification(veto_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, veto asymmetry exhibits both genuine coordination function (Madisonian stability, protection against majoritarian tyranny, transaction cost reduction for status quo defense) and substantial extraction (wealth-correlated policy influence, suppression of majority-preferred enactments, chokepoint rent-seeking). The constraint is not a natural law — alternative democratic architectures exist with different veto/enactment cost ratios — but neither is it pure extraction. Tangled rope classification reflects the irreducible hybrid: some of the blocking power genuinely coordinates against hasty change; some of it extracts rents from the diffuse majority.
constraint_indexing:constraint_classification(veto_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(veto_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(veto_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(veto_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(veto_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(veto_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Substantial. The constraint extracts from diffuse majority coalitions by suppressing majority-preferred policy (particularly redistributive policy) while enabling concentrated wealth holders to defend status quo at low cost. The value reflects that some blocking power serves genuine coordination (preventing hasty policy changes, protecting minority rights) but a significant fraction is extractive rent-seeking. The 0.62 value is higher than a pure coordination mechanism (rope ~0.15-0.25) but lower than a pure extraction mechanism (snare ~0.75-0.85) because the coordination function is real. Suppression (0.68): High. Substantial barriers to policy enactment include: coordination costs for diffuse coalitions that exceed per-capita benefits, wealth-correlated chokepoint capture costs, constitutional amendment difficulty, norm entrenchment around Madisonian stability rhetoric. Suppression has increased over the measurement interval as wealth concentration has risen. Theater_ratio (0.58): Moderate-high. Democratic participation rituals (voting, public comment, legislative hearings) increasingly produce predetermined outcomes due to chokepoint capture, but the theater is not total — some policy enactments do occur, and some chokepoints remain contestable. The ratio has increased as wealth concentration has made chokepoint capture more systematic. Accessibility_collapse (0.42): Moderate. Alternative democratic architectures exist (unicameral legislatures, proportional representation, no filibuster, shorter amendment processes) and are observable in other stable democracies, so the constraint is not a natural law. However, path dependency and constitutional entrenchment make alternatives difficult to access from within the current system. Resistance (0.71): High. The constraint faces sustained resistance from reform coalitions, progressive movements, and periodic populist backlash, indicating it is a constructed arrangement requiring active defense rather than a natural feature of democracy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural position determines classification. Diffuse majority coalitions see pure extraction (Snare) — they cannot organize to overcome vetoes and bear full cost of suppressed policy. Concentrated wealth holders see pure coordination (Rope) — the architecture efficiently solves their collective action problem of defending property rights. Chokepoint gatekeepers see coordination (Rope) — they provide a valuable service (agenda control) that both sides demand. Issue-specific advocacy coalitions and reform coalitions see mixed coordination-extraction (Tangled Rope) — they benefit from blocking power when defending gains but pay extraction costs when seeking change. The analytical observer sees irreducible hybrid (Tangled Rope) — genuine Madisonian stability coordination layered with substantial wealth-correlated extraction. The perspectival gap is not a disagreement about facts but a structural consequence of different positions in the extraction flow. The constraint simultaneously coordinates (protects against policy volatility) and extracts (suppresses majority-preferred redistribution) — which function dominates depends on where you sit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Diffuse majority coalitions are full victims (d → 1.0) — they bear the cost of blocked policy with no offsetting benefit and cannot exit due to coordination cost barriers. Concentrated wealth holders are full beneficiaries (d → 0.0) — they capture the value of blocked redistributive policy and can arbitrage between chokepoints. Chokepoint gatekeepers are beneficiaries (d → 0.1-0.2) — they extract rents from both sides but face some constraints from electoral accountability. Issue-specific advocacy coalitions have mixed directionality (d → 0.4-0.6) — they benefit when blocking threats to achieved policy but pay extraction costs when seeking new enactments; their position oscillates based on whether they are defending or advancing. Reform coalitions have moderate directionality (d → 0.5-0.6) — they pay extraction costs when blocked from structural reform but benefit from organization that reduces per-capita coordination costs. The analytical observer's directionality is neutral (d → 0.5) by construction — the analytical position experiences neither extraction nor subsidy, only observation of the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: Veto asymmetry resolves the mandatrophy by demonstrating that coordination and extraction are not mutually exclusive categories but can be structurally layered in the same constraint. The Madisonian stability function is genuine — chokepoint architecture does reduce policy volatility and protect against some forms of majoritarian tyranny. But this coordination function is layered with substantial extraction — the same architecture systematically suppresses majority-preferred redistributive policy and enables wealth-correlated chokepoint capture. The constraint is not 'really' coordination (rope) or 'really' extraction (snare) — it is irreducibly both (tangled rope). The analytical classification as tangled_rope reflects this structural hybridity. The false summit risk is naturalizing the entire asymmetry as necessary democratic stability (mountain) when the coordination/extraction ratio is contingent on wealth distribution and institutional design choices. Alternative architectures (fewer veto points, lower amendment thresholds, campaign finance limits) would preserve some coordination function while reducing extraction — the current ratio is not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'What fraction of veto asymmetry''s blocking power represents genuine Madisonian stability coordination versus extractive suppression of majority-preferred policy?',
    'Comparative institutional analysis: policy outcome distributions in democracies with different veto point architectures, controlling for wealth inequality. If high-veto-point systems produce similar policy distributions to low-veto-point systems when wealth concentration is controlled, the asymmetry is primarily extractive. If policy volatility increases substantially in low-veto-point systems regardless of wealth distribution, some coordination function is genuine.',
    'If coordination fraction > 0.6: analytical classification shifts toward rope (the blocking power is mostly legitimate stability mechanism). If coordination fraction < 0.3: analytical classification shifts toward snare (the blocking power is mostly extractive rent-seeking dressed in Madisonian language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Fraction of veto asymmetry representing coordination vs extraction').

omega_variable(
    chokepoint_cost_threshold,
    'At what wealth concentration level does chokepoint capture cost fall below the per-capita benefit threshold that would enable diffuse majority counter-organization?',
    'Empirical measurement of lobbying expenditures per chokepoint (committee chair races, filibuster-wielding senator campaigns, regulatory capture budgets) versus the per-capita cost of organizing diffuse coalitions at sufficient scale to overcome vetoes. Threshold is the Gini coefficient at which chokepoint costs equal or exceed distributed organization costs.',
    'If current wealth concentration is below threshold: veto asymmetry is a coordination problem (rope from more perspectives) — the diffuse majority could organize but chooses not to. If current concentration is above threshold: veto asymmetry is structural extraction (snare from more perspectives) — the diffuse majority cannot organize at any feasible cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chokepoint_cost_threshold, empirical, 'Wealth concentration threshold for chokepoint capture feasibility').

omega_variable(
    constitutional_amendment_sunset,
    'Is the reform coalition''s generational time horizon realistic, or is constitutional amendment to reduce veto points structurally infeasible under current wealth concentration?',
    'Historical analysis of successful constitutional amendments in high-inequality periods; game-theoretic modeling of amendment coalition formation costs under veto asymmetry (the constraint defends itself by making its own removal expensive).',
    'If amendment is feasible within 2-3 generations: reform coalition''s scaffold-like perception is justified. If amendment requires wealth deconcentration first (circular dependency): reform coalition is trapped in the same structure as the diffuse majority, and their ''organized'' classification overstates their agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_sunset, empirical, 'Feasibility of constitutional reform under veto asymmetry').

omega_variable(
    madisonian_naturalization,
    'Is the Federalist 10 framing (faction control via institutional fragmentation) a discovered principle of stable governance or a constructed justification for wealth-holder veto power?',
    'Cross-national comparison: do stable democracies with low veto-point counts (unicameral legislatures, no filibuster, proportional representation) exhibit higher policy volatility or majoritarian tyranny? If not, the Madisonian claim is naturalization of a contingent design choice.',
    'If Madisonian stability claim is empirically supported: veto asymmetry has genuine mountain-like properties (some blocking power is structural necessity). If unsupported: the ''stability'' framing is false summit — naturalization of extractive architecture as democratic necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(madisonian_naturalization, empirical, 'Whether Madisonian stability justification is natural law or constructed cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(veto_asymmetry, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(veto_asym_theater_1970, veto_asymmetry, theater_ratio, 0, 0.38).
narrative_ontology:measurement(veto_asym_theater_1985, veto_asymmetry, theater_ratio, 15, 0.45).
narrative_ontology:measurement(veto_asym_theater_2000, veto_asymmetry, theater_ratio, 30, 0.52).
narrative_ontology:measurement(veto_asym_theater_2015, veto_asymmetry, theater_ratio, 45, 0.57).
narrative_ontology:measurement(veto_asym_theater_2025, veto_asymmetry, theater_ratio, 55, 0.58).

% Extraction over time
narrative_ontology:measurement(veto_asym_extract_1970, veto_asymmetry, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(veto_asym_extract_1985, veto_asymmetry, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(veto_asym_extract_2000, veto_asymmetry, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(veto_asym_extract_2015, veto_asymmetry, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(veto_asym_extract_2025, veto_asymmetry, base_extractiveness, 55, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(veto_asym_suppress_1970, veto_asymmetry, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(veto_asym_suppress_1985, veto_asymmetry, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(veto_asym_suppress_2000, veto_asymmetry, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(veto_asym_suppress_2015, veto_asymmetry, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(veto_asym_suppress_2025, veto_asymmetry, suppression_requirement, 55, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(veto_asymmetry, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% Veto asymmetry is downstream of organization_floor (the mountain constraint that diffuse coalitions cannot organize below a minimum per-capita benefit threshold). The organization_floor sets the coordination cost barrier; veto_asymmetry exploits that barrier by concentrating blocking power at chokepoints whose capture cost is below the organization threshold for diffuse majorities. The two constraints are structurally distinct: organization_floor is a natural coordination limit (mountain), veto_asymmetry is a constructed institutional architecture that takes advantage of that limit (tangled_rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
