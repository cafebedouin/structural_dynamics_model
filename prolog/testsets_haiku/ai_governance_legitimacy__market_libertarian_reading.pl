% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: Market-Libertarian AI Governance Legitimacy (Property Rights as Pre-Political)
 *   domain: theological/political_theology/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates a market-libertarian reading of AI
 *   governance legitimacy. The reading claims that property rights are
 *   pre-political natural facts, that voluntary exchange is morally
 *   sovereign, that innovation flourishes under minimal collective mandates,
 *   and that dignity is protected through exit options and competitive
 *   discipline rather than centralized oversight. The reading explicitly
 *   rejects the Catholic encyclical's claim that economic freedom must be
 *   subordinated to common-good criteria and solidarity demands. It treats
 *   the Magisterium's subsidiarity principle as correct (decentralize where
 *   possible) but its solidarity principle as illegitimate coercion. The
 *   constraint describes the arrangement under THIS reading — not under the
 *   competing readings (magisterial, democratic, technocratic).
 *   Extractiveness is authored low (0.26) because the reading denies that
 *   property protection constitutes extraction from those without property;
 *   it is a natural arrangement, not a constructed transfer. Suppression is
 *   low (0.15) because the reading claims the constraint is self-enforcing
 *   through contract law, reputation, and voluntary participation — not
 *   through active coercive machinery. However, this low reading reflects the
 *   reading's own frame, not external validation. From the perspectives of
 *   communities facing coordination failures, workers in monopsony markets,
 *   and vulnerable populations, the same arrangement appears far more
 *   extractive and suppressive. The engine will compute per-seat
 *   classifications that diverge dramatically from the reading's own framing.
 *
 * KEY AGENTS:
 *   - entrepreneurs_and_investors: primary beneficiaries under this reading, structured with high exit options and arbitrage capacity
 *   - high_autonomy_individuals: secondary beneficiaries, protected by competitive discipline and exit options
 *   - workers_in_monopsony_labor_markets: primary victims, formally subject to exit but structurally trapped
 *   - communities_facing_coordination_failures: secondary victims, denied collective-mandate tools for addressing failures
 *   - vulnerable_populations: tertiary victims, lacking market power to shape governance and exposed to harms
 *   - catholic_magisterium: excluded authority, would demand subordination of freedom to common good
 *   - democratic_polities: excluded authority, would demand collective mandates and democratic consent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.26).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.15).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "Market-Libertarian AI Governance Legitimacy (Property Rights as Pre-Political)").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological/political_theology/technology_governance").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '0815e759-a060-41f6-ad2e-6ade14a975b8').
narrative_ontology:cs_kernel_codification('0815e759-a060-41f6-ad2e-6ade14a975b8', distributed).
narrative_ontology:cs_authority_grounding('0815e759-a060-41f6-ad2e-6ade14a975b8', extraction).
narrative_ontology:cs_interpretation_layer_present('0815e759-a060-41f6-ad2e-6ade14a975b8').
narrative_ontology:cs_reading_relation('0815e759-a060-41f6-ad2e-6ade14a975b8', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('0815e759-a060-41f6-ad2e-6ade14a975b8', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0815e759-a060-41f6-ad2e-6ade14a975b8', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_axiom('0815e759-a060-41f6-ad2e-6ade14a975b8', foundational, property_rights_are_pre_political).
narrative_ontology:cs_axiom_status(property_rights_are_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('0815e759-a060-41f6-ad2e-6ade14a975b8', property_rights_are_pre_political, deontological).
narrative_ontology:cs_axiom('0815e759-a060-41f6-ad2e-6ade14a975b8', foundational, voluntary_exchange_morality_supersedes_collective_mandate).
narrative_ontology:cs_axiom_status(voluntary_exchange_morality_supersedes_collective_mandate, holdable).
narrative_ontology:cs_axiom_grounding('0815e759-a060-41f6-ad2e-6ade14a975b8', voluntary_exchange_morality_supersedes_collective_mandate, deontological).
narrative_ontology:cs_axiom('0815e759-a060-41f6-ad2e-6ade14a975b8', secondary, solidarity_principle_as_illegitimate_coercion).
narrative_ontology:cs_axiom_status(solidarity_principle_as_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('0815e759-a060-41f6-ad2e-6ade14a975b8', solidarity_principle_as_illegitimate_coercion, deontological).
narrative_ontology:cs_reference_frame('0815e759-a060-41f6-ad2e-6ade14a975b8', natural_liberty_under_property_rights).
narrative_ontology:cs_drift_state('0815e759-a060-41f6-ad2e-6ade14a975b8', contemporary_regulatory_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0815e759-a060-41f6-ad2e-6ade14a975b8', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs_and_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, vulnerable_populations).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_as_natural_law).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_morality).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, exit_options_as_dignity_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate in AI development, deployment, and commercialization under the reading's framework. Benefit from unrestricted innovation cycles, property protection via contract law, minimal collective mandates or regulatory oversight. Their exit is high: they can migrate operations to jurisdictions that honor property rights and voluntary exchange. They are net beneficiaries of the constraint's enforcement (or rather, its non-enforcement: the constraint protects their freedom from collective claims on their innovations).
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs_and_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Access AI services through voluntary transactions in competitive markets. Dignity, in this reading, is protected through their ability to refuse participation and seek alternatives. They benefit from the discipline of competition: firms must serve their preferences or lose them to rivals. Exit options are their primary defense against predatory or exploitative arrangements.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Face concentrated AI-driven labor demand (single large employer or tight labor cartel) in their region or skill category. The reading's framework insists that their dignity is protected by exit options and market competition; the reality is that viable alternatives are foreclosed by geography, skill mismatch, or monopsony power itself. They bear the costs of the constraint's operation: their bargaining power is subordinated to property holders' freedom to deploy automation with minimal coordination or transition support.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets, payer,
    powerless, biographical, trapped, national).

% Confront problems that require collective action — industrial transition, AI safety standards, equitable access to training, mitigation of displacement — but the reading forbids collective mandates as coercive and illegitimate. Communities cannot tax, regulate, or require participation in coordination mechanisms. They are left with voluntary coordination, which fails when individual incentives diverge from collective good. They bear diffuse costs: degraded social cohesion, unmitigated risks, unequal opportunity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    organized, generational, constrained, regional).

% Lack market power to shape AI governance. The reading insists that dignity through exit is universal, but their exit options are minimal: they cannot afford alternative services, cannot relocate, cannot lobby effectively, and lack the technical expertise or capital to build alternatives. They bear extraction: algorithmic allocation of resources, denial of services, pricing discrimination, and exposure to harms where the market chooses not to protect them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, vulnerable_populations, payer,
    powerless, biographical, constrained, global).

% Issues encyclicals and moral teaching claiming authority over economic arrangements, including AI governance. This reading explicitly rejects the Magisterium's legitimacy to impose solidarity demands on markets. Were the Magisterium's voice heard on equal footing in the governance process, it would demand collective accountability, worker protections, and subordination of innovation to common-good criteria. It is excluded by the reading's architecture: market libertarianism does not admit religious authority over secular economic life.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, catholic_magisterium, excluded,
    institutional, civilizational, analytical, global).

% Would use collective mandate, regulation, and redistributive taxation to address coordination failures and protect vulnerable populations. This reading denies the legitimacy of their collective claims. Were democratic majorities to govern AI allocation and safety, they would impose requirements that this reading characterizes as coercive. They are excluded from the constraint's framework: market libertarianism holds that political legitimacy cannot override property rights, even when democratically enacted.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, democratic_polities, excluded,
    institutional, generational, analytical, national).

% Advocate for governance structures that maximize aggregate welfare and efficiency. They observe the market-libertarian reading as one framework among competing governance approaches. They take no sides but assess whether the constraint's operation produces stated outcomes and what tradeoffs it entails.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, technocratic_optimization_authorities, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not establish a coordination function; rather, it denies that collective mandate authority can legitimately impose coordination. The reading asserts that coordination should emerge from voluntary exchange and decentralized contract, not centralized authority. Where coordination is necessary, it arises through market discipline and reputation — entrepreneurs and investors coordinate innovation through competition, contract, and intellectual property. Communities coordinate through voluntary association. The reading's core claim is that centralized coordination imposed by political authority or religious doctrine is illegitimate coercion.
% TRANSFER_FUNCTION: The reading does not describe a transfer function in the sense of value moving from one party to another through the constraint's operation. Rather, the reading protects property holders' freedom to retain and deploy their innovation. The costs borne by vulnerable populations, monopsony workers, and communities — reduced bargaining power, exposure to risks, foreclosed coordination options — are side effects the reading treats as acceptable because they do not violate anyone's negative rights (the right not to be coerced). The reading rejects the framing of these costs as transfers; it denies that the constraint has positive duties to protect or compensate.
% ABSENT_VOICES: The Catholic Magisterium would object that the reading misrepresents subsidiarity, treating it as unlimited permission for market actors when it was always paired with solidarity — the principle that ensures those affected by economic decisions have voice in them and that their dignity is protected. Democratic polities would object that market structures are themselves governance arrangements that require legitimacy — and that legitimacy cannot derive from property claims alone, but must include consent of those affected by the arrangements. Vulnerable populations and workers in monopsony markets would object that exit options exist only in theory; their actual choices are constrained to impossible alternatives. These voices are excluded by the reading's architecture: it does not admit collective claims as legitimate.
% DISAPPEARANCE_RATIONALE: If the market-libertarian reading's claim that property rights are pre-political and voluntary exchange is morally sovereign were to disappear — if political authority, democratic majorities, or religious tradition reasserted authority over economic arrangements — the AI governance landscape would reorganize dramatically. Regulatory frameworks would expand. Worker protections, safety standards, and equity requirements would emerge. Communities would gain tools for collective coordination. This is not merely a rearrangement of incentives; it is a fundamental reshaping of the legitimacy structure. The reading itself would cease to function. From the reading's perspective, this verdict is contested because the parties disagree on whether the constraint describes natural law (property rights as pre-political, hence unchosen and ineradicable) or a constructed governance arrangement (market libertarianism as one political choice among others). The reading claims the former; the Magisterium, democracies, and vulnerable populations claim the latter.
% FOUNDING_PROBLEM: The reading addresses two founding problems: (1) Historical overreach by centralized authority — political regimes and religious hierarchies imposed mandates that stifled innovation and violated individual freedom. (2) The problem of moral authority — who has the right to determine how others should organize their economic lives? The reading answers: no one, except through voluntary transaction. Property rights and contract are pre-political solutions that bypass the problem of authority altogether.
% FOUNDING_PROBLEM_CORROBORATION: The reading itself claims the founding problem is live and ongoing: centralized authority continues to overreach (regulatory expansion, ideological mandates disguised as public health or justice). Property-rights libertarians and classical-liberal economists attest this. However, the Magisterium attests that the founding problem was a historical pathology (medieval tyranny, totalitarian regimes) now largely overcome, and that modern political authority is legitimized through democratic consent and constrained by constitutional rights — rendering the libertarian remedy obsolete. Democratic theorists and workers' advocates attest that centralized authority is necessary precisely because market structures are not self-legitimating and leave vulnerable populations without voice. Independent observers (economists studying monopsony power, labor market segmentation, and regulatory capture) offer mixed corroboration: some founding problems are real (regulators do sometimes serve special interests), but others are overstated (markets do not always equilibrate in favor of the powerless). No single external authority has settled the question.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.26, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.26 because the reading denies that market-structured inequality constitutes extraction. The reading's frame: property holders are not extracting from the propertyless; they are simply retaining what they own and trading voluntarily with those who choose to transact. Extraction only occurs through coercive taking, not through market discipline. The measurement series is relatively flat (0.22 to 0.27 to 0.26) because the reading presents market libertarianism as a stable institutional arrangement, not one that shifts in extractiveness over time. Suppression is authored at 0.15 because the reading claims that market discipline, contract, and reputation are self-enforcing — not actively suppressive machinery. However, this reflects the reading's own account of how the constraint operates. From other perspectives, what the reading calls 'market discipline' appears as suppression: workers cannot refuse bad labor terms without facing destitution; vulnerable populations cannot refuse AI services without facing exclusion; communities cannot coordinate without violating the reading's no-collective-mandate rule. Theater_ratio is very low (0.08) because the reading presents property protection and voluntary exchange as genuinely functional, not theatrical. The constraint performs what it claims. Accessibility_collapse is moderate (0.42) because alternatives do exist — one can, in theory, accumulate capital, start a business, move to a different labor market, build alternative services. But the reading's own stakeholder map shows that for many agents, alternatives are effectively foreclosed. Resistance is high (0.68) because the reading faces substantial opposition: democratic polities push back against property-absolutism; the Magisterium teaches that property is subordinate to common good; workers and communities resist the constraint's effects; vulnerable populations have no choice but to resist, even if their resistance is ineffectual. The measurement series stays flat because the reading projects a stable constraint, not one that erodes or intensifies over time — though uncertainty is high.
 *
 * PERSPECTIVAL GAP:
 *   This is the story's central analytical content. From the entrepreneur/investor seat (beneficiary, powerful, high exit), the constraint appears as property protection and freedom from coercive mandate — correctly classified as mountain, with low extractiveness and suppression. From the monopsony worker's seat (payer, powerless, trapped), the same constraint appears as enforced subordination to concentrated labor demand — extractive, suppressive, reclassifying toward snare. From the community's seat (payer, organized, constrained), the same constraint appears as denial of coordination tools — extractive, suppressive, reclassifying toward snare. From the Magisterium's seat (excluded, institutional), the same constraint appears as a political choice that privileges one set of values (freedom, property, innovation) over another (common good, dignity, participation) — contestable, not natural law. The engine will compute directionality for each seat from the structural data (beneficiary/victim + power + exit). Entrepreneurs have low d (beneficiary pod); workers have high d (victim pod); communities have high d (victim pod); Magisterium has d ≈ 0.5 or excluded entirely depending on how the computation handles agents with role='excluded'. The reading's own framing — 'this is how natural freedom works' — will not override the engine's structural computation of asymmetric distribution and foreclose-able alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each seat is derived from: (1) beneficiary/victim status, (2) power level, (3) exit options. Entrepreneurs_and_investors: declared beneficiary, powerful, arbitrage-grade exit → d near 0.0 (full beneficiary, subsidy pod). High_autonomy_individuals: declared beneficiary, moderate power, mobile exit → d near 0.15-0.25 (strong beneficiary, subsidy pod). Workers_in_monopsony_labor_markets: declared victim, powerless, trapped exit → d near 0.95-1.0 (full target, extraction pod). Communities_facing_coordination_failures: declared victim, organized power, constrained exit → d near 0.80-0.90 (strong target, extraction pod). Vulnerable_populations: declared victim, powerless, constrained exit → d near 0.90-1.0 (full target, extraction pod). Catholic_magisterium: excluded role, institutional power, analytical exit → excluded from directionality computation (or d ≈ 0.5 if treated as observer). Democratic_polities: excluded role, institutional power, analytical exit → excluded from directionality computation. These derivations follow from the structural data without override. The reading's own frame — that property rights are pre-political and that exit through voluntary exchange is always available — does not change the d computation; the computation reads the declared victim/exit data, not the reading's philosophical claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'contested' and the disappearance_verdict is 'contested' — signals that this reading faces a mandatrophy challenge. The reading was founded to solve a real problem (centralized authority overreach, suppression of innovation, violation of freedom). But the problem's status is disputed: the Magisterium and democratic theorists claim the founding problem was a historical pathology now largely addressed through democratic constitutionalism and subsidiarity-respecting pluralism. The reading insists the problem is live and ongoing. If the problem is dead but the constraint persists, the constraint exhibits mandatrophy — it becomes theatrical, performative, inertial. The measurement series staying flat (extractiveness 0.22-0.27) is consistent with a constraint that neither intensifies nor erodes, which could indicate mandatrophy: the constraint persists not because it solves a current problem but because beneficiaries (entrepreneurs, investors) continue to defend it. Conversely, if the founding problem is live (centralized overreach continues to threaten freedom), the constraint remains functionally necessary and mandatrophy is not yet present. The contest cannot be resolved from within the reading's own frame; external corroboration is needed. The omega variables route the unresolved questions: is the founding problem live or dead? Are exit options genuinely available for all agents? Is property rights pre-political or constructed? These are the empirical and conceptual facts the corpus needs to establish.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_pre_political_status,
    'Are property rights pre-political natural facts or constructed legal/social arrangements that require legitimation?',
    'Historical and comparative institutional analysis: do property rights emerge universally and unchanging across cultures, or do they vary radically with political and cultural context? Genealogical analysis of libertarian theory versus empirical property-system diversity.',
    'If property rights are constructed, the reading''s claim that they bypass the problem of authority collapses: someone must still answer ''who has the right to define property?'' If pre-political, the reading''s core premise stands and collective mandates that override property are illegitimate coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_rights_pre_political_status, conceptual, 'The ontological status of property rights: natural law or constructed?').

omega_variable(
    exit_options_as_dignity_protection,
    'Do exit options protect dignity equally when market power is asymmetrically distributed and alternatives are foreclosed by structural constraints?',
    'Empirical investigation of actual exit capacity for powerless agents in concentrated markets: can workers leave monopsony employers? Can vulnerable populations access alternative services? Do geographic, skill, or capital constraints eliminate exit despite formal availability?',
    'If exit is systematically foreclosed for powerless agents, the reading''s dignity-through-exit claim is false for those agents, and the constraint operates as suppressive extraction regardless of the reading''s framing. If exit is genuinely available, the reading''s framing is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_options_as_dignity_protection, empirical, 'Whether exit options are universally available or foreclosed for certain populations.').

omega_variable(
    voluntary_exchange_and_coercion_boundary,
    'Can a transaction be described as voluntary when one party faces no realistic alternative?',
    'Philosophical and legal analysis: under what conditions does consent withstand scrutiny? Test case: a worker in monopsony labor market; a vulnerable person denied AI service; a community facing automation with no transition mechanism.',
    'If coercion can exist within voluntary markets (through foreclosure of alternatives), the reading''s claim that coercion only arises from collective mandate is false. Some market extraction would then count as coercive, shifting classification upward and validating collective mandates as remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_exchange_and_coercion_boundary, conceptual, 'Whether absence of alternatives renders formally voluntary exchange actually coercive.').

omega_variable(
    kernel_reading_contest_ambiguity,
    'Is the legitimacy of AI governance a kernel question with multiple readings, or does one reading correctly describe pre-political facts that subsume the others?',
    'Meta-level analysis: if all four readings claim to describe facts (market libertarianism claims property rights ARE pre-political; Magisterial reading claims the common good IS morally prior; democrats claim legitimacy requires consent), then at most one can be correct — but all are authored as if their reading is factual, not chosen. Are these genuinely incommensurable readings, or does one reading capture the true structure and the others mistake political preferences for facts?',
    'If the contest is genuinely a kernel (incommensurable readings held by different communities), then this reading is one position among others, and classifying the constraint as ''mountain'' (natural law) is itself a contestable reading-specific claim. If one reading is factually correct, the others are false — and the question collapses to empirical verification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, preference, 'Whether AI governance legitimacy is a kernel question with multiple readings or a factual question with one correct answer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(ai_g_tr_t0, projected).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement_basis(ai_g_tr_t5, projected).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(ai_g_tr_t10, projected).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(ai_g_tr_t15, projected).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(ai_g_tr_t20, projected).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement_basis(ai_g_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(ai_g_be_t0, projected).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 5, 0.23).
narrative_ontology:measurement_basis(ai_g_be_t5, projected).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(ai_g_be_t10, projected).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement_basis(ai_g_be_t15, projected).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(ai_g_be_t20, projected).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 25, 0.26).
narrative_ontology:measurement_basis(ai_g_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(ai_g_su_t0, projected).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement_basis(ai_g_su_t5, projected).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement_basis(ai_g_su_t10, projected).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement_basis(ai_g_su_t15, projected).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement_basis(ai_g_su_t20, projected).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(ai_g_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).

% DUAL FORMULATION NOTE:
% This story is part of a constraint family decomposing the kernel 'ai_governance_legitimacy' into four structurally distinct readings. Each reading instantiates the kernel differently, with different ε values, beneficiary/victim structures, and enforcement mechanisms. All four readings compete for institutional authority in actual AI governance. Market-libertarian reading (this file) claims property rights and voluntary exchange as pre-political. Magisterial reading claims Catholic Social Doctrine principles as morally binding. Democratic reading claims legitimacy requires democratic consent and transparent public reason. Technocratic reading claims legitimacy derives from technical expertise and efficiency. These are linked in network.affects_constraints because each reading's adoption or dominance affects the instantiation of the others — e.g., if the market reading dominates, democratic and Magisterial voices are structurally excluded; if the democratic reading dominates, the market reading's enforcement mechanism (contract law, private arbitration, reputation) is subordinated to regulation and public mandate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
