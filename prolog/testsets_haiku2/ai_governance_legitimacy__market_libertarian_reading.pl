% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: AI Governance Legitimacy (Market-Libertarian Reading)
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   The market-libertarian reading of AI governance legitimacy treats
 *   voluntary exchange and property rights as pre-political foundations for
 *   legitimate authority. It rejects collective governance (democratic,
 *   religious, technocratic) as coercion and privileges entrepreneur autonomy
 *   and investor property claims. The constraint's low extractiveness (0.25)
 *   reflects the reading's claim that markets protect dignity through choice
 *   and exit. However, the reading's beneficiaries are concentrated
 *   (high-autonomy, capital-holding agents) while its victims are scattered
 *   and power-poor (monopsony workers, coordination-failure communities). The
 *   measurement series models the reading's emergence and stabilization in
 *   policy discourse circa 2024–2044. This JSON instantiates ONLY this
 *   reading, treating it as a single ε-invariant constraint. The kernel
 *   contest and sibling readings are other constraints, documented in omegas
 *   and cs_structure.
 *
 * KEY AGENTS:
 *   - entrepreneurs_and_innovators: beneficiary, powerful, global reach, arbitrage exit
 *   - capital_investors: beneficiary, institutional, funding allocation authority, arbitrage exit
 *   - high_autonomy_individuals: beneficiary, powerful, professional/affluent, mobile exit
 *   - workers_in_monopsony_markets: payer, powerless, geographically trapped, no exit
 *   - communities_lacking_market_power: payer, organized but powerless, constrained exit
 *   - communities_facing_coordination_failures: payer, organized, unable to coordinate collective action
 *   - religious_social_doctrine_traditions: excluded, institutional, legitimacy claims rejected
 *   - democratic_pluralist_authorities: excluded, institutional, deliberative authority rejected
 *   - market_libertarian_interpreters: agenda_setter, institutional, interpretive monopoly holder
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.15).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy (Market-Libertarian Reading)").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, 'e34aa5d6-e909-40d9-99fe-fe4f50fc9b51').
narrative_ontology:cs_kernel_codification('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', distributed).
narrative_ontology:cs_authority_grounding('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', distributed).
narrative_ontology:cs_reading_relation('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', ai_governance_legitimacy__democratic_pluralist_reading, forecloses).
narrative_ontology:cs_reading_relation('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', ai_governance_legitimacy__technocratic_optimization_reading, coexists_with).
narrative_ontology:cs_axiom('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', foundational, property_rights_foundational_legitimacy).
narrative_ontology:cs_axiom_status(property_rights_foundational_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', property_rights_foundational_legitimacy, deontological).
narrative_ontology:cs_axiom('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', foundational, voluntary_exchange_excludes_collective_mandate).
narrative_ontology:cs_axiom_status(voluntary_exchange_excludes_collective_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', voluntary_exchange_excludes_collective_mandate, deontological).
narrative_ontology:cs_axiom('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', secondary, exit_option_dignity_mechanism).
narrative_ontology:cs_axiom_status(exit_option_dignity_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', exit_option_dignity_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', property_rights_voluntary_exchange_baseline).
narrative_ontology:cs_drift_state('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', contemporary_ai_governance_arena, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e34aa5d6-e909-40d9-99fe-fe4f50fc9b51', '2026-08-03T14:32:15Z').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs_and_innovators).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, capital_investors).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_markets).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_lacking_market_power).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, those_facing_coordination_failures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, property_rights_pre_political).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_legitimacy).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, exit_option_dignity).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, competitive_market_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate AI companies and development platforms under minimal regulatory constraint. Benefit from property rights to their innovations, contractual freedom to set terms of service, and competitive market positioning. Can exit regulatory jurisdictions or renegotiate contract terms with users and employees. Their success signals market-validated value creation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs_and_innovators, beneficiary,
    powerful, biographical, arbitrage, global).

% Fund AI development and deployment. Benefit from unrestricted property claims to intellectual property, unregulated capital allocation decisions, and competitive returns on innovation bets. Exit is available through portfolio diversification and jurisdictional arbitrage. Their allocation decisions direct resources to perceived high-value outcomes.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, capital_investors, beneficiary,
    institutional, biographical, arbitrage, global).

% High-skill workers, professionals, and affluent users with negotiating power and exit options. Benefit from competitive service offerings, choice among platforms, and ability to customize terms through contract or market switching. Can refuse unfavorable terms or move to competitors. Protect their dignity through exercising preference, not through collective mandate.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    powerful, biographical, mobile, global).

% AI workers, content moderators, data annotators, and platform-dependent workers who face concentrated employer power. Limited exit options because AI employment is geographically concentrated and dominant firms set terms unilaterally. Bear the cost of terms set without their meaningful participation: low wages, algorithmic surveillance, unilateral policy changes, employment insecurity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_markets, payer,
    powerless, biographical, trapped, global).

% Low-income populations, developing nations, and groups without purchasing power or investment capital. Face AI systems deployed without their input or consent (algorithmic hiring, credit scoring, surveillance). Their market power is insufficient to influence terms; exit is not available because the systems are infrastructure. Bear costs of decisions made by property-holding decision-makers far away.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_lacking_market_power, payer,
    organized, generational, constrained, global).

% Groups that would benefit from collective action (labor standards, data privacy, safety requirements) but lack centralized enforcement mechanisms to coordinate across competitive firms. The constraint's market mechanism dissolves collective problems into individual contract choices, leaving coordination problems unsolved. Bear the cost of diffuse harms that markets don't price: pollution, monopsony wage depression, information asymmetry.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    organized, generational, constrained, global).

% The Magisterium, Catholic Social Doctrine tradition, and other religious authorities that articulate principles of common good, solidarity, and universal destination of goods. This reading explicitly rejects their legitimacy as governance principle. They are excluded from the decision-making frame — property rights and voluntary exchange are treated as pre-political, beyond the reach of religious ethical claims. Their participation would constrain entrepreneur autonomy.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, religious_social_doctrine_traditions, excluded,
    institutional, civilizational, trapped, global).

% Democratic governments and pluralist deliberative bodies that would seek to balance AI governance through public reason and inclusive processes. This reading rejects their authority to regulate beyond contract law and property protection. Their mandates are characterized as illegitimate coercion. Participation would require subordinating property rights to collective will.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, democratic_pluralist_authorities, excluded,
    institutional, generational, trapped, national).

% Intellectual tradition interpreting property rights, voluntary exchange, and market mechanisms as foundational legitimacy principles. Sets the frame through academic work, policy advocacy, legal argument, and institutional norm-setting. Defines which principles count as legitimate and which constraints count as 'coercion.' Administers the interpretive monopoly on what liberty and dignity mean.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, market_libertarian_interpreters, agenda_setter,
    institutional, generational, analytical, global).

% Secular ethical and philosophical observers who examine the constraint from outside both religious and libertarian frames. See this reading as one normative possibility among others, contestable on its own premises: whether property rights are truly pre-political, whether exit is actually available to powerless agents, whether markets protect dignity or merely preserve power asymmetry.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, observer_secular_ethics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint solves the problem of legitimacy attribution in AI governance: it answers the question 'On what grounds can any authority regulate AI?' by anchoring legitimacy in property rights and voluntary exchange rather than in democratic mandate, religious doctrine, or technocratic expertise.
% TRANSFER_FUNCTION: The arrangement transfers authority from collective deliberative bodies (democratic legislatures, religious traditions, pluralist publics) to individual property-holders and market mechanisms. Governance decisions move from processes of accountability and consent to contracts between parties with unequal power. High-autonomy, capital-holding agents keep governance authority; powerless agents transfer their say into terms of service.
% ABSENT_VOICES: Workers without market power, communities facing coordination failures, religious traditions articulating solidarity principle, democratic publics seeking participatory governance. These actors would argue for collective deliberation, common-good principles, and accountability mechanisms. They are structurally excluded because the reading treats their inclusion as 'coercion' rather than as legitimate governance.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if property rights no longer grounded legitimacy and voluntary exchange were subordinated to collective mandates — the world would not simply rearrange; it would hinge on whether the alternative reading (magisterial, democratic, or technocratic) replaced it. The payers (monopsony workers, powerless communities) would gain voice in governance. The beneficiaries (entrepreneurs, investors) would lose unilateral authority. Whether this is rearrangement or justice depends on the metaphysical and political presuppositions the reader brings.
% FOUNDING_PROBLEM: The problem this reading was built to solve: in the early AI governance moment, diverse authorities (religious traditions, democratic bodies, expert technocrats, indigenous communities) each claimed legitimacy to regulate AI. The market-libertarian reading resolves this conflict by denying that any collective authority has legitimacy — only property holders and voluntary contractors do. It eliminates the contest by hierarchically privileging one frame.
% FOUNDING_PROBLEM_CORROBORATION: Market-libertarian interpreters attest the problem is live: competing legitimacy claims do create genuine confusion in AI governance. However, outside observers (philosophers, democratic theorists, religious ethicists) dispute whether the 'solution' addresses the problem or merely asserts an unprovable metaphysical claim — that property rights are pre-political. Democratic theorists note that property itself is a legal construction requiring enforcement (hence political legitimacy). Religious traditions note that the reading misquotes subsidiarity to support its conclusions. Independent analysis from non-libertarian sources shows the reading's core premises (that exit is available, that markets don't fail, that voluntary exchange protects dignity) are empirically contestable.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.25, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Low extractiveness (0.25) is authorized because the reading claims markets are transparent coordination mechanisms where agents voluntarily choose terms — no hidden appropriation occurs. However, this presupposes that exit is available and that markets do not fail. These presuppositions are omegas, not facts. Low suppression (0.15) reflects the reading's claim that enforcement is minimal: contract law, property law, reputational mechanisms. Low theater (0.10) reflects that property-rights adjudication is claimed to be straightforward, not theatrical. The accessibility_collapse (0.35) is moderate because the reading's core premises (property rights are pre-political, exit is available) are contestable — alternative framings keep alternatives intellectually accessible. Resistance (0.72) is high because democratic, religious, and coordination-failure constituencies actively reject the reading. The measurement series runs on a single shared time grid at six points (t=0,4,8,12,16,20), modeling the reading's institutional consolidation over the 2024–2044 interval. Extractiveness, theater, and suppression all trend slightly upward as the reading becomes institutionalized in policy and legal structures, suggesting growing enforcement burden despite low nominal extractiveness.
 *
 * PERSPECTIVAL GAP:
 *   This reading's engine-computed type should diverge sharply across seats. From the beneficiary seats, the constraint should compute as rope or even mountain (natural law protecting their autonomy). From the payer seats, it should compute as snare (pure extraction dressed as neutrality). From the excluded seats (religious, democratic), it should compute as snare targeting their authority. The beneficiary frame sees voluntary exchange and property rights as self-evident facts. The payer frame sees property-rights enforcement as coercive apparatus. The excluded frame sees interpretive monopoly as illegitimate. The reading's authored claim of mountain status reflects the beneficiary frame's self-perception; the engine's per-seat computation should reveal this as false-summit terrain.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading's own logic denies that directionality should matter — it treats all agents as equal property-holders entering voluntary exchange. But structural facts contradict this: entrepreneurs hold property in AI systems; monopsony workers hold only their labor. Investors can allocate capital across jurisdictions; communities cannot relocate. High-autonomy individuals can refuse unfavorable terms; trapped workers cannot. The directionality derivation (beneficiary/victim + exit → d) surfaces these structural asymmetries that the reading's language glosses over.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem is 'how to attribute legitimacy in contested AI governance?' Its claim is that property rights and voluntary exchange solve this by replacing contest with hierarchy: property-holders decide, others comply or exit. The founding_problem_status (contested) and disappearance_verdict (contested) surface the constraint's mandatrophy. If the founding problem is solved, why do democratic publics, religious traditions, and coordination-failure communities continue to demand voice? The answer: the reading has not solved the contest, it has asserted hierarchy over the contest and excluded dissenters. This is not mandatrophy (a solved problem that persists) but rather a suppressed dissent that the reading reframes as illegitimate. The theater_ratio is low (0.10) but rising slightly, reflecting that the reading maintains its legitimacy mainly through theoretical argument and institutional positioning, not through demonstrable delivery of the promised outcomes (market protection of dignity for all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_pre_political_claim,
    'Are property rights truly pre-political facts of nature, or are they legal constructions that require political/enforcement authority to sustain?',
    'Philosophical/historical analysis of property regimes across societies, showing whether property rights exist independent of enforcement infrastructure or are always dependent on legal/political systems.',
    'If property rights are legal constructions, the reading''s claim to foundational legitimacy collapses — property rights would themselves require political justification. The constraint would reclassify from mountain (natural law) to rope or tangled_rope (a constructed coordination mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_rights_pre_political_claim, conceptual, 'Whether property rights are pre-political or require political legitimacy.').

omega_variable(
    exit_availability_powerless_agents,
    'Is exit actually available to workers in monopsony labor markets, communities lacking capital, and individuals in concentrated platform ecosystems?',
    'Empirical measurement of exit-cost ratios: cost-to-relocate / annual-income, availability of substitute employment, legal barriers to exit, switching costs in platform ecosystems. Compare across agent types: high-autonomy individuals vs. monopsony workers.',
    'If exit is unavailable or prohibitively costly for power-poor agents, the reading''s core claim that dignity is protected through exit collapses for those agents. The constraint would show high effective extraction for those seats despite low nominal extractiveness — reclassifying as snare or tangled_rope at the payer seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_availability_powerless_agents, empirical, 'Whether exit options are actually available to powerless agents.').

omega_variable(
    market_failure_coordination_problem,
    'Do competitive markets fail to coordinate on collective problems (safety standards, labor protections, environmental externalities)? Can those failures be systematically identified?',
    'Analysis of real-world coordination failures in AI: wage depression across competitors, information asymmetry in algorithmic decisions, externalized harms. Compare against markets where collective standards emerged (aviation safety, financial clearing standards).',
    'If markets systematically fail to coordinate on goods that powerless agents depend on, then the reading''s claim that markets protect dignity for all is false — it protects it only for those with sufficient power to force coordination. Some form of collective governance becomes necessary. The constraint''s beneficiaries would shrink to only high-power agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_failure_coordination_problem, empirical, 'Whether markets systematically fail to coordinate on collective harms.').

omega_variable(
    reading_vs_magisterial_solidarity,
    'Does the reading''s treatment of the encyclical''s solidarity principle as ''illegitimate coercion'' accurately represent Catholic Social Doctrine, or does it misquote subsidiarity to exclude solidarity?',
    'Textual analysis of the encyclical itself (Laudato Si'', Fratelli Tutti); testimony from Catholic ethicists and Magisterial interpreters outside the market-libertarian frame; comparison of how the reading frames subsidiarity vs. how the tradition frames it.',
    'If the reading misrepresents the encyclical, the omega surfaces the reading''s interpretive move as contestable. It remains a live reading but loses the rhetorical claim to support from Catholic teaching. The contested-reading relationship to magisterial_subsidiarity_reading clarifies: they would both cite the same text but interpret it differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_magisterial_solidarity, conceptual, 'Whether the reading''s use of subsidiarity accurately represents Catholic Social Doctrine.').

omega_variable(
    kernel_committer_frame,
    'This reading is one interpretation of the contested AI governance legitimacy kernel. What structural changes to the constraint would result if a sibling reading (democratic, magisterial, or technocratic) were adopted instead?',
    'Comparison of ε, beneficiary/victim structure, and claimed_type across the four sibling readings when each is instantiated separately.',
    'The four readings are NOT four perspectives on one constraint — they are four different constraints sharing a kernel. This omega signals that classification divergence between readings is expected and structurally meaningful. The kernel is the persisting commitment; the readings are the live interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_frame, conceptual, 'This constraint is one reading of a contested kernel; others emit different constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(ai_g_tr_t0, projected).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement_basis(ai_g_tr_t4, projected).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(ai_g_tr_t8, projected).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(ai_g_tr_t12, observed).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(ai_g_tr_t16, projected).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(ai_g_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(ai_g_be_t0, projected).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement_basis(ai_g_be_t4, projected).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement_basis(ai_g_be_t8, projected).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement_basis(ai_g_be_t12, observed).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(ai_g_be_t16, projected).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement_basis(ai_g_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(ai_g_su_t0, projected).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 4, 0.13).
narrative_ontology:measurement_basis(ai_g_su_t4, projected).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 8, 0.14).
narrative_ontology:measurement_basis(ai_g_su_t8, projected).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 12, 0.15).
narrative_ontology:measurement_basis(ai_g_su_t12, observed).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 16, 0.15).
narrative_ontology:measurement_basis(ai_g_su_t16, projected).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement_basis(ai_g_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__market_libertarian_reading, 0.18).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_market_ai_training).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, algorithmic_decision_asymmetry).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, platform_exit_cost_trap).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ai_governance_legitimacy kernel, which decomposes into four distinct constraints (market_libertarian, magisterial_subsidiarity, democratic_pluralist, technocratic_optimization). Each reading instantiates a different ε, beneficiary/victim structure, and claimed_type from the same kernel text (the encyclical and AI governance discourse). They are linked via network.affects_constraints as a constraint family. The decomposition follows ε-invariance (OQ-26 ruling): if measuring legitimacy one way (property-rights frame) yields low ε and measuring it another way (solidarity-principle frame) yields high ε, the observer is looking at different constraints. Each is authored as a complete, self-contained story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
