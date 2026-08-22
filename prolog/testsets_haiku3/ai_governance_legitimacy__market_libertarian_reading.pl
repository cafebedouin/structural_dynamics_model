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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: AI Governance Legitimacy — Market Libertarian Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the market-libertarian reading of contested
 *   AI governance legitimacy. It claims governance legitimacy derives
 *   exclusively from voluntary exchange, property rights, and market
 *   discipline — not from democratic deliberation, Magisterial authority, or
 *   centralized oversight. The reading selectively appropriates the
 *   encyclical's subsidiarity principle (decentralization good) while
 *   rejecting solidarity demands (collective mandates illegitimate).
 *   Beneficiaries are entrepreneurs, investors, and high-autonomy individuals
 *   with market power; victims are workers in monopsony labor markets,
 *   communities lacking market power, and populations facing coordination
 *   failures. The constraint's claimed type is mountain (property rights as
 *   pre-political naturals); the authored metrics (low extractiveness, low
 *   suppression, low theater) reflect the reading's legitimacy claim — but
 *   the presence of clear victims and beneficiaries, combined with measurable
 *   resistance, triggers False Summit evaluation. The constraint is part of
 *   the ai_governance_legitimacy kernel family, competing with
 *   democratic_pluralist, magisterial_subsidiarity, and
 *   technocratic_optimization readings.
 *
 * KEY AGENTS:
 *   - Entrepreneurs and investors: primary beneficiaries; claim exit protects all users
 *   - High-autonomy individuals: beneficiaries; defend maximum individual choice
 *   - Established market participants: beneficiaries capturing rents through network effects
 *   - Workers in monopsony labor markets: victims; face algorithmic management with no exit
 *   - Communities lacking market power: victims; experience AI in essential services without collective say
 *   - Populations facing coordination failures: victims; need collective action but reading forecloses it
 *   - Magisterial authority: excluded; its solidarity principle is reframed as coercion
 *   - Democratic pluralist authority: excluded; collective deliberation denied legitimacy standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.25).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.35).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "AI Governance Legitimacy — Market Libertarian Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, '69519567-0235-4016-ae4d-bafb478e96e7').
narrative_ontology:cs_kernel_codification('69519567-0235-4016-ae4d-bafb478e96e7', distributed).
narrative_ontology:cs_authority_grounding('69519567-0235-4016-ae4d-bafb478e96e7', extraction).
narrative_ontology:cs_reading_relation('69519567-0235-4016-ae4d-bafb478e96e7', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('69519567-0235-4016-ae4d-bafb478e96e7', ai_governance_legitimacy__magisterial_subsidiarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('69519567-0235-4016-ae4d-bafb478e96e7', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('69519567-0235-4016-ae4d-bafb478e96e7', foundational, property_rights_pre_political_naturals).
narrative_ontology:cs_axiom_status(property_rights_pre_political_naturals, holdable).
narrative_ontology:cs_axiom_grounding('69519567-0235-4016-ae4d-bafb478e96e7', property_rights_pre_political_naturals, deontological).
narrative_ontology:cs_axiom('69519567-0235-4016-ae4d-bafb478e96e7', foundational, market_discipline_superior_to_collective_authority).
narrative_ontology:cs_axiom_status(market_discipline_superior_to_collective_authority, holdable).
narrative_ontology:cs_axiom_grounding('69519567-0235-4016-ae4d-bafb478e96e7', market_discipline_superior_to_collective_authority, instrumental).
narrative_ontology:cs_axiom('69519567-0235-4016-ae4d-bafb478e96e7', secondary, solidarity_demands_illegitimate_coercion).
narrative_ontology:cs_axiom_status(solidarity_demands_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('69519567-0235-4016-ae4d-bafb478e96e7', solidarity_demands_illegitimate_coercion, deontological).
narrative_ontology:cs_reference_frame('69519567-0235-4016-ae4d-bafb478e96e7', property_rights_market_legitimate_governance).
narrative_ontology:cs_drift_state('69519567-0235-4016-ae4d-bafb478e96e7', contemporary_ai_concentration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('69519567-0235-4016-ae4d-bafb478e96e7', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs_and_investors).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, established_market_participants).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_lacking_market_power).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, populations_facing_coordination_failures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and deploy AI systems under minimal regulatory constraint. Market discipline and reputational mechanisms protect consumers more efficiently than centralized oversight. They claim exit options exist for users dissatisfied with their AI governance choices (choosing rival platforms, services, or jurisdictions). Their legitimacy narrative is that voluntary adoption of their systems demonstrates consent.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs_and_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Retain maximum freedom to adopt, deploy, or refuse AI systems according to individual judgment. Reject paternalistic governance structures that override personal choice in the name of collective good. Their dignity is protected by exit, not by political authority managing technology on their behalf. They are typically highly educated, resourced, and positioned to evaluate and switch between AI governance frameworks.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_individuals, beneficiary,
    powerful, biographical, arbitrage, global).

% Defend market-based governance as legitimate because it preserves their competitive position and their ability to capture rents through network effects, data moats, and switching costs. They use the legitimacy language of voluntary exchange while benefiting from structural positions that constrain actual exit for their users. They fund academic work and policy advocacy supporting market-libertarian readings.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, established_market_participants, beneficiary,
    institutional, generational, arbitrage, global).

% Face AI deployment in hiring, scheduling, and wage-setting with no meaningful exit. They cannot choose which AI governance framework applies to them; they must accept the terms set by dominant employers or face unemployment. The reading's claim that exit protects dignity does not extend to them — their market power is insufficient to credibly refuse employment. Algorithmic management decisions affecting their livelihoods are made in private by firms under market-legitimacy framing.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, workers_in_monopsony_labor_markets, payer,
    powerless, biographical, constrained, local).

% Experience AI systems deployed in essential services (healthcare access, credit, housing, public benefits) with no collective say in governance. Market-legitimacy framing assumes they can exit if dissatisfied, but the services are often essential and uncompetitive. They cannot organize as counterbalances to monopsony or monopoly power. They lack the technical literacy and capital to demand accountability or to relocate to jurisdictions with different governance frameworks.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_lacking_market_power, payer,
    powerless, biographical, trapped, local).

% Face governance problems (algorithmic collusion, data cartels, race-to-the-bottom AI deployment standards) that market mechanisms cannot coordinate. They need collective action to address but the reading denies legitimacy to governance structures that could enforce coordination. They experience the constraint as foreclosing collective remedies that might be individually rational from a coordination-failure perspective.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, populations_facing_coordination_failures, payer,
    organized, generational, constrained, global).

% The Catholic Magisterium, as the authoritative interpreter of Catholic Social Doctrine, is structurally excluded from the legitimacy framework this reading instantiates. The reading claims subsidiarity (a Magisterial principle) but rejects solidarity demands as illegitimate coercion (inverting the Magisterium's own hierarchy of principles). The Magisterium would object to this decomposition of its unified doctrine but is not a seat in this reading's governance arrangement.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterial_authority, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(ai_governance_legitimacy__market_libertarian_reading, magisterial_authority).

% Democratic deliberative processes and inclusive public reason are excluded from the reading's legitimacy source. This reading asserts that governance legitimacy derives from markets, not democratic decision-making. Democratic publics seeking to impose constraints on AI innovation find no standing in this framework — their collective will is reframed as illegitimate coercion of property rights holders.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, democratic_pluralist_authority, excluded,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(ai_governance_legitimacy__market_libertarian_reading, democratic_pluralist_authority).

% Technical and policy experts who measure welfare outcomes, coordinate governance mechanisms, and implement standards occupy an analytical seat. They observe the constraint but do not directly benefit or pay. Some are deployed as legitimacy brokers (external auditors, advisory boards) to make market-libertarian governance appear accountable while preserving ultimate corporate decision-making authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, technocratic_experts, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__market_libertarian_reading, entrepreneurs_and_investors).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__market_libertarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the innovation-speed problem: enables rapid AI development without the coordination costs and delays of deliberative multi-stakeholder governance processes. Market mechanisms are claimed to coordinate safety, ethics, and governance more efficiently than centralized authority.
% TRANSFER_FUNCTION: Transfers governance authority from collective decision-making (democratic, magisterial, or technocratic) to property-rights holders and voluntary-exchange participants. Those lacking market power accept whatever governance framework dominant platforms establish or exit to alternative systems (where available). The transfer is from the many (citizens, communities, workers) to the few (resource holders and exit-mobile individuals).
% ABSENT_VOICES: Workers in monopsony labor markets, communities without market power, and populations facing coordination failures are structurally excluded from the legitimacy narrative. They would object that exit is not available to them and that market discipline fails to protect them from algorithmic harms. Democratic deliberative processes and the Magisterial teaching authority are also excluded — they are reframed as sources of coercion rather than legitimate governance.
% DISAPPEARANCE_RATIONALE: From the reading's perspective: if market-libertarian AI governance vanished, bureaucratic over-regulation would stifle innovation and centralized authority would impose paternalistic mandates, harming all users. From opposing readings' perspectives: if this constraint were removed, plural governance frameworks could emerge, democratic processes could set terms, and collective protections could address coordination failures. The disagreement is structural, not empirical.
% FOUNDING_PROBLEM: The founding problem this reading instantiates is: How can rapid AI innovation proceed without being strangled by bureaucratic review, collective mandates that reflect no single coherent value hierarchy, or misguided paternalism? Property rights and voluntary exchange are claimed as the solution — they enable diversity of governance approaches and let each individual optimize for their own values without imposing on others.
% FOUNDING_PROBLEM_CORROBORATION: Entrepreneurs, investors, and libertarian philosophers and economists attest this problem is live and market-libertarian governance solves it. However, labor economists, communities experiencing algorithmic harms, advocates for democratic governance, and Magisterial authorities attest that the problem is mis-stated: the real problem is unaccountable corporate power using 'innovation' and 'exit' language as cover for extractive governance. Academic research on algorithmic discrimination, labor market monopsony, and coordination failures provides corroboration outside the beneficiary set that the founding problem, as stated, mischaracterizes the actual constraints faced by populations lacking market power.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Base extractiveness is authored at 0.25 (mid-range for a constraint claiming to be a natural law). The reading claims property rights are pre-political and that market mechanisms naturally protect all participants. However, the presence of identifiable victims (workers in monopsony, communities without market power) indicates the constraint does extract from specific populations even if it claims not to. Suppression is low (0.35) because the constraint's enforcement relies primarily on contract law, private arbitration, and reputational mechanisms rather than active coercion — but suppression is not zero because excluded parties (those without exit options) experience the constraint as foreclosing collective remedies. Theater is very low (0.15) because the reading makes minimal claim to performative legitimacy; it rests on a straightforward property-rights narrative without elaborate justification machinery. Accessibility collapse is low-to-moderate (0.40): alternatives exist in principle (democratic governance, magisterial authority, technocratic coordination) but the reading's claim that these are sources of coercion rather than legitimate governance makes them appear inaccessible within this frame. Resistance is high (0.72): the constraint meets substantial active resistance from labor advocates, communities experiencing algorithmic harms, democratic pluralists, and Magisterial authorities. The measurement series trace a slight upward drift in extractiveness (0.18 to 0.25 over 15 years observed, then plateau) as market concentration in AI intensifies and coordination failures become more acute, then projects stable extractiveness as political pressure stabilizes the reading's position.
 *
 * PERSPECTIVAL GAP:
 *   The gap is not between two seats viewing the same constraint; it is between two incommensurable legitimacy sources. From the beneficiary seat (entrepreneurs, investors, high-autonomy individuals), the constraint is a natural law protecting property and enabling voluntary exchange — extractiveness is low because coercion is minimal, just the background rule of property rights. From the victim seat (monopsony workers, communities without exit), the same constraint is deeply extractive because it forecloses collective remedies and leaves them subject to unaccountable corporate governance. From the Magisterial seat, the constraint violates the hierarchy of principles in Catholic Social Doctrine (solidarity is primary, subsidiarity is instrumental). From the democratic pluralist seat, the constraint illegitimately forecloses democratic deliberation. The engine computes these divergences from the structural data: same institutional power, same global scope, but dramatically different exit options (arbitrage for beneficiaries, trapped for victims) produce different directionality values and thus different effective extraction calculations.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrepreneurs and investors (powerful, arbitrage exit, global scope) compute as near-beneficiary end (d ≈ 0.15), receiving concentrated benefit. High-autonomy individuals (powerful, arbitrage exit, global scope) also compute near-beneficiary (d ≈ 0.15). Workers in monopsony markets (powerless, constrained exit, local scope) compute as targets (d ≈ 0.85), bearing extraction from a structural position of dependence. Communities lacking market power (powerless, trapped exit, local scope) compute as full targets (d ≈ 0.95) — the constraint has highest effective extraction on them because they have zero exit and the constraint directly forecloses collective action. Established market participants (institutional power, arbitrage exit, global scope) compute as beneficiaries (d ≈ 0.20), but with an override upward to reflect partial capture — they benefit from the legitimacy frame while also bearing some cost from competitive pressure within the market-libertarian system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rapid innovation without bureaucratic constraint) was live when written. However, as market concentration in AI has intensified (observed t=0-15), the coordination failures that plague monopsony labor markets and data cartels have become acute, while the founding problem (slow innovation due to over-regulation) has not materialized — innovation remains rapid under market-libertarian governance. The mismatch between founding_problem_status (contestedly live) and disappearance_verdict (contested) + rising evidence of coordination failures suggests the constraint may be experiencing mandatrophy: its founding justification (unconstrained innovation is the bottleneck) no longer matches observable reality (the bottleneck is now unaccountable corporate power and coordination failure). The reading maintains itself through narrative reframing (claiming coordination failures are either impossible or worse under alternatives) rather than through solving its founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_rights_naturalism_vs_construction,
    'Are property rights over AI systems a pre-political natural law, or are they a political construction that could be organized differently (collective ownership, stakeholder governance, public trusts)?',
    'This is conceptually irreducible without a prior commitment to a metaphysical framework. The reading asserts naturalism; sibling readings treat property rights as politically contingent. The question cannot be resolved empirically because both frameworks are internally coherent.',
    'If property rights are natural, the market-libertarian reading''s core premise holds and governance legitimacy flows from voluntary exchange. If they are constructed, the reading''s claim to pre-political status collapses and the constraint becomes a political choice like any other — opening it to contestation from democratic and magisterial authority sources.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_rights_naturalism_vs_construction, conceptual, 'Foundational disagreement about the ontological status of property rights.').

omega_variable(
    exit_availability_for_constrained_populations,
    'Do workers in monopsony labor markets and communities without market power genuinely have exit options, or does the constraint''s claim of exit-based protection rely on a fiction of mobility that is unavailable to precisely the populations most exposed to algorithmic governance?',
    'Empirical study of labor market mobility, geographic relocatability, information asymmetries, and network effects: How many workers in algorithmic management systems actually switch employers or platforms? How many communities without market power actually abandon essential services to access alternatives? What proportion of populations have the literacy, capital, and security to evaluate and switch AI governance frameworks?',
    'If exit is genuinely available for all populations, the reading''s claim that market discipline protects even the powerless holds and extraction remains low. If exit is confined to high-autonomy, resource-rich populations, then the constraint operates as pure extraction for those without exit, and effective extraction (chi) rises substantially for powerless agents despite low base extractiveness (epsilon). This would move the constraint from mountain toward snare in the per-seat classification view.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_availability_for_constrained_populations, empirical, 'Whether exit is universally available or stratified by power and resources.').

omega_variable(
    coordination_failure_impossibility_claim,
    'Can market mechanisms solve coordination problems in AI governance (algorithmic collusion, race-to-the-bottom standards, data cartels), or do these problems require collective action that the reading forecloses?',
    'Empirical observation: Do coordination failures in AI deployment emerge? Do platforms spontaneously adopt protective standards, or do standards only emerge after regulatory threat? Can exit alone discipline cartel behavior, or is exit suppressed by network effects?',
    'If market mechanisms can solve coordination failures, the reading''s framework remains coherent. If coordination failures emerge and markets do not self-correct, then the constraint operates as pure extraction for populations facing coordination failures, and the magnitude of this extraction is not captured by the reading''s low base extractiveness metric (epsilon is under-specified, requires decomposition into separate constraints per the ε-invariance principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_failure_impossibility_claim, empirical, 'Whether market mechanisms can coordinate solutions to collective-action AI governance problems.').

omega_variable(
    encyclical_principles_decomposition,
    'Is the reading''s selective appropriation of subsidiarity while rejecting solidarity coherent within Catholic Social Doctrine, or does it perform a decomposition of a unified doctrine that treats them as hierarchically integrated?',
    'Magisterial textual analysis and theological interpretation. The reading claims subsidiarity legitimates decentralization and market governance; the encyclical treats subsidiarity as instrumental to solidarity and the common good. Can one coherently adopt subsidiarity without the solidarity framework it is embedded in?',
    'If the reading''s decomposition is incoherent, its claim to ground itself in the encyclical fails, and it becomes a pure ideological assertion rather than an authentic reading of the kernel text. If coherent, the sibling magisterial_subsidiarity_reading has mischaracterized the encyclical''s unity. This affects credibility of the reading but not its internal logical structure or its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encyclical_principles_decomposition, conceptual, 'Whether the market-libertarian reading coherently appropriates the encyclical''s principles or performs an illegitimate decomposition.').

omega_variable(
    mountain_vs_false_summit_ambiguity,
    'Is this constraint a genuine pre-political natural law (mountain), or does the presence of identifiable beneficiaries and victims indicate it is a constructed constraint falsely claiming naturality (false summit)?',
    'The schema-enforced FSM gate: a mountain claiming beneficiaries must document the natural-law vs. constructed ambiguity in an omega. This omega IS that documentation. Empirical/conceptual resolution: Is the constraint''s persistence due to structural properties of reality that would obtain regardless of political choice, or is it maintained by active defense of property-rights doctrine and market-legitimacy framing?',
    'If the constraint is a genuine natural law, the classification mountain holds across all seats. If it is a false summit (constructed constraint using natural-law language to justify beneficial extraction), it reclassifies as tangled_rope or snare per the FSM override logic, and the beneficiary-driven enforcement becomes visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_false_summit_ambiguity, conceptual, 'Whether the claimed naturality of property rights is genuine or performed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 5, 0.2).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 25, 0.25).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 30, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__market_libertarian_reading, 0.18).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested ai_governance_legitimacy kernel. The market-libertarian reading claims governance legitimacy derives from property rights and voluntary exchange; it competes with readings that ground legitimacy in democratic deliberation (democratic_pluralist), Catholic Social Doctrine (magisterial_subsidiarity), and technical expertise (technocratic_optimization). Each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and classification. Sibling constraints share the same kernel text but produce structurally different constraint stories because each reading interprets the kernel's principles differently. The network edges model institutional influence: the market-libertarian reading forecloses collective governance mechanisms that sibling readings would activate, and simultaneously creates incentive structures that pressure siblings toward market-compatible compromises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__market_libertarian_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
