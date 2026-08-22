% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__magisterial_subsidiarity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: Magisterial Subsidiarity Reading of AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story instantiates the magisterial_subsidiarity_reading of the
 *   contested ai_governance_legitimacy kernel: legitimacy for AI governance
 *   arrangements derives from conformity to Catholic Social Doctrine
 *   principles as interpreted by the Magisterium. The doctrine explicitly
 *   rejects both pure technocratic optimization and market-libertarian
 *   voluntarism, demanding political oversight grounded in human dignity,
 *   subsidiarity, and solidarity. It is authored here as a tangled_rope: it
 *   genuinely coordinates otherwise-fragmented advocacy (workers, Global
 *   South communities, families, marginalized populations) around a shared
 *   moral vocabulary, while asymmetrically imposing reputational and
 *   compliance costs on tech monopolies, military AI contractors, and
 *   extractive finance — coordination and extraction running through the same
 *   structure, sustained only by continuous moral suasion, ecclesial witness,
 *   and civil-society pressure rather than binding law.
 *
 * KEY AGENTS:
 *   - the_magisterium: agenda_setter, interprets doctrine authoritatively but holds no binding regulatory power
 *   - industrial_workers, global_south_populations, families_and_local_communities, marginalized_and_disabled_populations: beneficiaries of the coordinated moral vocabulary
 *   - private_tech_monopolies, military_industrial_ai_contractors, extractive_finance_and_asset_managers: payers who bear reputational and compliance costs when the doctrine is successfully invoked
 *   - national_governments_and_regulators: excluded from the doctrinal authority structure despite holding actual binding power
 *   - lay_catholic_ai_ethicists: observer/beneficiary who apply and depend on the framework's continued relevance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.38).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "Magisterial Subsidiarity Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, '7dd7608f-ebe5-425d-ada2-a0175c070ba0').
narrative_ontology:cs_kernel_codification('7dd7608f-ebe5-425d-ada2-a0175c070ba0', fixed_text).
narrative_ontology:cs_authority_grounding('7dd7608f-ebe5-425d-ada2-a0175c070ba0', lineage).
narrative_ontology:cs_interpretation_layer_present('7dd7608f-ebe5-425d-ada2-a0175c070ba0').
narrative_ontology:cs_reading_relation('7dd7608f-ebe5-425d-ada2-a0175c070ba0', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('7dd7608f-ebe5-425d-ada2-a0175c070ba0', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7dd7608f-ebe5-425d-ada2-a0175c070ba0', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_axiom('7dd7608f-ebe5-425d-ada2-a0175c070ba0', foundational, human_dignity_prior_to_efficiency).
narrative_ontology:cs_axiom_status(human_dignity_prior_to_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('7dd7608f-ebe5-425d-ada2-a0175c070ba0', human_dignity_prior_to_efficiency, deontological).
narrative_ontology:cs_axiom('7dd7608f-ebe5-425d-ada2-a0175c070ba0', foundational, magisterial_interpretive_authority_over_common_good).
narrative_ontology:cs_axiom_status(magisterial_interpretive_authority_over_common_good, holdable).
narrative_ontology:cs_axiom_grounding('7dd7608f-ebe5-425d-ada2-a0175c070ba0', magisterial_interpretive_authority_over_common_good, conventional).
narrative_ontology:cs_axiom('7dd7608f-ebe5-425d-ada2-a0175c070ba0', secondary, solidarity_obligations_are_binding_not_optional).
narrative_ontology:cs_axiom_status(solidarity_obligations_are_binding_not_optional, holdable).
narrative_ontology:cs_axiom_grounding('7dd7608f-ebe5-425d-ada2-a0175c070ba0', solidarity_obligations_are_binding_not_optional, deontological).
narrative_ontology:cs_reference_frame('7dd7608f-ebe5-425d-ada2-a0175c070ba0', conciliar_social_doctrine_synthesis).
narrative_ontology:cs_drift_state('7dd7608f-ebe5-425d-ada2-a0175c070ba0', contemporary_ai_deployment_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7dd7608f-ebe5-425d-ada2-a0175c070ba0', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, industrial_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_and_local_communities).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_and_disabled_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_ai_contractors).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_and_asset_managers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, lay_catholic_ai_ethicists).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues encyclicals, pastoral letters, and diplomatic interventions declaring which AI governance arrangements conform to Catholic Social Doctrine. Convenes ecclesial bodies (e.g. Pontifical Academies, Vatican tech dialogues) to interpret subsidiarity and solidarity for novel technological contexts. Holds no direct regulatory power over states or firms but sets the interpretive standard that civil society, allied governments, and religious institutions invoke.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, the_magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Face displacement and de-skilling from AI-driven automation. The doctrine's insistence on subordinating technology to labor dignity and the common good gives them a moral and rhetorical lever — union campaigns, Catholic labor movements, and bishops' conferences cite the framework to demand retraining guarantees, algorithmic transparency at the workplace, and limits on surveillance-based management.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, industrial_workers, beneficiary,
    moderate, biographical, constrained, national).

% Bear the externalities of AI infrastructure (data extraction, resource-intensive compute, algorithmic bias trained on data that excludes them) without proportionate voice in governance. The universal-destination-of-goods principle is invoked by Church-affiliated NGOs and bishops' conferences to argue AI's benefits and data commons must be shared, not concentrated in originating firms and states.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Experience AI's effects through children's exposure to algorithmic media, gig-economy platform dependency, and erosion of local mediating institutions. Subsidiarity doctrine is invoked to insist decisions affecting families be resolved at the lowest competent level rather than by distant platform operators or state technocrats.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_and_local_communities, beneficiary,
    powerless, biographical, constrained, local).

% Are disproportionately harmed by algorithmic discrimination in credit, healthcare triage, and welfare administration. The doctrine's 'protection of the vulnerable' clause is cited in advocacy litigation and Church-run social service interventions to demand accountability mechanisms and human review of automated decisions affecting them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_and_disabled_populations, beneficiary,
    powerless, biographical, trapped, national).

% Operate dominant AI platforms optimized for engagement, efficiency, and capital return. Face reputational and regulatory pressure when Catholic institutions, allied governments, or shareholder coalitions invoke the doctrine to demand transparency audits, worker protections, or resource-sharing commitments. Can absorb compliance costs, relocate operations, or fund parallel 'ethical AI' initiatives that satisfy the letter without the substance — their exit options remain wide even under moral pressure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    institutional, biographical, arbitrage, global).

% Develop autonomous weapons systems and surveillance infrastructure that the doctrine's common-good and just-war-adjacent reasoning explicitly condemns. Face episodic Vatican diplomatic pressure, disinvestment campaigns by Catholic pension funds, and civil society mobilization, but retain state contracts and lobbying power that largely insulate core operations.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_ai_contractors, payer,
    institutional, generational, arbitrage, global).

% Fund and profit from AI infrastructure buildout (data centers, compute markets, algorithmic trading) without regard to distributive effects. Targeted by Catholic-affiliated shareholder activism and encyclical-referencing divestment campaigns; can reallocate capital across jurisdictions faster than doctrinal pressure can be operationalized into binding constraint.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_and_asset_managers, payer,
    institutional, biographical, arbitrage, global).

% Hold actual binding regulatory authority over AI but are not bound by, and often do not formally recognize, the Magisterium's interpretive authority. They would object that the doctrine's legitimacy claim bypasses democratic deliberation and pluralist consent-of-the-governed processes, yet they are not parties to the doctrinal reading — they observe and sometimes selectively cite it, but do not sit inside its authority structure.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, national_governments_and_regulators, excluded,
    institutional, generational, constrained, national).

% Interpret and apply the doctrine's principles to specific AI governance debates (algorithmic bias, labor automation, autonomous weapons), often within pontifical academies or Catholic universities. Gain professional standing and institutional voice from the framework's legitimacy claim, but depend on the Magisterium's continued authority for their own interpretive relevance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, lay_catholic_ai_ethicists, observer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, lay_catholic_ai_ethicists, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, non-market, non-state normative vocabulary (common good, subsidiarity, solidarity, universal destination of goods) through which otherwise unorganized parties — displaced workers, Global South communities, families, disabled populations — can jointly articulate demands on AI governance and coordinate advocacy, litigation, and shareholder pressure across borders and languages.
% TRANSFER_FUNCTION: Moves reputational costs, compliance burdens, and (where campaigns succeed) capital allocation and regulatory concessions from technology monopolies, military AI contractors, and extractive finance toward displaced workers, marginalized populations, and Global South communities, mediated through moral suasion, ecclesial witness, and civil-society/international-law advocacy rather than binding law.
% ABSENT_VOICES: National governments and regulators who hold actual enforcement power are structurally outside the doctrinal authority — they may cite the framework selectively but are not bound by Magisterial interpretation, meaning the constraint's demands often lack a party capable of converting moral suasion into binding rule. Non-Catholic religious and secular ethical traditions with competing accounts of dignity and the common good are also absent from the interpretive process even where they would concur with specific outcomes.
% DISAPPEARANCE_RATIONALE: Beneficiary groups and lay Catholic ethicists would say the world rearranges: a recognized moral vocabulary for contesting AI concentration would vanish, weakening cross-border worker and Global South advocacy coalitions that currently borrow its legitimacy and media reach. Tech monopolies, military contractors, and finance would say little changes materially, since the doctrine carries no binding enforcement power and its practical effect is reputational friction and occasional divestment pressure they already routinely absorb or route around.
% FOUNDING_PROBLEM: Rapid AI deployment concentrating economic and political power in unaccountable technical and corporate hierarchies, displacing labor, extracting from the Global South, and bypassing the dignity of persons treated as data sources or optimization targets rather than moral agents — the doctrine was articulated to insist technology remain subordinate to persons and the common good rather than treated as an autonomous, self-justifying force.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economists and UN-affiliated digital rights rapporteurs corroborate the displacement and Global South extraction problem the doctrine names, though they do not endorse Magisterial interpretive authority as the correct remedy. Secular AI ethics scholars (e.g. in algorithmic fairness and platform political economy literatures) attest the underlying problem — concentrated, unaccountable AI power — is real and unresolved, providing corroboration from outside the Catholic institutional apparatus that benefits from the doctrine's continued relevance.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.5, mid-range and rising over the interval, reflecting that the doctrine's enforcement mechanisms (moral suasion, divestment campaigns, ecclesial diplomacy) are becoming more organized and consequential but remain non-binding — real cost is imposed on payer seats, but nothing like the extraction ceiling of a legally enforced constraint. Suppression is moderate (0.38): the doctrine does not coerce compliance through law, but it does suppress alternative framings within Catholic institutional spaces (a Catholic university or diocese cannot easily adopt a pure market-libertarian AI ethics without institutional friction). Accessibility collapse is low (0.32) because tech monopolies, states, and finance retain ample alternative governance frameworks to invoke (technocratic, market-libertarian, or pluralist) — the doctrine has not foreclosed those options in the wider world, only within its own institutional domain. Resistance is high (0.68) because the payer seats are institutionally powerful and organized, actively contesting the doctrine's application through lobbying, counter-framing, and jurisdictional arbitrage.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups (workers, Global South populations, families, marginalized populations) are declared low-d: the doctrine's coordination function exists specifically to aggregate their otherwise-dispersed moral and political claims. Payer groups (tech monopolies, military contractors, extractive finance) are declared high-d: they bear the doctrine's reputational and compliance costs when its principles are successfully invoked against their practices, though their arbitrage-grade exit options (jurisdiction-shopping, parallel ethical-AI initiatives, capital reallocation) dampen the effective extraction relative to a target with no exit. The Magisterium itself sits as agenda_setter with analytical exit — it does not bear or collect material extraction, it administers the interpretive standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unaccountable concentration of AI power against human dignity and the common good) is authored as live, corroborated by independent labor economists and digital-rights scholars outside the Catholic institutional apparatus — this blocks a mandatrophy read where the doctrine's continued invocation would be pure zombie ritual. However, the doctrine's practical enforcement mechanism (moral suasion without binding authority) risks drifting toward theater if invocation substitutes for structural remedy — the rising theater_ratio (0.25 to 0.42) is authored to reflect this risk: as the doctrine becomes more frequently cited in corporate ESG and Vatican-adjacent conferences, an increasing share of its invocation may function as reputational cover rather than genuine constraint on practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterial_authority_vs_pluralist_legitimacy,
    'Does legitimate AI governance require a single authoritative moral tradition''s interpretation (as this reading holds), or does legitimacy require inclusive democratic deliberation among traditions (as the pluralist reading holds) — and can these be reconciled, or must one framework''s legitimacy claim be rejected by the other?',
    'Track whether international AI governance instruments (UN frameworks, EU AI Act successors) formally cite religious doctrinal sources as binding versus advisory; track whether Catholic-affiliated advocacy achieves binding regulatory change versus remaining persuasive-only.',
    'If doctrinal authority is treated as merely advisory input to pluralist processes, this reading''s legitimacy claim weakens toward the democratic_pluralist_reading; if states or supranational bodies formally defer to doctrinal interpretation in specific domains (e.g. bioethics-adjacent AI applications), it strengthens toward genuine co-authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_vs_pluralist_legitimacy, conceptual, 'Committer structure: whether Magisterial authority and democratic pluralist authority can coexist or are mutually foreclosing as legitimacy grounds.').

omega_variable(
    subsidiarity_solidarity_internal_tension,
    'Do subsidiarity (decisions at the lowest competent level) and solidarity (binding obligations toward the vulnerable at scale) pull the doctrine toward internally contradictory governance prescriptions for AI, given that AI harms are often only remediable at supranational scale?',
    'Case analysis of specific doctrinal interventions (e.g. Vatican statements on AI and labor) for whether subsidiarity or solidarity was invoked to justify the same policy recommendation, and whether local versus global framing changed the practical remedy demanded.',
    'If the tension is genuine and unresolved, the tangled_rope classification is reinforced (coordination function is internally strained, not merely externally contested); if the doctrine consistently resolves the tension coherently, the coordination function is more robust than the tangled_rope framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_solidarity_internal_tension, conceptual, 'Internal doctrinal tension between subsidiarity and solidarity as applied to transnational AI harms.').

omega_variable(
    enforcement_capacity_gap,
    'Given the doctrine has no binding regulatory power, does its moral-suasion-only enforcement mechanism actually shift material outcomes for beneficiary groups, or does it primarily produce reputational and rhetorical effects without altering the underlying distribution of AI''s costs and benefits?',
    'Longitudinal tracking of Catholic-affiliated divestment campaigns, shareholder resolutions citing the doctrine, and diocesan/NGO advocacy outcomes against a counterfactual baseline of campaigns not invoking the doctrinal framework.',
    'If material outcomes are negligible, the theater_ratio trajectory should be revised upward and the constraint reclassified toward piton (persisting through institutional inertia and ecclesial performance rather than genuine coordination); if material outcomes are measurable, the tangled_rope classification with rising extraction is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Whether moral-suasion enforcement produces material redistribution or primarily reputational theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 24, 0.5).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ai_governance_legitimacy__magisterial_subsidiarity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.1).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the ai_governance_legitimacy kernel. Each reading authors its own ε, beneficiary/victim structure, and claimed type from its own commitment framework: magisterial_subsidiarity_reading (this story, tangled_rope, ε≈0.50, grounds legitimacy in Magisterial doctrinal interpretation); technocratic_optimization_reading (legitimacy from welfare/efficiency maximization by technical experts); democratic_pluralist_reading (legitimacy from inclusive democratic deliberation); market_libertarian_reading (legitimacy from voluntary exchange and property rights, treating solidarity mandates as illegitimate coercion). The readings are linked bidirectionally in the network graph because each reading's success in institutional practice changes the resource availability and legitimacy conditions the others operate under (e.g. binding regulatory adoption of one reading crowds out practical space for the others) without any single reading logically foreclosing all the others in every framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
