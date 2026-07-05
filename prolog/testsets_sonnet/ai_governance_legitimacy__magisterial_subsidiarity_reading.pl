% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: AI Governance Legitimacy via Magisterial Catholic Social Doctrine (Subsidiarity Reading)
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story instantiates the magisterial_subsidiarity_reading of the
 *   ai_governance_legitimacy kernel: the claim that legitimate AI governance
 *   derives from conformity to Catholic Social Doctrine as authoritatively
 *   interpreted by the Magisterium, subordinating technology to common good,
 *   subsidiarity, solidarity, and universal destination of goods. This is one
 *   of four incompatible readings of what makes AI governance legitimate. It
 *   is generated as a clean, self-contained constraint with its own stable
 *   epsilon; the sibling readings (technocratic_optimization,
 *   democratic_pluralist, market_libertarian) are separate constraints, not
 *   alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - magisterium: agenda_setter (institutional/analytical) — issues doctrinal interpretation, convenes advocacy, holds no material enforcement power itself
 *   - industrial_and_gig_workers, global_south_populations, families_and_local_communities, marginalized_and_disabled_populations: beneficiaries (powerless/trapped) — gain a coordinating moral vocabulary and advocacy network they otherwise lack
 *   - private_tech_monopolies, military_industrial_complex, extractive_finance_sector: payers (institutional/arbitrage-to-constrained) — bear reputational and compliance costs from doctrinal pressure
 *   - national_governments, non_catholic_civil_society: excluded — hold real regulatory power or share overlapping goals but have no seat in Magisterial interpretive process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.42).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy via Magisterial Catholic Social Doctrine (Subsidiarity Reading)").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'ecc964ac-414c-45e7-8302-cad2ada7e785').
narrative_ontology:cs_kernel_codification('ecc964ac-414c-45e7-8302-cad2ada7e785', fixed_text).
narrative_ontology:cs_authority_grounding('ecc964ac-414c-45e7-8302-cad2ada7e785', lineage).
narrative_ontology:cs_interpretation_layer_present('ecc964ac-414c-45e7-8302-cad2ada7e785').
narrative_ontology:cs_reading_relation('ecc964ac-414c-45e7-8302-cad2ada7e785', ai_governance_legitimacy__technocratic_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('ecc964ac-414c-45e7-8302-cad2ada7e785', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ecc964ac-414c-45e7-8302-cad2ada7e785', ai_governance_legitimacy__market_libertarian_reading, influences).
narrative_ontology:cs_axiom('ecc964ac-414c-45e7-8302-cad2ada7e785', foundational, human_dignity_prior_to_efficiency).
narrative_ontology:cs_axiom_status(human_dignity_prior_to_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('ecc964ac-414c-45e7-8302-cad2ada7e785', human_dignity_prior_to_efficiency, deontological).
narrative_ontology:cs_axiom('ecc964ac-414c-45e7-8302-cad2ada7e785', foundational, magisterium_holds_authoritative_interpretive_primacy).
narrative_ontology:cs_axiom_status(magisterium_holds_authoritative_interpretive_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ecc964ac-414c-45e7-8302-cad2ada7e785', magisterium_holds_authoritative_interpretive_primacy, conventional).
narrative_ontology:cs_axiom('ecc964ac-414c-45e7-8302-cad2ada7e785', secondary, subsidiarity_requires_solidarity_correction).
narrative_ontology:cs_axiom_status(subsidiarity_requires_solidarity_correction, holdable).
narrative_ontology:cs_axiom_grounding('ecc964ac-414c-45e7-8302-cad2ada7e785', subsidiarity_requires_solidarity_correction, instrumental).
narrative_ontology:cs_reference_frame('ecc964ac-414c-45e7-8302-cad2ada7e785', conciliar_and_papal_social_teaching_corpus).
narrative_ontology:cs_drift_state('ecc964ac-414c-45e7-8302-cad2ada7e785', contemporary_ai_deployment_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ecc964ac-414c-45e7-8302-cad2ada7e785', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, industrial_and_gig_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_and_local_communities).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_and_disabled_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_sector).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues encyclicals and doctrinal interpretations declaring which AI governance arrangements conform to Catholic Social Doctrine. Convenes conferences, publishes statements (e.g. Rome Call for AI Ethics), and mobilizes the institutional Church's moral authority to pressure states and firms. Does not itself operate AI systems or hold market power; its leverage is reputational, pastoral, and diplomatic.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Subject to algorithmic management, automation displacement, and platform surveillance. The doctrine's insistence that technology serve labor rather than displace it without transition support gives them rhetorical and organizing leverage (labor encyclicals, Catholic union alliances) they otherwise lack. They have no direct exit from the AI-mediated labor market.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, industrial_and_gig_workers, beneficiary,
    powerless, biographical, trapped, national).

% Recipients of AI systems designed and deployed by Global North firms with little participatory input. The doctrine's universal-destination-of-goods principle is invoked to demand technology transfer, data sovereignty, and inclusion in governance design. They cannot exit the global AI supply chain but gain a moral vocabulary and ecclesial advocacy network (e.g. through local dioceses and Caritas) pressing their case internationally.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Targeted by algorithmic content systems, surveillance, and automated decision-making in education, welfare, and policing. The subsidiarity principle is invoked to argue decisions should be made at the most local competent level rather than centralized in distant platforms or states, giving local institutions (parishes, schools, municipalities) a doctrinal basis to resist top-down algorithmic administration.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_and_local_communities, beneficiary,
    powerless, biographical, constrained, local).

% Disproportionately harmed by biased automated decision systems in credit, hiring, healthcare triage, and benefits administration. The doctrine's protection-of-the-vulnerable clause is used to demand algorithmic audits, appeal mechanisms, and non-discrimination guarantees they cannot secure through market bargaining.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_and_disabled_populations, beneficiary,
    powerless, biographical, trapped, national).

% Large AI developers and platform firms whose business models depend on data extraction, engagement optimization, and labor automation at scale. Doctrinal pressure (reputational campaigns, shareholder activism coordinated with Catholic investment funds, regulatory alliances) imposes compliance costs — audits, transparency disclosures, participatory design requirements — that constrain their preferred deployment speed and data practices. They retain substantial capacity to relocate operations or reframe compliance as branding.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    institutional, biographical, arbitrage, global).

% Developers and procurers of autonomous weapons systems and military AI. The doctrine's common-good and dignity principles are invoked (papal statements against 'killer robots', lethal autonomous weapons) to build international advocacy coalitions pressing for bans or restrictions, imposing reputational and, where successful, legal costs on this sector.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Financial institutions deploying AI for algorithmic trading, predatory credit scoring, and automated foreclosure/eviction processing. Doctrinal advocacy for the universal destination of goods and solidarity is used to press for usury-adjacent lending scrutiny and algorithmic fairness mandates, constraining practices otherwise justified purely by profitability.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_sector, payer,
    institutional, biographical, arbitrage, global).

% Hold actual binding regulatory authority over AI within their jurisdictions but are not bound by Magisterial teaching and frequently balance competing doctrinal, economic, and geopolitical pressures. Secular and religiously pluralistic states have no formal seat in Magisterial deliberation despite bearing implementation responsibility for any resulting policy asks.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, national_governments, excluded,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, national_governments, observer).

% Secular human rights organizations, indigenous rights movements, and non-Christian religious bodies often share overlapping goals (algorithmic justice, labor protection) but are not party to the doctrine's interpretive process and may object to a single religious hierarchy claiming interpretive authority over universal governance norms.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, non_catholic_civil_society, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, internationally legible normative vocabulary and institutional advocacy network (dioceses, Catholic universities, Vatican diplomacy, Caritas) that coordinates otherwise fragmented resistance to extractive AI deployment across labor, Global South, family, and disability constituencies who individually lack leverage against powerful tech, military, and finance actors.
% TRANSFER_FUNCTION: Moves reputational costs, compliance costs, and in some cases binding regulatory costs from powerless, diffuse populations (workers, Global South communities, families, marginalized groups) onto concentrated institutional actors (tech monopolies, military-industrial firms, extractive finance) via moral suasion, civil society pressure, and international law advocacy — without moving material resources or governance authority into the Magisterium's own hands, since the Magisterium does not extract rents or control AI infrastructure itself.
% ABSENT_VOICES: National governments hold actual regulatory authority but have no seat in Magisterial doctrinal interpretation despite implementing consequences; secular civil society and non-Christian religious traditions share overlapping goals but object to a single hierarchical religious authority claiming universal interpretive primacy over AI governance legitimacy as such.
% DISAPPEARANCE_RATIONALE: The Magisterium and the beneficiary constituencies would argue the world rearranges substantially: a documented moral-advocacy infrastructure (Rome Call for AI Ethics signatories, Catholic labor and investment coalitions, diplomatic interventions on autonomous weapons) would lose its coordinating vocabulary and institutional convening power, weakening pressure on tech monopolies, military AI developers, and extractive finance. Technocratic and market-libertarian readers would argue the world is largely unchanged: actual binding AI regulation is produced by states and international bodies, not ecclesial pronouncement, and the doctrine's practical effect is supplementary moral pressure rather than load-bearing governance architecture.
% FOUNDING_PROBLEM: Catholic Social Doctrine's application to AI was built to prevent technology from being governed purely by market efficiency or state power logics that historically produced labor exploitation (industrial revolution), colonial extraction, and dehumanizing bureaucratic administration — extending a century-old tradition (Rerum Novarum onward) of asserting that economic and technological arrangements must answer to human dignity rather than the reverse.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economists and Global South technology-policy researchers (outside the Church hierarchy) corroborate that algorithmic labor displacement, data colonialism, and unaccountable automated decision-making in welfare and credit systems remain active, unresolved problems — the founding problem the doctrine addresses has not been solved by market or state action alone. However, secular AI ethics scholars dispute whether Magisterial interpretive authority specifically, as opposed to pluralistic deliberative processes, is the necessary or legitimate mechanism for resolving it — corroboration of the problem's persistence does not extend to corroboration of this reading's exclusive interpretive claim.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.50, mid-high, reflecting genuine but non-coercive redistribution of reputational and compliance costs from concentrated extractive actors to diffuse vulnerable populations via moral and diplomatic mechanisms rather than binding legal force. Suppression is moderate (0.42) because the mechanism is persuasive/reputational (moral suasion, civil society pressure, ecclesial witness) rather than coercive law; it cannot compel tech monopolies or states, only pressure them. Theater ratio starts moderate and rises over the interval (0.22 to 0.38) reflecting a documented pattern where high-profile signings (Rome Call for AI Ethics) accumulate symbolic commitments faster than measurable changes in deployed AI systems — a real risk of the doctrine's own critics that the story's metrics should not paper over. Accessibility collapse is moderate (0.40): alternative governance framings (secular, market, technocratic) remain fully available and actively compete, so alternatives have not collapsed. Resistance is high (0.72) because tech monopolies, defense contractors, and finance sector actors actively contest and route around the doctrine's claims, and other religious/secular traditions contest the Magisterium's interpretive monopoly itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups (workers, Global South populations, families, marginalized populations) are powerless and largely trapped in the AI systems governing their labor, credit, welfare, and information access; the doctrine's coordination function gives them a legible advocacy structure, pushing their directionality toward the beneficiary end. Payer groups (tech monopolies, military-industrial complex, extractive finance) hold institutional power and, for tech and finance, arbitrage-grade exit (relocation, regulatory shopping) that damps but does not eliminate the costs imposed on them; the military-industrial complex is more constrained because autonomous-weapons advocacy targets binding international law, not just reputation. The Magisterium itself is agenda_setter rather than beneficiary or payer — it does not collect material rents from the arrangement, which is structurally important: this is not a case of a religious hierarchy self-enriching, but of a hierarchy converting moral authority into pressure applied to others.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (technology and economic power operating without accountability to human dignity) remains empirically live per independent labor and Global South technology-policy research, which prevents this from being scored as a zombie mandate. But the founding_problem_status is authored as 'live' rather than 'resolved,' and the disappearance_verdict is authored as 'contested' rather than 'world_rearranges' outright — reflecting that critics reasonably dispute whether the Magisterium's specific interpretive apparatus, as opposed to the underlying problem, is doing the coordinating work, or whether it is a a supplementary voice riding on problems that secular and state mechanisms would address regardless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_vs_pluralism,
    'Does legitimate AI governance require a single authoritative interpretive tradition (the Magisterium), or is the Magisterium''s contribution properly one voice among many in a pluralistic deliberative process?',
    'Track whether binding international AI governance instruments (UN, OECD, EU AI Act successors) cite Magisterial doctrine as source authority versus as one input among many secular and religious traditions; track whether non-Catholic populations affected by the doctrine''s advocacy report feeling represented or excluded by its interpretive claims.',
    'If governance instruments and affected populations treat Magisterial doctrine as one input among many, this reading collapses toward the democratic_pluralist_reading''s structure; if it is treated as uniquely authoritative in practice (e.g., through disproportionate diplomatic access), the interpretive-monopoly claim is empirically vindicated within Catholic-influenced jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_vs_pluralism, conceptual, 'Whether AI governance legitimacy is properly singular (Magisterial) or plural (multi-tradition) — the central fault line between this reading and its siblings.').

omega_variable(
    moral_suasion_efficacy_ambiguity,
    'Does moral suasion and ecclesial witness produce measurable material change in AI deployment practices, or does it primarily produce symbolic commitments (signings, statements) that do not translate into binding constraint on tech monopolies, military AI developers, or extractive finance?',
    'Longitudinal tracking of Rome Call for AI Ethics signatories'' actual deployment practices against their commitments; comparison of algorithmic labor protections in jurisdictions with strong Catholic civil society presence versus without.',
    'If moral suasion produces negligible material change, the theater_ratio trajectory understates the degree of pure symbolic performance and the constraint drifts toward piton (form without function) rather than tangled_rope; if it produces measurable constraint on extractive actors, the tangled_rope classification with genuine coordination function is supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_suasion_efficacy_ambiguity, empirical, 'Whether the doctrine''s enforcement mechanism (moral suasion) achieves real extraction-reducing effects or is substantially theatrical.').

omega_variable(
    beneficiary_representation_ambiguity,
    'Do the declared beneficiary populations (workers, Global South communities, marginalized groups) actually experience the Magisterium''s advocacy as representing their interests, or is the Magisterium''s interpretation of their interests substituted for their own direct participation?',
    'Survey or participatory audit of whether affected populations were consulted in forming the doctrine''s specific AI governance positions, versus positions being derived top-down from doctrinal reasoning without direct input.',
    'If beneficiary populations were not meaningfully consulted, the ''participatory governance'' principle the doctrine itself demands of others is not met by the doctrine''s own production process — an internal consistency problem that would weaken (though not eliminate) its coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_representation_ambiguity, empirical, 'Whether declared beneficiaries had actual participatory input into the doctrine that claims to represent their interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 24, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 4, 0.31).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.1).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading in a four-member family sharing the kernel ai_governance_legitimacy. Each sibling reading locates legitimacy in a different authority structure (Magisterial doctrine, technical/aggregate-welfare expertise, democratic deliberation, voluntary market exchange) and produces a different beneficiary/victim structure and constraint type. This reading is authored as tangled_rope (genuine coordination of vulnerable-population advocacy entangled with real cost imposition on extractive institutional actors); the technocratic and market_libertarian readings would authored as lower-extraction ropes or even mountain-adjacent framings from their own proponents' seats, while the democratic_pluralist reading would authored with lower suppression and a more distributed beneficiary/victim structure. The families are linked bidirectionally via affects_constraints so contamination/coupling analysis can trace how doctrinal pressure from this reading changes the legitimacy conditions and resource availability the sibling readings operate under, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
