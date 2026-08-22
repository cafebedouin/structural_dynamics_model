% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: AI Governance Legitimacy via Magisterial Subsidiarity
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint story captures one reading of the contested kernel
 *   'ai_governance_legitimacy': the magisterial_subsidiarity_reading, which
 *   grounds AI governance legitimacy in conformity to Catholic Social
 *   Doctrine as authoritatively interpreted by the Magisterium. The reading
 *   entangles genuine coordination (a unified moral framework for protecting
 *   the vulnerable across labor, migration, health, finance, and warfare)
 *   with asymmetric extraction (the Magisterium and Catholic institutions
 *   gain legitimating authority and material influence; private tech
 *   monopolies, the military-industrial complex, and extractive finance bear
 *   compliance costs that threaten their core revenue logics). The constraint
 *   is a tangled_rope because it simultaneously solves a real coordination
 *   problem (fragmented ethical responses to AI harm) and extracts from
 *   identifiable actors who cannot exit the systems they dominate.
 *   Enforcement operates through moral suasion, civil society pressure,
 *   international law advocacy, and ecclesial witness — not state coercion —
 *   but the suppression metric reflects the structural difficulty these
 *   actors face in resisting the framework's demands without losing
 *   legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.4).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy via Magisterial Subsidiarity").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, '91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e').
narrative_ontology:cs_kernel_codification('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', formalized).
narrative_ontology:cs_authority_grounding('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', lineage).
narrative_ontology:cs_interpretation_layer_present('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e').
narrative_ontology:cs_reading_relation('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_reading_relation('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', foundational, human_dignity_primacy_over_efficiency).
narrative_ontology:cs_axiom_status(human_dignity_primacy_over_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', human_dignity_primacy_over_efficiency, deontological).
narrative_ontology:cs_axiom('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', foundational, universal_destination_of_goods_applies_to_data_and_compute).
narrative_ontology:cs_axiom_status(universal_destination_of_goods_applies_to_data_and_compute, holdable).
narrative_ontology:cs_axiom_grounding('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', universal_destination_of_goods_applies_to_data_and_compute, deontological).
narrative_ontology:cs_axiom('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', secondary, subsidiarity_requires_participatory_governance_not_mere_decentralization).
narrative_ontology:cs_axiom_status(subsidiarity_requires_participatory_governance_not_mere_decentralization, holdable).
narrative_ontology:cs_axiom_grounding('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', subsidiarity_requires_participatory_governance_not_mere_decentralization, conventional).
narrative_ontology:cs_axiom('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', foundational, lethal_autonomous_weapons_intrinsically_immoral).
narrative_ontology:cs_axiom_status(lethal_autonomous_weapons_intrinsically_immoral, holdable).
narrative_ontology:cs_axiom_grounding('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', lethal_autonomous_weapons_intrinsically_immoral, deontological).
narrative_ontology:cs_reference_frame('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', catholic_social_doctrine_1891_2020).
narrative_ontology:cs_drift_state('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', algorithmic_capitalism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('91d9f3ed-9ea9-4e4b-ad71-02ccb08b022e', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, industrial_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, precariat_laborers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, surveillance_capitalism_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets Catholic Social Doctrine for AI governance through encyclicals (e.g., Centesimus Annus, Laudato Si', Fratelli Tutti) and dicastery documents (e.g., Dicastery for Culture and Education, Pontifical Academy of Sciences). Sets the normative framework that subordinates technology to human dignity, common good, subsidiarity, solidarity, and universal destination of goods. Does not directly enforce but exercises moral authority that shapes Catholic institutions, civil society, and international advocacy networks. Exit is analytical: the Magisterium's authority is constitutive of its identity and cannot be exited without ceasing to be the Magisterium.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Networks of Catholic NGOs (Caritas, Sant'Egidio, Focolare, Catholic Worker), bishops' conferences, and lay movements that operationalize Magisterial teaching into advocacy, service delivery, and grassroots governance experiments. They benefit from the legitimating framework that connects their work to ecclesial authority, but their exit is constrained by institutional identity, funding dependencies, and the theological claim that this work expresses their baptismal vocation. They actively pressure tech companies and governments through shareholder advocacy, UN lobbying, and local governance participation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_civil_society, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_civil_society, agenda_setter).

% Factory workers, gig economy drivers, warehouse laborers, and informal sector workers whose dignity and rights are threatened by algorithmic management, predictive scheduling, and automated discipline. The Magisterial framework names their exploitation as a violation of subsidiarity (decisions made remotely without their participation) and solidarity (profits extracted without sharing gains). They are trapped: exit from algorithmic systems means loss of livelihood; collective bargaining is suppressed by platform architecture; their voice enters governance only through Catholic labor theology intermediaries.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, industrial_workers, beneficiary,
    powerless, biographical, trapped, global).

% Communities in Africa, Latin America, and Asia where AI deployment often takes extractive forms: data colonialism, algorithmic resource extraction, biometric surveillance tied to aid conditionality, and automation that bypasses labor-intensive development stages. The universal destination of goods principle frames their data and resources as common patrimony, not raw material for Northern monopolies. Exit is constrained by geopolitical dependency, debt structures, and the lack of alternative development models — but they hold moral authority in Magisterial discourse as the primary subjects of solidarity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities, beneficiary,
    moderate, generational, constrained, continental).

% Families facing algorithmic intrusion in education (edtech surveillance), healthcare (predictive analytics overriding parental judgment), social services (risk-scoring that targets poor families), and domestic life (smart devices harvesting intimate data). The Magisterial framework treats the family as a pre-political society protected by subsidiarity — decisions about children's formation belong to parents, not platforms. Exit is identity-locked: the family's self-understanding as a 'domestic church' makes technological mediation of its internal life a theological crisis, not just a privacy concern.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families, beneficiary,
    moderate, biographical, identity_locked, global).

% Migrants, refugees, undocumented persons, racialized minorities, disabled persons, and elderly isolated by digitalization — groups whom AI systems routinely exclude, misclassify, or target. The preferential option for the poor (a Magisterial axiom) makes their protection the test of any governance regime's legitimacy. They are trapped in systems they cannot audit, challenge, or escape: biometric borders, predictive policing, benefit fraud algorithms, inaccessible interfaces. Their inclusion in governance is structurally absent unless mediated by Catholic institutions that claim to represent them.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations, beneficiary,
    powerless, immediate, trapped, global).

% Platform companies (Alphabet, Meta, Amazon, Microsoft, Apple) and AI labs (OpenAI, Anthropic, DeepMind) whose business models depend on surveillance, behavioral modification, labor displacement, and regulatory capture. The Magisterial framework demands: data sovereignty for users, algorithmic transparency, worker participation in automation decisions, profit-sharing via universal destination of goods, and political oversight that limits scale. These firms have arbitrage-grade exit: they can relocate incorporation, restructure ownership, fund counter-narratives, and lobby supranational bodies — but the framework targets their core revenue logic, making compliance existentially costly.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, biographical, arbitrage, global).

% Defense contractors, autonomous weapons programs, and state surveillance apparatuses that develop lethal AI systems. The Magisterial framework (following Gaudium et Spes and recent dicastery statements) demands a ban on lethal autonomous weapons, human control over use-of-force decisions, and subordination of military AI to just war criteria. Exit is constrained: these actors are embedded in state power and cannot simply pivot; but they can absorb the framework as 'ethics washing' while continuing development, using the Magisterial language of 'human dignity' to legitimize 'human-in-the-loop' systems that preserve autonomy in name only.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Hedge funds, private equity, and algorithmic trading firms that use AI for high-frequency extraction, predatory lending algorithms, and financialization of basic needs (housing, water, care). The universal destination of goods principle directly challenges their claim to private appropriation of socially produced value. They have arbitrage exit: regulatory arbitrage, jurisdiction shopping, and complexity as a shield — but the framework's demand for participatory governance of finance threatens their structural opacity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance, payer,
    powerful, biographical, arbitrage, global).

% Data brokers, ad-tech ecosystems, and behavioral prediction markets that commodify human experience. The Magisterial framework treats attention, intimacy, and behavioral data as non-commodifiable aspects of human dignity — not 'raw material' for prediction products. They have arbitrage exit through technical obfuscation, legal personhood maneuvers, and capture of 'ethical AI' certification — but the framework's insistence on transparent accountability and participatory governance strikes at their epistemic monopoly.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, surveillance_capitalism_operators, payer,
    powerful, biographical, arbitrage, global).

% UN Human Rights Council, OECD AI Principles, EU AI Act architects, IEEE standards bodies — institutions that share substantive overlap (human dignity, non-discrimination, participation) but reject the Magisterial authority claim. They engage Catholic actors as partners in multi-stakeholder forums while maintaining that legitimacy derives from democratic consent, not ecclesial teaching. Their analytical seat is mobile: they can adopt Magisterial language instrumentally without accepting its theological ground.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_human_rights_institutions, observer,
    institutional, generational, analytical, global).

% Academic and corporate AI ethics researchers who frame governance as optimization of fairness, transparency, and safety metrics. They treat Catholic Social Doctrine as one 'value system' among many to be balanced against innovation and efficiency. Their exit is mobile: they can shift frameworks, funders, and institutional homes without identity cost — but their professional incentives align with the technocratic_optimization_reading, making them structural skeptics of Magisterial authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, technocratic_ai_ethicists, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified moral framework that translates abstract theological principles (common good, subsidiarity, solidarity, universal destination of goods) into concrete governance demands for AI: participatory design, algorithmic transparency, data sovereignty, worker voice in automation, prohibition of lethal autonomous weapons, and preferential protection for the vulnerable. This coordinates Catholic institutions, civil society, and allied movements across diverse contexts — labor, migration, health, finance, warfare — under a single legitimating grammar.
% TRANSFER_FUNCTION: Moves governance authority from private tech monopolies and state security apparatuses to politically accountable bodies informed by Magisterial teaching and Catholic social movements. Transfers material resources via demands for profit-sharing, data dividends, universal basic services, and reorientation of R&D from military/surveillance toward human needs. Transfers epistemic authority from proprietary algorithms to participatory auditing involving affected communities.
% ABSENT_VOICES: Non-Catholic religious traditions (Islamic, Buddhist, Indigenous cosmologies) that have their own AI ethics frameworks but are not invited into the Magisterial interpretation process. Secular feminists and queer theorists who contest the Magisterial anthropology of the family and gender. Workers in non-unionized, non-Catholic contexts who have no access to the Catholic labor theology intermediaries that claim to represent them. The excluded are structurally absent because the reading's authority claim is particularistic: it grounds legitimacy in *this* tradition's authoritative interpretation, not in inclusive public reason.
% DISAPPEARANCE_RATIONALE: If the Magisterial subsidiarity reading vanished overnight, the Catholic Church's vast institutional network (schools, hospitals, universities, NGOs, bishops' conferences, UN observer mission) would lose its coherent AI governance framework. Catholic shareholder advocacy would fragment. The moral vocabulary protecting workers in the Global South from data colonialism would lose its most organized institutional carrier. The demand for a lethal autonomous weapons ban would lose a key theological anchor in international diplomacy. The world would rearrange: tech governance would revert to the technocratic, market, and democratic-pluralist readings that currently dominate — with weaker protections for the vulnerable and no principle of universal destination of goods.
% FOUNDING_PROBLEM: The encyclicals Rerum Novarum (1891) through Fratelli Tutti (2020) identify the founding problem: industrial capitalism subordinates human dignity to efficiency, treats labor as a commodity, and concentrates power in ways that violate subsidiarity and solidarity. AI intensifies this problem by automating the extraction of behavioral data, displacing labor at scale, concentrating epistemic power in opaque systems, and enabling algorithmic warfare. The Magisterial reading was built to name this intensification as a theological crisis and to demand political subordination of technology to the human person.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the International Labour Organization (algorithmic management reports), UN Special Rapporteurs on extreme poverty and digital rights, and independent scholars (Shoshana Zuboff, Nick Couldry, Ulises Mejias) who document surveillance capitalism and data colonialism — none of whom are Catholic beneficiaries. The Magisterium's own tradition corroborates through the consistent thread from Leo XIII to Francis. The technocratic_optimization_reading disputes the status, claiming AI *solves* the founding problem via abundance; the market_libertarian_reading disputes it, claiming exit options protect dignity.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.5) reflects that the constraint demands substantial redistribution of epistemic and material power from tech monopolies and security apparatuses to vulnerable populations and participatory governance bodies. Suppression (0.4) is moderate: the constraint does not use state force but creates a legitimacy crisis for non-compliant actors — tech firms face shareholder revolts, regulatory risk, and talent loss; militaries face diplomatic isolation. Theater ratio (0.3) captures that Catholic institutions sometimes perform 'ethical AI' gestures (high-level principles, advisory boards) without structural change — but the Magisterial framework's specificity (universal destination of goods, subsidiarity as participation, preferential option for the poor) limits theatrical capture. Accessibility collapse (0.45) and resistance (0.55) reflect that alternatives (technocratic, market, democratic-pluralist) remain live and contested — the constraint does not collapse the option space. The measurement series track the intensification of the founding problem from industrial capitalism (Rerum Novarum) to algorithmic capitalism (Laudato Si', Fratelli Tutti), with extractiveness rising as AI amplifies the very dynamics Catholic Social Doctrine named.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's seat, the constraint is a rope: it coordinates the Church's global mission to defend human dignity in the digital age. From the trapped worker's seat, it is a tangled_rope: genuine protection entangled with dependence on Catholic intermediaries who may not represent them. From the tech monopoly's seat, it is a snare: the coordination story (human dignity) is cover for extracting their rents and epistemic monopoly. The engine computes this divergence from the structural data — the authored claim (tangled_rope) represents the generating model's structural judgment that the constraint *as a whole* entangles coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and Catholic civil society are structural beneficiaries (d near 0): they gain authority, coherence, and institutional relevance from the framework. Workers, Global South communities, families, and marginalized populations are beneficiaries with trapped or identity-locked exit — they gain protection but cannot leave the systems that harm them. Private tech monopolies, military-industrial complex, extractive finance, and surveillance capitalism are victims (d near 1): the framework targets their extraction logics directly. Secular human rights institutions and technocratic ethicists are observers with analytical/mobile exit — they engage instrumentally. The directionality derives from beneficiary/victim declarations plus exit: trapped beneficiaries have higher effective extraction than mobile ones; arbitrage-grade victims have lower effective extraction than constrained ones.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (industrial capitalism subordinating dignity to efficiency) is live and intensifying — AI automates and scales the extraction the Magisterium diagnosed. The constraint has not atrophied; its mandate has expanded. However, a mandatrophy risk exists: if Catholic institutions absorb 'ethical AI' language without challenging the technocratic_optimization_reading's dominance in actual governance (EU AI Act, OECD principles, corporate ethics boards), the Magisterial framework becomes performative — a piton. The theater ratio's plateau at 0.3 since 2020 signals this risk. The constraint remains a tangled_rope because the Magisterium *could* escalate (e.g., excommunication for AI weapons work, refusal of Church contracts with non-compliant firms) but has not — the enforcement remains moral suasion, not institutional discipline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterial_authority_vs_democratic_consent,
    'Does the Magisterium''s claim to authoritative interpretation of Catholic Social Doctrine for AI governance foreclose legitimate pluralism within Catholic communities and in public reason, or does it enable a distinctive contribution to overlapping consensus?',
    'Track whether Catholic institutions enforce doctrinal conformity on AI ethics (e.g., denying Communion to tech executives, censuring theologians) versus fostering internal debate. Track whether Magisterial interventions in UN/OEU forums are received as authoritative or as one voice among many.',
    'If the reading forecloses pluralism, it functions as a snare within Catholic communities (extracting conformity from dissenters). If it enables overlapping consensus, it functions as a rope in pluralistic governance. The current classification (tangled_rope) assumes both dynamics operate simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_vs_democratic_consent, conceptual, 'Whether Magisterial authority operates as coordination or extraction within the Catholic community itself.').

omega_variable(
    subsidiarity_as_participation_vs_decentralization,
    'Does the Magisterial reading''s subsidiarity principle demand *participatory governance* (workers and communities co-deciding AI deployment) or merely *decentralized governance* (decisions moved to lower levels but still undemocratic)?',
    'Analyze recent dicastery documents (Dicastery for Promoting Integral Human Development, Pontifical Academy for Life) and bishops'' conference statements on AI: do they name worker codetermination, community consent for data infrastructure, and algorithmic auditing by affected populations — or do they name ''local control'' without specifying democratic mechanisms?',
    'If subsidiarity = participation, the constraint''s coordination function is stronger and its extraction from tech monopolies is more justified (they lose decision-rights they illegitimately hold). If subsidiarity = decentralization, the constraint may enable local capture by elites — a snare dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_as_participation_vs_decentralization, conceptual, 'The operational meaning of subsidiarity in Magisterial AI governance.').

omega_variable(
    universal_destination_of_goods_enforceability,
    'Can the principle of universal destination of goods be translated into enforceable claims on AI-generated wealth (data dividends, automation taxes, compute commons) without becoming a tool for state expropriation that violates subsidiarity?',
    'Track legislative proposals inspired by Catholic Social Doctrine (e.g., EU Data Act, Chilean neuro-rights law, Brazilian AI bill) for mechanisms that implement universal destination without centralizing power. Track Catholic economist debates (e.g., Stefano Zamagni, Luigino Bruni) on the institutional forms of this principle.',
    'If enforceable mechanisms exist that respect subsidiarity, the constraint''s extraction from monopolies is coordinated redistribution — strengthening the rope aspect. If enforcement requires centralization that violates subsidiarity, the constraint entangles its own principles — a tangled_rope that may collapse into snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_destination_of_goods_enforceability, empirical, 'Whether the most radical Magisterial economic principle can be institutionalized without self-contradiction.').

omega_variable(
    catholic_institutional_complicity,
    'To what extent do Catholic institutions (universities, hospitals, investment funds, NGOs) themselves deploy extractive AI systems (predictive admissions, algorithmic patient triage, surveillance workplace tools) while advocating Magisterial governance for others?',
    'Audit Catholic institutional AI procurement and deployment against the Magisterial framework''s own criteria: worker participation, data sovereignty, transparency, preferential option for the poor. Compare to secular benchmarks.',
    'High complicity would indicate the constraint functions as a snare for Catholic institutions themselves — they extract legitimacy from the framework while violating it. Low complicity would strengthen the coordination claim. This is a theater_ratio driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_institutional_complicity, empirical, 'Whether the constraint''s agenda-setters comply with their own demands.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 1891, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t1891, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 1891, 0.1).
narrative_ontology:measurement(ai_g_tr_t1931, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 1931, 0.15).
narrative_ontology:measurement(ai_g_tr_t1961, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 1961, 0.2).
narrative_ontology:measurement(ai_g_tr_t1991, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 1991, 0.25).
narrative_ontology:measurement(ai_g_tr_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(ai_g_tr_t2020, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(ai_g_tr_t2025, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2025, 0.3).
narrative_ontology:measurement(ai_g_tr_t2030, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2030, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t1891, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 1891, 0.15).
narrative_ontology:measurement(ai_g_be_t1931, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 1931, 0.2).
narrative_ontology:measurement(ai_g_be_t1961, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 1961, 0.25).
narrative_ontology:measurement(ai_g_be_t1991, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement(ai_g_be_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(ai_g_be_t2020, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(ai_g_be_t2025, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement(ai_g_be_t2030, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2030, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t1891, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 1891, 0.2).
narrative_ontology:measurement(ai_g_su_t1931, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 1931, 0.25).
narrative_ontology:measurement(ai_g_su_t1961, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 1961, 0.3).
narrative_ontology:measurement(ai_g_su_t1991, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 1991, 0.35).
narrative_ontology:measurement(ai_g_su_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(ai_g_su_t2020, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement(ai_g_su_t2025, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2025, 0.4).
narrative_ontology:measurement(ai_g_su_t2030, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2030, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.08).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, labor_algorithmic_management).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, data_colonialism).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, lethal_autonomous_weapons_governance).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, surveillance_capitalism_regulation).

% DUAL FORMULATION NOTE:
% This constraint is the magisterial_subsidiarity_reading of the ai_governance_legitimacy kernel. It differs from the democratic_pluralist_reading in authority ground (Magisterium vs. public reason), from the market_libertarian_reading in its rejection of exit-as-dignity, and from the technocratic_optimization_reading in its subordination of efficiency to dignity. The ε values diverge: this reading authors high ε (0.5) because it demands structural redistribution; the market_libertarian_reading would author low ε for the same referent (seeing the Magisterial demands as illegitimate extraction); the technocratic_optimization_reading would author moderate ε (seeing ethical constraints as optimization costs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, powerful, 0.85).
constraint_indexing:directionality_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, powerless, 0.9).
constraint_indexing:directionality_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, moderate, 0.6).
constraint_indexing:directionality_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
