% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: AI Governance Legitimacy via Magisterial Subsidiarity Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the magisterial_subsidiarity_reading of the
 *   ai_governance_legitimacy kernel. It asserts that legitimate AI governance
 *   must conform to Catholic Social Doctrine principles (common good,
 *   subsidiarity, solidarity, universal destination of goods) as
 *   authoritatively interpreted by the Magisterium. The reading positions
 *   technology as subordinate to these principles, demanding transparent
 *   accountability, participatory governance, and protection of the
 *   vulnerable. It explicitly rejects pure technocratic optimization, market
 *   libertarian logics, and democratic pluralism as sufficient grounds for
 *   legitimacy, insisting that human dignity is non-negotiable and prior to
 *   efficiency. The constraint operates through moral suasion, civil society
 *   mobilization, international law advocacy (UN, ILO, EU), and ecclesial
 *   witness — not state coercion. Its extraction (ε=0.50) reflects the cost
 *   imposed on actors who violate these principles (tech monopolies,
 *   military-industrial complex, extractive finance) through reputational,
 *   regulatory, and market pressure organized by Catholic networks. The
 *   constraint is a tangled_rope: it genuinely coordinates dignity/efficiency
 *   and subsidiarity/solidarity tensions while asymmetrically extracting
 *   compliance from powerful actors who benefit from the status quo.
 *
 * KEY AGENTS:
 *   - magisterium: Primary agenda_setter (institutional/universal/analytical) — defines the authoritative interpretation
 *   - catholic_social_teachers: Secondary agenda_setter/beneficiary (organized/global/analytical) — transmit and apply the teaching
 *   - workers: Primary beneficiary (organized/global/constrained) — gain protection from algorithmic displacement and surveillance
 *   - global_south_populations: Primary beneficiary (powerless/global/trapped) — gain advocacy against data colonialism and extractive AI supply chains
 *   - families: Beneficiary (moderate/global/constrained) — gain protection from AI-mediated erosion of care and formation
 *   - marginalized_populations: Beneficiary (powerless/global/trapped) — gain defense against algorithmic discrimination and exclusion
 *   - private_tech_monopolies: Primary payer (powerful/global/mobile) — bear compliance costs and regulatory pressure
 *   - military_industrial_complex: Payer (institutional/global/constrained) — face moral limits on autonomous weapons and surveillance AI
 *   - extractive_finance: Payer (powerful/global/mobile) — face pressure on AI-driven speculative extraction and labor displacement
 *   - secular_policymakers: Excluded (institutional/national/analytical) — contest magisterial monopoly in pluralist forums
 *   - ai_ethics_researchers: Observer (organized/global/analytical) — engage the framework critically from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.35).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy via Magisterial Subsidiarity Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, '8e28a1ad-3b05-4ff8-a2b0-a307887e99cb').
narrative_ontology:cs_kernel_codification('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', formalized).
narrative_ontology:cs_authority_grounding('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', lineage).
narrative_ontology:cs_interpretation_layer_present('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb').
narrative_ontology:cs_reading_relation('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_reading_relation('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', foundational, human_dignity_as_nonnegotiable_constraint).
narrative_ontology:cs_axiom_status(human_dignity_as_nonnegotiable_constraint, holdable).
narrative_ontology:cs_axiom_grounding('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', human_dignity_as_nonnegotiable_constraint, deontological).
narrative_ontology:cs_axiom('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', foundational, technology_subordinated_to_common_good).
narrative_ontology:cs_axiom_status(technology_subordinated_to_common_good, holdable).
narrative_ontology:cs_axiom_grounding('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', technology_subordinated_to_common_good, deontological).
narrative_ontology:cs_reference_frame('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', magisterial_social_teaching_framework).
narrative_ontology:cs_drift_state('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', post_ai_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8e28a1ad-3b05-4ff8-a2b0-a307887e99cb', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_teachers).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, solidarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching office of the Catholic Church (Pope, CDF, pontifical academies, episcopal conferences) that authoritatively interprets Catholic Social Doctrine for AI governance. It sets the agenda through encyclicals, addresses to UN/ITU, and episcopal conference statements. It bears no compliance costs but invests institutional credibility. Exit is analytical — it does not exit its own teaching authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, generational, analytical, universal).

% Theologians, ethicists, and social doctrine centers (e.g., Pontifical Academy for Life, Catholic universities) who develop, transmit, and apply the magisterial framework to AI. They gain professional recognition and institutional resources but face pressure to conform to magisterial lines. Exit is analytical — they can dissent intellectually but identity_locked vocationally.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_teachers, agenda_setter,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, catholic_social_teachers, beneficiary).

% Laborers, gig workers, and union members who gain protection from algorithmic management, displacement, and surveillance through Catholic labor advocacy (ILO, Vatican labor office, Catholic unions). They benefit from dignity-based regulation but lack direct power over AI deployment. Exit is constrained — they cannot easily leave labor markets shaped by AI.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers, beneficiary,
    organized, biographical, constrained, global).

% Communities in the Global South facing data colonialism, extractive AI supply chains (mining, annotation labor), and algorithmic governance imposed by Northern tech firms. The magisterial framework provides their primary institutional advocate in global forums (UN, ITU, UNESCO). Exit is trapped — they cannot escape the structural position of extraction.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Households and kinship networks protected by the framework's emphasis on care, formation, and non-commodification of human relations against AI-mediated erosion (algorithmic caregiving, predictive parenting, surveillance). They gain moral language for resistance but limited structural power. Exit is constrained — family life is permeated by AI systems.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families, beneficiary,
    moderate, biographical, constrained, global).

% Racial, ethnic, religious, and sexual minorities, disabled persons, migrants, and poor communities disproportionately harmed by algorithmic discrimination, predictive policing, and automated exclusion. The framework's universal destination of goods and preferential option for the poor names their protection explicitly. Exit is trapped — marginalization is structural.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations, beneficiary,
    powerless, biographical, trapped, global).

% Large platform and AI companies (Alphabet, Microsoft, Meta, Amazon, OpenAI, etc.) that face regulatory pressure, reputational campaigns, and moral legitimacy costs from Catholic advocacy (e.g., Vatican AI ethics Rome Call, episcopal conference statements on labor/surveillance). They have mobile exit (lobbying, jurisdiction shopping, narrative capture) but bear real compliance costs. Extraction falls on them asymmetrically.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    powerful, biographical, mobile, global).

% Defense contractors, state militaries, and autonomous weapons programs constrained by magisterial teaching on lethal autonomous weapons (LAWs), AI-enabled surveillance, and just war doctrine. The Vatican's diplomatic pressure at UN CCW and national episcopal advocacy create regulatory and reputational friction. Exit is constrained — state security imperatives limit mobility.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Hedge funds, private equity, and algorithmic trading firms using AI for speculative extraction, labor displacement, and financialization of care/housing. Catholic social finance networks (impact investing, Vatican banking oversight, Catholic development funds) apply counter-pressure. Exit is mobile — capital flees regulatory frameworks.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance, payer,
    powerful, biographical, mobile, global).

% Legislators, regulators, and diplomats in pluralist democracies who must navigate magisterial claims alongside other legitimacy frameworks. They are structurally excluded from the magisterial interpretive monopoly but cannot ignore its influence in Catholic-majority countries and Global South forums. Exit is analytical — they engage from outside the framework.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_policymakers, excluded,
    institutional, biographical, analytical, national).

% Academic and industry researchers studying AI ethics, fairness, accountability, transparency who engage the magisterial framework as one tradition among many (IEEE, ACM, EU HLEG, UNESCO). They have analytical exit and no stake in the constraint's enforcement. Their work informs but does not determine the reading's authority.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_ethics_researchers, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, diffuse).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Subordinates technological development to human dignity, common good, and solidarity through a unified moral framework that resolves coordination problems between efficiency and justice, individual innovation and collective welfare, local autonomy (subsidiarity) and global responsibility (solidarity).
% TRANSFER_FUNCTION: Moves decision-making authority over AI development and deployment from unaccountable technical and market actors to participatory governance structures accountable to magisterial principles; transfers material benefits from extractive arrangements to workers, Global South populations, families, and marginalized populations.
% ABSENT_VOICES: Non-Catholic religious traditions (Islamic, Buddhist, Indigenous cosmologies), secular humanist frameworks, feminist and queer theorists critical of magisterial anthropology, and AI systems themselves (as potential moral patients) are structurally excluded from the magisterial interpretive monopoly.
% DISAPPEARANCE_RATIONALE: If the magisterial subsidiarity constraint vanished, AI governance would default to technocratic optimization or market libertarian logics, removing the only unified framework that explicitly subordinates technology to human dignity as non-negotiable; the Global South and marginalized would lose their primary institutional advocate in global governance forums; Catholic institutions would lose their distinctive public witness.
% FOUNDING_PROBLEM: The encyclical tradition (Rerum Novarum through Laudato Si' and Fratelli Tutti) identifies the founding problem as the subordination of the human person to technical, economic, and political systems that treat dignity as instrumental rather than intrinsic.
% FOUNDING_PROBLEM_CORROBORATION: The International Labour Organization, UN Human Rights bodies, and Global South episcopal conferences corroborate that the subordination of persons to systems remains the live problem; the Magisterium's own beneficiaries (workers' movements, Catholic social ministries, Sant'Egidio, Caritas) attest the problem persists in AI-driven labor displacement, algorithmic governance, and data colonialism.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness at 0.50 reflects substantial but not total extraction: the constraint imposes real compliance costs on powerful actors (tech monopolies, military-industrial complex, extractive finance) through coordinated moral, legal, and market pressure, but these actors retain significant exit options (mobile/constrained) and counter-lobbying capacity. Suppression at 0.35 captures the non-coercive but structurally potent enforcement: exclusion from Catholic institutional networks, canonical discipline for Catholic actors, reputational costs in Global South forums where Church moral authority is high. Theater ratio at 0.25 indicates genuine commitment with some performative alignment: many Catholic institutions endorse the framework while their AI procurement and investment practices lag. Accessibility collapse at 0.60 reflects that alternatives (technocratic, market, pluralist) remain intellectually and institutionally viable but are morally constrained for Catholic actors. Resistance at 0.55 captures organized pushback from tech lobbyists, military contractors, and secular governance advocates who frame the reading as sectarian overreach.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (tech monopolies, military-industrial complex, extractive finance) experience the constraint as enforced extraction limiting their autonomy — high effective χ. The beneficiary seats (workers, Global South, families, marginalized) experience it as protective coordination — low or negative χ. The agenda_setter seats (magisterium, Catholic social teachers) experience it as vocation — χ near zero or subsidy. The excluded seat (secular policymakers) experiences it as illegitimate monopoly — contested χ. The observer seat (AI ethics researchers) experiences it as one framework among many — analytical χ. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as workers, global_south_populations, families, marginalized_populations — these groups gain protective norms, advocacy channels, and institutional voice without bearing enforcement costs. Victims are declared as private_tech_monopolies, military_industrial_complex, extractive_finance — these actors bear compliance costs, regulatory risk, and reputational pressure. The magisterium and catholic_social_teachers are agenda_setters who administer the constraint; their directionality is near-beneficiary (d ≈ 0.15) because the constraint legitimates their authority. Secular policymakers are excluded — their exit is analytical but they are structurally locked out of the interpretive monopoly. The directionality derivation from beneficiary/victim + power + exit produces the right d-values for all seats; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (subordination of human person to technical/economic systems) remains live — AI has intensified rather than solved it. The constraint is not a piton: it has active enforcement, growing relevance, and concentrated beneficiaries who would defend it. It is not a snare: the coordination function (unified dignity framework resolving efficiency/justice tensions) is genuine and acknowledged by non-Catholic actors (ILO, UN Human Rights). It is a tangled_rope because the same structure that coordinates dignity also extracts from powerful actors who would prefer technocratic or market logics. The mandate has not atrophied; it has expanded in scope (from labor to AI) while its enforcement mechanisms have adapted (from labor unions to global governance advocacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterial_interpretive_monopoly,
    'Does the Magisterium''s claim to authoritative interpretation of Catholic Social Doctrine for AI governance constitute a genuine coordination function or an extractive monopoly over moral legitimacy?',
    'Compare outcomes in jurisdictions where episcopal conferences exercise teaching authority versus those where lay Catholic movements contest magisterial framing; track whether subordinate bodies (pontifical academies, CDF) function as interpretive buffers or enforcement arms.',
    'If monopoly, the constraint extracts legitimacy-rents from Catholic actors who dissent; if coordination, the interpretive unity prevents fragmentation that would weaken the dignity claim in global forums.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_interpretive_monopoly, conceptual, 'Whether magisterial interpretive authority coordinates or extracts.').

omega_variable(
    extraction_direction_ambiguity,
    'Is the measured extraction (ε=0.50) borne by the named victims (tech monopolies, military-industrial complex, extractive finance) through compliance costs, or by Catholic institutions and faithful through the cost of maintaining the witness?',
    'Trace resource flows: compliance costs imposed on non-compliant actors vs. institutional costs of advocacy, education, and ecclesial discipline borne by the Church.',
    'If victims bear costs, χ is high for payer seats (snare/tangled_rope signature); if Church bears costs, the constraint may be a scaffold (transitional support) with extraction inverted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_direction_ambiguity, empirical, 'Direction of extraction between named victims and magisterial institutions.').

omega_variable(
    moral_suasion_coercion_boundary,
    'At what point does ''moral suasion, civil society pressure, international law advocacy, ecclesial witness'' become structural suppression rather than persuasive coordination?',
    'Measure exit options for Catholic technologists, policymakers, and institutions who dissent: canonical penalties, funding withdrawal, exclusion from magisterial platforms.',
    'If exit is identity_locked or trapped for dissenting Catholics, suppression is higher than the 0.35 scalar suggests; if exit is mobile, the constraint is closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_suasion_coercion_boundary, empirical, 'Whether enforcement mechanisms constitute coercion for identity-bound agents.').

omega_variable(
    kernel_reading_contestation,
    'Is the magisterial_subsidiarity_reading a stable reading of the ai_governance_legitimacy kernel, or does the kernel''s under-specification (distributed codification) make every reading a de facto fork?',
    'Track whether the four declared readings converge on any operational governance mechanism (e.g., EU AI Act, UN Global Digital Compact) or remain irreducibly divergent in institutional recommendations.',
    'If irreducible divergence, the kernel has no effective codification (distributed/implicit) and each reading is a separate constraint family; if convergence, the kernel functions as a genuine coordination anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the kernel stabilizes readings or fragments into forks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_g_tr_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(ai_g_tr_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(ai_g_tr_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(ai_g_tr_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(ai_g_tr_t30, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_g_be_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ai_g_be_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(ai_g_be_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(ai_g_be_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 25, 0.49).
narrative_ontology:measurement(ai_g_be_t30, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 30, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_g_su_t5, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 5, 0.27).
narrative_ontology:measurement(ai_g_su_t10, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(ai_g_su_t15, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 15, 0.31).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(ai_g_su_t25, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 25, 0.34).
narrative_ontology:measurement(ai_g_su_t30, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.08).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, eu_ai_act_governance).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, un_global_digital_compact).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ilo_algorithmic_management_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the ai_governance_legitimacy kernel. The magisterial_subsidiarity_reading claims authoritative interpretation of Catholic Social Doctrine; the democratic_pluralist_reading claims public reason; the market_libertarian_reading claims voluntary exchange; the technocratic_optimization_reading claims aggregate welfare. They form a constraint family linked by shared referent (AI governance legitimacy) and structural rivalry. The magisterial reading forecloses the market_libertarian reading within a Catholic framework, coexists with the democratic_pluralist reading in pluralist polities, and influences the technocratic_optimization reading by forcing dignity parameters into optimization functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
