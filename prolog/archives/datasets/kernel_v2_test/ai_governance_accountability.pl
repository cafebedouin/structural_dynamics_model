% ============================================================================
% CONSTRAINT STORY: ai_governance_accountability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_accountability, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_governance_accountability
 *   human_readable: AI Governance Accountability Gap
 *   domain: technology_ethics/political_theology/regulatory_policy
 *
 * SUMMARY:
 *   The AI governance accountability gap describes the structural lag between
 *   widespread deployment of algorithmic decision systems in high-stakes
 *   domains (employment screening, credit allocation, criminal justice,
 *   healthcare triage, social services) and the construction of effective
 *   accountability mechanisms (explainability requirements, independent
 *   audits, liability frameworks, appeal pathways). This constraint exhibits
 *   scaffold dynamics: it is a transitional coordination failure with
 *   identifiable sunset mechanisms under construction. The EU AI Act
 *   (2024-2027 phased implementation), US state-level algorithmic
 *   accountability legislation, IEEE technical standards for algorithmic
 *   transparency, and emerging case law on algorithmic discrimination
 *   represent concrete institutional responses building the accountability
 *   infrastructure. However, the gap's theater_ratio (0.58 and rising)
 *   reflects that much current 'AI ethics' activity is performative:
 *   voluntary principles without enforcement, explainability techniques that
 *   provide legibility without recourse, audit frameworks that lack technical
 *   capacity. The constraint sits downstream of two structural forces: the
 *   technocratic paradigm (which treats efficiency optimization as
 *   self-justifying) and private power concentration (which resists
 *   transparency and liability as threats to competitive advantage). Catholic
 *   Social Teaching enters as an institutional voice coordinating toward
 *   human dignity-centered governance, constrained by limited enforcement
 *   power outside Catholic institutions but contributing normative framework
 *   (subsidiarity, common good, solidarity) that aligns with secular
 *   accountability movements.
 *
 * KEY AGENTS:
 *   - Algorithmic Decision Subjects: Primary victims (powerless/trapped) — bear costs of opaque decisions without recourse; cannot exit systems that have become infrastructure
 *   - AI Deploying Corporations: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains and competitive advantage during accountability gap; can forum-shop and claim trade secrecy
 *   - Regulatory Agencies: Mixed actors (moderate/constrained) — building accountability capacity but vulnerable to technical complexity and industry capture
 *   - Accountability Coalition: Organized transition agents (organized/constrained) — civil society, labor, digital rights groups building binding frameworks with clear sunset timeline
 *   - Catholic Social Teaching Framework: Institutional normative voice (institutional/constrained) — coordinating toward dignity-centered governance without secular enforcement power
 *   - Workers and Communities: Secondary victims (powerless/trapped) — face displacement and discriminatory systems without consultation rights or appeal mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_accountability, 0.35).
domain_priors:suppression_score(ai_governance_accountability, 0.5).
domain_priors:theater_ratio(ai_governance_accountability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_accountability, extractiveness, 0.35).
narrative_ontology:constraint_metric(ai_governance_accountability, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(ai_governance_accountability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_accountability, scaffold).
narrative_ontology:human_readable(ai_governance_accountability, "AI Governance Accountability Gap").
narrative_ontology:topic_domain(ai_governance_accountability, "technology_ethics/political_theology/regulatory_policy").

domain_priors:requires_active_enforcement(ai_governance_accountability).
narrative_ontology:has_sunset_clause(ai_governance_accountability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_accountability, '424e4d52-a373-4ab5-a478-1ae479ca6351').
narrative_ontology:cs_kernel_codification('424e4d52-a373-4ab5-a478-1ae479ca6351', formalized).
narrative_ontology:cs_authority_grounding('424e4d52-a373-4ab5-a478-1ae479ca6351', lineage).
narrative_ontology:cs_interpretation_layer_present('424e4d52-a373-4ab5-a478-1ae479ca6351').
narrative_ontology:cs_created_at('424e4d52-a373-4ab5-a478-1ae479ca6351', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_accountability, ai_deploying_corporations).
narrative_ontology:constraint_beneficiary(ai_governance_accountability, regulatory_agencies_building_capacity).
narrative_ontology:constraint_victim(ai_governance_accountability, algorithmic_decision_subjects).
narrative_ontology:constraint_victim(ai_governance_accountability, workers_displaced_without_recourse).
narrative_ontology:constraint_victim(ai_governance_accountability, communities_facing_discriminatory_systems).
narrative_ontology:constraint_vindicates(ai_governance_accountability, technological_neutrality_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_accountability, innovation_imperative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMIC DECISION SUBJECT (SNARE) — Trapped by opacity: cannot understand why denied employment/credit/services, cannot appeal effectively, cannot exit systems that have become infrastructure. Maximum extraction — bears full cost of errors and bias with no recourse mechanism.
constraint_indexing:constraint_classification(ai_governance_accountability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATORY AGENCY (TANGLED ROPE) — Constrained by technical capacity gaps and industry capture risk, but genuinely coordinating toward accountability frameworks (EU AI Act, algorithmic impact assessments). Mixed: building necessary governance infrastructure while vulnerable to regulatory arbitrage and lobbying pressure.
constraint_indexing:constraint_classification(ai_governance_accountability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AI DEPLOYING CORPORATION (ROPE) — Benefits from accountability gap: can deploy systems without liability exposure, can forum-shop across jurisdictions, can claim trade secrecy to resist transparency. Experiences current state as coordination problem being solved too slowly — prefers voluntary frameworks to binding regulation.
constraint_indexing:constraint_classification(ai_governance_accountability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACCOUNTABILITY COALITION (SCAFFOLD) — Organized civil society, labor unions, digital rights groups, and aligned regulators building transitional governance: explainability requirements, algorithmic audits, liability frameworks, worker consultation rights. Sees current gap as temporary coordination failure with clear sunset: binding accountability mechanisms are under construction (EU AI Act 2024-2027 implementation, US state-level algorithmic accountability bills, international AI governance frameworks). Moderate extraction because coalition has agency and sees viable path to resolution.
constraint_indexing:constraint_classification(ai_governance_accountability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CATHOLIC SOCIAL TEACHING FRAMEWORK (TANGLED ROPE) — Institutional voice (Papal Magisterium via encyclicals, Vatican AI ethics initiatives) coordinating toward human dignity-centered governance while constrained by limited enforcement power outside Catholic institutions. Genuine coordination function (articulating subsidiarity, common good, solidarity principles for AI) coexists with structural extraction (teaching authority cannot compel secular compliance; prophetic witness competes with technocratic paradigm's material power).
constraint_indexing:constraint_classification(ai_governance_accountability, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From civilizational scope, the accountability gap is a transitional governance failure characteristic of technology deployment outpacing institutional adaptation. Historical pattern: steam power, electricity, automobiles, internet all showed similar lag between deployment and liability/safety frameworks. Current gap has clear sunset mechanisms under construction: binding regulation (EU AI Act), technical standards (IEEE P7000 series), legal precedent (algorithmic discrimination cases), and normative consensus (AI ethics principles converging across secular and religious frameworks). Scaffold classification reflects that the gap is temporary and the transition is structurally underway, not that extraction is low.
constraint_indexing:constraint_classification(ai_governance_accountability, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_accountability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_governance_accountability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_governance_accountability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(ai_governance_accountability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate and rising. Corporations capture efficiency gains and avoid liability costs during the accountability gap. Workers face displacement without recourse; decision subjects bear costs of errors and bias. But extraction is not maximal — some jurisdictions have functioning appeal mechanisms, some sectors face reputational costs for algorithmic harms, and accountability infrastructure is under construction. The rising trajectory (0.25→0.38 over 9 years) reflects that deployment is outpacing governance. Suppression (0.50): Moderate and rising. Significant barriers to accountability include: technical opacity (black-box models), trade secrecy claims, jurisdictional arbitrage, asymmetric technical capacity between deployers and regulators, and lack of legal precedent. But suppression is not total — some transparency requirements exist, some audits occur, some cases succeed. The rising trajectory (0.40→0.52) reflects hardening of opacity as systems become more complex and proprietary. Theater ratio (0.58): Substantial and rising. Much AI ethics activity is performative: voluntary principles without enforcement teeth, explainability techniques that provide post-hoc rationalization rather than genuine recourse, ethics boards without veto power, audit frameworks that lack technical capacity to verify claims. The rising trajectory (0.35→0.62) reflects Goodhart dynamics: as accountability pressure increases, performative compliance substitutes for substantive change. However, binding regulation (EU AI Act) represents a potential inflection point.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how scaffold classification depends on observer position and time horizon. Algorithmic decision subjects at biographical scope see pure extraction (snare) — they are trapped in opaque systems with no recourse and no exit. AI deploying corporations at immediate scope see coordination (rope) — they are solving the legitimate problem of scaling decision-making, and accountability requirements are friction. Regulatory agencies at biographical scope see mixed coordination and extraction (tangled rope) — they are building necessary governance while vulnerable to capture. The accountability coalition at generational scope sees a transitional problem with clear sunset (scaffold) — binding frameworks are under construction with concrete timelines. Catholic Social Teaching at biographical scope sees tangled rope — genuine normative coordination constrained by limited enforcement power. The analytical observer at civilizational scope sees scaffold — historical pattern of technology outpacing governance, with accountability infrastructure following predictable institutional adaptation timeline. The gap reveals that 'temporary' vs 'permanent' is observer-relative: those trapped in the gap experience it as indefinite extraction; those building the exit see the sunset approaching.
 *
 * DIRECTIONALITY LOGIC:
 *   Algorithmic decision subjects are full victims with trapped exit — they experience maximum effective extraction because they cannot exit systems that have become infrastructure and have no recourse when harmed. AI deploying corporations are primary beneficiaries with arbitrage exit — they experience low or negative effective extraction because they capture gains and can avoid accountability through forum-shopping and trade secrecy. Regulatory agencies are mixed: they are building necessary coordination infrastructure (beneficiary aspect) but face capture risk and capacity constraints (victim aspect) — constrained exit yields moderate effective extraction. The accountability coalition has organized power and sees a viable exit path (the sunset mechanisms under construction) — constrained exit with agency yields moderate extraction. Catholic Social Teaching framework is an institutional voice with constrained exit (cannot compel secular compliance) — experiences moderate extraction as prophetic witness competing with technocratic paradigm's material power. The perspectival gap is diagnostic: beneficiaries see coordination (rope), victims see extraction (snare), transition agents see temporary failure with sunset (scaffold).
 *
 * MANDATROPHY ANALYSIS:
 *   SCAFFOLD SUNSET LOGIC: The accountability gap carries a declared sunset through multiple converging mechanisms: (1) EU AI Act phased implementation 2024-2027 creates binding explainability, audit, and liability requirements for high-risk AI systems in EU market; (2) US state-level legislation (Illinois Biometric Information Privacy Act, California algorithmic accountability bills) building patchwork that creates compliance pressure; (3) IEEE P7000 series technical standards for algorithmic transparency maturing toward industry adoption; (4) case law on algorithmic discrimination accumulating (employment screening, credit allocation, criminal justice) establishing liability precedent; (5) insurance market development for AI liability creating economic pressure for accountability. The sunset is not guaranteed — omega variables identify failure modes (regulatory capacity gaps, liability assignment incoherence, explainability theater) — but the institutional infrastructure is under active construction with concrete timelines. The scaffold classification reflects that the gap is a coordination failure being addressed, not a permanent extraction mechanism. However, the rising theater_ratio (0.35→0.62) indicates Goodhart risk: performative compliance may substitute for substantive accountability, degrading scaffold toward piton. The critical test: do binding regulations with technical enforcement capacity arrive before the gap normalizes into permanent opacity?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_threshold,
    'Do regulatory agencies have sufficient technical capacity to audit complex AI systems, or will enforcement remain theatrical?',
    'Track regulatory hiring of AI specialists, audit completion rates, enforcement actions with technical findings vs procedural violations. Compare to historical regulatory capacity-building timelines (FDA drug approval, EPA emissions testing).',
    'If capacity remains low: scaffold degrades to piton (performative compliance theater). If capacity builds: scaffold sunset proceeds as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_threshold, empirical, 'Whether regulatory agencies can build technical audit capacity').

omega_variable(
    liability_assignment_coherence,
    'Can legal systems coherently assign liability for AI harms when decision-making is distributed across training data providers, model developers, deploying organizations, and users?',
    'Track evolution of case law on algorithmic harms; emergence of strict liability vs negligence standards; insurance market development for AI liability.',
    'If liability remains diffuse: accountability gap persists indefinitely (scaffold fails to sunset). If coherent assignment emerges: recourse pathways become real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liability_assignment_coherence, conceptual, 'Whether distributed AI decision-making admits coherent liability assignment').

omega_variable(
    explainability_sufficiency,
    'Do current explainability techniques (LIME, SHAP, attention visualization) provide meaningful recourse, or merely legibility theater?',
    'Empirical studies of whether explanations enable successful appeals; comparison of appeal success rates with vs without explanations; user comprehension testing.',
    'If explanations are theatrical: accountability frameworks built on explainability requirements will not reduce extraction. If explanations enable genuine recourse: scaffold proceeds toward sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explainability_sufficiency, empirical, 'Whether technical explainability provides meaningful accountability').

omega_variable(
    subsidiarity_enforcement_gap,
    'Can Catholic Social Teaching''s subsidiarity principle (decisions at lowest competent level) be enforced in AI governance, or does it remain aspirational against technocratic centralization?',
    'Track whether AI governance frameworks include worker consultation rights, community input mechanisms, local override capacity. Compare to historical subsidiarity enforcement in labor law, environmental justice.',
    'If subsidiarity remains aspirational: CST perspective is prophetic witness without structural power (tangled rope persists). If enforced: CST coordination function strengthens (moves toward rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_enforcement_gap, preference, 'Whether subsidiarity principle can be structurally enforced in AI systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_accountability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_gov_acct_theater_2015, ai_governance_accountability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ai_gov_acct_theater_2018, ai_governance_accountability, theater_ratio, 3, 0.48).
narrative_ontology:measurement(ai_gov_acct_theater_2021, ai_governance_accountability, theater_ratio, 6, 0.58).
narrative_ontology:measurement(ai_gov_acct_theater_2024, ai_governance_accountability, theater_ratio, 9, 0.62).

% Extraction over time
narrative_ontology:measurement(ai_gov_acct_extract_2015, ai_governance_accountability, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_gov_acct_extract_2018, ai_governance_accountability, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(ai_gov_acct_extract_2021, ai_governance_accountability, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(ai_gov_acct_extract_2024, ai_governance_accountability, base_extractiveness, 9, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ai_gov_acct_suppress_2015, ai_governance_accountability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_gov_acct_suppress_2018, ai_governance_accountability, suppression_requirement, 3, 0.45).
narrative_ontology:measurement(ai_gov_acct_suppress_2021, ai_governance_accountability, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(ai_gov_acct_suppress_2024, ai_governance_accountability, suppression_requirement, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_accountability, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of technocratic_paradigm_vs_human_dignity (which treats efficiency optimization as self-justifying, resisting accountability as friction) and private_power_vs_subsidiarity_common_good (which concentrates AI development in corporations that resist transparency and liability). The accountability gap is a distinct structural constraint with its own extractiveness reflecting the governance lag, not merely an instance of the upstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_accountability, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
