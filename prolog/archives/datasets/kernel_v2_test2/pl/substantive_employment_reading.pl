% ============================================================================
% CONSTRAINT STORY: substantive_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substantive_employment_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substantive_employment_reading
 *   human_readable: Substantive Employment Reading: Economic Dependence as Definitional
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The substantive employment reading asserts that employment status should
 *   be determined by economic reality — dependence on platform income and
 *   subordination to algorithmic control — rather than by contract form. This
 *   reading emerged as platform companies scaled gig work arrangements that
 *   formally classified workers as independent contractors while exercising
 *   employment-like control through algorithmic dispatch, performance
 *   monitoring, and unilateral deactivation. The constraint coordinates
 *   worker protection with economic structure (genuine coordination function)
 *   but extracts from platforms by foreclosing their
 *   labor-cost-externalization business model. Enforcement requires active
 *   legal and regulatory intervention against platform resistance. The
 *   reading is one of three competing interpretations of the employment
 *   boundary kernel in platform economies.
 *
 * KEY AGENTS:
 *   - Platform Workers (trapped/constrained): Primary beneficiaries — gain employment protections, social insurance, job security; experience varies by exit options (trapped workers see snare, constrained workers see tangled rope)
 *   - Social Insurance Systems (institutional/mobile): Secondary beneficiaries — expanded contribution base, reduced precarity externalities, better risk pooling
 *   - Platform Companies (institutional/constrained): Primary victims — must provide full employment benefits, cannot externalize labor costs, business model restructuring required; constrained exit because regulatory jurisdiction limits forum shopping
 *   - Venture Capital Investors (powerful/mobile): Secondary victims — platform unit economics worsen under reclassification, reducing returns; mobile exit allows capital reallocation
 *   - Labor Rights Coalition (organized/mobile): Organized agents — see substantive reading as transitional scaffold toward portable benefits and sectoral bargaining
 *   - Analytical Observer (analytical/analytical): Sees tangled rope — genuine coordination of protection with economic reality, genuine extraction from platforms resisting reclassification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substantive_employment_reading, 0.58).
domain_priors:suppression_score(substantive_employment_reading, 0.67).
domain_priors:theater_ratio(substantive_employment_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substantive_employment_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substantive_employment_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(substantive_employment_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substantive_employment_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(substantive_employment_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substantive_employment_reading, tangled_rope).
narrative_ontology:human_readable(substantive_employment_reading, "Substantive Employment Reading: Economic Dependence as Definitional").
narrative_ontology:topic_domain(substantive_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(substantive_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substantive_employment_reading, 'ae2b5453-ebec-4ce5-be24-876a26ac810a').
narrative_ontology:cs_kernel_codification('ae2b5453-ebec-4ce5-be24-876a26ac810a', distributed).
narrative_ontology:cs_authority_grounding('ae2b5453-ebec-4ce5-be24-876a26ac810a', distributed).
narrative_ontology:cs_reading_relation('ae2b5453-ebec-4ce5-be24-876a26ac810a', substantive_employment_reading__formalist_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('ae2b5453-ebec-4ce5-be24-876a26ac810a', substantive_employment_reading__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('ae2b5453-ebec-4ce5-be24-876a26ac810a', foundational, economic_reality_primacy).
narrative_ontology:cs_axiom_status(economic_reality_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ae2b5453-ebec-4ce5-be24-876a26ac810a', economic_reality_primacy, deontological).
narrative_ontology:cs_axiom('ae2b5453-ebec-4ce5-be24-876a26ac810a', foundational, algorithmic_control_as_subordination).
narrative_ontology:cs_axiom_status(algorithmic_control_as_subordination, holdable).
narrative_ontology:cs_axiom_grounding('ae2b5453-ebec-4ce5-be24-876a26ac810a', algorithmic_control_as_subordination, empirically_contingent).
narrative_ontology:cs_axiom('ae2b5453-ebec-4ce5-be24-876a26ac810a', secondary, contract_form_irrelevance).
narrative_ontology:cs_axiom_status(contract_form_irrelevance, holdable).
narrative_ontology:cs_axiom_grounding('ae2b5453-ebec-4ce5-be24-876a26ac810a', contract_form_irrelevance, conventional).
narrative_ontology:cs_reference_frame('ae2b5453-ebec-4ce5-be24-876a26ac810a', new_deal_employment_framework).
narrative_ontology:cs_drift_state('ae2b5453-ebec-4ce5-be24-876a26ac810a', platform_economy_emergence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ae2b5453-ebec-4ce5-be24-876a26ac810a', '').
narrative_ontology:cs_kernel_id(substantive_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substantive_employment_reading, platform_workers).
narrative_ontology:constraint_beneficiary(substantive_employment_reading, social_insurance_systems).
narrative_ontology:constraint_victim(substantive_employment_reading, platform_companies).
narrative_ontology:constraint_victim(substantive_employment_reading, venture_capital_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substantive_employment_reading, platform_workers_constrained).
narrative_ontology:constraint_victim(substantive_employment_reading, platform_workers_trapped).
narrative_ontology:constraint_vindicates(substantive_employment_reading, economic_realism_doctrine).
narrative_ontology:constraint_vindicates(substantive_employment_reading, substance_over_form_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Economically dependent on single platform for survival income; no alternative employment available in local labor market; subject to algorithmic dispatch, performance monitoring, and unilateral deactivation; formally classified as independent contractor but experiences employment-like subordination without protections.
narrative_ontology:constraint_stakeholder(substantive_employment_reading, platform_workers_trapped, payer,
    powerless, immediate, trapped, local).

% Works across multiple platforms to diversify income risk; values schedule flexibility but bears costs of no benefits, no job security, algorithmic opacity; can exit to traditional employment at significant cost (lower flexibility, potentially lower earnings); benefits from substantive reading's protections but loses some autonomy.
narrative_ontology:constraint_stakeholder(substantive_employment_reading, platform_workers_constrained, beneficiary,
    moderate, biographical, constrained, national).

% Public and private insurance systems gain expanded contribution base when platform workers are reclassified as employees; reduced precarity externalities (fewer workers relying on emergency safety nets); better risk pooling across larger covered population; can adjust benefit structures to accommodate platform work patterns.
narrative_ontology:constraint_stakeholder(substantive_employment_reading, social_insurance_systems, beneficiary,
    institutional, generational, mobile, national).

% Business models designed around labor cost externalization through contractor classification; substantive reading requires providing full employment benefits (payroll taxes, health insurance, paid leave, job security protections, collective bargaining rights); exit options constrained by regulatory jurisdiction (cannot easily relocate to avoid reclassification); some platforms adapt by restructuring, others exit marginal markets, others resist through litigation and lobbying.
narrative_ontology:constraint_stakeholder(substantive_employment_reading, platform_companies, payer,
    institutional, biographical, constrained, global).

% Funded platform companies on assumption of contractor classification and low labor costs; substantive reading worsens unit economics and reduces returns; can exit by reallocating capital to other sectors or jurisdictions; some continue funding platforms that adapt to reclassification, others divest.
narrative_ontology:constraint_stakeholder(substantive_employment_reading, venture_capital_investors, payer,
    powerful, biographical, mobile, global).

% Organized advocacy groups (unions, worker centers, policy organizations) that litigate for reclassification, lobby for substantive employment statutes, and organize platform workers; see substantive reading as transitional framework toward portable benefits and sectoral bargaining; can shift strategy if substantive reading proves unenforceable or if hybrid approaches gain traction.
narrative_ontology:constraint_stakeholder(substantive_employment_reading, labor_rights_coalition, agenda_setter,
    organized, generational, mobile, continental).

% Legal tradition holding that contract form controls absent fraud; excluded from substantive reading's framework by design (the substantive reading explicitly rejects formalist primacy); persists as alternative reading in jurisdictions that have not adopted substantive approach.
narrative_ontology:constraint_stakeholder(substantive_employment_reading, formalist_legal_tradition, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(substantive_employment_reading, formalist_legal_tradition).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extending employment protections (social insurance, job security, collective bargaining rights) to workers who are economically dependent on platform income and subject to algorithmic control, regardless of formal contract classification. Solves collective action problem: individual workers cannot negotiate protections against platform power; social insurance systems cannot cover gig workers under contractor classification.
% TRANSFER_FUNCTION: Mandatory reclassification transfers costs from workers and public safety nets to platform companies: payroll taxes, health insurance premiums, paid leave, unemployment insurance, workers compensation, job security protections. Also transfers bargaining power from platforms (unilateral control) to workers (collective bargaining rights).
% ABSENT_VOICES: Small business owners and independent professionals who use platforms as true independent contractors (not economically dependent) and fear being swept into employee classification, losing flexibility and autonomy. Also: consumers who may face higher prices if platforms pass reclassification costs through. These voices are present in policy debates but structurally disadvantaged because substantive reading focuses on economically dependent workers, not genuinely independent contractors.
% DISAPPEARANCE_RATIONALE: If substantive employment reading disappeared overnight, platform companies would revert to contractor classification, externalizing labor costs; platform workers would lose employment protections and social insurance coverage; social insurance systems would face coverage gaps and precarity externalities; labor coalitions would lose legal basis for organizing platform workers. The world rearranges because the reading actively restructures platform business models and worker protections — it is not a description of natural fact but an enforced legal framework.
% FOUNDING_PROBLEM: Platform economy emergence in 2010s created large workforce formally classified as independent contractors but economically dependent on platform income and subject to algorithmic control resembling employment subordination, resulting in precarity (no benefits, no job security, no collective bargaining) and social insurance coverage gaps. Substantive employment reading was built to extend 20th-century employment protections to 21st-century platform work arrangements by prioritizing economic reality over contract form.
% FOUNDING_PROBLEM_CORROBORATION: Platform worker precarity persists and has grown with platform economy expansion (documented by labor economists, worker advocacy groups, and government reports across multiple jurisdictions). Social insurance coverage gaps remain (workers lack unemployment insurance, health insurance, retirement benefits). Algorithmic control has intensified (more sophisticated dispatch algorithms, performance monitoring, dynamic pricing). The founding problem is corroborated by: academic labor economics research (Prassl, Aloisi, De Stefano), worker testimony in litigation and legislative hearings, government studies (UK Taylor Review, EU Platform Work Directive), and platform companies' own resistance to reclassification (indicating they benefit from the gap the substantive reading addresses).
narrative_ontology:disappearance_verdict(substantive_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(substantive_employment_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED PLATFORM WORKER (SNARE) — Economically dependent on platform income with no alternative employment; experiences algorithmic control as pure extraction. Contract says independent but economic reality is subordination. Maximum experienced extraction from immediate survival perspective.
constraint_indexing:constraint_classification(substantive_employment_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED PLATFORM WORKER (TANGLED ROPE) — Works across multiple platforms to diversify risk; benefits from flexibility coordination but bears costs of no benefits, no job security, algorithmic opacity. Mixed experience: genuine coordination value in schedule autonomy, genuine extraction in precarity and unilateral control.
constraint_indexing:constraint_classification(substantive_employment_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOCIAL INSURANCE SYSTEM (ROPE) — Substantive employment reading solves coordination problem of extending social protections to economically dependent workers regardless of contract form. Beneficiary of expanded contribution base and reduced precarity externalities. Experiences constraint as coordination mechanism.
constraint_indexing:constraint_classification(substantive_employment_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM COMPANY (SNARE) — Victim of reclassification: must provide full employment benefits, job security protections, collective bargaining rights. Business model depends on labor cost externalization; substantive reading forecloses that model. High extraction from company perspective despite institutional power because exit options are constrained by regulatory jurisdiction.
constraint_indexing:constraint_classification(substantive_employment_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR RIGHTS COALITION (SCAFFOLD) — Organized advocacy groups see substantive employment reading as transitional framework bridging 20th-century employment law to 21st-century work arrangements. Sunset logic: once portable benefits and sectoral bargaining mature, the binary employee/contractor distinction becomes obsolete. Coordination function with declared endpoint.
constraint_indexing:constraint_classification(substantive_employment_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Substantive employment reading coordinates worker protection with economic reality (genuine coordination function) but extracts from platforms through mandatory reclassification that some resist as overreach. Active enforcement required to maintain against formalist counter-reading. Mixed structure visible from analytical distance.
constraint_indexing:constraint_classification(substantive_employment_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substantive_employment_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substantive_employment_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substantive_employment_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substantive_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substantive_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platforms bear substantial costs of reclassification (payroll taxes, benefits, job security protections, collective bargaining obligations) that their business models were designed to avoid. Workers gain protections but some lose flexibility and access (platforms may reduce workforce or exit marginal markets). The extraction is real but not maximal — some platforms adapt, some workers prefer contractor status, and the coordination function (matching protection to economic reality) has genuine value. Suppression (0.67): Moderate-high and rising. Platforms resist reclassification through litigation, lobbying, and jurisdiction shopping. Workers face retaliation risk for organizing or asserting employment rights. Enforcement requires sustained regulatory and legal pressure. The suppression trajectory shows platforms building more sophisticated resistance as the reading gains traction. Theater ratio (0.48): Moderate and rising. Some jurisdictions adopt substantive language without effective enforcement; some platforms perform compliance (benefits-like programs, voice mechanisms) while maintaining contractor classification; some court rulings affirm substantive principles but carve out platform-specific exceptions. The theater is not yet dominant but is growing as the reading spreads without uniform enforcement. Accessibility collapse (0.42): Moderate. Once the substantive reading is understood, some alternative framings (pure contractor independence, mutual benefit of flexibility) lose credibility, but formalist and hybrid readings remain live alternatives. The collapse is partial, not total. Resistance (0.71): High. Platforms, business coalitions, and some workers actively resist the reading through legal challenges, ballot initiatives (California Prop 22), and lobbying for carve-outs. The resistance is organized and well-funded, indicating the constraint is contested rather than naturalized.
 *
 * PERSPECTIVAL GAP:
 *   The substantive employment reading produces a clear perspectival inversion: workers (especially trapped workers) see snare or tangled rope because they experience precarity and control despite being the intended beneficiaries; platforms see snare because they are victims of mandatory reclassification; social insurance systems see rope because they are beneficiaries of expanded coverage; labor coalitions see scaffold because they view the reading as transitional toward portable benefits. The analytical observer sees tangled rope — the constraint genuinely coordinates worker protection with economic reality (coordination function) and genuinely extracts from platforms resisting cost internalization (extraction function), requiring active enforcement to maintain. The gap reveals that beneficiary/victim status does not determine experienced type — structural position (power, exit, time horizon) mediates the experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform workers are primary beneficiaries — the substantive reading extends employment protections to them. Their directionality is low (toward beneficiary end), modulated by exit options: trapped workers (no alternatives) experience the constraint as snare despite being beneficiaries because they cannot exit precarity even with protections; constrained workers (some alternatives) experience tangled rope because they gain protections but lose some flexibility. Social insurance systems are institutional beneficiaries with mobile exit — they gain expanded coverage and contributions, experiencing the constraint as coordination (rope). Platform companies are institutional victims with constrained exit — they must provide costly benefits and cannot easily exit regulatory jurisdictions, experiencing high extraction (snare from their perspective). The analytical observer sees the mixed structure: genuine coordination function (matching legal status to economic reality) combined with genuine extraction (platforms forced to internalize costs they designed their models to avoid), requiring active enforcement, yielding tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The substantive employment reading resolves mandatrophy by explicitly naming both coordination and extraction functions. Coordination: extending legal protections to match economic dependence and algorithmic subordination solves a real collective action problem (workers cannot individually negotiate against platform power; social insurance systems cannot cover gig workers under contractor classification). Extraction: platforms designed business models to externalize labor costs; substantive reading forecloses that externalization, extracting from platforms. The constraint is tangled rope from the analytical perspective because both functions are structural — it is not pure coordination (platforms genuinely resist and bear costs) and not pure extraction (the worker protection function is real and addresses genuine precarity). The mandate (protect economically dependent workers) has not outlived its function — platform precarity persists and the reading addresses it — so mandatrophy is not resolved. The constraint is active and contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the employment boundary kernel best read through economic dependence (substantive), contract form (formalist), or hybrid portable benefits (security)? This constraint instantiates the substantive reading; sibling readings are formalist_employment_reading and hybrid_security_reading.',
    'Cross-jurisdictional comparison of worker outcomes under different readings; longitudinal tracking of precarity metrics, platform compliance costs, and social insurance coverage gaps under each regime.',
    'If substantive reading prevails: platforms bear full employment costs, worker precarity declines, platform business models restructure. If formalist reading prevails: contract form controls, platforms externalize costs, precarity persists. If hybrid reading prevails: portable benefits decouple protection from employment status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the employment boundary kernel best fits platform economy structure').

omega_variable(
    algorithmic_control_threshold,
    'What degree of algorithmic control (dispatch assignment, performance monitoring, pricing determination, deactivation authority) constitutes employment-defining subordination versus legitimate coordination?',
    'Empirical measurement of worker autonomy across platforms with varying control mechanisms; correlation between control intensity and worker outcomes (earnings volatility, precarity, exit rates).',
    'If threshold is low: most platform arrangements qualify as employment under substantive reading. If threshold is high: only the most controlling platforms qualify, leaving many workers unprotected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithmic_control_threshold, empirical, 'Algorithmic control threshold for employment classification').

omega_variable(
    economic_dependence_measurement,
    'How is economic dependence measured for classification purposes? Single-platform income share? Total gig income as percentage of household income? Duration of platform relationship? Availability of alternative work?',
    'Comparative analysis of dependence metrics across jurisdictions that have adopted substantive readings; identification of which metrics best predict worker vulnerability and precarity outcomes.',
    'Measurement choice determines classification scope: income-share metric captures fewer workers than relationship-duration metric; household-income metric varies with family structure. Different metrics produce different victim and beneficiary sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_dependence_measurement, empirical, 'Operationalization of economic dependence for employment classification').

omega_variable(
    enforcement_jurisdiction_gap,
    'Can substantive employment reading be enforced when platforms operate transnationally but labor law is jurisdictional? Do platforms route through permissive jurisdictions to avoid reclassification?',
    'Analysis of platform corporate structure and jurisdiction shopping; measurement of enforcement effectiveness in jurisdictions that have adopted substantive readings versus those that have not.',
    'If jurisdiction shopping is effective: substantive reading becomes unenforceable for global platforms, reducing to theater. If enforcement is effective: platforms must comply in each jurisdiction, raising global labor costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_jurisdiction_gap, empirical, 'Enforceability of substantive reading across jurisdictional boundaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substantive_employment_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subst_emp_theater_2010, substantive_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subst_emp_theater_2013, substantive_employment_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(subst_emp_theater_2016, substantive_employment_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(subst_emp_theater_2019, substantive_employment_reading, theater_ratio, 9, 0.44).
narrative_ontology:measurement(subst_emp_theater_2022, substantive_employment_reading, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(subst_emp_extract_2010, substantive_employment_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subst_emp_extract_2013, substantive_employment_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(subst_emp_extract_2016, substantive_employment_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(subst_emp_extract_2019, substantive_employment_reading, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(subst_emp_extract_2022, substantive_employment_reading, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(subst_emp_suppress_2010, substantive_employment_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(subst_emp_suppress_2016, substantive_employment_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(subst_emp_suppress_2022, substantive_employment_reading, suppression_requirement, 12, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substantive_employment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substantive_employment_reading, formalist_employment_reading).
narrative_ontology:affects_constraint(substantive_employment_reading, hybrid_security_reading).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel decomposes into three structurally distinct readings with different ε values and different beneficiary/victim sets. The substantive reading (this constraint) has moderate-high ε (0.58) because platforms resist reclassification. The formalist reading has lower ε because it preserves platform business models. The hybrid reading has variable ε depending on implementation (portable benefits funded by platforms vs. public funding). These are not the same constraint viewed from different angles — they are competing legal and policy frameworks with different structural consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substantive_employment_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
