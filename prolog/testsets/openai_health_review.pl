% ============================================================================
% CONSTRAINT STORY: openai_health_review
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openai_health_review, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: openai_health_review
 *   human_readable: OpenAI ChatGPT Health Review Service
 *   domain: technological/economic
 *
 * SUMMARY:
 *   OpenAI's ChatGPT health review service creates a structural constraint
 *   where users delegate health decision-making to an LLM that mimics
 *   clinical reasoning without bearing medical liability. The service
 *   generates revenue for OpenAI by capturing users' willingness to pay for
 *   health guidance while transferring all adverse outcome risk to the user.
 *   This exhibits the hallmarks of a snare: high base extractiveness (revenue
 *   capture without reciprocal liability), high suppression (users cannot
 *   easily verify recommendations or contest outputs), and high theater (the
 *   interface and fluency create appearance of medical competence). The
 *   constraint is intensifying over time as user adoption grows, clinical
 *   validation remains absent, and regulatory frameworks lag behind technical
 *   deployment. The theater ratio has increased from 0.55 to 0.68 as users
 *   become accustomed to anthropomorphizing the interface, reducing their
 *   epistemic skepticism. Extractiveness has increased from 0.42 to 0.58 as
 *   the revenue model expands through subscription tiers and the user base
 *   grows larger relative to physician-mediated alternatives.
 *
 * KEY AGENTS:
 *   - End Users / Health Consumers: Primary victims (powerless/trapped) — bear full medical liability, cannot verify LLM accuracy, no appeal mechanism
 *   - OpenAI: Primary beneficiary (institutional/arbitrage) — captures subscription revenue and engagement metrics without proportional liability; can pivot or exit the health feature with minimal sunk cost
 *   - Licensed Physicians: Secondary beneficiary and victim (moderate/constrained) — see patient deflection and authority erosion but also see potential triage utility; constrained by regulatory liability that does not extend to the AI system
 *   - Medical Boards and FDA: Regulatory agents (organized/constrained) — face governance gap; developing frameworks for AI clinical decision support but sunset is 3-5 years minimum
 *   - Medical Epistemology System: Institutional victim (institutional/arbitrage) — the credential-based authority system degrades as users substitute LLM judgment for physician consultation
 *   - Analytical Observer: Views the constraint as structural liability asymmetry with pure extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_health_review, 0.58).
domain_priors:suppression_score(openai_health_review, 0.62).
domain_priors:theater_ratio(openai_health_review, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_health_review, extractiveness, 0.58).
narrative_ontology:constraint_metric(openai_health_review, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(openai_health_review, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_health_review, snare).
narrative_ontology:human_readable(openai_health_review, "OpenAI ChatGPT Health Review Service").
narrative_ontology:topic_domain(openai_health_review, "technological/economic").

domain_priors:requires_active_enforcement(openai_health_review).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_health_review, openai_revenue_capture).
narrative_ontology:constraint_beneficiary(openai_health_review, subscription_tier_expansion).
narrative_ontology:constraint_victim(openai_health_review, end_users_medical_liability).
narrative_ontology:constraint_victim(openai_health_review, medical_profession_epistemic_authority).
narrative_ontology:constraint_victim(openai_health_review, clinical_guideline_compliance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HEALTH CONSUMER (SNARE) — Individual users lack medical expertise to verify LLM recommendations against clinical evidence. Cannot exit: health decisions require immediate answers, and ChatGPT appears authoritative due to interface design and fluency. Suppressed alternatives: costly human physician consultation, time-intensive literature review. Bears full liability if LLM advice produces adverse outcomes. Maximum experienced extraction with no appeal mechanism.
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LICENSED PHYSICIANS (TANGLED ROPE) — Constrained by regulatory requirements, malpractice liability, and epistemic jurisdiction erosion. The service provides coordination benefit (accessible screening, triage logic) but extracts through patient deflection and authority dilution. Physicians see both a tool (rope function) and a competitor (snare function) simultaneously. Exit is constrained by patient demand for ChatGPT-first consultation.
constraint_indexing:constraint_classification(openai_health_review, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPENAI (ROPE) — Captures subscription revenue and user engagement expansion through health feature. Experiences the constraint as pure coordination: matching users to health information at scale. Arbitrage options are abundant (pivot feature, suppress health claims, rebrand as lifestyle tool). Net beneficiary with minimal extraction overhead. Effective suppression of regulatory friction through terms-of-service liability shields.
constraint_indexing:constraint_classification(openai_health_review, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY/CLINICAL GOVERNANCE (SCAFFOLD) — FDA, medical boards, clinical societies see this as a temporary governance gap with an identified sunset: clinical validation studies, professional liability frameworks, and regulatory guidance for AI-assisted medicine are being developed (21st Century Cures Act AI provisions, AAMI standards for clinical decision support). The extraction mechanism exists now, but the organized response (governance sunset) is reducing it. High suppression initially; declining over time as compliance infrastructure matures.
constraint_indexing:constraint_classification(openai_health_review, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICAL EPISTEMOLOGY (PITON) — Historically, the medical profession maintained epistemic authority through credentialing and liability. The ChatGPT health review degrades this system: it extracts legitimacy from the appearance of medical reasoning while having no accountable stake in outcomes. Theater is high (the interface mimics clinical consultation) but the functional verification is absent (no examination, no continuity of care, no legal liability for the AI system itself). The old authority structure persists through inertia — users still seek physician sign-off — but its practical function is eroding.
constraint_indexing:constraint_classification(openai_health_review, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a universal perspective, the constraint exhibits pure extraction through liability asymmetry: users bear full medical consequence risk while OpenAI bears only contractual reputational risk. The service generates revenue from users' health decisions without reciprocal responsibility for outcomes. No mitigation mechanism exists at scale. This is not coordination — it is transfer of decision-making authority without transfer of liability. Suppression is structural: users cannot contest the recommendation, cannot sue the AI system directly, and have no alternative for the speed/convenience combination.
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_health_review_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_health_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_health_review, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openai_health_review, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(openai_health_review, TR),
    TR >= 0.70.

:- end_tests(openai_health_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.58): High. The service captures revenue from users' health decisions (subscription revenue, engagement metrics, data from interactions) while transferring all adverse outcome liability to the user. The extraction is not total because some users do follow up with physicians, and the service does provide information access that would otherwise require costly consultation. However, the revenue model depends on users substituting ChatGPT for physician consultation, making the extractive component primary. Theater Ratio (0.68): High and increasing. The interface design (conversational, fluent, confident tone) mimics clinical consultation without the epistemic content. Users anthropomorphize the LLM, treating probabilistic outputs as definitive judgments. Recommended actions are presented with clinical plausibility but lack the accountability structures (examination, continuity of care, liability) that justify physician authority. Theater has increased as user familiarity breeds false confidence. Suppression (0.62): High. Users cannot easily verify LLM recommendations against clinical evidence bases. Alternative verification pathways (physician consultation, evidence review) are expensive and time-consuming. Users cannot sue the AI system directly — liability waivers are enforced through terms of service. Appeal mechanisms do not exist. Regulatory arbitrage allows OpenAI to operate in the health domain without FDA oversight (framed as lifestyle advice, not medical device). Claimed Type: Snare. The constraint meets all snare criteria: high extractiveness (0.58 > 0.46), high suppression (0.62 > 0.60), effective extraction for beneficiary (OpenAI) is maximized while victims (users, medical profession) have minimal exit options. The primary function is extraction (revenue capture) not coordination (health information provision) — health information is the mechanism, not the purpose.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's extractive nature. OpenAI experiences the service as pure coordination (Rope) — matching users to health information at scale, solving the access problem. Users experience it as a snare — trapped by the convenience/cost asymmetry, bearing liability without recourse. Physicians experience it as tangled rope — the service provides triage value but extracts through authority erosion and patient deflection. Regulatory observers see a scaffold with a sunset measured in years as clinical AI governance frameworks mature. The medical epistemology system sees it as piton degradation — the credential-based authority structure persists through inertia but loses functional weight as users bypass it. The analytical observer sees pure snare — structural liability asymmetry with no mitigation.
 *
 * DIRECTIONALITY LOGIC:
 *   OpenAI derives d ≈ 0.10 (beneficiary + arbitrage exit) → f(d) ≈ -0.01 → minimal experienced extraction. Users derive d ≈ 0.90 (victim + trapped exit) → f(d) ≈ 1.38 → high experienced extraction. Physicians derive d ≈ 0.55 (victim/beneficiary mixed + constrained exit) → f(d) ≈ 0.75 → moderate extraction. Regulatory agents derive d ≈ 0.45 (emerging victim via governance gap + constrained exit) → f(d) ≈ 0.50 → moderate extraction. The directionality values show that the constraint extracts from users and physicians while benefiting OpenAI. No overrides are needed — the structural relationships are clear from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy because the snare classification is not contestable across perspectives. All six perspectives converge on snare or snare-adjacent types (tangled rope, piton degradation are outcomes of the snare's operation). The mandatrophy would exist if there were a genuinely plausible coordination reading — but OpenAI cannot claim that users knowingly consent to liability transfer as part of a fair coordination mechanism. The users are trapped, not coordinating. The physicians' constrained exit is not voluntary participation in a coordination game — it is regulatory closure from above. The absence of a plausible rope or scaffold perspective from the beneficiary's standpoint indicates that the snare classification is robust. The false summit risk (analytical observer seeing natural law) is low because the constraint is entirely contingent on OpenAI's business model, regulatory arbitrage, and user behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clinical_validation_threshold,
    'What empirical validation evidence would convert this from a snare (pure extraction) to a tangled rope (mixed coordination/extraction)?',
    'Prospective randomized controlled trials comparing ChatGPT health recommendations to standard care; adverse event tracking; outcome-based liability data',
    'If validation shows >85% adherence to clinical guidelines: tension between snare and tangled_rope classification emerges. If <60%: snare classification is reinforced. Current data is absent — validation gap IS part of the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clinical_validation_threshold, empirical, 'Clinical validation evidence threshold for snare-to-tangled_rope transition').

omega_variable(
    liability_assignment_mechanism,
    'Can direct legal liability for AI-generated health advice be assigned through contract, statute, or case law such that OpenAI bears proportional outcome risk?',
    'Legislative/regulatory action (FDA reclassification as clinical decision support device); test case outcomes; insurance underwriting data for AI health services',
    'If liability assignment succeeds: extraction mechanism breaks, constraint downgrades to rope or scaffold. If liability remains with users: snare classification persists and deepens as adoption grows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_assignment_mechanism, empirical, 'Whether liability can be assigned to AI system provider').

omega_variable(
    user_literacy_and_contextualization,
    'Do users consistently understand ChatGPT health advice as probabilistic guidance requiring physician verification, or do they treat it as definitive medical judgment?',
    'User behavior studies; qualitative interviews; correlation between ChatGPT usage and physician consultation rates; adverse event attribution analysis',
    'If users consistently contextualize as guidance: suppression is lower than assessed, snare weakens. If users treat as judgment: suppression is higher, snare intensifies. Current evidence suggests users anthropomorphize and over-trust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_literacy_and_contextualization, empirical, 'User comprehension of LLM limitations in health context').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_health_review, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ohr_tr_t0, openai_health_review, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ohr_tr_t3, openai_health_review, theater_ratio, 3, 0.62).
narrative_ontology:measurement(ohr_tr_t6, openai_health_review, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(ohr_be_t0, openai_health_review, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ohr_be_t3, openai_health_review, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(ohr_be_t6, openai_health_review, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_health_review, information_standard).
narrative_ontology:affects_constraint(openai_health_review, medical_credential_authority_erosion).
narrative_ontology:affects_constraint(openai_health_review, clinical_liability_framework_gap).
narrative_ontology:affects_constraint(openai_health_review, regulatory_arbitrage_tech_health).

% DUAL FORMULATION NOTE:
% The ChatGPT health service is downstream of broader constraints on medical credentialing authority and clinical liability assignment in the digital health context. The service instantiates these upstream constraints at the user-facing layer, transforming them into direct extraction through the subscription model.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
