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
    constraint_indexing:directionality_override/3,
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
 *   domain: technological/economic/medical
 *
 * SUMMARY:
 *   OpenAI's ChatGPT health review service exemplifies a modern snare
 *   constraint: the platform captures revenue from users seeking health
 *   guidance while transferring all adverse outcome risk to those users
 *   through terms-of-service disclaimers and the fiction that LLM outputs are
 *   not medical advice. The constraint exhibits rising extractiveness (from
 *   0.45 to 0.68 over the interval) as user adoption increases and OpenAI's
 *   monetization deepens. Theater ratio (0.65) reflects the performative
 *   nature of medical-style reasoning without medical accountability — the
 *   LLM mimics clinical decision-making patterns (history, differential
 *   diagnosis, recommendation) while bearing none of the liability.
 *   Suppression (0.72) is structural: users face health urgency, lack
 *   expertise to evaluate LLM accuracy, cannot access licensed alternatives
 *   at equivalent cost, and face regulatory and epistemic asymmetry. The
 *   constraint persists because OpenAI has captured a regulatory gap and
 *   actively maintains it through institutional power (terms-of-service,
 *   opacity around medical training data, resistance to transparency
 *   standards). The analytical observer risks naturalizing this as an
 *   inherent limit on human-AI trust (mountain), but the structural data
 *   reveals it as a contingent extractive arrangement. Regulatory coalition
 *   perspectives (FDA, medical boards, health advocacy) see the constraint as
 *   a temporary gap with a sunset clause — medical AI standards and liability
 *   reclassification will eventually close the arbitrage.
 *
 * KEY AGENTS:
 *   - Health Decision Users: Primary victims (powerless/trapped) — bear full adverse outcome risk; have health urgency reducing exit options; cannot easily evaluate LLM medical accuracy
 *   - OpenAI Corporation: Primary beneficiary (institutional/arbitrage) — captures subscription/API revenue from health guidance without reciprocal liability; exploits regulatory gap
 *   - Clinical Medicine Practitioners: Secondary victims (moderate/constrained) — bear reputational and litigation risk as LLM outputs contaminate patient expectations; have professional ethics obligations but no direct remedy against OpenAI
 *   - Clinical Epistemic Reliability: Abstract victim (powerless/trapped) — health misinformation accumulates in user population; cannot organize or exit; bears cost of reduced trust in legitimate medical guidance
 *   - Medical Regulatory Coalition: Organized agents (organized/constrained) — FDA, medical boards, health advocacy groups seeking regulatory closure; see constraint as sunset-able through liability reclassification and AI medical device standards
 *   - Medical Liability System: Institutional actor (institutional/arbitrage) — terms-of-service framework transfers risk; courts have not yet formalized standards; sees own function as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent asymmetry as law of nature; needs structural data to distinguish natural limits from institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_health_review, 0.68).
domain_priors:suppression_score(openai_health_review, 0.72).
domain_priors:theater_ratio(openai_health_review, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_health_review, extractiveness, 0.68).
narrative_ontology:constraint_metric(openai_health_review, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(openai_health_review, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_health_review, snare).
narrative_ontology:human_readable(openai_health_review, "OpenAI ChatGPT Health Review Service").
narrative_ontology:topic_domain(openai_health_review, "technological/economic/medical").

domain_priors:requires_active_enforcement(openai_health_review).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_health_review, openai_corporation).
narrative_ontology:constraint_victim(openai_health_review, health_decision_users).
narrative_ontology:constraint_victim(openai_health_review, clinical_epistemic_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HEALTH DECISION USER (SNARE) — Trapped by health urgency, information asymmetry, and cost barriers. Cannot easily exit: medical guidance is valuable and alternatives (licensed physicians) are expensive or inaccessible. Bears full adverse outcome risk while OpenAI captures revenue. Maximum experienced extraction.
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLINICAL PRACTITIONERS (SNARE) — Constrained by liability exposure and professional ethics. Cannot avoid the consequence of LLM health claims contaminating patient expectations and self-diagnosis patterns. Bear reputational and litigation risk; no direct compensation from OpenAI. Extraction flows from clinical epistemic commons toward OpenAI.
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPENAI CORPORATION (ROPE) — Experiences the constraint as pure revenue coordination: capture user willingness-to-pay for health guidance, outsource liability through terms of service, monetize trust-seeking behavior. Arbitrage exit options (regulatory gap, jurisdictional variation). Net beneficiary — extraction runs consistently toward this agent.
constraint_indexing:constraint_classification(openai_health_review, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL REGULATORY COALITION (SCAFFOLD) — Organized agents (FDA, medical boards, health advocacy groups) see the constraint as a temporary gap in regulatory framework. Regulatory action (licensing requirements, liability reassignment, disclosure mandates) has a sunset clause — regulatory capture by tech will eventually resolve through either formalized medical AI standards or litigation. Constraint exists because regulatory lag, not because it's efficient.
constraint_indexing:constraint_classification(openai_health_review, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL LIABILITY SYSTEM (PITON) — The constraint persists through institutional inertia: medical liability law was architected for human practitioners, not LLM systems. Courts have not yet formalized liability standards for algorithmic health guidance. The system maintains the fiction that terms-of-service disclaimers transfer all risk to users, but this is increasingly theater — litigation precedent is accumulating (algorithmic negligence, failure to warn). The liability framework itself is degraded (no longer functional) but enforcement remains weak, so the constraint persists.
constraint_indexing:constraint_classification(openai_health_review, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing contingent institutional gaps as laws of nature. From a civilizational perspective, some information asymmetry between AI capability and user understanding is inherent — users will always struggle to calibrate trust in LLM outputs. This perspective sees the constraint as an immutable feature of how non-experts interface with black-box systems. However, the structural data contradicts this: OpenAI actively benefits from the asymmetry, has institutional power to close it, and chooses not to. This is not a natural law but a contingent extractive arrangement.
constraint_indexing:constraint_classification(openai_health_review, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_health_review_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_health_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_health_review, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.68): High. OpenAI captures revenue from users seeking health guidance (subscription tier includes health features, API access for health products) while transferring all adverse outcome risk through terms of service. The extraction is sustained because users face health urgency and cannot easily evaluate LLM accuracy. Unlike legitimate medical services that bear liability and operate under regulatory oversight, OpenAI captures the benefit without the cost. The rising trajectory (0.45 → 0.68) reflects deepening monetization as health features are integrated into base product and enterprise health API offerings expand. Suppression (0.72): High and rising. Structural barriers to exit include: (1) Health urgency — users seek guidance when facing health decisions; (2) Information asymmetry — users cannot evaluate whether LLM outputs are accurate without medical training; (3) Cost barriers — licensed medical consultation remains expensive or inaccessible; (4) Regulatory capture — OpenAI's terms-of-service framework legally transfers liability, creating epistemic trap (users cannot know actual risk); (5) Opacity — training data, accuracy rates on medical benchmarks, and failure modes are not disclosed. Theater ratio (0.65): High and rising. The service mimics clinical decision-making (taking history, generating differential diagnoses, recommending tests or specialist referrals) without medical accountability. The performative elements are: (1) The interface presents LLM reasoning in clinical format, suggesting medical expertise; (2) Disclaimers state 'not medical advice' but users interpret the detailed reasoning as substantive guidance; (3) The service updates with new medical knowledge (training data refresh) to maintain appearance of current clinical knowledge while remaining unvalidated on current standards; (4) OpenAI references medical training data in marketing but does not publish validation studies. These are all theater: they simulate accountability without instantiating it.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between OpenAI (rope) and health decision users (snare) is maximal. OpenAI sees pure coordination: they are providing a service users want, at a price users accept, with clear terms. This is rope from their perspective — solving the collective action problem of matching supply (AI health guidance capability) to demand (user willingness to pay). Users see extraction with no reciprocal liability: they pay, bear all risk, and OpenAI captures revenue while transferring adverse outcomes. The gap between these two perspectives is not a difference in observed facts but a difference in what each agent bears. The clinical practitioners (snare) see contamination of medical epistemology: users arrive at appointments expecting LLM-suggested diagnoses to be validated, reducing trust in physician expertise. The regulatory coalition (scaffold) sees the gap as temporary — medical AI standards and liability reclassification will close it. The piton perspective on medical liability sees the constraint persisting through degraded institutional function (terms-of-service fiction that liability can be transferred) rather than genuine efficiency. The analytical observer risks collapsing these perspectives into a single 'inherent limit' story (mountain) but the structural data shows this is naturalization of contingent arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: OpenAI captures subscription/API revenue from health features without reciprocal liability. The extraction flow runs consistently toward OpenAI — they monetize user trust without investing in medical oversight, validation, or insurance. Victim: Health decision users bear full adverse outcome risk while OpenAI captures revenue. The extraction flow runs away from users — they pay and assume outcome liability. The directionality principle: beneficiary + arbitrage exit = low d (negative chi); victim + trapped exit = high d (maximum chi). The snare classification holds because suppression (0.72) is high enough that users cannot easily exercise whatever exit options nominally exist (choosing licensed alternatives, opting out of health features).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the perspectival structure: all six types are legitimate readings from different structural positions. OpenAI (institutional/arbitrage) sees rope — a coordination mechanism. Health users (powerless/trapped) see snare — pure extraction. Clinical practitioners see snare contamination of their own epistemic commons. The regulatory coalition sees a temporary gap with a sunset (scaffold). The medical liability system sees its own degraded function (piton). The analytical observer risks naturalizing as mountain but the structural data (rising extractiveness, high suppression, OpenAI's active maintenance of regulatory gap) reveals this as false summit. The constraint does not resolve to a single type — it exhibits different types for different agents. The presheaf over the observation site reveals the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_framework_ambiguity,
    'Does the terms-of-service liability transfer actually hold in court, or is it theater masking latent institutional liability?',
    'Litigation outcomes on negligence claims; court decisions on whether algorithmic health guidance triggers duty-of-care standards; regulatory reclassification of LLM health products as medical devices',
    'If ToS holds: constraint remains a snare (users bear full risk). If courts reject transfer: constraint shifts toward institutional liability (OpenAI bears risk), reclassifying as tangled_rope or rope with regulatory enforcement. This is the highest-impact omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_framework_ambiguity, empirical, 'Whether terms-of-service liability transfer is enforceable or theater').

omega_variable(
    user_calibration_ceiling,
    'Can users realistically calibrate their trust in LLM health guidance to match actual accuracy rates, or does the interface inherently produce overconfidence?',
    'User studies on confidence vs accuracy; correlation between user self-assessed understanding and actual LLM failure modes; A/B testing of warning clarity and user behavior change',
    'If users can calibrate: suppression is lower than measured (users have agency to discount advice). If inherent overconfidence exists: suppression is structural, not discounted by user sophistication. Affects whether the constraint is purely extractive or has residual user agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_calibration_ceiling, empirical, 'User ability to calibrate trust in LLM health guidance').

omega_variable(
    alternative_access_viability,
    'Are regulatory-compliant alternatives (licensed telemedicine, AI-assisted physician consultation) economically viable as substitutes, or does cost/accessibility permanently trap users into the LLM path?',
    'Market analysis of telemedicine pricing vs LLM pricing; adoption rates of regulatory-compliant AI health services; geographic variation in alternative access',
    'If viable alternatives exist at comparable cost: users have actual exit options, reducing perceived suppression and possibly reclassifying from snare to constrained (tangled_rope). If alternatives remain expensive: suppression remains structural, trapping low-income users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_access_viability, empirical, 'Economic viability of regulatory-compliant health guidance alternatives').

omega_variable(
    false_summit_naturalization,
    'Is the constraint a natural limit on how non-experts can interface with black-box systems, or a contingent institutional arrangement that OpenAI actively maintains?',
    'Comparative analysis: do other LLM providers offer higher-transparency health guidance? Do regulated medical AI systems achieve different trust-calibration patterns? Is the asymmetry a feature of LLM cognition or a design choice?',
    'If natural law: mountain classification holds; constraint is immutable. If contingent: false-summit mountain is revealed; constraint is extractive arrangement that could be otherwise designed. This determines whether the analytical observer''s perspective is legitimate or naturalizing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether information asymmetry is inherent to LLM systems or contingent design choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_health_review, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ohr_tr_t0, openai_health_review, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ohr_tr_t2, openai_health_review, theater_ratio, 2, 0.55).
narrative_ontology:measurement(ohr_tr_t4, openai_health_review, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(ohr_be_t0, openai_health_review, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ohr_be_t2, openai_health_review, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(ohr_be_t4, openai_health_review, base_extractiveness, 4, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ohr_su_t0, openai_health_review, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ohr_su_t2, openai_health_review, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(ohr_su_t4, openai_health_review, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_health_review, information_standard).
narrative_ontology:affects_constraint(openai_health_review, medical_ai_liability_framework).
narrative_ontology:affects_constraint(openai_health_review, health_information_epistemic_commons).
narrative_ontology:affects_constraint(openai_health_review, telemedicine_regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% This constraint is decomposed from a broader 'AI health guidance' family. Upstream constraints (medical_ai_liability_framework) establish the legal fiction that liability can be transferred. This constraint (openai_health_review) instantiates that fiction in a specific high-adoption service. Downstream constraints (health_information_epistemic_commons) model contamination of medical knowledge among users. All three are linked via network effects: the liability framework enables OpenAI's business model, which in turn contaminates epistemic commons, which eventually forces regulatory response that closes the liability gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openai_health_review, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
