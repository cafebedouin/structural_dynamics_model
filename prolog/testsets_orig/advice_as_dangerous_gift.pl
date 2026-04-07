% ============================================================================
% CONSTRAINT STORY: advice_as_dangerous_gift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_advice_as_dangerous_gift, []).

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
 *   constraint_id: advice_as_dangerous_gift
 *   human_readable: The Hazard of Counsel: Advice as Dangerous Gift
 *   domain: social/philosophical
 *
 * SUMMARY:
 *   The hazard of counsel is the structural extraction embedded in the
 *   asymmetry between advice-giver and advice-receiver. The giver holds
 *   superior information (or claims to), while the receiver is socially
 *   obligated to take the counsel seriously. This creates a dynamic where the
 *   giver benefits from the receiver's deference and trust, while the
 *   receiver bears the cost of following bad advice without easy exit. The
 *   constraint is not malicious but structural: the very mechanism that makes
 *   advice useful (an asymmetry in knowledge or experience) also enables
 *   extraction. Over time, this constraint has accumulated theater — the
 *   performance of gratitude and deference to counsel has become ritualized
 *   independent of whether the advice actually helps. Simultaneously, the
 *   underlying extractiveness has grown as social obligation to heed counsel
 *   has strengthened (through mentorship culture, professional networks, and
 *   the ideology of wisdom-seeking) while feedback mechanisms and
 *   revisability norms remain weak.
 *
 * KEY AGENTS:
 *   - Advice Receiver: Primary victim (powerless/trapped) — bears the cost of following bad counsel and the social cost of visibly disagreeing; lacks exit without relationship damage
 *   - Advice Giver: Primary beneficiary (institutional/arbitrage) — retains epistemic authority and moral shelter ('I was just helping'); can exit without cost
 *   - Epistemic Autonomy: Secondary victim (powerless/trapped) — collective good that is eroded by obligation to defer to others' judgment; cannot organize or defend itself
 *   - Accountability Institutions: Organized actors (organized/constrained) — attempting to introduce reversibility norms, disclosure requirements, and duty-of-care standards to reduce the hazard through sunset mechanisms
 *   - Social Obligation Norms: Institutional structure (institutional/arbitrage) — the unspoken rule that counsel-seeking implies that you will seriously consider and likely follow the advice; persists through gratitude theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the asymmetry as inevitable rather than recognizing it as partly constructed by social deference norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(advice_as_dangerous_gift, 0.54).
domain_priors:suppression_score(advice_as_dangerous_gift, 0.65).
domain_priors:theater_ratio(advice_as_dangerous_gift, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(advice_as_dangerous_gift, extractiveness, 0.54).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(advice_as_dangerous_gift, tangled_rope).
narrative_ontology:human_readable(advice_as_dangerous_gift, "The Hazard of Counsel: Advice as Dangerous Gift").
narrative_ontology:topic_domain(advice_as_dangerous_gift, "social/philosophical").

domain_priors:requires_active_enforcement(advice_as_dangerous_gift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(advice_as_dangerous_gift, advice_giver).
narrative_ontology:constraint_victim(advice_as_dangerous_gift, advice_receiver).
narrative_ontology:constraint_victim(advice_as_dangerous_gift, epistemic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECEIVER (SNARE) — The advice-seeker is trapped by social obligation and power asymmetry. They cannot reject counsel without social cost (ingratitude, damaged relationship, or explicit conflict). They bear the full burden of following bad advice while the giver retains the moral shelter of 'I was just trying to help.' No exit option without social destruction.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RECEIVER WITH AGENCY (TANGLED ROPE) — Where the receiver has some social standing or alternative counsel sources, they experience partial exit options. The constraint remains (social expectation to seriously consider advice, career/relational costs of visible rejection), but is not total captivity. Benefits from genuine good counsel; costs from absorbing bad advice and the burden of filtering.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADVICE GIVER (ROPE) — The giver experiences the constraint as coordination: transmitting knowledge, helping others navigate uncertainty. From this perspective, the advice relationship is a pure coordination function with minimal extraction. The giver has high exit options (can choose not to advise, can revise counsel) and experiences the relationship as beneficial to both parties.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACCOUNTABILITY INSTITUTIONS (SCAFFOLD) — Organizations like mentorship programs, professional ethics bodies, and advice-licensing regimes see the hazard as a temporary structural problem solvable through sunset mechanisms: disclosure requirements, duty-of-care standards, and reversibility norms (advice must remain revisable, not binding). These coordinating institutions experience the constraint as having a clear exit path — better epistemic hygiene and accountability norms will reduce the hazard. This is architectural, not eternal.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GRATITUDE THEATER (PITON) — The ritual of thanking one's advisor, celebrating the mentor relationship, and performing deference to counsel persists long after its coordination function has atrophied. In modern contexts with access to diverse information sources, the performance of gratitude toward specific advice-givers has become largely theatrical — maintained through institutional inertia (graduation speeches about mentors, professional acknowledgments) rather than because the original coordination problem requires it. The piton maintains itself through narrative authority, not through structural necessity.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EPISTEMIC ASYMMETRY (MOUNTAIN) — From a civilizational perspective, some hazard in counsel is inherent to the structure of knowledge transmission: the giver has better information than the receiver (or claims to), and the receiver cannot fully verify that information without undoing the asymmetry that made the advice necessary. This could be read as a Mountain (immutable feature of human knowledge and social bonding), but the structural data reveals this as a false summit — the 'inherent' hazard is partly constructed by social obligation, power asymmetry, and lack of feedback mechanisms. Advice need not be a snare.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(advice_as_dangerous_gift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(advice_as_dangerous_gift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(advice_as_dangerous_gift, TR),
    TR >= 0.70.

:- end_tests(advice_as_dangerous_gift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The constraint extracts deference, cognitive labor (filtering bad advice), and sometimes material consequences from the receiver. However, the extraction is not maximal because: (1) some advice is genuinely helpful, (2) receivers retain some agency in how they apply counsel, and (3) the hazard depends on the giver's intent and the receiver's critical capacity. The value reflects that bad advice is a real structural harm, not merely an outcome risk. Suppression (0.65): High. Multiple suppression mechanisms: (1) Social obligation makes visible rejection costly, (2) Epistemic asymmetry (giver claims superior knowledge) suppresses the receiver's own judgment, (3) Gratitude norms suppress criticism, (4) Difficulty of assigning causation (is a bad outcome the advice's fault or the receiver's execution?) suppresses accountability. Theater ratio (0.58): Moderate-high. The constraint has shifted from a coordination mechanism (early mentor relationships with high information asymmetry) to a partly ritualized performance (gratitude theater, mentor worship in professional contexts) where the original function (transmitting hard-won knowledge) has been displaced by role-playing deference. The theater has increased because diverse information sources now provide alternatives to advice-seeking, but the social obligation persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The powerless receiver sees a snare (trapped, coerced into deference). The moderate receiver sees a mixed tangled rope (some exit options, some benefits). The beneficiary giver sees pure rope (coordination, mutual benefit). The accountability movement sees a solvable scaffold (sunset mechanisms can reduce the hazard). The gratitude ritual sees its own degradation as a piton (inertial performance). The civilizational observer risks naturalizing the asymmetry as a mountain but the structural data reveals this as a false summit — the obligation to defer is not inherent to knowledge transmission but constructed by social norms. The gap between powerless and institutional perspectives is maximum: the same relationship that feels like captivity to the receiver feels like helpful coordination to the giver.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position in the extraction flow. The advice receiver occupies high d (d ≈ 0.85-0.95): they are a victim with trapped exit options, bearing the full cost of following bad counsel while unable to walk away without social damage. The advice giver occupies low d (d ≈ 0.10-0.25): they are a beneficiary with arbitrage exit options — they can revise advice, disclaim responsibility, or exit the relationship with minimal cost. The asymmetry is structural: one party can exit freely while the other is socially bound to stay. This differential exit capacity drives the classification differences across perspectives. Epistemic autonomy occupies maximum d (d ≈ 1.0): it is the most powerless agent in the constraint system, completely unable to organize or defend itself against the erosion caused by obligation to defer to others' judgment.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by recognizing that the hazard of counsel is genuinely both coordination AND extraction simultaneously. It is NOT a false coordination masquerading as extraction (which would indicate a snare mislabeled as rope). Instead, the relationship authentically serves a coordination function (knowledge transfer, mentorship, guidance through uncertainty) while also extracting deference, cognitive labor, and sometimes harmful compliance. The mandatrophy is resolved by accepting that asymmetric coordination is possible — one party can benefit more than the other, and extraction can coexist with genuine coordination. The tangled rope classification holds because: (1) receivers do benefit from legitimate advice, (2) the constraint requires active enforcement (social obligation and gratitude norms must be maintained), (3) there are genuine victims (both the receiver who absorbs bad advice and the epistemic commons eroded by deference culture), and (4) there is a real coordination function (knowledge transmission). The false summit test: is the 'inherent hazard' of counsel actually inherent (Mountain) or constructed (Snare/Tangled Rope)? The structural data shows it is constructed — the suppression mechanisms (social obligation, deference norms, gratitude theater) are contingent institutional choices, not laws of nature. Therefore the analytical observer's mountain classification fails the false summit gate and is revealed as naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_causation_threshold,
    'At what severity threshold does bad advice constitute structural extraction rather than mere risk-taking or legitimate disagreement?',
    'Historical analysis of advice outcomes; distinction between outcome variance (advisor was wrong by chance) versus structural bias (advisor benefits from receiver following bad counsel)',
    'If threshold is low (any mismatch): most advice becomes snare. If threshold is high (severe harm only): benign extraction goes undetected. Mandatrophy resolution depends on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_causation_threshold, empirical, 'Threshold for distinguishing bad-luck outcomes from structural extraction').

omega_variable(
    asymmetry_necessity,
    'Is the epistemic asymmetry between giver and receiver necessary (some asymmetry must exist for advice to function) or constructed (social obligation exaggerates natural differences in knowledge)?',
    'Comparison of advice outcomes in high-transparency contexts (giver and receiver have equal access to information) versus opaque contexts; identification of whether suppressed alternatives explain the asymmetry',
    'If necessary: advice hazard is Mountain-adjacent (structural to knowledge). If constructed: the suppression (social obligation, deference norms) is the mechanism — snare/tangled rope dynamics are contingent institutional choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetry_necessity, conceptual, 'Whether epistemic asymmetry is inherent or constructed').

omega_variable(
    feedback_repair_effectiveness,
    'Do feedback mechanisms (advice-receiver can revise, dispute, or ignore without social cost) actually eliminate the hazard, or do they merely redistribute it?',
    'Longitudinal study of advice relationships with strong feedback norms (e.g., organizational contexts with psychological safety) versus those without; measurement of actual revision rates and costs of disagreement',
    'If feedback repairs hazard: scaffold sunset is achievable — norms of reversibility solve the snare problem. If feedback only redistributes: hazard persists in subtler form (receiver must appear grateful while privately ignoring, creating new extraction of emotional labor).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_repair_effectiveness, empirical, 'Whether feedback mechanisms eliminate or redistribute the hazard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(advice_as_dangerous_gift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(advc_tr_t0, advice_as_dangerous_gift, theater_ratio, 0, 0.35).
narrative_ontology:measurement(advc_tr_t5, advice_as_dangerous_gift, theater_ratio, 5, 0.48).
narrative_ontology:measurement(advc_tr_t10, advice_as_dangerous_gift, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(advc_be_t0, advice_as_dangerous_gift, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(advc_be_t5, advice_as_dangerous_gift, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(advc_be_t10, advice_as_dangerous_gift, base_extractiveness, 10, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(advice_as_dangerous_gift, information_standard).
narrative_ontology:affects_constraint(advice_as_dangerous_gift, trust_asymmetry).
narrative_ontology:affects_constraint(advice_as_dangerous_gift, epistemic_deference_culture).

% DUAL FORMULATION NOTE:
% The hazard of counsel is upstream of institutional trust dynamics and epistemic deference culture. The constraint represents the fundamental structural hazard; downstream constraints inherit the asymmetry and suppression mechanisms. Decomposition principle: if measuring advice by 'empirical accuracy of specific counsel' yields different ε than measuring by 'systemic suppression of receiver autonomy,' these are different constraints — the first is coordination (low ε), the second is extraction (high ε). This story measures the second.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(advice_as_dangerous_gift, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
