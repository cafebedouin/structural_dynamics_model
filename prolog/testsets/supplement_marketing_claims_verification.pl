% ============================================================================
% CONSTRAINT STORY: supplement_marketing_claims_verification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supplement_marketing_claims_verification, []).

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
 *   constraint_id: supplement_marketing_claims_verification
 *   human_readable: Supplement Marketing Claims Verification Bottleneck
 *   domain: health_commerce/regulatory_enforcement
 *
 * SUMMARY:
 *   The supplement marketing claims verification bottleneck creates a
 *   structural asymmetry between manufacturers' ability to make efficacy
 *   claims and consumers' ability to verify them. The U.S. regulatory
 *   framework (Dietary Supplement Health and Education Act, 1994) exempts
 *   supplements from pre-market efficacy requirements, creating a
 *   verification gap: manufacturers can claim health benefits based on weak
 *   evidence while consumers lack systematic access to comparative efficacy
 *   data. Theater has increased over the measurement interval as supplement
 *   marketing has become more sophisticated (celebrity endorsements, medical
 *   mimicry) while verification capacity has remained static or declined. The
 *   constraint exhibits different characters depending on observer position:
 *   pure extraction for trapped consumers, institutional coordination for
 *   manufacturers, regulatory compromise for agencies, and degraded ritual
 *   for third-party testing systems.
 *
 * KEY AGENTS:
 *   - Uninformed Consumer: Primary victim (powerless/trapped) — lacks expertise and access to verify claims; cannot exit marketplace
 *   - Health-Conscious Consumer: Secondary victim (moderate/constrained) — informed but structurally constrained by health conditions and perceived gaps in conventional medicine
 *   - Supplement Manufacturer: Primary beneficiary (institutional/arbitrage) — captures market value from unverified claims; can operate across jurisdictions with varying standards
 *   - Marketing Intermediaries: Secondary beneficiary (institutional/arbitrage) — influencers, retailers, distributors benefit from claim asymmetry
 *   - Regulatory Agency (FDA): Institutional actor caught in hybrid role (organized/constrained) — coordinates legitimate oversight while constrained by statutory limits and resource scarcity
 *   - Third-Party Testing System: Institutional performer (institutional/arbitrage) — conducts ritualistic testing that creates appearance of verification without preventing efficacy claims
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes the constraint as institutional choice, not natural limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supplement_marketing_claims_verification, 0.58).
domain_priors:suppression_score(supplement_marketing_claims_verification, 0.68).
domain_priors:theater_ratio(supplement_marketing_claims_verification, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supplement_marketing_claims_verification, extractiveness, 0.58).
narrative_ontology:constraint_metric(supplement_marketing_claims_verification, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(supplement_marketing_claims_verification, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supplement_marketing_claims_verification, snare).
narrative_ontology:human_readable(supplement_marketing_claims_verification, "Supplement Marketing Claims Verification Bottleneck").
narrative_ontology:topic_domain(supplement_marketing_claims_verification, "health_commerce/regulatory_enforcement").

domain_priors:requires_active_enforcement(supplement_marketing_claims_verification).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supplement_marketing_claims_verification, supplement_manufacturers).
narrative_ontology:constraint_beneficiary(supplement_marketing_claims_verification, marketing_intermediaries).
narrative_ontology:constraint_victim(supplement_marketing_claims_verification, consumer_health_outcomes).
narrative_ontology:constraint_victim(supplement_marketing_claims_verification, regulatory_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINFORMED CONSUMER (SNARE) — Consumers cannot exit the marketplace; verification barriers are completely opaque to them. Asymmetric information about product safety and efficacy creates pure extraction: manufacturers benefit from unverified claims while consumers bear health and financial costs with no mechanism to assess veracity.
constraint_indexing:constraint_classification(supplement_marketing_claims_verification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HEALTH-CONSCIOUS CONSUMER (SNARE) — Even informed consumers cannot fully exit: supplement use is often motivated by health conditions or gaps in conventional medicine. Exit costs are high (loss of perceived health benefit, switching to expensive pharmaceuticals). Verification capacity is constrained by lack of access to clinical data, funding for independent testing, and literacy barriers.
constraint_indexing:constraint_classification(supplement_marketing_claims_verification, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPPLEMENT MANUFACTURER (ROPE) — Benefits from weak verification requirements. Experiences the constraint as coordination: able to claim efficacy without expensive clinical trials, benefiting from the regulatory gap. Has full arbitrage exit (can operate in multiple jurisdictions with varying standards).
constraint_indexing:constraint_classification(supplement_marketing_claims_verification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — FDA coordinates legitimate supplement oversight (preventing adulteration, tracking adverse events) but cannot enforce full pre-market verification due to resource constraints and statutory limitations (DSHEA 1994). Agency simultaneously benefits from extractive enforcement (budget justification through catch-and-punish cycles) while bearing costs of inadequate verification capacity.
constraint_indexing:constraint_classification(supplement_marketing_claims_verification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THIRD-PARTY TESTING SYSTEM (PITON) — USP, NSF, and ConsumerLabs perform verification rituals (testing for label accuracy, adulterants) but these are largely performative and optional. Theater ratio (0.81) reflects that third-party seals create appearance of verification without preventing misleading efficacy claims. The ritual persists through consumer trust in logos, not through demonstrated verification effectiveness.
constraint_indexing:constraint_classification(supplement_marketing_claims_verification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, supplement verification contains genuine coordination (adverse event reporting, shared standards) nested within extractive asymmetry (unverified efficacy claims). The constraint is not immutable: regulatory frameworks (pre-market efficacy requirements) could exist, as do comparable systems in other jurisdictions. The barrier is institutional choice, not natural law.
constraint_indexing:constraint_classification(supplement_marketing_claims_verification, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supplement_marketing_claims_verification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supplement_marketing_claims_verification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supplement_marketing_claims_verification, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supplement_marketing_claims_verification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supplement_marketing_claims_verification, TR),
    TR >= 0.70.

:- end_tests(supplement_marketing_claims_verification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Manufacturers extract significant consumer surplus through unverified efficacy claims, but the extraction is not maximal because (1) some products deliver perceived benefits (placebo + behavior change), (2) health conditions create genuine market gap, and (3) some consumers are informed enough to partially compensate. The value increased from 0.38 to 0.58 over the interval as claim sophistication and marketing spend grew while verification mechanisms stagnated. Suppression (0.68): High. Multiple barriers prevent verification: (1) consumers lack resources for independent clinical testing ($5-15M per efficacy claim), (2) asymmetric information makes harm detection slow (adverse events accumulate over years), (3) regulatory capacity is structurally inadequate (FDA has ~100 investigators for ~80,000 supplement products), (4) publication bias in supplement research inflates perceived efficacy, (5) consumers internalize belief in supplement necessity through repeated messaging. Theater ratio (0.81): Very high and increasing. Third-party testing seals (USP, NSF, ConsumerLabs) verify label accuracy and absence of adulterants but do NOT verify efficacy claims — the seals create appearance of comprehensive verification while claims remain unvetted. Marketing increasingly exploits this distinction: products display third-party logos while efficacy claims remain speculative.
 *
 * PERSPECTIVAL GAP:
 *   The consumer and manufacturer perspectives are maximally separated. Consumers see a pure snare (no coordination function, only extraction). Manufacturers see rope (legitimate coordination of information, standards, distribution). The FDA sees tangled rope (genuine coordination of safety monitoring mixed with extractive asymmetry in claims). The third-party testing system sees its own role as rope (coordinating quality standards) while external observers see piton (performing ritualistic seals without preventing false claims). The analytical observer sees tangled rope (coordination exists but is nested in extraction) and recognizes the constraint as policy choice rather than natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Trapped consumers (powerless/trapped) experience maximum extractiveness: they cannot verify claims, cannot afford comprehensive testing, and exit costs are high (health risk + financial loss + inconvenience). Institutional manufacturers (institutional/arbitrage) experience low extractiveness: they benefit from the asymmetry and can exit at low cost (relocate to different regulatory jurisdiction). The FDA (organized/constrained) experiences mixed extractiveness: coordinating legitimate oversight (adverse event tracking, adulterant prevention) requires resources, but extractive asymmetry in claims justifies budget expansion—creating perverse incentive alignment. Directionality is straightforward: beneficiaries have arbitrage exit and low d; victims have trapped or constrained exit and high d. No overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the coordination/extraction tension by showing that verification bottleneck genuinely contains both functions. The FDA's adverse event reporting (coordination function) is real and valuable. The pre-market efficacy gap (extractive function) is also real and asymmetric. The constraint is NOT a snare disguised as rope, nor rope disguised as snare — it is a tangled rope where both functions are present. From the consumer's perspective, the coordination function is invisible (they cannot access adverse event data) and only extraction is visible → Snare. From the manufacturer's perspective, coordination (standards, distribution infrastructure) is salient → Rope. From the FDA's perspective, both are visible → Tangled Rope. The mandatrophy does not collapse to a single type because the constraint genuinely has different structural meanings for different agents. The resolution is NOT to claim all types are 'equally valid,' but to recognize that the perspectival gaps are diagnostically meaningful: they reveal that the constraint contains hidden extraction (the coordination function is real but asymmetrically distributed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    burden_of_proof_allocation,
    'Should manufacturers bear burden of proving efficacy (FDA drug model) or should consumers bear burden of proving harm (current DSHEA model)?',
    'Policy analysis comparing regulatory outcomes across jurisdictions with different burden allocations (EU, Canada, US); health outcome tracking before/after framework changes',
    'If manufacturer burden imposed: suppression and extractiveness drop significantly (→ Rope or Scaffold). If consumer burden maintained: extractiveness remains high (→ Snare or Tangled Rope). This is a choice variable, not an empirical discovery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_of_proof_allocation, preference, 'Allocation of proof burden between manufacturers and consumers').

omega_variable(
    efficacy_verification_cost_ceiling,
    'Below what product price threshold does clinical trial verification become economically infeasible for manufacturers?',
    'Cost analysis of clinical trials vs product price point; market data on supplement pricing; development cost data from natural products industry',
    'If ceiling < $20: most supplement market becomes unfeasible under efficacy requirements (→ Scaffold with sunset to pharmaceutical model). If ceiling > $50: efficacy verification becomes viable for broad market (→ Rope or Tangled Rope shift toward coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_verification_cost_ceiling, empirical, 'Economic viability threshold for efficacy verification by product price').

omega_variable(
    placebo_response_attribution,
    'How much measured efficacy in supplement users reflects actual bioactivity vs placebo response + health behavior changes (belief-driven lifestyle shifts)?',
    'Meta-analysis of supplement RCTs controlling for placebo; comparison of observed effect sizes with theoretical placebo response bounds; cohort tracking of lifestyle changes in supplement users',
    'If > 60% attributable to placebo + behavior: efficacy claims are largely theater (increases theater_ratio → Piton classification). If < 40%: genuine bioactivity is being suppressed by verification barriers (supports Snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(placebo_response_attribution, empirical, 'Attribution of measured efficacy to bioactivity vs placebo response and behavior change').

omega_variable(
    consumer_exit_cost_magnitude,
    'What is the actual health and financial cost for consumers to exit supplement reliance and transition to conventional medicine or lifestyle interventions?',
    'Prospective cohort study tracking cost and health outcomes for consumers discontinuing supplements; survey of switching costs (time, medical appointments, pharmaceutical costs); analysis of conditions where supplement reliance reflects genuine market gap vs perceived need',
    'If exit costs are very high (> $2000/year + significant health risk): consumers are genuinely trapped (→ Snare confirmed). If exit costs are moderate (< $500/year): consumers face constraint but have exit option (→ reclassify powerless agents to moderate or constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_exit_cost_magnitude, empirical, 'Actual exit cost for consumers transitioning away from supplement reliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supplement_marketing_claims_verification, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(suppl_tr_t0, supplement_marketing_claims_verification, theater_ratio, 0, 0.55).
narrative_ontology:measurement(suppl_tr_t10, supplement_marketing_claims_verification, theater_ratio, 10, 0.68).
narrative_ontology:measurement(suppl_tr_t20, supplement_marketing_claims_verification, theater_ratio, 20, 0.81).
narrative_ontology:measurement(suppl_tr_t5, supplement_marketing_claims_verification, theater_ratio, 5, 0.62).

% Extraction over time
narrative_ontology:measurement(suppl_be_t0, supplement_marketing_claims_verification, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(suppl_be_t10, supplement_marketing_claims_verification, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(suppl_be_t20, supplement_marketing_claims_verification, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(suppl_be_t5, supplement_marketing_claims_verification, base_extractiveness, 5, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supplement_marketing_claims_verification, information_standard).
narrative_ontology:affects_constraint(supplement_marketing_claims_verification, drug_efficacy_verification).
narrative_ontology:affects_constraint(supplement_marketing_claims_verification, advertising_health_claims_regulation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the DSHEA statutory framework and upstream of specific product category claims (probiotics, joint health, cognitive enhancement). The statutory framework creates the verification gap; individual product categories exhibit different extraction rates depending on evidentiary base and marketing intensity. See linked constraints for domain-specific variants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
