% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Legalization Regulatory Regime (Autonomy-Limited)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The legalization reading instantiates a constraint where competent adults
 *   possess autonomy over substance use, and state authority is legitimated
 *   only by preventing third-party harm. In practice, the regulatory regimes
 *   that implement this reading (e.g., cannabis legalization in US states,
 *   Canada, Uruguay; alcohol/tobacco frameworks) create a legal commercial
 *   market. The reading claims this regime is a Rope: a coordination
 *   mechanism for third-party harm prevention that respects autonomy. The
 *   authored metrics describe a regime where corporate actors capture the
 *   regulatory apparatus, extract surplus from users through pricing and
 *   taxation, suppress non-commercial alternatives (home grow, collectives,
 *   nonprofit models), and maintain enforcement against the residual illicit
 *   market. The divergence between the reading's claim (rope/mountain) and
 *   the operational metrics (tangled rope) is the measurement the corpus
 *   exists to take.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.65).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.55).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalization Regulatory Regime (Autonomy-Limited)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '2524fbd0-5a44-4861-8dd4-40d302ed3afe').
narrative_ontology:cs_kernel_codification('2524fbd0-5a44-4861-8dd4-40d302ed3afe', formalized).
narrative_ontology:cs_authority_grounding('2524fbd0-5a44-4861-8dd4-40d302ed3afe', extraction).
narrative_ontology:cs_interpretation_layer_present('2524fbd0-5a44-4861-8dd4-40d302ed3afe').
narrative_ontology:cs_reading_relation('2524fbd0-5a44-4861-8dd4-40d302ed3afe', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('2524fbd0-5a44-4861-8dd4-40d302ed3afe', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('2524fbd0-5a44-4861-8dd4-40d302ed3afe', foundational, bodily_autonomy_includes_substance_use).
narrative_ontology:cs_axiom_status(bodily_autonomy_includes_substance_use, holdable).
narrative_ontology:cs_axiom_grounding('2524fbd0-5a44-4861-8dd4-40d302ed3afe', bodily_autonomy_includes_substance_use, deontological).
narrative_ontology:cs_axiom('2524fbd0-5a44-4861-8dd4-40d302ed3afe', foundational, state_authority_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_authority_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('2524fbd0-5a44-4861-8dd4-40d302ed3afe', state_authority_limited_to_third_party_harm, conventional).
narrative_ontology:cs_axiom('2524fbd0-5a44-4861-8dd4-40d302ed3afe', secondary, commercial_market_is_necessary_for_safe_supply).
narrative_ontology:cs_axiom_status(commercial_market_is_necessary_for_safe_supply, holdable).
narrative_ontology:cs_axiom_grounding('2524fbd0-5a44-4861-8dd4-40d302ed3afe', commercial_market_is_necessary_for_safe_supply, instrumental).
narrative_ontology:cs_reference_frame('2524fbd0-5a44-4861-8dd4-40d302ed3afe', autonomy_based_regulatory_framework).
narrative_ontology:cs_drift_state('2524fbd0-5a44-4861-8dd4-40d302ed3afe', post_legalization_implementation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2524fbd0-5a44-4861-8dd4-40d302ed3afe', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, corporate_market_actors).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, regulatory_apparatus).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, small_scale_producers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, communities_affected_by_commercialization).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, corporate_market_actors).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, regulatory_apparatus).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, bodily_autonomy_principle).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, harm_reduction_over_criminalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce the legalization statute: licensing regimes, tax structures, advertising restrictions, potency caps, impaired-driving standards, and age-gating. They claim the regime balances autonomy with third-party harm prevention. They collect tax revenue and regulatory fees, and face lobbying from all sides.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_legislature_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Large-scale licensed producers and retailers (cannabis, alcohol, tobacco, emerging psychedelics). They capture the legal market's economic surplus through branding, lobbying for barriers to entry (high license fees, vertical integration mandates), and economies of scale. They pay compliance costs and taxes but shape regulations to favor incumbents. Exit means selling licenses or moving to other legal markets.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, corporate_market_actors, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, corporate_market_actors, payer).

% Collect excise, sales, and income taxes from the legal market. Revenue is earmarked for general funds or specific programs (treatment, education). They have a structural interest in maximizing taxable volume, which aligns with corporate actors against home production and low-tax alternatives.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_tax_authorities, beneficiary,
    institutional, generational, arbitrage, national).

% Agencies that license, inspect, test, and enforce. Their budgets and headcount grow with the regulated market. They frame their mission as public health protection but face regulatory capture pressure from industry. Staff rotate between agencies and industry (revolving door).
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, regulatory_apparatus, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, regulatory_apparatus, payer).

% No longer face criminal penalties for possession/use within legal limits. Gain access to tested, labeled products and legal retail. But pay high prices from taxes and corporate markups, face purchase limits, potency caps, and surveillance (track-and-trace, purchase records). Home cultivation is restricted or banned in many jurisdictions. Illicit market remains an exit option but carries legal risk.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, substance_users, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, substance_users, beneficiary).

% Legacy growers, craft producers, cooperatives. Face prohibitive licensing fees, capital requirements, compliance complexity, and distribution bottlenecks controlled by large operators. Many cannot enter the legal market and remain in the illicit economy or exit entirely. No meaningful political voice in regulatory design.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, small_scale_producers, payer,
    powerless, biographical, trapped, regional).

% Neighborhoods with high density of retail outlets, advertising, and normalized consumption. Experience increased youth exposure, traffic impacts, and cultural displacement. Promised community reinvestment from tax revenue often fails to materialize or is captured by municipal general funds. Organize locally but lack statewide influence.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, communities_affected_by_commercialization, payer,
    powerless, generational, trapped, local).

% Law enforcement unions, some parent groups, moral/religious organizations. Argue legalization increases use, harms youth, and fails to eliminate illicit market. They are structurally excluded from the regulatory conversation because the legalization framework treats prohibition as settled-defeated. Their exit is political reversal (re-criminalization), which is institutionally difficult.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% Advocates for non-commercial models: decriminalization without commercial sales, home-grow rights, nonprofit distribution, or user collectives. They argue the corporate legalization model replicates alcohol/tobacco harms. Excluded from licensing regimes that require capital and compliance infrastructure. Their exit is building parallel structures (gifting economies, buyers' clubs) which remain legally precarious.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, abolitionist_activists, excluded,
    moderate, biographical, constrained, national).

% Study usage patterns, health outcomes, youth initiation, traffic safety, and equity impacts under legalization. Produce evidence that all other seats cite selectively. Their independence varies with funding sources (government, foundations, industry). They do not set policy but define the empirical terrain.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, corporate_market_actors).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulating a legal substance market to prevent third-party harms (impaired driving, secondhand exposure, youth access, product contamination) while providing a safe, tested supply chain and displacing the illicit market's violence and unpredictability.
% TRANSFER_FUNCTION: Moves revenue from users to corporate market actors (via pricing power and branding) and to the state (via excise and sales taxes). Moves regulatory compliance costs onto producers (disproportionately burdening small operators). Moves enforcement resources from possession/cultivation crimes to impaired-driving enforcement, license compliance, and illicit-market suppression.
% ABSENT_VOICES: Prohibition advocates who want criminalization restored; abolitionist activists who want non-commercial legal models (home grow, collectives, nonprofit distribution); communities most impacted by retail density and advertising who lack standing in state-level licensing; legacy illicit-market participants barred from legal entry by capital and compliance barriers.
% DISAPPEARANCE_RATIONALE: If the legalization regime vanished overnight, the legal market would collapse. Users would revert to illicit supply (with its violence, adulteration, and criminal penalties) or to home production where feasible. The state would lose billions in tax revenue. Corporate actors would pivot to other markets or lobby for re-prohibition with carve-outs. A new arrangement — likely a patchwork of decriminalization, gray markets, and localized regulation — would emerge within months.
% FOUNDING_PROBLEM: The harms of prohibition: mass incarceration (racially disproportionate), unsafe/unregulated supply causing overdose and poisoning, criminal market violence and corruption, billions in enforcement costs, lost tax revenue, and the moral injury of criminalizing autonomous adult use.
% FOUNDING_PROBLEM_CORROBORATION: Drug Policy Alliance, ACLU, and public health researchers (outside corporate beneficiaries) attest prohibition's harms were real and legalization reduced incarceration and improved product safety. Law enforcement associations, some community coalitions, and youth prevention groups attest that prohibition's harms have been replaced by commercialization harms: high-potency products, aggressive marketing, youth normalization, and persistent illicit markets. No single independent body corroborates that the founding problem is fully solved.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects corporate pricing power, high tax burdens on users, and regulatory barriers that concentrate market power. Suppression (0.55) is lower than prohibition but substantial: enforcement targets unlicensed production/sale, home cultivation beyond limits, and impaired driving; marketing restrictions suppress commercial speech; track-and-trace systems surveil the legal supply chain. Theater ratio (0.45) captures the gap between public-health rhetoric (protecting youth, preventing impaired driving) and commercial reality (high-potency products, brand marketing, lobbyist-written regulations). Accessibility collapse (0.40) is moderate: illicit market and home grow persist as alternatives but are legally risky and supply-constrained. Resistance (0.60) is high: industry lobbies for favorable rules, users organize for home-grow rights, prohibitionists push for rollback, abolitionists build parallel structures.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state regulators) experiences the constraint as a genuine coordination problem: how to regulate a market that must exist (autonomy) while minimizing third-party harm. The payer seats (users, small producers, affected communities) experience it as extraction: high prices, restricted alternatives, commercialized harms. The beneficiary seats (corporate actors, tax authorities) experience it as a protected revenue stream. The engine computes this divergence from the structural data — the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulators are near-symmetric (d ~0.5): they administer the regime, collect fees, but face political pressure from all sides. Corporate actors are beneficiaries (d ~0.15): they capture surplus, shape rules, have mobile exit. Tax authorities are beneficiaries (d ~0.1): pure revenue collection with arbitrage-grade exit. Regulatory apparatus is dual (d ~0.4): budget depends on market size but mission is public health. Users are payers with constrained exit (d ~0.7): they pay the transfer, face surveillance, but have illicit-market outside option. Small producers are trapped payers (d ~0.9): no viable exit, high barriers. Affected communities are trapped payers (d ~0.85): geographic immobility, no voice. Prohibition advocates and abolitionists are excluded (d not computed): they are not governed by the regime's coordination logic but by its boundary enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prohibition's harms) is contested: incarceration and product safety improved, but commercialization introduced new harms. The regime persists not because the founding problem is solved, but because the corporate-state coalition benefits from the arrangement. This is mandatrophy: the mandate (third-party harm prevention) has been extended to cover revenue maximization and market control. The reading's own axioms (bodily autonomy, limited state authority) are overridden in practice by commercial imperatives. The constraint is not a snare (users are not primary victims of criminalization anymore) but a tangled rope: real coordination function (third-party harm prevention, product safety) fused with asymmetric extraction (corporate capture, regressive taxation, suppression of non-commercial models).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (legalization_reading) of the contested kernel substance_control_legitimacy. What structural elements distinguish this reading from its siblings (prohibition_reading, harm_reduction_reading)?',
    'Comparative analysis of each reading''s beneficiary/victim structure, claimed_type, and axioms. The engine computes per-reading classifications from authored structural data.',
    'If readings are not structurally distinct, they collapse into one constraint and ε-invariance fails. Distinct classifications across readings validate the kernel decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing: this file instantiates one reading of a contested kernel; sibling readings are separate constraint stories.').

omega_variable(
    corporate_capture_necessity,
    'Is the corporate extractiveness observed in legalization regimes (high barriers to entry, lobbying for favorable regulations, marketing of high-potency products) a necessary feature of any commercial legalization model, or a contingent capture that alternative designs (nonprofit distribution, user collectives, home-grow-only) could avoid?',
    'Natural experiment comparison: jurisdictions with different regulatory designs (e.g., Uruguay''s state monopoly vs. US commercial models vs. Netherlands'' coffee shop toleration vs. decriminalization-only Portugal). Measure extractiveness, suppression, and theater across designs.',
    'If corporate capture is necessary, the legalization reading''s axioms (autonomy, limited state) are structurally incompatible with any commercial regime — the reading forecloses itself. If contingent, the reading could be realized with lower extractiveness via different institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_capture_necessity, empirical, 'Whether the tangled rope character is inherent to legalization or a design failure.').

omega_variable(
    third_party_harm_boundary,
    'What counts as ''third-party harm'' under the legalization reading''s limiting principle? Does it include: secondhand smoke/vapor exposure? Normalized consumption increasing youth initiation? Workplace impairment risk? Community impacts of retail density? Fiscal externalities (healthcare costs)?',
    'Legislative and judicial record: how do legalization statutes and court rulings define the scope of permissible regulation? Comparative analysis of regulatory creep over the interval.',
    'A narrow definition (only acute physical harm to identifiable others) keeps the regime near rope. A broad definition (any population-level risk) expands state authority toward the prohibition reading''s scope, increasing suppression and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_harm_boundary, conceptual, 'The boundary of the reading''s own limiting principle determines the constraint''s operational scope.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by small producers and affected communities structural (licensing laws, capital requirements, zoning) or internalized (belief that legal market is the only legitimate path, stigma against illicit/gray market participation)?',
    'Post-exit trajectory study: if suppression persists after agents exit the legal market (e.g., return to illicit market, attempt collectives), reclassify as partially internalized. Survey data on perceived legitimacy of alternatives.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — agents carry the constraint''s legitimacy framework with them. This would increase effective extraction for trapped seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the legalization regime''s boundary enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__legalization_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__legalization_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__legalization_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__legalization_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__legalization_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__legalization_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__legalization_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__legalization_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__legalization_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% Kernel substance_control_legitimacy decomposes into three readings with distinct ε and stakeholder structures. This reading (legalization) claims autonomy as Mountain-like right but operates as commercial resource_allocation coordination with corporate extraction. Prohibition_reading claims moral duty Mountain but operates as Snare (criminalization extracts from users, benefits enforcement/prison). Harm_reduction_reading claims public health Rope but operates as Scaffold (transitional, no commercial market). The ε values differ: prohibition ε~0.85, legalization ε~0.65, harm_reduction ε~0.35. They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, institutional, 0.3).
constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, powerful, 0.2).
constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, moderate, 0.65).
constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
