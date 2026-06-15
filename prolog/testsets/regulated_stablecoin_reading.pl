% ============================================================================
% CONSTRAINT STORY: regulated_stablecoin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulated_stablecoin_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulated_stablecoin_reading
 *   human_readable: Regulated Stablecoin Framework
 *   domain: monetary_policy/digital_currency/behavioral_economics
 *
 * SUMMARY:
 *   This constraint instantiates the regulated stablecoin reading of the
 *   digital money legitimacy kernel. It describes a framework where private
 *   firms may issue digital currency tokens if they maintain full sovereign
 *   currency reserves and submit to regulatory oversight. The arrangement is
 *   claimed as tangled_rope: genuine coordination (payment efficiency,
 *   financial inclusion) coupled with asymmetric extraction (monetary policy
 *   leakage, seigniorage capture by private issuers). The metrics describe
 *   moderately extractive operation with rising enforcement requirements as
 *   the framework matures and issuers test regulatory boundaries.
 *
 * KEY AGENTS:
 *   - stablecoin_issuers: Primary agenda-setters (powerful/mobile) — issue tokens, set terms, collect fees and float income
 *   - payment_service_providers: Primary beneficiaries (organized/mobile) — gain settlement efficiency without banking licenses
 *   - underbanked_populations: Beneficiaries (powerless/constrained) — access payment functionality previously unavailable
 *   - central_bank_monetary_authority: Primary victim (institutional/constrained) — loses monetary policy transmission effectiveness
 *   - traditional_banking_sector: Victim (institutional/constrained) — faces deposit flight and lending capacity erosion
 *   - financial_regulators: Agenda-setters and observers (institutional/analytical) — design and enforce the framework
 *   - monetary_economists: Observers (analytical/analytical) — study transmission effects and stability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulated_stablecoin_reading, 0.42).
domain_priors:suppression_score(regulated_stablecoin_reading, 0.58).
domain_priors:theater_ratio(regulated_stablecoin_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulated_stablecoin_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(regulated_stablecoin_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(regulated_stablecoin_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(regulated_stablecoin_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(regulated_stablecoin_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulated_stablecoin_reading, tangled_rope).
narrative_ontology:human_readable(regulated_stablecoin_reading, "Regulated Stablecoin Framework").
narrative_ontology:topic_domain(regulated_stablecoin_reading, "monetary_policy/digital_currency/behavioral_economics").

domain_priors:requires_active_enforcement(regulated_stablecoin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(regulated_stablecoin_reading, '9e7c5c22-1004-4d57-bf76-a25b4681f43f').
narrative_ontology:cs_kernel_codification('9e7c5c22-1004-4d57-bf76-a25b4681f43f', formalized).
narrative_ontology:cs_authority_grounding('9e7c5c22-1004-4d57-bf76-a25b4681f43f', lineage).
narrative_ontology:cs_interpretation_layer_present('9e7c5c22-1004-4d57-bf76-a25b4681f43f').
narrative_ontology:cs_reading_relation('9e7c5c22-1004-4d57-bf76-a25b4681f43f', digital_money_legitimacy__sovereign_cbdc_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e7c5c22-1004-4d57-bf76-a25b4681f43f', digital_money_legitimacy__crypto_permissionless_reading, influences).
narrative_ontology:cs_axiom('9e7c5c22-1004-4d57-bf76-a25b4681f43f', foundational, private_innovation_within_regulatory_perimeter).
narrative_ontology:cs_axiom_status(private_innovation_within_regulatory_perimeter, holdable).
narrative_ontology:cs_axiom_grounding('9e7c5c22-1004-4d57-bf76-a25b4681f43f', private_innovation_within_regulatory_perimeter, conventional).
narrative_ontology:cs_axiom('9e7c5c22-1004-4d57-bf76-a25b4681f43f', foundational, reserve_backing_sufficiency_for_legitimacy).
narrative_ontology:cs_axiom_status(reserve_backing_sufficiency_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9e7c5c22-1004-4d57-bf76-a25b4681f43f', reserve_backing_sufficiency_for_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('9e7c5c22-1004-4d57-bf76-a25b4681f43f', bretton_woods_monetary_sovereignty).
narrative_ontology:cs_drift_state('9e7c5c22-1004-4d57-bf76-a25b4681f43f', post_fintech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e7c5c22-1004-4d57-bf76-a25b4681f43f', '').
narrative_ontology:cs_kernel_id(regulated_stablecoin_reading, digital_money_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulated_stablecoin_reading, payment_service_providers).
narrative_ontology:constraint_beneficiary(regulated_stablecoin_reading, underbanked_populations).
narrative_ontology:constraint_beneficiary(regulated_stablecoin_reading, cross_border_remittance_users).
narrative_ontology:constraint_victim(regulated_stablecoin_reading, central_bank_monetary_authority).
narrative_ontology:constraint_victim(regulated_stablecoin_reading, traditional_banking_sector).
narrative_ontology:constraint_vindicates(regulated_stablecoin_reading, private_innovation_efficiency_doctrine).
narrative_ontology:constraint_vindicates(regulated_stablecoin_reading, regulatory_perimeter_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Private firms that issue digital tokens backed by sovereign currency reserves held in regulated custodial accounts. They set redemption terms, manage reserve composition within regulatory bounds, and collect transaction fees and float income. They frame the arrangement as innovation-enabling financial inclusion while maintaining regulatory compliance.
narrative_ontology:constraint_stakeholder(regulated_stablecoin_reading, stablecoin_issuers, agenda_setter,
    powerful, biographical, mobile, global).

% Fintech platforms and payment processors that integrate stablecoins for faster, cheaper cross-border settlement. They benefit from reduced correspondent banking costs and real-time settlement without holding banking licenses. The regulatory framework legitimizes their use of private digital money.
narrative_ontology:constraint_stakeholder(regulated_stablecoin_reading, payment_service_providers, beneficiary,
    organized, biographical, mobile, global).

% Individuals without access to traditional banking who can hold stablecoins via mobile wallets. They gain payment functionality and store of value without minimum balances or credit checks. Their access depends entirely on the regulatory framework permitting private issuance.
narrative_ontology:constraint_stakeholder(regulated_stablecoin_reading, underbanked_populations, beneficiary,
    powerless, immediate, constrained, regional).

% Migrant workers and diaspora communities sending money across borders. Stablecoins reduce remittance costs from 6-8% to under 1% and enable near-instant settlement. Traditional wire transfer monopolies are bypassed.
narrative_ontology:constraint_stakeholder(regulated_stablecoin_reading, cross_border_remittance_users, beneficiary,
    moderate, immediate, constrained, global).

% Loses direct control over monetary aggregates as private stablecoins circulate alongside sovereign currency. Interest rate transmission weakens when significant economic activity settles in privately-issued money. They must regulate the framework but cannot prevent the leakage of monetary policy effectiveness to private issuers who capture seigniorage-like float income.
narrative_ontology:constraint_stakeholder(regulated_stablecoin_reading, central_bank_monetary_authority, payer,
    institutional, generational, constrained, national).

% Commercial banks face deposit flight as customers move funds to stablecoin wallets offering similar functionality without banking overhead. Their lending capacity shrinks as the deposit base erodes. They lobby for stricter regulation but cannot prevent the structural shift once the framework legitimizes private issuance.
narrative_ontology:constraint_stakeholder(regulated_stablecoin_reading, traditional_banking_sector, payer,
    institutional, generational, constrained, national).

% Design and enforce reserve requirements, redemption standards, and audit regimes for stablecoin issuers. They balance innovation goals against systemic risk and monetary sovereignty concerns. Their enforcement maintains the regulatory perimeter that legitimizes the arrangement.
narrative_ontology:constraint_stakeholder(regulated_stablecoin_reading, financial_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(regulated_stablecoin_reading, financial_regulators, observer).

% Study the transmission effects of private money issuance on monetary policy effectiveness, the stability of fractional reserve arrangements in digital form, and the distributional consequences of seigniorage capture by private issuers.
narrative_ontology:constraint_stakeholder(regulated_stablecoin_reading, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(regulated_stablecoin_reading, stablecoin_issuers).
narrative_ontology:fixing_cost_class(regulated_stablecoin_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of digital payment infrastructure: enables instant, low-cost, cross-border value transfer without requiring every participant to trust every other participant directly. The regulatory framework coordinates trust by mandating reserve backing and redemption guarantees.
% TRANSFER_FUNCTION: Moves transaction fee revenue and float income from traditional banking intermediaries to stablecoin issuers. Moves monetary policy transmission effectiveness from central banks to a hybrid system where private issuers partially control money supply growth. Moves payment access from banked-only populations to anyone with a mobile device.
% ABSENT_VOICES: Sovereign currency maximalists who argue all digital money should be central bank-issued are structurally excluded from the design process once the regulatory framework legitimizes private issuance. Unbanked populations in jurisdictions where regulators prohibit stablecoins have no voice in the global standard-setting.
% DISAPPEARANCE_RATIONALE: If the regulatory framework vanished overnight, existing stablecoin issuers would face immediate redemption runs without legal backing, payment service providers would revert to correspondent banking rails, cross-border remittance costs would spike back to pre-stablecoin levels, and central banks would regain full monetary policy transmission but lose the financial inclusion gains. The digital payments economy would reorganize around either unregulated crypto or sovereign CBDC alternatives.
% FOUNDING_PROBLEM: Early 2010s: cross-border payments were slow (3-5 days), expensive (6-8% fees), and inaccessible to unbanked populations. Cryptocurrency promised solutions but introduced volatility risk that made it unusable as money. The founding problem was: how to get cryptocurrency's payment efficiency without its price instability.
% FOUNDING_PROBLEM_CORROBORATION: Payment efficiency and financial inclusion gains are documented by World Bank remittance cost data (2015-2025) and fintech adoption studies from outside the stablecoin industry. Central banks and the Bank for International Settlements acknowledge the coordination function while contesting whether private issuance is necessary to achieve it.
narrative_ontology:disappearance_verdict(regulated_stablecoin_reading, world_rearranges).
narrative_ontology:founding_problem_status(regulated_stablecoin_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(regulated_stablecoin_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-15',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(regulated_stablecoin_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulated_stablecoin_reading_tests).
:- end_tests(regulated_stablecoin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) because the constraint genuinely solves coordination problems (cross-border payments, financial inclusion) while simultaneously enabling private capture of seigniorage-like float income and weakening central bank monetary policy transmission. Suppression is higher (0.58) because the framework's persistence depends on active regulatory enforcement preventing both unregulated crypto alternatives and sovereign CBDC displacement. Theater ratio is moderate-low (0.28): reserve audits and redemption guarantees are real regulatory functions, but a growing share of enforcement activity defends the legitimacy of private issuance itself rather than protecting users. The measurement series shows extraction and enforcement requirements rising over time as issuers scale and test regulatory boundaries, while theater increases as the framework matures into an established institutional arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (issuers, payment providers, underbanked users) should compute this as genuine coordination enabling previously impossible functionality. The victim seats (central bank, traditional banks) should compute it as enforced extraction where private parties capture public monetary functions. The observer seat (monetary economists) sees both: real coordination gains coupled with structural transfer of monetary policy effectiveness from public to private hands. The engine computes this divergence from the structural data; the claimed type (tangled_rope) asserts both functions coexist.
 *
 * DIRECTIONALITY LOGIC:
 *   Stablecoin issuers are structural beneficiaries (collect fees and float income, set terms within regulatory bounds — d near beneficiary end). Payment service providers and underbanked populations are beneficiaries (gain functionality, bear minimal direct costs — d near beneficiary end). Central bank monetary authority and traditional banking sector are victims (lose policy effectiveness and deposit base respectively, constrained exit — d near target end). Financial regulators sit near symmetric: they coordinate the framework but also bear the cost of enforcement complexity. The structural asymmetry is that coordination benefits flow to users and issuers while extraction (policy leakage, seigniorage capture) is borne by institutions that cannot exit without abandoning monetary sovereignty or banking franchise.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this as pure coordination (rope) by requiring acknowledgment of the victims (central bank monetary authority, traditional banking sector) and the asymmetric extraction (policy leakage, seigniorage capture). It also prevents mislabeling as pure extraction (snare) by requiring acknowledgment of genuine coordination function (payment efficiency, financial inclusion). The mandate (enable digital payment innovation) has not outlived its function, but the extraction component (private capture of monetary policy transmission) is growing as the framework scales.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_backing_sufficiency,
    'Are full sovereign currency reserves sufficient to prevent systemic runs, or does private issuance introduce fragility that no reserve ratio can eliminate?',
    'Stress test data from jurisdictions that have experienced stablecoin redemption surges, or natural experiments from bank-run-like events in stablecoin markets.',
    'If reserves are insufficient to prevent runs, the regulatory framework''s stability claim collapses and the constraint reclassifies toward snare (extraction without genuine coordination). If reserves hold under stress, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_backing_sufficiency, empirical, 'Whether reserve backing eliminates systemic fragility or merely masks it.').

omega_variable(
    monetary_policy_transmission_threshold,
    'At what scale of stablecoin circulation does monetary policy transmission degrade enough to constitute a sovereignty loss rather than a marginal efficiency gain?',
    'Central bank empirical studies measuring interest rate pass-through effectiveness as stablecoin market share grows, or cross-country comparison of jurisdictions with different stablecoin penetration rates.',
    'If transmission degradation is negligible below some threshold, the extraction is bounded and the tangled_rope classification holds. If degradation is linear with scale, the constraint becomes increasingly extractive as it succeeds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monetary_policy_transmission_threshold, empirical, 'Whether monetary policy leakage is a marginal cost or a structural sovereignty transfer.').

omega_variable(
    private_vs_public_issuance_necessity,
    'Is private issuance structurally necessary to achieve the coordination gains (payment efficiency, financial inclusion), or could a sovereign CBDC deliver the same benefits without the extraction?',
    'Natural experiment from jurisdictions that deploy CBDCs with similar functionality to stablecoins, comparing adoption rates, transaction costs, and inclusion outcomes.',
    'If CBDCs can match stablecoin functionality, the private issuance framework is revealed as extraction riding on coordination that could be achieved publicly. If CBDCs cannot match (due to institutional constraints, slower innovation, or political barriers), the private framework''s coordination function is validated as irreplaceable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_vs_public_issuance_necessity, conceptual, 'Whether the coordination and extraction components are structurally separable.').

omega_variable(
    kernel_reading_under_determination,
    'Is the regulated stablecoin reading the only defensible framing of digital money legitimacy, or do the sovereign CBDC and crypto permissionless readings represent equally coherent but incompatible commitments?',
    'Cross-jurisdictional comparison of digital money frameworks: if different readings produce stable, functional systems in different contexts, the kernel is genuinely contested. If one reading systematically outperforms others on coordination and extraction metrics, that reading is structurally superior.',
    'If the kernel is genuinely contested, classification divergence across readings is expected and legitimate. If one reading is structurally superior, divergence indicates misclassification in the inferior readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Whether the digital money legitimacy kernel admits multiple stable readings or has a single correct framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulated_stablecoin_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regu_tr_t0, regulated_stablecoin_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(regu_tr_t0, observed).
narrative_ontology:measurement(regu_tr_t5, regulated_stablecoin_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(regu_tr_t5, observed).
narrative_ontology:measurement(regu_tr_t10, regulated_stablecoin_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(regu_tr_t10, observed).
narrative_ontology:measurement(regu_tr_t15, regulated_stablecoin_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(regu_tr_t15, observed).
narrative_ontology:measurement(regu_tr_t20, regulated_stablecoin_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(regu_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(regu_be_t0, regulated_stablecoin_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(regu_be_t0, observed).
narrative_ontology:measurement(regu_be_t5, regulated_stablecoin_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(regu_be_t5, observed).
narrative_ontology:measurement(regu_be_t10, regulated_stablecoin_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(regu_be_t10, observed).
narrative_ontology:measurement(regu_be_t15, regulated_stablecoin_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement_basis(regu_be_t15, observed).
narrative_ontology:measurement(regu_be_t20, regulated_stablecoin_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(regu_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(regu_su_t0, regulated_stablecoin_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(regu_su_t0, observed).
narrative_ontology:measurement(regu_su_t5, regulated_stablecoin_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(regu_su_t5, observed).
narrative_ontology:measurement(regu_su_t10, regulated_stablecoin_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(regu_su_t10, observed).
narrative_ontology:measurement(regu_su_t15, regulated_stablecoin_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(regu_su_t15, observed).
narrative_ontology:measurement(regu_su_t20, regulated_stablecoin_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(regu_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulated_stablecoin_reading, resource_allocation).
narrative_ontology:affects_constraint(regulated_stablecoin_reading, sovereign_cbdc_reading).
narrative_ontology:affects_constraint(regulated_stablecoin_reading, crypto_permissionless_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the digital_money_legitimacy kernel. The regulated_stablecoin_reading permits private issuance within regulatory bounds; sovereign_cbdc_reading restricts issuance to central banks; crypto_permissionless_reading grounds legitimacy in cryptographic proof rather than regulatory permission. Each reading has different beneficiary/victim structures and different ε values. They are linked via network.affects_constraints because the success or failure of one reading creates structural pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
