% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__consumer_holdings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__consumer_holdings_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__consumer_holdings_reading
 *   human_readable: Consumer-Holdings Boundary for Digital Money Emergence (E-Money Directive Reading)
 *   domain: monetary_economics/financial_regulation/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the consumer-holdings reading of the digital
 *   money emergence kernel: money is deemed to have digitally emerged only
 *   when individuals could directly hold and transact digital instruments
 *   outside a traditional bank deposit account, crystallized legally by the
 *   2000 EU Electronic Money Directive (EMD) following a decade of e-purse
 *   experimentation (Mondex, Proton). This is the latest of the three
 *   candidate boundaries in the kernel and the one most entangled with
 *   contemporary regulatory and statistical categories (the M4/M5
 *   distinction). It is deliberately NOT the conceptualization boundary
 *   (1960s-1985, Chaum) or the infrastructure boundary (1967-1977,
 *   ATMs/ACH/SWIFT) — those are separate constraints with their own ε and
 *   stakeholder structures, linked via network.affects_constraints, not
 *   folded into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, 0.52).
domain_priors:suppression_score(digital_money_emergence_boundary__consumer_holdings_reading, 0.4).
domain_priors:theater_ratio(digital_money_emergence_boundary__consumer_holdings_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__consumer_holdings_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__consumer_holdings_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__consumer_holdings_reading, "Consumer-Holdings Boundary for Digital Money Emergence (E-Money Directive Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__consumer_holdings_reading, "monetary_economics/financial_regulation/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__consumer_holdings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__consumer_holdings_reading, 'beefb316-619b-40ed-8e75-96ac865d9d93').
narrative_ontology:cs_kernel_codification('beefb316-619b-40ed-8e75-96ac865d9d93', distributed).
narrative_ontology:cs_authority_grounding('beefb316-619b-40ed-8e75-96ac865d9d93', extraction).
narrative_ontology:cs_interpretation_layer_present('beefb316-619b-40ed-8e75-96ac865d9d93').
narrative_ontology:cs_reading_relation('beefb316-619b-40ed-8e75-96ac865d9d93', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('beefb316-619b-40ed-8e75-96ac865d9d93', digital_money_emergence_boundary__infrastructure_reading, influences).
narrative_ontology:cs_axiom('beefb316-619b-40ed-8e75-96ac865d9d93', foundational, moneyness_requires_direct_unmediated_individual_holding).
narrative_ontology:cs_axiom_status(moneyness_requires_direct_unmediated_individual_holding, holdable).
narrative_ontology:cs_axiom_grounding('beefb316-619b-40ed-8e75-96ac865d9d93', moneyness_requires_direct_unmediated_individual_holding, conventional).
narrative_ontology:cs_axiom('beefb316-619b-40ed-8e75-96ac865d9d93', secondary, legal_licensing_recognition_is_constitutive_not_merely_descriptive).
narrative_ontology:cs_axiom_status(legal_licensing_recognition_is_constitutive_not_merely_descriptive, holdable).
narrative_ontology:cs_axiom_grounding('beefb316-619b-40ed-8e75-96ac865d9d93', legal_licensing_recognition_is_constitutive_not_merely_descriptive, instrumental).
narrative_ontology:cs_reference_frame('beefb316-619b-40ed-8e75-96ac865d9d93', pre_2000_undefined_moneyness_status).
narrative_ontology:cs_drift_state('beefb316-619b-40ed-8e75-96ac865d9d93', post_emd_licensing_regime_2024, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('beefb316-619b-40ed-8e75-96ac865d9d93', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, e_money_regulatory_authorities).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__consumer_holdings_reading, fintech_emi_issuers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, unlicensed_prepaid_scheme_operators).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, e_purse_holders_pre_2000).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__consumer_holdings_reading, cross_border_remittance_users).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, legal_tender_requires_direct_individual_holding).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__consumer_holdings_reading, m4_m5_distinction_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The EU (and analogous national regulators elsewhere) drew the legal line in the 2000 Electronic Money Directive (EMD): a digital instrument becomes 'money' only when a consumer can hold it directly, outside a bank deposit account, redeemable at par. This act created a whole new licensing category (Electronic Money Institution, EMI) that the authorities alone administer, and it is the authorities who benefit from the resulting jurisdiction over an entire new class of firms and the statistical/legal categories (M4 vs M5-style distinctions) that only make sense once this boundary is drawn.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, e_money_regulatory_authorities, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__consumer_holdings_reading, e_money_regulatory_authorities, beneficiary).

% Licensed e-money institutions (early e-purse schemes, later PayPal-style and mobile wallet issuers) gained a legally recognized product category distinct from banking, letting them issue redeemable stored value without a full banking license. This regulatory carve-out is the foundation of their business model; they lobby actively to keep the consumer-holdings line exactly where it is, since a narrower or broader definition would either exclude their products from 'money' status or subject them to full banking capital requirements.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, fintech_emi_issuers, beneficiary,
    organized, biographical, arbitrage, continental).

% Operators of prepaid card and voucher schemes that existed before or outside the EMD framework were forced either to become licensed EMIs (absorbing compliance cost) or to be reclassified as non-money instruments with reduced consumer trust and market access. The consumer-holdings boundary determined which of their products counted as 'digital money' at all, with direct commercial consequences they did not choose.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, unlicensed_prepaid_scheme_operators, payer,
    moderate, biographical, constrained, national).

% Early adopters of 1990s e-purse cards (e.g. Mondex, Proton) transacted with instruments that, prior to 2000, had no settled legal status as money — leaving them exposed to issuer insolvency without deposit-guarantee-style protections. The later regulatory boundary retroactively defines what they were holding, but did not exist to protect them at the time; their exposure was resolved by definition, not by remedy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, e_purse_holders_pre_2000, payer,
    powerless, immediate, trapped, national).

% Migrant workers and others sending small-value cross-border payments through e-money rails bear the compliance costs (KYC, transaction limits, redemption friction) that the consumer-holdings/EMI licensing regime imposes on issuers and passes through to users, while having no voice in how the boundary between 'e-money' and 'bank money' is drawn.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, cross_border_remittance_users, payer,
    powerless, biographical, constrained, global).

% Traditional deposit-taking banks were largely bystanders to the drafting of the consumer-holdings boundary, which created a competing category of near-money instruments outside their balance sheets. They would have preferred a boundary that folded e-money into ordinary deposit regulation (and their competitive moat), but were not the primary drafters of the EMD framework.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, commercial_banks, excluded,
    institutional, generational, mobile, continental).

% Study when 'digital money' can be said to have emerged, comparing this consumer-holdings boundary against the conceptualization boundary (1960s-1985) and the infrastructure boundary (1967-1977). They note this reading is the most legally precise but also the most self-serving for the institutions that drew it, since the boundary conveniently creates the jurisdiction those institutions then occupy.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__consumer_holdings_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty about which digital instruments are 'money' subject to redemption guarantees and issuer capital requirements, versus which are mere accounting entries or bank deposits — enabling consumer protection rules and monetary statistics (M4/M5) to attach to the right category of instrument.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction and market-definition power to the bodies that drew the boundary (EU/national regulators) and moves competitive advantage to firms that qualify as licensed e-money issuers under the new category, while imposing compliance costs on prior unlicensed schemes and passing friction costs to end users of e-money products.
% ABSENT_VOICES: Commercial banks who lost exclusive claim to deposit-like instruments were marginal to the drafting process; ordinary e-purse holders from the 1990s whose instruments were retroactively defined had no representation when the boundary was set in 2000; cross-border remittance users bearing downstream compliance costs are structurally absent from EMI licensing debates.
% DISAPPEARANCE_RATIONALE: If the consumer-holdings legal boundary vanished, EMI-licensed fintech firms would lose their distinct regulatory category and either fold into banking regulation or operate in a legal gray zone; regulators would lose the jurisdictional apparatus built around it. Monetary historians would say the underlying economic reality (people already held and transacted digital value via e-purses) predates and does not depend on the legal boundary — hence contested: the legal-institutional world rearranges sharply, but the money-like phenomenon it describes would persist regardless.
% FOUNDING_PROBLEM: Regulators needed to determine whether pre-paid electronic value (e-purses, early digital wallets) constituted 'money' subject to consumer protection and monetary policy, or merely a private accounting claim against an issuer — a gap 1990s e-purse schemes exposed by operating with no settled legal status.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and national central banks (EMD drafters, corroborating from inside the regulatory apparatus) maintain the boundary remains necessary as e-money volumes grow. Independent monetary economists and historians outside the regulatory and fintech-issuer beneficiary set argue the underlying economic phenomenon (consumer-held digital value) was already real by the mid-1990s and that the 2000 legal boundary mainly formalized jurisdiction rather than solving a genuinely unresolved problem — supporting a contested status rather than a settled live/dead read.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__consumer_holdings_reading, contested).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__consumer_holdings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__consumer_holdings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__consumer_holdings_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__consumer_holdings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__consumer_holdings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52: the boundary genuinely solves a real classification problem (is a stored-value instrument 'money' subject to redemption guarantee?) but the resulting EMI licensing regime also creates rents for regulators (new jurisdiction) and incumbent licensed issuers (competitive moat against unlicensed schemes), while imposing compliance costs on prior unlicensed operators and downstream users. Suppression (0.4) reflects active enforcement of the licensing boundary against non-compliant prepaid schemes, but is well below a snare-level suppression because genuine alternatives (bank accounts, cash, unregulated tokens in some jurisdictions) persist. Theater ratio (0.3) captures that some of the boundary's apparatus (periodic reclassification exercises, consultation processes) is more performative than functionally necessary once the core EMI category was established.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory authorities and fintech EMI issuers are structural beneficiaries: the former gain durable jurisdiction and statistical categories, the latter gain a licensing status that lets them compete with banks without full banking capital requirements. Unlicensed prepaid operators, 1990s e-purse holders (retroactively defined, not protected at the time), and cross-border remittance users are targets — the first two bear direct compliance/definitional costs, the third bears downstream friction costs passed through the licensing chain. Commercial banks are excluded rather than targeted: they lost exclusivity but were not drafters of the boundary and retain substantial market power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal uncertainty over the status of stored electronic value) was real in the 1990s given e-purse issuer insolvency exposure with no deposit-guarantee-equivalent protection. Whether that problem remains live in 2024 is contested: e-money volumes and use cases have expanded well past the original e-purse use case, arguably validating continued need for the boundary, but critics outside the regulator/issuer beneficiary set argue the boundary's practical function has shifted from consumer protection toward jurisdictional and competitive gatekeeping. Classifying this as tangled_rope rather than snare or rope prevents both an overclaim of pure extraction (the redemption-guarantee coordination function is real and protects consumers today) and an overclaim of pure coordination (the boundary also manufactures rents for the parties who drew it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_holdings_vs_infrastructure_priority,
    'Is the consumer-holdings boundary the correct locus of ''emergence,'' or does it merely mark the point at which pre-existing infrastructure-enabled digital value was given legal recognition — meaning the infrastructure_reading is doing the real explanatory work and this reading is downstream legal formalization?',
    'Compare economic usage data: if consumers were already functionally holding and transacting digital value via 1990s e-purses at meaningful scale before 2000, the legal boundary is formalization rather than emergence; if usage only became significant after legal recognition (network effects from redemption guarantees), the boundary reading has independent causal weight.',
    'If the boundary is mere formalization, this reading''s claimed beneficiary structure (regulators/issuers gaining from drawing the line) is strengthened — the boundary''s main effect is redistributive/jurisdictional rather than constitutive of the phenomenon. If the boundary has independent causal weight (redemption guarantees genuinely enabled broader adoption), the coordination function is stronger than a pure gatekeeping read would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_holdings_vs_infrastructure_priority, empirical, 'Whether legal recognition is constitutive of digital money''s emergence or merely formalizes a prior infrastructure-driven fact.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the three sibling readings (conceptualization, infrastructure, consumer_holdings) disagree — is it about WHEN money emerged, or about WHAT PROPERTY must be present for an instrument to count as money at all (thinkability, transferability, or individual holdability)?',
    'This is a conceptual/definitional dispute, not resolvable by further data alone: it turns on which property theorists and regulators treat as necessary and sufficient for ''moneyness.'' Documenting the disagreement site (the property, not the date) clarifies that the three readings are not merely different historical estimates of one fact but different definitions of the kernel concept itself.',
    'If the disagreement is about the necessary property (holdability vs. transferability vs. thinkability), the three readings are genuinely non-competing framings that can coexist as parallel legal/economic/historical narratives rather than needing adjudication into a single ''true'' emergence date.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'The kernel dispute is located in which property of ''moneyness'' is treated as necessary, not merely in dating a single historical event.').

omega_variable(
    m4_m5_necessity_or_artifact,
    'Is the M4/M5 monetary-aggregate distinction a necessary analytical consequence of e-money''s emergence (a real gap in existing monetary statistics that had to be closed), or is it primarily an artifact created to justify and staff the regulatory category this reading''s beneficiaries administer?',
    'Examine whether central banks outside the EU (jurisdictions without an EMD-equivalent licensing regime) independently developed comparable statistical distinctions for e-money versus bank deposits absent the EU''s legal framework.',
    'Independent convergence on the distinction elsewhere would support it as a genuine analytical necessity (rope-leaning); if the distinction is largely confined to EMD-adjacent regulatory reporting, it supports the tangled_rope reading that the statistical apparatus partly exists to legitimize the jurisdictional boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(m4_m5_necessity_or_artifact, empirical, 'Whether the M4/M5 distinction is analytically necessary or a jurisdictional byproduct of the EMD boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__consumer_holdings_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(digi_tr_t1996, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(digi_tr_t2008, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2008, 0.27).
narrative_ontology:measurement(digi_tr_t2016, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2016, 0.29).
narrative_ontology:measurement(digi_tr_t2024, digital_money_emergence_boundary__consumer_holdings_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(digi_be_t1996, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 1996, 0.28).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(digi_be_t2008, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2008, 0.46).
narrative_ontology:measurement(digi_be_t2016, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2016, 0.5).
narrative_ontology:measurement(digi_be_t2024, digital_money_emergence_boundary__consumer_holdings_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(digi_su_t1996, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 1996, 0.22).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(digi_su_t2008, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(digi_su_t2016, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2016, 0.39).
narrative_ontology:measurement(digi_su_t2024, digital_money_emergence_boundary__consumer_holdings_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__consumer_holdings_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__consumer_holdings_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__consumer_holdings_reading, digital_money_emergence_boundary__infrastructure_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'when digital money emerged' per the epsilon-invariance principle. Each sibling reading (conceptualization: 1960s-1985; infrastructure: 1967-1977; consumer_holdings: 1990s-2000) has a distinct epsilon, distinct beneficiary/victim structure, and distinct claimed_type, because they answer structurally different questions (what property counts as 'moneyness') rather than the same question measured differently. The infrastructure_reading plausibly influences this reading (infrastructure created the conditions for consumer-holdable products to be technically possible before they were legally recognized), which is why network.affects_constraints links infrastructure_reading as an upstream influence rather than treating all three as independent siblings with no causal ordering.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
