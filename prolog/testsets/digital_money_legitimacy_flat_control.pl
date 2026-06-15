% ============================================================================
% CONSTRAINT STORY: digital_money_legitimacy_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_legitimacy_flat_control, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_legitimacy_flat_control
 *   human_readable: Digital Money Legitimacy in Sovereign Monetary Systems
 *   domain: monetary_policy/digital_currency/behavioral_economics
 *
 * SUMMARY:
 *   Sovereign monetary systems define digital money legitimacy through legal
 *   tender laws, payment system regulations, and licensing regimes that
 *   determine which digital instruments can operate legally. The constraint
 *   is presented as necessary for monetary sovereignty and financial
 *   stability; critics argue it protects incumbent financial institutions
 *   from competition and excludes populations that alternative digital money
 *   systems could serve. The claim/metric independence is deliberate: the
 *   constraint is claimed as tangled_rope (genuine coordination function with
 *   asymmetric extraction) while metrics describe substantially extractive
 *   operation with rising enforcement requirements over time.
 *
 * KEY AGENTS:
 *   - central_banks: Primary agenda-setter (institutional/arbitrage) — defines legitimacy standards, issues currency, regulates payment systems
 *   - commercial_banking_sector: Primary beneficiary (institutional/constrained) — protected intermediary position, deposit base preserved
 *   - cryptocurrency_users: Primary payer (organized/constrained) — forced conversion costs, legal uncertainty, limited acceptance
 *   - unbanked_populations: Secondary payer (powerless/trapped) — excluded from legitimate digital money, dependent on cash or risky informal systems
 *   - monetary_economists: Analytical observer — documents coordination benefits and extraction costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_legitimacy_flat_control, 0.68).
domain_priors:suppression_score(digital_money_legitimacy_flat_control, 0.72).
domain_priors:theater_ratio(digital_money_legitimacy_flat_control, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_legitimacy_flat_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_money_legitimacy_flat_control, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_money_legitimacy_flat_control, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_legitimacy_flat_control, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(digital_money_legitimacy_flat_control, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_legitimacy_flat_control, tangled_rope).
narrative_ontology:human_readable(digital_money_legitimacy_flat_control, "Digital Money Legitimacy in Sovereign Monetary Systems").
narrative_ontology:topic_domain(digital_money_legitimacy_flat_control, "monetary_policy/digital_currency/behavioral_economics").

domain_priors:requires_active_enforcement(digital_money_legitimacy_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_legitimacy_flat_control, '79050c19-fc7b-4600-8a50-9a8a0d554dd0').
narrative_ontology:cs_kernel_codification('79050c19-fc7b-4600-8a50-9a8a0d554dd0', formalized).
narrative_ontology:cs_authority_grounding('79050c19-fc7b-4600-8a50-9a8a0d554dd0', lineage).
narrative_ontology:cs_interpretation_layer_present('79050c19-fc7b-4600-8a50-9a8a0d554dd0').
narrative_ontology:cs_created_at('79050c19-fc7b-4600-8a50-9a8a0d554dd0', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(digital_money_legitimacy_flat_control, digital_money_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_legitimacy_flat_control, central_banks).
narrative_ontology:constraint_beneficiary(digital_money_legitimacy_flat_control, commercial_banking_sector).
narrative_ontology:constraint_beneficiary(digital_money_legitimacy_flat_control, payment_processors).
narrative_ontology:constraint_victim(digital_money_legitimacy_flat_control, cryptocurrency_users).
narrative_ontology:constraint_victim(digital_money_legitimacy_flat_control, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_legitimacy_flat_control, cross_border_remitters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_legitimacy_flat_control, fintech_innovators).
narrative_ontology:constraint_vindicates(digital_money_legitimacy_flat_control, state_monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(digital_money_legitimacy_flat_control, financial_stability_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define what counts as legal tender and legitimate payment instruments within their jurisdictions. They issue currency, set monetary policy, and regulate payment systems. Digital money legitimacy is framed as necessary for monetary sovereignty, financial stability, and anti-money-laundering enforcement. They control the licensing regime that determines which digital instruments can operate legally.
narrative_ontology:constraint_stakeholder(digital_money_legitimacy_flat_control, central_banks, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from legitimacy rules that channel digital payments through regulated banking infrastructure. The constraint protects their intermediary position: digital money that bypasses banks is delegitimized or heavily regulated, preserving deposit bases and transaction fee revenue. They lobby for legitimacy frameworks that require banking licenses for digital payment services.
narrative_ontology:constraint_stakeholder(digital_money_legitimacy_flat_control, commercial_banking_sector, beneficiary,
    institutional, generational, constrained, national).

% Operate within the legitimacy framework by obtaining licenses and integrating with banking infrastructure. They benefit from barriers to entry that the legitimacy regime creates: compliance costs are fixed investments that favor established players. Alternative payment rails that lack official legitimacy cannot compete on equal terms.
narrative_ontology:constraint_stakeholder(digital_money_legitimacy_flat_control, payment_processors, beneficiary,
    powerful, biographical, mobile, global).

% Use decentralized digital currencies that operate outside sovereign legitimacy frameworks. They face legal uncertainty, limited merchant acceptance, tax reporting burdens, and in some jurisdictions outright prohibition. The constraint forces them to convert to legitimate money for most economic activity, paying conversion fees and accepting surveillance. Exit means abandoning digital currency use entirely.
narrative_ontology:constraint_stakeholder(digital_money_legitimacy_flat_control, cryptocurrency_users, payer,
    organized, biographical, constrained, global).

% Lack access to banking infrastructure required for legitimate digital money. Mobile money and informal digital payment systems that could serve them are often delegitimized or heavily restricted. The legitimacy framework assumes banking access as a prerequisite, leaving them dependent on cash or informal systems that carry legal risk and limited functionality.
narrative_ontology:constraint_stakeholder(digital_money_legitimacy_flat_control, unbanked_populations, payer,
    powerless, immediate, trapped, regional).

% Send money across borders using whatever channels are available. Legitimate channels impose high fees and slow settlement; alternative digital money systems offer better terms but face legitimacy barriers that make them legally risky or unavailable. The constraint forces them into expensive legitimate channels or illegal informal ones.
narrative_ontology:constraint_stakeholder(digital_money_legitimacy_flat_control, cross_border_remitters, payer,
    moderate, immediate, constrained, global).

% Develop new digital payment technologies that could reduce costs and increase access. They face legitimacy barriers that require expensive licensing, banking partnerships, and compliance infrastructure before they can operate. Many innovations are foreclosed entirely because they cannot fit the legitimacy framework's assumptions about institutional intermediaries.
narrative_ontology:constraint_stakeholder(digital_money_legitimacy_flat_control, fintech_innovators, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(digital_money_legitimacy_flat_control, fintech_innovators, excluded).

% Study the effects of digital money legitimacy regimes on financial inclusion, monetary policy transmission, and payment system efficiency. They document the coordination benefits of unified monetary systems and the extraction costs of regulatory capture by incumbent financial institutions.
narrative_ontology:constraint_stakeholder(digital_money_legitimacy_flat_control, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified standard for what payment instruments are legally enforceable, trusted by merchants, and integrated with the broader financial system. Solves the collective action problem of payment acceptance and reduces fraud risk through regulated intermediaries.
% TRANSFER_FUNCTION: Channels digital payment activity through licensed institutions that pay regulatory compliance costs, maintain capital reserves, and remit transaction data to authorities. Transfers economic surplus from users of alternative digital money systems to incumbent financial institutions via forced intermediation and compliance overhead.
% ABSENT_VOICES: Cryptocurrency developers, informal economy participants, and populations in jurisdictions with unstable sovereign currencies would argue for legitimacy frameworks that recognize non-state digital money. They are structurally excluded from the regulatory process that defines legitimacy.
% DISAPPEARANCE_RATIONALE: If legitimacy constraints vanished, multiple competing digital money systems would operate simultaneously, payment routing would fragment across state and non-state rails, central banks would lose monetary policy transmission channels, and the banking sector's deposit base would face direct competition from digital alternatives. The financial system would reorganize around whatever payment instruments users and merchants actually preferred.
% FOUNDING_PROBLEM: Pre-digital monetary systems faced counterfeiting, fraud, and coordination failures where different regions or institutions issued incompatible currencies. Sovereign money legitimacy solved this by establishing a single trusted issuer and legal tender framework.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and banking regulators attest the founding problem remains live, citing fraud risk and monetary stability concerns. Cryptocurrency advocates, financial inclusion researchers, and cross-border payment analysts attest that digital technology has changed the problem structure: cryptographic verification reduces counterfeiting risk, and network effects can establish trust without state backing. Independent research on mobile money in developing economies demonstrates coordination without sovereign legitimacy.
narrative_ontology:disappearance_verdict(digital_money_legitimacy_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_legitimacy_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_legitimacy_flat_control, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-15',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(digital_money_legitimacy_flat_control, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_legitimacy_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_legitimacy_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_legitimacy_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68 at interval end) because legitimacy rules channel activity through licensed intermediaries whose costs exceed the marginal cost of digital payment processing. The gap between coordination cost and actual extraction has widened as digital technology reduced technical barriers while regulatory barriers remained high. Suppression is high (0.72) because the constraint's persistence depends on actively prohibiting or delegitimizing alternative digital money systems that users would otherwise adopt. Theater ratio is moderate (0.42): anti-money-laundering and consumer protection functions are real, but a growing share of enforcement activity defends incumbent positions rather than addressing the founding problems. Accessibility collapse is moderate (0.48) because alternative digital money systems continue to operate despite delegitimization, though with significant legal and practical barriers. Resistance is substantial (0.61) because cryptocurrency adoption, mobile money innovation, and cross-border payment demand all push against the legitimacy framework.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute the constraint as coordination with manageable overhead; the payer seats should compute it as substantially extractive. The powerless/trapped seat (unbanked populations) should show the highest effective extraction because they face both exclusion from legitimate systems and legal risk from alternatives. The organized/constrained seat (cryptocurrency users) should show high but slightly lower extraction because they have some ability to operate in gray areas.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and commercial banks are structural beneficiaries: they set the rules and collect from the arrangement through preserved market positions and regulatory rents. Cryptocurrency users, unbanked populations, and cross-border remitters are targets: they pay through forced intermediation, exclusion, or legal risk. Payment processors occupy a mixed position: they benefit from barriers to entry but also bear compliance costs. The engine will compute different effective extraction for each seat based on these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy characteristics: the founding problem (counterfeiting, fraud, coordination failure in physical currency systems) has been substantially transformed by digital technology, but the legitimacy framework persists in a form that increasingly serves incumbent protection rather than the original coordination function. The measurement series shows extraction accumulating over time as enforcement intensifies while the underlying coordination problem becomes less severe. However, genuine coordination benefits remain: unified legal tender does solve real collective action problems, and some regulatory oversight prevents fraud. The tangled_rope classification captures this: real coordination function, asymmetric extraction, active enforcement required to maintain the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_vs_incumbent_protection,
    'How much of the legitimacy framework''s restrictiveness is necessary for monetary coordination versus protecting incumbent financial institutions from competition?',
    'Natural experiments from jurisdictions that liberalize digital money regulations: if coordination and stability outcomes hold while legitimacy barriers fall, the excess restriction is incumbent protection. Comparative analysis of mobile money systems in developing economies that operate with lighter regulatory frameworks.',
    'If most restriction is incumbent protection, the constraint is closer to pure snare than tangled_rope. If coordination genuinely requires most of the current framework, the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_incumbent_protection, empirical, 'Whether legitimacy restrictions serve coordination or capture.').

omega_variable(
    monetary_sovereignty_vs_user_sovereignty,
    'Is state control over money legitimacy a necessary feature of monetary sovereignty, or can monetary systems coordinate without sovereign legitimacy frameworks?',
    'Conceptual analysis of what monetary sovereignty requires: does it demand exclusive control over legitimacy, or only over issuance and legal tender for tax payments? Historical analysis of periods with competing private currencies.',
    'If monetary sovereignty is compatible with competing legitimacy frameworks, the current constraint is more restrictive than its stated justification requires. If sovereignty requires exclusive legitimacy control, the extraction is inherent to the sovereignty claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monetary_sovereignty_vs_user_sovereignty, conceptual, 'Whether monetary sovereignty requires legitimacy monopoly.').

omega_variable(
    technology_shift_obsolescence,
    'Has digital technology fundamentally changed the coordination problem that sovereign money legitimacy was designed to solve?',
    'Technical analysis of whether cryptographic verification, distributed ledgers, and network effects can provide the trust and coordination functions that previously required state backing. Empirical observation of cryptocurrency and mobile money adoption patterns.',
    'If technology has obsoleted the founding problem, the constraint is a zombie institution persisting through inertia and incumbent power. If the founding problem remains structurally similar, the constraint''s coordination function is still live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_shift_obsolescence, empirical, 'Whether digital technology has obsoleted the founding coordination problem.').

omega_variable(
    financial_inclusion_tradeoff,
    'Does the legitimacy framework''s emphasis on regulated intermediaries necessarily exclude populations that alternative digital money systems could serve, or could the framework be reformed to accommodate both coordination and inclusion?',
    'Policy experiments with tiered legitimacy frameworks that allow lighter-touch regulation for systems serving unbanked populations. Analysis of mobile money regulatory approaches in East Africa versus other regions.',
    'If inclusion and coordination are compatible, the current exclusion is a design choice favoring incumbents. If they are in tension, the extraction on unbanked populations is an unavoidable cost of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_inclusion_tradeoff, preference, 'Whether coordination and financial inclusion are compatible goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_legitimacy_flat_control, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_legitimacy_flat_control, theater_ratio, 0, 0.22).
narrative_ontology:measurement(digi_tr_t5, digital_money_legitimacy_flat_control, theater_ratio, 5, 0.27).
narrative_ontology:measurement(digi_tr_t10, digital_money_legitimacy_flat_control, theater_ratio, 10, 0.32).
narrative_ontology:measurement(digi_tr_t15, digital_money_legitimacy_flat_control, theater_ratio, 15, 0.36).
narrative_ontology:measurement(digi_tr_t20, digital_money_legitimacy_flat_control, theater_ratio, 20, 0.39).
narrative_ontology:measurement(digi_tr_t25, digital_money_legitimacy_flat_control, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_legitimacy_flat_control, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(digi_be_t5, digital_money_legitimacy_flat_control, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(digi_be_t10, digital_money_legitimacy_flat_control, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(digi_be_t15, digital_money_legitimacy_flat_control, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(digi_be_t20, digital_money_legitimacy_flat_control, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(digi_be_t25, digital_money_legitimacy_flat_control, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_legitimacy_flat_control, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(digi_su_t5, digital_money_legitimacy_flat_control, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(digi_su_t10, digital_money_legitimacy_flat_control, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(digi_su_t15, digital_money_legitimacy_flat_control, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(digi_su_t20, digital_money_legitimacy_flat_control, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(digi_su_t25, digital_money_legitimacy_flat_control, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_legitimacy_flat_control, resource_allocation).
narrative_ontology:affects_constraint(digital_money_legitimacy_flat_control, central_bank_digital_currency_design).
narrative_ontology:affects_constraint(digital_money_legitimacy_flat_control, cryptocurrency_regulatory_status).
narrative_ontology:affects_constraint(digital_money_legitimacy_flat_control, cross_border_payment_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
