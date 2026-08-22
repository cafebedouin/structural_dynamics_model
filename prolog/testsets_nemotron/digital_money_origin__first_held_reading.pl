% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: First-Held Digital Money Origin (Individual Practical Holding)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'first_held_reading' of the
 *   digital_money_origin kernel: digital money emerged when individuals first
 *   held non-physical monetary instruments as practical stores of value. This
 *   reading dates emergence to the mid-1990s (stored-value cards, Mondex,
 *   early e-money pilots) rather than the earlier conceptual moment
 *   (cryptographic cash proposals, 1980s) or the later regulatory recognition
 *   moment (central bank digital currency frameworks, 2010s). The constraint
 *   set includes implementation barriers (connectivity, device access,
 *   literacy) and network effects (a digital monetary instrument only
 *   functions as money when held by enough individuals to create liquidity).
 *   Beneficiaries are early adopters with infrastructure access and the
 *   operators who capture the resulting flows. Victims are populations
 *   excluded from the holding infrastructure — their monetary agency is
 *   degraded as the system reorients around digital holding.
 *
 * KEY AGENTS:
 *   - early_adopter_holders: Primary beneficiaries (moderate/mobile) — gained new monetary capabilities
 *   - payment_rail_operators: Primary agenda-setters (institutional/arbitrage) — set rules, capture rents
 *   - financial_intermediaries_with_digital_infrastructure: Beneficiaries and agenda-setters (institutional/arbitrage) — absorbed holding activity, shaped regulation
 *   - unbanked_populations: Primary payers/victims (powerless/trapped) — bear exclusion costs
 *   - rural_communities_without_connectivity: Primary payers/victims (powerless/trapped) — infrastructure absence locks them out
 *   - cash_dependent_informal_economy_participants: Victims (powerless/constrained) — delegitimized and surveilled
 *   - monetary_historians: Observers (analytical/analytical) — date regime transitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.38).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.42).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "First-Held Digital Money Origin (Individual Practical Holding)").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '2c15a9b1-afe2-4f5f-b874-ecf95a005911').
narrative_ontology:cs_kernel_codification('2c15a9b1-afe2-4f5f-b874-ecf95a005911', distributed).
narrative_ontology:cs_authority_grounding('2c15a9b1-afe2-4f5f-b874-ecf95a005911', practice).
narrative_ontology:cs_reading_relation('2c15a9b1-afe2-4f5f-b874-ecf95a005911', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c15a9b1-afe2-4f5f-b874-ecf95a005911', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('2c15a9b1-afe2-4f5f-b874-ecf95a005911', foundational, holding_is_necessary_for_monetary_emergence).
narrative_ontology:cs_axiom_status(holding_is_necessary_for_monetary_emergence, holdable).
narrative_ontology:cs_axiom_grounding('2c15a9b1-afe2-4f5f-b874-ecf95a005911', holding_is_necessary_for_monetary_emergence, conventional).
narrative_ontology:cs_axiom('2c15a9b1-afe2-4f5f-b874-ecf95a005911', secondary, network_effects_constitute_monetary_legitimacy).
narrative_ontology:cs_axiom_status(network_effects_constitute_monetary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2c15a9b1-afe2-4f5f-b874-ecf95a005911', network_effects_constitute_monetary_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('2c15a9b1-afe2-4f5f-b874-ecf95a005911', first_individual_digital_holding).
narrative_ontology:cs_drift_state('2c15a9b1-afe2-4f5f-b874-ecf95a005911', universal_access_debate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c15a9b1-afe2-4f5f-b874-ecf95a005911', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopter_holders).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, payment_rail_operators).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, financial_intermediaries_with_digital_infrastructure).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, unbanked_populations).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, rural_communities_without_connectivity).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, cash_dependent_informal_economy_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First individuals to hold digital monetary instruments (stored-value cards, early e-money, proto-crypto) as practical stores of value. They gained liquidity, cross-border transfer capability, and programmable features before mainstream adoption. Could exit to cash or traditional banking with moderate friction.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopter_holders, beneficiary,
    moderate, biographical, mobile, global).

% Operators of the first digital payment rails (Visa/MC networks, PayPal, early e-money issuers, Bitcoin miners/validators). They set the technical and economic rules of holding, capture transaction fees and seigniorage-like benefits, and can migrate across rail architectures.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, payment_rail_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Banks and fintechs that already possessed digital ledger infrastructure. They absorbed early holding activity, captured float and data rents, and shaped regulatory interpretation. Their exit is effectively zero — they are the infrastructure.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, financial_intermediaries_with_digital_infrastructure, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, financial_intermediaries_with_digital_infrastructure, agenda_setter).

% Individuals without access to the digital infrastructure required to hold digital money. They bear the cost of financial exclusion as monetary systems reorient around digital holding — cash infrastructure atrophies, fees rise on remaining analog channels, and their monetary agency contracts.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, unbanked_populations, payer,
    powerless, biographical, trapped, global).

% Communities where broadband and mobile data are absent or unaffordable. Digital money's emergence as 'first held' presupposes connectivity they lack. They pay in degraded access to monetary services and rising dependence on intermediaries who extract rents from their exclusion.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, rural_communities_without_connectivity, payer,
    powerless, biographical, trapped, regional).

% Workers and merchants in cash-intensive informal economies. The shift to digital holding as the marker of monetary emergence delegitimizes their monetary practices. They face surveillance, exclusion from digital payment networks, and forced formalization that extracts compliance costs without delivering proportional benefits.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, cash_dependent_informal_economy_participants, payer,
    powerless, immediate, constrained, local).

% Scholars who date monetary regime transitions. This reading gives them a concrete, observable threshold (first individual holding) but forces them to confront the infrastructural violence embedded in that threshold.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the double-coincidence-of-wants and settlement-finality problems for digital transactions by establishing a widely held, individually accessible digital monetary instrument that functions as a practical store of value and medium of exchange without physical settlement.
% TRANSFER_FUNCTION: Moves monetary agency and seigniorage-like benefits from cash-based systems to operators of digital payment rails and early holders with infrastructure access; moves exclusion costs onto populations without digital access; moves settlement risk from counterparties to rail operators.
% ABSENT_VOICES: The global unbanked (1.4B+ adults per World Bank), rural populations in the Global South without broadband, and informal economy participants whose monetary practices are rendered invisible by the 'first held' threshold. They are not in the room when monetary historians or central bankers date the emergence of digital money.
% DISAPPEARANCE_RATIONALE: If the 'first held' threshold vanished as the origin marker, the historical narrative would shift to earlier conceptual or regulatory moments. The material infrastructure of digital holding would still exist, but the legitimating story that centers individual holding as the monetary Rubicon would collapse — changing how policymakers justify financial inclusion investments and how regulators define the perimeter of monetary regulation.
% FOUNDING_PROBLEM: The problem of creating a monetary instrument that could function as a practical store of value for individuals in digital form — solving the trust, custody, and settlement problems that prevented earlier digital representations of value from being 'money' rather than 'claims on money.'
% FOUNDING_PROBLEM_CORROBORATION: Payments industry narratives and early e-money advocates (e.g., Mondex, Visa Cash pilots) attest the founding problem was solving individual digital holding. Central bank researchers (e.g., BIS working papers on retail CBDC) and financial inclusion advocates attest the problem was misdiagnosed — the real problem was and remains universal access, not the technical capacity for holding. No independent corroboration exists outside the beneficiary set for the 'holding = emergence' framing.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).
:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the transfer of seigniorage and data rents to rail operators and intermediaries, and the extraction of compliance costs from excluded populations. Suppression (0.42) is moderate — the constraint does not primarily operate through direct coercion but through infrastructural exclusion: you cannot hold what you cannot access. Theater (0.22) is low-moderate — the coordination function (solving digital settlement) is real, but the 'first held' narrative obscures the infrastructural violence. Accessibility collapse (0.35) is moderate — cash alternatives persist but atrophy. Resistance (0.45) is significant — from unbanked advocates, cash preservation movements, and informal economy participants. The claimed type is tangled_rope: genuine coordination (digital settlement) + asymmetric extraction (infrastructure owners capture, excluded populations pay) + active enforcement (rails actively maintain exclusivity through KYC/AML, technical barriers, regulatory capture).
 *
 * PERSPECTIVAL GAP:
 *   From the early_adopter_holder seat, the constraint looks like a rope — genuine coordination gain with manageable costs. From the payment_rail_operator seat, it looks like a coordination function they built and profit from. From the unbanked_population seat, it looks like a snare — exclusion masquerading as progress. From the monetary_historian seat, it looks like a contested origin claim that structures the entire field's periodization. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and rail operators are structural beneficiaries (d near 0.0-0.2): they collect capabilities and rents. Financial intermediaries are beneficiaries with agenda-setting power (d ~ 0.1). Unbanked, rural, and informal economy participants are structural targets (d near 0.8-1.0): they bear costs without collecting benefits, and their exit options are trapped or constrained. The directionality derivation from beneficiary/victim declarations + exit options captures this asymmetry. No overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (individual digital holding as practical store of value) is contested: technically solved for the connected, but the universal-access version of the problem remains live. The arrangement persists not because the founding problem is solved, but because the infrastructure now embeds extraction. This is not pure mandatrophy (the coordination function is real) but a tangled_rope where the coordination victory for some is the extraction mechanism for others. The classification prevents mislabeling this as pure extraction (snare) because the digital settlement function is genuine, and prevents mislabeling as pure coordination (rope) because the asymmetric extraction is structural, not incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ''first_held_reading'' a distinct constraint from its sibling readings, or a different observable of the same underlying constraint?',
    'Apply the ε-invariance test: if measuring digital money''s emergence via ''first held'' yields a different extractiveness profile than ''became thinkable'' or ''regulatory recognition'', they are distinct constraints. Compare the beneficiary/victim structures and enforcement requirements across readings.',
    'If distinct, each reading gets its own constraint story with independent ε and classification. If same constraint, the ε variance signals measurement error in the framework. This is the foundational committer-frame ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate structurally distinct constraints per ε-invariance.').

omega_variable(
    infrastructure_access_as_extraction_mechanism,
    'Is the infrastructure access requirement (connectivity, devices, literacy) a genuine coordination cost or an extraction mechanism that benefits rail operators?',
    'Compare the marginal cost of providing digital holding infrastructure to the rents captured by operators. If rents vastly exceed costs across the adoption curve, the access barrier functions as extraction. Natural experiments: M-Pesa (low barrier, high adoption) vs. early e-money (high barrier, low adoption).',
    'If extraction mechanism, the constraint''s extractiveness is higher than the coordination floor; if genuine cost, the measured extraction includes necessary overhead. Affects tangled_rope vs rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_access_as_extraction_mechanism, empirical, 'Whether infrastructural exclusion is coordination necessity or engineered extraction.').

omega_variable(
    cash_atrophy_as_suppression,
    'Does the atrophy of cash infrastructure (ATM closures, bank branch reductions, merchant cash refusal) constitute active suppression of the alternative, or a natural consequence of digital adoption?',
    'Track whether cash infrastructure withdrawal correlates with rail operator lobbying, regulatory changes favoring digital, or pure demand substitution. If policy/lobbying driven, suppression is higher than measured.',
    'If active suppression, the constraint''s suppression metric understates coercive force; the ''first held'' narrative masks a deliberate alternative-closure campaign. Could shift classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cash_atrophy_as_suppression, empirical, 'Whether cash infrastructure decay is endogenous adoption or engineered exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1993, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmo_fhr_tr_t1993, digital_money_origin__first_held_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(dmo_fhr_tr_t1998, digital_money_origin__first_held_reading, theater_ratio, 1998, 0.14).
narrative_ontology:measurement(dmo_fhr_tr_t2003, digital_money_origin__first_held_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(dmo_fhr_tr_t2008, digital_money_origin__first_held_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(dmo_fhr_tr_t2013, digital_money_origin__first_held_reading, theater_ratio, 2013, 0.21).
narrative_ontology:measurement(dmo_fhr_tr_t2018, digital_money_origin__first_held_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(dmo_fhr_tr_t2023, digital_money_origin__first_held_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(dmo_fhr_be_t1993, digital_money_origin__first_held_reading, base_extractiveness, 1993, 0.15).
narrative_ontology:measurement(dmo_fhr_be_t1998, digital_money_origin__first_held_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(dmo_fhr_be_t2003, digital_money_origin__first_held_reading, base_extractiveness, 2003, 0.28).
narrative_ontology:measurement(dmo_fhr_be_t2008, digital_money_origin__first_held_reading, base_extractiveness, 2008, 0.31).
narrative_ontology:measurement(dmo_fhr_be_t2013, digital_money_origin__first_held_reading, base_extractiveness, 2013, 0.34).
narrative_ontology:measurement(dmo_fhr_be_t2018, digital_money_origin__first_held_reading, base_extractiveness, 2018, 0.36).
narrative_ontology:measurement(dmo_fhr_be_t2023, digital_money_origin__first_held_reading, base_extractiveness, 2023, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dmo_fhr_su_t1993, digital_money_origin__first_held_reading, suppression_requirement, 1993, 0.2).
narrative_ontology:measurement(dmo_fhr_su_t1998, digital_money_origin__first_held_reading, suppression_requirement, 1998, 0.28).
narrative_ontology:measurement(dmo_fhr_su_t2003, digital_money_origin__first_held_reading, suppression_requirement, 2003, 0.33).
narrative_ontology:measurement(dmo_fhr_su_t2008, digital_money_origin__first_held_reading, suppression_requirement, 2008, 0.37).
narrative_ontology:measurement(dmo_fhr_su_t2013, digital_money_origin__first_held_reading, suppression_requirement, 2013, 0.4).
narrative_ontology:measurement(dmo_fhr_su_t2018, digital_money_origin__first_held_reading, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement(dmo_fhr_su_t2023, digital_money_origin__first_held_reading, suppression_requirement, 2023, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.12).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, financial_inclusion_mandate).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, cash_preservation_regulation).

% DUAL FORMULATION NOTE:
% Part of the digital_money_origin constraint family (3 readings). This reading (first_held) has ε=0.38, claimed tangled_rope. The became_thinkable_reading has earlier origin, lower ε (conceptual phase, less extraction), claimed rope. The regulatory_recognition_reading has later origin, higher ε (state-backed extraction), claimed tangled_rope/snare. The three stories share a kernel but instantiate different constraints with different ε, beneficiaries, and victims — linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
