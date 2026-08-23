% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: First-Held Digital Money Emergence
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story models the 'first_held_reading' of the
 *   digital_money_origin kernel: digital money emerged when individuals first
 *   held non-physical monetary instruments as practical stores of value. The
 *   constraint is the socio-technical system (electronic funds transfer, core
 *   banking ledgers, later internet/mobile banking) that made such holding
 *   possible and legally recognized. The reading claims a later origin date
 *   than the became_thinkable_reading (which locates emergence in the 1960s
 *   conceptual breakthrough of electronic ledger entries) and an earlier date
 *   than the regulatory_recognition_reading (which requires formal
 *   statistical incorporation). The constraint exhibits genuine coordination
 *   (solving distance/trust in value transfer) AND asymmetric extraction
 *   (early adopters and infrastructure owners capture rents; the excluded
 *   bear transition costs). Active enforcement is required: technical
 *   standards (SWIFT, ACH, EMV), legal tender laws, KYC/AML regimes, and
 *   cash-discouragement policies maintain the digital-by-default arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.48).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.35).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "First-Held Digital Money Emergence").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'e251cbc4-6014-477f-8d21-818ee897e8e2').
narrative_ontology:cs_kernel_codification('e251cbc4-6014-477f-8d21-818ee897e8e2', distributed).
narrative_ontology:cs_authority_grounding('e251cbc4-6014-477f-8d21-818ee897e8e2', practice).
narrative_ontology:cs_interpretation_layer_present('e251cbc4-6014-477f-8d21-818ee897e8e2').
narrative_ontology:cs_reading_relation('e251cbc4-6014-477f-8d21-818ee897e8e2', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('e251cbc4-6014-477f-8d21-818ee897e8e2', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('e251cbc4-6014-477f-8d21-818ee897e8e2', foundational, money_is_what_people_actually_hold_and_use).
narrative_ontology:cs_axiom_status(money_is_what_people_actually_hold_and_use, holdable).
narrative_ontology:cs_axiom_grounding('e251cbc4-6014-477f-8d21-818ee897e8e2', money_is_what_people_actually_hold_and_use, empirically_contingent).
narrative_ontology:cs_axiom('e251cbc4-6014-477f-8d21-818ee897e8e2', secondary, legal_tender_status_follows_practice_not_precedes_it).
narrative_ontology:cs_axiom_status(legal_tender_status_follows_practice_not_precedes_it, holdable).
narrative_ontology:cs_axiom_grounding('e251cbc4-6014-477f-8d21-818ee897e8e2', legal_tender_status_follows_practice_not_precedes_it, conventional).
narrative_ontology:cs_reference_frame('e251cbc4-6014-477f-8d21-818ee897e8e2', distributed_ledger_practice_precedent).
narrative_ontology:cs_drift_state('e251cbc4-6014-477f-8d21-818ee897e8e2', contemporary_cbdc_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e251cbc4-6014-477f-8d21-818ee897e8e2', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters_with_access).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, financial_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, technology_providers).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, infrastructure_excluded).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, digital_ledger_superiority_claim).
narrative_ontology:constraint_vindicates(digital_money_origin__first_held_reading, financial_inclusion_via_technology_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Corporate treasuries, tech-forward banks, and later fintech-savvy individuals who gained first-mover advantages in digital payment rails, algorithmic trading, and automated cash management. They pay fees but capture efficiency gains and network-position rents. Exit is constrained by sunk investment in digital workflows and counterparty lock-in.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters_with_access, beneficiary,
    moderate, biographical, constrained, national).

% Unbanked, underbanked, rural, elderly, and Global South populations lacking reliable connectivity, digital literacy, or identity documentation required for digital money access. They bear the costs of cash-phase-out (fees, travel, vulnerability to theft) without accessing digital benefits. Exit from cash dependency is blocked by the very infrastructure gap the constraint presupposes.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, infrastructure_excluded, payer,
    powerless, biographical, trapped, global).

% Banks, payment networks, and central banks that design, operate, and govern the digital ledger infrastructure. They set technical standards, access rules, and fee structures. They collect interchange, data, and seigniorage-adjacent rents. Their exit options are maximal — they can shift rails, lobby for favorable regulation, or acquire competitors.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__first_held_reading, financial_institutions, beneficiary).

% Fintech firms, core banking vendors, blockchain protocols, and cloud providers selling the stack that makes digital holding possible. They capture licensing, transaction, and data revenues. They can pivot across jurisdictions and standards, giving them high exit mobility.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, technology_providers, beneficiary,
    powerful, biographical, mobile, global).

% Central banks and treasuries that define legal tender, oversee payment systems, and publish monetary aggregates. They legitimate the constraint by incorporating digital balances into M1/M2 and by licensing issuers. Their analytical seat lets them observe the full structure while their policy levers shape it.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Informal economies, migrant remittance corridors, and communities where cash is a social institution (trust, privacy, offline resilience). They are not merely lacking access — their social coordination runs on cash. Digital mandate threatens the relational fabric. Identity-locked because exit means abandoning the social world that cash constitutes.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, cash_dependent_communities, excluded,
    organized, generational, identity_locked, local).

% Academic monetary historians, central bank researchers, and standards-body economists who trace the emergence, measure inclusion/exclusion, and model counterfactuals. They neither collect nor pay — they map the constraint's topology across readings.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, payment_system_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of moving value across distance and time without physical carriage: enables instant, programmable, auditable transfer between parties who never meet, supporting commerce, credit, and monetary policy transmission at scale.
% TRANSFER_FUNCTION: Moves seigniorage-adjacent rents, interchange revenue, and data-value from the infrastructure_excluded (who pay cash-premium fees, travel costs, and exclusion penalties) and from early_adopters_with_access (who pay platform fees) to financial_institutions and technology_providers. Also transfers systemic risk onto the excluded when digital rails fail.
% ABSENT_VOICES: Cash-dependent communities and infrastructure-excluded populations are structurally absent from the standards bodies (ISO 20022, EMVCo, BIS CPMI) and central bank working groups where digital money's technical and legal parameters are set. Their objection — that digital-by-default erases the offline, peer-to-peer, identity-free payment mode — is not in the room.
% DISAPPEARANCE_RATIONALE: If the first-held digital money constraint vanished overnight (i.e., the legal/technical recognition of non-physical balances as 'money' disappeared), commercial banking would revert to physical settlement, fintech valuations would collapse, monetary policy transmission would break, and the Global South would face acute dollarization pressure. The world would rearrange violently around cash and bearer instruments.
% FOUNDING_PROBLEM: The Triffin dilemma and the physical limits of correspondent banking: how to settle cross-border and high-value domestic payments without moving gold or cash, while preserving central bank control over the unit of account.
% FOUNDING_PROBLEM_CORROBORATION: Central bank archives (BIS, Fed, BoE) confirm the founding problem was cross-border settlement efficiency and monetary control — not retail inclusion. The inclusion narrative was retrofitted in the 2000s by development agencies (World Bank, CGAP) and fintech lobbyists. No corroborating source outside the beneficiary set (financial_institutions, technology_providers) attests that retail inclusion was the founding problem.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate-high: the constraint transfers value from excluded populations and fee-paying users to infrastructure owners, but the coordination function is real and substantial. Suppression (0.35) is moderate: cash remains legal tender in most jurisdictions, but the practical suppression of cash infrastructure (ATM closures, merchant refusal, policy nudges) is rising. Theater ratio (0.22) is low-moderate: the security/efficiency narrative is largely genuine, but 'financial inclusion' rhetoric increasingly covers extraction. Accessibility collapse (0.42) reflects that alternatives (crypto, community currencies, cash) persist but are marginal. Resistance (0.58) is significant: cash preservation movements, privacy advocates, and Global South central banks resisting premature cash-phase-out.
 *
 * PERSPECTIVAL GAP:
 *   From the financial_institution seat, the constraint is a Rope (genuine coordination, mutual benefit). From the infrastructure_excluded seat, it is a Snare (extraction enforced by infrastructure denial). From the monetary_authority seat, it is a Tangled Rope (coordination with distributive consequences they must manage). The engine computes this divergence from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial_institutions and technology_providers are structural beneficiaries (d ~ 0.15-0.25): they collect rents, set rules, and have arbitrage-grade exit. Early_adopters_with_access are moderate beneficiaries (d ~ 0.35): they gain efficiency but pay fees and face lock-in. Infrastructure_excluded are full targets (d ~ 0.9): trapped, identity-locked for cash-dependent communities, bearing costs without benefits. Monetary_authorities are near-symmetric (d ~ 0.5): they gain policy transmission but lose seigniorage visibility and face stability risks. Payment_system_analysts are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cross-border settlement, monetary control) is substantially solved — the original mandate is dead. Yet the constraint persists and expands into retail inclusion, where the coordination benefit is real but the extraction is pronounced. The mandate has atrophied into a new function (data capture, programmable money, CBDC precursors) without formal revision. This is mandatrophy: the arrangement survives by drifting into adjacent domains where its extraction profile changes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the choice of ''first held'' as the emergence criterion (vs. ''became thinkable'' or ''regulatory recognition'') structurally change the constraint''s beneficiary/victim sets, temporal boundaries, and extraction profile?',
    'Comparative analysis across the three kernel readings: map each reading''s claimed origin date to its implied constraint set (technical standards, legal recognition, statistical incorporation), then trace how each set distributes costs and benefits across the same stakeholder population.',
    'If ''first held'' yields a constraint with higher extraction on infrastructure_excluded than ''became thinkable'', the reading choice is not neutral — it naturalizes a particular extraction profile. The kernel contest is a fight over which constraint''s χ values govern policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame ambiguity: the kernel supports multiple ε-invariant constraints; the reading selection determines which constraint''s metrics apply.').

omega_variable(
    inclusion_rhetoric_vs_extraction_reality,
    'Is the ''financial inclusion'' narrative a genuine coordination expansion or a cover for extracting data and fees from newly captured populations?',
    'Longitudinal study of fee structures, data harvesting, and credit outcomes for newly banked populations in Kenya (M-Pesa), India (UPI), Brazil (Pix) — comparing pre/post digital adoption welfare metrics against a synthetic control.',
    'If inclusion delivers net welfare gains, the constraint trends toward Rope. If it delivers net extraction (fees > benefits, data asymmetry), it trends toward Snare. The first_held_reading''s ε depends on this empirical resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusion_rhetoric_vs_extraction_reality, empirical, 'Whether the coordination function claimed for retail digital money is net-beneficial for the excluded or extractive.').

omega_variable(
    cash_phaseout_coercion_mechanism,
    'Is the suppression of cash infrastructure structural (policy-driven ATM/branch closures, merchant mandates) or internalized (behavioral nudges, generational habit shift)?',
    'Track cash acceptance rates and ATM density after policy interventions (e.g., Nigeria''s 2022-23 cash swap, Sweden''s 2010s bank-led closures) vs. in jurisdictions with no policy push but similar digital adoption.',
    'If structural, suppression is a policy choice — removable by political action. If internalized, suppression persists even if policy reverses — the constraint has colonized the subject''s exit imagination. This changes the omega''s type_class and the constraint''s reversibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cash_phaseout_coercion_mechanism, empirical, 'Structural vs. internalized suppression mechanism for cash displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digital_money_origin__first_held_reading_tr_t1970, digital_money_origin__first_held_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(digital_money_origin__first_held_reading_tr_t1980, digital_money_origin__first_held_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(digital_money_origin__first_held_reading_tr_t1990, digital_money_origin__first_held_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(digital_money_origin__first_held_reading_tr_t2000, digital_money_origin__first_held_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(digital_money_origin__first_held_reading_tr_t2010, digital_money_origin__first_held_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(digital_money_origin__first_held_reading_tr_t2020, digital_money_origin__first_held_reading, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(digital_money_origin__first_held_reading_be_t1970, digital_money_origin__first_held_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(digital_money_origin__first_held_reading_be_t1980, digital_money_origin__first_held_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(digital_money_origin__first_held_reading_be_t1990, digital_money_origin__first_held_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(digital_money_origin__first_held_reading_be_t2000, digital_money_origin__first_held_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(digital_money_origin__first_held_reading_be_t2010, digital_money_origin__first_held_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(digital_money_origin__first_held_reading_be_t2020, digital_money_origin__first_held_reading, base_extractiveness, 2020, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(digital_money_origin__first_held_reading_su_t1970, digital_money_origin__first_held_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(digital_money_origin__first_held_reading_su_t1980, digital_money_origin__first_held_reading, suppression_requirement, 1980, 0.15).
narrative_ontology:measurement(digital_money_origin__first_held_reading_su_t1990, digital_money_origin__first_held_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(digital_money_origin__first_held_reading_su_t2000, digital_money_origin__first_held_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(digital_money_origin__first_held_reading_su_t2010, digital_money_origin__first_held_reading, suppression_requirement, 2010, 0.32).
narrative_ontology:measurement(digital_money_origin__first_held_reading_su_t2020, digital_money_origin__first_held_reading, suppression_requirement, 2020, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_origin__first_held_reading, 0.15).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_money_origin__regulatory_recognition_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, cbd_emergence_constraint).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, payment_rails_oligopoly_constraint).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, financial_inclusion_metrics_constraint).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel decomposes into three constraint stories (became_thinkable_reading, first_held_reading, regulatory_recognition_reading) linked by network.affects_constraints. Each reading claims a different emergence event, yielding different ε, different stakeholder sets, and different claimed_type. The became_thinkable_reading is upstream (lower extraction, earlier, more Mountain-like); first_held_reading is midstream (Tangled Rope, active enforcement); regulatory_recognition_reading is downstream (Scaffold or Rope depending on CBDC trajectory). The upstream reading's coordination claim is cited as evidence for the midstream reading's legitimacy — a classic contamination pathway.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__first_held_reading, institutional, 0.2).
constraint_indexing:directionality_override(digital_money_origin__first_held_reading, powerless, 0.9).
constraint_indexing:directionality_override(digital_money_origin__first_held_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
