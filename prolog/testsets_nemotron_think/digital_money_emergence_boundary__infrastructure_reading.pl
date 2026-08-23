% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence at the Banking Infrastructure Boundary
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the infrastructure_reading of the
 *   digital_money_emergence_boundary kernel: digital money emerges when
 *   banking infrastructure enables electronic interbank transfer (1967 ATMs,
 *   1972 ACH, 1977 SWIFT), even though consumers cannot yet directly hold
 *   digital instruments. The boundary sits at the bank-to-bank layer — M4/M5
 *   monetary aggregates begin collapsing here as electronic deposits blur the
 *   line between money and credit. The primary beneficiaries are the
 *   infrastructure providers (SWIFT, ACH operators, ATM networks) who control
 *   the rails and extract network fees. The constraint is a Tangled Rope: it
 *   solves a genuine coordination problem (interbank settlement at scale)
 *   while embedding asymmetric extraction (rail operators as toll
 *   collectors).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.45).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.35).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence at the Banking Infrastructure Boundary").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'e44f85d4-de47-4b70-8ca7-95408743b3a4').
narrative_ontology:cs_kernel_codification('e44f85d4-de47-4b70-8ca7-95408743b3a4', distributed).
narrative_ontology:cs_authority_grounding('e44f85d4-de47-4b70-8ca7-95408743b3a4', practice).
narrative_ontology:cs_reading_relation('e44f85d4-de47-4b70-8ca7-95408743b3a4', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('e44f85d4-de47-4b70-8ca7-95408743b3a4', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('e44f85d4-de47-4b70-8ca7-95408743b3a4', foundational, money_exists_when_banks_transfer_electronically).
narrative_ontology:cs_axiom_status(money_exists_when_banks_transfer_electronically, holdable).
narrative_ontology:cs_axiom_grounding('e44f85d4-de47-4b70-8ca7-95408743b3a4', money_exists_when_banks_transfer_electronically, conventional).
narrative_ontology:cs_axiom('e44f85d4-de47-4b70-8ca7-95408743b3a4', secondary, interbank_settlement_layer_is_definitional_for_money).
narrative_ontology:cs_axiom_status(interbank_settlement_layer_is_definitional_for_money, holdable).
narrative_ontology:cs_axiom_grounding('e44f85d4-de47-4b70-8ca7-95408743b3a4', interbank_settlement_layer_is_definitional_for_money, conventional).
narrative_ontology:cs_reference_frame('e44f85d4-de47-4b70-8ca7-95408743b3a4', banking_infrastructure_boundary).
narrative_ontology:cs_drift_state('e44f85d4-de47-4b70-8ca7-95408743b3a4', contemporary_digital_currency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e44f85d4-de47-4b70-8ca7-95408743b3a4', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__infrastructure_reading, electronic_interbank_transfer_constitutes_money).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the electronic payment rails (SWIFT, ACH, ATM networks) that enable interbank digital transfer. Set technical standards, access rules, and fee structures. Collect network fees and data rents from every transaction flowing through their infrastructure. Their control of the rails is the structural basis for claiming this boundary as the emergence point of digital money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain the ability to settle obligations electronically at scale, replacing costly physical cash and cheque transport. Pay access fees to infrastructure providers but capture efficiency gains and new product possibilities (credit cards, wire services). Their adoption of the infrastructure is voluntary but collectively self-reinforcing — a bank that refuses electronic rails loses competitiveness.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary,
    organized, biographical, mobile, global).

% Bear the indirect costs of infrastructure fees passed through bank service charges and reduced deposit rates. Cannot directly hold or initiate digital money — all electronic funds are bank liabilities mediated by the infrastructure. Exit to cash is formally possible but practically constrained by the increasing digitization of wages, bills, and commerce. No direct seat at the infrastructure governance table.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumers, payer,
    powerless, biographical, trapped, national).

% Oversee payment system stability and monetary policy transmission. Provide settlement finality (central bank reserves) that backs the commercial bank deposits moving over private rails. Influence infrastructure standards through oversight but do not operate the rails directly. Their analytical frame treats the infrastructure boundary as a monetary policy transmission channel, not a definitional boundary for money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter).

% Develop alternative digital money forms (Chaum's e-cash, later e-purses, stablecoins) that challenge the infrastructure-bound definition. Structurally excluded from the interbank rail network — cannot access central bank settlement or SWIFT/ACH without becoming a bank. Their exclusion is what maintains the infrastructure providers' gatekeeper position. Would argue the emergence boundary should be drawn at consumer-accessible digital instruments.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, competing_payment_innovators, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interbank settlement coordination problem: enabling banks to transfer obligations electronically at speed and scale, replacing physical transport of cash and cheques with a shared, standardized rail network.
% TRANSFER_FUNCTION: Moves control of payment routing, fee extraction, and transaction data from dispersed bank-to-bank relationships to centralized infrastructure operators (SWIFT, ACH operators, ATM networks). Transfers the cost of maintaining payment rails from individual banks (who would each need bilateral connections) to a shared utility model with toll-based revenue.
% ABSENT_VOICES: Consumers who had no direct access to digital money and bore indirect costs; cash-reliant populations and informal economies excluded by digitization; alternative payment innovators (Chaum, e-purse developers, later crypto advocates) structurally locked out of the interbank settlement layer. These voices are absent because the infrastructure boundary defines money at the bank-to-bank layer, rendering consumer-held digital instruments a later category.
% DISAPPEARANCE_RATIONALE: If electronic interbank transfer infrastructure vanished overnight, the modern payment system would collapse. Banks would revert to physical cash/cheque settlement, digital commerce would halt, wage payments and bill payments would fail, and monetary policy transmission through the banking system would break. The world would forcibly rearrange around physical settlement until new rails were built.
% FOUNDING_PROBLEM: The need for banks to settle interbank obligations at scale and speed without the cost, risk, and delay of physically transporting cash and cheques between institutions. The post-war explosion in transaction volume made physical settlement operationally untenable.
% FOUNDING_PROBLEM_CORROBORATION: Banking historians (e.g., James, Battilossi) and central bank archives (BIS, Fed) document the settlement crisis of the 1960s-70s as the driver for ACH and SWIFT. Infrastructure operators themselves (SWIFT annual reports, NACHA histories) attest to solving the settlement problem. No significant dissenting account from outside the benefiting parties — the problem is acknowledged even by critics of the resulting structure.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).
:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that infrastructure providers extract tolls on every transaction but also provide real coordination value — the fee is not pure rent. Suppression (0.35) is moderate: cash and physical settlement remain legally available but are practically marginalized for large-scale commerce. Theater ratio (0.2) is low — the infrastructure genuinely works and its standards are technically necessary, though governance increasingly serves operator interests. Accessibility collapse (0.5) captures that alternatives exist but are inferior for systemic scale. Resistance (0.25) is low because banks adopted voluntarily for efficiency; consumers had no direct choice. The temporal series shows extraction rising as the network effects lock in and operators gain pricing power, while theater and suppression stabilize once the infrastructure becomes the universal backbone.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure provider seat, the constraint is a Rope — a coordination utility they built and maintain. From the consumer seat, it is a Snare — they pay for rails they cannot access and have no alternative. From the commercial bank seat, it is a Tangled Rope — genuine coordination value with embedded extraction. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analytical seat that sees both functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure providers are structural beneficiaries (d near 0.0) — they collect fees, set standards, and face arbitrage-grade exit (they could sell the network). Commercial banks are moderate beneficiaries (d ~0.3) — they gain efficiency but pay tolls; exit is mobile (they can switch rails at cost). Consumers are targets (d ~0.8) — they bear passed-through costs, have trapped exit (cannot leave the banking system), and no governance voice. Central banks are observers (d ~0.5) — they oversee but don't extract. Competing innovators are excluded (d ~0.9) — their exclusion is the enforcement mechanism that protects the infrastructure rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interbank settlement at scale) remains live — the volume and speed demands have only increased. However, the infrastructure has layered additional extraction (data monetization, anti-competitive access rules) beyond what the founding problem required. The mandate has not atrophied but has been extended — the constraint persists because the coordination function is still essential, but the extraction component has grown disproportionately. This is not mandatrophy (where the function dies but the form remains); it is function-capture where a live coordination core is wrapped in extractive governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_location,
    'Does the digital_money_emergence_boundary kernel have a single correct boundary, or are the three readings (infrastructure, conceptualization, consumer_holdings) legitimate alternative boundary-drawing practices for different analytical purposes?',
    'Compare the predictive and explanatory power of each boundary for monetary policy transmission, financial stability analysis, and consumer protection frameworks. If each reading excels in a different domain, the kernel is genuinely multi-boundary.',
    'If multi-boundary is validated, no single reading forecloses the others — they are complementary analytical tools. If a single boundary is defensible, the losing readings become mischaracterizations of the monetary phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the kernel admits one true boundary or multiple legitimate boundary-drawing practices.').

omega_variable(
    infrastructure_extraction_necessity,
    'Is the extraction by infrastructure providers (SWIFT, ACH operators) a necessary cost of the coordination function, or is it separable monopoly rent that could be reduced without degrading settlement reliability?',
    'Analyze cost structures of infrastructure operators vs. marginal cost of transaction processing; examine jurisdictions with mandated open-access rails (e.g., EU PSD2) for evidence on whether coordination survives reduced extraction.',
    'If extraction is necessary cost, the Tangled Rope classification is structurally stable. If separable rent, the constraint tends toward Snare as the coordination cover thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_extraction_necessity, empirical, 'Whether infrastructure provider extraction is coordination-cost or monopoly-rent.').

omega_variable(
    consumer_exclusion_as_design,
    'Was the exclusion of consumers from direct digital money holding (1967-1990s) a necessary consequence of the infrastructure architecture, or a deliberate design choice that benefited infrastructure providers and banks?',
    'Historical analysis of early ATM/ACH/SWIFT design documents and governance debates: were consumer-direct digital instruments technically feasible but rejected, or genuinely infeasible at the time?',
    'If deliberate exclusion, the infrastructure boundary is a constructed beneficiary-favoring choice, strengthening the Tangled Rope / Snare case. If necessary consequence, the boundary reflects a genuine technological frontier.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_exclusion_as_design, empirical, 'Whether consumer exclusion from direct digital money was architectural necessity or interested design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1972, 0.08).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(digi_tr_t1990, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(digi_tr_t2000, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(digi_tr_t2010, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(digi_tr_t2024, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.25).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement(digi_be_t1990, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(digi_be_t2000, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(digi_be_t2010, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(digi_be_t2024, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.2).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.3).
narrative_ontology:measurement(digi_su_t1990, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(digi_su_t2000, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(digi_su_t2010, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2010, 0.35).
narrative_ontology:measurement(digi_su_t2024, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_money_emergence_boundary__infrastructure_reading, 0.15).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'digital money emergence' into three structurally distinct boundary claims. The infrastructure_reading draws the boundary at bank-to-bank electronic transfer capability (1967-1977), with infrastructure providers as beneficiaries. The conceptualization_reading draws it at theoretical formalization (1960s-1985), with cryptographers/standard-setters as beneficiaries. The consumer_holdings_reading draws it at consumer-direct digital instruments (1990s-2000), with fintechs/consumers as beneficiaries. Each reading has a different ε, different stakeholder structure, and different type classification. They are linked via affects_constraints because the infrastructure boundary is historically upstream — it created the rails that later readings contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, institutional, 0.1).
constraint_indexing:directionality_override(digital_money_emergence_boundary__infrastructure_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
