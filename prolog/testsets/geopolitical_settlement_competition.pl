% ============================================================================
% CONSTRAINT STORY: geopolitical_settlement_competition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_settlement_competition, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geopolitical_settlement_competition
 *   human_readable: Geopolitical Settlement Infrastructure Competition
 *   domain: economic/geopolitical/monetary
 *
 * SUMMARY:
 *   The constraint describes the structural competition between dollar-based
 *   correspondent banking (the incumbent settlement infrastructure) and
 *   alternative rails (mBridge, CIPS, SPFS, bilateral CBDC bridges) built by
 *   non-dollar reserve issuers. The competition is presented by alternative
 *   rail operators as solving sanctions vulnerability and dollar dependency;
 *   dollar-system defenders read it as geopolitical extraction that fragments
 *   global liquidity and raises systemic risk. The constraint is claimed as
 *   tangled_rope because it genuinely coordinates cross-border settlement
 *   (solving a real collective action problem) while asymmetrically
 *   extracting from dollar-system incumbents and imposing costs on dollar
 *   debt holders. KEY AGENTS (by structural relationship): -
 *   dollar_seigniorage_recipients: Primary target (institutional/constrained)
 *   — lose reserve currency privilege and fiscal subsidy -
 *   non_dollar_reserve_issuers: Primary beneficiary (institutional/mobile) —
 *   gain monetary autonomy and seigniorage -
 *   alternative_settlement_operators: Agenda setter (institutional/mobile) —
 *   build and govern alternative rails - sanctioned_economies: Secondary
 *   beneficiary (organized/constrained) — gain sanctions evasion capacity -
 *   correspondent_banking_networks: Secondary target (powerful/constrained) —
 *   lose transaction volume and fee revenue -
 *   dollar_denominated_debt_holders: Secondary target (organized/trapped) —
 *   bear rising debt service costs - global_trade_participants: Mixed
 *   position (organized/mobile) — gain efficiency, pay fragmentation costs -
 *   monetary_policy_analysts: Analytical observer — measure reserve
 *   composition shifts and fiscal impacts
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_settlement_competition, 0.68).
domain_priors:suppression_score(geopolitical_settlement_competition, 0.71).
domain_priors:theater_ratio(geopolitical_settlement_competition, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_settlement_competition, extractiveness, 0.68).
narrative_ontology:constraint_metric(geopolitical_settlement_competition, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geopolitical_settlement_competition, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geopolitical_settlement_competition, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(geopolitical_settlement_competition, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_settlement_competition, tangled_rope).
narrative_ontology:human_readable(geopolitical_settlement_competition, "Geopolitical Settlement Infrastructure Competition").
narrative_ontology:topic_domain(geopolitical_settlement_competition, "economic/geopolitical/monetary").

domain_priors:requires_active_enforcement(geopolitical_settlement_competition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geopolitical_settlement_competition, '9e83872c-59b5-4577-8f70-550d2919837b').
narrative_ontology:cs_kernel_codification('9e83872c-59b5-4577-8f70-550d2919837b', distributed).
narrative_ontology:cs_authority_grounding('9e83872c-59b5-4577-8f70-550d2919837b', distributed).
narrative_ontology:cs_created_at('9e83872c-59b5-4577-8f70-550d2919837b', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_settlement_competition, non_dollar_reserve_issuers).
narrative_ontology:constraint_beneficiary(geopolitical_settlement_competition, alternative_settlement_operators).
narrative_ontology:constraint_beneficiary(geopolitical_settlement_competition, sanctioned_economies).
narrative_ontology:constraint_victim(geopolitical_settlement_competition, dollar_seigniorage_recipients).
narrative_ontology:constraint_victim(geopolitical_settlement_competition, correspondent_banking_networks).
narrative_ontology:constraint_victim(geopolitical_settlement_competition, dollar_denominated_debt_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geopolitical_settlement_competition, global_trade_participants).
narrative_ontology:constraint_victim(geopolitical_settlement_competition, global_trade_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The US Treasury and Federal Reserve system collect seigniorage revenue from global dollar demand and benefit from low borrowing costs due to reserve currency status. As alternative settlement infrastructure gains adoption, demand for dollar reserves declines, raising US borrowing costs and reducing the fiscal subsidy from reserve currency privilege. Exit would mean abandoning dollar hegemony entirely, which is structurally difficult given existing debt stock and institutional arrangements.
narrative_ontology:constraint_stakeholder(geopolitical_settlement_competition, dollar_seigniorage_recipients, payer,
    institutional, generational, constrained, global).

% Central banks issuing yuan, euro, rupee, and other currencies gain from reduced dollar dependency. They build alternative settlement rails (mBridge, CIPS, SPFS) to route trade outside SWIFT and dollar correspondent banking. Success means their currencies capture a larger share of global reserves, reducing vulnerability to dollar-based sanctions and increasing their own seigniorage revenue and monetary policy autonomy.
narrative_ontology:constraint_stakeholder(geopolitical_settlement_competition, non_dollar_reserve_issuers, beneficiary,
    institutional, generational, mobile, global).

% Institutions operating mBridge, CIPS, SPFS, and bilateral CBDC bridges set the technical standards and governance rules for non-dollar settlement. They coordinate participating central banks, define interoperability protocols, and enforce compliance with their networks' rules. Their success depends on attracting transaction volume away from SWIFT and dollar correspondent banking.
narrative_ontology:constraint_stakeholder(geopolitical_settlement_competition, alternative_settlement_operators, agenda_setter,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(geopolitical_settlement_competition, alternative_settlement_operators, beneficiary).

% Countries under US or EU sanctions gain access to trade settlement outside dollar-based enforcement. Alternative rails allow them to transact with willing partners without triggering correspondent bank compliance blocks. Their participation drives adoption of alternative infrastructure but also concentrates reputational risk on those networks.
narrative_ontology:constraint_stakeholder(geopolitical_settlement_competition, sanctioned_economies, beneficiary,
    organized, biographical, constrained, national).

% Major international banks operating dollar correspondent accounts lose transaction volume and fee revenue as trade migrates to alternative settlement rails. They face compliance costs from sanctions enforcement and reputational risk from processing sanctioned counterparties, which alternative networks bypass. Exit means abandoning the correspondent banking business model, but staying means competing with state-backed infrastructure.
narrative_ontology:constraint_stakeholder(geopolitical_settlement_competition, correspondent_banking_networks, payer,
    powerful, biographical, constrained, global).

% Emerging market governments, corporations, and households holding dollar-denominated debt face rising debt service costs as dollar demand weakens and US interest rates rise to compensate for reduced reserve currency privilege. They are locked into existing debt contracts and cannot exit without default or costly refinancing.
narrative_ontology:constraint_stakeholder(geopolitical_settlement_competition, dollar_denominated_debt_holders, payer,
    organized, biographical, trapped, global).

% Exporters and importers gain from reduced transaction costs and faster settlement when alternative rails work efficiently, and from reduced sanctions risk when trading with non-aligned partners. They pay through fragmented liquidity across multiple settlement systems and increased operational complexity managing multiple currency exposures.
narrative_ontology:constraint_stakeholder(geopolitical_settlement_competition, global_trade_participants, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(geopolitical_settlement_competition, global_trade_participants, payer).

% Academic economists, central bank researchers, and geopolitical strategists study the transition from dollar-centric to multipolar settlement infrastructure. They measure reserve currency composition shifts, model the fiscal impact of reduced seigniorage, and assess whether alternative rails constitute genuine coordination or geopolitical extraction.
narrative_ontology:constraint_stakeholder(geopolitical_settlement_competition, monetary_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of cross-border trade settlement: matching buyers and sellers across currencies, providing liquidity, clearing transactions, and enforcing payment finality without requiring every trading pair to establish bilateral trust.
% TRANSFER_FUNCTION: Moves seigniorage revenue and monetary policy autonomy from dollar-issuing institutions to alternative reserve currency issuers; moves transaction fee revenue from dollar correspondent banks to alternative settlement operators; imposes debt service cost increases on dollar-denominated debt holders as US borrowing costs rise.
% ABSENT_VOICES: Smaller economies without the scale to operate their own settlement infrastructure are structurally excluded from governance of alternative rails and must accept the terms set by larger non-dollar issuers. They would argue for open interoperability standards and neutral governance but lack the institutional power to demand it.
% DISAPPEARANCE_RATIONALE: If alternative settlement infrastructure vanished overnight, sanctioned economies would lose access to trade finance, non-dollar reserve issuers would return to dollar dependency, US borrowing costs would fall as reserve demand recovered, and correspondent banking networks would recapture lost transaction volume. The global trade finance system would re-centralize around dollar rails.
% FOUNDING_PROBLEM: Dollar-based correspondent banking created single-point-of-failure vulnerability to US sanctions policy and concentrated seigniorage revenue in dollar-issuing institutions, leaving non-aligned economies structurally dependent on a system whose rules they did not control.
% FOUNDING_PROBLEM_CORROBORATION: Non-dollar reserve issuers attest the problem is live, citing ongoing sanctions risk and dollar dependency costs. Independent geopolitical analysts and central bank researchers outside the benefiting parties confirm that sanctions weaponization and reserve concentration remain active concerns driving alternative infrastructure investment, as documented in BIS working papers and IMF reserve composition data.
narrative_ontology:disappearance_verdict(geopolitical_settlement_competition, world_rearranges).
narrative_ontology:founding_problem_status(geopolitical_settlement_competition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geopolitical_settlement_competition, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-15',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(geopolitical_settlement_competition, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_settlement_competition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_settlement_competition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geopolitical_settlement_competition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the constraint transfers seigniorage revenue and monetary policy autonomy from dollar issuers to alternative reserve issuers, and imposes debt service cost increases on trapped dollar debt holders. The extraction is not merely competitive displacement — it is structural transfer of fiscal subsidy and policy control. Suppression is high (0.71) because alternative rails must actively enforce participation to achieve network effects, and dollar-system defenders must actively suppress alternative rail adoption through sanctions threats and correspondent banking pressure to maintain dollar centrality. Theater ratio is moderate (0.42): the settlement coordination function is real and growing, but an increasing share of activity is geopolitical signaling (announcing mBridge pilots, bilateral CBDC bridge MOUs) rather than operational transaction volume. The measurement series shows extraction and suppression intensifying as alternative infrastructure matures and the competition becomes zero-sum.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (alternative rail operators and non-dollar issuers) should compute the constraint as coordination with modest extraction — they built infrastructure to solve a real dependency problem and the costs are competitive displacement. The target seats (dollar seigniorage recipients, correspondent banks, debt holders) should compute it as substantial extraction enforced through geopolitical pressure — the coordination story is cover for transferring fiscal privilege and fragmenting global liquidity. The engine computes this divergence from the structural data; the claimed type (tangled_rope) does not adjudicate between the framings but asserts both coordination and extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Dollar seigniorage recipients are structural targets: they lose fiscal subsidy and borrowing cost advantage as reserve demand shifts, and their exit options are constrained by existing debt stock and institutional lock-in (d near target end). Non-dollar reserve issuers are structural beneficiaries: they gain seigniorage revenue, monetary autonomy, and sanctions immunity, with mobile exit options because they can choose which alternative rails to support (d near beneficiary end). Alternative settlement operators are agenda setters and beneficiaries: they build the infrastructure and set governance rules, capturing transaction fees and geopolitical influence. Sanctioned economies are beneficiaries with constrained exit: they gain sanctions evasion but are locked into alternative rails by their exclusion from dollar systems. Correspondent banks are targets with constrained exit: they lose revenue but cannot abandon the business model without exiting international banking entirely. Dollar debt holders are trapped targets: locked into existing contracts, bearing rising costs with no exit. Global trade participants sit near symmetric: genuine coordination gains offset by fragmentation costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling this as pure coordination (rope) or pure extraction (snare). It is not a rope because identifiable victims (dollar debt holders, correspondent banks) bear concentrated costs beyond competitive displacement, and suppression is required to enforce participation in alternative rails and suppress dollar-system network effects. It is not a snare because the coordination function is genuine — alternative rails do solve cross-border settlement problems and reduce sanctions vulnerability for participating economies. The mandate (solving dollar dependency and sanctions risk) has not outlived its function; the founding problem remains live as documented by ongoing sanctions policy and reserve diversification. The constraint is a hybrid: real coordination layered with geopolitical extraction, requiring active enforcement to maintain both functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seigniorage_transfer_magnitude,
    'What is the actual fiscal impact on dollar-issuing institutions from reduced reserve currency demand, relative to the coordination gains from alternative settlement infrastructure?',
    'Econometric analysis of US Treasury borrowing cost changes correlated with reserve composition shifts; comparison of alternative rail transaction costs to correspondent banking fees; measurement of sanctions evasion volume versus legitimate trade efficiency gains.',
    'A large fiscal transfer with modest coordination gains would establish the constraint as primarily extractive (snare-adjacent); a small fiscal impact with substantial trade efficiency gains would support the coordination framing (rope-adjacent). Current evidence suggests the transfer is substantial but the coordination function is also real, supporting tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_transfer_magnitude, empirical, 'Whether the constraint''s extraction component dominates its coordination component.').

omega_variable(
    alternative_rail_governance_neutrality,
    'Are alternative settlement rails governed as neutral public goods, or do they replicate dollar-system extraction under different geopolitical control?',
    'Analysis of mBridge and CIPS governance structures: who sets rules, who can be excluded, whether smaller economies have voice in governance. Natural experiment from jurisdictions that adopt alternative rails: do they gain monetary autonomy or merely shift dependency from dollar to yuan/euro systems?',
    'If alternative rails are governed neutrally with open participation, the constraint is genuine coordination solving dollar dependency. If they replicate extraction under yuan or euro control, the constraint is geopolitical rent-seeking dressed as coordination, and smaller economies remain structurally dependent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_rail_governance_neutrality, conceptual, 'Whether alternative rails solve dependency or merely shift its locus.').

omega_variable(
    fragmentation_systemic_risk,
    'Does multipolar settlement infrastructure reduce systemic risk by eliminating single points of failure, or increase it by fragmenting liquidity and creating interoperability failures?',
    'Stress-test analysis of cross-border payment systems under crisis scenarios; measurement of liquidity fragmentation costs; assessment of interoperability standards development. Historical comparison to pre-Bretton Woods multipolar currency regimes.',
    'If fragmentation increases systemic risk, the coordination gains are offset by new failure modes, and the constraint''s net welfare effect is ambiguous. If it reduces risk by diversifying settlement paths, the coordination function is stronger than dollar-system defenders claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmentation_systemic_risk, empirical, 'Whether the constraint''s coordination function creates or destroys systemic resilience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_settlement_competition, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geop_tr_t0, geopolitical_settlement_competition, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(geop_tr_t0, observed).
narrative_ontology:measurement(geop_tr_t5, geopolitical_settlement_competition, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(geop_tr_t5, observed).
narrative_ontology:measurement(geop_tr_t10, geopolitical_settlement_competition, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(geop_tr_t10, observed).
narrative_ontology:measurement(geop_tr_t15, geopolitical_settlement_competition, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(geop_tr_t15, observed).
narrative_ontology:measurement(geop_tr_t20, geopolitical_settlement_competition, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(geop_tr_t20, observed).
narrative_ontology:measurement(geop_tr_t25, geopolitical_settlement_competition, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(geop_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(geop_be_t0, geopolitical_settlement_competition, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(geop_be_t0, observed).
narrative_ontology:measurement(geop_be_t5, geopolitical_settlement_competition, base_extractiveness, 5, 0.53).
narrative_ontology:measurement_basis(geop_be_t5, observed).
narrative_ontology:measurement(geop_be_t10, geopolitical_settlement_competition, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(geop_be_t10, observed).
narrative_ontology:measurement(geop_be_t15, geopolitical_settlement_competition, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(geop_be_t15, observed).
narrative_ontology:measurement(geop_be_t20, geopolitical_settlement_competition, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(geop_be_t20, observed).
narrative_ontology:measurement(geop_be_t25, geopolitical_settlement_competition, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(geop_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(geop_su_t0, geopolitical_settlement_competition, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(geop_su_t0, observed).
narrative_ontology:measurement(geop_su_t5, geopolitical_settlement_competition, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(geop_su_t5, observed).
narrative_ontology:measurement(geop_su_t10, geopolitical_settlement_competition, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(geop_su_t10, observed).
narrative_ontology:measurement(geop_su_t15, geopolitical_settlement_competition, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(geop_su_t15, observed).
narrative_ontology:measurement(geop_su_t20, geopolitical_settlement_competition, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(geop_su_t20, observed).
narrative_ontology:measurement(geop_su_t25, geopolitical_settlement_competition, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(geop_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_settlement_competition, global_infrastructure).
narrative_ontology:boltzmann_floor_override(geopolitical_settlement_competition, 0.22).
narrative_ontology:affects_constraint(geopolitical_settlement_competition, digital_money_legitimacy_sovereign_cbdc).
narrative_ontology:affects_constraint(geopolitical_settlement_competition, digital_money_legitimacy_regulated_stablecoin).
narrative_ontology:affects_constraint(geopolitical_settlement_competition, sanctions_enforcement_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is structurally upstream of the digital money legitimacy kernel readings. The geopolitical settlement competition creates the demand for CBDCs and stablecoins as alternative settlement instruments, and the success of alternative rails depends on which digital money reading becomes institutionally dominant. A sovereign CBDC reading strengthens state-controlled alternative rails; a regulated stablecoin reading enables private settlement innovation; a crypto permissionless reading would bypass state-controlled infrastructure entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geopolitical_settlement_competition, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
