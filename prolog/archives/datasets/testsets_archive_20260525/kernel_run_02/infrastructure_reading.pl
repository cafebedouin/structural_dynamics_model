% ============================================================================
% CONSTRAINT STORY: infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: infrastructure_reading
 *   human_readable: Digital Money Emergence: Infrastructure Control Reading
 *   domain: monetary_economics/financial_infrastructure
 *
 * SUMMARY:
 *   This constraint instantiates the INFRASTRUCTURE READING of when digital
 *   money emerged: money became digital when banks gained the technological
 *   capacity to move it electronically (1967 ATMs enabling remote cash
 *   access, 1972 ACH enabling batch interbank clearing, 1977 SWIFT
 *   establishing standardized electronic messaging for international
 *   payments). This reading differs from competing readings: the
 *   CONCEPTUALIZATION reading locates emergence when money was formally
 *   redefined in economic theory to include electronic deposits (1960s-70s
 *   shift from M1 to M2-M5); the CONSUMER HOLDINGS reading locates emergence
 *   when consumers themselves could hold and transfer digital money directly
 *   (credit cards 1950s-60s, debit cards 1980s-90s, mobile wallets
 *   2000s-10s). Each reading anchors to different empirical phenomena and
 *   produces different victim and beneficiary sets. The infrastructure
 *   reading identifies beneficiaries as the operators of these systems
 *   (SWIFT, ACH operators, central banks) who gain control over the
 *   definition of money itself through controlling the rails. It identifies
 *   victims as non-bank actors and consumers locked out of the definition
 *   process — they experience money as what the infrastructure permits, not
 *   what they control. The key structural feature: once money is defined as
 *   electronic transfers on banking infrastructure, anyone not connected to
 *   that infrastructure is structurally outside the monetary system.
 *
 * KEY AGENTS:
 *   - Banking Infrastructure Operators (SWIFT, ACH, Fed Reserve): Primary beneficiaries (institutional/arbitrage) — define money through control of transfer rails; capture rents through settlement fees and message costs
 *   - Central Banks: Secondary beneficiaries (institutional/constrained) — coordinate monetary policy through infrastructure but depend on private operators for international settlement
 *   - Non-Bank Intermediaries (Fintechs, Money Market Funds): Constrained participants (organized/constrained) — can access but do not control infrastructure; pay fees and adapt to operator-set standards
 *   - Consumers and Cash Holders: Primary victims (powerless/trapped) — excluded from infrastructure, must either use cash (increasingly marginalized) or trust banks as intermediaries; no exit from constraint
 *   - Legacy Cash System: Degraded system (institutional/arbitrage) — persists through political legitimacy and contingency role but functionally displaced by digital infrastructure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing infrastructure control as inevitable property of monetary systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_reading, 0.58).
domain_priors:suppression_score(infrastructure_reading, 0.48).
domain_priors:theater_ratio(infrastructure_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(infrastructure_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(infrastructure_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(infrastructure_reading, "Digital Money Emergence: Infrastructure Control Reading").
narrative_ontology:topic_domain(infrastructure_reading, "monetary_economics/financial_infrastructure").

domain_priors:requires_active_enforcement(infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(infrastructure_reading, fixed_text).
narrative_ontology:cs_authority_grounding(infrastructure_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(infrastructure_reading).
narrative_ontology:cs_kernel_id(infrastructure_reading, digital_money_emergence_boundary).
narrative_ontology:cs_reading_relation(infrastructure_reading, conceptualization_reading, influences).
narrative_ontology:cs_reading_relation(infrastructure_reading, consumer_holdings_reading, influences).
narrative_ontology:cs_axiom(infrastructure_reading, foundational, infrastructure_control_defines_monetary_function).
narrative_ontology:cs_axiom_status(infrastructure_control_defines_monetary_function, holdable).
narrative_ontology:cs_axiom_grounding(infrastructure_reading, infrastructure_control_defines_monetary_function, empirically_contingent).
narrative_ontology:cs_axiom(infrastructure_reading, foundational, beneficiary_authority_via_infrastructure_control).
narrative_ontology:cs_axiom_status(beneficiary_authority_via_infrastructure_control, holdable).
narrative_ontology:cs_axiom_grounding(infrastructure_reading, beneficiary_authority_via_infrastructure_control, conventional).
narrative_ontology:cs_reference_frame(infrastructure_reading, technical_capability_money_definition).
narrative_ontology:cs_drift_state(infrastructure_reading, contemporary_cbdc_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_reading, banking_infrastructure_operators).
narrative_ontology:constraint_beneficiary(infrastructure_reading, central_banks_settlement_authority).
narrative_ontology:constraint_victim(infrastructure_reading, non_bank_financial_entities).
narrative_ontology:constraint_victim(infrastructure_reading, consumer_cash_exit_options).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-BANK ACTORS & CASH HOLDERS (SNARE) — Cannot participate in the definition of money once it is anchored to electronic infrastructure. Trapped by lack of access to SWIFT/ACH rails. Must either hold cash (losing value, excluded from money markets) or trust banks as intermediaries. No exit from the constraint itself — either participate on infrastructure terms or be outside the monetary system entirely.
constraint_indexing:constraint_classification(infrastructure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-BANK INTERMEDIARIES (TANGLED ROPE) — Money market funds, credit unions, payment processors can participate in electronic networks but do not control the rails. Constrained by having to route through SWIFT/ACH; some coordination benefit from standardized protocols but significant extraction through gatekeeping. Access to infrastructure is real coordination function; control of infrastructure is asymmetric extraction.
constraint_indexing:constraint_classification(infrastructure_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BANKING INFRASTRUCTURE OPERATORS / SWIFT / ACH (ROPE) — Define what money is through control of the infrastructure. Experience the constraint as pure coordination: standardizing message formats, settlement protocols, and real-time gross settlement enables global finance. Net beneficiary — extraction flows toward these operators through control of the rails and data on all transactions. Can arbitrage between national payment systems.
constraint_indexing:constraint_classification(infrastructure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANKS & SETTLEMENT AUTHORITIES (TANGLED ROPE) — Coordinate monetary policy through infrastructure control (reserve settlement in Fed wires, ECB TARGET2, etc.) but also face extraction from private infrastructure operators (correspondent banking fees, SWIFT message costs). Genuine coordination function (monetary policy transmission) embedded in asymmetric terms dictated partly by private operators. Constrained by dependence on infrastructure they don't fully control.
constraint_indexing:constraint_classification(infrastructure_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CASH & COMMODITY SYSTEMS (PITON) — Functionally degraded as primary money but persists through institutional inertia. Physical cash still used for small transactions and payment of last resort but has lost primary function — central banks maintain it for contingency and political legitimacy ('money we can hold'). Theater ratio reflects that cash production, distribution, and security consume real resources while serving increasingly theatrical role as 'true money' when digital systems now carry most monetary function.
constraint_indexing:constraint_classification(infrastructure_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/technical perspective, money requires some infrastructure for transfer — this is an irreducible constraint of scalable monetary systems. Electronic infrastructure is simply the natural evolution of what monetary systems require. No alternatives to infrastructure dependency exist for large-scale economies. However, this perspective naturalizes what is actually a political choice: WHICH infrastructure controls money definition. The mountain is a false summit — the structural data reveals that infrastructure type is contingent and engineered, not natural.
constraint_indexing:constraint_classification(infrastructure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_reading, TR),
    TR >= 0.70.

:- end_tests(infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The infrastructure operators capture substantial rents through controlling the monetary rails (SWIFT fees averaging $0.32-$2.50 per message; ACH operator margins; Fed reserve settlement risk premiums). However, this is not maximum extraction (0.80+) because the infrastructure also provides genuine coordination function — standardized message formats and real-time settlement enable global commerce that would be impossible under alternative systems. The measurement trajectory shows steady accumulation: pre-ATM (0.15) when money was primarily cash and checks; post-ACH (0.48) when most interbank clearing was electronic; post-SWIFT dominance (0.58) when international transfers required compliance with operator-controlled standards. Suppression (0.48): Moderate. Barriers to exit are real but not total. Non-banks can access infrastructure (paying fees), cash remains available (though increasingly discouraged), and some alternatives exist (cryptocurrencies, community currencies, barter). The suppression reflects the structural reality: any large-scale monetary system requires some infrastructure, but THIS specific infrastructure (SWIFT/ACH) has switching costs and coordination lock-in. Theater ratio (0.35): Low. The infrastructure reading has low theater because it focuses on what actually works: message formats, settlement protocols, real-time gross settlement. This is functional rather than performative. Compare to the legacy cash perspective (perspective 5, theater 0.7+) where cash production is increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure reading produces maximum perspectival divergence from the other two readings of the same kernel. From the infrastructure perspective, money emergence is technological and inevitable (once banks can move it electronically, money is digital — the boundary is hard and empirical). From the conceptualization reading, emergence is definitional and interpretive (money became digital when economists redefined monetary aggregates to include electronic deposits — the boundary is soft and theoretical). From the consumer-holdings reading, emergence is experiential and democratic (money is digital only when consumers control digital holdings — the boundary is practical and individual). The infrastructure reading generates snare perception for non-bank actors (trapped outside the definition process) while the beneficiaries experience rope (pure coordination of transfers). This gap reveals that the 'emergence of digital money' is not a single event but a contested transition where different stakeholders anchor to different observable phenomena. The false summit (perspective 6) naturalizes what is actually a political choice about who controls the infrastructure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (banking infrastructure operators, central banks) have arbitrage exit options: they can shift between different protocol standards, move settlement to different systems, or design new infrastructure. Their d-values are low (0.05-0.20 range), producing negative or near-zero effective extraction from their perspective — they experience pure coordination benefit. Victims (non-bank actors, consumers) have trapped or constrained exit options: they cannot redesign the infrastructure and face high switching costs. Their d-values are high (0.80-0.95 range), producing high effective extraction from their perspective — they experience the constraint as coercive. The organized non-bank intermediaries occupy middle ground (d ~0.55) with constrained exit: they can access infrastructure but cannot control standards. Central banks are institutionally constrained (d ~0.50) because they depend on private infrastructure for international settlement despite their nominal authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The infrastructure reading resolves mandatrophy by showing that the three readings represent DIFFERENT STRUCTURED PERSPECTIVES on the same underlying event, not contradictory claims about what 'really' happened. The constraint does not suffer from type confusion (rope vs snare indeterminacy) because the ε values reflect genuine structural differences: infrastructure operators experience rope (coordination benefit), consumers experience snare (trapped extraction), non-banks experience tangled rope (mixed). The classification diversity is not a sign of analytical failure — it is the signature of a contested kernel where different institutional positions anchor to different observable phenomena. The false summit perspective (mountain) is the risk: naturalizing infrastructure control as inherent to money-ness rather than recognizing it as a political-institutional choice about who controls the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_necessity_vs_ownership,
    'Is infrastructure control over money definition a necessary property of monetary systems (natural law) or a contingent institutional arrangement that could be differently structured?',
    'Historical analysis of alternative monetary architectures (commodity-backed, community-issued, distributed ledger systems); identification of cases where money definition proceeded from non-infrastructure-controlling entities. Comparison of M0 (state money) vs M2-M5 (infrastructure-dependent money) definitions and their stability.',
    'If necessary: infrastructure reading holds as foundation; all other readings are aspirational. If contingent: infrastructure reading is a political choice; alternative readings become structurally viable and competing readings are equally valid. This determines whether the constraint is mountain or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_necessity_vs_ownership, conceptual, 'Whether infrastructure control is necessary or contingent to money definition').

omega_variable(
    boundary_specificity_across_readings,
    'Does the infrastructure reading of money emergence (ATMs, ACH, SWIFT as the boundary) capture the same phenomenon that the conceptualization and consumer-holdings readings identify, or are these fundamentally measuring different emergences?',
    'Cross-reading comparison: (1) What did each reading identify as ''money'' before and after the boundary? (2) Do the three readings agree on whether mid-tier actors (non-bank intermediaries) had access to monetary function before vs. after? (3) What empirical facts would force one reading to revise its boundary placement? If readings differ on what counts as evidence, they are reading different kernels.',
    'If measuring same emergence: the three readings represent legitimate perspectival ambiguity; engine can model all three as valid. If measuring different emergences: the readings should be decomposed into separate kernels with separate infrastructure, conceptualization, and consumer-holdings constraints for each.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_specificity_across_readings, conceptual, 'Whether all three readings measure the same emergence event or different events').

omega_variable(
    extraction_vs_coordination_allocation,
    'Is the asymmetric benefit flowing to infrastructure operators (SWIFT fees, ACH operator profits, settlement risk premiums) properly characterized as extraction within a coordination system, or is this extraction better modeled as the natural rent on control of a critical infrastructure?',
    'Comparative institutional analysis: (1) Can alternative infrastructure exist at similar cost and scale? (2) Are SWIFT/ACH profits and operating margins higher than competitive benchmarks for communication/settlement services? (3) What would happen if infrastructure were shifted to public ownership (Fed-operated SWIFT-equivalent, Treasury-run ACH)? Measurement of what portion of operator profit derives from coordination necessity vs. monopoly position.',
    'If extraction is separable from coordination: operators could be compensated for coordination work at fair rates while excess extraction is removed; constraint could shift from tangled_rope to rope. If rent is structural: any operator would enjoy similar monopoly position; constraint is inherently tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_allocation, empirical, 'Whether infrastructure operator profits are coordination cost or extractive rent').

omega_variable(
    cbdc_foreclosure_of_infrastructure_reading,
    'Would central bank digital currencies (CBDCs) that bypass private infrastructure operators foreclose the infrastructure reading, forcing reclassification of the money definition boundary?',
    'Analysis of CBDC architectures: (1) Do CBDCs eliminate infrastructure dependency or merely relocate it from SWIFT/ACH to central bank digital systems? (2) Can CBDCs achieve same settlement speed and scale without equivalent infrastructure? (3) If CBDCs become primary money, does the infrastructure reading still apply (with central bank as operator instead of private), or does the constraint dissolve?',
    'If CBDCs foreclose infrastructure control: the reading''s foundational axiom (infrastructure control defines money) is invalidated; reading becomes overridden. If CBDCs merely shift operator: reading persists with different beneficiary (central bank instead of private operator). This determination affects whether the kernel remains contested or one reading achieves dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdc_foreclosure_of_infrastructure_reading, empirical, 'Whether CBDCs foreclose the infrastructure control reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_theater_t0_pre_atm, infrastructure_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(infra_theater_t5_ach, infrastructure_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(infra_theater_t10_swift, infrastructure_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(infra_extract_t0_pre_atm, infrastructure_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(infra_extract_t3_atm_era, infrastructure_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(infra_extract_t5_ach_consolidation, infrastructure_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(infra_extract_t10_swift_dominance, infrastructure_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(infrastructure_reading, conceptualization_reading).
narrative_ontology:affects_constraint(infrastructure_reading, consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The infrastructure reading is the middle boundary of three competing readings of the same kernel (digital_money_emergence_boundary). Upstream: conceptualization_reading anchors to economic redefinition of monetary aggregates (earlier, more theoretical boundary). Downstream: consumer_holdings_reading anchors to consumer access to digital money (later, more experiential boundary). All three share the same beneficiary/victim structure but locate the emergence event at different points. Each story has independent ε; they are not measurable variants of one story but genuinely distinct constraints on the emergence process.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(infrastructure_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
