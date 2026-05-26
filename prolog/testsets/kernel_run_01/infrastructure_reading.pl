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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Digital Money as Institutional Infrastructure (ACH/SWIFT Reading)
 *   domain: monetary_theory/financial_infrastructure/payment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the infrastructure reading of the contested
 *   kernel 'digital money origin.' The reading dates digital money's
 *   emergence to 1972-1977, when ACH and SWIFT protocols enabled
 *   institutional banks to electronically transfer value at scale. Under this
 *   reading, digital money is fundamentally an institutional infrastructure
 *   phenomenon — a ledger coordination mechanism among licensed, monitored
 *   deposit-taking institutions. The constraint exhibits high perspectival
 *   diversity because the definition embeds a regulatory choice
 *   (institutional gatekeeping) that benefits some actors (banks, central
 *   banks) while imposing extraction on others (retail consumers, alternative
 *   payment systems). The infrastructure reading is not the only legitimate
 *   reading: the consumer_access_reading dates digital money to when
 *   individuals can directly hold digital bearer instruments (late
 *   2010s/2020s); the peer_to_peer_reading dates it to when settlement can
 *   occur without institutional intermediation (Bitcoin 2009, Ethereum
 *   later). These are not three different observations of one constraint —
 *   they are three different constraints with different ε values, different
 *   beneficiary/victim structures, and different classifications. This file
 *   documents the infrastructure reading alone.
 *
 * KEY AGENTS:
 *   - Deposit-Taking Institutions (Banks, Credit Unions): Primary beneficiary (institutional/arbitrage) — capture fees, spreads, and liquidity premium from mandatory routing of all value transfers through their ledgers
 *   - Central Banks: Institutional beneficiary (institutional/arbitrage) — maintain monetary policy authority and financial stability surveillance through control of settlement infrastructure
 *   - Clearing Houses (ACH, SWIFT, etc.): Institutional beneficiary (institutional/arbitrage) — extract membership fees and processing margins while providing genuine coordination service
 *   - Retail Consumers: Primary victim (powerless/trapped) — must route all digital transactions through institutional intermediaries; cannot directly access or control settlement layer; bear extraction as fees and latency
 *   - Alternative Payment Systems (Blockchain, P2P networks): Secondary victim (organized/constrained) — face regulatory barriers, lack critical mass for mandatory adoption, and operate in architectural dependence on the institutional infrastructure they seek to replace
 *   - Regulatory Authorities (Central Banks, Financial Regulators): Institutional actor with dual role — maintains the infrastructure as delegated authority but partially captured by the institutions whose interests the infrastructure serves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_reading, 0.38).
domain_priors:suppression_score(infrastructure_reading, 0.45).
domain_priors:theater_ratio(infrastructure_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(infrastructure_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(infrastructure_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(infrastructure_reading, "Digital Money as Institutional Infrastructure (ACH/SWIFT Reading)").
narrative_ontology:topic_domain(infrastructure_reading, "monetary_theory/financial_infrastructure/payment_systems").

domain_priors:requires_active_enforcement(infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(infrastructure_reading, formalized).
narrative_ontology:cs_authority_grounding(infrastructure_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(infrastructure_reading).
narrative_ontology:cs_kernel_id(infrastructure_reading, digital_money_origin).
narrative_ontology:cs_reading_relation(infrastructure_reading, digital_money_consumer_access_reading, coexists_with).
narrative_ontology:cs_reading_relation(infrastructure_reading, digital_money_peer_to_peer_reading, coexists_with).
narrative_ontology:cs_axiom(infrastructure_reading, foundational, digital_money_institutional_infrastructure_constitutive).
narrative_ontology:cs_axiom_status(digital_money_institutional_infrastructure_constitutive, holdable).
narrative_ontology:cs_axiom(infrastructure_reading, foundational, settlement_requires_centralized_ledger).
narrative_ontology:cs_axiom_status(settlement_requires_centralized_ledger, overridden).
narrative_ontology:cs_reference_frame(infrastructure_reading, institutional_infrastructure_monopoly).
narrative_ontology:cs_drift_state(infrastructure_reading, blockchain_technical_maturation_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_reading, deposit_taking_institutions).
narrative_ontology:constraint_beneficiary(infrastructure_reading, central_banks).
narrative_ontology:constraint_beneficiary(infrastructure_reading, interbank_clearing_houses).
narrative_ontology:constraint_victim(infrastructure_reading, retail_access_asymmetry).
narrative_ontology:constraint_victim(infrastructure_reading, alternative_payment_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL CONSUMER (SNARE) — Trapped within the institutional infrastructure. Cannot directly participate in digital money creation or transfer; must route all transactions through licensed deposit-taking institutions. Faces persistent extraction as spreads, fees, and latency remain embedded in the institutional layer. No exit option: alternative payment systems exist but lack critical mass for daily settlement.
constraint_indexing:constraint_classification(infrastructure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LARGE COMMERCIAL BANK (ROPE) — Primary beneficiary. Experiences the constraint as coordination: ACH and SWIFT enable settlement efficiency, reduce counterparty risk, and provide monopoly on value transfer. The institutional infrastructure is genuinely coordinating a difficult collective action problem (synchronizing transfers across disparate ledgers) while simultaneously extracting margin. Benefits far outweigh costs.
constraint_indexing:constraint_classification(infrastructure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMMUNITY BANK / CREDIT UNION (TANGLED ROPE) — Constrained by mandatory participation in the SWIFT/ACH infrastructure. Gains genuine coordination benefit (access to clearing, settlement, liquidity) but bears extraction cost: mandatory membership fees, compliance burden, and architectural dependence on decisions made by large banks. Cannot exit without losing market access; limited voice in governance.
constraint_indexing:constraint_classification(infrastructure_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CENTRAL BANK (PITON) — Maintains the infrastructure as a delegated authority, but the functional necessity has partially degraded. In early ACH/SWIFT era (1970s-1990s), the infrastructure solved a genuine coordination problem: synchronizing dispersed ledgers at scale was technically hard. By 2020s, blockchain and distributed ledgers offer technical alternatives, yet central banks continue maintaining the infrastructure largely through inertia and regulatory advantage. Theater ratio high: much activity is regulatory compliance and legacy system maintenance rather than functional necessity.
constraint_indexing:constraint_classification(infrastructure_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN FINANCE / BLOCKCHAIN COALITION (SCAFFOLD) — Organized challengers (cryptographic networks, fintech platforms, regulatory innovators) see digital money infrastructure as a temporary coordination stage with a sunset. Distributed ledgers and non-custodial protocols are building alternative pathways that bypass central institutional gatekeeping. The coalition has agency and sees exit possibilities. Sunset clause implicit: as decentralized infrastructure matures and regulatory frameworks evolve, the institutional infrastructure's monopoly weakens.
constraint_indexing:constraint_classification(infrastructure_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, digital money requires some infrastructure to solve the double-spending problem and maintain a canonical ledger. The idea that 'money must be institutionally infrastructure' appears as an immutable constraint on any settlement system. However, the structural data contradicts the mountain classification — the infrastructure reading is naturalizing a specific institutional choice (centralized, bank-mediated) that is contingent, not necessary.
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
    constraint_indexing:constraint_classification(infrastructure_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_reading, TR),
    TR >= 0.70.

:- end_tests(infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The institutional infrastructure provides genuine coordination value — synchronizing dispersed ledgers at scale was technically difficult in 1972 and remained genuinely complex through the 1990s. The extractiveness value reflects that a portion of the fees and spreads extracted from retail users represents legitimate coordination cost, while a portion represents monopoly rent. As the measurement interval progresses (1972 → 2012), extractiveness rises slightly (0.15 → 0.38) as technical complexity decreases but regulatory barriers and fee structures increase. This trajectory suggests the coordination value is relatively stable while the extraction overlay grows. Suppression (0.45): Moderate. Barriers to exit the infrastructure exist but are not maximal. Regulatory barriers (licensing requirements, mandatory participation in clearing systems) are high for institutional actors attempting to establish alternatives, but retail consumers theoretically have exit options (cash, informal transfer systems, barter). In practice, the infrastructure's critical mass makes functional exit nearly impossible in a modern economy. Theater ratio (0.55): Moderate-high. Early in the interval (1970s), theater was low — ACH and SWIFT performed essential technical functions that were genuinely difficult. By the 2010s, distributed ledger technology had demonstrated that the core functions (double-spend prevention, canonical ledger maintenance) could be implemented differently. The institutional infrastructure persists increasingly through regulatory mandate and path dependency rather than technical necessity. The theater ratio rise (0.25 → 0.55) reflects this shift.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces near-maximal perspectival diversity. The retail consumer trapped in the system sees pure extraction (Snare): they have no choice but to pay fees, accept delays, and trust institutional intermediaries with no technical oversight. The commercial bank sees coordination (Rope): the infrastructure genuinely solves their settlement problem and provides market access. The community bank sees mixed experience (Tangled Rope): genuine benefit from access to clearing, but also extraction cost from mandatory participation and compliance burden. The central bank sees a degraded ritual (Piton): the infrastructure solves problems that were real in 1972 but are largely solved by alternative technologies by 2012; it persists through regulatory authority and path dependency. The blockchain coalition sees a temporary problem being solved (Scaffold): alternative infrastructure is being built, regulatory frameworks are shifting, and the sunset of the institutional monopoly is plausible. The civilizational observer risks seeing immutability (Mountain): 'settlement systems must be institutionally managed' appears as a law of economics, but the structural data reveals this as a false summit — institutional management is one solution, not the only solution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from: (1) whether the agent benefits from the constraint (low d) or bears costs (high d), and (2) whether they can exit the constraint. Beneficiaries with arbitrage options (large banks, central banks) get low d, producing negative or low effective extraction chi — they experience the constraint as coordination. Trapped agents bearing costs (retail consumers) get high d, producing high chi — they experience maximum extraction. Constrained agents with mixed benefit/cost (community banks) get moderate d, producing moderate chi. The piton classification derives from the theater gate (theater_ratio ≥ 0.70 in later measurements), not from directionality. The mountain classification at civilizational scope is identified as a false summit by the presence of structural beneficiaries (deposit-taking institutions) — the constraint naturalizes an institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing the infrastructure reading from alternative readings. A naive analyst might say 'digital money is just money that exists digitally' (tautology) or 'digital money is whatever central banks define it to be' (regulatory circularity). The infrastructure reading breaks the circle by anchoring 'digital money' to a specific technology (ACH/SWIFT) and a specific historical moment (1972-1977) when that technology emerged. This reading makes falsifiable claims: digital money did not exist before institutions developed infrastructure to enable it; digital money will cease to exist (in this reading) if the infrastructure is abandoned or replaced. The beneficiary/victim structure is now clear and measurable. The classification (Tangled Rope) reflects a genuine hybrid: the infrastructure provides coordination (solves the double-spending problem) and extraction (controls who can access the ledger). The mandatrophy is resolved not by finding 'the' correct type but by showing that the infrastructure reading is one coherent, falsifiable definition among others — each with its own ε, its own beneficiaries/victims, and its own classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is digital money fundamentally defined by institutional infrastructure (ACH/SWIFT), or is institutional mediation merely one implementation?',
    'Definitional archaeology: trace how the term ''digital money'' is used across central bank documents, academic literature, and regulatory frameworks. Does the definition presuppose institutional infrastructure as constitutive or as contingent?',
    'If infrastructure is constitutive: this reading''s axioms are foundational and the sibling readings (consumer_access, peer_to_peer) are secondary interpretations or functional variants. If infrastructure is contingent: this reading is one reading among equals, and the sibling readings are not less legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether institutional infrastructure is constitutive or contingent to digital money definition').

omega_variable(
    successor_technology_sufficiency,
    'Do distributed ledgers and peer-to-peer protocols achieve functional equivalence to centralized ACH/SWIFT for payment settlement?',
    'Empirical comparison: transaction finality times, settlement risk, throughput, cost, and accessibility across blockchain networks (Lightning, Polygon, Solana) vs traditional payment systems. Regulatory assessment of non-custodial settlement fitness for monetary policy.',
    'If functionally equivalent: the scaffold perspective is confirmed as real structural pathway, and the sunset clause is active. This reading''s monopoly claim weakens. If not equivalent: alternatives remain aspirational, and the infrastructure reading''s extraction mechanism is more durable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(successor_technology_sufficiency, empirical, 'Whether distributed ledgers provide functional alternatives to centralized settlement').

omega_variable(
    regulatory_capture_depth,
    'To what extent does regulatory authority over digital money definition remain with the institutions that control the infrastructure, versus independent monetary authority?',
    'Policy analysis: who controls definitional authority in central bank digital currency (CBDC) frameworks? Do regulatory bodies independently specify what counts as digital money, or does definition follow institutional capacity? Cross-jurisdiction comparison.',
    'If regulators are captured by infrastructure institutions: the extraction mechanism is more durable and the piton perspective is undervalued. If regulatory authority is independent: the central bank perspective is genuine coordination rather than theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Whether regulatory authority over digital money is independent or captured by infrastructure institutions').

omega_variable(
    measurement_collapse_meaning,
    'Does the collapse of M4/M5 monetary aggregates under distributed ledger adoption indicate ontological change in what ''money'' is, or merely a measurement problem?',
    'Historical precedent: how did economists handle previous shifts in payment mechanisms (checks, credit cards) that changed aggregate definitions? Do definitional shifts always indicate ontological instability?',
    'If ontological: the infrastructure reading is in terminal crisis and the peer-to-peer reading becomes necessary. If measurement: the infrastructure reading survives by redefining aggregates, and extraction mechanisms persist under new accounting frameworks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_collapse_meaning, conceptual, 'Whether M-aggregate collapse indicates ontological or measurement shift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_reading, 1972, 2012).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_theater_1970s, infrastructure_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(infra_theater_1990s, infrastructure_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(infra_theater_2010s, infrastructure_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(infra_extract_1970s, infrastructure_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(infra_extract_1990s, infrastructure_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(infra_extract_2010s, infrastructure_reading, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(infrastructure_reading, 0.12).
narrative_ontology:affects_constraint(infrastructure_reading, digital_money_consumer_access_reading).
narrative_ontology:affects_constraint(infrastructure_reading, digital_money_peer_to_peer_reading).
narrative_ontology:affects_constraint(infrastructure_reading, monetary_aggregate_collapse).
narrative_ontology:affects_constraint(infrastructure_reading, payment_system_rent_extraction).

% DUAL FORMULATION NOTE:
% The infrastructure reading is the upstream constraint for the other readings in the digital_money_origin kernel family. The consumer_access and peer_to_peer readings describe what happens when alternative technologies challenge the institutional monopoly. The payment_system_rent_extraction constraint is a narrower focus on the extraction mechanism alone. The monetary_aggregate_collapse constraint describes what happens to measurement systems when the infrastructure reading's foundational assumptions fail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(infrastructure_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
