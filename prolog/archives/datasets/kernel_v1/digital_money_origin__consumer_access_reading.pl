% ============================================================================
% CONSTRAINT STORY: digital_money_origin__consumer_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__consumer_access_reading, []).

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
 *   constraint_id: digital_money_origin__consumer_access_reading
 *   human_readable: Digital Money Origin: Consumer Access Reading
 *   domain: monetary_theory/technology_history/financial_infrastructure
 *
 * SUMMARY:
 *   The consumer access reading of digital money origin frames the emergence
 *   of direct consumer-held electronic value (1990s-2000s) as the moment
 *   money became genuinely detached from deposit institutions. Under this
 *   reading, 'digital money exists when individuals can hold and transact
 *   electronic value directly, not merely as bank account entries.' This
 *   reading dates the origin to e-purses, online banking systems, and early
 *   payment service providers operating in the regulatory gap before
 *   comprehensive digital asset frameworks emerged. The reading treats the
 *   period of monetary authority measurement failure (M4/M5 collapse,
 *   inability to track digital holdings) as structural evidence of genuine
 *   ontological shift in what money is — no longer synonymous with bank
 *   deposits, but rather a tokenized value held and controlled directly by
 *   the consumer. The constraint this creates is tangled: fintech firms
 *   benefit from the regulatory gap and new market opportunity, but the
 *   system also coordinates new payment modalities that genuinely reduce
 *   transaction costs. Traditional banking institutions lose direct-contact
 *   margin but preserve their payment processing role. Consumers gain direct
 *   access but face new risks (custodian failure, regulatory uncertainty).
 *   Central monetary authorities experience loss of measurement and partial
 *   loss of control, responding with performative regulatory theater (KYC,
 *   AML frameworks) that asserts authority without recovering it.
 *
 * KEY AGENTS:
 *   - Depositors Without Direct Digital Access (powerless/trapped): Pre-1990s baseline — held money only as bank account ledger entries, with no direct electronic value access. Structurally dependent on banking intermediation.
 *   - Traditional Banking Institutions (organized/constrained): Coordinated domestic payments (genuine rope function) while extracting rent via deposit-taking monopoly. Lost margin to fintech but preserved payment infrastructure role.
 *   - Early Fintech Firms (moderate/constrained): E-purse operators, online payment providers, digital wallet creators. Beneficiaries during regulatory gap (1994-2000) but constrained by legal uncertainty about digital asset custody.
 *   - Fintech Capital and Investors (institutional/arbitrage): Primary beneficiaries. Positioned to capture first-mover advantage and network effects in new market. Experienced constraint as enabling, not limiting.
 *   - Central Bank Monetary Authority (institutional/arbitrage): Experienced loss of measurement authority (M-aggregate targeting became impossible as digital holdings outpaced tracking). Responded with surveillance theater (regulatory frameworks) rather than control recovery.
 *   - Consumer Empowerment Coalition (organized/constrained): Open banking advocates, consumer protection groups, technology standards bodies. See direct digital money as temporary coordination gap with sunset — mature institutional frameworks (CBDC design, stablecoin regulation, custody rules) will normalize consumer digital access.
 *   - Analytical Observer (analytical/analytical): Treats direct digital consumer access as technologically inevitable consequence of electronic infrastructure. Risks naturalizing what was contingent institutional conflict.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__consumer_access_reading, 0.38).
domain_priors:suppression_score(digital_money_origin__consumer_access_reading, 0.48).
domain_priors:theater_ratio(digital_money_origin__consumer_access_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__consumer_access_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(digital_money_origin__consumer_access_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__consumer_access_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__consumer_access_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__consumer_access_reading, "Digital Money Origin: Consumer Access Reading").
narrative_ontology:topic_domain(digital_money_origin__consumer_access_reading, "monetary_theory/technology_history/financial_infrastructure").

domain_priors:requires_active_enforcement(digital_money_origin__consumer_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__consumer_access_reading, '6cae96db-df30-4bd8-889d-edd85fa568dc').
narrative_ontology:cs_kernel_codification('6cae96db-df30-4bd8-889d-edd85fa568dc', distributed).
narrative_ontology:cs_authority_grounding('6cae96db-df30-4bd8-889d-edd85fa568dc', extraction).
narrative_ontology:cs_interpretation_layer_present('6cae96db-df30-4bd8-889d-edd85fa568dc').
narrative_ontology:cs_reading_relation('6cae96db-df30-4bd8-889d-edd85fa568dc', digital_money_origin__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cae96db-df30-4bd8-889d-edd85fa568dc', digital_money_origin__peer_to_peer_reading, influences).
narrative_ontology:cs_axiom('6cae96db-df30-4bd8-889d-edd85fa568dc', foundational, direct_consumer_electronic_value_holding_is_possible).
narrative_ontology:cs_axiom_status(direct_consumer_electronic_value_holding_is_possible, holdable).
narrative_ontology:cs_axiom_grounding('6cae96db-df30-4bd8-889d-edd85fa568dc', direct_consumer_electronic_value_holding_is_possible, empirically_contingent).
narrative_ontology:cs_axiom('6cae96db-df30-4bd8-889d-edd85fa568dc', foundational, money_definition_includes_institutional_independence).
narrative_ontology:cs_axiom_status(money_definition_includes_institutional_independence, holdable).
narrative_ontology:cs_axiom_grounding('6cae96db-df30-4bd8-889d-edd85fa568dc', money_definition_includes_institutional_independence, deontological).
narrative_ontology:cs_reference_frame('6cae96db-df30-4bd8-889d-edd85fa568dc', bank_deposit_monetary_standard).
narrative_ontology:cs_drift_state('6cae96db-df30-4bd8-889d-edd85fa568dc', post_regulatory_gap_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6cae96db-df30-4bd8-889d-edd85fa568dc', '').
narrative_ontology:cs_kernel_id(digital_money_origin__consumer_access_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__consumer_access_reading, early_fintech_firms).
narrative_ontology:constraint_beneficiary(digital_money_origin__consumer_access_reading, payment_service_providers).
narrative_ontology:constraint_beneficiary(digital_money_origin__consumer_access_reading, financial_technology_investors).
narrative_ontology:constraint_victim(digital_money_origin__consumer_access_reading, traditional_banking_institutions).
narrative_ontology:constraint_victim(digital_money_origin__consumer_access_reading, central_bank_monetary_authority).
narrative_ontology:constraint_victim(digital_money_origin__consumer_access_reading, depositor_account_holders_without_direct_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPOSITORS WITHOUT DIRECT ACCESS (SNARE) — Trapped within the traditional banking system; money exists only as bank account ledger entries, not as directly controllable digital assets. Exit is structurally blocked: consumer cannot access electronic value without institutional intermediation. No alternative distribution channel existed until late 1990s. Maximum extraction — full dependence on bank as sole custodian.
constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRADITIONAL BANKING (TANGLED ROPE) — Institutional victims and coordinating actors simultaneously. Banking system genuinely coordinated domestic payments and credit allocation (rope function), but also extracted rent through deposit-taking monopoly and transaction fees (extraction mechanism). The emergence of direct consumer digital money undercuts their fee extraction but preserves coordination at scale. Constrained exit: banks cannot abandon payment processing, only lose direct-contact margin.
constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY FINTECH (TANGLED ROPE) — Emerged to coordinate new digital value transfer modality (e-purses, online payment systems), but also extracted market share and network effects from traditional banks. Constrained by regulatory uncertainty (1994-2000 gap): no legal framework for non-bank digital value holding. Beneficiary in growth phase (access to new market), victim of regulatory risk (constraint could be reversed).
constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FINTECH CAPITAL (ROPE) — Primary beneficiary during emergence phase. Positioned to arbitrage the regulatory gap (1994-2000) where digital money could operate without bank charter requirements. Pure coordination benefit (enabling new transactions) combined with first-mover margin capture. Net beneficiary — extraction flows toward this agent; they experience constraint as enabling, not limiting.
constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRAL BANK AUTHORITY (PITON) — Money supply measurement (M1/M2/M3 aggregates) was the primary institutional function. Direct consumer digital money (M4/M5 in this reading) breaks the measurement framework: central banks cannot easily track or control value held outside deposit institutions. The institutional response is largely performative: regulatory frameworks (Know Your Customer, Anti-Money Laundering) are theater designed to maintain measurement illusion while the underlying monetary authority has partially atrophied. High theater ratio reflects that regulations assert control without achieving it.
constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSUMER EMPOWERMENT (SCAFFOLD) — Organized agents (open banking initiatives, consumer protection advocates, technology standards bodies) see direct digital money as temporary coordination problem with built-in sunset. As consumer-controlled digital wallets, blockchain infrastructure, and decentralized finance mature, direct consumer access becomes normative. Extraction mechanism (the regulatory gap that privileged fintech over consumers) self-dissolves as new institutional frameworks (digital asset custody rules, stablecoins regulation, CBDC design) resolve the gap. Sunset is real — estimated 15-20 years for institutional adaptation.
constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL/NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, electronic transmission of value is a natural consequence of digital technology. Once electronic payment infrastructure exists (automated clearing houses, SWIFT networks), consumers eventually gain direct access — this is treated as inevitable technological progress, not contingent institutional design. However, the structural data contradicts the mountain classification. The regulatory gap (1994-2000) was not technologically forced; it reflected policy choices and institutional power. The engine will compute this as a false summit, revealing that 'technological inevitability' naturalizes what was actually contingent institutional conflict.
constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__consumer_access_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_money_origin__consumer_access_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_money_origin__consumer_access_reading, TR),
    TR >= 0.70.

:- end_tests(digital_money_origin__consumer_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The regulatory gap (1994-2000) created genuine asymmetry: fintech firms could capture first-mover margin and network effects without banking charter requirements. Traditional banks lost some customer relationship margin. But the extraction is not maximal because the constraint also enabled genuine new coordination (faster settlement, lower transaction costs for some transactions). The measurement trajectory (0.28 → 0.38) reflects rising extraction as fintech firms consolidated market share and regulatory uncertainty persisted. Suppression (0.48): Moderate-high. Consumers lacked direct digital access pre-1990s (structural barrier). Regulatory frameworks (1994-2000 gap, later AML/KYC rules) created legal uncertainty about digital asset custody (institutional suppression). But suppression is not total — some institutional alternatives emerged (early e-cash systems, digital wallet platforms), and regulatory gaps were exploitable. Theater ratio (0.62): Moderately high. Central bank response to digital money emergence was substantially performative: KYC/AML regulations asserted measurement and control authority without recovering it. The regulations created reporting theater (transaction reporting, account verification) that masked the underlying loss of monetary authority. As scaffolding toward mature institutional frameworks, theater should eventually decline.
 *
 * PERSPECTIVAL GAP:
 *   The consumer access reading generates a sharp perspectival split that maps to the three sibling readings. From the consumer depositor's view, digital money is liberation from banking intermediation (Snare → Rope narrative). From the fintech firm's view, digital money is market opportunity (Rope/Tangled Rope — coordination plus extraction). From the central bank's view, digital money is loss of institutional function (Piton — measurement authority atrophied, response is theater). From the broader consumer empowerment coalition's view, digital money is a temporary coordination gap heading toward sunset (Scaffold — mature frameworks will normalize access). The analytical observer risks treating the whole emergence as technologically inevitable (Mountain) when it was actually contingent on institutional choices about whether to allow non-bank digital value holding. This perspectival spread shows why the constraint is tangled_rope at the base level: it coordinates new payment capabilities (genuine coordination benefit) while extracting margin from the regulatory gap (institutional capture).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Fintech firms (beneficiaries with arbitrage exit) derive low d-values, experiencing negative effective extraction (they benefit from the constraint). Traditional banks (victims with constrained exit — cannot abandon payment processing, only lose margin) derive medium-high d-values. Consumers without pre-1990s direct access (victims with trapped exit) derive maximum d-values, experiencing maximum extraction. The piton perspective (central bank) derives d from arbitrage exit combined with institutional victim status (loss of measurement function), producing moderate d despite institutional power — the measurement loss is real and costly, but the institution retains high absolute power. The scaffold perspective's organized agents (constrained exit, beneficiary-victim mix) derive moderate d — they see the constraint as temporary and surmountable through institutional design. The analytical observer's d derives from analytical power and analytical exit, producing moderate d consistent with the critical distance needed to identify false summits.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification (tangled_rope) reflects genuine mixed coordination-extraction structure, not ambiguity between coordination and extraction. The coordination function is real: early payment systems solved genuine transaction speed and cost problems. The extraction is real: fintech firms captured first-mover margin during the regulatory gap, and banking institutions lost customer relationship margin. The constraint is not a pure extraction mechanism hidden as coordination (snare), nor pure coordination (rope) — it is genuinely both. The measurement trajectory (extractiveness rising from 0.28 to 0.38, theater rising from 0.45 to 0.62) shows extraction accumulating as fintech consolidation proceeded and regulatory uncertainty persisted. The scaffold perspective (consumer empowerment coalition seeing digital money as temporary coordination gap with sunset) shows why the constraint can be legitimately temporary: as regulatory frameworks mature (stablecoin custody rules, CBDC design, open banking standards), the extraction mechanism (regulatory gap arbitrage) self-dissolves and the coordination mechanism persists. This is the hallmark of genuine scaffolding — the extraction was contingent on institutional gap, not structural to the technology itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_gap_causation,
    'Was the 1994-2000 regulatory gap enabling direct digital money a structural outcome of technological pace outrunning regulation (technology inevitability), or a deliberate institutional design to preserve banking monopoly on customer relationships?',
    'Historical analysis of regulatory proceedings, central bank correspondence, and industry lobbying records. Distinguish between: (a) regulatory inertia due to technical complexity, (b) deliberate delay to protect banking interests, (c) genuine legal uncertainty about digital asset treatment.',
    'If (a): the constraint is technological inevitability (mountain). If (b): the constraint is institutional extraction (snare/tangled rope). If (c): the constraint is genuine coordination gap (rope/scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_gap_causation, empirical, 'Whether the regulatory gap was accidental or deliberate institutional design').

omega_variable(
    money_definition_ontological_shift,
    'Does direct digital consumer access constitute a genuine ontological shift in what ''money'' means (as this reading claims: money detached from deposit institutions), or is it merely a new distribution channel for the same monetary substance (as infrastructure reading claims)?',
    'Comparative analysis of monetary functions across readings: store of value, medium of exchange, unit of account, deferred payment. Does direct digital money preserve all four functions outside institutional frameworks? Does M4/M5 collapse represent loss of measurement or revelation of hidden transactions?',
    'If ontological shift: consumer_access_reading correctly dates digital money origin to 1990s-2000s. If distribution channel only: infrastructure_reading is correct (origin is earlier, with interbank SWIFT systems). This determines the fundamental nature of digital money.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(money_definition_ontological_shift, conceptual, 'Whether digital money is ontological shift or distribution channel').

omega_variable(
    peer_to_peer_capability_requirement,
    'Does direct digital consumer access require peer-to-peer settlement without intermediaries (peer_to_peer_reading), or can institutional intermediaries (payment processors, digital wallet custodians) provide direct consumer access while maintaining institutional architecture?',
    'Empirical assessment of existing digital payment systems: can PayPal, Venmo, digital wallets function as ''direct consumer digital money'' while maintaining institutional custody? Does the absence of true peer-to-peer settlement disqualify them from the reading''s definition?',
    'If intermediaries suffice: consumer_access_reading is viable and broader. If peer-to-peer required: consumer_access_reading conflates access with settlement, and peer_to_peer_reading provides the sharper distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peer_to_peer_capability_requirement, conceptual, 'Whether direct consumer access requires peer-to-peer settlement').

omega_variable(
    monetary_authority_loss_of_control,
    'Does the emergence of direct digital consumer money represent a genuine loss of central bank monetary control, or merely a shift in control mechanisms from quantity regulation (M1/M2 targeting) to surveillance and transaction monitoring?',
    'Comparison of pre- and post-digital-money central bank operating procedures. Did direct digital money reduce effectiveness of interest rate policy, reserve requirement policy, or quantitative easing? Or did central banks adapt instruments (negative interest rates, wealth taxation, asset seizure rules)?',
    'If genuine loss of control: central bank piton classification is correct (institutional function has atrophied). If control shifted: central bank retains functional authority through new means (surveillance piton rather than measurement piton). This affects whether the monetary authority''s perspective should be piton or something closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_authority_loss_of_control, empirical, 'Whether digital money represents loss of central bank control or shift to new control mechanisms').

omega_variable(
    definition_scope_boundary,
    'Does ''digital money'' in this reading include only consumer-directly-held assets (e-purses, digital wallets, crypto), or also institutional custody of digital assets (bank digital deposit accounts, platform holdings, stablecoin custodians)? Does the boundary lie in who holds the private key, who owns the account, or who controls settlement?',
    'Clarification of scope boundaries through reference documents and regulatory practice. Current usage spans from narrow (private-key control) to broad (any electronic transaction). Resolution determines who counts as ''victim'' (depositors without direct access) and who counts as ''beneficiary'' (early fintech).',
    'Narrow boundary: consumer_access_reading is sharper but applies to smaller set of systems. Broad boundary: reading applies more widely but conflates distinct institutional arrangements (custodied digital assets vs. directly-held digital money).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_scope_boundary, conceptual, 'Definitional scope boundary of ''directly held'' digital money').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__consumer_access_reading, 1990, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmon_ca_tr_t0, digital_money_origin__consumer_access_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(dmon_ca_tr_t3, digital_money_origin__consumer_access_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(dmon_ca_tr_t6, digital_money_origin__consumer_access_reading, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(dmon_ca_be_t0, digital_money_origin__consumer_access_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dmon_ca_be_t3, digital_money_origin__consumer_access_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(dmon_ca_be_t6, digital_money_origin__consumer_access_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dmon_ca_su_t0, digital_money_origin__consumer_access_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(dmon_ca_su_t3, digital_money_origin__consumer_access_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(dmon_ca_su_t6, digital_money_origin__consumer_access_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__consumer_access_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__consumer_access_reading, digital_money_origin__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_origin__consumer_access_reading, digital_money_origin__peer_to_peer_reading).
narrative_ontology:affects_constraint(digital_money_origin__consumer_access_reading, monetary_authority_measurement_authority).
narrative_ontology:affects_constraint(digital_money_origin__consumer_access_reading, fintech_regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel decomposes into three constraint stories, one per reading. Each reading has different beneficiary/victim structure and different origin date: (1) consumer_access_reading dates origin to 1990s-2000s (e-purses, online banking) and treats it as consumer empowerment with regulatory gap extraction. (2) infrastructure_reading dates origin earlier (automated clearing houses, 1960s-1980s) and treats it as coordination between institutions. (3) peer_to_peer_reading dates origin to 2000s-2010s (Bitcoin, blockchain) and treats it as disintermediation movement. These are not the same constraint viewed from different angles — they have different ε values (consumer_access 0.38, infrastructure ~0.15-0.25, peer_to_peer ~0.50+), different beneficiary/victim sets, and different claimed types. All three network-influence each other: infrastructure reading provides the institutional foundation that consumer access reading operates within; peer_to_peer reading proposes to displace both infrastructure and consumer_access readings; consumer_access reading's regulatory gap created the space where peer_to_peer alternatives could emerge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__consumer_access_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
