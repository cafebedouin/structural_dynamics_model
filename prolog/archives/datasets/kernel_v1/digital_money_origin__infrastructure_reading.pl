% ============================================================================
% CONSTRAINT STORY: digital_money_origin__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__infrastructure_reading, []).

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
 *   constraint_id: digital_money_origin__infrastructure_reading
 *   human_readable: Digital Money as Institutional Infrastructure (ACH/SWIFT Origin Reading)
 *   domain: monetary_theory/financial_infrastructure/technology_history
 *
 * SUMMARY:
 *   This constraint story instantiates one reading of the contested kernel
 *   'digital money origin.' The infrastructure reading defines digital money
 *   as existing when institutional infrastructure (interbank settlement
 *   networks like ACH and SWIFT) enables electronic value transfer between
 *   banks. This reading dates digital money's origin to 1972-1977, when these
 *   systems became operationally dominant in major economies. It treats
 *   digital money as fundamentally an institutional ledger phenomenon, not an
 *   individual asset or peer-to-peer exchange mechanism. It asserts that
 *   M4/M5 measurement problems in the digital era reflect measurement
 *   methodology updates, not ontological crises. It maintains that regulatory
 *   authority over 'money' legitimately remains with central banks and
 *   deposit-taking institutions. This reading is one of three live positions
 *   in contemporary monetary theory: the consumer-access reading (digital
 *   money requires individual ability to hold and transact electronic value
 *   directly) and the peer-to-peer reading (digital money requires transfer
 *   without institutional intermediaries) represent structurally different
 *   claims about what digital money fundamentally IS. All three readings
 *   coexist in contemporary discourse; none has achieved decisive
 *   institutional dominance, though the infrastructure reading has the
 *   strongest regulatory backing. The infrastructure reading's classification
 *   as Tangled Rope reflects both its genuine coordination function (solving
 *   the settlement problem at scale) and its asymmetric extraction
 *   (maintaining institutional gatekeeping over monetary legitimacy). The
 *   false-summit mountain perspective reveals how institutional readings
 *   naturalize what are actually contingent regulatory choices as immutable
 *   laws of monetary necessity.
 *
 * KEY AGENTS:
 *   - Central Banks and Regulatory Authorities: Primary beneficiary (institutional/arbitrage) — maintain definitional authority over what counts as 'money' and which systems require regulatory approval; control the boundary between legitimate and illegitimate monetary systems
 *   - Deposit-Taking Institutions: Primary beneficiary (institutional/arbitrage) — depend on institutional infrastructure reading to protect their role as mandatory intermediaries in digital value transfer; extract through fee-taking in the settlement process
 *   - Settlement Networks (ACH/SWIFT): Institutional actor (institutional/arbitrage) — coordinate genuine interbank clearing problem; benefit from regulatory mandate that all digital transactions flow through their infrastructure
 *   - Individual Depositors: Primary victim (powerless/trapped) — cannot exit institutional mediation; hold only account entries (IOUs), not digital money directly; bear suppression of alternatives (negative interest rates, account freezes, debanking)
 *   - Alternative Monetary System Designers: Secondary victim (organized/constrained) — face delegitimization as their systems are excluded from the 'money' definition; constrained by regulatory authority that reserves 'money' label for institutional systems; must rebrand as 'cryptocurrencies' or 'payment systems'
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the institutional definition as inevitable rather than contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__infrastructure_reading, 0.38).
domain_priors:suppression_score(digital_money_origin__infrastructure_reading, 0.52).
domain_priors:theater_ratio(digital_money_origin__infrastructure_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__infrastructure_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(digital_money_origin__infrastructure_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(digital_money_origin__infrastructure_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__infrastructure_reading, "Digital Money as Institutional Infrastructure (ACH/SWIFT Origin Reading)").
narrative_ontology:topic_domain(digital_money_origin__infrastructure_reading, "monetary_theory/financial_infrastructure/technology_history").

domain_priors:requires_active_enforcement(digital_money_origin__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__infrastructure_reading, 'c171b274-5412-476e-a8d6-66011c56deed').
narrative_ontology:cs_kernel_codification('c171b274-5412-476e-a8d6-66011c56deed', formalized).
narrative_ontology:cs_authority_grounding('c171b274-5412-476e-a8d6-66011c56deed', extraction).
narrative_ontology:cs_interpretation_layer_present('c171b274-5412-476e-a8d6-66011c56deed').
narrative_ontology:cs_reading_relation('c171b274-5412-476e-a8d6-66011c56deed', digital_money_origin__consumer_access_reading, coexists_with).
narrative_ontology:cs_reading_relation('c171b274-5412-476e-a8d6-66011c56deed', digital_money_origin__peer_to_peer_reading, influences).
narrative_ontology:cs_axiom('c171b274-5412-476e-a8d6-66011c56deed', foundational, interbank_settlement_defines_digital_money).
narrative_ontology:cs_axiom_status(interbank_settlement_defines_digital_money, holdable).
narrative_ontology:cs_axiom_grounding('c171b274-5412-476e-a8d6-66011c56deed', interbank_settlement_defines_digital_money, conventional).
narrative_ontology:cs_axiom('c171b274-5412-476e-a8d6-66011c56deed', foundational, central_bank_authority_over_monetary_legitimacy).
narrative_ontology:cs_axiom_status(central_bank_authority_over_monetary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c171b274-5412-476e-a8d6-66011c56deed', central_bank_authority_over_monetary_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('c171b274-5412-476e-a8d6-66011c56deed', institutional_settlement_infrastructure_primacy).
narrative_ontology:cs_drift_state('c171b274-5412-476e-a8d6-66011c56deed', contemporary_blockchain_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c171b274-5412-476e-a8d6-66011c56deed', '').
narrative_ontology:cs_kernel_id(digital_money_origin__infrastructure_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__infrastructure_reading, central_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__infrastructure_reading, deposit_taking_institutions).
narrative_ontology:constraint_beneficiary(digital_money_origin__infrastructure_reading, settlement_networks).
narrative_ontology:constraint_victim(digital_money_origin__infrastructure_reading, individual_financial_autonomy).
narrative_ontology:constraint_victim(digital_money_origin__infrastructure_reading, alternative_monetary_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL DEPOSITOR (SNARE) — Cannot exit the infrastructure constraint; all electronic transactions flow through banking intermediaries. Individual holds no direct access to digital money substrate, only a bank account entry (IOU). Trapped in institutional mediation with suppressed alternatives.
constraint_indexing:constraint_classification(digital_money_origin__infrastructure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERBANK SETTLEMENT NETWORK (ROPE) — Operates as pure coordination mechanism; ACH/SWIFT solve the genuine problem of clearing payments between thousands of banks at scale. The infrastructure benefits all participating institutions. Low extraction overhead; high coordination value. Experiences constraint as enabling their function.
constraint_indexing:constraint_classification(digital_money_origin__infrastructure_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ALTERNATIVE MONETARY SYSTEM DESIGNERS (TANGLED_ROPE) — Face the definition battle directly. The institutional reading's success in codifying 'digital money' as interbank infrastructure delegitimizes peer-to-peer and consumer-direct approaches. Genuine coordination function (banking system needs settlement) coexists with asymmetric extraction (institutional definition prevents alternative systems from claiming legitimacy as 'money'). Constrained by regulatory authority; some capacity to develop alternatives but at high legitimacy cost.
constraint_indexing:constraint_classification(digital_money_origin__infrastructure_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL BANKS AND REGULATORY AUTHORITIES (TANGLED_ROPE) — Benefit from the institutional reading by maintaining definitional control over what counts as 'money' and which systems require regulatory approval. Simultaneously provide genuine infrastructure coordination (preventing systemic risk, enabling cross-border settlement). The reading locks monetary authority into institutional intermediation, which coordinates systemic stability but extracts through regulatory gatekeeping. Active enforcement required to maintain boundary between legitimate and illegitimate monetary systems.
constraint_indexing:constraint_classification(digital_money_origin__infrastructure_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ACH/SWIFT PROTOCOLS (PITON) — The 1972-1977 institutional infrastructure (ACH in US, SWIFT internationally) persists as the canonical definition of digital money largely through institutional inertia. Newer technologies (blockchain, instant settlement, decentralized ledgers) could functionally replace these protocols, but the regulatory framework and definitional authority remain locked into the legacy system. Theater is high: much of the 'necessity' of ACH/SWIFT is now performative (instant settlement is technically possible but not permitted). Maintenance of legacy definition serves institutional interests rather than technical necessity.
constraint_indexing:constraint_classification(digital_money_origin__infrastructure_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, the institutional infrastructure reading claims that digital money is inherently tied to institutional intermediation—that the coordination problem of large-scale value transfer REQUIRES central ledgers and banking infrastructure. This perspective naturalizes what is actually a contingent regulatory and technical choice. The engine will flag this as a false summit, revealing that the 'necessity' of institutional infrastructure is not a law of nature but a legal and architectural choice.
constraint_indexing:constraint_classification(digital_money_origin__infrastructure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__infrastructure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_money_origin__infrastructure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_money_origin__infrastructure_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_money_origin__infrastructure_reading, TR),
    TR >= 0.70.

:- end_tests(digital_money_origin__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The infrastructure reading consolidates institutional control over monetary definition, which extracts through gatekeeping and regulatory privilege. However, the extraction is not maximal because genuine coordination benefits exist—settlement networks do solve real problems. The reading's extractiveness has risen from 0.22 to 0.38 over 50 years as alternative systems became technically feasible but remained suppressed through regulatory choice rather than technical necessity. Suppression (0.52): Moderate-high. Significant barriers exist to alternatives: regulatory prohibition of peer-to-peer settlement, licensing requirements for deposit-taking, mandatory use of institutional rails, legal tender laws that privilege institutional money. But suppression is not total—cryptocurrencies exist and operate, though at reduced legitimacy. Theater ratio (0.58): Moderate-high. The institutional infrastructure reading has become increasingly performative. ACH and SWIFT protocols could be replaced by instant settlement technology, but regulatory framework mandates their use because they provide settlement control and monetary policy transmission. The 'necessity' of the institutional infrastructure is now more regulatory than technical. Claimed type (Tangled Rope): The infrastructure reading genuinely coordinates settlement (rope function) while simultaneously extracting through definitional gatekeeping and institutional intermediation (snare function). The active enforcement requirement is high—regulatory authority must continuously prevent alternatives from claiming 'money' status.
 *
 * PERSPECTIVAL GAP:
 *   The infrastructure reading produces dramatically different classifications across perspectives. Beneficiaries (central banks, deposit-takers) experience the constraint as Rope—it coordinates their settlement function. The interbank network sees pure coordination (Rope). But individual depositors experience Snare—mandatory institutional mediation with no exit. Alternative systems experience Tangled Rope—the institutional reading simultaneously provides coordination framework (they operate within institutional rails) and extracts (it delegitimizes them as alternatives). The legacy ACH/SWIFT protocols appear as Piton—technically superseded but maintained through regulatory requirement. The analytical observer risks seeing Mountain—treating institutional necessity as a law of nature rather than a regulatory choice. This perspectival spread reveals that the infrastructure reading, while empirically successful (dominant in regulatory structure), is structurally extractive and contains its own delegitimization mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to this specific constraint. Central banks and deposit-taking institutions are beneficiaries with arbitrage options (can migrate to alternative systems but prefer the institutional infrastructure); they experience low effective extraction (d≈0.15-0.20). Individual depositors are victims trapped in institutional mediation; they experience maximum extraction (d≈0.95). Alternative monetary system designers are victims with constrained options (they can build alternatives but cannot claim them as 'money'); they experience high extraction (d≈0.75-0.85). The interbank settlement network sees pure coordination (d≈0.50, balanced benefit and cost). The analytical observer sees the full structure but risks naturalizing it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_kernel_instability,
    'What constitutes the foundational kernel for ''digital money''? Is it the technology enabling electronic transfer, the institutional authorization to claim monetary status, the consumer accessibility, or the absence of intermediaries?',
    'Historical comparative analysis: trace which reading was institutionalized in different jurisdictions and when; document the explicit choice points where regulatory authorities selected the institutional definition over alternatives; identify technical capabilities that existed but were suppressed by regulatory choice',
    'If technology-centered: the infrastructure reading is contingent on past technical constraints now overcome; digital money becomes available under multiple institutional arrangements (consumer-direct, peer-to-peer, institutional). If authorization-centered: the institutional reading is self-perpetuating; only definitions endorsed by central banks constitute ''money''; alternative systems must rebrand as ''cryptocurrencies'' or ''payment systems.'' If accessibility-centered: the snare perspective is correct; digital money requires individual access, not merely interbank coordination. If intermediary-independent: the peer-to-peer reading is correct; institutional infrastructure reading misses the ontological essence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_kernel_instability, conceptual, 'What constitutes the foundational kernel for digital money definition').

omega_variable(
    regulatory_authority_source,
    'Does the institutional infrastructure reading''s regulatory authority derive from genuine coordination necessity or from historical accident of institutional entrenchment?',
    'Counterfactual analysis: if ACH/SWIFT had failed to establish regulatory dominance in the 1970s-1980s, would alternative systems (distributed ledgers, peer-to-peer networks) have claimed comparable authority? Technical review: do the coordination problems ACH/SWIFT solve require institutional intermediation or could they be solved by distributed consensus mechanisms?',
    'If coordination necessity: the suppression of alternatives is justified by systemic risk prevention; the snare and tangled_rope perspectives underestimate genuine coordination benefits. If historical accident: the institutional reading''s suppression of alternatives is extractive rent-seeking; the snare perspective is correct; digital money is ontologically independent of institutional intermediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_authority_source, empirical, 'Whether regulatory authority derives from coordination necessity or entrenchment').

omega_variable(
    sibling_reading_logical_status,
    'Are the consumer-access and peer-to-peer readings truly incompatible with the infrastructure reading, or do they describe complementary layers of the same system?',
    'Formal analysis: trace whether each reading''s core claim about what digital money IS logically forecloses the others or merely disagrees about which layer or epoch defines the phenomenon. Empirical history: did systems advance from institutional infrastructure → consumer access → peer-to-peer as a necessary sequence, or did they develop in parallel with jurisdictional variation?',
    'If incompatible: the readings are forecloses relations; only one can be correct. If complementary: the readings are coexists_with relations; all three are live positions held by different actors in different contexts. If sequential: the readings show influences relations; infrastructure reading enables but is being supplanted by consumer and peer-to-peer readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_status, conceptual, 'Whether sibling readings logically foreclose or coexist with the infrastructure reading').

omega_variable(
    measurement_problem_versus_ontological_crisis,
    'Does the apparent collapse of M4/M5 monetary aggregates in the digital era represent a measurement problem (as the infrastructure reading claims) or an ontological crisis in which ''money'' no longer fits institutional definitions?',
    'Data: compare M4/M5 trajectories across jurisdictions with different digital money regulatory regimes; correlate aggregate stability with institutional response (tightening or loosening of ''money'' definition). Conceptual: if central banks expand their definition of ''money'' to include cryptocurrencies or stablecoins, does this vindicate the infrastructure reading (measuring correctly) or refute it (the definition adapts to maintain authority)?',
    'If measurement problem: the institutional reading is sound; digital money remains institutional even as measurement methods adapt. If ontological crisis: alternative readings (consumer-access, peer-to-peer) were correct all along; the institutional definition has become obsolete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_problem_versus_ontological_crisis, empirical, 'M4/M5 collapse: measurement problem or ontological crisis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__infrastructure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1972_origin, digital_money_origin__infrastructure_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_1997_swift_dominance, digital_money_origin__infrastructure_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(theater_2022_blockchain_challenge, digital_money_origin__infrastructure_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_1972_origin, digital_money_origin__infrastructure_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(extract_1997_swift_dominance, digital_money_origin__infrastructure_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement(extract_2022_blockchain_challenge, digital_money_origin__infrastructure_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1972_origin, digital_money_origin__infrastructure_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(suppression_1997_swift_dominance, digital_money_origin__infrastructure_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(suppression_2022_blockchain_challenge, digital_money_origin__infrastructure_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__infrastructure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__infrastructure_reading, digital_money_origin__consumer_access_reading).
narrative_ontology:affects_constraint(digital_money_origin__infrastructure_reading, digital_money_origin__peer_to_peer_reading).
narrative_ontology:affects_constraint(digital_money_origin__infrastructure_reading, monetary_aggregates_measurement_crisis).
narrative_ontology:affects_constraint(digital_money_origin__infrastructure_reading, central_bank_digital_currency_authority).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel contains three structurally distinct readings with different ε values, different beneficiary/victim structures, and different constraint types. Each reading is a separate constraint story. The infrastructure reading (this story, ε=0.38) defines digital money as institutional interbank ledgers. The consumer-access reading (sibling, expected ε=0.25-0.35) defines it as individual electronic holdings. The peer-to-peer reading (sibling, expected ε=0.42-0.55) defines it as intermediary-free transfer. All three link to downstream constraints about monetary measurement and central bank digital currency authority, which will vary in their classification depending on which reading is adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_money_origin__infrastructure_reading, institutional, 0.18).
constraint_indexing:directionality_override(digital_money_origin__infrastructure_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
