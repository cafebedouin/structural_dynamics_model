% ============================================================================
% CONSTRAINT STORY: became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_became_thinkable_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: became_thinkable_reading
 *   human_readable: Digital Money Became Thinkable: Institutional Framing Constraint (1980s-2010)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   Between 1980 and 2010, 'digital money' became institutionally thinkable:
 *   the concept shifted from speculative science fiction to research agenda,
 *   policy problem, and technical reality. This constraint captures the
 *   institutional framing process itself — the constraint that structured
 *   what 'digital money' could mean and who could participate in defining it.
 *   The framing emerged through cryptographic standards bodies (NIST),
 *   banking consortia (the Group of Ten), central bank research programs, and
 *   academic funding patterns. By the time Bitcoin was implemented in 2009,
 *   the conceptual ground had been prepared: digital money meant
 *   cryptographically secured value, compatible with existing financial
 *   infrastructure, state-currency denominated or pegged. Alternative
 *   possibilities — decentralized protocols, complementary currencies, bearer
 *   instruments — had been largely written out of the institutional narrative
 *   not through technical refutation but through institutional gatekeeping.
 *   The constraint's extractiveness increased over the 30-year interval as
 *   the dominant framing calcified: early debate (1980s) gave way to
 *   consensus (1990s) and then to regulatory enforcement of the consensus
 *   definition (2000s-2010). The theater_ratio increased as cryptography's
 *   technical appearance masked the institutional choices embedded in
 *   standards adoption and as compliance frameworks treated the framing as
 *   natural rather than contingent.
 *
 * KEY AGENTS:
 *   - Early Institutional Architects: Central banks, major commercial banks, NIST, IMF research programs (institutional/arbitrage) — benefited from establishing the institutional consensus; had framing power
 *   - Cryptographic Researchers: Academic cryptographers, NSA researchers, NIST standards committees (powerful/mobile) — benefited from funding concentration around cryptographic approaches; research agendas shaped by institutional priorities
 *   - Banking Innovation Centers: Innovation labs at major banks, financial technology consortia (powerful/arbitrage) — benefited from clarity on what digital money was; could invest in infrastructure aligned with the framing
 *   - Excluded Conceptual Communities: Complementary currency theorists, monetary pluralism advocates, distributed systems researchers outside banking (powerless/trapped) — could not participate in institutional framing; alternatives were foreclosed without direct refutation
 *   - Mid-Tier Technical Communities: Telecommunications engineers, mid-tier fintech developers, open-source communities (moderate/constrained) — constrained by the dominant framing; had to fit innovations into the institutional box rather than expanding boundaries
 *   - Cryptocurrency Communities (Bitcoin, Ethereum): Emerged ~2009 as organized exit from the constraint (organized/mobile) — represented an escape from the institutional framing; forced reconsideration of what digital money could be
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(became_thinkable_reading, 0.58).
domain_priors:suppression_score(became_thinkable_reading, 0.48).
domain_priors:theater_ratio(became_thinkable_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(became_thinkable_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(became_thinkable_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(became_thinkable_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(became_thinkable_reading, "Digital Money Became Thinkable: Institutional Framing Constraint (1980s-2010)").
narrative_ontology:topic_domain(became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(became_thinkable_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(became_thinkable_reading, cryptographic_researchers).
narrative_ontology:constraint_beneficiary(became_thinkable_reading, banking_innovation_centers).
narrative_ontology:constraint_victim(became_thinkable_reading, excluded_conceptual_framing_communities).
narrative_ontology:constraint_victim(became_thinkable_reading, monetary_pluralism_advocates).
narrative_ontology:constraint_victim(became_thinkable_reading, decentralized_payment_systems_proponents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CONCEPTUAL COMMUNITIES (SNARE) — Alternative payment system designers, complementary currency theorists, and monetary pluralism advocates could not participate in the institutional framing that defined what 'digital money' meant. The dominant narrative (digital = cryptographically secured, store-of-value denominated in state currency, compatible with existing banking infrastructure) foreclosed conceptual space for other possibilities (digital bearer instruments, protocol-based money creation, decentralized systems). These communities bore the cost of being written out of the possibility space without exit.
constraint_indexing:constraint_classification(became_thinkable_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER TECHNICAL COMMUNITIES (TANGLED ROPE) — Distributed systems researchers, telecommunications engineers, and mid-tier financial technologists experienced mixed coordination and extraction. They benefited from the emerging institutional consensus (funding, research clusters, infrastructure investment) but were constrained by the framing's dominant narrative. Their innovations had to fit the pre-existing institutional box rather than expand the conceptual boundaries. Constrained by career path dependence and institutional funding gatekeeping, but also benefiting from the coordination function the framing provided.
constraint_indexing:constraint_classification(became_thinkable_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY INSTITUTIONAL ARCHITECTS (ROPE) — Central banks, major commercial banks, cryptographic standards bodies (NIST), and regulatory agencies benefited from establishing the institutional consensus around what digital money was. They had arbitrage options (they could reframe the concept or exit the institutional arrangements), but experienced the constraint as a coordination solution: defining digital money enabled infrastructure investment, regulatory clarity, and research prioritization. Their experience was of solving a coordination problem, not of extraction.
constraint_indexing:constraint_classification(became_thinkable_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE CRYPTOCURRENCY COMMUNITIES (SCAFFOLD) — Bitcoin (2009), Ethereum, and subsequent cryptocurrency networks represent an organized escape from the institutional framing. These communities saw the constraint as temporary (a particular institutional narrative that was historically contingent, not inevitable). They had sufficient agency and coordination capacity to build alternative technical and conceptual infrastructure outside the original institutional framework. The sunset lies in cryptocurrency adoption forcing institutional rethinking: central banks now must address that digital money includes non-state-issued systems, dynamic proof-of-work protocols, and decentralized governance models the original framing excluded.
constraint_indexing:constraint_classification(became_thinkable_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE THEATER (PITON) — By 2020s, the original institutional framing (digital money = cryptographically secured state-currency denomination within banking infrastructure) persists through regulatory inertia despite being functionally degraded. Stablecoins, NFTs, DeFi protocols, and central bank digital currencies (CBDCs) demonstrate that the original framing is no longer the only viable model. Yet regulatory compliance still assumes the original narrative — AML/KYC rules, banking reserve requirements, and securities laws all embed assumptions about digital money being an institutional product, not a protocol phenomenon. The theater persists because institutions haven't fully replaced the original framework, not because it remains functionally dominant.
constraint_indexing:constraint_classification(became_thinkable_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the rise of digital money as thinkable might appear as an inevitable consequence of technological capability: once cryptographic primitives and computer networks reached sufficient maturity, digital money became a natural solution to coordination problems in exchange. This perspective sees the thinkability constraint as emerging from technological possibility, not institutional choice. However, the structural data contradicts the mountain classification — identifiable beneficiaries shaped the framing, and alternative conceptualizations were actively suppressed, not naturally eliminated. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(became_thinkable_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(became_thinkable_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(became_thinkable_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(became_thinkable_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(became_thinkable_reading, TR),
    TR >= 0.70.

:- end_tests(became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint exhibits substantial but not total extraction. The institutional framing created benefits for those aligned with it (faster funding, clearer regulatory pathways, research coordination) and costs for those excluded from it (marginalized research agendas, foreclosed technical directions). The extraction is not as severe as a Snare would suggest because the framing was experienced by many participants as genuine technical consensus-building (cryptography as the 'right' approach) rather than as cynical exclusion. However, the exclusion of alternative conceptualizations — especially decentralized and non-state-currency-denominated systems — was systematic and enforced through institutional mechanisms (funding gatekeeping, standards bodies, regulatory authority). The ε=0.58 reflects this mixed picture: moderate extractiveness because the institutional enforcement was real but experienced as technical consensus rather than coercion. Suppression (0.48): Moderate suppression reflecting institutional barriers (gatekeeping in standards bodies, funding concentration) combined with some internalized acceptance of the framing as inevitable ('cryptography is the only secure approach'). The suppression is not total because alternative proposals existed in the literature and could be articulated, even if they lacked institutional venues. Theater (0.65): Moderate-high theater reflecting the use of cryptography's technical authority to naturalize institutional choices. The 'cryptographically secured' framing masks institutional decisions about which cryptographic systems, governance models, and economic assumptions would be canonical. The theater increases over the interval as the framing becomes more established and less questioned.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how institutional framing creates divergent experiences of the same phenomenon. Early institutional architects see coordination (Rope) — they are solving the legitimate problem of defining digital money technically and enabling infrastructure investment. Excluded conceptual communities see extraction (Snare) — their alternatives are systematically foreclosed. Mid-tier technical communities see mixed coordination and extraction (Tangled Rope) — they benefit from the framing's coordination function but are constrained by its boundaries. Cryptocurrency communities see a temporary constraint with an exit path (Scaffold) — the dominant framing was contingent, and they successfully built alternative technical and institutional infrastructure. The regulatory compliance system sees its own degraded ritual (Piton) — by the 2020s, the original framing (digital = cryptographically secured state-currency denomination) no longer covers the actual landscape of digital money systems, yet regulatory frameworks still assume it. The analytical observer risks seeing the framing as a natural consequence of technological maturity (Mountain), treating cryptographic capability as the definer of what digital money is, while missing the institutional choices that shaped which cryptographic approaches, governance models, and economic assumptions became canonical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural position relative to the framing constraint. Early institutional architects (beneficiaries with arbitrage options) derive low d values — they can reframe or exit at low cost, but they benefit from the current framing. Excluded communities (victims with no exit) derive high d values — they cannot escape the foreclosed conceptual space and bear the cost of being written out of the institutional narrative. Mid-tier technical communities (constrained victims who also benefit from coordination) derive moderate-high d values reflecting their asymmetric position. Cryptocurrency communities (organized agents with mobile exit) derive lower d values reflecting their ability to escape and build alternative infrastructure. The directionality computation surfaces a structural feature of this constraint: it operates through institutional gatekeeping and framing power rather than through direct coercion. Exit is possible (as cryptocurrency communities demonstrated) but requires organizing new institutional infrastructure. The performative component (cryptography as technical authority) masks the institutional nature of the framing, making the constraint appear more like Mountain than it actually is.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through committer framing: the same institutional process appears as coordination (Rope) to the architects and as extraction (Snare) to the excluded. The mandatrophy is not 'which type is correct?' but 'what counts as genuine coordination versus what is extraction disguised as coordination?'. The false summit (Mountain) represents the risk that the framing becomes naturalized: treating 'digital money must be cryptographically secured' as a consequence of technological laws rather than as contingent institutional choices. The scaffold perspective validates that the framing was contingent — new institutional infrastructure (cryptocurrency networks) could and did establish alternative definitions. The Tangled Rope classification of the constraint reflects that the institutional process had genuine coordination components (reducing uncertainty, enabling infrastructure investment) alongside genuine extraction components (foreclosing alternatives, concentrating framing power).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Which kernel reading of ''digital money origin'' is this constraint instantiating, and what sibling readings would produce different structural classifications?',
    'Comparison with constraint stories for first_held_reading (origin = first successful implementation; beneficiaries = early adopters/miners) and regulatory_recognition_reading (origin = formal regulatory status; beneficiaries = compliance infrastructure). The became_thinkable_reading emphasizes institutional conceptual framing (1980s-2010) as the origin point, earlier than implementation or regulatory recognition.',
    'Different readings produce different ε values and different victim sets. The became_thinkable_reading treats the constraint as operating at the conceptual/framing level; sibling readings treat it as operating at implementation or regulatory levels. The three readings are structurally distinct constraints with kinship through shared kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Kernel reading identification and sibling relationship to first_held_reading and regulatory_recognition_reading').

omega_variable(
    institutional_framing_inevitability,
    'Was the institutional framing of digital money (cryptographically secured, store-of-value, state-currency denominated) inevitable given the technical capabilities of the 1980s-2000s, or was it a contingent choice by specific institutional actors?',
    'Historical counterfactual analysis: What alternative framings existed in the technical and policy literature? What institutional gatekeepers made the framing canonical? Did marginalized communities propose structurally distinct definitions that were actively suppressed rather than naturally eliminated? Archival analysis of rejected proposals (complementary currency systems, bearer instruments, decentralized protocols) in banking and regulatory literature.',
    'If inevitable: constraint is more Mountain-like (structural/technological limit, less extractive). If contingent: constraint is more Snare-like (institutional choices that excluded alternatives, more extractive). Current ε=0.58 assumes moderate contingency — the framing was shaped by institutional choices but not purely arbitrary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_framing_inevitability, empirical, 'Whether the dominant digital money framing was technologically inevitable or institutionally contingent').

omega_variable(
    cryptographic_neutrality_myth,
    'Does the appearance of cryptography as a ''technical'' (neutral) basis for digital money framing conceal institutional choices about which cryptographic systems, governance models, and economic assumptions would become canonical?',
    'Technical history analysis: Compare NIST cryptographic standards adoption vs. rejected alternatives (Diffie-Hellman vs. elliptic curve vs. post-quantum); trace how RSA became canonical despite earlier Merkle tree work; analyze how hash-function standardization shaped mining-based consensus models. Political economy analysis: Who benefited from cryptographic standards adoption? Whose interests did alternative cryptographic framings serve?',
    'If cryptography is treated as neutral: framing appears more natural/mountain-like. If cryptography is treated as politically laden: framing appears more contingent/snare-like. The theater_ratio (0.65) reflects that cryptography''s technical appearance masks institutional selection — significant performative content in treating ''the math'' as the definer of what digital money is.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cryptographic_neutrality_myth, conceptual, 'Whether cryptographic standards appear neutral while actually embedding institutional choices').

omega_variable(
    suppression_mechanism_type,
    'Is the measured suppression (0.48) structural (resource barriers, institutional gatekeeping) or performative (the excluded communities internalized the framing and self-selected out)?',
    'Archival analysis: Did alternative proposals exist but lack institutional venues? Or did communities accept the dominant framing as inevitable? Interview retrospectives with mid-career technologists from excluded communities. Measurement of publication patterns: were alternative proposals rejected by gatekeeping or simply not submitted?',
    'If structural: suppression persists after institutional barriers are removed (identity lock component). If performative: suppression declines as alternative institutional venues open (cryptocurrency communities demonstrate this). Current measurement assumes mixed suppression — some structural barriers, some internalized framing. Post-cryptocurrency data suggests the suppression was partly performative (now being overcome).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_type, empirical, 'Whether suppression of alternative framings is structural gatekeeping or internalized inevitability acceptance').

omega_variable(
    beneficiary_identification_ambiguity,
    'Were the declared beneficiaries (early institutional architects, cryptographic researchers, banking innovation centers) intentional framers or merely institutional actors whose interests aligned with the framing that happened to win?',
    'Historical agency analysis: Did these groups deliberately construct the framing to serve their interests? Or did they participate in a process they understood as technical consensus-building? Documentary analysis of institutional decisions (NIST standards committees, banking association task forces, academic funding patterns). Oral history: what did gatekeepers understand themselves to be doing?',
    'If intentional: extracted deliberately, more Snare-like. If incidental: benefited from structural position, more Tangled-Rope-like. ε=0.58 assumes beneficiaries were institutional actors whose position enabled them to benefit from the framing process, not that they deliberately constructed it to exclude others. However, some institutional actors (particularly in banking and standards bodies) may have been more intentional than others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether beneficiaries intentionally shaped framing or benefited incidentally from institutional position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(became_thinkable_reading, 1980, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beca_tr_t0, became_thinkable_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(beca_tr_t5, became_thinkable_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(beca_tr_t10, became_thinkable_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(beca_be_t0, became_thinkable_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(beca_be_t5, became_thinkable_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(beca_be_t10, became_thinkable_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(became_thinkable_reading, first_held_reading).
narrative_ontology:affects_constraint(became_thinkable_reading, regulatory_recognition_reading).
narrative_ontology:affects_constraint(became_thinkable_reading, cryptocurrency_escape_dynamics).
narrative_ontology:affects_constraint(became_thinkable_reading, central_bank_digital_currency_capture).

% DUAL FORMULATION NOTE:
% The three readings of the digital_money_origin kernel (became_thinkable_reading, first_held_reading, regulatory_recognition_reading) form a constraint family representing different temporal and institutional origin points. Each reading instantiates a structurally distinct constraint with different ε values and different beneficiary/victim structures. This story focuses on the institutional framing process; sibling stories focus on implementation and regulatory recognition. The three are linked through a shared kernel but are not identical constraints — each has its own classification and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(became_thinkable_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
