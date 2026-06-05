% ============================================================================
% CONSTRAINT STORY: digital_money_origin__peer_to_peer_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__peer_to_peer_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_origin__peer_to_peer_reading
 *   human_readable: Digital Money Origin: Peer-to-Peer Reading
 *   domain: monetary_theory/financial_infrastructure/technology_history
 *
 * SUMMARY:
 *   The peer-to-peer reading of digital money origin establishes that 'true
 *   digital money' exists when value can be transferred without institutional
 *   intermediaries, achieving genuine peer-to-peer settlement. This reading
 *   dates the origin of digital money to 2009 (Bitcoin) and retroactively
 *   reclassifies all prior electronic money systems as 'merely digitized
 *   fiat' — institutional money in electronic form, not structural innovation
 *   in money itself. The reading suppresses the legitimacy of Visa, ACH,
 *   Swift, central bank digital currencies, and bank deposits as forms of
 *   digital money, asserting that these all require trusted intermediaries
 *   and therefore fail the peer-to-peer test. It claims Hayek's
 *   competing-currencies vision is realized only in cryptocurrency form, and
 *   diagnoses mainstream economists' surprise at non-institutional money as
 *   epistemic failure. This constraint exhibits extraction of narrative
 *   authority (who gets to define 'digital money'), suppression of
 *   alternative institutional definitions, and coordination around a shared
 *   understanding of monetary decentralization. The theater ratio has risen
 *   from 0.35 to 0.65 as mimetic institutional responses (CBDCs claiming to
 *   be 'digital money') have proliferated without achieving genuine
 *   peer-to-peer property, revealing the performative nature of the
 *   institutional response. Extractiveness has accumulated from 0.28 to 0.58
 *   as the reading has consolidated market power, narrative authority, and
 *   definitional control in the cryptocurrency ecosystem.
 *
 * KEY AGENTS:
 *   - Peer-to-peer reading advocates (organized/arbitrage): Cryptocurrency communities, Hayek-influenced libertarians, decentralization narrativists. Primary beneficiaries — control narrative definition, extract authority over what counts as 'digital money.'
 *   - Traditional monetary theory (powerless/trapped): Academic economists, central banks, textbook authors bound to institutional M0-M5 frameworks. Cannot exit the suppression without abandoning the conceptual apparatus that makes monetary claims coherent.
 *   - Institutional finance systems (moderate/constrained): Visa, ACH, Swift, commercial banks, central banks. Constrained by the reading's definitional exclusion; attempt mimetic response (CBDCs) that cannot achieve peer-to-peer property without abandoning institutional control.
 *   - Cryptographic technology communities (moderate/constrained): Bitcoin, Ethereum, other cryptocurrency projects. Mixed: genuinely solve coordination problems (censorship resistance, double-spend without intermediary) but also extract from alternative technical framings (sidechains, payment channels).
 *   - Analytical observer (analytical/analytical): Sees the reading as a natural law (money IS peer-to-peer transfer) but structural data reveals false summit — the suppression of institutional definitions and extraction of narrative authority show this is a contingent epistemic choice, not a natural fact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__peer_to_peer_reading, 0.58).
domain_priors:suppression_score(digital_money_origin__peer_to_peer_reading, 0.72).
domain_priors:theater_ratio(digital_money_origin__peer_to_peer_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__peer_to_peer_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__peer_to_peer_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_money_origin__peer_to_peer_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__peer_to_peer_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__peer_to_peer_reading, "Digital Money Origin: Peer-to-Peer Reading").
narrative_ontology:topic_domain(digital_money_origin__peer_to_peer_reading, "monetary_theory/financial_infrastructure/technology_history").

domain_priors:requires_active_enforcement(digital_money_origin__peer_to_peer_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__peer_to_peer_reading, '7c323173-369a-4073-8756-b109548b0109').
narrative_ontology:cs_kernel_codification('7c323173-369a-4073-8756-b109548b0109', fixed_text).
narrative_ontology:cs_authority_grounding('7c323173-369a-4073-8756-b109548b0109', distributed).
narrative_ontology:cs_reading_relation('7c323173-369a-4073-8756-b109548b0109', digital_money_origin__infrastructure_reading, forecloses).
narrative_ontology:cs_reading_relation('7c323173-369a-4073-8756-b109548b0109', digital_money_origin__consumer_access_reading, coexists_with).
narrative_ontology:cs_axiom('7c323173-369a-4073-8756-b109548b0109', foundational, institutional_intermediaries_disqualify_money).
narrative_ontology:cs_axiom_status(institutional_intermediaries_disqualify_money, holdable).
narrative_ontology:cs_axiom_grounding('7c323173-369a-4073-8756-b109548b0109', institutional_intermediaries_disqualify_money, deontological).
narrative_ontology:cs_axiom('7c323173-369a-4073-8756-b109548b0109', foundational, peer_to_peer_settlement_technical_breakthrough).
narrative_ontology:cs_axiom_status(peer_to_peer_settlement_technical_breakthrough, holdable).
narrative_ontology:cs_axiom_grounding('7c323173-369a-4073-8756-b109548b0109', peer_to_peer_settlement_technical_breakthrough, empirically_contingent).
narrative_ontology:cs_reference_frame('7c323173-369a-4073-8756-b109548b0109', pre_cryptographic_monetary_theory).
narrative_ontology:cs_drift_state('7c323173-369a-4073-8756-b109548b0109', post_2009_ecosystem_consolidation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7c323173-369a-4073-8756-b109548b0109', '').
narrative_ontology:cs_kernel_id(digital_money_origin__peer_to_peer_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__peer_to_peer_reading, cryptocurrency_advocates).
narrative_ontology:constraint_beneficiary(digital_money_origin__peer_to_peer_reading, decentralization_narrative_holders).
narrative_ontology:constraint_victim(digital_money_origin__peer_to_peer_reading, traditional_monetary_theory).
narrative_ontology:constraint_victim(digital_money_origin__peer_to_peer_reading, institutional_finance_legitimacy).
narrative_ontology:constraint_victim(digital_money_origin__peer_to_peer_reading, historical_continuity_understanding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL MONETARY THEORY (SNARE) — Trapped within the institutional framework's definitional categories (M0-M5 aggregates). Cannot exit the reading's suppression without abandoning the conceptual apparatus that makes monetary claims meaningful. The peer-to-peer reading forecloses the legitimacy of state-centric monetary history. Full experienced extraction — no exit path available within the framework.
constraint_indexing:constraint_classification(digital_money_origin__peer_to_peer_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL FINANCE (SNARE) — Constrained by the reading's claim that pre-2009 electronic money systems are 'merely digitized fiat,' not true digital money. This categorically denies institutional legitimacy to Visa, ACH, Swift, and central bank digital currencies as 'real' money. High suppression of alternative definitions; significant extraction of institutional narrative authority. Barriers to exit are institutional path dependence and regulatory architecture.
constraint_indexing:constraint_classification(digital_money_origin__peer_to_peer_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CRYPTOGRAPHIC TECHNOLOGY COMMUNITIES (TANGLED ROPE) — Mixed coordination and extraction. The peer-to-peer reading provides legitimate validation that decentralized systems are technically feasible and address real coordination problems (censorship resistance, double-spend without trusted intermediary). But also extracts from alternative technical framings by declaring them non-functional: Lightning Network, payment channels, and sidechains are less canonical 'true digital money' than on-chain settlement. Constrained by technical debt and network effects that make migration costly.
constraint_indexing:constraint_classification(digital_money_origin__peer_to_peer_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CRYPTOCURRENCY ADVOCATES AND NARRATIVE HOLDERS (ROPE) — Primary beneficiaries with arbitrage options. The reading provides definitional authority: 'true digital money' is achievable, institutional intermediaries are not necessary, the 2009 invention narrative marks a fundamental breakthrough. Experiences the constraint as pure coordination: establishes shared language for discussing post-institutional monetary systems. Net positive experience; can arbitrage between this reading and others if needed (institutional systems can provide reference pricing, legal stability).
constraint_indexing:constraint_classification(digital_money_origin__peer_to_peer_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CENTRAL BANKS AND MONETARY POLICYMAKERS (PITON) — The peer-to-peer reading's suppression of M4/M5 as irrelevant to 'true digital money' is largely performative for CBDCs and digital currency policy. Central banks maintain the fiction that their systems represent 'institutional digital money' while acknowledging (off-record) that the reading has redefined the category in ways their infrastructure cannot match. Theater_ratio high because the CBDC response is structurally mimetic — reproducing cryptographic signatures while maintaining institutional control — not genuinely peer-to-peer. Institutional degradation: the category they held is now populated by systems they cannot control.
constraint_indexing:constraint_classification(digital_money_origin__peer_to_peer_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the reading appears as a natural classification: money's defining property IS the ability to transfer value peer-to-peer without intermediaries; anything requiring intermediaries is by definition not complete money, just credit. This perspective naturalizes what is actually a contingent epistemic choice: the decision that peer-to-peer settlement is the canonical measure. But the structural data contradicts this — the suppression of alternative definitions (institutional money, fiat digitization, bank accounts as money) and the extraction of narrative authority reveal this as a false summit.
constraint_indexing:constraint_classification(digital_money_origin__peer_to_peer_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__peer_to_peer_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_money_origin__peer_to_peer_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_money_origin__peer_to_peer_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__peer_to_peer_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_money_origin__peer_to_peer_reading, TR),
    TR >= 0.70.

:- end_tests(digital_money_origin__peer_to_peer_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The peer-to-peer reading extracts significant narrative and definitional authority from competing frameworks. By establishing that 'true digital money' requires peer-to-peer settlement without intermediaries, it denies legitimacy to institutional systems (M1-M5, bank deposits, payment networks) that serve analogous economic functions. The extraction accumulates over time as the reading consolidates — early (t=0, ε=0.28) it was a fringe technical proposal; by t=15 (ε=0.58) it has achieved substantial cultural authority and institutional response (CBDC development). However, extractiveness is not maximal because the reading also provides genuine coordination function — decentralized systems do solve technical problems of censorship resistance and double-spend that institutional systems handle through trusted intermediaries. Suppression (0.72): High. The reading requires active suppression of alternative legitimate definitions: (a) institutional money systems are real digital money even with intermediaries, (b) electronic payment networks are genuine coordination mechanisms not merely 'digitized fiat,' (c) prior cryptographic proposals partially achieved peer-to-peer properties, (d) economists had sound economic reasoning (not epistemic blindness) for expecting intermediaries. This suppression is enforced through narrative dominance, definitional redefinition, and retroactive historical rewriting. Theater ratio (0.65): Moderate-high. The performative dimension includes: CBDC development as mimetic institutional response (claiming 'digital currency' legitimacy while retaining intermediary control), continued use of M4/M5 aggregates while dismissing their theoretical relevance, and rhetorical declaration that prior systems 'weren't really digital money' without technical argumentation. The theater has increased over time as the reading's dominance has grown and institutional responses have become more imitative rather than substantive.
 *
 * PERSPECTIVAL GAP:
 *   Radical divergence across power positions. The cryptocurrency advocates (organized/arbitrage) see pure coordination (Rope) — solving the genuine problem of peer-to-peer value transfer. The cryptographic communities (moderate/constrained) see mixed coordination and extraction (Tangled Rope) — real technical advance plus extraction from alternative framings. The institutional finance (powerless in this frame/constrained) sees the constraint as pure extraction (Snare) — their legitimate definitions are suppressed without technical argumentation. Traditional monetary theory (powerless/trapped) experiences complete foreclosure (Snare) — the reading denies the legitimacy of their entire conceptual apparatus. The analytical observer at the civilizational level risks seeing a natural law (Mountain) — money IS peer-to-peer transfer, institutions are contingent — but the structural data reveals a false summit: suppression and extraction of authority are what enable the 'natural' appearance.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position. Beneficiaries (cryptocurrency advocates) have arbitrage exit options and control the definitional authority — low d, experienced as coordination benefit. Cryptographic technologists have constrained exit (path dependence in technical protocols) but genuine function as beneficiary — moderate d, mixed experience. Institutional finance has constrained exit (regulatory architecture, legacy infrastructure) and victim status (definitional suppression) — high d, high experienced extraction. Traditional monetary theory has trapped exit (the reading forecloses their conceptual apparatus) — maximum d, maximum experienced extraction. The analytical observer has analytical exit but risks identity lock (commitment to naturalization of contingent epistemic choices) — moderate d but persistent misclassification as mountain.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_money_legitimacy,
    'Is the peer-to-peer reading''s exclusion of institutional digital money (Visa, ACH, Swift, CBDCs) from the category ''digital money'' a factual claim about technical architecture, or a definitional choice that suppresses alternative legitimate monetary systems?',
    'Historical analysis of pre-2009 electronic money systems'' functional role in economic coordination, transaction volumes, and value transfer; examination of whether M1/M2 (which include bank deposits) are empirically less ''money'' than on-chain cryptocurrency; comparison of institutional digital currency adoption rates and economic impact vs cryptocurrency use.',
    'If factual/technical: the reading is a correct classification that prior systems genuinely lacked peer-to-peer property. If definitional/suppressive: the reading is a contingent epistemic choice that denies legitimacy to functionally equivalent systems for ideological reasons. This determines whether we are classifying constraint as Tangled Rope (mixed coordination + extraction) or Snare (pure extraction of narrative authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_money_legitimacy, empirical, 'Whether institutional digital money systems are legitimately excluded or suppressed').

omega_variable(
    id_2009_origin_claim_sufficiency,
    'Does the 2009 (Bitcoin) origin date represent a genuine technological discontinuity in peer-to-peer property, or a definitional redrawing that ignores earlier decentralized payment experiments and cryptographic innovations that partially achieved the same properties?',
    'Technical history: David Chaum''s DigiCash (1990s) — had cryptographic anonymity but relied on trusted issuer; Hashcash (1997) — decentralized consensus without intermediary for proof-of-work; b-money and bit gold proposals (1998) — explicit peer-to-peer designs that predate Bitcoin. Determine whether these systems genuinely failed due to technical necessity or were suppressed by institutional dominance and narrative rewriting.',
    'If genuine discontinuity: 2009 origin is structurally justified, prior systems were functionally different. If redrawing: the reading suppresses earlier work retroactively, extracting originality narrative. This affects assessment of how much suppression feeds the constraint''s extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(id_2009_origin_claim_sufficiency, empirical, 'Whether 2009 represents a genuine technological discontinuity or definitional redrawing').

omega_variable(
    hayek_realization_claim,
    'Does the claim that Hayek''s competing-currencies vision is ''realized only with cryptocurrency'' accurately characterize Hayek''s proposal, or does it impose a post-hoc reading that transforms his open-discussion of monetary alternatives into an endorsement of this specific technological form?',
    'Close reading of Hayek''s ''Denationalisation of Money'' (1976): what specific properties did Hayek identify as necessary for competing currencies to function? Do cryptocurrencies possess all those properties, or only the peer-to-peer settlement property? Did Hayek identify peer-to-peer settlement as the critical innovation, or was he concerned with other properties (e.g., stability, market discipline, absence of central planning)?',
    'If accurate realization: cryptocurrency is the natural fulfillment of Hayek''s vision, and his authority backs the reading. If post-hoc reading: the reading extracts from Hayek by claiming his legitimacy for a technological form he could not have foreseen, and the suppression of alternative interpretations of his work increases extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hayek_realization_claim, empirical, 'Whether cryptocurrency is the true realization of Hayek''s competing-currencies vision').

omega_variable(
    economists_epistemic_failure_diagnosis,
    'Is the claim that mainstream economists'' surprise at non-institutional money represents an ''epistemic failure to recognize non-institutional money as possible'' a diagnosis of genuine blindness, or a retroactive dismissal of legitimate theoretical reasons economists expected institutional intermediaries?',
    'Economic history: examination of 1980s-2000s literature on digital money, electronic payment systems, and alternative currencies. Did economists articulate reasons they expected intermediaries (the double-spend problem, information asymmetries, settlement risk)? Were these reasons empirically overcome by cryptographic innovation, or dismissed without addressing the underlying economic concerns?',
    'If genuine blindness: economists failed to imagine a solution that was technically feasible; this validates the reading''s claim that the peer-to-peer system represents a breakthrough that required overcoming ideological capture. If legitimate theoretical concern: economists had sound economic reasons for their skepticism, and the surprise reflects not epistemic failure but the engineering solution''s unexpected effectiveness. This affects assessment of how much suppression is present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economists_epistemic_failure_diagnosis, empirical, 'Whether economist surprise reflects epistemic failure or legitimate theoretical concerns').

omega_variable(
    competing_readings_logical_consistency,
    'Can the peer-to-peer reading coexist with the infrastructure and consumer access readings, or do they logically foreclose one another?',
    'Formal definition analysis: if digital money is defined as ''peer-to-peer value transfer without institutional intermediaries'' (P2P reading), can a system also satisfy ''institutional infrastructure enabling value transfer'' (infrastructure reading) or ''individuals holding electronic value directly'' (consumer access reading)? Do these definitions map to different properties or the same property viewed differently?',
    'If logically foreclosing: this reading rules out the sibling readings'' core premises; the constraint classification should shift from Tangled Rope toward Snare (pure extraction via definitional authority). If coexisting: the readings represent different legitimate perspectives on the same underlying phenomenon; the classification as Tangled Rope (mixed coordination + suppression) is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_readings_logical_consistency, conceptual, 'Whether the three readings logically coexist or foreclose one another').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__peer_to_peer_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dmptp_theater_t0, digital_money_origin__peer_to_peer_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dmptp_theater_t7, digital_money_origin__peer_to_peer_reading, theater_ratio, 7, 0.52).
narrative_ontology:measurement(dmptp_theater_t15, digital_money_origin__peer_to_peer_reading, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(dmptp_extract_t0, digital_money_origin__peer_to_peer_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dmptp_extract_t7, digital_money_origin__peer_to_peer_reading, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(dmptp_extract_t15, digital_money_origin__peer_to_peer_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dmptp_suppress_t0, digital_money_origin__peer_to_peer_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(dmptp_suppress_t7, digital_money_origin__peer_to_peer_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(dmptp_suppress_t15, digital_money_origin__peer_to_peer_reading, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__peer_to_peer_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__peer_to_peer_reading, digital_money_origin__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_origin__peer_to_peer_reading, digital_money_origin__consumer_access_reading).
narrative_ontology:affects_constraint(digital_money_origin__peer_to_peer_reading, central_bank_monetary_control).
narrative_ontology:affects_constraint(digital_money_origin__peer_to_peer_reading, institutional_intermediary_necessity).
narrative_ontology:affects_constraint(digital_money_origin__peer_to_peer_reading, hayek_competing_currencies_vision).

% DUAL FORMULATION NOTE:
% The digital_money_origin kernel decomposes into three constraint stories, one per reading. Each reading makes a different claim about what properties are necessary and sufficient for 'digital money' to exist. The peer-to-peer reading defines digital money as value transfer without institutional intermediaries. The infrastructure reading defines it as institutional systems enabling electronic transfer. The consumer access reading defines it as direct individual holding of electronic value. These are not three perspectives on the same constraint — they are three genuinely different constraints with different ε values, different beneficiary/victim structures, and different classification profiles. The three readings coexist in public discourse because different communities hold different definitions. The peer-to-peer reading is downstream of cryptographic theory but upstream of all downstream constraints about institutional monetary control that must respond to the redefinition of what 'digital money' means.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
