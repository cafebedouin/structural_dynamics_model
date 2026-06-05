% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__strict_convertibility_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Convertibility as Binding Legal Obligation (Strict Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   Article IV of the Bretton Woods Agreement (1944) established
 *   convertibility of the U.S. dollar to gold at a fixed price ($35/oz) as
 *   the foundation of the post-war international monetary system. This
 *   constraint story models the STRICT CONVERTIBILITY READING: the
 *   interpretation that treats Article IV as a binding legal obligation
 *   constraining U.S. monetary policy. Under this reading, the U.S. committed
 *   itself to maintaining convertibility regardless of economic conditions,
 *   making the dollar America's currency but the world's money. This reading
 *   produces a snare classification: the constraint extracts monetary policy
 *   discretion from the U.S. economy, suppresses alternatives (domestic
 *   inflation, flexible exchange rates), and benefits creditor nations
 *   holding dollar-denominated reserves. The constraint's severity increases
 *   over the interval (1944–1954) as gold outflows mount and the Triffin
 *   dilemma becomes apparent: the system requires simultaneous impossible
 *   conditions (fixed price, free capital flows, independent monetary
 *   policy). The theater ratio rises (0.25 → 0.38) as the institutional
 *   apparatus increasingly performs convertibility through central bank
 *   coordination (London Gold Pool, forward swaps) rather than actual
 *   conversion, indicating that the legal obligation remains binding but the
 *   functional mechanism degrades. This reading coexists with alternative
 *   readings (policy_flexible_reading treats convertibility as revocable
 *   policy; triffin_structural_reading treats it as logically impossible)
 *   within the broader kernel of contested interpretation around dollar-gold
 *   linkage and U.S. monetary sovereignty.
 *
 * KEY AGENTS:
 *   - U.S. Treasury/Federal Reserve: Primary victim (powerful/trapped) — bound by Article IV obligation to maintain convertibility; cannot expand money supply or conduct independent monetary policy without breach
 *   - Creditor Nations (UK, France, Belgium, Netherlands): Primary beneficiaries (institutional/arbitrage) — hold enforceable claims on U.S. gold reserves; benefit from stable dollar value and fixed exchange rates; can demand conversion anytime
 *   - Gold Reserve Holders / Central Banks: Secondary beneficiary (institutional/arbitrage) — accumulate dollar reserves as good-as-gold asset; benefit from low interest rates on dollar balances and U.S. willingness to absorb inflation rather than breach
 *   - Emerging Market Currencies: Secondary victim (moderate/constrained) — forced to peg to dollar or accept volatility; locked into dollar hegemony; cannot develop independent monetary policy
 *   - Bretton Woods Institutional Complex (IMF, World Bank, BIS): Institutional guardian (organized/constrained) — enforces rules through coordination; perceives genuine coordination function but increasingly sees system degrading as contradictions mount
 *   - International Central Banking Coalition: Organized coalition (organized/constrained) — coordinates to maintain the fiction of convertibility through forward swaps and gold pool arrangements; requires active enforcement as confidence erodes
 *   - The Analytical Observer: Views constraint as natural law (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable economic principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.68).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.72).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Convertibility as Binding Legal Obligation (Strict Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '83151a0f-bc67-4cf1-8e70-14843b0ae7d4').
narrative_ontology:cs_kernel_codification('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', fixed_text).
narrative_ontology:cs_authority_grounding('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', lineage).
narrative_ontology:cs_interpretation_layer_present('83151a0f-bc67-4cf1-8e70-14843b0ae7d4').
narrative_ontology:cs_reading_relation('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_reading_relation('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', foundational, article_iv_legally_binding).
narrative_ontology:cs_axiom_status(article_iv_legally_binding, holdable).
narrative_ontology:cs_axiom_grounding('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', article_iv_legally_binding, conventional).
narrative_ontology:cs_axiom('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', foundational, convertibility_incompatible_with_expansion).
narrative_ontology:cs_axiom_status(convertibility_incompatible_with_expansion, holdable).
narrative_ontology:cs_axiom_grounding('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', convertibility_incompatible_with_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', articles_of_agreement_bretton_woods).
narrative_ontology:cs_drift_state('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', increasing_gold_outflow_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('83151a0f-bc67-4cf1-8e70-14843b0ae7d4', '2026-02-26T18:42:00Z').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_reserve_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_policy_space).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, emerging_market_currencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: U.S. TREASURY / DOMESTIC POLICY SPACE (SNARE) — Trapped by Article IV's legal binding on U.S. monetary sovereignty. The U.S. cannot expand monetary supply, reduce gold reserves, or adjust policy in response to domestic economic pressures without violating international law. The constraint forces gold outflows to satisfy foreign demand at fixed price. Suppression is extreme: the alternative (domestic inflation, unemployment absorption) carries political and economic costs that make exit appear impossible. This is pure extraction — creditor nations extract monetary policy discretion from the U.S. economy.
constraint_indexing:constraint_classification(dollar_gold_convertibility__strict_convertibility_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREDITOR NATIONS / GOLD RESERVE HOLDERS (ROPE) — Net beneficiaries under strict convertibility. Hold enforceable claims on U.S. gold reserves at fixed price; can demand conversion anytime, creating an asymmetric advantage. Experience this constraint as coordination mechanism: the gold standard provides stable currency value, predictable exchange rates, and enforceability of trade agreements. Arbitrage exits available—can shift reserves, adjust trade policies, or diversify holdings. For these agents, the constraint solves the collective action problem of trust in fiat currency.
constraint_indexing:constraint_classification(dollar_gold_convertibility__strict_convertibility_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EMERGING MARKET CURRENCIES (SNARE) — Constrained by dollar hegemony anchored in Article IV convertibility. These currencies must peg to dollar or accept volatility. Cannot develop independent monetary policy; must absorb shocks transmitted through dollar instability. As U.S. faces mounting gold pressure, dollar instability increases, forcing emerging markets to either hold dollar-denominated assets (extractive) or break the peg (costly). The constraint extracts monetary policy discretion from non-convertible-currency issuers.
constraint_indexing:constraint_classification(dollar_gold_convertibility__strict_convertibility_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: BRETTON WOODS INSTITUTION / THEATER ANALYSIS (PITON) — The strict convertibility obligation is increasingly performative by the early 1960s. The institutional apparatus (IMF, World Bank, fixed-parity rules) maintains the ritual of convertibility even as the structural conditions (gold flows, speculative runs, Triffin dilemma) hollow out the system's function. Theater ratio is moderate (0.38) because the legal obligation remains binding and some conversions do occur—but the institution's core purpose (stable international credit) is degraded by the contradictions. Theater emerges as central banks begin managing the system with coordination rather than enforcement.
constraint_indexing:constraint_classification(dollar_gold_convertibility__strict_convertibility_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the perspective of economic theory, strict convertibility at a fixed price is presented as an immutable constraint on monetary policy—a natural law of sound finance. The argument: 'You cannot both maintain fixed convertibility, allow free capital flows, and conduct independent monetary policy; it is logically impossible.' This is King's Trilemma or the Mundell-Fleming constraint rendered as natural law. However, the structural data contradicts this: creditor nations benefit substantially from the constraint, making it not natural law but extracted governance. The engine's false summit detector will flag this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(dollar_gold_convertibility__strict_convertibility_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: INTERNATIONAL CENTRAL BANKING COALITION (TANGLED ROPE) — Organized actors (central banks, BIS participants) perceive genuine coordination function: convertibility provides a common unit of account, enables trade credit, and reduces currency manipulation. But the constraint also extracts from this coalition: the U.S. obligation constrains policy options for all connected central banks, and the system's fragility (gold pressure, speculation) forces defensive coordination (coordination of coordination). Requires active enforcement (central bank cooperation, forward swaps, gold pool arrangements) to maintain the fiction of stability.
constraint_indexing:constraint_classification(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, TR),
    TR >= 0.70.

:- end_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint extracts monetary policy discretion—the U.S. cannot expand money supply (which would threaten gold reserves), cannot adjust to domestic unemployment, and must allow gold outflows to satisfy foreign demand at fixed price. Initial extractiveness (0.42) reflects that the system provided genuine benefits in 1944 (postwar stability, confidence in dollar). But as gold reserves decline and speculative pressure mounts, extractiveness rises (0.62 by t=5, 0.68 by t=10) because the U.S. must choose between abandoning the obligation and accepting severe domestic policy constraints. By 1954, the extraction is severe: the U.S. faces persistent gold outflows, cannot respond with monetary expansion, and creditor nations increasingly call conversions. Suppression (0.72): Extreme and rising. The alternative to accepting the constraint is (in the strict reading) treaty breach—a violation of binding international law. This carries political, legal, and economic costs that make exit appear impossible. Additionally, any attempt to exit (announce flexible rates) would trigger immediate gold runs as dollar holders panic. The suppression rises (0.55 → 0.72) as confidence erodes and the system becomes more fragile—exiting becomes more costly because the market knows the system is breaking down. Theater ratio (0.25 → 0.38): The constraint begins with genuine enforcement mechanism (actual gold conversions). By t=5–10, enforcement increasingly occurs through coordination (central banks agree to support dollar, London Gold Pool pools reserves, forward swaps replace actual conversions) rather than legal obligation. The theater ratio rises because the performative apparatus (coordination, central bank cooperation) becomes more necessary to maintain the fiction of convertibility.
 *
 * PERSPECTIVAL GAP:
 *   The strict convertibility reading produces sharp perspectival gaps. The U.S. Treasury sees a snare (trapped by binding legal obligation, extracting domestic policy space). Creditor nations see a rope (stable coordination mechanism delivering currency reliability). The analytical observer risks seeing a mountain (the Mundell-Fleming trilemma: 'you cannot have fixed rates, free capital flows, and independent monetary policy simultaneously—it is logically impossible'). But the false summit detector will flag this as naturalization: the 'impossibility' is structural to a specific institutional choice (Article IV convertibility), not an immutable law. If the U.S. abandoned convertibility (as it does in 1971), the 'impossibility' disappears—revealing it was contingent on the constraint, not a law of nature. The Bretton Woods institution sees itself degrading (piton) as coordination replaces enforcement. The international central banking coalition sees mixed coordination and extraction (tangled rope) as they must cooperate to maintain stability while the system extracts policy discretion from all members.
 *
 * DIRECTIONALITY LOGIC:
 *   The strict reading treats Article IV as binding legal obligation, making the U.S. a victim/trapped agent with high d (structural position as target of extraction). Creditor nations are beneficiaries with arbitrage exits (can demand conversion, shift reserves, adjust policy) yielding low d. The derivation chain: U.S. power (powerful nominally, but trapped by obligation) + exit_options (trapped, no legal exit) + victim status (bears costs of maintaining convertibility) → high d → high f(d) ≈ 1.28 → high χ. Creditor nations: power (institutional) + exit_options (arbitrage) + beneficiary status (benefits from stable dollar) → low d ≈ 0.15 → f(d) ≈ -0.01 → near-zero or negative χ (they benefit from the constraint). The suppression (0.72) is NOT scaled by directionality—it reflects the structural barriers to exit (legal obligation, political costs, market panic) that apply equally to all parties. Extractiveness is scaled: χ = ε × f(d) × σ(S). For the U.S. (d=0.85, f(d)≈1.28), χ ≈ 0.68 × 1.28 × 1.2 (global scope) ≈ 1.04 (capped at 1.0). For creditor nations (d=0.15, f(d)≈-0.01), χ ≈ 0.68 × (-0.01) × 1.2 ≈ -0.01 (they experience negative extraction, i.e., benefit). This directional asymmetry is the core mechanism of the snare: extractiveness flows FROM the U.S. TO creditor nations.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict convertibility reading resolves the mandatrophy by making clear that Article IV's binding nature creates genuine asymmetry in extraction. The constraint is NOT a pure coordination problem (both sides benefiting) but a mixed mechanism where one side's benefit directly extracts from the other's policy space. The mandatrophy question—'Is this coordination or extraction?'—is answered by the structural data: creditor nations benefit (rope perspective), U.S. suffers (snare perspective), and the system requires active enforcement (central bank cooperation) to maintain the fiction that both sides benefit equally. The snare classification stands because one party (the U.S.) is trapped, beneficiaries exist (creditor nations), and exit is suppressed (no legal alternative, market panic if exit attempted).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_bindingness_vs_contingent_commitment,
    'Is Article IV a legally binding constraint on U.S. sovereignty or a contingent international commitment revocable by U.S. policy choice?',
    'Examine treaty language, ratification procedures, and U.S. legislative authority over convertibility. Trace the shift from Article IV as binding law to Article IV as policy commitment that can be suspended (August 1971).',
    'If legally binding: U.S. is victim/trapped in the constraint structure, and snare classification is correct. If contingent commitment: U.S. retains exit option (arbitrage), reclassifying perspective as rope or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_bindingness_vs_contingent_commitment, conceptual, 'Distinction between legal bindingness and contingent policy commitment').

omega_variable(
    gold_outflow_causality,
    'Do gold outflows drive the constraint''s severity, or does the constraint''s legal structure necessitate gold outflows as the extraction mechanism?',
    'Counterfactual analysis: under flexible exchange rates (no Article IV), would gold outflows occur? Under Article IV with different initial conditions (larger gold reserves), would extractiveness change? Historical comparison across countries.',
    'If outflows are mechanically necessary: extractiveness reflects structural logic of convertibility (high ε ≈ 0.68 correct). If outflows reflect speculative behavior or political choices: extractiveness may be lower and suppression mechanism differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_outflow_causality, empirical, 'Whether gold outflows are mechanically required or contingent on demand behavior').

omega_variable(
    convertibility_reading_distinction,
    'This constraint represents the STRICT CONVERTIBILITY READING of the contested kernel (dollar_gold_convertibility). What distinguishes this reading from the sibling readings (policy_flexible_reading, triffin_structural_reading), and which reading describes the actual institutional reality?',
    'Comparative analysis of reading axioms: strict reading treats Article IV as binding legal obligation; policy_flexible_reading treats it as revocable policy choice; triffin_structural_reading treats it as structural impossibility. Examine historical decision-making (Eisenhower, Kennedy, Johnson administrations) to determine which reading best predicts policy behavior and institutional framing.',
    'If strict reading is correct: snare classification stands. If policy_flexible_reading dominates institutional framing: U.S. is actually mobile/arbitrage (not trapped), and classification shifts to rope. If triffin_structural_reading is correct: all readings coexist as recognition of the system''s internal contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convertibility_reading_distinction, conceptual, 'Kernel reading distinction and institutional reality mapping').

omega_variable(
    suppression_mechanism_source,
    'Does suppression (0.72) derive from legal obligation, economic necessity, or political/ideological commitment to the Bretton Woods system?',
    'Analyze U.S. policy debates: Was exit (Truman''s option to abandon convertibility) perceived as legally impossible, economically ruinous, or politically illegitimate? Trace rhetorical framing in policy documents and congressional testimony.',
    'If legal: suppression reflects binding constraint. If economic: suppression reflects real costs of exit (inflation, unemployment, trade disruption). If ideological: suppression reflects commitment capture—the U.S. internalized the constraint as legitimate. Different suppression sources imply different identity_locked vs trapped modalities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Source of suppression: legal obligation vs economic necessity vs ideological commitment').

omega_variable(
    beneficiary_enforcement_capacity,
    'Do creditor nations actively enforce the convertibility obligation through threat and demand, or does the U.S. voluntarily honor it through institutional commitment?',
    'Trace instances of actual or threatened gold conversion demands (London Gold Pool operations, French demand for gold conversion 1965, Switzerland''s convertibility rights). Distinguish between enforcement capacity and institutional habitus.',
    'If enforcement capacity is real: snare classification is strengthened (external force creates suppression). If voluntary commitment: constraint is more rope-like (both parties benefit from the system). Mixed evidence suggests tangled_rope despite high extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_enforcement_capacity, empirical, 'Active enforcement vs voluntary institutional compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_strict_theater_t0, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dgc_strict_theater_t5, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(dgc_strict_theater_t10, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(dgc_strict_extract_t0, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dgc_strict_extract_t5, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(dgc_strict_extract_t10, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dgc_strict_suppress_t0, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dgc_strict_suppress_t5, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(dgc_strict_suppress_t10, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, global_infrastructure).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, triffin_dilemma).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_institutional_fragility).

% DUAL FORMULATION NOTE:
% The dollar-gold convertibility kernel decomposes into three constraint stories corresponding to three competing readings held by different institutional actors at different times. The strict_convertibility_reading (this story) treats Article IV as binding law and produces snare classification. The policy_flexible_reading treats it as revocable policy and produces tangled_rope (mixed coordination and extraction with exit option). The triffin_structural_reading treats it as logically impossible and produces mountain (natural law of monetary trilemma). These are NOT the same constraint viewed from different angles—they have different ε values, different beneficiary/victim structures, and different stability properties. The strict reading's ε (0.68) reflects high extraction under the assumption that the constraint is binding. The policy_flexible reading's ε (estimated 0.35) reflects lower extraction when exit is recognized as possible. The triffin_structural reading's ε (estimated 0.05) reflects that the 'constraint' is actually a natural limit that no institutional choice can overcome. Link all three stories as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
