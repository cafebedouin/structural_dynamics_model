% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility as Structural Trap (Triffin Dilemma Reading)
 *   domain: international_political_economy/monetary_policy/bretton_woods
 *
 * SUMMARY:
 *   The Triffin structural reading instantiates the dollar-gold
 *   convertibility constraint as a fundamentally unsustainable design flaw
 *   inherent to the Bretton Woods architecture, in which the US assumes
 *   simultaneous roles that are mathematically incompatible: providing a
 *   stable reserve currency, maintaining convertibility to gold at fixed
 *   parity, and preserving monetary policy autonomy. This reading emphasizes
 *   that both the issuer (US) and creditor nations face extractive traps —
 *   the US cannot inflate or expand fiscal policy without losing gold;
 *   creditor nations cannot exit dollar accumulation without triggering the
 *   system's collapse. The constraint operates as a dual-victim snare where
 *   each actor is locked into behaviors that perpetuate an unsustainable
 *   architecture. Extractiveness rises from 0.35 (1944, design phase,
 *   structural flaws not yet visible) to 0.78 (1968, two-tier gold market,
 *   gold pool breakdown, capital controls proliferating, Kennedy-Johnson
 *   inflation) before collapsing to 0.68 when the system breaks in 1973.
 *   Theater ratio rises from 0.42 to 0.68 as policymakers engage in
 *   increasingly performative defense operations — gold pool management,
 *   Roosa bonds, special drawing rights — that manage symptoms rather than
 *   resolve the structural contradiction. Suppression rises from 0.45 to 0.80
 *   as capital controls, gold export restrictions, and monetary constraints
 *   tighten to defend the parity. The three sibling readings are: (1)
 *   strict_convertibility_reading — emphasizes gold backing as a genuine
 *   constraint on monetary expansion, treating any deviation as policy
 *   failure rather than structural necessity; (2) policy_flexible_reading —
 *   argues that modest reforms (gold price adjustment, parity band widening)
 *   could have stabilized the system indefinitely if adopted early. The
 *   Triffin reading forecloses the strict reading's core premise (that
 *   convertibility can be maintained through fiscal discipline) and coexists
 *   with the flexible reading (whose reforms, the Triffin reading contends,
 *   would only delay inevitable breakdown).
 *
 * KEY AGENTS:
 *   - United States (Reserve Currency Issuer): Primary victim (powerful/trapped) — locked into impossible choice between monetary autonomy, convertibility maintenance, and reserve role adequacy; trapped because exiting any role collapses the system.
 *   - Creditor Nations (UK, France, Japan, West Germany, etc.): Co-victim (powerful/trapped) — forced to accumulate dollars for reserves, exposed to devaluation risk, unable to convert to gold without system collapse; trapped because all exit routes lead to financial exposure.
 *   - Bretton Woods Institutional Order (IMF, gold pool, parity defense): Institutional beneficiary (institutional/arbitrage) — the formal apparatus experiences convertibility as coordination mechanism; benefits from stability of fixed-parity governance.
 *   - International Financial Community (Banks, Corporations): Mixed position (organized/constrained) — benefits from trade certainty (coordination) but extracts arbitrage from system contradictions (extraction); constrained because departure threatens access to dollar liquidity.
 *   - Floating Exchange Rate Coalition (Economists, Policy Reformers): Beneficiary of collapse (organized/constrained) — advocates see fixed convertibility as temporary coordination failure; benefit from the eventual transition to floating regimes that enable monetary autonomy.
 *   - Post-Bretton Woods Floating Regime Architecture: Ultimate beneficiary — the constraint's eventual collapse enables the alternative regime that frees monetary policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.68).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.72).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, snare).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility as Structural Trap (Triffin Dilemma Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_policy/bretton_woods").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '803a48cc-09f8-42dd-a374-66c968c64428').
narrative_ontology:cs_kernel_codification('803a48cc-09f8-42dd-a374-66c968c64428', formalized).
narrative_ontology:cs_authority_grounding('803a48cc-09f8-42dd-a374-66c968c64428', extraction).
narrative_ontology:cs_interpretation_layer_present('803a48cc-09f8-42dd-a374-66c968c64428').
narrative_ontology:cs_reading_relation('803a48cc-09f8-42dd-a374-66c968c64428', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('803a48cc-09f8-42dd-a374-66c968c64428', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_axiom('803a48cc-09f8-42dd-a374-66c968c64428', foundational, convertibility_trilemma_structural_necessity).
narrative_ontology:cs_axiom_status(convertibility_trilemma_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('803a48cc-09f8-42dd-a374-66c968c64428', convertibility_trilemma_structural_necessity, empirically_contingent).
narrative_ontology:cs_axiom('803a48cc-09f8-42dd-a374-66c968c64428', foundational, dual_victim_extraction_lock).
narrative_ontology:cs_axiom_status(dual_victim_extraction_lock, holdable).
narrative_ontology:cs_axiom_grounding('803a48cc-09f8-42dd-a374-66c968c64428', dual_victim_extraction_lock, empirically_contingent).
narrative_ontology:cs_created_at('803a48cc-09f8-42dd-a374-66c968c64428', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_reserve_currency_role).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_foreign_exchange_management).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: US RESERVE CURRENCY TRAP (SNARE) — Issuer of reserve currency is locked into an impossible trilemma: maintain gold convertibility at fixed parity (requires monetary discipline incompatible with domestic full employment), provide adequate dollar liquidity for global trade (requires deficit spending that undermines convertibility), or allow sovereign monetary autonomy (incompatible with fixed parity obligation). The US cannot exit without collapsing the system it benefits from nominally; yet the system extracts maximum cost as the contradiction intensifies. Trapped: exiting the constraint requires ending the Bretton Woods order itself.
constraint_indexing:constraint_classification(dollar_gold_convertibility__triffin_structural_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREDITOR NATIONS TRAP (SNARE) — Creditor countries (UK, France, Japan, etc.) accumulate dollars as reserves but face the inverse extraction: holding dollars exposes them to US inflation risk and eventual currency devaluation; converting to gold triggers gold drain and system collapse; refusing to accumulate dollars starves the system of liquidity needed for their own trade. Trapped: all three exit routes lead to system breakdown and sovereign financial exposure. The constraint extracts from both the issuer and the creditors simultaneously — a rare dual-target snare.
constraint_indexing:constraint_classification(dollar_gold_convertibility__triffin_structural_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: BRETTON WOODS ORDER (ROPE) — The formal institutional apparatus (IMF, fixed parities, gold pool mechanism) experiences the convertibility obligation as a coordination mechanism: it enables predictable international trade, stable exchange rates, and capital flow management. The institutional perspective sees the constraint as functional and necessary — the performative management of gold pools and parity defense appears as legitimate governance. The beneficiary from stability, not from extraction.
constraint_indexing:constraint_classification(dollar_gold_convertibility__triffin_structural_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL COMMUNITY (TANGLED ROPE) — International banks and multinational corporations benefit from fixed-parity certainty for trade and long-term contracting (coordination function) but also extract arbitrage profits from parity defense operations, gold speculation, and the systematic asymmetries the constraint creates. Exit constrained: leaving the system means losing access to dollar liquidity and stability services, yet staying means exposure to eventual devaluation or collapse. Mixed experience — genuine coordination benefit coupled with extractive opportunity.
constraint_indexing:constraint_classification(dollar_gold_convertibility__triffin_structural_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FLOATING REGIME COALITION (SCAFFOLD) — Economists, policy reformers, and financial technologists see fixed convertibility as a temporary coordination failure with a clear sunset path: floating exchange rates, capital controls, and decoupled monetary autonomy. This coalition views the constraint as a transitional coordination problem (1944-1973) with built-in obsolescence. Low effective extraction from this perspective because the coalition has agency, clear policy pathway, and sees the endpoint. The beneficiary of the eventual transition — floating regimes reduce monetary extraction once convertibility obligation dissolves.
constraint_indexing:constraint_classification(dollar_gold_convertibility__triffin_structural_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the Triffin dilemma appears as an immutable structural impossibility: no nation can simultaneously maintain fixed exchange rates, provide reserve currency liquidity, and retain monetary policy autonomy. This perspective treats the trilemma as a fundamental economic law — a natural limit on monetary arrangements. However, the structural data contradicts the mountain classification: the trilemma is a consequence of SPECIFIC institutional choices (Bretton Woods design), not a physical or logical necessity. The engine will flag this as a false summit — the 'impossible' framing naturalizes what is actually a contingent architecture.
constraint_indexing:constraint_classification(dollar_gold_convertibility__triffin_structural_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The Triffin dilemma extracts from both the US and creditor nations through an impossible trilemma. The US must choose between maintaining gold backing (requires monetary discipline incompatible with postwar full employment commitments), providing adequate liquidity (requires deficits that undermine convertibility), or retaining monetary autonomy (incompatible with parity obligation). Creditors must choose between holding devaluation-risk dollars, converting to gold and triggering system collapse, or refusing dollars and starving trade of liquidity. Both face extraction in the form of constrained policy options and forced participation in unsustainable arrangements. The measurement trajectory shows extractiveness rising as the trilemma intensifies: from 0.35 in 1944 (design phase, constraints not yet binding) to 0.78 in 1968 (maximum pressure, gold pool bleeding, capital flight) before collapsing to 0.68 when the system breaks. Suppression (0.72): High. The system enforces compliance through multiple mechanisms: capital controls, gold export restrictions, mandatory parity defense operations, and the implicit threat of system collapse if anyone exits. By 1968, London gold pool operations required daily intervention; by 1972, the Smithsonian Agreement attempted to narrow parity bands while maintaining the core obligation. The measurement trajectory shows suppression rising from 0.45 to 0.80 as enforcement intensifies, then settling at 0.72 after the system collapses — residual suppression from the institutional memory that exit was forced rather than chosen. Theater ratio (0.58): Moderate-high. The constraint includes genuine coordination (fixed parities do reduce trade uncertainty) but increasingly performative defense operations. By the 1960s, policymakers adopted measures that managed symptoms rather than resolving the underlying trilemma: Roosa bonds (short-term debt instruments to manage gold flows), two-tier gold market (separating official and private gold prices), Special Drawing Rights (attempting to create a substitute reserve asset). The theater increases from 0.42 to 0.68 as the gap widens between the formal system's claims (sustainable coordination through convertibility) and reality (unsustainable trilemma requiring constant intervention). Theater drops to 0.58 after 1973 because the fiction is no longer needed — floating rates require no performative defense.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound: the trapped reserve-currency issuer and creditor nations (snare classification) experience the same institutional order that the institutional beneficiary (rope classification) experiences as functional coordination. The gap reveals the false summit dynamic — from the civilizational analytical perspective, the trilemma appears as an immutable law (mountain), but this naturalizes what is actually a contingent institutional choice. The floating regime coalition (scaffold) sees the constraint as a temporary failure with a clear exit path, contrasting with the trapped victims who see no exit at all. The financial community (tangled rope) mixes genuine benefit (trade stability) with extraction (arbitrage opportunity from system contradiction). The perspectival multiplicity demonstrates that this is not a single constraint but a presheaf: the same structural phenomenon appears as immutable law, functional coordination, temporary coordination failure, mixed benefit-extraction, and dual-victim trap depending on the observer's position and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the US and creditor nations derive high directionality values (d approaching 1.0) — they are dual victims rather than beneficiaries or observers. The US faces d ≈ 0.92 as a victim of its own reserve currency role: the power it appears to hold (issuing the world's reserve currency) becomes a trap (unable to devalue, inflate, or adjust without system collapse). Creditors face d ≈ 0.88 as victims of forced dollar accumulation: their power in nominal terms (holding reserve balances) masks structural exposure (eventual revaluation, forced participation in unsustainable arrangement). The beneficiary is the institutional order itself (Bretton Woods apparatus) and ultimately the floating regime that emerges from the system's collapse — d ≈ 0.05 for the institutional beneficiary (the IMF, parity system, formal coordination mechanisms experience minimal extraction, only the cost of maintaining the apparatus). The post-1973 floating regime experiences d ≈ 0.00 (pure beneficiary — it captures all the benefits of monetary autonomy without the constraint). The Bretton Woods coalition (institutional/arbitrage perspective) experiences moderate extraction from defending fixed parities but benefits from coordination function — d ≈ 0.50, consistent with tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CHALLENGE — DUAL VICTIM SNARE: This constraint challenges the standard mandatrophy resolution because it is a snare where both the apparent issuer-beneficiary (US) and the apparent beneficiary-counterparties (creditor nations) are locked into victim positions. The mandatrophy asks: is this extraction mechanism functioning to preserve coordination (rope/tangled rope, some genuine coordination benefit) or pure extraction (snare, minimal coordination function)? The Triffin reading answers: it is pure extraction disguised as coordination. The fixed parities do provide trade certainty (coordination benefit), but the dual-victim lock mechanism (both issuer and creditors face impossible choices) reveals the coordination function is incidental to the extraction mechanism. The structure persists 1944-1973 not because the coordination is valuable (floating rates eventually provide equivalent coordination) but because the extraction mechanism (monopoly on reserve creation, forced dollar accumulation, trapped exit options) locks both parties into the system despite its unsustainability. The mandatrophy resolution: recognizing the trilemma as inherent to the design (not a policy failure) converts the classification from rope (sustainable coordination) to snare (unsustainable extraction) by showing that the coordination benefit cannot be preserved without the victim lock. Remove the lock (floating rates) and the coordination benefit is replaced by alternative mechanisms (market-based pricing, diversified reserves, independent monetary policy). The extraction mechanism (forced dollar usage, concentrated reserve privilege) is eliminated. This is snare behavior: the system benefits from the lock remaining in place, and the extraction would collapse if either party achieved genuine exit. Mandatrophy resolved by structural analysis of what persists if the lock is removed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trilemma_logical_necessity,
    'Is the Triffin trilemma a necessary mathematical law of monetary systems, or a contingent consequence of Bretton Woods institutional design?',
    'Counterfactual institutional design analysis: examine whether alternative architectures (gold standard without reserve currency role, multiple reserve currencies, decentralized settlement) avoid the trilemma or embed it differently. Post-1973 floating regime performance as evidence of trilemma resolution or substitution.',
    'If necessary law: mountain classification vindicated — the constraint is immutable regardless of policy. If contingent: false summit confirmed — the ''impossibility'' is institutional, not physical, and other designs resolve it differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trilemma_logical_necessity, conceptual, 'Whether the trilemma is logically necessary or contingent to Bretton Woods design').

omega_variable(
    dual_victim_extraction_mechanism,
    'Does the constraint extract asymmetrically from US and creditor nations, or is extraction symmetric despite appearing directionally opposed?',
    'Comparative welfare analysis: measure real macroeconomic costs to US of maintaining gold convertibility (inflation constraint, foregone fiscal expansion) vs. creditor nations (exchange rate exposure, forced dollar accumulation, eventual devaluation loss). Time-series analysis of capital flows, gold flows, and reserve composition. Counterfactual: what would each actor''s economic trajectory have been under floating rates?',
    'If asymmetric extraction: snare classification correct, with US bearing greater cost. If symmetric: both countries bear equal costs through different mechanisms, suggesting tangled_rope for both. Classification changes imply different policy response strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_victim_extraction_mechanism, empirical, 'Whether extraction from US and creditors is asymmetric or symmetric').

omega_variable(
    policy_flexibility_channel,
    'Did US and creditor policymakers retain genuine options to reform Bretton Woods gradually (narrow band flexibility, parity adjustment mechanism, gold price increase), or was the system structurally locked into binary collapse?',
    'Historical institutional analysis of attempted reforms (Roosa bonds, two-tier gold market, Special Drawing Rights expansion, Smithsonian Agreement). Counterfactual: would earlier modest reforms (gold price increase to $40/oz in 1960s, permitted parity band widening, reserve asset diversification) have stabilized the system or merely delayed inevitable breakdown?',
    'If genuine policy flexibility existed: the constraint is less of a snare and more of a tangled_rope (some escape routes available but politically costly). If structurally locked: snare classification holds — the system had no viable reform path once dilemma recognized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_flexibility_channel, empirical, 'Degree of policy flexibility within Bretton Woods architecture').

omega_variable(
    beneficiary_identity_ambiguity,
    'Who is the actual beneficiary of the Bretton Woods constraint: the US as reserve currency issuer (capturing seigniorage), or the post-1973 floating regime that enabled monetary autonomy?',
    'Comparative institutional analysis: measure seigniorage capture by US during Bretton Woods (1944-1973) vs. after (1973-present). Assess whether floating regime''s stability and the US dollar''s continued dominance represent continuation or dissolution of the constraint''s beneficiary structure. Examine whether ''beneficiary'' is temporal: the trap benefits no one once revealed, but floating regime benefits from the prior constraint''s collapse enabling autonomy.',
    'If US is beneficiary: constraint is institutional extraction masquerading as coordination (false summit risk). If floating regime is beneficiary: the constraint is an extractive bridge to a new architecture. Changes framing of who extracted value from whom and when.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, conceptual, 'Identity of the constraint''s beneficiary: issuer, creditors, or post-Bretton Woods architecture').

omega_variable(
    knowledge_and_inevitability,
    'Was the Triffin dilemma logically inherent in the Bretton Woods design from inception (1944), or did it emerge as a surprise in the late 1950s-60s as real-world conditions deviated from design assumptions?',
    'Examination of contemporary 1944 economic analysis, Keynes-White plan debates, and early post-war policy discussions. Assessment of whether the trilemma was mathematically demonstrable ex ante or only visible ex post through empirical drift. Comparison with Triffin''s 1960 publication and response by policymakers — did awareness change the system''s trajectory or merely document its inevitability?',
    'If inherent from inception: the constraint was knowably unsustainable and its perpetuation represents institutional denial (theater increases). If emergent: the system operated under genuine uncertainty about its duration, changing the moral and epistemic status of policymaker choices during the 1960s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_and_inevitability, empirical, 'Whether Triffin dilemma was logically inherent or emerged through historical drift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_triffin_theater_1944, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dgc_triffin_theater_1952, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(dgc_triffin_theater_1960, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 16, 0.56).
narrative_ontology:measurement(dgc_triffin_theater_1968, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 24, 0.68).
narrative_ontology:measurement(dgc_triffin_theater_collapse_1973, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 29, 0.58).

% Extraction over time
narrative_ontology:measurement(dgc_triffin_extract_1944, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dgc_triffin_extract_1952, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(dgc_triffin_extract_1960, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(dgc_triffin_extract_1968, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(dgc_triffin_extract_collapse_1973, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 29, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dgc_triffin_suppress_1944, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dgc_triffin_suppress_1960, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(dgc_triffin_suppress_1968, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(dgc_triffin_suppress_collapse_1973, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 29, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, bretton_woods_gold_pool_operational).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_hegemony_post_1973).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, reserve_currency_seigniorage_asymmetry).

% DUAL FORMULATION NOTE:
% The dollar-gold convertibility constraint has three structurally distinct readings with different ε values and beneficiary/victim configurations: strict_convertibility_reading (ε≈0.25, treating breakdown as policy failure, rope classification dominant), policy_flexible_reading (ε≈0.42, treating system as reformable tangled rope), and triffin_structural_reading (ε≈0.68, treating system as structurally unsustainable snare). Each reading produces different upstream/downstream constraints. The Triffin reading views the system's eventual collapse and transition to floating rates as endemic to the structure, making the floating regime a downstream beneficiary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
