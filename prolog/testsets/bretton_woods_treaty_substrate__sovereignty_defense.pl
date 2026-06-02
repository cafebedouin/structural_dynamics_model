% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Treaty Substrate: Sovereignty Defense Reading
 *   domain: international_political_economy/monetary_institutions
 *
 * SUMMARY:
 *   The Bretton Woods Treaty (1944) established a system of fixed exchange
 *   rates anchored to gold and the U.S. dollar, presented as a mechanism for
 *   preserving national monetary sovereignty while enabling international
 *   trade coordination. This constraint story instantiates the SOVEREIGNTY
 *   DEFENSE reading: the system functions as a snare for non-reserve-currency
 *   states (forced dollar dependence, subordination of monetary autonomy), a
 *   rope-like coordination mechanism for the U.S. (exorbitant privilege,
 *   seigniorage), and a tangled rope for other powerful states (stability
 *   benefits offset by inflation export and limited influence). The
 *   extractiveness trajectory (0.35→0.62) reflects the historical dynamic: at
 *   founding, the constraint appeared as genuine coordination with modest
 *   asymmetry. As U.S. inflation accumulated and the Triffin dilemma
 *   tightened (1955-1965), non-reserve states faced increasing pressure to
 *   absorb dollar overhang through either accepting inflation or accumulating
 *   reserves they could not convert. By 1965, the extraction mechanism had
 *   sharpened — gold outflows accelerated, conversion pressure mounted, and
 *   the suppression required to keep non-reserve states in the system
 *   increased. The system collapsed in 1971 when the constraint could no
 *   longer be maintained. This reading emphasizes that the
 *   sovereignty-preservation rhetoric of Bretton Woods concealed an
 *   asymmetric architecture that served the interests of the reserve currency
 *   issuer and capital surplus states at the expense of deficit states and
 *   gold creditors.
 *
 * KEY AGENTS:
 *   - U.S. Treasury / Federal Reserve (institutional/arbitrage): Primary beneficiary — captures exorbitant privilege of dollar issuance, seigniorage, and ability to finance deficits via currency expansion without external constraint
 *   - Non-Reserve-Currency States (powerless/trapped): Primary victim — forced into dollar dependency for international transactions; must hold dollar reserves; subordinate monetary policy to U.S. preferences; cannot exit without losing access to global payments infrastructure
 *   - Gold-Backed Creditors (France, Switzerland, Germany) (organized/constrained): Secondary victims and organized challengers — accumulated gold reserves and demanded conversion at fixed parity; benefits from exchange stability but bears cost of U.S. inflation and lacks proportional influence on Fed policy
 *   - Western Europe / Capital Surplus States (powerful/constrained): Mixed position — constrained by dollar dependence but benefit from reconstruction financing (Marshall Plan) and capital inflows; constrained but not trapped; some negotiation power
 *   - Developmental States (organized/analytical): Treat constraint as temporary scaffold with development exit (import substitution, capital controls) — see Bretton Woods as supporting industrialization pathway with planned sunset via regional trade and domestic financial deepening
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.58).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.68).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Treaty Substrate: Sovereignty Defense Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_institutions").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, 'bretton-woods-sovereignty-defense-2026').
narrative_ontology:cs_kernel_codification('bretton-woods-sovereignty-defense-2026', formalized).
narrative_ontology:cs_authority_grounding('bretton-woods-sovereignty-defense-2026', extraction).
narrative_ontology:cs_interpretation_layer_present('bretton-woods-sovereignty-defense-2026').
narrative_ontology:cs_reading_relation('bretton-woods-sovereignty-defense-2026', keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('bretton-woods-sovereignty-defense-2026', neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('bretton-woods-sovereignty-defense-2026', foundational, reserve_currency_subordination_inherent).
narrative_ontology:cs_axiom_status(reserve_currency_subordination_inherent, holdable).
narrative_ontology:cs_axiom_grounding('bretton-woods-sovereignty-defense-2026', reserve_currency_subordination_inherent, empirically_contingent).
narrative_ontology:cs_axiom('bretton-woods-sovereignty-defense-2026', foundational, exorbitant_privilege_incompatible_equal_sovereignty).
narrative_ontology:cs_axiom_status(exorbitant_privilege_incompatible_equal_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('bretton-woods-sovereignty-defense-2026', exorbitant_privilege_incompatible_equal_sovereignty, deontological).
narrative_ontology:cs_reference_frame('bretton-woods-sovereignty-defense-2026', gold_backed_monetary_discipline).
narrative_ontology:cs_drift_state('bretton-woods-sovereignty-defense-2026', contemporary_post_1971, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('bretton-woods-sovereignty-defense-2026', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, capital_surplus_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, gold_reserve_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-RESERVE CURRENCY STATE (SNARE) — Trapped by the dollar-centric architecture. Must hold dollar reserves to conduct international trade; cannot exit without losing access to global payments infrastructure. Suppression mechanism: capital controls threatened against defectors, unequal access to IMF financing. Maximum extraction — the state's monetary autonomy is structurally subordinated to U.S. policy.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__sovereignty_defense, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESERVE CURRENCY ISSUER / U.S. TREASURY (ROPE) — Experiences the constraint as coordination mechanism: Bretton Woods architecture enables the U.S. to finance external deficits, export capital, and influence global economic policy through the privileged position of dollar issuance. The perceived function is coordination of international payments — the extraction is invisible from this position because the issuer derives exorbitant privilege (the ability to finance deficits via currency creation while others must balance payments through exports or capital flows).
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__sovereignty_defense, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: WESTERN EUROPEAN STATE (TANGLED ROPE) — Constrained by dollar dependence for reconstruction financing (Marshall Plan tied to dollar reserves) but benefits from access to large U.S. capital markets and the coordination function of fixed-rate exchange rates for intra-European trade. Mixed extraction: U.S. inflation exports (Triffin dilemma) force adjustment, but European growth is subsidized by capital inflows. Moderate power enables some negotiation room; not trapped, but constrained.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: GOLD-BACKED CREDITOR COALITION (TANGLED ROPE) — States that accumulated gold reserves (France, Germany, Switzerland) and demanded conversion at fixed parity. They benefit from exchange-rate stability for trade but bear the cost of U.S. monetary expansion (inflation) without proportional influence on Fed policy. Organized enough to extract concessions (Roosa bonds, swap lines) but constrained by collective action problems (coordinating currency defense). The constraint functions as both coordination (stable exchange rates) and extraction (forced absorption of U.S. inflation).
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPMENTAL STATE / ALTERNATIVE FRAMEWORK (SCAFFOLD) — States pursuing import substitution and capital controls (India, Brazil, South Korea) treat Bretton Woods as a temporary coordination structure with an internal sunset: as domestic industries develop, reliance on dollar financing decreases. The constraint is experienced as support for development (access to foreign exchange for capital goods imports) with a planned exit via industrialization and regional trade partnerships. Theater low because the development logic is functionally genuine, not performative.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__sovereignty_defense, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, monetary discipline under gold standard is an immutable natural law: currencies must be backed by specie, deficits must be corrected by contraction or devaluation, no state can sustainably spend beyond its means. The constraint appears as a mathematical necessity, not a contingent institutional arrangement. However, the structural data — identifiable beneficiaries (U.S., dollar bloc), victims (non-reserve states), and active enforcement (IMF conditionality) — contradicts this naturalization. The engine will detect this as a false summit: monetary discipline is a political choice anchored to institutional power, not a law of nature.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__sovereignty_defense, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__sovereignty_defense, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__sovereignty_defense, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. At founding (1944), the asymmetry was masked by genuine coordination benefits — Bretton Woods did stabilize exchange rates and enable trade recovery. But the baseline extractiveness reflects the structural subordination of non-reserve states to dollar-cycle policy: they must hold dollar reserves and accept whatever monetary conditions U.S. policy produces. By 1965, extractiveness rises sharply as the Triffin dilemma tightens — non-reserve states accumulate dollars they cannot convert, effectively subsidizing U.S. external deficits. The rise from 0.35→0.62 captures the historical transition from coordination-with-asymmetry to clear extraction. Suppression (0.68, rising to 0.75): Non-reserve states face substantial barriers to exit — loss of trade access (capital controls threatened), unequal IMF financing terms, and the collective action problem of coordinating an alternative currency system. The suppression mechanism is multi-layered: legal (IMF articles restrict unilateral exchange controls), institutional (IMF conditionality threatens access to emergency financing), and structural (the dollar is the only functioning global reserve asset). Theater ratio (0.55): Moderate. The coordination narrative is partly genuine (exchange stability does reduce trade friction) but increasingly theatrical — by 1965, the system's function as a coordination mechanism is degrading while the rhetoric persists. IMF articles proclaim sovereignty preservation; practice shows subordination. The theater reflects the gap between the stated purpose (coordination) and actual operation (extraction via inflation export). The ratio remains below piton threshold because genuine coordination benefits persist (trade does grow under fixed rates), unlike a purely theatrical constraint.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap between the beneficiary (rope, sees coordination) and victims (snare, sees extraction). The U.S. Treasury experiences Bretton Woods as coordination — it enables the issuer to finance deficits, export capital, and influence economic policy through the privileged position of dollar issuance. Exorbitant privilege is invisible from this perspective because the U.S. is deriving it costlessly; it appears as the natural coordination benefit of leadership. Non-reserve states see the opposite: they are forced into dollar dependency, must accept whatever inflation the Fed produces, and cannot exit without losing access to trade. The gap arises because the constraint's asymmetry is built into its architecture — the reserve currency issuer is insulated from the external discipline that binds non-issuers. The powerful state (Europe) sees tangled rope — genuine benefits (stability, capital access) mixed with costs (inflation export, constrained policy space). The organized creditors see tangled rope with stronger emphasis on extraction — they attempted to extract concessions (Roosa bonds, IMF reforms) but faced collective action barriers. The developmental state sees scaffold — a temporary structure with an exit path via industrialization. The civilizational analytical observer risks seeing mountain (monetary discipline as natural law) but structural data contradicts this: the system requires active enforcement (IMF conditionality), identifiable beneficiaries and victims, and eventually collapses when suppression cost exceeds benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from structural position: beneficiary/victim status, power level, and exit options. The U.S. Treasury (institutional/arbitrage) has d ≈ 0.05 (full beneficiary): exorbitant privilege is the defining feature of their structural position. Non-reserve states (powerless/trapped) have d ≈ 0.95 (full target): they bear extraction and cannot exit. Western Europe (powerful/constrained) has d ≈ 0.55 (symmetric): both benefits (capital flows, trade stability) and costs (inflation export, constrained policy). Gold creditors (organized/constrained) have d ≈ 0.70 (mostly target): they accumulate unwanted dollars and cannot force conversion without breaking the system. The sigmoid f(d) maps these to experienced extraction magnitudes. The beneficiary's d=0.05 produces f(d)≈-0.12, washing out or reversing the base extraction metric — they don't experience snare. The trapped state's d=0.95 produces f(d)≈1.42, amplifying extraction. The organized creditor's d=0.70 produces f(d)≈1.08, significant but manageable. These are not simple d derivations but rather structural derivations grounded in power atom + exit options + beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing how the same institutional structure produces six distinct classification outcomes from different structural positions. The apparent contradiction — is it rope or snare? — dissolves when the observer position is specified. The U.S. issuer legitimately sees coordination (rope) because the system does reduce transaction costs and enable multi-party trade. The non-reserve state legitimately sees extraction (snare) because the system does subordinate their autonomy to dollar cycles. Both are true simultaneously from their respective positions. The reading's contribution is clarifying which position observes which type, and crucially, that the position determines the type, not the other way around. The false summit detector flags the natural-law perspective: monetary discipline appears immutable only from the standpoint of an agent who is insulated from its constraints (the reserve issuer, who can violate monetary discipline with fewer immediate consequences). From the trapped state's perspective, the discipline is visibly contingent — it persists via enforcement mechanisms that could be withdrawn or replaced. The mandatrophy resolution is: the six types are not incompatible readings of the same constraint but rather the same constraint experienced from six different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exorbitant_privilege_stability,
    'How sustainable is U.S. exorbitant privilege under the fixed-rate gold standard? Does the Triffin dilemma (reserve currency issuer''s conflicting obligations to domestic and international stability) represent a structural contradiction or a manageable technical problem?',
    'Historical trajectory analysis: if U.S. gold reserves decline and dollar confidence erodes despite attempts to sustain the peg, the dilemma is structural. If technical adjustments (Roosa bonds, IMF special drawing rights) can indefinitely defer the crisis, it is manageable. The empirical test is 1965-1971 data on U.S. gold flows and currency conversion pressure.',
    'If structural: the sovereignty_defense reading''s classification as tangled_rope is durable — the extraction mechanism persists until the system breaks. If manageable: the constraint could revert to rope classification if exorbitant privilege can be sustained through policy coordination rather than coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exorbitant_privilege_stability, empirical, 'Whether exorbitant privilege is sustainable under the gold standard').

omega_variable(
    capital_control_enforceability,
    'Are capital controls (through which non-reserve states maintain monetary discipline) actually enforceable, or do they degrade over time through evasion and financial innovation? Do they constitute genuine suppression or theatrical suppression?',
    'Comparative analysis of capital control effectiveness across countries and time periods (1950s U.K., 1960s France, 1970s Brazil). Measure: correlation between capital control tightness and actual cross-border capital flows; rates of evasion via trade mispricing, transfer pricing, and informal channels.',
    'If enforceable (theatrical suppression low): suppression metric ~0.68 is accurate, and the snare classification from the powerless state''s perspective is justified. If degraded (theatrical suppression high): suppression is theatrical, the tangled_rope classification dominates, and exit optionality is higher than the powerless-state perspective suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_enforceability, empirical, 'Enforceability of capital controls under Bretton Woods').

omega_variable(
    sovereignty_vs_coordination_framing,
    'Is the constraint primarily about monetary sovereignty defense (the reading''s core claim) or about stabilizing international trade coordination? Do these framings produce the same or different constraint structures?',
    'Discourse analysis of Bretton Woods negotiators'' intent (White vs. Keynes proposals); examination of whether the system was designed to preserve national monetary policy autonomy or to enable international payments coordination via multilateral rules. Check whether policy flexibility (e.g., capital controls) was treated as a right of member states or a temporary deviation from the ideal.',
    'If sovereignty-defense framing is dominant: the extraction mechanism (subordination of non-reserve states'' monetary autonomy to U.S. preferences) is the primary function, and the victim set is correct. If coordination framing is dominant: the constraint''s primary function is coordination, and extraction is a side effect rather than a core mechanism. This reshapes the claimed_type: sovereignty_defense reading maintains tangled_rope; coordination framing would produce rope with asymmetric benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_coordination_framing, conceptual, 'Whether the constraint is fundamentally about sovereignty defense or trade coordination').

omega_variable(
    embedded_liberalism_compatibility,
    'Does the sovereignty_defense reading coexist coherently with the embedded_liberalism reading, or do their core premises logically foreclose each other? Can a state simultaneously defend monetary sovereignty AND accept the embedded liberal bargain (capital controls permitted, but non-discriminatory trade)?',
    'Logical analysis of the two readings'' foundational axioms. If a state can use capital controls to defend monetary sovereignty while accepting non-discriminatory trade rules, they coexist. If accepting embedded liberalism logically commits a state to relinquish monetary sovereignty (because trade openness forces capital account openness), then sovereignty_defense forecloses embedded_liberalism. The empirical test is historical behavior: did signatories treat capital controls and trade liberalization as compatible or contradictory?',
    'If coexistent: reading_relations should be ''coexists_with''. If foreclosed: reading_relations should be ''forecloses''. This determines whether the kernel permits multiple simultaneous readings or forces a choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embedded_liberalism_compatibility, conceptual, 'Logical compatibility between sovereignty_defense and embedded_liberalism readings').

omega_variable(
    reading_evidence_base,
    'Is this reading (sovereignty_defense) grounded in the actual stated commitments of Bretton Woods signatories, or is it an interpretive retrospective imposition? Did contemporaries (1944-1950) understand the constraint as fundamentally about preserving monetary sovereignty, or did that framing emerge later as revisionist history?',
    'Primary source analysis: Bretton Woods Conference records, IMF founding documents, White Plan vs. Keynes Plan, early IMF staff analysis, statements by participating governments. Assess whether the sovereignty framing was explicit at founding or emerged later (e.g., during dollar-sterling conflicts in 1950s, French withdrawal, or post-1971 revisionism).',
    'If foundational: the reading''s authority_grounding is legitimate (lineage — direct transmission from founding intent). If retrospective: the reading''s authority is weaker (interpretation imposed on ambiguous kernel), and the cs_structure.authority_grounding should reflect this (distributed authority, interpretive drift). This affects whether the reading should be classified as lineage-grounded or interpretation-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_evidence_base, empirical, 'Historical evidence for sovereignty_defense framing in Bretton Woods').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwss_theater_1944_founding, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.42).
narrative_ontology:measurement(bwss_theater_1955_consolidation, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1955, 0.51).
narrative_ontology:measurement(bwss_theater_1965_triffin, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.55).

% Extraction over time
narrative_ontology:measurement(bwss_extractiveness_1944_founding, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(bwss_extractiveness_1955_consolidation, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1955, 0.48).
narrative_ontology:measurement(bwss_extractiveness_1965_triffin_crisis, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bwss_suppression_1944_founding, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.52).
narrative_ontology:measurement(bwss_suppression_1955_consolidation, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1955, 0.68).
narrative_ontology:measurement(bwss_suppression_1965_pressure, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__sovereignty_defense, 0.2).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, dollar_hegemony_post_1971).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, imf_structural_adjustment_conditionality).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, triffin_dilemma_unresolvable).

% DUAL FORMULATION NOTE:
% The Bretton Woods system decomposes into at least three distinct constraints with different ε values: (1) EXCHANGE RATE COORDINATION (ε≈0.15, rope) — genuine coordination benefit from fixed rates for trade; (2) SOVEREIGN MONETARY SUBORDINATION (ε≈0.58, tangled_rope) — this story; (3) GOLD STANDARD CREDIBILITY TRAP (ε≈0.72, snare) — the Triffin dilemma forcing gold outflows. Each story has its own perspectives and temporal trajectory. Network edges show how the monetary-subordination constraint upstream influences both the post-1971 dollar hegemony (structure persists after institutional mechanism changes) and IMF conditionality (inherits the enforcement machinery for non-reserve states). The Triffin dilemma is upstream — it makes the monetary-subordination constraint unsustainable — and affects this story's terminal state (system collapse inevitable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
