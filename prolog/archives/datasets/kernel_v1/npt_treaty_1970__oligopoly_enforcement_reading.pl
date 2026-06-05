% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Oligopoly Enforcement (Articles I-II Asymmetric Obligation Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The NPT (1970) presents a kernel ambiguity: is it a disarmament bargain
 *   (Article VI aspirational path) or an oligarchy enforcement regime
 *   (Articles I-II binding asymmetry)? This constraint story instantiates the
 *   oligopoly enforcement reading — the interpretation that treats Articles
 *   I-II as the binding core and Article VI as performative vestige. Under
 *   this reading, the NPT is a tangled rope: the P5 coordinate technology
 *   denial and verification surveillance of non-weapon states while
 *   maintaining their own arsenals unchecked, gaining both coordination
 *   benefits (mutual verification, supplier cartel) and asymmetric extraction
 *   (deterrent monopoly, strategic hierarchy). NNWS gain security guarantees
 *   (umbrella allies) or transparency legitimacy (non-aligned movement), but
 *   at the cost of inspection burden, denied deterrent pathway, and endemic
 *   strategic inequality. Articles I-II enforce this extraction through IAEA
 *   safeguards, NSG export controls, and sanctions architecture; Article VI
 *   performs legitimacy through RevCon rhetoric and disarmament language with
 *   no binding enforcement. The regime's theater has increased over 50+ years
 *   as Article VI has atrophied from implied binding obligation (1968
 *   negotiating expectation) to purely ceremonial covenant (2020s reality: no
 *   disarmament, no timeline, no enforcement).
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (P5): Collective institutional beneficiary (arbitrage exit) — coordinate verification of rivals, control technology transfer, maintain deterrent monopoly while binding others to non-weapon threshold
 *   - Non-Aligned NNWS (NAM): Primary victim (powerless/trapped) — full-scope IAEA inspections, denied deterrent pathway, strategic vulnerability, catastrophic exit cost
 *   - Threshold States (India, Pakistan, Israel, Iran pathway): Secondary victim (moderate/constrained) — binary choice between compliance (inspection, no deterrent, umbrella-dependent) or exit (sanctions, isolation, but deterrent acquired)
 *   - Swing States (Japan, South Korea, Egypt, Turkey): Mixed position (powerful/constrained) — security umbrella + technology access in exchange for inspections and compliance
 *   - IAEA Secretariat: Institutional enforcer (institutional/arbitrage) — derives legitimacy from verification mandate; technical body executing Articles I-III
 *   - Nuclear Supplier Group: Cartel mechanism (institutional/arbitrage) — coordinates technology denial, enforces export controls, maintains NWS technology monopoly
 *   - Non-Aligned Movement & Disarmament Advocates: Organized coalition (organized/mobile) — pushes Article VI enforcement, scaffolding pathway with sunset contingent on P5 disarmament
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.58).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.65).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Oligopoly Enforcement (Articles I-II Asymmetric Obligation Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'e6c65efc-24d4-45b1-90df-f1555c600b3b').
narrative_ontology:cs_kernel_codification('e6c65efc-24d4-45b1-90df-f1555c600b3b', formalized).
narrative_ontology:cs_authority_grounding('e6c65efc-24d4-45b1-90df-f1555c600b3b', extraction).
narrative_ontology:cs_interpretation_layer_present('e6c65efc-24d4-45b1-90df-f1555c600b3b').
narrative_ontology:cs_reading_relation('e6c65efc-24d4-45b1-90df-f1555c600b3b', npt_treaty_1970__reciprocal_disarmament_reading, forecloses).
narrative_ontology:cs_reading_relation('e6c65efc-24d4-45b1-90df-f1555c600b3b', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('e6c65efc-24d4-45b1-90df-f1555c600b3b', foundational, articles_i_ii_binding_enforcement).
narrative_ontology:cs_axiom_status(articles_i_ii_binding_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('e6c65efc-24d4-45b1-90df-f1555c600b3b', articles_i_ii_binding_enforcement, conventional).
narrative_ontology:cs_axiom('e6c65efc-24d4-45b1-90df-f1555c600b3b', foundational, article_vi_performative_covenant).
narrative_ontology:cs_axiom_status(article_vi_performative_covenant, holdable).
narrative_ontology:cs_axiom_grounding('e6c65efc-24d4-45b1-90df-f1555c600b3b', article_vi_performative_covenant, empirically_contingent).
narrative_ontology:cs_reference_frame('e6c65efc-24d4-45b1-90df-f1555c600b3b', p5_oligarchic_stability).
narrative_ontology:cs_drift_state('e6c65efc-24d4-45b1-90df-f1555c600b3b', contemporary_2023_arsenal_stagnation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6c65efc-24d4-45b1-90df-f1555c600b3b', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nws_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nws_security_umbrella_allies).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, nnws_inspection_targets).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_aligned_movement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED NNWS (SNARE) — IAEA inspections apply exhaustively; exit costs are catastrophic (international isolation, sanctions, military intervention). No domestic nuclear deterrent pathway available. Trapped by comprehensive safeguards while NWS maintain arsenals unchecked. Maximum extraction: burden of proof, verification cost, strategic vulnerability asymmetry.
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THRESHOLD STATES (SNARE) — States with nascent nuclear capability face a binary: formally renounce deterrent pathway under NPT or exit the regime and face sanctions. Those that chose exit (India 1974, Pakistan follow-on, North Korea 2003) paid severe costs but gained deterrent capacity. Those that remain bound (Iran pre-JCPOA, Egypt, Turkey, Japan) are constrained by inspections and denied the security guarantee that NWS enjoy. Extraction flow: sacrifice deterrent for compliance, with no reciprocal NWS disarmament.
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SWING STATES (TANGLED ROPE) — States with advanced nuclear fuel cycles, security vulnerabilities, and strong allies among NWS. Gain security guarantees (umbrella) in exchange for NPT compliance and inspections. Mixed extraction/coordination: inspection burden is real, but security umbrella and technology access are genuine benefits. Constrained exit (withdrawal triggers ally abandonment and sanctions) but not trapped. Chi moderately high due to inspection asymmetry but offset by security coordination.
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: IAEA (ROPE) — Technical body that implements verification under Article III. Derives legitimacy and budget from verification mandate; functions as pure coordination mechanism on Article I-II surveillance. No victim relationship; sees the regime as enabling expertise deployment. Benefits from expansion of safeguards reach. Low extraction (institutional arbitrage position: maintains technical authority through verification).
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NUCLEAR SUPPLIER GROUP (ROPE) — Mechanism coordinating technology denial/control. Does not extract on its own; rather, enables NWS to extract verification concessions from technology-seeking states. Sees regime as coordination of supplier interests (technology control, market access). Beneficiary position (arbitrage: maintains market power through coordinated export controls).
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-ALIGNED MOVEMENT & ARTICLE VI ADVOCATES (SCAFFOLD) — Coalition of NNWS, civil society, and progressive NWS (UK, France constituencies) pushing for time-bound disarmament. See Articles I-II as temporary enforcement pending Article VI implementation. Theater moderate because the coalition has demonstrated organizing capacity (RevCons, treaty pressure). Low chi because they have exit options (coalition withdrawal from regime, legal challenge, diplomatic pressure). Sunset contingent on NWS disarmament progress — structured as temporal scaffolding.
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: P5 NUCLEAR WEAPON STATES (TANGLED ROPE) — Collective beneficiary under Articles I-II. Gain coordination benefits (verification of rivals' compliance, technology export control coordination via NSG). Simultaneously extract: maintain arsenal while binding others to no-weapon threshold; enforce inspections while avoiding accountability (Article VI unenforceable); dictate security norms while denying deterrent pathway to others. Active enforcement required to prevent defection by threshold states and to block disarmament pressure. Arbitrage exit: can withdraw (India pathway) but collectively maintain institutional regime. Asymmetric extraction: enforcement burden on NNWS, none on P5.
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ARTICLE VI AS DEGRADED OBLIGATION (PITON) — Disarmament language in Article VI has become theatric: 'pursue negotiations in good faith' with no enforcement, no timeline, no binding milestones. Theater ratio 0.95 (purely performative). CTBT unsigned by major powers, FMCT never concluded, nuclear stockpiles stable or rising. Article VI persists as institutional inertia — necessary to maintain regime legitimacy with NNWS, but functionally abandoned. No actual disarmament pathway. Piton classification: the obligation is maintained through ritual invocation (RevCons, diplomatic language) while the underlying function (binding disarmament) has atrophied.
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective on great power politics, NPT codifies an immutable structural fact: only states with existing nuclear capacity can credibly maintain deterrent. Any other state attempting to acquire weapons faces an organized coalition (P5 + NSG + security umbrella allies) with overwhelming resources. Therefore, the regime's enforcement asymmetry is not contingent policy but a reflection of underlying power realities — the constraint 'emerges naturally' from the distribution of nuclear-production capacity and military force. However, this reading is a false summit: the asymmetry is maintained by the legal-institutional regime, not by physics. The regime creates the oligopoly; the oligopoly does not create the regime.
constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_treaty_1970__oligopoly_enforcement_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, TR),
    TR >= 0.70.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This represents moderate-high extraction with genuine coordination present. Articles I-II create a verification burden (inspections, transparency requirements, compliance costs) borne asymmetrically by NNWS. The P5 coordinate but do not reciprocate. However, some NNWS genuinely benefit from security umbrellas and technology access, so extractiveness is not maximal (snare-level 0.66+). The value has risen from 0.35 in 1970 to 0.58 in 2023 as disarmament expectations have eroded and the asymmetry has become normalized. Suppression (0.65): High suppression reflects the regime's enforcement architecture and exit barriers. IAEA inspections, NSG export controls, sanctions threat for withdrawal or non-compliance, and implicit military threat (Israel 1981 Osirak strike, Iraq 1991 forced inspections) create formidable barriers to exit. However, some states have exited successfully (India, Pakistan, North Korea) by accepting isolation costs, so suppression is not total. Suppression_requirement has increased from 0.50 to 0.65 as the enforcement apparatus has matured. Theater ratio (0.68): Moderate-high, and rising. Articles I-II have increasingly strong enforcement mechanisms (IAEA inspections, real verification consequences), so theater is not extreme. But Article VI has become purely ceremonial — RevCons invoke 'disarmament' language while no binding commitments are made, no timeline specified, no enforcement structure exists. Theater has risen from 0.42 to 0.68 as the gap between Article VI language and Article VI implementation has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The P5 collectively see a coordination mechanism (Rope) enabling mutual verification and supplier cartel. The swing states see mixed coordination-extraction (Tangled Rope) — they get security benefits but pay inspection costs. The non-aligned states see pure extraction (Snare) — inspection burden without benefits, denied deterrent path without reciprocal disarmament. The organized coalition (NAM) sees a temporary problem with a sunset (Scaffold) — Article VI is treated as binding promise waiting to be enforced, with sunset scheduled for when disarmament occurs. The IAEA sees a pure coordination role (Rope) — technical verification of others' compliance. Article VI sees itself as a degraded vestige (Piton) — invoked ceremonially but functionally abandoned. The analytical observer risks seeing an immutable power law (Mountain) — deterrent monopoly is a natural consequence of power distribution — but this is a false summit: the regime creates and maintains the monopoly; without it, threshold states could develop deterrents more freely. The perspectival gaps reveal that the regime is structured to produce these divergent experiences: it is not accidental that the P5 see coordination while the non-aligned see extraction. The regime's architecture produces that distribution of perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective flows from the structural position and exit options: P5 beneficiaries with arbitrage exit get low d (approximately 0.15-0.20), producing negative effective extractiveness (institutional -0.12 canonical); non-aligned NNWS victims with trapped exit get maximum d (approximately 1.0), producing maximum f(d) ≈ 1.42 (powerless canonical); threshold states in between get d around 0.85-0.95 depending on whether they retain exit option (constrained) or lose it through regime tightening. Swing states with security umbrella and constrained but not trapped exit get d around 0.55-0.65. The organizational coalition (NAM) with mobile exit gets d around 0.40-0.50. The IAEA as technical enforcer gets d around 0.30 (institutional/expertise position, no victim relationship). Scope modifier σ(S): global scope at 1.2 amplifies effective extraction relative to local/regional constraints. A constraint with identical ε but regional scope would show lower χ due to σ(regional)=0.9. This is diagnostically significant: the NPT's global reach amplifies its extraction effect. A regional non-proliferation arrangement (e.g., Latin American nuclear-free zone) with identical terms would be less extractive due to scope scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The oligopoly enforcement reading resolves mandatrophy by explicitly rejecting the claim that Articles I-II and Article VI are symmetric binding obligations. This reading accepts that the regime is tangled rope: it has real coordination functions (verification, supplier coordination, security guarantees) alongside asymmetric extraction (deterrent monopoly, inspection burden, strategic hierarchy). The mandatrophy resolution is achieved by naming the beneficiary structure (P5 + umbrella allies) and victim structure (threshold states + non-aligned NNWS) explicitly and deriving the classification from their asymmetric structural relationship. If the regime were to be reframed as a reciprocal disarmament bargain (the sibling reading), the mandatrophy would shift: Articles I-II would appear as temporary enforcement pending Article VI implementation, making the regime Scaffold rather than Tangled Rope. But this reading does not make that claim. Instead, it asserts that Articles I-II are the regime's core and Article VI is theater. This forecloses the reciprocal-bargain reading at the level of regime interpretation, though it coexists with the withdrawal-sovereignty reading (which treats the NPT as a constraint that rational actors would exit if they could).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status,
    'Is Article VI a genuine binding obligation or a performative covenant with no enforcement mechanism?',
    'Document review: identify enforcement trigger, sanction structure, timeline specification for disarmament; compare with Articles I-III enforcement apparatus (IAEA inspections, safeguards verification, concrete compliance metrics). Structural comparison with equivalent binding disarmament treaties (e.g., CFE in conventional arms).',
    'If binding: NPT is Tangled Rope with reciprocal obligation burden. If performative: NPT is Snare (Articles I-II binding asymmetry with Article VI theater). Classification pivots on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_status, conceptual, 'Whether Article VI is binding disarmament obligation or performative covenant').

omega_variable(
    threshold_state_exit_cost_asymmetry,
    'Why do some threshold states (India, Pakistan) exit the regime and others (Japan, South Korea) remain bound despite identical security incentives?',
    'Comparative analysis: security umbrella credibility (US security commitment depth, geographic exposure, alliance history), domestic nuclear constituency strength, non-aligned positioning, international isolation cost tolerance.',
    'If credibility of umbrella is the determinant: the regime operates as legitimate coordination (Rope from swing state perspective). If domestic politics dominates: the regime is constraint imposed against state preference (Snare from threshold perspective regardless of umbrella).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_exit_cost_asymmetry, empirical, 'Determinants of threshold state compliance vs exit decision').

omega_variable(
    nws_arsenal_stability_as_regime_side_effect,
    'Do Article I-II enforcement mechanisms (verification of others'' compliance) stabilize or destabilize NWS arsenals among themselves?',
    'Game-theoretic analysis: arms race dynamics in NWS dyads (US-USSR/Russia, China-India, etc.) pre/post-NPT. Does IAEA inspection of NNWS material flows affect NWS mutual suspicion or deterrent posture? Correlation between regime verification strength and NWS arms development trajectory.',
    'If inspections reduce NWS mutual suspicion: Article I-II serves coordination function for P5 → Rope for P5 perspective. If inspections are orthogonal to NWS mutual deterrent: Article I-II is pure extraction mechanism against NNWS, no P5 coordination benefit → Snare against all but P5.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_arsenal_stability_as_regime_side_effect, empirical, 'Whether Article I-II verification stabilizes or destabilizes P5 mutual deterrence').

omega_variable(
    regime_reading_kernel_ambiguity,
    'Is the NPT kernel codified as Articles I-II (enforcement asymmetry) or as the implied bargain (disarmament in exchange for non-proliferation)?',
    'Textual analysis: original negotiating history (1968 drafting), ratification debates, subsequent RevCons (1975-2022). What interpretation carries authority in treaty commissions, ICJ advisory opinions, state practice?',
    'If kernel is Articles I-II: this reading (oligopoly enforcement) forecloses the reciprocal disarmament reading. If kernel is the bargain: this reading coexists with (not forecloses) the disarmament reading — both are partial instantiations of a contested kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_reading_kernel_ambiguity, conceptual, 'Whether NPT kernel centers on enforcement asymmetry or disarmament bargain').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 0, 53).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_oligop_theater_1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(npt_oligop_theater_1985, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(npt_oligop_theater_2000, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 30, 0.68).
narrative_ontology:measurement(npt_oligop_theater_2023, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 53, 0.68).

% Extraction over time
narrative_ontology:measurement(npt_oligop_extract_1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(npt_oligop_extract_1985, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(npt_oligop_extract_2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(npt_oligop_extract_2023, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 53, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt_oligop_suppress_1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(npt_oligop_suppress_1985, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(npt_oligop_suppress_2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(npt_oligop_suppress_2023, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 53, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_authority).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, nsg_technology_cartel).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iran_jcpoa_breakpoint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT kernel. The sibling readings (reciprocal-disarmament, withdrawal-sovereignty) are separate constraint stories with different ε values and classification types. They are linked as a constraint family via network.affects_constraints, documenting the structural relationship: this reading forecloses the disarmament reading at the interpretation level, and coexists with the sovereignty reading. The ε-invariance principle applies: each reading gets its own ε reflecting the empirical status of its core claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
