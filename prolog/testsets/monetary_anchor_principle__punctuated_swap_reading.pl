% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Monetary Anchor Principle: Punctuated Swap Reading (Nixon Shock as Discrete Institutional Choice)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of a contested kernel: the
 *   monetary anchor principle and the meaning of the August 15, 1971
 *   transition from Bretton Woods to floating exchange rates. The
 *   PUNCTUATED_SWAP READING treats the transition as a discrete institutional
 *   choice—a unilateral defection from one monetary regime to another—rather
 *   than as inevitable structural collapse (Triffin dilemma reading) or as an
 *   overdetermined composite of convergent pressures (overdetermined
 *   reading). Under this reading, the constraint is a TANGLED ROPE: the gold
 *   standard functioned as a coordination mechanism (solving the problem of
 *   how to organize international trade under stable exchange rates) while
 *   simultaneously extracting from those locked into the dollar as reserve
 *   currency (foreign central banks exposed to devaluation). The
 *   institutional choice to abandon gold backing on August 15, 1971 was a
 *   unilateral defection from the coordination equilibrium—a choice made by
 *   the U.S. to restore fiscal autonomy, which redistributed wealth from
 *   foreign dollar holders to U.S. fiscal authority. The extraction mechanism
 *   is the asymmetric shock: those holding dollars absorbed sudden
 *   devaluation with no exit or negotiation. The coordination function is
 *   genuine: pre-1971, the gold standard did coordinate international trade
 *   and enabled predictable capital flows. After the shock, a new
 *   coordination emerges around floating rates. The constraint's
 *   extractiveness (0.48) reflects that the choice was institutional (not
 *   natural) and reversible in principle (could have been negotiated rather
 *   than imposed unilaterally), but the extraction is real and substantial.
 *
 * KEY AGENTS:
 *   - U.S. Fiscal Authority (Nixon Administration): Primary beneficiary (institutional/arbitrage) — gains fiscal autonomy to run deficits without gold constraint; defector from coordination
 *   - Foreign Dollar Holders (Central Banks, Foreign Governments): Primary victims (powerless/trapped at moment of shock) — absorb sudden devaluation with no exit or recourse; expropriated value
 *   - Allied Industrial Powers (Germany, Japan): Secondary beneficiary-victims (powerful/mobile) — constrained by fixed rates pre-shock but benefit from competitive devaluation post-shock; mixed extraction
 *   - Central Banks of Bretton Woods System: Institutional coordinators (institutional/constrained) — experience coordination failure but adapt to floating-rate coordination
 *   - Organized Finance Sector: Beneficiary (organized/mobile) — profit from transition chaos and new infrastructure requirements
 *   - Bretton Woods Institutions (IMF, World Bank): Degraded institutional actors (institutional/arbitrage) — lose primary coordination function; persist through inertia
 *   - Analytical Observer (Structural Inevitability View): Observer (analytical/analytical) — risks misclassifying a choice as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.48).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.52).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Monetary Anchor Principle: Punctuated Swap Reading (Nixon Shock as Discrete Institutional Choice)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '87a242a2-94f5-4269-8a42-5924f67f8eec').
narrative_ontology:cs_kernel_codification('87a242a2-94f5-4269-8a42-5924f67f8eec', fixed_text).
narrative_ontology:cs_authority_grounding('87a242a2-94f5-4269-8a42-5924f67f8eec', extraction).
narrative_ontology:cs_interpretation_layer_present('87a242a2-94f5-4269-8a42-5924f67f8eec').
narrative_ontology:cs_reading_relation('87a242a2-94f5-4269-8a42-5924f67f8eec', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('87a242a2-94f5-4269-8a42-5924f67f8eec', monetary_anchor_principle__triffin_inevitability_reading, forecloses).
narrative_ontology:cs_axiom('87a242a2-94f5-4269-8a42-5924f67f8eec', foundational, august_15_as_institutional_choice).
narrative_ontology:cs_axiom_status(august_15_as_institutional_choice, holdable).
narrative_ontology:cs_axiom_grounding('87a242a2-94f5-4269-8a42-5924f67f8eec', august_15_as_institutional_choice, empirically_contingent).
narrative_ontology:cs_axiom('87a242a2-94f5-4269-8a42-5924f67f8eec', foundational, gold_standard_regime_reversible_in_principle).
narrative_ontology:cs_axiom_status(gold_standard_regime_reversible_in_principle, holdable).
narrative_ontology:cs_axiom_grounding('87a242a2-94f5-4269-8a42-5924f67f8eec', gold_standard_regime_reversible_in_principle, instrumental).
narrative_ontology:cs_reference_frame('87a242a2-94f5-4269-8a42-5924f67f8eec', gold_standard_monetary_coordination).
narrative_ontology:cs_drift_state('87a242a2-94f5-4269-8a42-5924f67f8eec', post_august_15_1971_floating_rate_regime, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('87a242a2-94f5-4269-8a42-5924f67f8eec', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, united_states_fiscal_authority).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, fixed_exchange_rate_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOREIGN DOLLAR HOLDERS (SNARE) — Nations holding dollar reserves face sudden devaluation with no exit option available at time of shock. The constraint is an abrupt unilateral defection by the hegemon; victims are locked into an extractive regime that redistributes wealth upward to U.S. fiscal authority. No recourse, no pre-warning, no negotiation—classic snare signature.
constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: U.S. FISCAL AUTHORITY (ROPE) — Experiences the constraint as a coordination problem solved by unilateral choice: the gold standard required disciplined fiscal policy (the coordination problem). August 15, 1971 was the institutional choice to exit that coordination—a discrete decision with immediate effect. From this perspective, the constraint is coordination (how to manage reserve currency flows); the solution is defection from the old regime. Net beneficiary.
constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ALLIED INDUSTRIAL POWERS (TANGLED ROPE) — Germany and Japan were both constrained by Bretton Woods (mandatory fixed exchange rates suppressed export competitiveness) and coordinated with U.S. through the regime (predictable dollar flows, stable trade). Nixon's shock breaks the coordination (benefits them: competitive devaluation now possible) but also extracts via sudden loss of expected stability. They have exit-path mobility through floating rates, but the shock still redistributes wealth. Both coordination and extraction present.
constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANKS (ROPE) — Experienced the gold standard as a coordination mechanism enabling international trade and capital flow predictability. The shock is experienced as violation of coordination expectations, but the institutional response (moving to floating rates) is perceived as restoration of coordination at a new equilibrium. Constrained by institutional frameworks but benefit from the eventual stability of float regimes.
constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED FINANCE SECTOR (SCAFFOLD) — Benefits from the transition as a temporary coordination failure requiring new settlement mechanisms, new forex infrastructure, new swap lines. The chaos creates institutional arbitrage opportunities and infrastructure needs. The shock is experienced as a sunset on the old Bretton Woods clearinghouse system; new mechanisms (SWIFT, floating-rate hedging) are the building replacement. Theater_ratio is moderate—real infrastructure innovation is happening, not purely performative adjustment.
constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: BRETTON WOODS INSTITUTIONS (PITON) — The IMF and World Bank saw their roles as coordinators of a fixed-rate regime. After August 15, their primary coordination function atrophied—floating rates reduced their need as arbiters of par values. They persisted through institutional inertia, adapting their mandates to development finance and surveillance roles. Theater_ratio high for original function; low for actual verification of compliance.
constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY VIEW (MOUNTAIN) — From a long-term analytical frame, the gold standard collapse appears inevitable: no fixed-rate regime could survive the scale of U.S. deficits and technological capital mobility of the 1960s. The constraint (gold backing as anchor) is seen as physically impossible to maintain given macroeconomic conditions—a natural law of international monetary dynamics. The August 15 choice is then read as merely the moment when the accumulated pressures forced acknowledgment of what was already structurally determined. However, this reading naturalizes what this constraint story identifies as a CHOICE—a discrete institutional defection, reversible in principle.
constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_anchor_principle__punctuated_swap_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, TR),
    TR >= 0.70.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-to-high, reflecting institutional choice with real redistribution. The shock transferred wealth from foreign reserve holders to U.S. fiscal authority through sudden devaluation (~10% nominal, more in real terms adjusted for subsequent inflation). This is asymmetric extraction—victims had no negotiating power or exit option at the moment of shock. However, the extractiveness is not maximal (≤0.66 snare threshold) because the mechanism is reversible in principle: a different U.S. administration could have negotiated a cooperative transition or maintained gold backing with different fiscal discipline. The institutional choice nature means the extraction is agent-dependent, not a structural law. Suppression (0.52): Moderate-high. Pre-shock suppression is low (0.10)—participants accept gold backing as coordination mechanism. Post-shock suppression rises dramatically (0.52) as victims realize they cannot exit the devaluation or negotiate compensation. The shock itself is a suppression mechanism: it eliminates alternatives to accepting the new floating-rate regime. Suppression then plateaus as the new regime stabilizes. Theater ratio (0.35): Moderate. The shock involves significant genuine coordination disruption—actual settlement mechanisms must change, forex markets must emerge, new institutional structures must form. There is performative element (ritual justifications, economic rhetoric about 'discipline'), but the functional disruption is real. Theater rises modestly post-shock (0.35-0.38) as the new floating-rate regime becomes institutionalized and some ritual elements (central bank intervention rhetoric) emerge.
 *
 * PERSPECTIVAL GAP:
 *   This constraint instantiates maximum perspectival disagreement: beneficiary sees coordination + choice (rope), victim sees extraction + shock (snare), observer sees structural inevitability (mountain). The gap reveals that the classification depends entirely on whether you treat August 15 as institutional choice or structural determination. This is exactly the contest the KERNEL captures: three incompatible readings of the same institutional event.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. fiscal authority (beneficiary, institutional, arbitrage): Low d because they are net beneficiaries with high exit options (they are the defector). Engine derives d from beneficiary status + institutional power + arbitrage exit → d ≈ 0.05-0.20, producing negative χ from their perspective (they experience the constraint as removing barriers, not as extraction). Foreign dollar holders (victims, powerless, trapped): High d because they are net victims with no exit at shock moment. Engine derives d from victim status + powerless + trapped → d ≈ 0.90-0.95, producing maximum χ from their perspective (full experienced extraction). Allied powers (mixed victims-beneficiaries, powerful, mobile): Moderate d reflecting mixed extraction (suppressed pre-shock by fixed rates, benefit from devaluation post-shock). Engine derives d ≈ 0.48-0.55, producing moderate χ. The directionality spread across perspectives is large (0.05 to 0.95), indicating high asymmetry in the distribution of extraction—diagnostic feature of snare from victims' perspective, rope from beneficiary's perspective, tangled rope from mixed perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING MANIFESTATION: This constraint resolves the mandatrophy by instantiating ONE READING of a three-reading contest about the meaning of August 15, 1971. The mandatrophy question is not 'which type is correct?' but 'which reading of the kernel's meaning do you adopt?' If the transition was a CHOICE (this reading), then it is TANGLED_ROPE—coordination function + unilateral extraction. If the transition was STRUCTURALLY DETERMINED (Triffin reading), then it approaches MOUNTAIN—a natural law of international monetary dynamics under those conditions. If the transition was OVERDETERMINED (composite reading), then it is ROPE + SNARE blended, depending on which pressure you privilege. The three readings are not wrong versions of one story; they are three structurally distinct interpretations of what 'the August 15 transition' means. This story takes the institutional-choice reading and executes it faithfully with tangled_rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_choice_vs_structural_determination,
    'Was August 15, 1971 a discrete institutional choice by Nixon/Kissinger/Connally, or was it a forced handoff from inevitable structural pressures (Triffin dilemma, deficits, capital mobility)?',
    'Archival analysis of internal Nixon administration deliberations (tape recordings, declassified NSC memos); comparison of timing of policy decision vs. timing of triggering events (gold draws, speculative runs); counterfactual: would a different U.S. administration have maintained gold standard longer, or was collapse inevitable by 1971 regardless of administration?',
    'If institutional choice: constraint is TANGLED_ROPE (coordination failure + unilateral extraction). If structurally determined: constraint is MOUNTAIN (or rope coerced by structure). This reading instantiates the choice hypothesis; sibling readings instantiate determination. The engine will apply false summit detection if mountain misclassifies an agent-benefiting choice as natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_choice_vs_structural_determination, empirical, 'Whether August 15, 1971 was a discrete institutional choice or structurally inevitable').

omega_variable(
    counterfactual_regime_continuation,
    'Could Bretton Woods have persisted past 1971 if the U.S. had deployed alternative fiscal policies (austere budget discipline, capital controls, negotiated burden-sharing with allies)?',
    'Historical modeling: what policy combinations would have sustained gold backing at 1971 deficit levels? Comparison with other reserve currency regimes that maintained anchors under similar pressures (gold standard countries that did NOT abandon before 1971); analysis of why U.S. chose the shock over alternatives.',
    'If regime continuation possible: Nixon Shock is pure institutional choice (supports this reading as discrete swap). If continuation impossible: Shock is forced by structure (supports structural determination reading). The choice between ''defect unilaterally'' vs ''negotiate renegotiation'' becomes a secondary decision rather than the primary one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_regime_continuation, empirical, 'Whether Bretton Woods continuation was possible with alternative U.S. fiscal policies').

omega_variable(
    beneficiary_vs_victim_classification_ambiguity,
    'Were foreign dollar holders genuinely expropriated by the shock, or did they benefit from the transition to floating rates and subsequent dollar weakness (devaluation gave them competitive advantage)?',
    'Historical analysis of post-1971 real wealth flows to foreign reserve holders; comparison of net benefit (revaluation of remaining assets + competitive gains) vs. direct loss from reserve devaluation; timeline: immediate loss vs. 5-year cumulative outcome.',
    'If true expropriation: foreign governments are pure victims (supports snare from their perspective). If mixed/net-positive outcome: constraint is less extractive than nominal devaluation suggests—victims may have become moderate beneficiaries in medium term (supports tangled_rope instead). Classification stability depends on temporal horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_vs_victim_classification_ambiguity, empirical, 'Whether foreign dollar holders were net expropriated or net benefited by transition').

omega_variable(
    reading_kernel_contest_specification,
    'This constraint is one reading of the ''monetary_anchor_principle'' kernel. How do the three readings (punctuated_swap, overdetermined_composite, triffin_inevitability) relate structurally?',
    'Formal analysis of the reading relations: Does punctuated_swap FORECLOSE the inevitability readings (choice rules out determinism)? Do the readings COEXIST (different scholars hold different views)? Or do they INFLUENCE each other (choice timing influenced by structural pressure magnitude)?',
    'If readings foreclose: only one can be correct; the kernel has a determinate answer. If coexist: different analytic communities hold different readings; the constraint''s classification depends on which reading you instantiate. This omega documents that the constraint story is ONE READING of a THREE-READING contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_specification, conceptual, 'Structural relationship between the three readings of monetary anchor principle kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monanchor_swap_tr_t0, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(monanchor_swap_tr_t15, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(monanchor_swap_tr_t30, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(monanchor_swap_be_t0, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(monanchor_swap_be_t15, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(monanchor_swap_be_t30, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(monanchor_swap_su_t0, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(monanchor_swap_su_t15, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(monanchor_swap_su_t30, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel has three structurally distinct readings with different epsilon values and classification outcomes. This story instantiates the punctuated_swap_reading (institutional choice, ε≈0.48, tangled_rope). Sibling readings instantiate structural determination (higher ε, mountain or rope-snare blend) and Triffin inevitability (structural law, mountain-approximating). Each reading is a separate constraint story. The network links reveal their interdependence: all three are interpretations of the same historical event (August 15, 1971) but from incompatible analytical frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
