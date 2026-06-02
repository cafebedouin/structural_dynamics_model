% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Sovereignty Primacy Reading
 *   domain: international_trade_law/regulatory_federalism/political_economy
 *
 * SUMMARY:
 *   NAFTA's jurisdictional architecture embeds a contested commitment: the
 *   treaty text explicitly preserves states' regulatory authority over labor,
 *   environmental, and health standards within their territories, while
 *   simultaneously creating mechanisms (Chapter 11 investor-state dispute
 *   resolution, dispute panels, trade remedies) that constrain how that
 *   authority can be exercised. This constraint describes ONE READING of that
 *   kernel: the sovereignty_primacy reading, which holds that treaty
 *   obligations are subordinate to domestic law, that regulatory agencies
 *   retain full jurisdictional authority, and that extraction is limited to
 *   voluntary compliance costs and procedural burdens. This reading is
 *   structurally distinct from the capital_supremacy reading (which treats
 *   the treaty's dispute mechanisms as de facto overrides to domestic law)
 *   and the embedded_liberalism reading (which treats the treaty as a
 *   negotiated compromise that acknowledges both capital mobility and
 *   regulatory legitimacy). Under the sovereignty_primacy reading, Mexico,
 *   the United States, and Canada entered into a coordination mechanism for
 *   trade predictability while retaining the power to regulate labor and
 *   environmental standards as they see fit — the treaty does not compel any
 *   particular standard, only transparent procedures for setting them.
 *   However, the empirical trajectory (rising theater_ratio and extraction
 *   over the interval) suggests that the nominal preservation of sovereignty
 *   has experienced pressure from the dispute mechanism's expansion and
 *   investor-state litigation's strategic deployment.
 *
 * KEY AGENTS:
 *   - Domestic Regulatory Agencies (institutional/arbitrage): Nominal authority holders under this reading; retain jurisdiction and benefit from treaty's coordination function
 *   - Signatory Governments (institutional/constrained): States as treaty parties; constrain own regulatory flexibility through treaty commitment while benefiting from market access
 *   - Transnational Production Networks (powerless/trapped): Capital's organizational form across borders; face suppression from domestic regulations they cannot influence or exit
 *   - Labor and Environmental Constituencies (organized/constrained): Domestic political groups benefiting from state capacity to enforce standards but bearing procedural costs of treaty compliance
 *   - Investor-State Dispute Panels (institutional/arbitrage): Treaty enforcement apparatus; perform coordination function nominally but operate as a second-order regulatory review in practice (piton perspective)
 *   - Analytical Observer (analytical/analytical): Civilizational view; risks treating sovereignty preservation as natural law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.35).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.42).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/regulatory_federalism/political_economy").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'a42e180e-50e7-41ce-9436-576ff550b8e0').
narrative_ontology:cs_kernel_codification('a42e180e-50e7-41ce-9436-576ff550b8e0', fixed_text).
narrative_ontology:cs_authority_grounding('a42e180e-50e7-41ce-9436-576ff550b8e0', lineage).
narrative_ontology:cs_interpretation_layer_present('a42e180e-50e7-41ce-9436-576ff550b8e0').
narrative_ontology:cs_reading_relation('a42e180e-50e7-41ce-9436-576ff550b8e0', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a42e180e-50e7-41ce-9436-576ff550b8e0', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_axiom('a42e180e-50e7-41ce-9436-576ff550b8e0', foundational, regulatory_authority_categorically_distinct_from_treaty_obligation).
narrative_ontology:cs_axiom_status(regulatory_authority_categorically_distinct_from_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a42e180e-50e7-41ce-9436-576ff550b8e0', regulatory_authority_categorically_distinct_from_treaty_obligation, deontological).
narrative_ontology:cs_axiom('a42e180e-50e7-41ce-9436-576ff550b8e0', foundational, treaty_obligations_enter_cost_structure_not_normative_hierarchy).
narrative_ontology:cs_axiom_status(treaty_obligations_enter_cost_structure_not_normative_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('a42e180e-50e7-41ce-9436-576ff550b8e0', treaty_obligations_enter_cost_structure_not_normative_hierarchy, deontological).
narrative_ontology:cs_reference_frame('a42e180e-50e7-41ce-9436-576ff550b8e0', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('a42e180e-50e7-41ce-9436-576ff550b8e0', contemporary_investor_state_litigation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a42e180e-50e7-41ce-9436-576ff550b8e0', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_environmental_health_constituencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, multinational_capital).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, transnational_production_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSNATIONAL PRODUCTION NETWORK (SNARE) — Trapped by national borders and domestic regulatory variation. Cannot exit jurisdiction without restructuring supply chains. Faces full extraction: compliance costs imposed by labor/environmental/health standards that treaty does not constrain. Zero degrees of freedom; suppression is material (exit costs exceed any benefit from relocation).
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__sovereignty_primacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC REGULATORY AGENCIES (ROPE) — Retain full authority. Experience the constraint as pure coordination: treaty establishes predictable rules, enabling agencies to coordinate across borders without surrendering jurisdiction. No extraction — agencies see the mechanism as a coordination device that preserves their power. Arbitrage available: can adjust standards at will, bearing only compliance-negotiation costs from trading partners.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LABOR/ENVIRONMENTAL CONSTITUENCIES (TANGLED ROPE) — Organized groups benefit from the treaty's preservation of domestic regulatory authority (coordination function: states can enforce standards). But also experience extraction: the treaty creates a procedural burden (dispute panels, transparency requirements, compliance documentation) that raises the cost of standard-setting. These are real coordination gains (predictable market access) alongside real asymmetric extraction (regulatory costs disproportionately borne by non-capital constituencies).
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__sovereignty_primacy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: SIGNATORY GOVERNMENT (MEXICO) (TANGLED ROPE) — Mexico retains jurisdiction but faces constrained exit: the treaty locks in a particular regulatory posture. Coordination benefit: predictable market access and capital investment incentives for maintaining standards. But also extraction: regulatory flexibility is surrendered; adjusting labor or environmental standards requires treaty renegotiation (high cost). Constrained rather than trapped because exit is possible (treaty withdrawal) but carries severe economic penalties.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__sovereignty_primacy_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: NAFTA DISPUTE PANEL APPARATUS (PITON) — The institutional machinery for treaty enforcement is substantially performative. Chapter 11 dispute panels review regulations nominally for 'fair and equitable treatment' but function as a second-order review of domestic regulatory decisions — without formal reversal power, yet with sufficient reputational/coercive pressure to operate as a shadow veto. Theater_ratio is high because the panels claim technical neutrality but operate as political negotiation forums. The original coordination function (transparent dispute resolution) has degraded into theatrical legitimation of capital pressure on regulatory agencies.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__sovereignty_primacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, state sovereignty over labor/environmental/health standards might appear to be an immutable principle of international law: states are defined by their ability to regulate their territory. From this view, NAFTA's preservation of regulatory authority is a natural-law constraint on what treaties can legitimately contain. However, this reading is a false summit — the structural data shows beneficiaries (domestic regulatory agencies) and victims (transnational capital), indicating the 'sovereignty principle' is a contingent institutional arrangement, not a law of nature. The engine will detect this via FSM.
constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__sovereignty_primacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__sovereignty_primacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nafta_jurisdictional_boundary__sovereignty_primacy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, TR),
    TR >= 0.70.

:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Under the sovereignty_primacy reading, extraction flows through compliance costs borne by transnational capital that must adapt to multiple regulatory regimes, plus procedural costs borne by domestic constituencies that must justify their standards to dispute panels. The extraction is not severe (ε ≤ 0.46) because the treaty does not formally subordinate domestic law to treaty obligations; states retain full authority to set standards. However, the de facto pressure from investor-state disputes and the threat of retaliation create suppression that limits how freely that authority is exercised. Theater ratio (0.58): Moderate-high, rising over the interval. The dispute panel apparatus was designed as a neutral technical review mechanism, but operates as a politically-loaded forum for negotiating regulatory boundaries. States must perform compliance justification work (documenting that regulations serve legitimate public purposes, not protectionist intent) even when the treaty nominally preserves their authority to regulate. Suppression (0.42): Moderate. Transnational capital faces material barriers to exit (cannot relocate production networks without massive cost) and faces extraction via labor/environmental regulations. Signatory governments face constrained exit (cannot withdraw from treaty without economic penalties) and face suppression via anticipated investor disputes that raise the political cost of standard-setting. Neither agent is fully trapped, but both face high friction.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates a substantial gap between nominal (text-based) and experienced (behavioral) classification. The treaty's text preserves regulatory authority → rope from agencies' perspective. But the dispute apparatus operates as a second-order regulator → snare from capital's perspective and piton from panels' perspective. Labor constituencies see mixed extraction and coordination (tangled_rope). The sovereign state's civilizational self-understanding treats regulatory authority as a natural law property of statehood → mountain from analytical context. But the presence of identifiable beneficiaries (domestic agencies, labor constituencies) and victims (transnational capital) triggers FSM evaluation, revealing the sovereignty principle as a contingent institutional arrangement, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   No overrides needed. Directionality is derived cleanly from structural position: beneficiaries (domestic agencies) get low d; victims (transnational capital) get high d; organized constituencies get intermediate d.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by maintaining a clear distinction between nominal jurisdictional authority (preserved by treaty text) and effective regulatory power (constrained by dispute mechanisms). The tangled_rope classification captures this duality: genuine coordination function (predictable rules for trade) alongside asymmetric extraction (regulatory flexibility constrained by investor-state pressure). The piton classification (dispute apparatus) separately identifies the performative dimension of treaty enforcement. No claim that the treaty is purely extraction (that would be capital_supremacy) nor purely coordination (that would be embedded_liberalism); instead, this reading holds both and separates them structurally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispute_panel_authority_scope,
    'Do NAFTA Chapter 11 dispute panels possess implied authority to overturn domestic regulations, or only to award damages?',
    'Analysis of arbitration awards and settlement outcomes; identification of cases where panels'' findings correlate with actual regulatory rollback or modification vs. merely financial liability',
    'If panels possess de facto veto power: extraction is substantially higher (treated as snare from governments'' perspective). If panels award only damages: extraction is lower (treated as tangled_rope). The jurisdiction interpretation determines the effective suppression value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_panel_authority_scope, conceptual, 'Whether dispute panels operate as de facto regulatory overseers or merely damage adjudicators').

omega_variable(
    regulatory_flexibility_preservation_empirical,
    'In practice, do signatory states retain substantive regulatory flexibility to adjust labor/environmental/health standards, or do anticipated investor-state disputes create a de facto regulatory freeze?',
    'Historical analysis of labor and environmental standards changes in signatory countries post-NAFTA; correlation between regulatory proposals and investor-state dispute initiation; measurement of regulatory velocity (rate of standard changes) pre- and post-treaty',
    'If flexibility is substantively preserved: sovereignty_primacy reading is accurate (extraction limited to procedural costs). If regulatory freeze occurs without formal penalty: the treaty functions as a snare despite its nominal language preserving authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_flexibility_preservation_empirical, empirical, 'Actual preservation of regulatory flexibility post-NAFTA').

omega_variable(
    kernel_reading_contestation_location,
    'Is the jurisdictional boundary between treaty obligation and domestic authority contestable within the same commitment framework, or do different readings foreclose each other?',
    'Examination of how different parties to NAFTA disputes articulate the treaty''s jurisdictional scope; identification of whether disagreement is empirical (about what the text says) vs. foundational (about what authority sources are legitimate)',
    'If purely empirical disagreement: readings coexist_with each other within a shared framework. If foundational disagreement over authority sources: readings foreclose each other. This determines whether the kernel admits genuine pluralism or embeds a zero-sum competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_location, conceptual, 'Whether jurisdictional readings coexist or logically foreclose each other').

omega_variable(
    this_reading_vs_capital_supremacy_empirical_anchor,
    'What empirical observations would distinguish the sovereignty_primacy reading from the capital_supremacy reading? At what threshold does one reading''s predictions fail?',
    'Define operational signatures for each reading: sovereignty_primacy predicts regulatory agencies retain flexibility and pursue labor/environmental standards despite treaty pressure; capital_supremacy predicts investor-state disputes suppress standard-setting before it reaches formal proposal. Measure actual behavior against both predictions.',
    'If signature evidence supports sovereignty_primacy prediction: the reading is empirically grounded and distinguishable from sibling readings. If evidence supports capital_supremacy prediction: the sovereignty_primacy reading is revealed as aspirational rather than descriptive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_vs_capital_supremacy_empirical_anchor, empirical, 'Empirical signature distinguishing sovereignty_primacy from capital_supremacy reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_sov_theater_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(nafta_sov_theater_t7, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 7, 0.5).
narrative_ontology:measurement(nafta_sov_theater_t14, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(nafta_sov_extract_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(nafta_sov_extract_t7, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 7, 0.28).
narrative_ontology:measurement(nafta_sov_extract_t14, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 14, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(nafta_sov_suppress_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(nafta_sov_suppress_t7, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 7, 0.38).
narrative_ontology:measurement(nafta_sov_suppress_t14, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 14, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, investor_state_dispute_panel_apparatus).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, regulatory_arbitrage_transnational_capital).

% DUAL FORMULATION NOTE:
% The NAFTA jurisdictional boundary kernel decomposes into three constraint stories: sovereignty_primacy_reading (this story, ε=0.35), capital_supremacy_reading (ε=0.68), and embedded_liberalism_reading (ε=0.42). Each reading instantiates a different structural claim about the treaty's jurisdiction hierarchy. The epsilon values differ because they measure different observables: sovereignty_primacy measures compliance costs within domestic jurisdiction; capital_supremacy measures investor-state dispute pressure as a regulatory override; embedded_liberalism measures negotiated allocation of authority across parties. All three are valid ε-invariant constraints on the same kernel. They are linked via reading_relations in cs_structure (coexists_with and influences edges), not via network.affects_constraints. The affects_constraints edges link to downstream constraints (the dispute apparatus itself, regulatory arbitrage dynamics) that depend on the kernel interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
