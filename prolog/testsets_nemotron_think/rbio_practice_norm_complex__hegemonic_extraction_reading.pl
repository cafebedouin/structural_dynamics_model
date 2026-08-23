% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Norms as Frozen Hegemonic Project (Hegemonic Extraction Reading)
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   The Rules-Based International Order (RBIO) norm complex — comprising the
 *   UN Charter system, Bretton Woods institutions, human rights treaties, and
 *   the post-1990 'liberal interventionist' repertoire — is formally
 *   revisable through multilateral amendment procedures. In practice, the P5
 *   veto in the Security Council, weighted voting in the IMF/World Bank, and
 *   path-dependent institutional inertia make substantive amendment nearly
 *   impossible. Enforcement is highly selective: norms against aggression,
 *   human rights violations, and non-proliferation are invoked against Global
 *   South states while P5 members and their allies face no comparable
 *   accountability. This reading interprets the selectivity not as a capacity
 *   failure but as evidence that the constraint's persistent function is the
 *   extraction of policy autonomy and economic surplus from the Global South
 *   for the benefit of U.S. and European capital.
 *
 * KEY AGENTS:
 *   - us_european_capital: Primary beneficiary (institutional/arbitrage) — captures economic surplus via conditionalities and market access rules
 *   - p5_states: Agenda setter (institutional/arbitrage) — controls amendment veto and enforcement triggers
 *   - global_south_states: Primary payer (organized/constrained) — bears conditionalities, interventions, and unequal treaty obligations
 *   - global_south_populations: Primary payer (powerless/trapped) — bears austerity, displacement, and violence from structural adjustment and interventions
 *   - international_bureaucracies: Agenda setter (institutional/biographical) — administers norms, manages conditionalities, legitimizes selective enforcement
 *   - global_south_movements: Excluded (powerless/trapped) — would contest extraction but are structurally excluded from norm-making
 *   - critical_scholars: Observer (analytical/analytical) — analyzes extraction but lacks institutional leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.82).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Norms as Frozen Hegemonic Project (Hegemonic Extraction Reading)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '36c76c39-b3a8-4c9b-8116-98faada60bd9').
narrative_ontology:cs_kernel_codification('36c76c39-b3a8-4c9b-8116-98faada60bd9', formalized).
narrative_ontology:cs_authority_grounding('36c76c39-b3a8-4c9b-8116-98faada60bd9', extraction).
narrative_ontology:cs_interpretation_layer_present('36c76c39-b3a8-4c9b-8116-98faada60bd9').
narrative_ontology:cs_reading_relation('36c76c39-b3a8-4c9b-8116-98faada60bd9', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('36c76c39-b3a8-4c9b-8116-98faada60bd9', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('36c76c39-b3a8-4c9b-8116-98faada60bd9', foundational, rbio_norms_serve_hegemonic_extraction).
narrative_ontology:cs_axiom_status(rbio_norms_serve_hegemonic_extraction, holdable).
narrative_ontology:cs_axiom_grounding('36c76c39-b3a8-4c9b-8116-98faada60bd9', rbio_norms_serve_hegemonic_extraction, empirically_contingent).
narrative_ontology:cs_axiom('36c76c39-b3a8-4c9b-8116-98faada60bd9', secondary, p5_veto_blocks_legitimate_amendment).
narrative_ontology:cs_axiom_status(p5_veto_blocks_legitimate_amendment, holdable).
narrative_ontology:cs_axiom_grounding('36c76c39-b3a8-4c9b-8116-98faada60bd9', p5_veto_blocks_legitimate_amendment, empirically_contingent).
narrative_ontology:cs_reference_frame('36c76c39-b3a8-4c9b-8116-98faada60bd9', post_war_liberal_order).
narrative_ontology:cs_drift_state('36c76c39-b3a8-4c9b-8116-98faada60bd9', contemporary_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36c76c39-b3a8-4c9b-8116-98faada60bd9', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_states).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, hegemonic_stability_theory_as_extraction_rationale).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Global financial and industrial capital headquartered in the U.S. and Europe. Benefits from RBIO norms that guarantee market access, enforce intellectual property, enable capital mobility, and discipline Global South policy space via IMF/World Bank conditionalities and investor-state dispute settlement. Does not directly administer the norms but captures the lion's share of material gains. Exit is trivial: capital can relocate, hedge, or diversify across jurisdictions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital, beneficiary,
    institutional, generational, arbitrage, global).

% The five permanent Security Council members (U.S., UK, France, Russia, China). They hold veto power over amendment and enforcement authorization. They use RBIO norms to legitimize their great-power prerogatives (intervention, sanctions, recognition) while shielding themselves and allies from accountability. They bear some costs of enforcement (military expenditures, diplomatic capital) but control the rule-making agenda. Exit is not meaningful: they are the architects of the system.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_states, beneficiary).

% UN Secretariat, IMF, World Bank, WTO, ICC, and regional development banks. They administer the norm complex: design conditionalities, monitor compliance, legitimize selective enforcement through technical reports and legal opinions. They derive institutional rents (budgets, staffing, mandate expansion) but are ultimately constrained by principal states (especially P5 and major shareholders). Exit would mean institutional dissolution or radical mandate change, which they resist.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, international_bureaucracies, agenda_setter,
    institutional, biographical, constrained, global).

% Postcolonial states in Africa, Latin America, Asia, and the Pacific. They are subject to IMF/World Bank structural adjustment, WTO dispute rulings, human rights conditionality, and selective military intervention. They participate in norm-making forums (UNGA, NAM, G77) but lack veto power and weighted voting share. Exit is constrained: leaving the system means losing market access, aid, and diplomatic recognition; staying means accepting asymmetric obligations. They occasionally form coalitions to resist but are fragmented by great-power patronage.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    organized, biographical, constrained, global).

% Working classes, peasants, indigenous peoples, and marginalized communities in the Global South. They bear the material costs of structural adjustment (austerity, privatization, deregulation), military intervention (displacement, death, infrastructure destruction), and unequal treaties (resource extraction, labor exploitation). They have no formal voice in RBIO institutions and face repression when they mobilize. Exit is nearly impossible: borders are closed, citizenship is non-negotiable, and transnational solidarity networks are weak.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations, payer,
    powerless, biographical, trapped, global).

% Social movements, trade unions, peasant organizations, feminist collectives, and climate justice networks in the Global South. They articulate alternative normative frameworks (food sovereignty, climate reparations, debt cancellation, decolonial international law) but are structurally excluded from RBIO decision-making. Their exclusion is not incidental: the norm complex's legitimacy depends on presenting itself as universal while silencing the very populations it extracts from.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_movements, excluded,
    powerless, biographical, trapped, global).

% Academics, journalists, and independent researchers who analyze the RBIO norm complex from critical perspectives (TWAIL, Marxist IR, decolonial theory, feminist security studies). They have no institutional leverage but produce the analytical evidence that exposes the extraction-coordination gap. Their 'exit' is intellectual: they can change frameworks, but their work is often marginalized in mainstream policy discourse.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, critical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: RBIO norms coordinate state behavior by providing a shared legal vocabulary, dispute resolution mechanisms, and standardized rules for trade, finance, human rights, and use of force. They reduce transaction costs in international relations and create focal points for cooperation.
% TRANSFER_FUNCTION: The arrangement moves policy autonomy and economic surplus from Global South states and populations to U.S. and European capital. Conditionalities transfer decision-making authority over domestic policy to creditors. Selective enforcement transfers security and resources to Northern states and corporations. Intellectual property and investment rules transfer rents from Global South producers to Northern IP holders and investors.
% ABSENT_VOICES: Global South populations, social movements, and alternative institutional visions (e.g., New International Economic Order, Bandung principles, Buen Vivir) are excluded from the drafting, interpretation, and amendment of RBIO norms. They would object to the extraction but are kept out by the same sovereignty and voting structures that the norms enshrine.
% DISAPPEARANCE_RATIONALE: If the RBIO norm complex vanished overnight, the institutional architecture governing trade, finance, intervention, and human rights would collapse. A scramble would ensue: new regional orders (BRICS+, African Continental Free Trade Area, ALBA) would compete to fill the void; great powers would revert to explicit spheres of influence; Global South states would attempt to build alternative frameworks. The world would rearrange violently and unpredictably.
% FOUNDING_PROBLEM: Post-WWII need for a stable international order to prevent great power war and manage decolonization without chaotic collapse of empires.
% FOUNDING_PROBLEM_CORROBORATION: Critical IR scholars (e.g., Antony Anghie, Sundhya Pahuja, Martti Koskenniemi) and historians of decolonization (e.g., Adom Getachew, Gary Wilder) attest that the founding problem of great power war is now managed by nuclear deterrence, not RBIO norms, and that decolonization's promise of sovereign equality was subverted by the very institutions created to manage it. The beneficiary states (U.S., EU) and international bureaucracies claim the problem is still live, citing 'rules-based order' rhetoric, but their own policy practice (selective enforcement, veto use) contradicts this claim.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the norm complex channels material benefits (market access, capital flows, debt terms) to Northern capital while imposing costs (structural adjustment, intervention, unequal treaties) on the Global South. Suppression (0.82) is higher still because the constraint's persistence depends on active enforcement: P5 veto blocks amendment, IMF conditionality enforces policy conformity, and military intervention enforces compliance — all backed by credible coercion. Theater ratio (0.42) reflects that the norm complex performs genuine coordination (dispute settlement, standard-setting) but a growing share of activity is performative maintenance of legitimacy (rhetoric about 'rules-based order') while extraction proceeds. Accessibility collapse (0.79) is high because alternative institutional imaginaries (New International Economic Order, Bandung principles) have been marginalized or co-opted. Resistance (0.48) is moderate: there is sustained contestation (G77, Non-Aligned Movement, contemporary Global South coalitions) but it has not structurally altered the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the P5/agenda-setter seat, the constraint appears as a rope (genuine coordination with manageable friction). From the Global South state seat, it appears as a tangled_rope (coordination exists but extraction is asymmetric and enforced). From the Global South population seat, it appears as a snare (coordination is a cover; extraction is the function). The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analytical observer's assessment that both coordination and extraction are real and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S./European capital (beneficiary) sits at d ≈ 0.15: they capture gains, have arbitrage-grade exit (capital mobility). P5 states (agenda_setter) sit at d ≈ 0.25: they bear some costs of enforcement but control the rules and extract geopolitical rents. International bureaucracies (agenda_setter) sit at d ≈ 0.35: they administer the system and derive institutional rents but are constrained by principal states. Global South states (payer) sit at d ≈ 0.85: they bear conditionalities and interventions, exit is constrained (sovereignty costs, retaliation risk). Global South populations (payer) sit at d ≈ 0.95: they bear the heaviest costs (austerity, violence), exit is nearly impossible (trapped). Excluded movements (excluded) have no seat in the mechanism; their directionality is not computed but their absence is structural evidence of suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great power war, managing decolonization) is dead: great power war has been prevented by nuclear deterrence, not RBIO norms; decolonization is formally complete. Yet the constraint persists and has expanded (humanitarian intervention, R2P, counter-terrorism, WTO dispute settlement). The mandate has atrophied into extraction: the coordination function (war prevention) is no longer the primary driver; the extraction function (capital access, policy autonomy transfer) is. This is not a piton because the constraint is actively maintained and expanded, not inert. It is a tangled_rope whose coordination cover has thinned while extraction has intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_irreducible_ambiguity,
    'Is the RBIO norm complex a genuine coordination framework that has been captured, or was it designed from inception as a hegemonic extraction mechanism?',
    'Historical institutional analysis of founding negotiations (San Francisco 1945, Bretton Woods 1944) combined with counterfactual simulation of alternative institutional designs.',
    'If designed for extraction, the constraint is a snare from origin; if captured, it is a tangled_rope that degraded from a rope. Classification of the kernel''s original intent changes the genealogical status of all three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_irreducible_ambiguity, conceptual, 'Origin intent vs. capture trajectory of the RBIO kernel.').

omega_variable(
    enforcement_selectivity_mechanism,
    'Is the observed enforcement selectivity (interventions against Global South, impunity for P5 allies) structural (built into institutional rules) or political (discretionary choices by powerful states)?',
    'Comparative case study of intervention authorizations vs. non-authorizations since 1990, coding for legal basis, P5 voting patterns, and material interests.',
    'If structural, the constraint''s suppression is inherent and harder to reform; if political, it is contingent on current power configuration and could shift with multipolarity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Structural vs. political character of enforcement selectivity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(rbio_tr_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(rbio_tr_t45, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(rbio_tr_t60, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(rbio_tr_t79, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 79, 0.42).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rbio_be_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(rbio_be_t45, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 45, 0.71).
narrative_ontology:measurement(rbio_be_t60, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(rbio_be_t79, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 79, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(rbio_su_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(rbio_su_t45, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(rbio_su_t60, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(rbio_su_t79, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 79, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% The RBIO norm complex decomposes into three constraint stories linked by network.affects_constraints. The hegemonic_extraction_reading (this story) has high extractiveness (0.78) and suppression (0.82) because it reads enforcement selectivity as structural extraction. The liberal_institutional_reading would show low extractiveness and low suppression, reading the same institutions as genuine coordination. The sovereignty_maximalist_reading would show moderate extractiveness but high suppression from the perspective of states targeted by humanitarian intervention. The three readings share the same kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, institutional, 0.25).
constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, organized, 0.85).
constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
