% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO DSB Advisory Coordination Function
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body operates as an advisory coordination
 *   mechanism under this reading. Panels issue expert legal opinions on
 *   treaty interpretation that facilitate negotiated settlements between
 *   member states. States retain ultimate policy discretion — compliance is
 *   voluntary, achieved through diplomatic bargaining and bilateral power
 *   dynamics rather than institutional compulsion. The arrangement
 *   coordinates by providing a shared epistemic baseline (the panel's legal
 *   analysis) that reduces information asymmetry in trade disputes.
 *   Extraction is low because no party is compelled to concede; the
 *   constraint persists because the coordination benefit (predictable dispute
 *   resolution, reduced retaliation spirals) exceeds the cost of
 *   participation for most members. The binding_referee_reading and
 *   judicial_activism_reading describe different structural claims about the
 *   same institutional machinery — they are sibling constraints in a family,
 *   not perspectival variants.
 *
 * KEY AGENTS:
 *   - member_states: Primary participants (institutional/biographical/arbitrage) — use DSB opinions as negotiation inputs; sovereignty preserved
 *   - developing_economies: Beneficiaries of epistemic leveling (organized/biographical/constrained) — gain access to legal reasoning that offsets power asymmetry in bilateral talks
 *   - trade_negotiators: Operational users (organized/biographical/mobile) — deploy panel findings as bargaining chips; exit via alternative forums or unilateral action
 *   - major_economies: Power holders in enforcement (institutional/biographical/arbitrage) — compliance leverage derives from market size, not DSB authority
 *   - analytical_observer: Observes full structure (analytical/civilizational/analytical) — sees coordination function and its power-contingent limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.12).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.18).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO DSB Advisory Coordination Function").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international_law/trade_governance/institutional_legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, '10be1742-7df9-489f-ba87-6a1125923a11').
narrative_ontology:cs_kernel_codification('10be1742-7df9-489f-ba87-6a1125923a11', formalized).
narrative_ontology:cs_authority_grounding('10be1742-7df9-489f-ba87-6a1125923a11', lineage).
narrative_ontology:cs_interpretation_layer_present('10be1742-7df9-489f-ba87-6a1125923a11').
narrative_ontology:cs_reading_relation('10be1742-7df9-489f-ba87-6a1125923a11', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('10be1742-7df9-489f-ba87-6a1125923a11', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('10be1742-7df9-489f-ba87-6a1125923a11', foundational, sovereign_discretion_preserved_in_compliance).
narrative_ontology:cs_axiom_status(sovereign_discretion_preserved_in_compliance, holdable).
narrative_ontology:cs_axiom_grounding('10be1742-7df9-489f-ba87-6a1125923a11', sovereign_discretion_preserved_in_compliance, conventional).
narrative_ontology:cs_axiom('10be1742-7df9-489f-ba87-6a1125923a11', foundational, panel_opinions_are_negotiation_inputs_not_judgments).
narrative_ontology:cs_axiom_status(panel_opinions_are_negotiation_inputs_not_judgments, holdable).
narrative_ontology:cs_axiom_grounding('10be1742-7df9-489f-ba87-6a1125923a11', panel_opinions_are_negotiation_inputs_not_judgments, conventional).
narrative_ontology:cs_reference_frame('10be1742-7df9-489f-ba87-6a1125923a11', dsb_advisory_coordination_framework).
narrative_ontology:cs_drift_state('10be1742-7df9-489f-ba87-6a1125923a11', post_appellate_body_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('10be1742-7df9-489f-ba87-6a1125923a11', '2026-08-04T14:30:00Z').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, developing_economies).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, trade_negotiators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, trade_negotiators).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, sovereign_policy_discretion_preserved).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, consensus_based_compliance).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, expert_advisory_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use DSB panel opinions as expert legal inputs to bilateral negotiations. Retain full policy discretion — compliance is a political choice, not a legal compulsion. Can exit to bilateral deals, regional agreements, or unilateral measures at any time. The coordination benefit is a shared epistemic baseline that reduces information asymmetry and retaliation risk.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, member_states, beneficiary,
    institutional, biographical, arbitrage, global).

% Gain disproportionate benefit from the advisory function: panel opinions provide legal reasoning that offsets power asymmetry when negotiating with major economies. Without the DSB's epistemic baseline, they would face raw power bargaining. Exit is constrained — alternative forums are fewer and less authoritative — but the DSB's advisory character means they are not bound by adverse rulings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, developing_economies, beneficiary,
    organized, biographical, constrained, global).

% Operational users who deploy panel findings as bargaining chips in negotiations. They bear the cost of preparing and litigating cases (legal teams, time, political capital) but gain the coordination benefit of an authoritative legal analysis. Exit is mobile — they can shift disputes to other forums or bilateral tracks — which keeps their directionality near symmetric.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, trade_negotiators, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, trade_negotiators, payer).

% Set the de facto enforcement terms through market power. When a panel opinion favors them, they leverage market access to extract compliance; when it disfavors them, they can delay, negotiate concessions, or ignore it (as the US did pre-2019 and post-2019). They benefit from the system's predictability for their own exporters but pay no extraction cost — their power substitutes for institutional enforcement.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, major_economies, agenda_setter,
    institutional, biographical, arbitrage, global).

% Observes the full structure: a genuine coordination mechanism (epistemic baseline for trade disputes) that functions because states find it useful, not because they are compelled. Sees the power-contingent enforcement gap that the binding_referee_reading denies and the judicial_activism_reading condemns. Neither collects nor pays.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__advisory_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__advisory_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, authoritative epistemic baseline (expert legal analysis of treaty obligations) that reduces information asymmetry in trade disputes, enabling negotiated settlements without requiring states to surrender policy discretion to a supranational adjudicator.
% TRANSFER_FUNCTION: Moves epistemic authority and negotiation leverage from the DSB's expert panels to the disputing parties. No mandatory resource transfer occurs; the 'transfer' is the informational good (the panel's reasoning) that both parties use to structure their bargain. Major economies gain additional leverage from their enforcement capacity, but this is a power effect, not a constraint-mediated transfer.
% ABSENT_VOICES: Future member states (not yet acceded) who would inherit the advisory system without having consented to its design. Domestic constituencies (labor, environmental, consumer groups) in member states who bear trade policy consequences but have no standing in DSB proceedings. These voices are excluded by the intergovernmental design of the WTO.
% DISAPPEARANCE_RATIONALE: If the DSB's advisory function vanished overnight, states would lose the shared epistemic baseline for dispute resolution. Trade disputes would revert to pure power bargaining or fragment into bilateral/regional forums with inconsistent legal standards. Retaliation spirals would increase. The coordination function is real and its absence would rearrange the trade governance landscape.
% FOUNDING_PROBLEM: 1995: Create a rules-based dispute resolution mechanism that prevents trade wars by providing authoritative legal interpretations, without establishing a supranational court that could override domestic policy sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The WTO Secretariat and most member states attest the problem is live — trade disputes persist and the advisory function continues to deliver settlement-facilitating epistemic goods. The US (major economy) has contested whether the *Appellate Body* (a judicial layer added atop the advisory panels) serves the founding problem, but has not contested the panel advisory function itself. Independent legal scholarship (e.g., Hudec, Mavroidis) corroborates that the panel advisory function remains the system's core coordination mechanism.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__advisory_coordination_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).
:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint does not compel transfers — it provides an epistemic good (expert legal analysis) that parties use voluntarily. Suppression is low (0.18) because alternatives (bilateral negotiation, regional forums, unilateral measures) remain fully accessible; the DSB does not foreclose exits. Theater ratio is modest (0.22) — some procedural ritual exists (panel composition, appellate review pre-2019) but the core function is genuine coordination. Accessibility collapse is low (0.25) — states routinely resolve disputes outside the DSB. Resistance is moderate (0.35) — the US blockage of Appellate Body appointments (2019–) reflects resistance to judicialization, not to the advisory coordination function itself. The claimed type is rope: genuine coordination with minimal coercion, net beneficiaries across seats.
 *
 * PERSPECTIVAL GAP:
 *   The advisory_coordination_reading computes as rope from the member-state and developing-economy seats (genuine coordination, low extraction). From the major-economy seat it may compute as mountain-like (the constraint barely binds them). The binding_referee_reading would compute as tangled_rope or snare from developing-economy seats (binding rulings + power-based enforcement = asymmetric extraction). The judicial_activism_reading computes as snare from the sovereignty-prioritizing seat. The engine computes these per-seat divergences from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states are structural beneficiaries (d ~ 0.15–0.25): they gain a shared epistemic baseline for disputes without surrendering policy discretion. Developing economies benefit disproportionately (d ~ 0.1) because the advisory opinions offset power asymmetry in bilateral negotiations. Major economies sit closer to symmetric (d ~ 0.4–0.5): they provide the enforcement capacity the system lacks and extract concessions through power, not DSB authority. Trade negotiators are operational users with mobile exit (d ~ 0.3). The directionality derivation from beneficiary declarations + exit options captures this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1995): provide a rules-based dispute resolution mechanism that prevents trade wars without supranational adjudication. Status: live — trade disputes persist, the coordination need remains, and the advisory function continues to deliver epistemic value. No mandatrophy: the constraint's function has not atrophied; the Appellate Body crisis reflects resistance to a *different* reading (binding_referee) gaining traction, not failure of the advisory coordination function. The constraint remains a rope, not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    advisory_vs_binding_ambiguity,
    'Is the DSB''s advisory character a structural feature of the WTO treaty design, or a pragmatic accommodation that could shift toward binding enforcement without treaty amendment?',
    'Trace the drafting history of DSU Articles 3.2, 19.1, and 21.3; measure the frequency and consequence of non-compliance with panel/advisory body recommendations across the interval; assess whether the Appellate Body''s judicialization (pre-2019) represented drift or design.',
    'If structural, the low-extraction advisory reading is the only faithful one; if accommodation, the binding_referee_reading describes a real available mode that states have chosen not to activate — changing the constraint''s ε profile and type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_vs_binding_ambiguity, conceptual, 'Whether the advisory character is constitutional or contingent').

omega_variable(
    enforcement_asymmetry,
    'Does the reliance on bilateral power dynamics for enforcement create de facto extraction from weaker members, despite the advisory formal structure?',
    'Compare compliance outcomes for disputes involving symmetrical vs. asymmetrical power dyads; measure the concession extraction rate when the complainant is a major economy vs. a developing economy.',
    'If enforcement asymmetry systematically disadvantages weaker members, the advisory coordination mask conceals a power-based extraction function — the constraint would compute as tangled_rope or snare from the weaker-member seat despite the author''s rope claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether bilateral enforcement substitutes power for law in a way that extracts from the weak').

omega_variable(
    kernel_reading_identity,
    'Is the advisory_coordination_reading a distinct constraint from the binding_referee_reading and judicial_activism_reading, or are they observer-relative framings of one institutional practice?',
    'Apply the ε-invariance test: do the three readings author different ε values for the same standing arrangement? If yes, they are distinct constraints linked by network.affects_constraints. If no, they are perspectival variants of one constraint.',
    'Determines whether this JSON describes one constraint in a family of three, or whether the kernel frame is misapplied. The engine requires ε-invariance per constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel vs. reading boundary validity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_advisory_tr_t1995, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2001, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2008, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2013, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2013, 0.21).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2019, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2019, 0.22).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2024, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(wto_dsb_advisory_be_t1995, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 1995, 0.08).
narrative_ontology:measurement(wto_dsb_advisory_be_t2001, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2001, 0.09).
narrative_ontology:measurement(wto_dsb_advisory_be_t2008, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2008, 0.1).
narrative_ontology:measurement(wto_dsb_advisory_be_t2013, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2013, 0.11).
narrative_ontology:measurement(wto_dsb_advisory_be_t2019, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2019, 0.12).
narrative_ontology:measurement(wto_dsb_advisory_be_t2024, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_advisory_su_t1995, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 1995, 0.12).
narrative_ontology:measurement(wto_dsb_advisory_su_t2001, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2001, 0.15).
narrative_ontology:measurement(wto_dsb_advisory_su_t2008, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2008, 0.17).
narrative_ontology:measurement(wto_dsb_advisory_su_t2013, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2013, 0.18).
narrative_ontology:measurement(wto_dsb_advisory_su_t2019, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2019, 0.18).
narrative_ontology:measurement(wto_dsb_advisory_su_t2024, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__advisory_coordination_reading, 0.02).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% The wto_dsb_authority kernel decomposes into three constraint stories with distinct ε values and structural profiles. The advisory_coordination_reading (this story) claims rope with low extraction; the binding_referee_reading claims tangled_rope with power-asymmetric extraction; the judicial_activism_reading claims snare with illegitimate extraction. They share the same institutional machinery (panels, Appellate Body, DSU procedures) but disagree on what the machinery *is* structurally. The upstream (advisory) reading influences the downstream (binding/judicial) readings because the advisory function is often cited as the legitimate basis that the other readings exceed or distort.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
