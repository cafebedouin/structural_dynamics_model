% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity as Diplomatic Capital Requirement
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement is a foundational institutional
 *   rule that forces all member states to agree on Council decisions (with
 *   specific exceptions carved out by treaty amendment). This constraint
 *   exhibits a profound perspectival divide: the same institutional rule
 *   appears as a coordination mechanism that produces durable consensus
 *   (diplomatic capital reading), a sovereignty protection mechanism
 *   (sovereignty guarantor reading), or a veto trap that enables strategic
 *   obstruction (veto trap reading). This story instantiates ONE of these
 *   three readings — the diplomatic capital reading — which treats unanimity
 *   as a coordination cost with legitimacy payoff: the requirement forces
 *   iterative negotiation that produces buy-in, strengthens policy
 *   durability, and binds all parties to the outcome. This reading does NOT
 *   deny that unanimity can function as obstruction (veto trap) or as
 *   sovereignty protection (sovereignty guarantor); rather, it asserts a
 *   specific causal mechanism by which the coordination process itself
 *   generates legitimacy that reduces downstream defection. The structural
 *   evidence for this reading includes: (1) lower defection rates on
 *   unanimous decisions compared to QMV-imposed decisions in historical data;
 *   (2) member state rhetoric emphasizing legitimacy and buy-in as rationales
 *   for unanimity; (3) the treaty-entrenchment of unanimity despite
 *   recognized efficiency costs, suggesting states value the legitimacy
 *   mechanism. However, this reading is contested by states that experience
 *   unanimity as a constraint on efficiency and by analysts who see the
 *   legitimacy claim as post-hoc rationalization of veto power.
 *
 * KEY AGENTS:
 *   - Member State Coalition (Beneficiary/Constrained) — states that benefit from consensus legitimacy and can shift coalitions; experience moderate extraction because while negotiation is costly, the product is durable buy-in
 *   - Minority Preference States (Victim/Trapped) — states with genuine minority positions unable to exit; face maximum suppression through exhaustion-driven capitulation or isolation
 *   - Commission/Council Presidency (Beneficiary/Arbitrage) — institutional actors seeking legitimacy through consensus; have exit options and can shift negotiation frames
 *   - EU Legitimacy System (Victim/Trapped) — abstract collective dependent on perceived fairness; cannot exit or organize; bears cost when unanimity appears to be coercion rather than consensus
 *   - Smaller States with Median Preferences (Beneficiary/Constrained) — benefit from consensus legitimacy when included in majorities; constrained by coalition dynamics
 *   - Analytical Observer (Civilizational View) — risks naturalizing a contingent institutional choice as inevitable requirement of state sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.28).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.32).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity as Diplomatic Capital Requirement").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations").

domain_priors:requires_active_enforcement(eu_council_unanimity__diplomatic_capital_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, 'f4d23761-e464-4fa0-9f19-21e4eb81eab8').
narrative_ontology:cs_kernel_codification('f4d23761-e464-4fa0-9f19-21e4eb81eab8', formalized).
narrative_ontology:cs_authority_grounding('f4d23761-e464-4fa0-9f19-21e4eb81eab8', lineage).
narrative_ontology:cs_interpretation_layer_present('f4d23761-e464-4fa0-9f19-21e4eb81eab8').
narrative_ontology:cs_reading_relation('f4d23761-e464-4fa0-9f19-21e4eb81eab8', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4d23761-e464-4fa0-9f19-21e4eb81eab8', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('f4d23761-e464-4fa0-9f19-21e4eb81eab8', foundational, consensus_iteration_produces_legitimacy).
narrative_ontology:cs_axiom_status(consensus_iteration_produces_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f4d23761-e464-4fa0-9f19-21e4eb81eab8', consensus_iteration_produces_legitimacy, instrumental).
narrative_ontology:cs_axiom('f4d23761-e464-4fa0-9f19-21e4eb81eab8', secondary, legitimacy_justifies_efficiency_cost).
narrative_ontology:cs_axiom_status(legitimacy_justifies_efficiency_cost, holdable).
narrative_ontology:cs_axiom_grounding('f4d23761-e464-4fa0-9f19-21e4eb81eab8', legitimacy_justifies_efficiency_cost, deontological).
narrative_ontology:cs_reference_frame('f4d23761-e464-4fa0-9f19-21e4eb81eab8', westphalian_state_sovereignty_protection).
narrative_ontology:cs_drift_state('f4d23761-e464-4fa0-9f19-21e4eb81eab8', contemporary_eu_governance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f4d23761-e464-4fa0-9f19-21e4eb81eab8', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, member_state_veto_coalition).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, consensus_legitimacy_seekers).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, rapid_decision_efficiency).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, smaller_states_with_minority_views).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVERRULED MINORITY STATE (SNARE) — A state holding a genuinely minority position on EU policy faces unanimity as pure extraction. Cannot exit the EU (treaty obligations, economic integration, geopolitical isolation). Must either capitulate or obstruct. Negotiation window produces coercion through exhaustion, not genuine persuasion. Experiences high suppression: the cost of veto (diplomatic isolation, reputation damage, retaliation) forces eventual capitulation.
constraint_indexing:constraint_classification(eu_council_unanimity__diplomatic_capital_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COALITIONAL MEMBER STATE (TANGLED ROPE) — A state embedded in shifting coalitions experiences unanimity as both coordination and extraction. The requirement forces iterative negotiation that can reveal common ground and distribute benefits, but also enables coalition partners to extract side-deals and concessions. The state can threaten veto (some agency) but faces costly retaliation for overuse. Benefits from consensus legitimacy when its preferences align with the median; bears costs when minority.
constraint_indexing:constraint_classification(eu_council_unanimity__diplomatic_capital_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMMISSION/CONSENSUS SEEKERS (ROPE) — Actors invested in EU legitimacy and durability (Commission, Council presidencies, major states with stable coalitions) experience unanimity as pure coordination. The requirement forces dialogue that produces buy-in, reduces downstream defection, and strengthens policy durability. These actors have arbitrage: they can shift to QMV frameworks in specific domains or exit particular negotiation cycles. Net benefit: legitimacy from process.
constraint_indexing:constraint_classification(eu_council_unanimity__diplomatic_capital_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: EMERGENCY RESPONSE COALITION (SCAFFOLD) — Crisis situations (pandemic, security threat, economic shock) reveal unanimity as a temporarily necessary fiction with an understood sunset. States tolerate slower decision-making because the alternative (paralysis) is worse. But actors simultaneously build workarounds: emergency QMV mechanisms, enhanced cooperation provisions, informal consensus on specific domains. The scaffold has an explicit sunset: as crisis intensity declines, pressure for QMV reforms increases.
constraint_indexing:constraint_classification(eu_council_unanimity__diplomatic_capital_reading, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: TREATY FRAMEWORK AUTHORITY (PITON) — The unanimity requirement in the Treaty is maintained through institutional inertia despite widespread recognition that it produces gridlock in practice. The rule persists because reform itself requires unanimity (recursive enforcement). States mouth the language of 'protecting state sovereignty' while simultaneously negotiating workarounds (enhanced cooperation, QMV carve-outs by domain). Theater is high: sovereignty rhetoric persists despite actual decision-making shifting incrementally toward QMV in practice.
constraint_indexing:constraint_classification(eu_council_unanimity__diplomatic_capital_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a pure decision-theory perspective, any system of supra-national governance must coordinate autonomous agents. Unanimity appears as an inevitable requirement of state sovereignty — without it, weaker states have no protection against coalition exploitation. The constraint appears natural, not contingent. However, the structural data reveals this as a false summit: the 'natural' unanimity requirement is supported by states that benefit from veto power and opposed by those that prefer efficiency. The naturalness claim masks a political choice.
constraint_indexing:constraint_classification(eu_council_unanimity__diplomatic_capital_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_council_unanimity__diplomatic_capital_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_council_unanimity__diplomatic_capital_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, TR),
    TR >= 0.70.

:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The diplomatic capital reading asserts that while the negotiation process is costly (time, diplomatic resources, side-deals), the product is legitimacy and durability that reduces downstream defection costs. Unlike the veto trap reading (which would score higher), this reading treats the extraction as concentrated in the negotiation process itself, not in the policy outcome. The low score reflects the core claim of this reading: the coordination benefit (durable consensus) justifies the process cost. The upward trajectory (0.18 → 0.28 over the interval) reflects incremental increases in negotiation complexity as EU membership expanded and issue domains multiplied, requiring more extensive consensus-building. Suppression (0.32): Moderate. The unanimity requirement creates real barriers to disagreement — the cost of veto includes isolation, retaliation, and coalition pressure — but suppression is not maximal because states can threaten veto and occasionally execute it, providing some voice. Smaller/weaker states experience higher suppression; larger states with coalitional power experience lower suppression. The metric value reflects an average across member states. Theater ratio (0.45): Moderate-low. The consensus-building process has performative elements — states engage in ritualized negotiation, symbolic concessions, and public presentations of agreement — but substantial functional work occurs (issue exploration, coalition-building, preference revelation). The upward trajectory reflects increasing ritualization as the EU has matured and consensus-building has become institutionalized (presidency agendas, multitrack negotiation formats). At t=0 (pre-Maastricht), more negotiation was substantive; at t=10 (post-Lisbon), more of the work occurs in pre-Council coordination and the Council session itself is partially theater.
 *
 * PERSPECTIVAL GAP:
 *   The diplomatic capital reading generates different classifications from the other two sibling readings at the same base property values. Against the veto trap reading (which would classify this as Snare from the victim's perspective and emphasize obstruction dynamics), the diplomatic capital reading emphasizes legitimacy payoff and durability. Against the sovereignty guarantor reading (which would justify unanimity as protecting state autonomy), the diplomatic capital reading treats the legitimacy as the primary mechanism, not sovereignty. The perspectival gap appears most sharply in the 'overruled minority' vs 'protected sovereign' debate: the sovereignty reading frames unanimity as a protection mechanism; the diplomatic capital reading frames it as a legitimacy mechanism. A minority state could simultaneously be overruled (snare perspective) and protected (sovereignty perspective) — the question is which framing captures the actual mechanism. This constraint's perspectival gap IS the disagreement among the three sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation for this constraint differs from typical institutional constraints because the beneficiary/victim structure is not symmetric. The 'member state veto coalition' benefits through legitimacy and predictable coalitional position (d≈0.25, low extraction); the 'consensus legitimacy seekers' benefit through process legitimacy (d≈0.20); but the 'rapid decision efficiency' and 'smaller states with minority views' bear costs. The Commission and Council presidencies have arbitrage options (can shift frames or invoke emergency QMV), producing low d. Smaller states with minority views face constrained exit, producing higher d and higher experienced extraction. The analytical observer at civilizational scope produces moderate d (0.72) because they must see both the legitimacy payoff AND the efficiency cost simultaneously.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_vs_coercion_boundary,
    'When does iterative negotiation under unanimity produce genuine consensus vs. exhaustion-driven capitulation?',
    'Post-agreement durability analysis: do agreements reached under unanimity show lower defection rates than QMV agreements? Longitudinal tracking of policy compliance and re-litigation in subsequent councils.',
    'If high durability from unanimity consensus: the rope/tangled_rope readings are confirmed (coordination with legitimacy payoff). If durability is equivalent regardless of procedure: the snare reading is confirmed (negotiation is coercive extraction masked as consensus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_vs_coercion_boundary, empirical, 'Consensus legitimacy vs. exhaustion-driven compliance under unanimity').

omega_variable(
    alternate_reading_empirics,
    'Which of the three sibling readings (diplomatic_capital, sovereignty_guarantor, veto_trap) is most supported by the empirical record of EU decision-making over the past 20 years?',
    'Comparative analysis: (1) breakdown of decisions by unanimity vs QMV; (2) duration of negotiations; (3) state-reported reasons for agreement/veto threats; (4) post-agreement defection rates; (5) correlation between coalitional power and policy outcomes.',
    'If diplomatic_capital reading: unanimity produces durable consensus with legitimacy payoff (lower ε, coordination benefit). If sovereignty_guarantor: unanimity protects weak states from exploitation (higher moral legitimacy, moderate ε). If veto_trap: unanimity enables systematic obstruction by strategic minorities (higher ε, pure extraction for non-veto-holding states).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternate_reading_empirics, empirical, 'Empirical support for competing readings of EU unanimity requirement').

omega_variable(
    legitimacy_measurement_ambiguity,
    'Is the perceived legitimacy of unanimous decisions genuine normative acceptance or internalized procedural compliance (agents accepting outcomes because the process was felt to be fair, even if outcomes weren''t preferred)?',
    'Surveys and interviews of member-state officials post-agreement: distinguish between ''I accept this outcome because I had voice'' vs ''I accept this outcome because it''s substantively fair.'' Track whether legitimacy persists when preferences shift.',
    'If genuine acceptance: the diplomatic_capital reading is correct (consensus produces durable buy-in). If procedural legitimacy masks underlying discontent: the reading is aspirational and the true mechanism is softer coercion through normalization (closer to snare or piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_measurement_ambiguity, empirical, 'Whether unanimity-derived legitimacy is normative acceptance or procedural compliance').

omega_variable(
    foreclusion_vs_coexistence_reading_structure,
    'Do the three sibling readings (diplomatic_capital, sovereignty_guarantor, veto_trap) truly coexist as live options for different parties, or does the empirical record show one reading foreclosing others?',
    'Analysis of member-state rhetoric and revealed preferences: do states simultaneously hold (or credibly could hold) both the diplomatic_capital axiom (consensus strengthens legitimacy) and the veto_trap axiom (unanimity enables obstruction)? Historical cases where the same state shifts readings depending on coalitional position.',
    'If coexistence: the reading_relations are correctly marked as coexists_with. If one reading''s axioms logically foreclose another: update to forecloses. If influence is directional: update to influences with specific downstream mechanisms identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclusion_vs_coexistence_reading_structure, conceptual, 'Whether competing unanimity readings coexist or foreclose each other').

omega_variable(
    false_summit_sovereignty_naturalization,
    'Is the claim that unanimity is a natural requirement of state sovereignty (mountain reading) a genuine logical necessity or a constructed claim that benefits states with veto power?',
    'Comparative institutional analysis: do federal systems without unanimity requirements (US, Australia, Canada) lack legitimacy for decisions affecting constituent units? Do non-state supra-national systems (FIFA, IOC) use unanimity or alternatives? What legitimacy mechanism substitutes for unanimity in other contexts?',
    'If unanimity is universal requirement: mountain classification confirmed. If legitimacy is achievable through other mechanisms: the mountain reading is a false summit (naturalization of a contingent choice that benefits specific state coalitions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_sovereignty_naturalization, empirical, 'Whether unanimity is natural requirement of sovereignty or constructed institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecu_dip_theater_1995, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ecu_dip_theater_2005, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(ecu_dip_theater_2015, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(ecu_dip_extractiveness_1995, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ecu_dip_extractiveness_2005, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(ecu_dip_extractiveness_2015, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(ecu_dip_suppression_1995, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ecu_dip_suppression_2005, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 5, 0.29).
narrative_ontology:measurement(ecu_dip_suppression_2015, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, qualified_majority_voting_efficiency_gains).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, enhanced_cooperation_institutional_fragmentation).

% DUAL FORMULATION NOTE:
% The EU Council unanimity requirement is a single kernel that three structurally distinct readings interpret differently. This story (diplomatic_capital_reading) models unanimity as coordination with legitimacy payoff (low ε, tangled rope). The sovereignty_guarantor_reading models it as state protection against majoritarian exploitation (different ε, different beneficiary structure). The veto_trap_reading models it as obstruction mechanism (higher ε, snare for victims). Each reading gets its own constraint_id and separate story file. They are linked via network.affects_constraints and explicitly cross-referenced in omega variables that address reading competition and foreclosure relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__diplomatic_capital_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
