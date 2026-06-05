% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause — Maximal Withdrawal Reading (Full Retrocession Binding)
 *   domain: international_law/treaty_interpretation/territorial_integrity
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) established that 'the acquisition of territory
 *   by war is inadmissible' and called for 'withdrawal of Israeli armed
 *   forces from territories occupied in the recent conflict.' The maximal
 *   withdrawal reading interprets this as a comprehensive, mandatory
 *   obligation to retrocede all occupied territories to the territorial
 *   entity (state or people) with prior territorial title. The reading is
 *   grounded in Charter Article 2(4) territorial integrity default and the
 *   French definite article in the original text ('des territoires occupés' —
 *   'the occupied territories,' implying a specific determinate set). This
 *   reading binds the occupier to a complete retrocession obligation
 *   independent of bilateral agreement or security conditions. It establishes
 *   a coordination mechanism (the territorial integrity principle benefits
 *   all states) while simultaneously extracting from the occupying state
 *   (obligating costly withdrawal). The constraint exhibits all features of
 *   Tangled Rope: genuine coordination function (establishes boundaries for
 *   conquest), asymmetric extraction (targets occupier specifically), and
 *   requirement for active enforcement (UN mechanisms, sanctions, legal
 *   pressure). Over 50 years, extractiveness has increased as settlement
 *   expansion has raised the cost of compliance; theater ratio has increased
 *   as UN processes have become more ceremonial relative to enforcement
 *   capacity; suppression has intensified as the occupying state's security
 *   arguments have hardened to prevent compliance.
 *
 * KEY AGENTS:
 *   - Occupying State: Powerful institutional actor (powerful/constrained) — bears full extraction cost of withdrawal obligation; experiences suppression from dispossessed population's claims and international pressure
 *   - Dispossessed Territorial Claimants: Powerless collective (powerless/trapped) — beneficiary of the maximal reading in principle, but trapped in occupation indefinitely without enforcement; victim of extraction via non-compliance
 *   - Settlement Populations: Victims trapped in occupied territory (moderate/identity_locked) — identity fused with occupation project; exit would require abandoning ideological/national identity commitment
 *   - International Legal Order: Institutional beneficiary (institutional/arbitrage) — benefits from coordination principle (territorial integrity as universal norm); maintains legitimacy through principle reaffirmation
 *   - UN Security Council: Institutional theater (institutional/arbitrage) — maintains performative enforcement authority while actual enforcement blocked by veto power; supervises ceremonial rather than material compliance
 *   - Coalition of Territorial Claimants: Organized beneficiary (organized/mobile) — benefits from maximal reading establishing norm floor; constrained by same rule when they are occupiers; maintains coalition through legal advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.62).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.68).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause — Maximal Withdrawal Reading (Full Retrocession Binding)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/treaty_interpretation/territorial_integrity").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '3eddd03b-8e01-4e00-9e9d-3463e20c99f0').
narrative_ontology:cs_kernel_codification('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', fixed_text).
narrative_ontology:cs_authority_grounding('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', extraction).
narrative_ontology:cs_interpretation_layer_present('3eddd03b-8e01-4e00-9e9d-3463e20c99f0').
narrative_ontology:cs_reading_relation('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', foundational, territorial_integrity_absolute_default).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute_default, holdable).
narrative_ontology:cs_axiom_grounding('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', territorial_integrity_absolute_default, deontological).
narrative_ontology:cs_axiom('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', foundational, occupation_obligation_comprehensive_scope).
narrative_ontology:cs_axiom_status(occupation_obligation_comprehensive_scope, holdable).
narrative_ontology:cs_axiom_grounding('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', occupation_obligation_comprehensive_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', universal_territorial_integrity_principle).
narrative_ontology:cs_drift_state('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', post_fifty_year_occupation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3eddd03b-8e01-4e00-9e9d-3463e20c99f0', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settlers_in_occupied_territory).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED POPULATION (SNARE) — Trapped in occupied territory with no exit path. The maximal reading establishes a legal claim but provides no enforcement mechanism, no timeline, no reparations, and no protection during the indefinite interim. Extraction: occupying state maintains territorial control indefinitely while invoking 'security concerns' that override the nominal legal obligation. Suppression is maximal — settlement expansion, administrative incorporation, military governance all consolidate occupation despite the legal constraint.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__maximal_withdrawal_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTERNATIONAL LEGAL SYSTEM — NEUTRAL VIEW (ROPE) — The maximal reading as pure coordination mechanism: establishes a shared norm (territorial integrity default) that enables all states to appeal to the same principle against conquest. The constraint binds the occupier to accept the legal framework even while violating it operationally. Beneficiary: the international order gains legitimacy through consistent principle application. Low effective extraction because the system experiences the rule as coordination benefit — it stabilizes state boundaries universally.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: OCCUPYING STATE (TANGLED ROPE) — The maximal reading creates a genuine tension between coordination and extraction. Coordination function: the rule establishes clear boundaries for territorial claims and prevents open conquest as a legitimate policy tool (this benefits the occupying state itself when it is a claimant against others, or when external actors might occupy it). Extraction: the maximal reading obligates the occupier to retrocede territory it has incorporated administratively and strategically, constraining its sovereignty and requiring costly withdrawal. The constraint requires active enforcement (international pressure, sanctions, legal liability) to bind the powerful actor. Suppression: moderate-high because the occupier can resist enforcement through military deterrence and strategic ambiguity about compliance timelines.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__maximal_withdrawal_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: COALITION OF TERRITORIAL CLAIMANTS (TANGLED ROPE) — Organized states and movements with territorial disputes. Coordination function: the maximal reading establishes a normative floor for all occupation disputes — no actor can claim legitimate acquisition of territory by force. This benefits claimants by preventing occupiers from normalizing occupation indefinitely. Extraction: the rule constrains claimants as well — those with disputed territories cannot use force themselves to reclaim them. The constraint requires enforcement (UN recognition, sanctions regime) but claimants have some agency through coalition-building and legal advocacy. Exit options: mobile because actors can defect from the framework through unilateral recognition of occupation or armed resistance, though at significant cost.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__maximal_withdrawal_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UN SECURITY COUNCIL — THEATER VIEW (PITON) — The maximal reading is maintained through performative UN processes (resolutions, statements, symbolic condemnations) while lacking enforcement capacity. Theater ratio is elevated (0.35 → 0.55 over interval) because the Council ritualistically reaffirms the principle while the occupying state's veto or major-power protection prevents enforcement measures. The functional verification of compliance has degraded to symbolic voting rather than material accountability. The constraint persists through institutional inertia and ceremonial legitimation rather than operational enforcement.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__maximal_withdrawal_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational/universal perspective, the maximal reading instantiates the Charter's foundational coordination principle: territorial integrity as the default constraint on state action. This perspective sees the constraint as pure coordination — establishing shared normative space that benefits all states by preventing norm-erosion toward conquest legitimacy. The analytical view abstracts away from enforcement failure and operational violation, treating the reading as binding coordination logic.
constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__maximal_withdrawal_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unsc_242_withdrawal_clause__maximal_withdrawal_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, TR),
    TR >= 0.70.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The maximal reading obligates the occupier to full retrocession, imposing substantial costs (territorial loss, strategic disadvantage, settler displacement, administrative dismantling). However, extractiveness is not extreme (not ≥0.70) because: (1) enforcement mechanism is weak (no automatic sanctions, no credible military enforcement, veto power protection available), (2) occupier has mitigation strategies (indefinite interim period, security arguments, strategic settlement), (3) the constraint binds through normative/legal force rather than physical coercion. The baseline ε=0.45 (1973) reflects the initial binding force of the principle; ε=0.62 (2023) reflects accumulated cost as settlement has grown and interim period has calcified. Suppression (0.68): Moderate-high. Suppression operates through multiple mechanisms: military deterrence prevents enforcement action, diplomatic veto protection blocks sanctions, strategic security framing overrides legal obligation, settlement expansion normalizes occupation, international attention fatigue. But suppression is not total because: (1) dispossessed population maintains legal claims, (2) international stigma persists, (3) liability exposure is real (ICC jurisdiction ambiguity), (4) coalition pressure continues. Theater ratio (0.25 → 0.52): Rising sharply. Initial theater was low (genuine enforcement machinery, serious legal binding) but has escalated as the gap between stated obligation (withdrawal) and actual practice (expansion) has widened. UN processes have become increasingly ceremonial (annual resolutions with predictable vote counts, symbolic statements, no enforcement). The rising theater reflects Piton mechanics: institutional inertia maintains the constraint's performative form while abandoning its functional content.
 *
 * PERSPECTIVAL GAP:
 *   The maximal reading produces a severe perspectival gap between the occupying state and the dispossessed population. The occupier sees Tangled Rope — the constraint coordinates state boundaries (benefits all states equally) while extracting from this specific state (forces withdrawal). The occupier can point to legitimate coordination gains (territorial integrity principle protects occupier when it faces external threats). The dispossessed see Snare — they are trapped in occupation indefinitely, the legal obligation provides no material benefit without enforcement, and normalization (settlement, infrastructure, cultural incorporation) accumulates extraction over time. The International Legal System (neutral arbiter) sees Rope — the constraint is pure coordination establishing shared principle. The UN sees Piton — ceremonial reaffirmation of a principle that institutional veto power prevents from being enforced. This perspectival gap is the diagnostic signature of the constraint: the same legal text produces fundamentally different experienced constraints depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (ε=0.62), the agent's directionality value (d), and scope modifier. The occupying state (powerful/constrained) experiences high d (~0.68–0.75) because it is the direct victim of the extraction (territorial obligation). Dispossessed claimants (powerless/trapped) also experience high d (~0.92) because they are the nominal beneficiaries but cannot collect on the benefit (extraction runs toward occupier but benefit is blocked by non-enforcement). The International Legal System (institutional/arbitrage) experiences low d (~0.15) because it is the beneficiary of the coordination principle. The analytical observer (analytical/analytical) experiences d~0.72 (observer position) applied to the structurally-determined extraction. The powerful occupier's constrained exit (not arbitrage) keeps d higher than if it had unilateral choice; the dispossessed's trapped exit drives d near maximum despite theoretical beneficiary status.
 *
 * MANDATROPHY ANALYSIS:
 *   The maximal reading resolves mandatrophy by instantiating Tangled Rope with full structural justification. The coordination function is genuine (territorial integrity as universal principle; benefits all states including the occupier). The asymmetric extraction is genuine (targets the occupier with withdrawal obligation; targets dispossessed with indefinite interim period). Active enforcement is required (UN mechanisms, sanctions, legal liability). The constraint is not pure extraction (Snare, which would have no coordination function) because the occupier does benefit from the universal principle when it faces external threats. The constraint is not pure coordination (Rope) because the extraction cost is substantial and asymmetrically distributed. The Tangled Rope classification is the unique stable point for this structure. FALSE SUMMIT CHECK: The analytical observer's Rope perspective might be tempted toward Mountain (territorial integrity as natural law of the international system) — this would be a false summit, naturalizing what is actually a contingent institutional arrangement (Charter Article 2(4) is written law, not discovered principle). The constraint remains Tangled Rope from the analytical view because the internationl legal order's existence is contingent on state agreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_ambiguity,
    'Does mandatory withdrawal without specified enforcement mechanism constitute a binding legal obligation or aspirational principle?',
    'Historical analysis of similar non-enforced territorial obligations; comparison of compliance rates for principles vs rules with enforcement apparatus; examination of occupier''s good-faith negotiation indicators',
    'If binding: constraint is Snare or Tangled Rope (extraction via non-compliance). If aspirational: constraint downgrade to Rope (coordination without enforcement teeth). This resolves which reading (maximal withdrawal vs partial withdrawal compromise) is structurally instantiated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_ambiguity, conceptual, 'Whether absence of enforcement mechanism negates binding force of withdrawal obligation').

omega_variable(
    definitional_scope_ambiguity,
    'Does ''withdrawal from all occupied territories'' include settlements established by the occupier, or only military/administrative presence?',
    'Textual exegesis of 242; comparison with subsequent interpretive precedents (Taba, Golan Heights, Cyprus); analysis of what territorial entity the dispossessed population claims',
    'If settlements included: extraction cost for occupier is maximal (ε = 0.68–0.75). If settlements excluded: occupier has mitigation path (ε = 0.45–0.55). This determines whether the maximal reading''s comprehensive scope is binding or permits strategic redefinition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definitional_scope_ambiguity, conceptual, 'Whether settlement evacuation is mandated by the withdrawal clause').

omega_variable(
    interim_state_legitimacy_gap,
    'For an indefinite interim period (decades or generations), can occupation be legitimate under the maximal reading even though the terminal obligation is withdrawal?',
    'Temporal analysis of occupation length vs. legitimacy claims; examination of whether interim period allows normalization (settlement expansion, institutional incorporation); test of whether occupier''s compliance with interim transparency/accountability standards affects terminal legitimacy',
    'If interim legitimacy is possible: the maximal reading permits indefinite delay (extraction mechanism is time itself). If interim period implies illegitimacy of occupation: the reading enforces pressure toward compliance (snare from dispossessed perspective becomes squeezed toward resolution). This determines whether the constraint functions as binding (Snare) or as indefinite postponement (Rope with theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interim_state_legitimacy_gap, preference, 'Whether indefinite interim occupation is compatible with maximal withdrawal reading').

omega_variable(
    kernel_contest_constraint_identity,
    'Is the maximal withdrawal reading a different constraint from the partial withdrawal reading, or are they interpretations of a single constraint?',
    'Structural comparison: do the two readings produce different ε values when measured by identical observables (enforcement capacity, settlement scope, timeline pressure)? If ε differs by >0.15, they are different constraints per DP-001 (ε-invariance principle). If ε is same but interpretation differs, they are readings of one kernel.',
    'If different constraints: each story is authored separately with separate ε, separate perspectives, separate networks. If readings of one kernel: cs_structure documents the relationship via reading_relations and axioms. This determines whether the JSON output structure is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_constraint_identity, conceptual, 'Whether maximal and partial readings are distinct constraints or interpretations of UNSC 242 kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1973, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc242_max_theater_1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unsc242_max_theater_1998, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(unsc242_max_theater_2023, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(unsc242_max_extractiveness_1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(unsc242_max_extractiveness_1998, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(unsc242_max_extractiveness_2023, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(unsc242_max_suppression_1973, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(unsc242_max_suppression_1998, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(unsc242_max_suppression_2023, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, golan_heights_annexation_legality).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settlement_expansion_legitimacy).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, reparations_obligation_non_compliance).

% DUAL FORMULATION NOTE:
% The maximal withdrawal reading and partial withdrawal reading are both readings of the UNSC 242 kernel, but they instantiate different ε values and different constraints structurally. The maximal reading obligates full retrocession (high extraction), while the partial reading permits strategic retention (low extraction, shifted beneficiary). These are not the same constraint viewed from different angles — they are different constraints that share a common textual origin. Link them via network.affects_constraints because the maximal reading establishes the normative floor that the partial reading negotiates from.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
