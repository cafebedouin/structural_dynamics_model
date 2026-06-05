% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Conventions Protective Scope: State-Centric Reading
 *   domain: international_humanitarian_law/armed_conflict_law/legal_theory
 *
 * SUMMARY:
 *   The Geneva Conventions of 1949 establish an international humanitarian
 *   law regime designed to protect combatants and civilians during armed
 *   conflict. The state-centric reading of this regime interprets Article 4
 *   as the canonical definition of lawful combatant: only those in uniform,
 *   under responsible command structure, carrying arms openly, and wearing
 *   fixed emblems/insignia qualify for combatant immunity and prisoner-of-war
 *   (POW) protections. All other armed actors — non-state groups, insurgents,
 *   militias, armed civilians, unprivileged belligerents — fall outside the
 *   treaty's protective scope for combatants. This reading has major
 *   structural consequences: it narrows the victim set (excludes non-state
 *   armed actors from combatant protections), lowers the ethical and legal
 *   constraints on targeting unprivileged combatants (they receive no
 *   immunity from being targeted as long as they are not actively engaged),
 *   and benefits conventional state militaries facing asymmetric conflicts
 *   where insurgents cannot or will not meet Article 4 criteria. The
 *   state-centric reading is one of three major contested interpretations of
 *   the Geneva Conventions' protective kernel, alongside the
 *   hybrid-proportionality reading (which scales protections by conflict type
 *   and applies proportionality analysis) and the universal-rights reading
 *   (which extends protections to all persons regardless of combatant status
 *   via Common Article 3 and human rights law). This story models the
 *   state-centric reading as a clean, extractive institutional position with
 *   genuine coordination functions, operative primarily in state military
 *   strategic contexts and increasingly challenged by proportionality
 *   doctrine and human rights adjudication.
 *
 * KEY AGENTS:
 *   - Conventional State Militaries: Institutional beneficiary (institutional/arbitrage) — enjoy clear combatant immunity and POW status; benefit from legal certainty that permits targeting of unprivileged belligerents
 *   - Unprivileged Belligerents / Insurgent Fighters: Primary victims (powerless/trapped) — excluded from combatant immunity and full POW protections; bear maximum extraction and suppression
 *   - Non-State Armed Groups: Secondary victim (moderate/constrained) — face classification as unlawful combatants but see incentive pathway through Article 4 formalization; constrained exit
 *   - Occupying Powers / Counter-Insurgency States: Powerful beneficiaries (powerful/mobile) — derive operational flexibility from state-centric reading; can target non-Article-4-compliant groups without legal restriction
 *   - International Humanitarian Law Institutional Framework: Institutional actor (institutional/arbitrage) — maintains state-centric reading through interpretive lineage and treaty authority; increasingly piton (performative) as proportionality doctrine and hybrid conflicts diverge from Article 4 categories
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing state-centric reading as inevitable logical consequence of combatant law, missing its historical contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.52).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.68).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Conventions Protective Scope: State-Centric Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/armed_conflict_law/legal_theory").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '39c1438c-d57f-4ac3-9804-ea0261dd5de3').
narrative_ontology:cs_kernel_codification('39c1438c-d57f-4ac3-9804-ea0261dd5de3', fixed_text).
narrative_ontology:cs_authority_grounding('39c1438c-d57f-4ac3-9804-ea0261dd5de3', lineage).
narrative_ontology:cs_interpretation_layer_present('39c1438c-d57f-4ac3-9804-ea0261dd5de3').
narrative_ontology:cs_reading_relation('39c1438c-d57f-4ac3-9804-ea0261dd5de3', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_reading_relation('39c1438c-d57f-4ac3-9804-ea0261dd5de3', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('39c1438c-d57f-4ac3-9804-ea0261dd5de3', foundational, article_4_exclusivity_test).
narrative_ontology:cs_axiom_status(article_4_exclusivity_test, holdable).
narrative_ontology:cs_axiom_grounding('39c1438c-d57f-4ac3-9804-ea0261dd5de3', article_4_exclusivity_test, conventional).
narrative_ontology:cs_axiom('39c1438c-d57f-4ac3-9804-ea0261dd5de3', foundational, state_centric_protection_structure).
narrative_ontology:cs_axiom_status(state_centric_protection_structure, holdable).
narrative_ontology:cs_axiom_grounding('39c1438c-d57f-4ac3-9804-ea0261dd5de3', state_centric_protection_structure, conventional).
narrative_ontology:cs_reference_frame('39c1438c-d57f-4ac3-9804-ea0261dd5de3', state_centric_combatant_taxonomy).
narrative_ontology:cs_drift_state('39c1438c-d57f-4ac3-9804-ea0261dd5de3', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('39c1438c-d57f-4ac3-9804-ea0261dd5de3', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_combatants).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, civilian_protection_claims_outside_article_4).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPRIVILEGED COMBATANT (SNARE) — Bears maximum extraction under state-centric reading. Fails Article 4 criteria (no uniform, no responsible command structure, or non-state origin). Falls outside combatant immunity and POW protections. Faces targeting without legal restraint; combatant status itself is the mechanism of suppression. Zero degrees of freedom — cannot exit asymmetric conflict while remaining combatant; cannot claim status protections. Trapped.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__state_centric_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-STATE ARMED GROUP (TANGLED ROPE) — Constrained exit: faces legal classification as unprivileged combatant if they do not meet Article 4 criteria, but also sees genuine coordination function in Common Article 3 minimum protections and incentives to organize hierarchically, wear insignia, and claim responsibility for action to access combatant immunity. The constraint both excludes and incentivizes — creates pathway to limited legitimacy through formalization. Extraction occurs (denial of full POW protections) but coordination benefit exists (legal recognition available if criteria met).
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__state_centric_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CONVENTIONAL STATE MILITARY (ROPE) — Net beneficiary of state-centric reading. Enjoys clear combatant immunity and POW status for uniformed personnel meeting Article 4 criteria. Can target unprivileged belligerents without restriction. Interprets constraint as coordination mechanism: clarifies who is protected, enables planning and targeting strategy, reduces legal ambiguity in asymmetric conflicts. Sees the reading as solving legitimate coordination problem of distinguishing lawful combatant from civilian. Arbitrage: can shift targeting based on threat classification.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__state_centric_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OCCUPYING POWER / COUNTER-INSURGENCY STATE (TANGLED ROPE) — Powerful institutional actor (military, security apparatus) faces genuine coordination need (must identify combatants vs civilians to conduct legally defensible operations) but also derives extraction benefit from state-centric reading that narrows victim set and permits targeting of non-Article-4-compliant groups. Mobile exit (can change operational doctrine, can negotiate) but constrained by need to maintain international legitimacy. Sees state-centric reading as enabling operational flexibility while preserving legal cover. Moderate extraction with coordination function.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__state_centric_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: IHL INSTITUTIONAL FRAMEWORK (PITON) — The state-centric reading is maintained through institutional inertia and interpretive lineage (state-centric reading is the 'legacy' interpretation of Geneva Conventions from 1949 drafting era, designed for inter-state wars). Much of its actual function is now performative — the reading's operative legal scope is constantly narrowed by judicial interpretation, state practice divergence, and hybrid conflict typologies that don't fit the original Article 4 taxonomy. Theater ratio reflects institutional persistence (treaties remain authoritative) despite reduced functional discrimination (non-international armed conflicts proliferate; Article 4 categories no longer cleanly map to operational reality). Maintained through authority of the text, not because the reading solves current classification problems.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__state_centric_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely logical standpoint, the state-centric reading appears as a structural necessity: ANY legal system protecting combatants must define who counts as a combatant; therefore Article 4 criteria represent an immutable threshold. However, this perspective collapses under structural scrutiny — the criteria are historically contingent (suited to 1949 state-on-state warfare) and actively contested (hybrid conflicts, proportionality doctrine, human rights adjudication challenge the categorization). Engine will classify this as false summit: the 'logical necessity' naturalizes a reading that is historically specific and politically consequential.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__state_centric_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geneva_conventions_protective_scope__state_centric_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geneva_conventions_protective_scope__state_centric_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, TR),
    TR >= 0.70.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, rising from 0.38): Moderate-high, with upward trajectory. The state-centric reading creates extractive asymmetry by excluding non-state combatants from protections while maintaining protections for state forces. The extraction is not maximal (snare-level ε≈0.72) because the reading includes genuine coordination functions — it clarifies who qualifies as lawful combatant, enables military planning, and reduces legal ambiguity in targeting decisions. However, extractiveness has risen over the interval (1949-2024) as asymmetric conflicts have proliferated (insurgencies, counterinsurgency, terrorism) making the Article 4 criteria increasingly difficult to apply and increasingly selective in whom they protect. The rise from 0.38 to 0.52 reflects accumulating extraction: state military doctrine has adapted to use the state-centric reading explicitly as a justification for targeting non-Article-4-compliant groups, while proportionality doctrine simultaneously develops as a competing constraint. Suppression (0.68, rising from 0.55): High and increasing. The reading suppresses alternative forms of combatant recognition (customary law-based recognition, honor and respect for non-uniformed combatants, functional definitions of combatancy beyond Article 4 formalism). The rise reflects institutional hardening: states have doubled down on Article 4 as the exclusive test, using legal categorization as a suppression mechanism (exclusion from protections). Theater ratio (0.35, rising from 0.25): Moderate and increasing. The state-centric reading operates with moderate performative content: the Article 4 categories are maintained as the official legal test, but actual operations increasingly apply proportionality analysis or context-specific targeting criteria that de facto implement different categorizations. The rise reflects growing gap between official doctrine (state-centric) and operational doctrine (increasingly hybrid/proportionality-based), making the institutional maintenance of Article 4 primacy increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The state-centric reading produces sharp perspectival gaps. Conventional state militaries see Rope — the constraint solves a coordination problem (clear combatant identification) and benefits them strategically. Occupying powers and counter-insurgency states see Tangled Rope — genuine coordination need (must distinguish combatants from civilians) combined with extraction benefit (narrows victim set). Unprivileged combatants see Snare — maximum extraction and suppression with no exit option. Non-state armed groups see Tangled Rope with different directionality — they face extraction (denial of full protections) but also see incentive pathway to limited legitimacy through formalization. The IHL institutional framework sees Piton — the reading persists through interpretive inertia despite declining functional fit for modern conflicts. The analytical observer risks Mountain (natural law) — falsely naturalizing what is a historically contingent institutional choice. The perspectival distribution reveals that the state-centric reading is extractive precisely because it conflates coordination function (identifying lawful combatants in conventional inter-state war) with institutional power (excluding non-state combatants from protection in asymmetric conflict).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: power level, exit options, and beneficiary/victim status. Conventional state militaries: institutional power + arbitrage exit + beneficiary status → low d (≈0.15) → negative f(d) ≈ -0.01 → negative chi (experiencing the constraint as beneficial coordination, not extraction). Unprivileged combatants: powerless + trapped exit + victim status → high d (≈0.95) → f(d) ≈ 1.42 → high chi (maximum experienced extraction). Non-state armed groups: moderate power + constrained exit + mixed beneficiary/victim (excluded from protections but incentivized to formalize) → mid-range d (≈0.55) → f(d) ≈ 0.75 → moderate chi. Occupying powers: powerful + mobile exit + beneficiary (operational flexibility) → mid-low d (≈0.35) → f(d) ≈ 0.30 → moderate chi (significant benefit but some legal/legitimacy constraints). IHL institutional framework: institutional + arbitrage exit + beneficiary (maintains treaty authority) → low d (≈0.15) → f(d) ≈ -0.01 → piton classification dominates over chi. The directionality logic reveals that the state-centric reading's extractiveness is concentrated on powerless, trapped agents (non-state combatants) while benefiting institutional actors with mobile or arbitrage exits. No directionality overrides required — the derivation chain produces correct chi values for each perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_4_criteria_fit,
    'Do Article 4 criteria (uniform, hierarchical command, carried arms openly, emblems/insignia) adequately identify combatants in non-international armed conflicts and asymmetric warfare?',
    'Empirical: analysis of non-state armed groups meeting vs failing Article 4 criteria; correlation between Article 4 compliance and lawful combatant function. Conceptual: whether the criteria are intended as functional tests or formal requirements.',
    'If criteria fit: state-centric reading classifies many groups correctly; extractiveness remains ε≈0.52 (moderate). If criteria fail fit: state-centric reading misclassifies compliant groups as unprivileged; extractiveness rises to ε≈0.68+ (high snare). If criteria are only formal: reading is performative theater, elevating piton perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_4_criteria_fit, empirical, 'Adequacy of Article 4 criteria for non-international conflict classification').

omega_variable(
    article_4_vs_common_article_3_hierarchy,
    'Is the state-centric reading''s distinction between Article 4 (international conflict, full POW protections) and Common Article 3 (non-international conflict, minimal protections) itself a form of extraction, or a genuine coordination differentiation?',
    'Structural: does the distinction solve a real coordination problem (identifying combatants in different conflict types) or does it primarily serve to narrow victim protections in conflicts where non-state actors dominate? Comparative: analysis of whether different protective levels correspond to different operational contexts or to political power asymmetries.',
    'If coordination: state-centric reading remains Rope or Tangled Rope across most perspectives. If extraction-motivated: extraction value (ε) rises; reading reclassifies toward Snare for non-state victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_vs_common_article_3_hierarchy, conceptual, 'Whether Article 4 / Common Article 3 distinction is functional or extractive').

omega_variable(
    proportionality_doctrine_displacement,
    'To what extent has proportionality analysis in international humanitarian law (IHL) doctrine rendered the binary Article 4 distinction (combatant/unprivileged) obsolete in actual legal reasoning?',
    'Doctrinal: analysis of case law from international courts (ICC, ICJ), hybrid tribunals, and state practice showing whether proportionality is applied as a substitute for or in addition to Article 4 categorical distinctions. If proportionality dominates reasoning, state-centric reading is piton (functionally degraded). If Article 4 remains primary gating, state-centric reading retains functional force.',
    'If proportionality dominates: state-centric reading''s operative scope is dramatically narrower than text suggests; theater_ratio rises to ≥0.70; reclassify to Piton. If Article 4 retains primacy: state-centric reading remains Tangled Rope / Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_doctrine_displacement, empirical, 'Proportionality analysis displacement of Article 4 categorical distinctions').

omega_variable(
    sibling_reading_operational_differentiation,
    'In actual state military practice during asymmetric conflict, how do operators differentiate between state-centric reading and hybrid-proportionality reading targeting decisions?',
    'Empirical: analysis of military law of war training, targeting memoranda, rules of engagement during specific conflicts. Do doctrines explicitly cite Article 4 / Common Article 3 distinction, or do they apply proportionality-first analysis that de facto implements hybrid reading? Interviews or declassified materials from military legal advisors.',
    'If state-centric operationalized: reading has real behavioral force; ε reflects actual targeting decisions. If hybrid operationalized: state-centric reading is institutional fantasy (piton); actual constraint is the hybrid reading, not state-centric. This affects which sibling reading is the real operative constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_operational_differentiation, empirical, 'Operational differentiation between state-centric and hybrid readings in military practice').

omega_variable(
    reading_kernel_interpretation_layer,
    'Does the Geneva Conventions text itself (1949 treaty language, preamble, Articles 1-4) support a single definitive reading, or are state-centric, hybrid-proportionality, and universal-rights readings all legitimate readings of the same kernel?',
    'Textual analysis: careful parsing of Article 4 language, Common Article 3, preamble humanitarian spirit. Meta-doctrinal: examination of ILC travaux préparatoires, state ratification declarations, and founding debates to determine authorial intent. Hermeneutic: whether the kernel is determinate (one reading correct) or under-determined (multiple readings defensible).',
    'If kernel determinate (state-centric interpretation intended): other readings are misinterpretations; state-centric reading gains authority; omegas about alternative readings become false alternatives. If kernel under-determined: all three readings are defensible; state-centric reading is ONE legitimate reading, not THE reading; omega routing to committer frame is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_interpretation_layer, conceptual, 'Determinacy of Geneva Conventions kernel text regarding protective scope').

omega_variable(
    false_summit_naturalization_mechanism,
    'Is the analytical observer''s ''mountain'' perspective (Article 4 criteria as logically necessary for any combatant law) a genuine natural law, or does it naturalize what is a historically contingent institutional choice?',
    'Comparative law: analysis of alternative legal systems for defining combatants (e.g., Islamic law of armed conflict, customary law of some armed groups, non-Western legal traditions). Genealogical: historical trace of Article 4 criteria back to specific 1949 diplomatic choices and earlier Hague Convention traditions, showing the criteria are constructed, not discovered.',
    'If genuine natural law: mountain classification correct; state-centric reading is immutable; no false summit signal. If naturalized construction: mountain is false summit; engine reclassifies to tangled_rope or snare; reveals that the reading''s legitimacy derives from institutional authority, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization_mechanism, conceptual, 'Whether Article 4 criteria are natural law or naturalized contingency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gc_state_theater_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gc_state_theater_t40, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(gc_state_theater_t75, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 75, 0.35).

% Extraction over time
narrative_ontology:measurement(gc_state_extract_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gc_state_extract_t40, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(gc_state_extract_t75, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 75, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gc_state_suppress_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gc_state_suppress_t40, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(gc_state_suppress_t75, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerent_targeting_exclusion).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, article_4_criteria_application_asymmetric_conflict).

% DUAL FORMULATION NOTE:
% The state-centric reading is part of a three-reading constraint family decomposing the contested Geneva Conventions protective scope kernel. Each reading has distinct ε (state-centric: 0.52; hybrid-proportionality: 0.58; universal-rights: expected 0.65+) because each reading produces different victim sets and different extraction mechanisms. The family is linked via network.affects_constraints. The state-centric reading is the baseline institutional position; the hybrid and universal readings layer additional protections and constraints, making them downstream in the norm evolution sequence. However, in operational military practice, hybrid-proportionality is increasingly the de facto operative constraint, rendering state-centric partially piton (performative).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
