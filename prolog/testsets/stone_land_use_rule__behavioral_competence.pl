% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Land-Use Rule: Behavioral Competence Reading
 *   domain: disaster_anthropology/institutional_memory/land_governance
 *
 * SUMMARY:
 *   A stone marker placed by ancestors 78 years ago delineates a settlement
 *   boundary, marking the highest extent of historical flood risk. The
 *   constraint reads as follows in the behavioral_competence interpretation:
 *   the stone is a live behavioral rule maintained through daily spatial
 *   practice — when community members walk past it, work land near it, plan
 *   settlement expansion relative to it, they are reinforcing their
 *   understanding that this line marks a safety threshold. Compliance is
 *   sustained not through external enforcement but through internalized
 *   competence — the community 'reads' the stone as encoding ancestral
 *   knowledge about the flood zone. This reading contrasts with the
 *   commemorative_husk reading, in which the stone is a memorial artifact
 *   whose original functional meaning has decayed; compliance becomes
 *   performative adherence to 'what we've always done' without understanding
 *   the rationale. In the behavioral_competence reading, the constraint
 *   operates as a coordination mechanism (Rope) solving the collective action
 *   problem of maintaining settlement safety across generational transitions.
 *   The economic cost is real (steep hill climb to build on safe land,
 *   opportunity cost of lower-value land) but accepted because the
 *   coordination function is transparent and internalized.
 *
 * KEY AGENTS:
 *   - Community Practice Collective: Primary beneficiary (moderate/constrained/generational) — solves intergenerational coordination problem of maintaining settlement away from flood zones; benefits from collective knowledge preservation.
 *   - Individual Landowners: Secondary actors (powerful/mobile/biographical) — experience economic cost (cannot build on optimal land) but accept constraint because coordination rationale is transparent; could exit but choose not to.
 *   - Ancestral Knowledge Transmitters: Functional agents (no power atom; represented through intergenerational practice) — inscribed the rule through placement and through oral tradition; the constraint persists as their behavioral legacy.
 *   - Analytical Observer: Risk of false naturalization (analytical/analytical/civilizational) — tempted to see the stone as encoding immutable flood geography rather than as a contingent institutional arrangement reflecting ancestral choices.
 *   - Formal Authority (State/Municipal): Degraded institutional actor (institutional/constrained/biographical) — when authority tries to codify or enforce the rule through legal mechanisms, the enforcement becomes performative; theater ratio rises as formal documentation replaces embedded understanding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.18).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.35).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.18).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule: Behavioral Competence Reading").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/institutional_memory/land_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, '83a19fbe-c83c-468b-875f-68070aef5c48').
narrative_ontology:cs_kernel_codification('83a19fbe-c83c-468b-875f-68070aef5c48', fixed_text).
narrative_ontology:cs_authority_grounding('83a19fbe-c83c-468b-875f-68070aef5c48', practice).
narrative_ontology:cs_interpretation_layer_present('83a19fbe-c83c-468b-875f-68070aef5c48').
narrative_ontology:cs_reading_relation('83a19fbe-c83c-468b-875f-68070aef5c48', stone_land_use_rule__commemorative_husk, coexists_with).
narrative_ontology:cs_axiom('83a19fbe-c83c-468b-875f-68070aef5c48', foundational, stone_encodes_live_behavioral_rule).
narrative_ontology:cs_axiom_status(stone_encodes_live_behavioral_rule, holdable).
narrative_ontology:cs_axiom_grounding('83a19fbe-c83c-468b-875f-68070aef5c48', stone_encodes_live_behavioral_rule, conventional).
narrative_ontology:cs_axiom('83a19fbe-c83c-468b-875f-68070aef5c48', foundational, functional_understanding_enables_voluntary_compliance).
narrative_ontology:cs_axiom_status(functional_understanding_enables_voluntary_compliance, holdable).
narrative_ontology:cs_axiom_grounding('83a19fbe-c83c-468b-875f-68070aef5c48', functional_understanding_enables_voluntary_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('83a19fbe-c83c-468b-875f-68070aef5c48', ancestral_settlement_safety_knowledge).
narrative_ontology:cs_drift_state('83a19fbe-c83c-468b-875f-68070aef5c48', administrative_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83a19fbe-c83c-468b-875f-68070aef5c48', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, community_survival_collective).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITY PRACTICE (ROPE) — Local community members experience the stone constraint as functional coordination across generations. The steep hill climb is a cost they accept because it encodes collective knowledge about flood risk and settlement safety. Exit is constrained by the fact that violating the rule risks catastrophic loss, but the constraint itself provides genuine coordination benefit — it solves a collective action problem (maintaining settlement away from danger zones) with minimal coercive overhead. The constraint persists because compliance is internally reinforced through lived understanding of the rule's rationale.
constraint_indexing:constraint_classification(stone_land_use_rule__behavioral_competence, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: LANDOWNER (ROPE) — Individual with land above or below the stone line experiences the constraint as a coordination mechanism with economic costs. The landowner has exit capacity (can move, can lobby for boundary change, can ignore the prohibition) but chooses compliance because the coordination function — maintaining the settlement's collective safety — is internalized. The constraint is experienced as binding but not coercive. Low effective extraction because compliance flows from understanding, not from being trapped or suppressed.
constraint_indexing:constraint_classification(stone_land_use_rule__behavioral_competence, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the stone line can appear as encoding a natural law — the immutable geography of flood risk, the unchangeable physics of water movement, the irreducible constraint of settlement on alluvial plains. This perspective risks falsely naturalizing what is actually a contingent institutional arrangement: the specific elevation was chosen by ancestors, the rule persists through practice, the interpretation of where the stone marks depends on reading oral tradition. The analytical observer is tempted to see the constraint as a mountain rather than as a rope, thereby naturalizing the behavioral mechanism as if it were a physical law.
constraint_indexing:constraint_classification(stone_land_use_rule__behavioral_competence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: FORMAL AUTHORITY / ADMINISTRATIVE AGENT (PITON) — When state or municipal authority attempts to codify or enforce the stone rule through legal mechanisms, the enforcement mechanism itself often becomes performative. The formal authority (surveyor, official, bureaucrat) treats the stone as an artifact to be documented but may lack the embedded knowledge of why the line was drawn there. Theater ratio rises — the enforcement ritual (official inspection, boundary marking, regulatory filing) persists while the functional understanding decays. The institutional actor experiences the constraint as inertial — maintained because changing it requires more effort than maintaining it, not because the enforcement mechanism works.
constraint_indexing:constraint_classification(stone_land_use_rule__behavioral_competence, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, TR),
    TR >= 0.70.

:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. This is the signature of pure coordination (Rope) without significant extraction. The community benefits from the settlement safety coordination; landowners accept economic cost voluntarily because the rationale is transparent and understood. No agent extracts rent or asymmetrically benefits — the constraint solves a shared problem. The low value reflects that compliance flows from comprehension, not coercion or incentive misalignment. Suppression (0.35): Moderate. The constraint suppresses one category of action (building below the stone line) but not through absolute prohibition — the suppression is conditional on understanding the rationale. This is lower than snare or tangled_rope suppression (which relies on opacity or coercion) because the behavioral competence reading maintains that agents understand why the line exists. However, suppression is above Rope baseline because violating the rule is costly (social sanction, risk of loss, resource waste). Theater ratio (0.22): Low. The behavioral_competence reading posits minimal theater — the stone and daily spatial practice constitute genuine functional communication of the rule. Theater is low because compliance is reinforced through lived understanding, not through performative ritual. Theater ratio rises over time (0.10 → 0.22) as formal documentation and administrative enforcement begin to replace embedded knowledge, gradually shifting the mechanism toward piton. The 78-year interval reflects the actual historical span: placement at origin (t=0) through contemporary period (t=78 years), tracking how the constraint evolves as generational transmission faces pressure from urbanization and administrative codification.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between the behavioral_competence reading (Rope: functional coordination maintained through understanding) and the risk of false naturalization (Mountain: the stone encodes immutable flood physics rather than ancestral choice). The gap is closed by recognizing that the analytical observer's mountain classification is a false summit — the constraint is not a natural law but a living institutional arrangement that depends on continued transmission of functional understanding. The piton perspective (degraded enforcement when formal authority codifies the rule) shows the constraint's vulnerability: as administrative documentation replaces embedded knowledge, the mechanism shifts toward performative maintenance. The landowner perspective (Rope at biographical scale) shows that even those who bear economic cost accept the constraint voluntarily because they understand its rationale.
 *
 * DIRECTIONALITY LOGIC:
 *   The community practice agent is a beneficiary of the coordination mechanism (low d), but the beneficiary status does not create extraction because the rationale is transparent. The constraint operates at the biographical time horizon for individual landowners (who experience the economic cost directly) but at the generational horizon for the community practice collective (who benefits from intergenerational transmission of safety knowledge). At generational scale, the constraint is experienced as Rope (pure coordination); at biographical scale with individual landowners, it remains Rope but with higher-felt constraint due to opportunity cost. The analytical observer's perspective is included as a mountain to diagnose the false summit risk — the temptation to naturalize contingent institutional arrangements as immutable laws. The formal authority's piton perspective reflects the degradation of the constraint as administrative enforcement replaces embedded understanding.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oral_tradition_transmission_fidelity,
    'Does the behavioral competence (understanding why the stone marks a flood boundary) transmit reliably across generational transitions, or does knowledge decay into pure adherence to a formal rule?',
    'Interview and observation studies tracking comprehension of the rule across age cohorts; comparison of rationales given by different generations; frequency of rule-following that persists after the original justification is forgotten.',
    'If transmission fidelity remains high: behavioral_competence reading is sustained (Rope). If comprehension decays: constraint drifts toward piton or commemorative_husk reading (performative maintenance without understanding).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oral_tradition_transmission_fidelity, empirical, 'Intergenerational fidelity of functional understanding vs. rule adherence').

omega_variable(
    flood_event_recency_and_salience,
    'How does proximity to actual flood events (witnessed or recent collective memory) affect compliance and understanding? Does the constraint depend on experiential reinforcement or does it persist on institutional inertia alone?',
    'Longitudinal comparison of compliance and stated understanding in communities with recent flood events vs. those with > 50 year event gaps; analysis of compliance trajectory relative to time since last significant flood.',
    'If compliance is reinforced by recent events: behavioral_competence persists and ε stays low. If compliance persists despite event rarity: institutional inertia is the binding mechanism, suggesting drift toward piton or commemorative_husk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flood_event_recency_and_salience, empirical, 'Whether behavioral competence depends on recent experiential reinforcement').

omega_variable(
    alternative_enforcement_mechanism_sufficiency,
    'If the physical stone marker were removed or destroyed, would compliance persist through institutional knowledge, or would the constraint rapidly degrade?',
    'Natural experiment from stone loss events; interviews about counterfactual scenarios; comparison of compliance in communities with intact vs. damaged physical markers.',
    'If compliance persists after marker loss: behavioral_competence is internalized (Rope stable). If compliance collapses: the stone itself is the binding mechanism, not the rule''s rationale; constraint is performative or dependent on physical artifact, suggesting piton or commemorative_husk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_enforcement_mechanism_sufficiency, empirical, 'Whether behavioral competence persists independent of physical marker').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Do the behavioral_competence and commemorative_husk readings represent genuinely alternative frameworks that could coexist in different communities, or does one reading logically foreclose the other?',
    'Ethnographic documentation of how different communities describe and justify adherence to the same stone rule; analysis of whether both understandings are internally coherent or mutually exclusive.',
    'If coexistent: both readings are live and valid in different contexts (sibling relation: coexists_with). If foreclosing: one reading''s core premise negates the other''s (sibling relation: forecloses). This affects how the engine models uncertainty about which reading applies to a given community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Logical relationship between behavioral_competence and commemorative_husk readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stone_beh_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stone_beh_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.18).
narrative_ontology:measurement(stone_beh_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.22).

% Extraction over time
narrative_ontology:measurement(stone_beh_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(stone_beh_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.17).
narrative_ontology:measurement(stone_beh_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(stone_beh_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(stone_beh_su_t26, stone_land_use_rule__behavioral_competence, suppression_requirement, 26, 0.3).
narrative_ontology:measurement(stone_beh_su_t52, stone_land_use_rule__behavioral_competence, suppression_requirement, 52, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, resource_allocation).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% The behavioral_competence reading (this file) and commemorative_husk reading are two instantiations of the same physical artifact — a stone marker that delineates settlement boundary. They share identical base constraints (the same stone exists, marks the same line) but differ fundamentally in how the constraint operates: behavioral_competence reads the stone as a live rule sustained through functional understanding (ε=0.18, Rope); commemorative_husk reads it as a memorial whose meaning has decayed (higher ε, Piton or degraded constraint). These are distinct constraints with different epsilon values, different social mechanisms, and different classification profiles. They are linked because the same physical stone can instantiate either reading depending on whether intergenerational transmission of functional understanding persists or decays. Temporal measurement in this file (theater_ratio rising from 0.10 to 0.22) tracks the constraint's drift toward commemorative_husk as administrative documentation begins to replace embedded knowledge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
