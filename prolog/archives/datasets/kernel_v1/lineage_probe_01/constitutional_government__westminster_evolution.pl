% ============================================================================
% CONSTRAINT STORY: constitutional_government__westminster_evolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_government__westminster_evolution, []).

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
 *   constraint_id: constitutional_government__westminster_evolution
 *   human_readable: Constitutional Government as Westminster Evolution (Unwritten Accumulation)
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Westminster reading of constitutional government treats the
 *   constitution as an evolving set of binding conventions, statutes, and
 *   precedents that accumulate through practice rather than through
 *   deliberate founding acts or written instruments. Constitutional
 *   government is whatever the constitutional actors (Parliament, the Crown,
 *   the judiciary, established institutions) treat as binding. This reading
 *   instantiates one of four competing interpretations of the constitutional
 *   kernel. The Westminster evolution reading differs from ancient
 *   constitutionalism (which locates constitutional constraint in the balance
 *   of social orders), postwar constitutionalism (which grounds legitimacy in
 *   formal rights-protections against tyranny), and revolutionary
 *   constitutionalism (which invokes a founding moment and written
 *   supremacy). The Westminster reading is characterized by low formal
 *   suppression (the conventions are not written; no formal gate prevents
 *   change) but high conventional suppression (breach is unthinkable before
 *   it is illegal because deviation from established practice is experienced
 *   as madness, not rule-breaking, by those socialized into the tradition).
 *   Beneficiaries are incumbent institutional actors who have internalized
 *   the conventions and use them to navigate governance. Victims include
 *   outsiders to the tradition, would-be reformers, and excluded publics who
 *   lack the tacit knowledge required to operate within the constitutional
 *   order.
 *
 * KEY AGENTS:
 *   - Incumbent Constitutional Actors (institutional/arbitrage): Parliament, Crown, judiciary, senior civil service — benefit from flexibility of unwritten norms; experience the constraint as coordination enabling smooth adjustment
 *   - Constitutional Tradition Keepers (institutional/arbitrage): Academics, legal commentators, constitutional historians who transmit and interpret the conventions; serve as custodians of unwritten knowledge
 *   - Constitutional Reformers (moderate/constrained): Political movements seeking structural change; benefit from flexibility but face suppression through conventional resistance
 *   - Outsiders to Convention (powerless/trapped): Citizens, new majorities, marginalized groups lacking tacit knowledge of how the constitution operates; experience maximum extraction through epistemic exclusion
 *   - Reform Coalitions (organized/constrained): Parliament-based or civil-society movements pushing for constitutional codification or formal amendment; face institutional inertia
 *   - Formal Legal System (institutional/arbitrage): Courts, statutory bodies, legal professions; maintain performative binding while treating the constitution as discovered rather than created
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_government__westminster_evolution, 0.32).
domain_priors:suppression_score(constitutional_government__westminster_evolution, 0.48).
domain_priors:theater_ratio(constitutional_government__westminster_evolution, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_government__westminster_evolution, extractiveness, 0.32).
narrative_ontology:constraint_metric(constitutional_government__westminster_evolution, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_government__westminster_evolution, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_government__westminster_evolution, tangled_rope).
narrative_ontology:human_readable(constitutional_government__westminster_evolution, "Constitutional Government as Westminster Evolution (Unwritten Accumulation)").
narrative_ontology:topic_domain(constitutional_government__westminster_evolution, "political/legal/constitutional").

domain_priors:requires_active_enforcement(constitutional_government__westminster_evolution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_government__westminster_evolution, '50929709-08b5-449b-8eae-cebf51ea8215').
narrative_ontology:cs_kernel_codification('50929709-08b5-449b-8eae-cebf51ea8215', implicit).
narrative_ontology:cs_authority_grounding('50929709-08b5-449b-8eae-cebf51ea8215', lineage).
narrative_ontology:cs_interpretation_layer_present('50929709-08b5-449b-8eae-cebf51ea8215').
narrative_ontology:cs_reading_relation('50929709-08b5-449b-8eae-cebf51ea8215', constitutional_government__ancient_constitutionalism, coexists_with).
narrative_ontology:cs_reading_relation('50929709-08b5-449b-8eae-cebf51ea8215', constitutional_government__postwar_constitutionalism, influences).
narrative_ontology:cs_reading_relation('50929709-08b5-449b-8eae-cebf51ea8215', constitutional_government__revolutionary_constitutionalism, coexists_with).
narrative_ontology:cs_axiom('50929709-08b5-449b-8eae-cebf51ea8215', foundational, constitution_is_what_actors_treat_as_binding).
narrative_ontology:cs_axiom_status(constitution_is_what_actors_treat_as_binding, holdable).
narrative_ontology:cs_axiom_grounding('50929709-08b5-449b-8eae-cebf51ea8215', constitution_is_what_actors_treat_as_binding, conventional).
narrative_ontology:cs_axiom('50929709-08b5-449b-8eae-cebf51ea8215', foundational, convention_precedes_and_conditions_statute).
narrative_ontology:cs_axiom_status(convention_precedes_and_conditions_statute, holdable).
narrative_ontology:cs_axiom_grounding('50929709-08b5-449b-8eae-cebf51ea8215', convention_precedes_and_conditions_statute, conventional).
narrative_ontology:cs_reference_frame('50929709-08b5-449b-8eae-cebf51ea8215', liberal_parliamentary_precedent_based_authority).
narrative_ontology:cs_drift_state('50929709-08b5-449b-8eae-cebf51ea8215', contemporary_codification_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('50929709-08b5-449b-8eae-cebf51ea8215', '').
narrative_ontology:cs_kernel_id(constitutional_government__westminster_evolution, constitutional_government).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_government__westminster_evolution, incumbent_institutional_actors).
narrative_ontology:constraint_beneficiary(constitutional_government__westminster_evolution, constitutional_tradition_keepers).
narrative_ontology:constraint_victim(constitutional_government__westminster_evolution, outsiders_to_convention).
narrative_ontology:constraint_victim(constitutional_government__westminster_evolution, constitutional_reformers).
narrative_ontology:constraint_victim(constitutional_government__westminster_evolution, excluded_publics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% An actor who lacks knowledge of the unwritten conventions faces maximum extraction without knowing they are trapped. The constitution is whatever the insiders treat as binding — and the outsider has no access to that knowledge until breach renders them guilty. Suppression operates through epistemic exclusion: the binding norms are tacit, transmitted through elite socialization, not written. The outsider experiences the constraint as snare because they cannot perceive what binds them.
constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Those who have been socialized into the conventions experience the constraint as coordination. The unwritten constitution enables flexibility — actors can adjust without formal amendment. Breach is unthinkable before it is illegal because the conventions are so deeply internalized that violation appears as madness rather than rule-breaking. The beneficiary sees this as smooth governance, not extraction.
constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% An actor attempting to change the constitution faces both coordination benefits (the flexibility of unwritten norms enables incremental reform) and asymmetric extraction (the burden of proof that a norm has changed falls on the reformer; the status quo is assumed binding until overwhelming elite consensus shifts). Extractiveness is moderate because reform is possible but costly — it requires converting the incumbents.
constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Organized actors (parliament, civil society, new majorities) seeking structural constitutional change benefit from the flexibility of unwritten norms (no formal amendment gates required) but face suppression through conventional resistance. The established actors treat breach of convention as illegitimate even when statutes permit it. The coalition experiences extraction through institutional inertia — the weight of 'how we do things' — but retains agency through persistent pressure.
constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From the perspective of the tradition as a distributed institution, the unwritten constitution is a pure coordination mechanism. It solves the problem of how to preserve legitimacy while enabling gradual adaptation without formal rupture. The tradition benefits from this mechanism — it perpetuates itself through incremental adjustment rather than revolutionary replacement. Extractiveness is nearly zero because the mechanism serves the tradition's core function.
constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The formal judiciary and legislative apparatus treat the unwritten constitution as performatively binding while acknowledging it has no written form. The courts apply precedent and statutory interpretation as though they were discovering pre-existing constitutional rules. This is largely theatrical — the constitution is whatever the institutional actors treat as binding, but the judicial system maintains the appearance of discovering transcendent law rather than creating it through practice. Theater ratio reflects this performative quality.
constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a universal, civilizational perspective, this constraint might appear as a natural law: any political system must have some operative constitution (some set of binding norms that govern power), and that constitution will necessarily be shaped by accumulated practice and convention rather than pure written form. The analytical observer risks naturalizing what is actually a specific reading of the constitutional kernel — treating Westminster evolution as the only coherent form.
constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_government__westminster_evolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_government__westminster_evolution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_government__westminster_evolution, TR),
    TR >= 0.70.

:- end_tests(constitutional_government__westminster_evolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Low to moderate. The Westminster evolution reading produces low formal suppression (anyone can attempt to change a convention) but high conventional suppression (breach is unthinkable before it is illegal). This produces moderate extractiveness because the barrier to change is real but operates through internalization rather than force. Beneficiaries gain advantage through knowledge of unstated rules, not through coercive mechanisms. The extractiveness is procedurally veiled — it appears as 'how things are done' rather than as conscious extraction. Suppression (0.48): Moderate-high. Low formal suppression (conventions have no written enforcement mechanism) is offset by high conventional suppression (deviation triggers institutional resistance, loss of legitimacy, social sanction from the tradition-keeper community). The constraint operates through what is unthinkable rather than through what is forbidden. Theater ratio (0.55): Moderate. The formal legal system performs the discovery of constitutional meaning while it actually creates meaning through practice. Courts apply precedent as though recovering pre-existing law rather than making it. This performative quality accounts for the theater: the system maintains an appearance of constraint-by-law while operating through constraint-by-convention. Claimed type (tangled_rope): Satisfies the gate. Beneficiaries present (incumbent actors); victims present (reformers, outsiders); requires_active_enforcement = true (the tradition must be continuously maintained through socialization and institutional pressure). The constraint coordinates smooth governance (genuine coordination function) while extracting advantage for insiders (asymmetric benefit distribution).
 *
 * PERSPECTIVAL GAP:
 *   The outsider and the incumbent experience radically different constraints. The outsider faces snare: they cannot know what binds them until breach renders them guilty. The incumbent faces rope: they experience the same norms as enabling coordination and flexibility. The reformer faces tangled rope: the norms enable gradual change but suppress rapid transformation. The formal legal system performs piton: it treats its role as discovering law, but courts are actually creating law through reinterpretation while maintaining the appearance of discovering pre-existing constitutional rules. The analytical observer risks mountain classification (treating evolution as necessary to any constitution) but the structural data reveals this as false-summit: Westminster constitutionalism is a specific reading of the kernel, not a law of nature. Alternative readings (revolutionary, postwar, ancient) remain live in different parts of the constitutional order (e.g., Scottish independence movements invoke revolutionary legitimacy; human rights courts invoke postwar frameworks).
 *
 * DIRECTIONALITY LOGIC:
 *   The power-derived directionality of incumbent institutional actors is low-to-negative (they benefit from the constraint and have arbitrage options) while the powerless outsider has high directionality (they bear costs and are trapped). The moderate reformer has mid-range directionality (they have constrained exit options — reform is possible but costly). The organized coalition has slightly lower directionality than its constituents (organization provides agency but does not eliminate suppression). The engaged analytical observer has canonical directionality for the analytical power atom (0.73) because they stand outside the constraint's structure. The derivation chain produces these values from beneficiary/victim declarations plus exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convention_vs_statutory_precedence,
    'When statute and convention conflict, which is binding? Is the statute supreme, or do conventions trump statute through reinterpretation?',
    'Case law analysis: instances where parliament enacted statute that violated established convention; track whether courts enforced the statute literally or reinterpreted it to preserve convention. Historical examples: the Succession to the Crown Act vs. the convention of ministerial accountability.',
    'If statute is supreme: the constraint is less extractive (written law provides clarity and limits incumbent advantage). If convention is supreme: the constraint is more extractive (written norms become subordinate to tacit ones; institutional actors can claim statute ''always meant'' the conventional interpretation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convention_vs_statutory_precedence, empirical, 'Whether convention or statute has precedence when they conflict').

omega_variable(
    reading_distinction_vs_natural_law,
    'Is Westminster constitutionalism a contingent reading of the constitutional kernel, or a necessary feature of any viable constitution?',
    'Comparative constitutional analysis: examine whether other polities achieve constitutional stability through written, formally-amended instruments. If yes: Westminster evolution is a reading. If no: it may approach natural law status.',
    'If reading: the constraint is fully subject to mandatrophy analysis — it can be displaced by alternative readings (revolutionary, postwar, ancient). If natural law: the mountain classification becomes defensible rather than false-summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_distinction_vs_natural_law, conceptual, 'Whether Westminster evolution is a contingent reading or structural necessity').

omega_variable(
    convention_knowledge_distribution,
    'How much of the suppression in this constraint derives from objective difficulty (conventions are genuinely hard to codify) versus from deliberate gatekeeping (incumbents benefit from exclusive knowledge)?',
    'Track reform movements that sought to write down the unwritten constitution (Levellers, Chartists, modern reform commissions). If they succeeded in codifying: suppression was partly gatekeeping. If codification attempts consistently failed despite sustained effort: suppression is partly structural.',
    'If gatekeeping: beneficiaries are deliberately maintaining epistemic exclusion; victim group has stronger moral claims to access; extractiveness may be higher than measured. If structural: the constraint may be less intentionally extractive; reforms would require genuine innovation, not mere inscription.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convention_knowledge_distribution, empirical, 'Whether suppression is structural or deliberately gatekept').

omega_variable(
    westminster_vs_sibling_foreclosure,
    'Does the Westminster evolution reading logically foreclose any of its sibling readings (ancient, postwar, revolutionary), or do they coexist as competing commitments held by different factions?',
    'Examine contemporary constitutional debates: Can a modern polity hold Westminster evolution as its operative norm while some actors invoke revolutionary legitimacy (e.g., Scottish independence referenda invoking popular sovereignty) or postwar human rights frameworks? If both can be invoked credibly in the same legal system: coexistence. If one crowd''s invocation delegitimizes the other: foreclosure.',
    'If foreclosure: one reading has consolidated authority; the constraint''s beneficiary class is larger and more unified. If coexistence: the constraint is contested; extractiveness is lower because multiple legitimacy claims are in play.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(westminster_vs_sibling_foreclosure, conceptual, 'Whether Westminster evolution forecloses or coexists with rival readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_government__westminster_evolution, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_westm_tr_t0, constitutional_government__westminster_evolution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(const_westm_tr_t100, constitutional_government__westminster_evolution, theater_ratio, 100, 0.52).
narrative_ontology:measurement(const_westm_tr_t200, constitutional_government__westminster_evolution, theater_ratio, 200, 0.55).

% Extraction over time
narrative_ontology:measurement(const_westm_be_t0, constitutional_government__westminster_evolution, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(const_westm_be_t100, constitutional_government__westminster_evolution, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(const_westm_be_t200, constitutional_government__westminster_evolution, base_extractiveness, 200, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(const_westm_su_t0, constitutional_government__westminster_evolution, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(const_westm_su_t100, constitutional_government__westminster_evolution, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(const_westm_su_t200, constitutional_government__westminster_evolution, suppression_requirement, 200, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_government__westminster_evolution, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_government__westminster_evolution, 0.12).
narrative_ontology:affects_constraint(constitutional_government__westminster_evolution, constitutional_government__ancient_constitutionalism).
narrative_ontology:affects_constraint(constitutional_government__westminster_evolution, constitutional_government__postwar_constitutionalism).
narrative_ontology:affects_constraint(constitutional_government__westminster_evolution, constitutional_government__revolutionary_constitutionalism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional government kernel. The other readings (ancient, postwar, revolutionary) are structurally distinct constraints with different ε values and beneficiary/victim profiles. All four stories share the same kernel_id but have different reading_ids. The readings coexist in the contemporary constitutional order — different actors invoke different readings to legitimize different claims. Mapping between readings: Westminster reading influences the postwar reading (postwar human rights frameworks must be integrated into Westminster procedure), and is influenced by revolutionary reading (Scottish independence movements invoke founding legitimacy that Westminster must accommodate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
