% ============================================================================
% CONSTRAINT STORY: symbolic_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbolic_continuity_reading, []).

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
 *   constraint_id: symbolic_continuity_reading
 *   human_readable: Ritual as Identity-Marker Preservation and Mourning Practice (Symbolic Continuity Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the symbolic_continuity_reading of the
 *   contested kernel catastrophe_memory_transmission. The reading asserts
 *   that mourning-practice in diaspora communities serves primarily as an
 *   identity-marker and that the practice itself (ritual form, symbolic
 *   precision, textual fidelity) IS the preserved substrate.
 *   Survival-competence (the community's adaptive capacity to function in
 *   diaspora context) is treated as an incidental byproduct of identity
 *   preservation, not as the organizing principle. The reading is upheld by
 *   ritual authority institutions that maintain interpretive control through
 *   fidelity requirements. It is contested by the adaptive_competence_reading
 *   (which asserts that functional survival is primary and form should adapt)
 *   and the hybrid_resilience_reading (which asserts both matter equally).
 *   The symbolic_continuity_reading produces a constraint story where ritual
 *   authority appears as beneficiary and enforcer, mourning practitioners
 *   appear as victims of extraction, and the modernizing youth appear as
 *   trapped between identity loyalty and adaptive necessity. The constraint's
 *   extractiveness has increased from 0.35 to 0.63 over the observation
 *   interval (0-75 years), reflecting intensifying enforcement as
 *   assimilation pressures rise and communities demand modification.
 *
 * KEY AGENTS:
 *   - Diaspora Mourner: Primary victim (powerless/identity_locked) — identity constituted through exact ritual performance; cannot modify without identity dissolution
 *   - Economically Marginalized Practitioner: Secondary victim (powerless/trapped) — bears material extraction (cost of materials, time, travel); structurally immobile
 *   - Modernizing Youth: Contested agent (moderate/constrained) — seeks to preserve identity-marker while adapting practice; experiences tension between coordination function and extractive enforcement
 *   - Ritual Authority Class: Primary beneficiary (institutional/arbitrage) — preserves interpretive monopoly through fidelity requirement; collects legitimacy, resources, and intellectual authority
 *   - Tradition Guardianship Institution: Institutional actor (institutional/constrained) — maintains fidelity doctrine through scholarly apparatus and policy enforcement; theater ratio suggests defensive maintenance rather than functional necessity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — at risk of naturalizing contingent institutional arrangement as immutable law of symbolic transmission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbolic_continuity_reading, 0.58).
domain_priors:suppression_score(symbolic_continuity_reading, 0.62).
domain_priors:theater_ratio(symbolic_continuity_reading, 0.74).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbolic_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(symbolic_continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(symbolic_continuity_reading, theater_ratio, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbolic_continuity_reading, tangled_rope).
narrative_ontology:human_readable(symbolic_continuity_reading, "Ritual as Identity-Marker Preservation and Mourning Practice (Symbolic Continuity Reading)").
narrative_ontology:topic_domain(symbolic_continuity_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(symbolic_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbolic_continuity_reading, 'c58f417c-b88e-4bd0-985e-58459548e2c5').
narrative_ontology:cs_kernel_codification('c58f417c-b88e-4bd0-985e-58459548e2c5', fixed_text).
narrative_ontology:cs_authority_grounding('c58f417c-b88e-4bd0-985e-58459548e2c5', extraction).
narrative_ontology:cs_interpretation_layer_present('c58f417c-b88e-4bd0-985e-58459548e2c5').
narrative_ontology:cs_reading_relation('c58f417c-b88e-4bd0-985e-58459548e2c5', symbolic_continuity_reading__adaptive_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c58f417c-b88e-4bd0-985e-58459548e2c5', symbolic_continuity_reading__hybrid_resilience_reading, coexists_with).
narrative_ontology:cs_axiom('c58f417c-b88e-4bd0-985e-58459548e2c5', foundational, mourning_practice_form_irreducible).
narrative_ontology:cs_axiom_status(mourning_practice_form_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('c58f417c-b88e-4bd0-985e-58459548e2c5', mourning_practice_form_irreducible, deontological).
narrative_ontology:cs_axiom('c58f417c-b88e-4bd0-985e-58459548e2c5', secondary, ritual_fidelity_grounds_identity).
narrative_ontology:cs_axiom_status(ritual_fidelity_grounds_identity, holdable).
narrative_ontology:cs_axiom_grounding('c58f417c-b88e-4bd0-985e-58459548e2c5', ritual_fidelity_grounds_identity, deontological).
narrative_ontology:cs_reference_frame('c58f417c-b88e-4bd0-985e-58459548e2c5', ancestral_mourning_form_transmission).
narrative_ontology:cs_drift_state('c58f417c-b88e-4bd0-985e-58459548e2c5', contemporary_diaspora_assimilation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c58f417c-b88e-4bd0-985e-58459548e2c5', '').
narrative_ontology:cs_kernel_id(symbolic_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbolic_continuity_reading, ritual_authority_class).
narrative_ontology:constraint_beneficiary(symbolic_continuity_reading, interpretive_tradition_lineage).
narrative_ontology:constraint_victim(symbolic_continuity_reading, diaspora_communities_under_assimilation).
narrative_ontology:constraint_victim(symbolic_continuity_reading, economically_marginalized_practitioners).
narrative_ontology:constraint_victim(symbolic_continuity_reading, youth_seeking_adaptive_modification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA MOURNER (SNARE) — Identity fused with ancestral mourning practice; cannot modify ritual without experiencing identity dissolution. Structurally mobile (could relocate, switch communities) but identity-locked by multigenerational commitment to exact ceremonial form. Bears full extraction cost: resource burden, time commitment, social isolation in secular environment. No exit without abandoning identity frame.
constraint_indexing:constraint_classification(symbolic_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: ECONOMICALLY MARGINALIZED PRACTITIONER (SNARE) — Trapped by both material barriers and identity. Ritual requirement (expensive materials, time off work, travel to ritual site) extracts disproportionately. Cannot afford modified or streamlined versions. Cannot exit through assimilation because identity is too tightly bound. Experiences maximum extraction across material and cognitive dimensions.
constraint_indexing:constraint_classification(symbolic_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MODERNIZING YOUTH (TANGLED ROPE) — Seeks to preserve ritual identity-marker while adapting practice to contemporary context. Constrained exit: breaking from tradition carries family rupture and identity cost; staying costs cognitive dissonance. Experiences genuine coordination function (ritual does anchor identity) alongside asymmetric extraction (must bear cost of performing practices that feel alienating). Some agency — can advocate for modification — but enforcement machinery resists adaptation.
constraint_indexing:constraint_classification(symbolic_continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: RITUAL AUTHORITY CLASS (ROPE) — Preserves interpretive control through fidelity requirement. Experiences the constraint as pure coordination: maintaining exact ritual form ensures transmission of identity-marker and maintains authority's interpretive monopoly. Benefits from the constraint through institutional legitimacy, resource flows (donations, prestige), and intellectual capital (custodianship of tradition). Can exit (adopt modernized form) but chooses not to because arbitrage value is high. Sees enforcement as necessary coordination maintenance.
constraint_indexing:constraint_classification(symbolic_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITION GUARDIANSHIP INSTITUTION (PITON) — Maintains fidelity requirement through scholarly authority and institutional policy, but the functional substrate (mourning practice as psychological-social anchor) has atrophied relative to the performative maintenance (ritual textual authenticity, symbolic purity doctrine). Guardianship institution may not acknowledge that the original functional problem (rapid memory loss in oral-tradition societies) is solved by written records, photography, and distributed digital archives. Enforcement machinery persists through institutional inertia — theater ratio (0.74) suggests much activity is defensive maintenance of the interpretive monopoly rather than functional preservation.
constraint_indexing:constraint_classification(symbolic_continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From civilizational scale, the constraint appears as immutable: mourning-practice is inherently bound to ritual form; identity-marker preservation requires fidelity; symbolic continuity demands exact transmission. This perspective risks naturalizing what is a contingent institutional arrangement. The engine's false summit detector will identify this as a false summit because identifiable beneficiaries exist (ritual authority class) and the structural data shows extractiveness and suppression — these contradict genuine natural law. The 'immutable law' framing serves the beneficiaries.
constraint_indexing:constraint_classification(symbolic_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbolic_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbolic_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbolic_continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(symbolic_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(symbolic_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(symbolic_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from powerless and identity-locked agents (diaspora mourners) by requiring resource expenditure, time commitment, and identity fusion. The extraction is asymmetric: ritual authority benefits from maintained interpretive control and institutional prestige while mourners bear costs. The trajectory from 0.35 to 0.63 reflects rising extraction as diaspora pressure increases — communities demand modification but enforcement machinery tightens. Suppression (0.62): High. Multiple suppression mechanisms operate: (1) identity fusion makes exit psychologically unthinkable; (2) institutional enforcement through ritual authority denies legitimacy to modified forms; (3) social pressure from co-practitioners; (4) economic barriers to participation that make exit appear as community abandonment. Theater ratio (0.74): High and rising (0.55→0.74). This reflects that much enforcement activity is defensive maintenance of the fidelity doctrine rather than functional mourning practice. Written records, photography, and digital archives have solved the functional problem (catastrophe memory preservation that motivated original fidelity requirement in oral-tradition contexts). Enforcement machinery now primarily maintains the institutional authority's interpretive monopoly and symbolic purity doctrine. Rising theater ratio suggests increasing theater as the functional necessity declines.
 *
 * PERSPECTIVAL GAP:
 *   The deepest gap emerges between the ritual authority (Rope) and the diaspora mourner (Snare). The authority genuinely experiences coordination (the ritual form does encode identity and cultural memory). The mourner genuinely experiences extraction (the fidelity requirement is enforced against adaptive pressure). Both experiences are structurally real — the constraint simultaneously coordinates and extracts. This is the definition of Tangled Rope. The modernizing youth perspective confirms this: genuine coordination function + asymmetric extraction = Tangled Rope. The piton perspective (institutional guardianship) reveals the mechanism: the functional problem (memory loss in oral tradition) is solved by external means (writing, archives, digital), but the enforcement machinery persists through institutional inertia and doctrinal commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity_locked exit option appears in the diaspora mourner perspective because the agent's identity is constituted through ritual participation. They face high material barriers to exit (family rupture, social isolation, identity dissolution) but the binding mechanism is cognitive — the mourner cannot imagine themselves outside the ritual frame. This is identity_locked, not trapped. The distinction matters: a trapped agent (economically marginalized practitioner) could imagine adaptation if barriers were removed, but faces insurmountable material costs. An identity-locked agent could theoretically escape material barriers but cannot escape the identity frame that makes exit unthinkable from within.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the constraint (preserve catastrophe memory and identity-marker through exact ritual transmission) has outlived its primary functional problem. The functional problem — rapid memory loss in oral-tradition societies without writing — is solved by external means: written texts, photography, digital archives, institutional records. Yet the enforcement machinery persists through (1) institutional inertia — the guardianship institution maintains the fidelity doctrine because its authority depends on exclusive interpretive control; (2) identity fusion — diaspora communities have internalized the constraint and now experience modification as identity dissolution; (3) symbolic purity doctrine — the authority has elevated fidelity from functional requirement to intrinsic good. The constraint is not resolved mandatrophy (mandatrophy_resolved: false) because the assimilation pressure continues to drive enforcement intensification. The trajectory shows rising extractiveness and theater as the functional problem is solved externally but the constraint persists. Resolution would require either (a) institutional reform that decouples identity-marker from exact ritual form, or (b) collapse of the enforcement machinery as diaspora communities make unilateral adaptations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_mechanism,
    'Is the constraint''s binding force identity fusion (the mourner''s self-concept constituted through ritual fidelity) or external enforcement (institutional pressure to conform)?',
    'Post-exit longitudinal study: mourners who adapt or abandon ritual; measurement of psychological distress, identity continuity, and social reintegration. If distress persists after enforcement pressure is removed, binding is identity fusion; if distress resolves, binding was primarily external.',
    'If identity fusion: the constraint is far more stable and resistant to modification (classification Snare with identity_locked exit is correct). If external enforcement: the constraint is more brittle and vulnerable to institutional reform (classification shifts toward tangled_rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, empirical, 'Whether binding is identity fusion or external enforcement').

omega_variable(
    mourning_practice_functional_necessity,
    'Does mourning-practice serve irreducible psychological or community-integration functions that cannot be served by alternative expressions (secular ceremony, written memory, digital commemoration)?',
    'Comparative psychology of grief processing and community belonging across different memorial practices. Neuroscience of ritual participation in grief recovery. Ethnographic observation of diaspora communities that have adapted or abandoned traditional mourning while retaining identity-marker recognition.',
    'If mourning-practice is functionally necessary: the constraint coordinates genuine collective action (Rope from authority perspective is correct; Tangled Rope from youth perspective reflects legitimate tension). If alternative expressions serve the function equally: the constraint is cover story for extractive identity control (Snare classification is correct across more perspectives; authority''s Rope is false summit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mourning_practice_functional_necessity, empirical, 'Whether mourning-practice serves irreducible psychological functions').

omega_variable(
    kernel_reading_contest,
    'Which reading of the catastrophe_memory_transmission kernel is structurally correct: symbolic_continuity_reading (fidelity-centered), adaptive_competence_reading (function-centered), or hybrid_resilience_reading (both)?',
    'Comparative historical analysis of diaspora communities that survived catastrophe memory loss under different transmission strategies. Measurement of (identity preservation rate, adaptive survival capacity) for each strategy. Cross-reading evidence collection: catastrophe survival archives, migration pattern studies, trauma transmission pathways.',
    'This omega encodes the committer-frame ambiguity: the three readings coexist but are not fully compatible. The symbolic_continuity_reading asserts that fidelity IS the preservation function (axiom: mourning_practice_form_is_irreducible). The adaptive_competence_reading asserts that functional survival is primary and form is contingent (axiom: adaptive_modification_preserves_transmission). The hybrid_resilience_reading asserts both matter. If empirical evidence shows symbolic_continuity_reading falsifies adaptive_competence_reading (surviving communities systematically modified practice), the classification shifts to coexists_with rather than forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The committer-frame contest: which kernel reading captures the catastrophe transmission constraint?').

omega_variable(
    false_summit_natural_law,
    'Is mourning-practice fidelity a natural law of human symbolic transmission (Mountain), or a contingent institutional arrangement serving beneficiaries (Tangled Rope / Snare)?',
    'Comparative study of catastrophe memory systems across cultures without shared institutional authority: oral traditions, indigenous protocols, secular memorial movements. If fidelity-to-original-form reliably emerges as a spontaneous solution, natural law hypothesis gains support. If variation in form and successful transmission coexist, the ''natural law'' framing is revealed as institutional naturalization.',
    'If natural law: mountain classification is correct; beneficiaries are artifacts of the natural structure. If institutional: the beneficiary class (ritual authority) is the primary actor, and false summit detection applies — the engine reclassifies from mountain to tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, empirical, 'Whether fidelity requirement is natural law or institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbolic_continuity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symcont_theater_t0, symbolic_continuity_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(symcont_theater_t25, symbolic_continuity_reading, theater_ratio, 25, 0.64).
narrative_ontology:measurement(symcont_theater_t50, symbolic_continuity_reading, theater_ratio, 50, 0.71).
narrative_ontology:measurement(symcont_theater_t75, symbolic_continuity_reading, theater_ratio, 75, 0.74).

% Extraction over time
narrative_ontology:measurement(symcont_extract_t0, symbolic_continuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(symcont_extract_t25, symbolic_continuity_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(symcont_extract_t50, symbolic_continuity_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(symcont_extract_t75, symbolic_continuity_reading, base_extractiveness, 75, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(symcont_suppress_t0, symbolic_continuity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(symcont_suppress_t25, symbolic_continuity_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(symcont_suppress_t50, symbolic_continuity_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(symcont_suppress_t75, symbolic_continuity_reading, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbolic_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(symbolic_continuity_reading, 0.12).
narrative_ontology:affects_constraint(symbolic_continuity_reading, adaptive_competence_reading).
narrative_ontology:affects_constraint(symbolic_continuity_reading, hybrid_resilience_reading).
narrative_ontology:affects_constraint(symbolic_continuity_reading, diaspora_assimilation_pressure).
narrative_ontology:affects_constraint(symbolic_continuity_reading, institutional_authority_legitimacy).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_transmission kernel decomposes into three constraint stories representing three structurally distinct readings. Each reading has its own ε value (symbolic_continuity_reading ε=0.58; adaptive_competence_reading ε~0.35; hybrid_resilience_reading ε~0.42) because the observables differ: fidelity enforcement vs. survival capacity vs. both-weighted. These are not three perspectives on one constraint — they are three constraints arising from contested kernel interpretation. The symbolic_continuity_reading dominates in institutional contexts with strong authority control; the adaptive_competence_reading dominates in contexts with high assimilation pressure and weak institutional authority; the hybrid_resilience_reading emerges in negotiated contexts. All three coexist in diaspora communities globally — the constraint families are linked via affects_constraints to enable contamination propagation analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
