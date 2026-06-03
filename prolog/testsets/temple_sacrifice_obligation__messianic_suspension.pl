% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation (Messianic Suspension Reading)
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   The messianic suspension reading interprets the Temple sacrifice
 *   obligation as deferred rather than nullified or operatively fulfilled.
 *   When the Temple was destroyed (70 CE), Jewish law faced a fundamental
 *   problem: a divinely commanded obligation that was structurally impossible
 *   to perform. Three halakhic readings emerged to resolve this: (1)
 *   Messianic Suspension: the obligation is suspended in legal status—neither
 *   fulfilled nor violated, pending restoration of the Temple; (2) Study as
 *   Archiving: study of sacrifice law preserves knowledge for future
 *   restoration but explicitly does not fulfill the obligation; (3) Study as
 *   Occupation: study of sacrifice law constitutes a legitimate alternative
 *   fulfillment of the obligation in the Temple's absence. This constraint
 *   story instantiates the first reading: suspension. The obligation's
 *   normative content persists—it is not nullified—but its enforcement is
 *   deferred to an eschatological event. Rabbinics maintain interpretive
 *   authority over the obligation while acknowledging that its fulfillment
 *   lies outside their jurisdiction. This reading produces very low
 *   extractiveness (ε ≈ 0.08) because no current obligation is imposed, no
 *   victim set exists, and no resource flows from the suspended state. The
 *   authority structure (rabbinic Judaism) benefits from the framework by
 *   preserving interpretive control over a major commandment, but this
 *   benefit does not constitute extraction in the strict sense—it is the
 *   legitimate coordination of authority between earthly and eschatological
 *   domains.
 *
 * KEY AGENTS:
 *   - Rabbinic Authority Structure: Institutional actor (institutional/arbitrage) — maintains interpretive jurisdiction over the obligation while deferring enforcement; benefits from preserving the obligation's operative status in principle
 *   - Jewish Legal Community: Powerful collective (powerful/mobile) — coordinates maintenance of knowledge across indefinite suspension without claiming current obligation
 *   - Observant Individual Jew: Moderate biographical agent (moderate/constrained) — performs ritualized memorial practices and study understood as symbolic maintenance, not substantive obligation fulfillment
 *   - Temple (Absent/Restored): Eschatological actor (analytical/analytical) — the condition of possibility for obligation fulfillment; defines the restoration event as the terminus of suspension
 *   - Messianic Restoration Event: Meta-constraint (analytical/analytical) — treats the restoration as the analytical observer position that resolves the suspension framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.12).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation (Messianic Suspension Reading)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious_studies/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, 'f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc').
narrative_ontology:cs_kernel_codification('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', formalized).
narrative_ontology:cs_authority_grounding('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', lineage).
narrative_ontology:cs_interpretation_layer_present('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc').
narrative_ontology:cs_reading_relation('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_reading_relation('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_axiom('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', foundational, obligation_deferred_not_nullified).
narrative_ontology:cs_axiom_status(obligation_deferred_not_nullified, holdable).
narrative_ontology:cs_axiom_grounding('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', obligation_deferred_not_nullified, deontological).
narrative_ontology:cs_axiom('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', foundational, eschatological_authority_subordinates_earthly).
narrative_ontology:cs_axiom_status(eschatological_authority_subordinates_earthly, holdable).
narrative_ontology:cs_axiom_grounding('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', eschatological_authority_subordinates_earthly, conventional).
narrative_ontology:cs_reference_frame('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', temple_operative_obligation).
narrative_ontology:cs_drift_state('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', post_destruction_indefinite, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f5f61b4b-6af4-44a6-9cfc-a1dc16a20bcc', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, rabbinic_authority_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JEWISH LEGAL COMMUNITY (ROPE) — The halakhic community experiences the suspension as pure coordination: maintaining knowledge of sacrifice law without current enforcement obligation. The constraint solves a collective action problem — how to preserve a commandment's normative content across an indefinite hiatus without claiming it is nullified or falsified. No extraction occurs; beneficiaries and victims are absent because the obligation is deferred, not imposed. Suppression is minimal because the framework is consensual — everyone agrees the Temple is absent and the obligation is suspended.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__messianic_suspension, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 2: RABBINIC AUTHORITY STRUCTURE (ROPE) — Rabbinic hermeneutics experience the suspension framework as legitimate coordination. The authority structure benefits from the framework because it allows rabbinic jurisdiction over a major commandment (sacrifice) to persist in principle while deferring enforcement to the messiah. This is not extraction—it is the proper division of labor between earthly and eschatological authority. The framework coordinates deferred obligation: study maintains knowledge, messianic restoration will fulfill it. Suppression is low because the authority structure is sustained through interpretive consensus, not coercion.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__messianic_suspension, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational analytical view, the suspension is a logical necessity: a commandment cannot simultaneously be operative (generating current obligation) and impossible (no Temple exists). The suspension resolves this by deferring the obligation—a move that appears as an unchangeable law of coherent religious authority systems. However, this perspective risks false summit: the mountain classification naturalizes what is actually a contested interpretive choice between three competing halakhic readings. The engine will detect beneficiary presence (rabbinic authority) and flag this as naturalization.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__messianic_suspension, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: INDIVIDUAL OBSERVANT JEW (PITON) — At biographical scale, the suspension framework appears performative. The individual studies sacrifice law, recites memorial prayers at the Temple's location, or performs substitution practices (Talmudic analysis standing in for Temple service), but these rituals are explicitly recognized as non-fulfilling the actual obligation. The theater is high (0.35–0.60) because the activity is acknowledged as symbolic maintenance, not substantive compliance. Yet the framework persists through institutional inertia and liturgical centrality. The individual experiences this as a degraded obligation—one that has lost its primary function but remains embedded in law and prayer.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__messianic_suspension, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: MESSIANIC RESTORATION AS CONSTRAINT TERMINUS (ROPE) — Treating the restoration event as an analytical observer position: the messianic framework solves a coordination problem across indefinite time. It establishes a coherent legal status ('obligation exists but is suspended') that prevents both nullification and false compliance. It preserves the obligation's normative content for future fulfillment while permitting current non-fulfillment. Extractiveness is extremely low because no resource or labor flows from the suspended state—the constraint is purely structural (a legal fiction solving coherence problems). Suppression is low because the eschatological framework is accepted as legitimate by the authority structure and most of the community.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__messianic_suspension, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, TR),
    TR >= 0.70.

:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The suspension framework imposes no current obligation, generates no current victim set, and produces no extraction flow. Rabbinics benefit from maintaining interpretive authority, but this is a legitimate coordinate benefit, not extraction. The benefit is access to normative jurisdiction over a category of law—a coordination function, not an asymmetric extraction. The slight rise over 1000 years (0.05 → 0.10) reflects gradually increasing theater as substitute practices (memorial services, study of sacrifice law, textual imaginative reconstruction) become more elaborate and institutionalized. Suppression (0.12): Very low. The framework is sustained through interpretive consensus and textual authority, not coercion. No agent is forced to accept the suspension reading; it coexists with the two sibling readings in ongoing halakhic discourse. The minimal suppression reflects that the framework is legitimate within Jewish law rather than imposed externally. Theater ratio (0.35 → 0.45): Moderate-rising. At t=0 (early post-destruction period), the suspension framework is relatively austere—the obligation is deferred, study is maintenance, no substitute practice claims equivalence. As the centuries pass, the theater increases: memorial prayers at the Temple site, elaborate imaginative reconstructions of sacrifice procedures in study, liturgical centralization of Temple themes. By t=1000, the individual observant Jew encounters a substantial ritualized infrastructure dedicated to symbolic remembrance and knowledge preservation. This theater is not deception—it is explicitly acknowledged as non-fulfilling the actual obligation—but it represents the increasing performative elaboration of the suspension framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_nullification,
    'Is the suspension reading a genuine legal status (obligation deferred), or is it a conceptual cover story for practical nullification?',
    'Historical analysis of halakhic discourse: do subsequent generations treat the obligation as retrievable (suspension) or as permanently replaced by substitute practices (functional nullification)? Linguistic analysis of obligation language across the three readings.',
    'If suspension is coherent: this reading''s ε ≈ 0.08 (rope) is accurate. If suspension collapses into functional nullification: ε rises to ~0.35 (the obligation is de facto null, and the reading obscures this). This affects the CS structure classification — is deferral a legitimate frame or a false legal fiction?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_nullification, conceptual, 'Whether suspension is a coherent legal status or a cover story for nullification').

omega_variable(
    authority_grounding_legitimacy,
    'What grounds the authority of the rabbinic hermeneutics that defer the obligation to the messiah? Is it the authority of transmitted tradition, or is it extraction (rabbinic benefit from maintaining interpretive jurisdiction over an operative-in-principle commandment)?',
    'Comparative analysis of how the three readings (messianic_suspension, study_as_archiving, study_as_occupation) distribute authority between rabbinics and eschatology. Does suspension privilege rabbinic authority to defer, or does it subordinate rabbinics to messianic restoration? Textual analysis of rabbinic justification.',
    'If grounded in transmitted tradition: cs_structure.authority_grounding = ''lineage'' (suspension is a legitimate hermeneutic move within Jewish law). If grounded in rabbinic extraction: authority_grounding = ''extraction'' (rabbinics maintain jurisdiction as a side benefit). This affects whether the messianic_suspension reading is stable or inherently contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_legitimacy, conceptual, 'Whether rabbinic authority to defer is traditional or extractive').

omega_variable(
    study_function_ambiguity,
    'Does study of sacrifice law function as archiving (preserving knowledge in anticipation of restoration), occupying the obligation (fulfilling it symbolically in law), or maintaining interpretive authority (rabbinic benefit)?',
    'Explicit textual differentiation across the three readings: messianic_suspension frames study as knowledge preservation; study_as_archiving frames it as memory; study_as_occupation frames it as legitimate compliance. These are distinct normative claims about what study DOES. Map the three readings to determine which study function each reading entails.',
    'If study is archiving only: the messianic_suspension reading''s low extractiveness (0.08) holds. If study is occupation: extractiveness rises (~0.20) because rabbinics are claiming the obligation is being fulfilled. If study is authority maintenance: extractiveness rises further (~0.35) because study becomes a mechanism for rabbinic benefit. This is the structural delta between the three readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_function_ambiguity, conceptual, 'Functional role of sacrifice law study across the three readings').

omega_variable(
    temporal_indefiniteness_coherence,
    'Can a legal obligation genuinely be ''suspended'' across an indefinite timespan (potentially forever), or does indefinite suspension collapse into functional cancellation?',
    'Historical analysis of how suspension has been maintained across 2000 years: does the framework cohere across this timescale, or do subsidiary readings (study_as_occupation, study_as_archiving) emerge as practical workarounds for the incoherence of pure suspension?',
    'If indefinite suspension is coherent: ε ≈ 0.08. If indefinite suspension is conceptually unstable: the three readings emerge as incompatible alternatives for resolving the instability, and the choice between them (study_as_occupation vs study_as_archiving vs messianic_suspension) becomes the real constraint, with higher extractiveness. This affects whether messianic_suspension is a stable terminal reading or a holding pattern generating offspring readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_indefiniteness_coherence, conceptual, 'Coherence of indefinite suspension across historical timescale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tso_susp_theater_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tso_susp_theater_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.35).
narrative_ontology:measurement(tso_susp_theater_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.45).

% Extraction over time
narrative_ontology:measurement(tso_susp_extract_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tso_susp_extract_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(tso_susp_extract_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested halakhic kernel (the Temple sacrifice obligation). The kernel codified in Talmudic law permits multiple readings with different structural properties. The messianic_suspension reading (ε=0.08, Rope) produces minimal extractiveness because it defers adjudication to an eschatological event. The study_as_archiving reading (expected ε~0.12, Rope) frames study as knowledge preservation without claiming obligation fulfillment. The study_as_occupation reading (expected ε~0.25, Tangled Rope) claims study fulfills the obligation, generating asymmetric coordination benefits for rabbinics. Each reading is a distinct constraint with distinct ε values. They coexist as competing halakhic positions because the kernel (the written obligation) permits multiple coherent readings. The three readings form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
