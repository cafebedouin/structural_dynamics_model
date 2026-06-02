% ============================================================================
% CONSTRAINT STORY: study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_performance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: study_as_performance
 *   human_readable: Study as Cosmic Performance: Talmudic Obligation in Absence of Temple
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   The reading 'study as performance' instantiates one interpretation of
 *   Rabbinic obligation in the absence of the Second Temple (destroyed 70
 *   CE). The reading holds that engaging in textual study of sacrificial law
 *   (Kodashim tractates) performs the cosmic function that Temple sacrifice
 *   once performed: maintaining the cosmic order through ritual action. This
 *   reading grounds the obligation to study Talmudic discussions of
 *   sacrificial procedure entirely in cosmic function, independent of Temple
 *   restoration. The beneficiary is cosmic order itself, not any
 *   institutional actor. The constraint exhibits zero extractiveness and
 *   minimal suppression because no identifiable human agent benefits
 *   asymmetrically — the obligation is coordinated communal participation in
 *   a cosmic service. This reading contrasts sharply with two sibling
 *   readings: (1) study_as_preparation frames study as preparation for
 *   eventual Temple restoration, making the obligation contingent on
 *   restoration possibility, and (2) study_as_archive frames study as
 *   preservation of endangered textual knowledge, making the obligation
 *   contingent on institutional continuity. The study_as_performance reading
 *   uniquely decouples the obligation from any institutional outcome or
 *   restoration condition.
 *
 * KEY AGENTS:
 *   - Cosmic Order: Primary beneficiary (non-agentic) — the reading frames cosmic maintenance as the constraint's ultimate function, not any human or institutional benefit
 *   - Halakhic Community: Coordinated participants (organized/mobile) — engaged in shared textual study; no coercive extraction; exit is mobile (one may leave the tradition)
 *   - Individual Student: Moderate agent (moderate/constrained) — experiences the obligation as communal constraint (suppression=0.08) but with genuine coordination function (rope classification)
 *   - Analytical Observer: Examines the reading's internal coherence (analytical/analytical) — assesses whether cosmic necessity is a natural law or a theological framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_performance, 0.02).
domain_priors:suppression_score(study_as_performance, 0.08).
domain_priors:theater_ratio(study_as_performance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(study_as_performance, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(study_as_performance, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_performance, rope).
narrative_ontology:human_readable(study_as_performance, "Study as Cosmic Performance: Talmudic Obligation in Absence of Temple").
narrative_ontology:topic_domain(study_as_performance, "religious_studies/jewish_law/textual_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(study_as_performance, '1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75').
narrative_ontology:cs_created_at('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', '').
narrative_ontology:cs_kernel_codification('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', fixed_text).
narrative_ontology:cs_authority_grounding('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', lineage).
narrative_ontology:cs_interpretation_layer_present('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75').
narrative_ontology:cs_kernel_id(study_as_performance, kodashim_obligation).
narrative_ontology:cs_reading_relation('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', study_as_preparation, influences).
narrative_ontology:cs_reading_relation('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', study_as_archive, coexists_with).
narrative_ontology:cs_axiom('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', foundational, cosmic_function_persists_absent_temple).
narrative_ontology:cs_axiom_status(cosmic_function_persists_absent_temple, holdable).
narrative_ontology:cs_axiom_grounding('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', cosmic_function_persists_absent_temple, theological).
narrative_ontology:cs_axiom('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', foundational, textual_recitation_is_ritual_performance).
narrative_ontology:cs_axiom_status(textual_recitation_is_ritual_performance, holdable).
narrative_ontology:cs_axiom_grounding('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', textual_recitation_is_ritual_performance, theological).
narrative_ontology:cs_reference_frame('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', sacrificial_obligation_continuous).
narrative_ontology:cs_drift_state('1aca4baa-d5b0-40d7-94b8-5c1fb03aeb75', post_temple_destruction, gap(stable, minor, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_performance, cosmic_order).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HALAKHIC COMMUNITY (ROPE) — Community engaged in textual study of sacrificial law experiences the constraint as pure coordination. The obligation to study is a collective action problem solved by shared commitment to textual interpretation. No extraction: the beneficiary (cosmic order) is external to the community's internal cost-benefit. Participants exit freely (mobile); participation is sustained by genuine consensus that study maintains cosmic function. This is coordination without extraction.
constraint_indexing:constraint_classification(study_as_performance, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER / COSMIC NECESSITY (MOUNTAIN) — From a civilizational perspective grounded in the reading's own metaphysical commitments, the cosmic function of study is treated as immutable: the reading holds that sacrificial law maintains cosmic order through textual performance regardless of Temple existence. This is presented as a natural law of the spiritual cosmos, not a contingent institutional arrangement. The analytical perspective adopts the reading's own framework and finds zero extractiveness and zero suppression because cosmic law admits no alternatives.
constraint_indexing:constraint_classification(study_as_performance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: INDIVIDUAL STUDENT (TANGLED ROPE) — An individual learner at biographical time experiences the study obligation as both coordination and constraint. The obligation coordinates communal textual practice (rope function: many eyes preserve text, shared interpretation prevents degradation). But it also constrains individual choice — one cannot simply stop studying without social cost (constrained exit: career as rabbi, community standing, self-identity as learned Jew). Extractiveness is minimal (0.08) because the extraction is not toward an identifiable beneficiary but toward an abstract cosmic order. Yet suppression exists: community pressure, time demands, specialized knowledge barriers. This is hybrid: genuine coordination function with internalized constraint.
constraint_indexing:constraint_classification(study_as_performance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_performance_tests).
:- end_tests(study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.02): Minimal. The reading explicitly denies that any institutional actor benefits from study obligation — the beneficiary is cosmic order, which is non-extractive by definition. No victim class exists because no asymmetric extraction occurs. The minimal value (0.02 rather than 0.00) reflects measurement uncertainty: the reading's own internal self-assessment is that cosmic function generates zero extraction, but the analytical observer may identify latent institutional benefits (sustaining rabbinic authority, justifying study time investment, naturalizing communal labor). This is why omega_intercommunal_extraction_latency is critical. Suppression (0.08): Minimal. The reading presents study obligation as a coordinated commitment to cosmic maintenance, not as coerced compliance. Participants are mobile (can exit the tradition) and ostensibly participate through consensus. However, minimal suppression acknowledges that social cost of exit exists (community pressure, identity cost, career dependency for rabbis). This is not high suppression because the reading emphasizes genuine agreement with cosmic function, not coercion. Theater ratio (0.15): Very low. The reading frames study as actual cosmic performance, not performative ritual. No theater — the obligation is meant to be literally functional in cosmic maintenance. The low theater distinguishes this from piton (which would emerge if study were described as degraded ritual maintained through institutional inertia). Claimed type: Rope. Pure coordination without extraction. The community solves a collective action problem (maintaining shared textual knowledge, coordinating study practice) without asymmetric extraction toward any beneficiary. The absence of an extracting beneficiary is the defining feature.
 *
 * PERSPECTIVAL GAP:
 *   The reading's core analytical claim — that cosmic function persists independent of Temple existence — generates a perspectival gap between the reading's internal self-assessment (zero extraction, pure coordination) and the analytical observer's potential external assessment (unidentified institutional benefits, naturalization of communal labor). From within the reading's own framework, extractiveness is zero because cosmic order is the sole beneficiary. From outside the framework, an observer might identify extraction toward institutional actors (rabbinic authority, community continuity) that the reading naturalizes as cosmic function. This gap is not a defect in the reading but a structural feature of how theological frameworks ground obligations: they naturalize costs by referencing transcendent beneficiaries. The gap is tractable through omega variables, not through revising the extractiveness metric.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading declares cosmic_order as the sole beneficiary, which is a non-agentic entity. The directionality derivation chain has no institutional beneficiary to which extraction could flow — the cosmic order does not accumulate benefit or power. Therefore, d (directionality) is not computed through standard beneficiary-victim mapping. Instead, the reading's structure maps to: no extraction flow toward human agents, no suppression beyond minimal social cost of exit, pure coordination function. The beneficiary is declared as cosmic_order (not a power atom) to signal that the constraint's justification is entirely theological, not institutional. If an institutional beneficiary (e.g., rabbinic_authority) were present, directionality would shift dramatically and extractiveness would rise. The minimalist beneficiary declaration is precise to the reading's claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmic_function_efficacy,
    'Does sacrificial law study actually maintain cosmic order, or is the cosmic efficacy claim a theological framework that naturalizes communal text preservation?',
    'Comparative textual analysis across different readings: study_as_preparation grounds efficacy in preparation for actual Temple restoration (falsifiable if Temple restoration becomes possible); study_as_archive grounds efficacy in record preservation (testable against degradation outcomes); study_as_performance grounds efficacy in cosmic maintenance (not falsifiable within the framework itself — the reading must be evaluated on internal coherence and theological authority, not empirical test).',
    'If cosmic efficacy is framework-internal (unfalsifiable): the constraint is legitimated by theological commitment, not empirical fact. The mountain classification becomes a false summit — naturalization of a metaphysical claim. If cosmic efficacy is grounded in observable maintenance (record preservation, community continuity): the reading converges toward study_as_archive, and extractiveness increases if study serves institutional preservation over cosmic function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_function_efficacy, conceptual, 'Whether cosmic function of study is empirically testable or framework-internal metaphysical claim').

omega_variable(
    temple_restoration_counterfactual,
    'If the Temple were restored and sacrificial service resumed, would study obligation diminish or remain obligatory?',
    'Historical-comparative analysis: Maimonides and other medieval authorities debated whether study obligation would persist post-restoration. Modern thinkers differ on whether restored Temple service would replace or supplement study. The reading''s claim that Temple absence is ''irrelevant to spiritual efficacy'' generates a counterfactual: does restoration change the obligation?',
    'If study obligation persists unchanged post-restoration: the reading''s independence claim is confirmed (Temple status is truly irrelevant). If study obligation diminishes: the reading obscures a latent dependence on Temple absence (the constraint''s structure changes with restoration), and the reading is partially overridden by changing conditions. This is not foreclosure but revision under counterfactual conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_restoration_counterfactual, conceptual, 'Whether study obligation persists or diminishes if Temple is restored').

omega_variable(
    reading_distinguishability_empirical,
    'What observable difference distinguishes this reading (study as cosmic performance) from study_as_archive (study as preservation of endangered knowledge)?',
    'Textual analysis of motivational framing: study_as_performance emphasizes maintenance of cosmic order through textual recitation; study_as_archive emphasizes preservation against textual loss. Both readings motivate study obligation, but with different ultimate beneficiaries (cosmic order vs. community knowledge-continuity). Empirically, both readings will produce identical study behaviors. The difference is entirely in framing and ultimate purpose — this is a reading distinction, not a behavioral one.',
    'If no observable difference exists: the readings are equivalent under the pragmatic criterion, and the choice between them is purely theological/metaphysical. This confirms that the reading distinction is located in CS structure (which axioms, which cosmic framework), not in extractiveness or suppression metrics. Both readings should produce very low extractiveness because neither extracts from identifiable victims toward identifiable beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinguishability_empirical, conceptual, 'Observable behavioral difference between study_as_performance and study_as_archive readings').

omega_variable(
    intercommunal_extraction_latency,
    'Does this reading''s emphasis on cosmic function obscure extraction of communal labor (study, teaching, textual maintenance) toward sustaining rabbinic authority and institutional legitimacy?',
    'Structural analysis: compare extractiveness of study_as_performance (framed as cosmic obligation) vs. study_as_preparation (framed as preparation for Temple restoration). If the two readings produce the same obligatory study behavior but justify it through different cosmic functions, does one reading more effectively mask institutional extraction? If study obligations increase when framed as cosmic maintenance, the reading may enable higher suppression by naturalizing the obligation.',
    'If the cosmic frame naturalizes communal labor extraction: extractiveness should increase, and suppression should increase, contrary to the reading''s internal self-assessment. This would reveal the reading as a false summit — a commitment-system framing that naturalizes asymmetric costs. If the cosmic frame genuinely sustains participation through genuine consensus (not naturalizing extraction): the low extractiveness is accurate, and the reading is a legitimate coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intercommunal_extraction_latency, empirical, 'Whether cosmic framing naturalizes institutional extraction of communal labor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_performance, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(study_as_performance, study_as_preparation).
narrative_ontology:affects_constraint(study_as_performance, study_as_archive).

% DUAL FORMULATION NOTE:
% The kodashim_obligation kernel has three constraint readings: study_as_performance (cosmic function maintenance, ε≈0.02), study_as_preparation (Temple restoration preparation, ε≈0.35), and study_as_archive (textual preservation, ε≈0.25). These are not three measurements of one constraint but three structurally distinct constraints grounded in the same halakhic obligation. Each reading generates different ε values because the ultimate beneficiary (cosmic order, Temple restoration possibility, textual tradition continuity) and victim classes differ. All three readings produce identical study behaviors; the differences are purely in metaphysical grounding and ultimate purpose. The network links reflect how each reading's adoption influences the others: study_as_performance makes preparation contingent; study_as_archive offers institutional alternative; study_as_preparation offers Temple-restoration exit from the cosmic-function frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
