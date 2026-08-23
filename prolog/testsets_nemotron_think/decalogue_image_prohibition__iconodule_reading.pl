% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconodule_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconodule_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconodule_reading
 *   human_readable: Iconodule Distinction: Latria Forbidden, Dulia Permitted via Incarnation
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The iconodule reading of the Second Commandment establishes a
 *   coordination constraint: material images may be venerated (dulia) as
 *   conduits to their divine prototypes, because the Incarnation sanctifies
 *   matter. This is not a natural law but a theological construct requiring
 *   active enforcement — iconographic canons, episcopal approval of images,
 *   liturgical rubrics governing veneration. It solves a genuine coordination
 *   problem: how can Christians honor the saints and Christ visually without
 *   idolatry? The latria/dulia distinction provides the semantic boundary.
 *   The constraint is Rope-type: participants (laity, clergy, iconographers)
 *   are net beneficiaries; alternatives (iconoclasm, moderate regulation)
 *   persist but are excluded from the coordination. Extraction is near-zero —
 *   no rents flow to authorities from the veneration itself. Suppression is
 *   low but nonzero: the constraint maintains itself by anathematizing the
 *   iconoclast position and policing the boundary of orthodox depiction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconodule_reading, 0.15).
domain_priors:suppression_score(decalogue_image_prohibition__iconodule_reading, 0.2).
domain_priors:theater_ratio(decalogue_image_prohibition__iconodule_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconodule_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconodule_reading, rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconodule_reading, "Iconodule Distinction: Latria Forbidden, Dulia Permitted via Incarnation").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconodule_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconodule_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconodule_reading, 'd427eeea-a8ac-40d1-b255-4c7260f710af').
narrative_ontology:cs_kernel_codification('d427eeea-a8ac-40d1-b255-4c7260f710af', formalized).
narrative_ontology:cs_authority_grounding('d427eeea-a8ac-40d1-b255-4c7260f710af', lineage).
narrative_ontology:cs_interpretation_layer_present('d427eeea-a8ac-40d1-b255-4c7260f710af').
narrative_ontology:cs_reading_relation('d427eeea-a8ac-40d1-b255-4c7260f710af', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('d427eeea-a8ac-40d1-b255-4c7260f710af', decalogue_image_prohibition__moderate_iconoclast_reading, coexists_with).
narrative_ontology:cs_axiom('d427eeea-a8ac-40d1-b255-4c7260f710af', foundational, matter_sanctified_by_incarnation).
narrative_ontology:cs_axiom_status(matter_sanctified_by_incarnation, holdable).
narrative_ontology:cs_axiom_grounding('d427eeea-a8ac-40d1-b255-4c7260f710af', matter_sanctified_by_incarnation, deontological).
narrative_ontology:cs_axiom('d427eeea-a8ac-40d1-b255-4c7260f710af', foundational, latria_dulia_distinction_valid).
narrative_ontology:cs_axiom_status(latria_dulia_distinction_valid, holdable).
narrative_ontology:cs_axiom_grounding('d427eeea-a8ac-40d1-b255-4c7260f710af', latria_dulia_distinction_valid, deontological).
narrative_ontology:cs_axiom('d427eeea-a8ac-40d1-b255-4c7260f710af', secondary, images_as_conduits_to_prototypes).
narrative_ontology:cs_axiom_status(images_as_conduits_to_prototypes, holdable).
narrative_ontology:cs_axiom_grounding('d427eeea-a8ac-40d1-b255-4c7260f710af', images_as_conduits_to_prototypes, conventional).
narrative_ontology:cs_reference_frame('d427eeea-a8ac-40d1-b255-4c7260f710af', seventh_council_iconodule_settlement).
narrative_ontology:cs_drift_state('d427eeea-a8ac-40d1-b255-4c7260f710af', contemporary_post_reformation_iconoclasm, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d427eeea-a8ac-40d1-b255-4c7260f710af', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, faithful_laity).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, church_authorities).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconodule_reading, iconographers).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, incarnation_sanctifies_matter).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, latria_dulia_distinction).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconodule_reading, images_as_conduits_to_prototypes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the latria/dulia distinction through ecumenical councils, iconographic canons, and episcopal oversight. Authorize which images are orthodox and police devotional practice to prevent slippage into idolatry. Their authority derives from conciliar lineage and the claim to guard the Incarnation's implications for matter.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, church_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconodule_reading, church_authorities, beneficiary).

% Access the divine through venerated images in liturgy and private prayer. The distinction gives them a theologically safe path: honor (dulia) passes through the image to its prototype (Christ, the Virgin, saints) without collapsing into worship (latria). Exit means leaving the sacramental worldview entirely — constrained by identity, community, and salvation theology.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, faithful_laity, beneficiary,
    organized, biographical, constrained, global).

% Receive canonical commissions and theological guidance for producing images that conform to typological rules (inverse perspective, stylized features, inscribed names). Their craft is sacralized — not mere decoration but participation in the economy of incarnation. They can technically exit to secular art markets, but lose the theological vocation that structures their practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconographers, beneficiary,
    moderate, biographical, mobile, regional).

% Hold that any religious image used in worship violates the Second Commandment and constitutes idolatry. They are structurally excluded from the iconodule framework — their position is anathematized by the Seventh Council. They cannot participate in the coordination without abandoning their core conviction. Historically, they held imperial power (Byzantine Iconoclasm) and later Protestant establishments.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, iconoclast_theologians, excluded,
    organized, generational, trapped, global).

% Accept two-dimensional images under strict regulation (no sculpture, no kisses, no candles, didactic-only use) but reject the full iconodule theology of veneration. They occupy a middle ground that the iconodule settlement does not accommodate — the Seventh Council anathematizes both total rejection and restricted use. Their exit is constrained: they must either accept the full iconodule logic or reject images entirely.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, moderate_iconoclasts, excluded,
    organized, generational, constrained, regional).

% Scholars of religion, art historians, and comparative theologians who analyze the latria/dulia distinction as a structural solution to the problem of material mediation. They do not participate devotionally but track how the constraint shapes visual culture, church architecture, and inter-confessional boundaries.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconodule_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables laity to honor divine prototypes through material images without committing idolatry, by distinguishing latria (worship due to God alone) from dulia (veneration/honor given to saints and images as conduits), grounded in the theology that the Incarnation sanctifies matter as a valid medium for divine presence.
% TRANSFER_FUNCTION: Moves devotional attention from the material image to its prototype (Christ, the Virgin, saints) — no material transfer occurs; the image functions as a semiotic and sacramental conduit. The constraint channels honor through matter without the matter absorbing the honor.
% ABSENT_VOICES: Iconoclast theologians who would prohibit all religious images as idolatry; moderate iconoclasts who would permit only regulated two-dimensional didactic images. Both are excluded by the Seventh Council's anathemas and cannot participate in the iconodule coordination without surrendering their defining conviction.
% DISAPPEARANCE_RATIONALE: If the latria/dulia distinction vanished overnight, the entire economy of Christian visual culture would collapse: iconostases, fresco programs, private icon corners, liturgical processions with images, and the theological grammar of 'veneration not worship' would lose their coordinating principle. Protestant iconoclasm and secular museum display show what the world looks like without this constraint — images become either idols or art objects, not conduits.
% FOUNDING_PROBLEM: How can the Incarnation's sanctification of matter be expressed in worship without violating the Second Commandment's prohibition on graven images? The iconodule reading answers: the Word became flesh, therefore matter can bear the divine; honor given to an image passes to its prototype (dulia), not to the matter itself (which would be latria).
% FOUNDING_PROBLEM_CORROBORATION: Seventh Ecumenical Council (Nicaea II, 787) defined the distinction and anathematized its denial. John of Damascus (De Imaginibus) and Theodore the Studite provided the systematic theology. The corroboration comes from the conciliar reception across Eastern and Western churches (with Western qualifications at Frankfurt 794). No significant theological tradition outside the iconodule settlement corroborates the problem as solved — iconoclast and Protestant traditions attest the problem is either unsolved or solved by prohibition.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconodule_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconodule_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconodule_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconodule_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconodule_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconodule_reading_tests).
:- end_tests(decalogue_image_prohibition__iconodule_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint coordinates devotional access rather than extracting resources. Theater ratio is low (0.1) — the theological function is genuine, not performative. Suppression (0.2) reflects the active boundary maintenance (canons, anathemas) but not coercive extraction. Accessibility collapse (0.4) is moderate: the iconoclast alternative remains conceptually available and historically potent, but within the iconodule framework the distinction is treated as settled. Resistance (0.5) reflects the persistent iconoclast challenge (Byzantine Iconoclasm, Protestant Reformation, modern secularism) that the constraint must actively resist to maintain its coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the church authority seat, the constraint is a Rope: it coordinates the faithful's access to the divine through matter sanctified by the Incarnation. From the iconoclast seat (excluded), the same structure appears as a Snare: it legitimates idolatry under a semantic distinction they reject. From the moderate iconoclast seat, it appears as a Tangled Rope: it coordinates but overreaches by requiring full veneration where didactic use would suffice. The engine computes these per-seat classifications from the structural data — the authored claim (rope) reflects the internal logic of the reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Church authorities (agenda_setter) sit near the beneficiary end (d ~ 0.2): they administer the coordination and gain institutional coherence from it, but bear enforcement costs. Faithful laity (beneficiary) sit at strong beneficiary end (d ~ 0.1): they gain devotional access with minimal cost. Iconographers (beneficiary) sit near symmetric (d ~ 0.4): they gain sacramental vocation but submit to strict canonical rules. Iconoclast and moderate iconoclast theologians (excluded) are not coordinated by this constraint — they experience it as suppression from outside. Theological observers (observer) sit at analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to honor the Incarnation's materiality without idolatry) remains live — the constraint has not atrophied. The Seventh Council's settlement is actively maintained in Orthodox and Catholic practice. No mandatrophy: the coordination function is the current operating purpose, not a vestige of a dead problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the iconodule_reading a distinct constraint with its own stable ε, or does its classification depend on which observable (liturgical practice, theological treatise, iconographic production) is measured?',
    'Apply the ε-invariance test: if measuring extraction via devotional practice (low) vs. institutional enforcement machinery (moderate) yields different ε, decompose into separate constraints. Current judgment: the coordination function is unitary across observables.',
    'If ε varies by observable, the iconodule position fragments into multiple constraints (e.g., ''liturgical veneration coordination'' vs. ''iconographic canon enforcement''), each with its own classification. This would require separate JSON files linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the iconodule reading is ε-invariant across measurement bases — a kernel-reading integrity check.').

omega_variable(
    latria_dulia_operationalization,
    'Can the latria/dulia distinction be operationally maintained in practice, or does devotional psychology inevitably collapse veneration into worship?',
    'Empirical study of devotional behavior: do practitioners distinguish honor-to-prototype from worship-of-image in self-report and behavioral measures? Historical analysis of iconoclast critiques — were they describing a real slippage or a theological category error?',
    'If the distinction collapses in practice, the constraint''s extractiveness is understated — the coordination function fails and the constraint becomes a Snare (idolatry enabled by theological cover). If it holds, the Rope classification stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(latria_dulia_operationalization, empirical, 'Whether the core semantic boundary of the constraint is psychologically and practically sustainable.').

omega_variable(
    incarnation_as_sufficient_ground,
    'Does the Incarnation logically entail the permissibility of images, or is the iconodule inference a contingent theological development?',
    'Patristic textual analysis: does the iconodule argument (John of Damascus, Nicaea II) derive image-permissibility necessarily from Chalcedonian Christology, or does it introduce an additional premise? Comparative theology: do all Chalcedonian traditions accept the inference?',
    'If the inference is necessary, the constraint approaches Mountain-like naturalness within the Chalcedonian framework (high accessibility_collapse). If contingent, it remains a Rope — a coordination choice among Chalcedonians.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incarnation_as_sufficient_ground, conceptual, 'Whether the Incarnation-to-images inference is logically compelled or theologically discretionary.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of iconoclast positions structural (anathemas, imperial enforcement, episcopal policing) or internalized (theological formation that makes iconoclasm unthinkable)?',
    'Post-exit trajectory study: when communities leave the iconodule framework (e.g., Protestant Reformation), does iconoclastic suppression persist internally or vanish? If it persists, internalization is significant.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than structural measures suggest — the boundary is carried by the agents themselves. This would raise the computed χ for excluded seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the iconodule/iconoclast boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconodule_reading, 787, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t787, decalogue_image_prohibition__iconodule_reading, theater_ratio, 787, 0.05).
narrative_ontology:measurement(deca_tr_t843, decalogue_image_prohibition__iconodule_reading, theater_ratio, 843, 0.08).
narrative_ontology:measurement(deca_tr_t1054, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1054, 0.1).
narrative_ontology:measurement(deca_tr_t1517, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(deca_tr_t1700, decalogue_image_prohibition__iconodule_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement(deca_tr_t2026, decalogue_image_prohibition__iconodule_reading, theater_ratio, 2026, 0.1).

% Extraction over time
narrative_ontology:measurement(deca_be_t787, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 787, 0.08).
narrative_ontology:measurement(deca_be_t843, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 843, 0.1).
narrative_ontology:measurement(deca_be_t1054, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1054, 0.12).
narrative_ontology:measurement(deca_be_t1517, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1517, 0.18).
narrative_ontology:measurement(deca_be_t1700, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(deca_be_t2026, decalogue_image_prohibition__iconodule_reading, base_extractiveness, 2026, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t787, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 787, 0.15).
narrative_ontology:measurement(deca_su_t843, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 843, 0.25).
narrative_ontology:measurement(deca_su_t1054, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1054, 0.2).
narrative_ontology:measurement(deca_su_t1517, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(deca_su_t1700, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 1700, 0.2).
narrative_ontology:measurement(deca_su_t2026, decalogue_image_prohibition__iconodule_reading, suppression_requirement, 2026, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconodule_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconodule_reading, 0.08).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconodule_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the Second Commandment prohibition into three structurally distinct readings. The iconodule_reading (this file) instantiates a Rope: latria/dulia distinction + Incarnation ground = coordination. The iconoclast_reading instantiates a Snare (or Mountain, from its own seat): total prohibition enforced by coercion. The moderate_iconoclast_reading instantiates a Scaffold: regulated 2D use as transitional toward either full iconodule or full iconoclast settlement. The ε values differ: iconodule ε≈0.15 (coordination), iconoclast ε≈0.7 (extraction under enforcement), moderate ε≈0.3 (transitional regulation). They are linked by shared kernel and historical contention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconodule_reading, institutional, 0.2).
constraint_indexing:directionality_override(decalogue_image_prohibition__iconodule_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
