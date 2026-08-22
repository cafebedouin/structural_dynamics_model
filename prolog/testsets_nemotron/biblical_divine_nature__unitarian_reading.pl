% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Biblical Divine Nature
 *   domain: theological/doctrinal
 *
 * SUMMARY:
 *   The unitarian reading of the biblical divine nature asserts that the
 *   Father alone is God in the strict numerical sense; the Son is either a
 *   created being (Arian/Socinian) or a subordinate divine agent
 *   (subordinationist), and the Spirit is the power or presence of the
 *   Father, not a distinct person. This reading has recurred across Christian
 *   history whenever the trinitarian consensus is challenged from a
 *   scripturalist or rationalist direction. It is not a single movement but a
 *   structural option within the biblical_divine_nature kernel — a way of
 *   resolving the kernel's tension that minimizes metaphysical commitments at
 *   the cost of ecclesial legitimacy. The constraint operates as a rope: it
 *   coordinates dissenting communities around a shared hermeneutic (sola
 *   scriptura, numerical monotheism) without extracting from them; its
 *   victims are the institutional structures whose authority depends on the
 *   trinitarian boundary. The reading's extractiveness is low because it does
 *   not demand material tribute or enforced conformity from its adherents;
 *   its suppression is low because it survives through persuasion and print,
 *   not coercion. Theater is low but non-zero: some unitarian bodies maintain
 *   liturgical forms that mimic trinitarian worship while denying its
 *   content.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.22).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.15).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Biblical Divine Nature").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theological/doctrinal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '2807d02c-1efe-475f-a9ae-26414151bff1').
narrative_ontology:cs_kernel_codification('2807d02c-1efe-475f-a9ae-26414151bff1', formalized).
narrative_ontology:cs_authority_grounding('2807d02c-1efe-475f-a9ae-26414151bff1', lineage).
narrative_ontology:cs_interpretation_layer_present('2807d02c-1efe-475f-a9ae-26414151bff1').
narrative_ontology:cs_reading_relation('2807d02c-1efe-475f-a9ae-26414151bff1', biblical_divine_nature__trinitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('2807d02c-1efe-475f-a9ae-26414151bff1', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('2807d02c-1efe-475f-a9ae-26414151bff1', foundational, father_alone_is_god_numerically).
narrative_ontology:cs_axiom_status(father_alone_is_god_numerically, holdable).
narrative_ontology:cs_axiom_grounding('2807d02c-1efe-475f-a9ae-26414151bff1', father_alone_is_god_numerically, deontological).
narrative_ontology:cs_axiom('2807d02c-1efe-475f-a9ae-26414151bff1', foundational, son_is_subordinate_or_created).
narrative_ontology:cs_axiom_status(son_is_subordinate_or_created, holdable).
narrative_ontology:cs_axiom_grounding('2807d02c-1efe-475f-a9ae-26414151bff1', son_is_subordinate_or_created, empirically_contingent).
narrative_ontology:cs_reference_frame('2807d02c-1efe-475f-a9ae-26414151bff1', apostolic_monotheism).
narrative_ontology:cs_drift_state('2807d02c-1efe-475f-a9ae-26414151bff1', post_nicene_settlement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2807d02c-1efe-475f-a9ae-26414151bff1', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, low_church_laity).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, radical_reformers).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, rationalist_theologians).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy_guardians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary believers who find the unitarian reading more accessible and less abstract than trinitarian formulations. They benefit from a simpler devotional focus on the Father as the sole object of worship, but remain embedded in congregations where trinitarian liturgy dominates.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, low_church_laity, beneficiary,
    powerless, biographical, constrained, local).

% Reform leaders (e.g., Servetus, Socinus, early Unitarian organizers) who actively promote the reading as a return to biblical simplicity and a challenge to creedal coercion. They set the interpretive agenda for unitarian communities and bear the institutional risks of heterodoxy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, radical_reformers, agenda_setter,
    organized, generational, mobile, regional).

% Intellectuals who adopt the reading because it aligns with philosophical commitments to reason, scriptural perspicuity, and anti-metaphysical theology. They gain professional and social capital in Enlightenment and post-Enlightenment circles, and can move between academic and dissenting ecclesiastical positions.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, rationalist_theologians, beneficiary,
    moderate, biographical, arbitrage, national).

% Episcopal and conciliar structures (Catholic, Orthodox, magisterial Protestant) whose authority rests on guarding the trinitarian formula as the boundary of orthodoxy. The unitarian reading directly undermines their claim to be the custodians of the apostolic faith; they cannot exit the conflict without surrendering their defining function.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_hierarchy, payer,
    institutional, generational, trapped, continental).

% Confessional bodies, theological faculties, and catechetical systems that treat the Nicene-Constantinopolitan Creed as the non-negotiable grammar of Christian identity. Their professional and communal identity is fused with trinitarian orthodoxy; the unitarian reading is experienced as an existential threat to the coherence of their tradition.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy_guardians, payer,
    organized, generational, identity_locked, continental).

% Scholar of doctrinal history who tracks the reading's emergence, suppression, and persistence across centuries. Sees the unitarian reading as a recurring structural option within the biblical_divine_nature kernel — not a deviation but a latent possibility that resurfaces whenever the kernel's interpretive pressure is relaxed.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, historical_theologian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic key that resolves the tension between biblical monotheism and the plurality of divine actors in scripture by identifying the Father alone as God, relegating Son and Spirit to subordinate or created status — thus preserving a numerically singular God without metaphysical complexity.
% TRANSFER_FUNCTION: Moves interpretive authority and devotional focus from the triune God of the creeds to the Father alone; transfers the cost of doctrinal enforcement from dissenters to the institutional hierarchy (which must police boundaries) and credal guardians (whose identity is threatened). Resources (printing, preaching, communal formation) flow to unitarian communities; legitimacy flows from the creedal center to the scripturalist margin.
% ABSENT_VOICES: The pre-Nicene subordinationist theologians (e.g., Origen, Tertullian in early phases) who held the Son to be subordinate but not created — their position is neither fully unitarian nor trinitarian and is excluded by both the unitarian and trinitarian readings. Also absent: the Johannine community's own self-understanding, which the unitarian reading must reinterpret as non-literal.
% DISAPPEARANCE_RATIONALE: If the unitarian reading vanished overnight, the spectrum of live options within the biblical_divine_nature kernel would collapse toward the trinitarian-modalist axis; radical reform movements would lose a theological anchor; the history of dissent would lose a major strand; and the pressure on institutional orthodoxy from the scripturalist margin would diminish — though the kernel itself would persist through its other readings.
% FOUNDING_PROBLEM: How to confess the God of Jesus and the apostles as the one God of Israel without importing Greek metaphysical categories (ousia, hypostasis) that the New Testament does not contain — i.e., how to be biblically monotheistic in a world that demands philosophical precision.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the persistent reappearance of unitarian movements across 17 centuries (Paulicians, Cathars, Socinians, Polish Brethren, English Unitarians, American Unitarians, Christadelphians, Oneness Pentecostals) — each arising independently from scriptural engagement, not from institutional transmission. No credal body corroborates the problem; they treat it as settled by the councils. Modern biblical scholarship (e.g., James Dunn, Larry Hurtado, Richard Bauckham) corroborates that early Christology was more fluid than the creeds suggest, but does not endorse the unitarian resolution.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).
:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22) reflects the reading's cost to its adherents: social marginalization, loss of sacramental recognition, and exclusion from mainstream ecclesial resources. This is not extraction by the reading itself but extraction imposed on its holders by the dominant orthodoxy. Suppression (0.15) is the internal pressure within unitarian communities to maintain doctrinal purity against trinitarian reversion — a mild form of boundary policing. Theater (0.18) captures the performative retention of trinitarian language (baptismal formulas, doxologies) in some unitarian traditions. Accessibility collapse (0.35) is moderate: the reading is intelligible and its alternatives (trinitarian, modalist) remain conceptually available. Resistance (0.42) is the persistent institutional opposition the reading faces. The claimed type 'rope' reflects the reading's function as a coordination mechanism for dissent without internal extraction — the engine will compute per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the radical_reformer seat, the reading is a liberating coordination (rope). From the institutional_hierarchy seat, the same structure is experienced as a snare that corrodes the foundations of orthodoxy — but the hierarchy is the victim, not the target of extraction. The engine computes this divergence from the declared roles and exit options: the hierarchy's d-value is driven toward 1.0 by its identity_locked/trapped position, while the laity's d-value is driven toward 0.0 by their beneficiary role. The claimed_type 'rope' is the author's structural judgment; the engine's per-seat computation may yield different classifications for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The low_church_laity are beneficiaries: they gain a simpler, more accessible object of worship, but their exit is constrained by family and community ties. Radical_reformers are agenda_setters: they organize the reading's communal expression and bear the highest institutional risk. Rationalist_theologians are beneficiaries with arbitrage exit: they can move between academic and dissenting spheres. Institutional_hierarchy and credal_orthodoxy_guardians are victims: the reading's existence undermines their authority and identity; they are trapped (hierarchy) or identity_locked (guardians) because their institutional role is constituted by the very boundary the reading denies. The historical_theologian_observer sits at the analytical seat with no structural stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading has no mandate to resolve — it was never instituted as a solution to a temporary problem. Its founding problem (biblical monotheism without Greek metaphysics) remains live because the trinitarian settlement imported precisely those metaphysics. The reading persists not by institutional inertia but by recurrent scriptural rediscovery. Mandatrophy is inapplicable; the reading is a permanent structural option within the kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordinationist_boundary,
    'Does the unitarian reading structurally include pre-Nicene subordinationism (Son as subordinate but uncreated), or is it limited to created-Son positions (Arian/Socinian)?',
    'Historical-theological taxonomy of unitarian movements: classify each by its Christology (created vs. subordinate-uncreated) and assess whether the coordination function differs.',
    'If subordinationism is included, the reading''s coordination function broadens and its victim set expands to include Nicene orthodoxy''s specific anathemas against subordination. If excluded, the reading is a narrower Socinian-type position with different historical traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationist_boundary, conceptual, 'Whether the reading''s Christological boundary includes subordinationism or only created-Son positions.').

omega_variable(
    scripturalist_coherence,
    'Can the unitarian reading coherently account for the full New Testament witness (especially John, Hebrews, Revelation) without ad hoc reinterpretation?',
    'Exegetical consensus survey across unitarian and non-unitarian scholarship; assessment of whether the reading''s hermeneutic generates more anomalies than the trinitarian reading.',
    'If the reading requires systematic special pleading on key texts, its coordination function is impaired — it coordinates only by ignoring counter-evidence. If it has a coherent exegetical framework, its rope classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scripturalist_coherence, empirical, 'Whether the reading''s scriptural coherence is sufficient for stable coordination.').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading of the biblical_divine_nature kernel. How does the kernel''s committer structure (the triune formula as a stabilized commitment) shape the extraction dynamics of each reading?',
    'Compare the three readings'' extractiveness, suppression, and victim/beneficiary structures. The kernel''s stabilization at Nicaea/Constantinople created a commitment system where deviation is structurally extractive for the deviant (social cost) and the guardian (identity threat).',
    'If the kernel''s committer structure is the primary driver of extraction, then the unitarian reading''s low extractiveness is an artifact of its marginalization, not its intrinsic structure. A counterfactual where unitarianism became dominant would reverse the victim/beneficiary polarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'The committer-axis dynamics of the biblical_divine_nature kernel and how they distribute extraction across readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__unitarian_reading, theater_ratio, 325, 0.05).
narrative_ontology:measurement(bibl_tr_t600, biblical_divine_nature__unitarian_reading, theater_ratio, 600, 0.08).
narrative_ontology:measurement(bibl_tr_t1100, biblical_divine_nature__unitarian_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(bibl_tr_t1550, biblical_divine_nature__unitarian_reading, theater_ratio, 1550, 0.22).
narrative_ontology:measurement(bibl_tr_t1750, biblical_divine_nature__unitarian_reading, theater_ratio, 1750, 0.2).
narrative_ontology:measurement(bibl_tr_t1900, biblical_divine_nature__unitarian_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(bibl_tr_t2025, biblical_divine_nature__unitarian_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__unitarian_reading, base_extractiveness, 325, 0.08).
narrative_ontology:measurement(bibl_be_t600, biblical_divine_nature__unitarian_reading, base_extractiveness, 600, 0.12).
narrative_ontology:measurement(bibl_be_t1100, biblical_divine_nature__unitarian_reading, base_extractiveness, 1100, 0.15).
narrative_ontology:measurement(bibl_be_t1550, biblical_divine_nature__unitarian_reading, base_extractiveness, 1550, 0.25).
narrative_ontology:measurement(bibl_be_t1750, biblical_divine_nature__unitarian_reading, base_extractiveness, 1750, 0.22).
narrative_ontology:measurement(bibl_be_t1900, biblical_divine_nature__unitarian_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(bibl_be_t2025, biblical_divine_nature__unitarian_reading, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__unitarian_reading, suppression_requirement, 325, 0.05).
narrative_ontology:measurement(bibl_su_t600, biblical_divine_nature__unitarian_reading, suppression_requirement, 600, 0.1).
narrative_ontology:measurement(bibl_su_t1100, biblical_divine_nature__unitarian_reading, suppression_requirement, 1100, 0.25).
narrative_ontology:measurement(bibl_su_t1550, biblical_divine_nature__unitarian_reading, suppression_requirement, 1550, 0.45).
narrative_ontology:measurement(bibl_su_t1750, biblical_divine_nature__unitarian_reading, suppression_requirement, 1750, 0.25).
narrative_ontology:measurement(bibl_su_t1900, biblical_divine_nature__unitarian_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(bibl_su_t2025, biblical_divine_nature__unitarian_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__unitarian_reading, 0.08).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% The biblical_divine_nature kernel decomposes into three constraint stories (unitarian, trinitarian, modalist) because each reading has a distinct ε, distinct beneficiary/victim structure, and distinct coordination function. The unitarian reading coordinates scripturalist dissent; the trinitarian reading coordinates institutional orthodoxy; the modalist reading coordinates modalist communities (historically marginal). They are linked by network.affects_constraints because they share the same kernel and their historical trajectories are causally entangled (councils, anathemas, reformations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__unitarian_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
