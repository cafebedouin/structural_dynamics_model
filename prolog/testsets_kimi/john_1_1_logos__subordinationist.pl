% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Subordinationist Logos Reading of John 1:1
 *   domain: theological/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   The subordinationist reading of John 1:1 construes Logos as a created,
 *   subordinate divine agent â the first and highest creation, but not
 *   co-eternal or consubstantial with the Father. This reading functions as a
 *   theological constraint that limits Christological worship (veneration
 *   permitted, latria prohibited) and undermines the sacramental and
 *   authority claims of high-church Nicene traditions. It is one reading of a
 *   contested kernel; siblings are the orthodox christological reading
 *   (co-eternal, consubstantial) and the non-incarnational monotheist reading
 *   (poetic/functional language, no distinct hypostasis).
 *
 * KEY AGENTS:
 *   - subordinationist_teaching_authority: Agenda setter (institutional/identity_locked) â administers the reading and enforces worship boundaries
 *   - subordinationist_communities: Beneficiary (organized/identity_locked) â receive theological identity and monotheistic boundary
 *   - high_church_traditions: Primary target (institutional/identity_locked) â bear cost of delegitimization
 *   - trinitarian_worshippers: Secondary target (moderate/constrained) â constrained devotional practice
 *   - nicene_theologians: Excluded voice (institutional/mobile) â would object but are outside the conversation
 *   - critical_biblical_scholars: Analytical observer (analytical/analytical) â sees the structural contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.58).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.62).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Subordinationist Logos Reading of John 1:1").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theological/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, 'd6004e11-60f3-49ae-88bf-ee4cc4596f40').
narrative_ontology:cs_kernel_codification('d6004e11-60f3-49ae-88bf-ee4cc4596f40', fixed_text).
narrative_ontology:cs_authority_grounding('d6004e11-60f3-49ae-88bf-ee4cc4596f40', lineage).
narrative_ontology:cs_interpretation_layer_present('d6004e11-60f3-49ae-88bf-ee4cc4596f40').
narrative_ontology:cs_reading_relation('d6004e11-60f3-49ae-88bf-ee4cc4596f40', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('d6004e11-60f3-49ae-88bf-ee4cc4596f40', john_1_1_logos__non_incarnational_monotheist, forecloses).
narrative_ontology:cs_axiom('d6004e11-60f3-49ae-88bf-ee4cc4596f40', foundational, logos_created_subordinate_hypostasis).
narrative_ontology:cs_axiom_status(logos_created_subordinate_hypostasis, holdable).
narrative_ontology:cs_axiom_grounding('d6004e11-60f3-49ae-88bf-ee4cc4596f40', logos_created_subordinate_hypostasis, theological).
narrative_ontology:cs_axiom('d6004e11-60f3-49ae-88bf-ee4cc4596f40', foundational, creator_creature_worship_boundary).
narrative_ontology:cs_axiom_status(creator_creature_worship_boundary, holdable).
narrative_ontology:cs_axiom_grounding('d6004e11-60f3-49ae-88bf-ee4cc4596f40', creator_creature_worship_boundary, theological).
narrative_ontology:cs_reference_frame('d6004e11-60f3-49ae-88bf-ee4cc4596f40', strict_monotheistic_hierarchy).
narrative_ontology:cs_drift_state('d6004e11-60f3-49ae-88bf-ee4cc4596f40', post_nicene_ecumenical_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('d6004e11-60f3-49ae-88bf-ee4cc4596f40', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_communities).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, trinitarian_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the doctrinal boundary that the Logos is the first and highest creation of the Father, subordinate in essence and not co-eternal. Teaches that Jesus may be venerated as the divine Son but not worshipped with the latria due to the Father alone. Catechizes members, polices interpretive boundaries, and disciplines deviations toward trinitarian worship. Their institutional legitimacy is fused with the correctness of this reading.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_teaching_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Gather around the subordinationist reading as a marker of theological fidelity and communal identity. They benefit from a clear boundary against what they regard as trinitarian idolatry and from the coherence of a strict monotheistic cosmology. Their worship practices, educational materials, and social relations are organized around the distinction between Creator and created Logos.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_communities, beneficiary,
    organized, generational, identity_locked, global).

% Maintain sacramental, liturgical, and hierarchical traditions whose authority depends on the full divinity of Christ. They bear the cost of delegitimization when the subordinationist reading spreads: their eucharistic theology, priestly mediation, and doxological language are ruled idolatrous or heterodox by the subordinationist frame. Their creedal authority and historical legitimacy are directly undermined.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_traditions, payer,
    institutional, civilizational, identity_locked, global).

% Within subordinationist communities or under subordinationist teaching, they are prohibited from praying to Jesus as God or participating in trinitarian doxology. They experience the constraint as a prohibition on their devotional instincts and a spiritual cost. Exit from the community is possible but carries family, social, and identity forfeitures.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, trinitarian_worshippers, payer,
    moderate, biographical, constrained, local).

% Represent the orthodox christological tradition that affirms the Logos as consubstantial and co-eternal with the Father. They would object that subordinationism abandons the Rule of Faith and the Nicene settlement, but they are structurally excluded from the subordinationist interpretive community and its teaching forums.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, nicene_theologians, excluded,
    institutional, civilizational, mobile, global).

% Study the philology and historical context of John 1:1 and Second Temple Jewish Logos traditions without enforcing doctrinal boundaries. They provide textual and historical analysis used by all sides but occupy an analytical seat outside the theological constraint.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, critical_biblical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves absolute monotheism by maintaining an ontological distinction between the uncreated Father and the created Logos; orders worship hierarchically so that divine worship (latria) is reserved for the Father alone.
% TRANSFER_FUNCTION: Moves authority and legitimacy from Nicene/trinitarian institutional traditions to subordinationist communities; moves Christ from the object of divine worship to the object of veneration/subordinate honor.
% ABSENT_VOICES: Orthodox christological theologians and high-church sacramental leaders are structurally excluded from subordinationist interpretive communities; they would argue that the reading abandons the apostolic Rule of Faith and the catholic creed.
% DISAPPEARANCE_RATIONALE: Subordinationist communities hold that removing the constraint would collapse into trinitarian idolatry; orthodox and high-church traditions hold that removing it would restore proper divine worship of Christ and legitimate sacramental authority. The rearrangement depends entirely on which seat is describing the disappearance.
% FOUNDING_PROBLEM: How to interpret the Johannine Logos without either collapsing Christian devotion into Jewish monotheism (denying Christ's significance) or abandoning monotheism by deifying a creature.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of early Christianity attest the 3rdâ4th century Christological controversy was genuine and contested. Nicene confessional bodies attest the problem was resolved at the ecumenical councils. Subordinationist authorities attest the problem remains live and that Nicaea introduced a non-apostolic innovation. No single corroborating source from entirely outside the theological dispute exists; the historical discipline provides partial external attestation of the controversy's reality but not of its resolution status.
narrative_ontology:disappearance_verdict(john_1_1_logos__subordinationist, contested).
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__subordinationist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.58 because the constraint extracts worship status from Christ and authority from trinitarian institutions. Suppression is 0.62: active enforcement through teaching authority and boundary maintenance is required to sustain the reading against overwhelming orthodox consensus. Theater ratio is 0.45: significant genuine exegetical and philosophical labor supports the reading, but a substantial portion of its maintenance is performative differentiation from Nicene orthodoxy. Accessibility collapse is 0.70: once the subordinationist frame is adopted, trinitarian alternatives appear as idolatry and collapse as live options. Resistance is 0.80: intense, continuous resistance from Nicene and trinitarian institutions across centuries.
 *
 * PERSPECTIVAL GAP:
 *   The subordinationist teaching authority experiences the constraint as preserving true monotheism and apostolic simplicity (coordination); the high-church and trinitarian payer seats experience it as a delegitimizing extraction of their core devotional and sacramental identity. The engine computes this divergence from shared structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The subordinationist teaching authority and communities are beneficiaries (low d): the constraint subsidizes their theological identity and boundary. High-church traditions and trinitarian worshippers are targets (high d): the constraint extracts worship status and authority from them. Nicene theologians are excluded (no d contribution). Critical scholars sit analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it carries a genuine coordination function (strict monotheism, Creator-creature distinction) alongside identifiable asymmetric extraction (victims: high-church traditions). Pure rope classification would ignore the victims; pure snare would ignore the genuine theological problem (monotheism) the reading addresses. The tangled rope classification captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    primitive_reading_vs_constructed_boundary,
    'Is the subordinationist reading the natural primitive Christian reading of John 1:1, or is it a later constructed boundary serving anti-Nicene institutional identity?',
    'Historical-philological analysis of pre-Nicene Logos usage against reconstruction of 4th-century polemical context.',
    'If primitive, the constraint''s extraction is lower (genuine coordination of early belief); if constructed, higher (identity enforcement extracting from rival traditions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primitive_reading_vs_constructed_boundary, conceptual, 'Whether the reading emerges naturally from the text or is a constructed polemical tool.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of trinitarian worship within subordinationist communities enforced through structural barriers (exclusion, teaching discipline) or internalized cognitive patterns (identity-fused horror of idolatry)?',
    'Post-exit trajectory study: does suppression of trinitarian instinct persist after structural barriers are removed?',
    'If internalized, effective suppression is higher than the structural measure suggests, amplifying extraction for identity-locked worshippers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in worship constraint.').

omega_variable(
    sibling_reading_interaction,
    'Does the subordinationist reading function primarily as a positive theological claim or as a negative boundary against orthodox christological readings?',
    'Discourse analysis of subordinationist teaching materials: ratio of positive Logos-exposition to anti-Trinitarian polemic.',
    'If primarily negative boundary, extraction from high-church traditions is the dominant function, supporting snare-like classification; if primarily positive, coordination function dominates, supporting rope-like classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_interaction, conceptual, 'Whether the reading is a positive doctrine or a negative boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% This constraint is the subordinationist reading of the John 1:1 kernel. It is structurally distinct from the orthodox christological reading (different epsilon, different victim/beneficiary structure) and the non-incarnational monotheist reading (different ontological commitment to Logos as hypostasis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
