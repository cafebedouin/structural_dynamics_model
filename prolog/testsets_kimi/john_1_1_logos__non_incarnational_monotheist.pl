% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__non_incarnational_monotheist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__non_incarnational_monotheist, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: john_1_1_logos__non_incarnational_monotheist
 *   human_readable: John 1:1 Logos â Non-Incarnational Monotheist Reading
 *   domain: theological/biblical_hermeneutics/christology
 *
 * SUMMARY:
 *   This constraint instantiates the non-incarnational monotheist reading of
 *   John 1:1, treating 'Logos' as poetic or functional language for divine
 *   wisdom, plan, or creative speech rather than as a distinct hypostasis or
 *   incarnate being. Within the historical-critical guild and certain
 *   non-creedal religious communities, this reading coordinates strict
 *   monotheism with Johannine prologue material by dissolving ontological
 *   claims into literary ones. Simultaneously, it extracts textual legitimacy
 *   and doctrinal coherence from creedal Trinitarian and sacramental
 *   traditions that depend on Logos-as-divine-person for their christological
 *   and liturgical economies. The reading is presented as a neutral
 *   philological recovery but functions as an authoritative boundary
 *   mechanism that invalidates incarnational exegesis as anachronistic.
 *
 * KEY AGENTS:
 *   - historical_critical_guild: Agenda-setter (institutional/analytical) â defines method and gatekeeps publication/translation
 *   - non_incarnational_monotheist_communities: Beneficiary (organized/identity_locked) â gains doctrinal coherence and textual inclusion without abandoning unitarian commitments
 *   - creedal_trinitarian_communities: Primary target (institutional/identity_locked) â loses foundational text for Nicene ontology and christological boundaries
 *   - sacramental_churches: Primary target (institutional/identity_locked) â loses textual grounding for sacramental economy tied to incarnate divine presence
 *   - interfaith_dialogue_advocates: Secondary beneficiary (moderate/mobile) â gains conversational space by removing the incarnation stumbling block
 *   - incarnational_systematic_theologians: Excluded voice (organized/constrained) â structurally absent from scholarly contexts where methodological naturalism rules out their hermeneutical framework a priori
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__non_incarnational_monotheist, 0.58).
domain_priors:suppression_score(john_1_1_logos__non_incarnational_monotheist, 0.62).
domain_priors:theater_ratio(john_1_1_logos__non_incarnational_monotheist, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, extractiveness, 0.58).
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(john_1_1_logos__non_incarnational_monotheist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__non_incarnational_monotheist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__non_incarnational_monotheist, "John 1:1 Logos â Non-Incarnational Monotheist Reading").
narrative_ontology:topic_domain(john_1_1_logos__non_incarnational_monotheist, "theological/biblical_hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__non_incarnational_monotheist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__non_incarnational_monotheist, 'f8924a9d-a2bf-4333-8184-4199a41a5710').
narrative_ontology:cs_kernel_codification('f8924a9d-a2bf-4333-8184-4199a41a5710', fixed_text).
narrative_ontology:cs_authority_grounding('f8924a9d-a2bf-4333-8184-4199a41a5710', expertise).
narrative_ontology:cs_interpretation_layer_present('f8924a9d-a2bf-4333-8184-4199a41a5710').
narrative_ontology:cs_reading_relation('f8924a9d-a2bf-4333-8184-4199a41a5710', john_1_1_logos__orthodox_christological, forecloses).
narrative_ontology:cs_reading_relation('f8924a9d-a2bf-4333-8184-4199a41a5710', john_1_1_logos__subordinationist, forecloses).
narrative_ontology:cs_axiom('f8924a9d-a2bf-4333-8184-4199a41a5710', foundational, logos_as_poetic_wisdom_not_hypostasis).
narrative_ontology:cs_axiom_status(logos_as_poetic_wisdom_not_hypostasis, holdable).
narrative_ontology:cs_axiom_grounding('f8924a9d-a2bf-4333-8184-4199a41a5710', logos_as_poetic_wisdom_not_hypostasis, empirically_contingent).
narrative_ontology:cs_axiom('f8924a9d-a2bf-4333-8184-4199a41a5710', foundational, strict_monotheism_precludes_incarnate_agency).
narrative_ontology:cs_axiom_status(strict_monotheism_precludes_incarnate_agency, holdable).
narrative_ontology:cs_axiom_grounding('f8924a9d-a2bf-4333-8184-4199a41a5710', strict_monotheism_precludes_incarnate_agency, deontological).
narrative_ontology:cs_reference_frame('f8924a9d-a2bf-4333-8184-4199a41a5710', jewish_monotheist_poetic_frame).
narrative_ontology:cs_drift_state('f8924a9d-a2bf-4333-8184-4199a41a5710', contemporary_global_christianity, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f8924a9d-a2bf-4333-8184-4199a41a5710', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__non_incarnational_monotheist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__non_incarnational_monotheist, non_incarnational_monotheist_communities).
narrative_ontology:constraint_beneficiary(john_1_1_logos__non_incarnational_monotheist, interfaith_dialogue_advocates).
narrative_ontology:constraint_victim(john_1_1_logos__non_incarnational_monotheist, creedal_trinitarian_communities).
narrative_ontology:constraint_victim(john_1_1_logos__non_incarnational_monotheist, sacramental_churches).
narrative_ontology:constraint_vindicates(john_1_1_logos__non_incarnational_monotheist, historical_critical_method).
narrative_ontology:constraint_vindicates(john_1_1_logos__non_incarnational_monotheist, non_incarnational_monotheism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces the historical-critical method in biblical studies through peer review, translation committees, curricula, and hiring. Maintains that incarnational readings are anachronistic projections. Benefits from institutional prestige and disciplinary continuity; exit means abandoning the methodological paradigm that constitutes professional competence.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, historical_critical_guild, agenda_setter,
    institutional, generational, analytical, global).

% Religious communities (Unitarian, certain progressive Jewish-Christian dialogue groups, strict monotheist bodies) who use this reading to claim Johannine authority without accepting incarnation. Receive doctrinal coherence and textual inclusion. Exit would require accepting Trinitarian or incarnational theology, which would rupture their foundational identity.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, non_incarnational_monotheist_communities, beneficiary,
    organized, civilizational, identity_locked, national).

% Historic Christian churches whose creeds, liturgies, and catechisms depend on Logos-as-divine-person. This reading strips their textual foundation and invalidates their christological boundaries. Exit would mean abandoning the Nicene-Constantinopolitan framework and the sacramental economy built upon it.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, creedal_trinitarian_communities, payer,
    institutional, civilizational, identity_locked, global).

% Churches whose sacramental theology â especially Eucharist as encounter with the incarnate Logos â requires a divine hypostasis in John 1:1. The reading dissolves the link between the prologue and their sacramental practice. Exit would require reconstructing sacramentality around non-incarnational categories.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, sacramental_churches, payer,
    institutional, civilizational, identity_locked, global).

% Facilitators of Jewish-Christian-Muslim dialogue who benefit from removing the incarnation stumbling block. The reading provides a shared textual ground that does not require Christian partners to defend ontological divinity. Exit is mobile because they can shift dialogue frames without identity rupture.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, interfaith_dialogue_advocates, beneficiary,
    moderate, biographical, mobile, global).

% Systematic theologians whose work assumes Chalcedonian ontology. In contexts where the non-incarnational reading dominates (certain academic journals, accrediting bodies, translation committees), their hermeneutical objections are structurally excluded by methodological prerequisites that rule out Nicene exegesis as non-scholarly.
narrative_ontology:constraint_stakeholder(john_1_1_logos__non_incarnational_monotheist, incarnational_systematic_theologians, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a strict monotheist reading of John 1:1 by allowing communities to incorporate the prologue into their theological framework without abandoning unitarian or non-incarnational commitments; solves the apparent tension between Jewish monotheism and Johannine high Christology by reinterpreting the tension as poetic rather than ontological.
% TRANSFER_FUNCTION: Moves interpretive authority and textual legitimacy from creedal incarnational traditions to non-incarnational monotheist communities and the historical-critical scholarly framework; transfers the power to define legitimate exegesis from confessional traditions to accredited academic guilds.
% ABSENT_VOICES: Incarnational systematic theologians and Nicene exegetes are structurally absent from the scholarly conversation in contexts where this reading is hegemonic; their objections are ruled out by methodological naturalism or historical-critical assumptions that precede textual engagement. Trinitarian scholars from the Global South are often underrepresented in translation committees and peer-review boards where this reading is enforced.
% DISAPPEARANCE_RATIONALE: If the non-incarnational reading vanished from academic and religious communities, strict monotheist groups would lose their primary hermeneutical path to claiming John 1:1; creedal and sacramental churches would recover the textual grounding for incarnational theology; seminary curricula, translation footnotes, and interfaith dialogue frameworks would reorganize around christological or agnostic interpretations.
% FOUNDING_PROBLEM: How to read John 1:1 without importing later fourth-century Trinitarian or incarnational theology into a first-century Jewish text; how to respect the historical particularity of the prologue against anachronistic ontological readings.
% FOUNDING_PROBLEM_CORROBORATION: Critical biblical scholars and Jewish historians of the Second Temple period attest the problem from outside the benefiting religious communities, arguing that Logos-language has Jewish wisdom precedents. Creedal theologians and patristics scholars attest that the problem is artificially manufactured by Enlightenment-era methodological naturalism and does not reflect the text's own communicative intent or the continuous reception history of the church. Independent historians of doctrine note that pre-Nicene readings were diverse, but this plurality does not uniquely corroborate the non-incarnational reading over subordinationist alternatives.
narrative_ontology:disappearance_verdict(john_1_1_logos__non_incarnational_monotheist, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__non_incarnational_monotheist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__non_incarnational_monotheist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__non_incarnational_monotheist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__non_incarnational_monotheist, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__non_incarnational_monotheist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__non_incarnational_monotheist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__non_incarnational_monotheist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-to-high because the reading strips a foundational prooftext from global institutional traditions that have organized their identity around it for millennia; the extraction is doctrinal and authority-based rather than financial. Suppression (0.62) reflects that incarnational readings are systematically ruled out in critical scholarly contexts by methodological commitments disguised as neutral philology. Theater ratio (0.45) captures the genuine historical-linguistic labor performed alongside the growing share of performative objectivity that masks theological pre-commitment. Accessibility collapse (0.48) is moderate: incarnational alternatives remain robust outside the academy but are increasingly collapsed within accredited biblical studies programs. Resistance (0.72) is high because the global creedal tradition actively contests the reading through apologetics, alternative scholarly societies, and confessional seminaries.
 *
 * PERSPECTIVAL GAP:
 *   The historical-critical guild experiences the constraint as a genuine coordination mechanism (solving the anachronism problem) and a scholarly advance; from this seat the reading is rope-like or minimally tangled. The creedal and sacramental communities experience the identical constraint as an ontological dismantling of their foundational text; from this seat the reading computes as substantially extractive or snare-like. The engine measures this divergence from the structural asymmetry in exit options (analytical vs. identity_locked) and the beneficiary-victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are non-incarnational monotheist communities and interfaith dialogue advocates, who receive low directionality (subsidized by the constraint's interpretive framework). The historical-critical guild, as agenda-setter with analytical exit, also sits near the beneficiary end despite not collecting rents directly. Targets are creedal Trinitarian and sacramental communities, whose identity-locked exit and institutional scope amplify their effective extraction. The excluded incarnational theologians sit at the high-target end because their structural relationship is one of total exclusion from the interpretive regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â avoiding anachronistic importation of later Trinitarian theology into a first-century Jewish text â remains live in historical scholarship, preventing a clean piton or snare classification. However, the constraint has accumulated extraction over time: what began as a methodological caution has hardened into a gatekeeping regime that disqualifies Nicene exegesis without engaging its arguments. The mandatrophy is partially resolved in the scholarly seat (the problem is still live) but unresolved in the ecclesial seat (the arrangement persists as an identity threat even where the philological caution has been absorbed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_apologetic_projection,
    'Does the non-incarnational reading recover the author''s intended meaning, or does it project modern monotheist concerns onto a first-century Jewish text?',
    'Archaeological and linguistic evidence from Second Temple Jewish literature; comparative analysis of wisdom terminology and the history of dubar/logos concepts in Hebrew and Aramaic sources.',
    'If the reading is apologetic projection, the constraint''s extraction is higher than its coordination merit and the historical-critical guild functions as an extractive gatekeeper; if original intent, the coordination function for monotheist communities is structurally stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_vs_apologetic_projection, empirical, 'Whether the reading is historically grounded or retrojective.').

omega_variable(
    methodological_neutrality_ambiguity,
    'Is the exclusion of incarnational readings in critical scholarship achieved by neutral philological method or by theological pre-commitment dressed as methodological neutrality?',
    'Examination of peer-review gatekeeping patterns, translation committee composition and decisions, and seminary curriculum design to detect systematic exclusion of Nicene exegetes independent of philological merit.',
    'If the latter, suppression is higher than structurally claimed and the constraint shifts toward snare; if the former, the enforcement is epistemically warranted and the tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_neutrality_ambiguity, conceptual, 'Whether scholarly method is genuinely neutral or masks theological pre-commitment.').

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the non_incarnational_monotheist reading of kernel john_1_1_logos; how would the classification change if the orthodox christological or subordinationist reading were adopted instead?',
    'Comparison of the completed sibling constraint stories in the john_1_1_logos kernel family.',
    'The orthodox christological reading would reinstitute sacramental authority and high coordination for creedal communities, likely computing as rope or mountain from the orthodox seat; the subordinationist reading would retain anti-incarnational extraction but reify Logos as a created being rather than poetic language, altering the beneficiary-victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta between sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__non_incarnational_monotheist, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_1_1_logos_nim_tr_t0, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(john_1_1_logos_nim_tr_t0, observed).
narrative_ontology:measurement(john_1_1_logos_nim_tr_t25, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(john_1_1_logos_nim_tr_t25, observed).
narrative_ontology:measurement(john_1_1_logos_nim_tr_t50, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 50, 0.35).
narrative_ontology:measurement_basis(john_1_1_logos_nim_tr_t50, observed).
narrative_ontology:measurement(john_1_1_logos_nim_tr_t75, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 75, 0.4).
narrative_ontology:measurement_basis(john_1_1_logos_nim_tr_t75, observed).
narrative_ontology:measurement(john_1_1_logos_nim_tr_t100, john_1_1_logos__non_incarnational_monotheist, theater_ratio, 100, 0.45).
narrative_ontology:measurement_basis(john_1_1_logos_nim_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(john_1_1_logos_nim_be_t0, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(john_1_1_logos_nim_be_t0, observed).
narrative_ontology:measurement(john_1_1_logos_nim_be_t25, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 25, 0.35).
narrative_ontology:measurement_basis(john_1_1_logos_nim_be_t25, observed).
narrative_ontology:measurement(john_1_1_logos_nim_be_t50, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 50, 0.45).
narrative_ontology:measurement_basis(john_1_1_logos_nim_be_t50, observed).
narrative_ontology:measurement(john_1_1_logos_nim_be_t75, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 75, 0.52).
narrative_ontology:measurement_basis(john_1_1_logos_nim_be_t75, observed).
narrative_ontology:measurement(john_1_1_logos_nim_be_t100, john_1_1_logos__non_incarnational_monotheist, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(john_1_1_logos_nim_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(john_1_1_logos__non_incarnational_monotheist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__non_incarnational_monotheist, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__non_incarnational_monotheist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__non_incarnational_monotheist, john_1_1_logos__subordinationist).

% DUAL FORMULATION NOTE:
% This constraint is one member of the john_1_1_logos kernel family. The kernel (John 1:1) supports multiple structurally distinct readings with different epsilon values, beneficiary sets, and classification profiles. Decomposition follows the epsilon-invariance principle: the orthodox, subordinationist, and non-incarnational readings are not the same constraint viewed from different angles but different constraints linked by shared textual material.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
