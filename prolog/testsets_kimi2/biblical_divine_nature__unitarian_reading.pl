% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Biblical Divine Nature
 *   domain: theology/religious_authority
 *
 * SUMMARY:
 *   The unitarian reading of the biblical divine nature constrains
 *   Christology and pneumatology by asserting numerical singularity: the
 *   Father alone is God, while the Son and Spirit are subordinate or created
 *   beings. It operates as a doctrinal boundary within Restorationist, Arian,
 *   and modern Unitarian communities, coordinating strict monotheism and flat
 *   ecclesiology while extracting theological authority from institutional
 *   hierarchies and credal orthodox parties that depend on trinitarian
 *   ontology. This story instantiates the unitarian reading of the contested
 *   biblical_divine_nature kernel, structurally foreclosing both trinitarian
 *   and modalist siblings.
 *
 * KEY AGENTS:
 *   - unitarian_exegetes: Primary agenda_setter (organized/global/constrained) â administers the reading and polices doctrinal boundaries.
 *   - monotheist_congregations: Primary beneficiary (moderate/local/identity_locked) â gains flat ecclesiology and direct monotheistic access.
 *   - institutional_church_hierarchy: Primary payer (institutional/global/mobile) â bears loss of sacramental and hierarchical legitimacy.
 *   - credal_orthodox_communities: Secondary payer (organized/global/constrained) â bears delegitimization and apologetic costs.
 *   - trinitarian_believers: Excluded voice (organized/global/constrained) â foreclosed by the reading's core premise.
 *   - historical_critical_scholars: Analytical observer (analytical/civilizational/analytical) â sees the full structure without theological commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.6).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.5).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Biblical Divine Nature").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '2703e510-e2d5-4006-83de-6ed63972c099').
narrative_ontology:cs_kernel_codification('2703e510-e2d5-4006-83de-6ed63972c099', fixed_text).
narrative_ontology:cs_authority_grounding('2703e510-e2d5-4006-83de-6ed63972c099', lineage).
narrative_ontology:cs_interpretation_layer_present('2703e510-e2d5-4006-83de-6ed63972c099').
narrative_ontology:cs_reading_relation('2703e510-e2d5-4006-83de-6ed63972c099', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('2703e510-e2d5-4006-83de-6ed63972c099', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('2703e510-e2d5-4006-83de-6ed63972c099', foundational, father_alone_is_god).
narrative_ontology:cs_axiom_status(father_alone_is_god, holdable).
narrative_ontology:cs_axiom_grounding('2703e510-e2d5-4006-83de-6ed63972c099', father_alone_is_god, theological).
narrative_ontology:cs_axiom('2703e510-e2d5-4006-83de-6ed63972c099', foundational, son_subordinate_created).
narrative_ontology:cs_axiom_status(son_subordinate_created, holdable).
narrative_ontology:cs_axiom_grounding('2703e510-e2d5-4006-83de-6ed63972c099', son_subordinate_created, theological).
narrative_ontology:cs_reference_frame('2703e510-e2d5-4006-83de-6ed63972c099', primitive_apostolic_monotheism).
narrative_ontology:cs_drift_state('2703e510-e2d5-4006-83de-6ed63972c099', post_nicene_orthodoxy, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2703e510-e2d5-4006-83de-6ed63972c099', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, monotheist_congregations).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_exegetes).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_church_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodox_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and defend exegetical works that restrict the title God to the Father alone, and police doctrinal boundaries within their communities against trinitarian and modalist interpretations. Their standing depends on maintaining this hermeneutical stance within a network of like-minded congregations and publishers.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_exegetes, agenda_setter,
    organized, generational, constrained, global).

% Gather for worship directed solely to the Father, understanding Jesus as a subordinate lord or created agent and the Spirit as a divine force or subordinate person. They avoid creedal formulations and hierarchical mediation, experiencing their community as a return to simple biblical practice. Leaving would mean abandoning their core religious identity and social world.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, monotheist_congregations, beneficiary,
    moderate, biographical, identity_locked, local).

% Claims sacramental and teaching authority derived from trinitarian ontology, where the divine Son mediates salvation and the Spirit guides the magisterium. The unitarian reading denies the metaphysical basis of this mediation, forcing the hierarchy to defend its legitimacy through alternative arguments or disciplinary enforcement against the reading's spread.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_church_hierarchy, payer,
    institutional, generational, mobile, global).

% Maintain identity through adherence to Nicene and Athanasian creeds. The unitarian reading classifies these creeds as corrupt philosophical innovations rather than apostolic teaching, imposing a constant cost of apologetic defense and identity maintenance against the charge of polytheism or Hellenization.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodox_communities, payer,
    organized, generational, constrained, global).

% Hold that the Son and Spirit are fully divine and co-equal with the Father. Within unitarian hermeneutical communities, this view is ruled out by definitional fiat; its adherents are excluded from teaching roles and fellowship unless they abandon the conviction.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_believers, excluded,
    organized, biographical, constrained, global).

% Study the historical development of Christology from earliest Christianity through the councils, analyzing textual evidence and socio-political context without committing to the theological authority of any single reading.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, historical_critical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves strict monotheism by assigning absolute deity exclusively to the Father, eliminating metaphysical complexity and providing a single, unmediated object of worship and obedience.
% TRANSFER_FUNCTION: Moves theological authority and ecclesial legitimacy from institutional hierarchies grounded in trinitarian ontology to flat congregational structures and individual exegetes who appeal directly to the biblical text; simultaneously demotes Christ and Spirit from divine to created or subordinate status.
% ABSENT_VOICES: Trinitarian believers and modalist communities are structurally excluded from the conversation within unitarian hermeneutical spaces; they would argue for the full divinity of the Son or the modal unity of God but are foreclosed by the reading's definitional premise that Father alone is God.
% DISAPPEARANCE_RATIONALE: If the unitarian reading vanished overnight, the communities that depend on it would reorganize around trinitarian or modalist frameworks, institutional hierarchies would recover their christological grounding, and the flat ecclesiology sustained by direct-to-Father theology would yield to mediating structures.
% FOUNDING_PROBLEM: How to preserve uncompromised Jewish-style monotheism in the face of New Testament texts and early Christian devotion that appear to elevate Jesus and the Spirit to divine status.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian and restorationist historians attest the problem is live, citing patristic Hellenization. Trinitarian historians and credal churches attest it was resolved at Nicaea and Constantinople. Critical biblical scholars and historians of religion outside both camps corroborate that the tension between Second-Temple Jewish monotheism and high Christology is genuine, while disputing that any single reading resolves it neutrally.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.60) because the reading imposes a permanent ontological cost on Christ and Spirit while diverting authority from hierarchical institutions. Suppression (0.50) reflects the active boundary maintenance required to sustain the reading against trinitarian hegemony and modalist alternatives. Theater ratio (0.40) captures the performative appeal to primitive biblical purity that exceeds what the textual evidence unambiguously delivers. Accessibility collapse is high (0.75): once the numerical-singularity premise is accepted, trinitarian readings appear as self-evident polytheism. Resistance is high (0.72) because the reading confronts seventeen centuries of credal orthodoxy. The temporal series shows oscillation corresponding to the reading's shifting political fortune â peaking under imperial suppression of orthodoxy, retreating under medieval consolidation, and resurging during reform movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as restoration of primitive truth and liberation from metaphysical obscurity. The payer seats experience the same arrangement as delegitimization of their foundational theology and institutional identity. The engine computes this divergence from the structural asymmetry: the institutional hierarchy has global scope and mobile exit (can mobilize counter-enforcement), while the congregations are identity-locked to the reading and the exegetes are professionally constrained by it.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian exegetes and monotheist congregations sit near the beneficiary end: the constraint subsidizes their theological coherence, anti-hierarchical ecclesiology, and textual hermeneutic. Institutional hierarchy and credal orthodox communities sit near the target end: the constraint extracts their ontological and legitimating foundations by denying the divinity of the mediating Son and guiding Spirit. The excluded trinitarian believers are positioned outside the directionality map â their exclusion is itself the enforcement mechanism that stabilizes the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem â preserving strict monotheism amid high New Testament Christology â remains contested but structurally live. The constraint persists because the textual tension is genuine and the coordination function (simple monotheism) is real, not merely because of institutional inertia. This prevents piton classification. At the same time, the clear asymmetric cost imposed on hierarchical and credal institutions prevents classification as pure rope. The tangled_rope typing captures both the genuine coordination and the asymmetric extraction without reducing the arrangement to either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_continuity,
    'Is the unitarian reading a historically continuous tradition from the ante-Nicene period, or a modern reconstruction reading antiquity anachronistically?',
    'Historical philology and patristic source analysis tracing subordinationist exegesis from the second through fourth centuries and its reactivation in the Reformation and Restorationist movements.',
    'If the reading is largely reconstructed, its authority_grounding shifts from lineage toward practice or extraction, weakening the coordination claim and potentially raising theater_ratio. If continuous, the lineage claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_continuity, empirical, 'Historical continuity of the unitarian reading').

omega_variable(
    sibling_reading_reversal,
    'Would adopting the trinitarian reading dissolve the current victim set or merely redistribute extraction to unitarian communities?',
    'Comparative structural analysis of the trinitarian_reading constraint story''s beneficiary and victim arrays against this reading.',
    'If the trinitarian reading simply inverts the victim-beneficiary structure, the kernel represents a zero-sum authority contest rather than a coordination problem; if it produces a different structural topology, the readings are not simple mirrors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_reversal, conceptual, 'Whether trinitarian reading reverses or restructures victimhood').

omega_variable(
    cs_framing_underdetermination,
    'Does the constraint rest on a fixed textual kernel (the Bible), or on the interpretive tradition that selects and translates the texts supporting numerical singularity?',
    'Text-critical analysis of canon boundaries, translation choices in key Christological passages, and hermeneutical tradition: if the reading requires a specific selection and translation regime to hold, the true kernel is the interpretive layer rather than the text itself.',
    'If the interpretive layer is the true kernel, the constraint''s authority_grounding is better described as practice or distributed than as lineage-grounded fixed_text, altering the false-summit evaluation and Boltzmann coordination classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is text or interpretive tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unitarian_rd_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unitarian_rd_tr_t6, biblical_divine_nature__unitarian_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(unitarian_rd_tr_t12, biblical_divine_nature__unitarian_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(unitarian_rd_tr_t18, biblical_divine_nature__unitarian_reading, theater_ratio, 18, 0.4).

% Extraction over time
narrative_ontology:measurement(unitarian_rd_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(unitarian_rd_be_t6, biblical_divine_nature__unitarian_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(unitarian_rd_be_t12, biblical_divine_nature__unitarian_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(unitarian_rd_be_t18, biblical_divine_nature__unitarian_reading, base_extractiveness, 18, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(unitarian_rd_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(unitarian_rd_su_t6, biblical_divine_nature__unitarian_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(unitarian_rd_su_t12, biblical_divine_nature__unitarian_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(unitarian_rd_su_t18, biblical_divine_nature__unitarian_reading, suppression_requirement, 18, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
