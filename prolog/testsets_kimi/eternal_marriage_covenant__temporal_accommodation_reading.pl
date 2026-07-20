% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Temporal Accommodation Reading of Eternal Marriage Covenant
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto suspended the practice of plural marriage in the LDS
 *   church without removing D&C 132 from scripture or renouncing its status
 *   as an eternal principle. This constraint story models the
 *   temporal_accommodation_reading of the eternal_marriage_covenant kernel:
 *   federal pressure creates a temporary suspension of practice, leaving the
 *   doctrine dormant but formally valid. The reading treats the Manifesto not
 *   as prophetic override or doctrinal repudiation, but as a subordination of
 *   celestial command to civil law under duress.
 *
 * KEY AGENTS:
 *   - church_institutional_leadership: agenda_setter and beneficiary (institutional/constrained) â preserves authority and institutional existence by administering the suspension.
 *   - theological_traditionalists: primary payer (moderate/identity_locked) â bear the theological cost of indefinitely suspended doctrine they believe required for salvation.
 *   - existing_plural_families: payer (powerless/trapped) â bear legal and social precarity as their formerly sanctioned family structure is abandoned.
 *   - dissenting_apostles: excluded (moderate/trapped) â silenced to maintain unanimous institutional compliance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.72).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.74).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.63).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.63).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Temporal Accommodation Reading of Eternal Marriage Covenant").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, 'bd459f80-42ad-41bb-878b-827c932849b9').
narrative_ontology:cs_kernel_codification('bd459f80-42ad-41bb-878b-827c932849b9', fixed_text).
narrative_ontology:cs_authority_grounding('bd459f80-42ad-41bb-878b-827c932849b9', lineage).
narrative_ontology:cs_interpretation_layer_present('bd459f80-42ad-41bb-878b-827c932849b9').
narrative_ontology:cs_reading_relation('bd459f80-42ad-41bb-878b-827c932849b9', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd459f80-42ad-41bb-878b-827c932849b9', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('bd459f80-42ad-41bb-878b-827c932849b9', foundational, civil_law_obedience_over_celestial_command).
narrative_ontology:cs_axiom_status(civil_law_obedience_over_celestial_command, holdable).
narrative_ontology:cs_axiom_grounding('bd459f80-42ad-41bb-878b-827c932849b9', civil_law_obedience_over_celestial_command, deontological).
narrative_ontology:cs_axiom('bd459f80-42ad-41bb-878b-827c932849b9', foundational, dormant_doctrine_retains_binding_force).
narrative_ontology:cs_axiom_status(dormant_doctrine_retains_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('bd459f80-42ad-41bb-878b-827c932849b9', dormant_doctrine_retains_binding_force, deontological).
narrative_ontology:cs_reference_frame('bd459f80-42ad-41bb-878b-827c932849b9', celestial_marriage_as_eternal_law).
narrative_ontology:cs_drift_state('bd459f80-42ad-41bb-878b-827c932849b9', post_federal_accommodation_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('bd459f80-42ad-41bb-878b-827c932849b9', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, church_general_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, theological_traditionalists).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Manifesto and subsequent enforcement, interpreting D&C 132 as eternally valid but currently suspended. Must preserve institutional existence and property while maintaining prophetic credibility. Their authority depends on managing the tension between the fixed text and the political accommodation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership, beneficiary).

% Receive continued institutional existence and mainstream social acceptance in exchange for accepting the doctrinal limbo around plural marriage. Their communal identity and salvation ordinances remain available only through compliance with the suspended-practice order.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, church_general_membership, beneficiary,
    organized, generational, identity_locked, national).

% Hold that D&C 132 is an immutable requirement for exaltation. Experience profound cognitive dissonance and spiritual cost when the church suspends practice without renouncing doctrine. They are expected to comply with the Manifesto despite believing it imperils eternal progression.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, theological_traditionalists, payer,
    moderate, civilizational, identity_locked, national).

% Lived in legally and theologically sanctioned plural families before the Manifesto. After 1890 they face federal prosecution, social stigma, and loss of institutional cover. The church leadership urges them to comply with federal law, leaving their family structures in legal limbo.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, existing_plural_families, payer,
    powerless, biographical, trapped, national).

% Senior leaders who regard the Manifesto as a departure from eternal principle. Silenced through removal from leadership, loss of platform, or excommunication when they continue to perform plural marriages. Their exclusion is structurally necessary to maintain the appearance of unanimous institutional compliance.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, dissenting_apostles, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective survival of the religious community under existential federal pressure by unifying compliance with federal law while preserving doctrinal continuity and communal identity.
% TRANSFER_FUNCTION: Moves obedience and theological compliance from traditionalists and plural families to institutional leadership, who trade active practice for federal toleration and institutional preservation; moves the legitimacy-cost of doctrinal dormancy onto the membership.
% ABSENT_VOICES: Dissenting apostles and underground practitioners who regard the Manifesto as apostasy are excluded from the institutional conversation; they would argue for continued practice as essential to exaltation but are silenced through excommunication or removal from leadership.
% DISAPPEARANCE_RATIONALE: If this constraint vanishedâmeaning the church either fully renounced D&C 132 or resumed open plural marriageâthe institutional structure would reorganize: renunciation would alienate traditionalists and rewrite theology; resumption would trigger federal and social destruction. The current liminal arrangement holds these forces in equilibrium.
% FOUNDING_PROBLEM: Federal seizure of church property, imprisonment of leaders, and legal dissolution of the corporate church threatened the community's annihilation in the late 19th-century United States.
% FOUNDING_PROBLEM_CORROBORATION: Federal government historical records and Supreme Court decisions (e.g., Late Corp. of the Church of Jesus Christ of Latter-Day Saints v. United States, 1890) attest to the existential threat from outside the beneficiary set. Secular historians corroborate that the threat subsided by the mid-20th century. The church's own historical department documents the federal pressure as a resolved historical episode, not a current condition.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.72) is high because the constraint imposes indefinite theological limbo on traditionalists and legal precarity on plural families to secure institutional survival. Suppression (0.74) reflects active church discipline (excommunication of post-Manifesto practitioners) layered atop historical federal prosecution. Theater ratio (0.63) has risen over the interval as the original federal threat disappeared but the doctrinal dormancy is maintained as a respectability performance. Accessibility collapse (0.78) is high because once the Manifesto is accepted as prophetic guidance, open plural marriage becomes institutionally unthinkable. Resistance (0.58) is moderate: underground movements (FLDS) and historical dissenting apostles attest to ongoing opposition.
 *
 * PERSPECTIVAL GAP:
 *   The leadership seat experiences the constraint as necessary crisis coordination that saved the church; traditionalist and plural-family seats experience it as coerced abandonment of a salvific requirement. The engine will compute high directionality (d near 1.0) for the victim seats and low directionality (d near 0.0) for the beneficiary/agenda-setter seat, producing divergent per-seat classifications from the same structural arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership is the structural beneficiary (d near 0): the constraint delivers institutional survival and maintained authority. Church general membership sits near symmetric (d ~0.4) because they gain institutional continuity while accepting diffuse theological cost. Theological traditionalists and existing plural families are structural victims (d near 1.0): they bear concentrated theological and legal costs with identity-locked or trapped exit options. Dissenting apostles are excluded entirely, their opposition constituting the suppressed alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfederal annihilation of the churchâis dead, yet the constraint persists and is actively enforced. This creates a mandatrophy mismatch signal (founding_problem_status: dead, disappearance_verdict: world_rearranges). However, the constraint is not a pure piton because its identity_coordination function (maintaining mainstream Christian legitimacy and internal orthodoxy) remains live. The rising theater_ratio captures the drift from crisis response to performative maintenance, but the active enforcement against modern polygamists prevents a pure piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Is the temporal accommodation reading a genuine doctrinal category within the kernel, or is it a post-hoc rationalization of political capitulation?',
    'Historical-theological analysis of pre-1890 church leader statements about ''laws of the land'' versus doctrine; examination of whether ''suspension without renunciation'' appears as a theological category before federal pressure.',
    'If post-hoc rationalization, the constraint''s coordination function is weaker and its extraction (theological obedience under duress) stronger, pushing classification toward snare. If genuine theological category, the tangled_rope claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Whether the temporal accommodation is a genuine doctrinal category or political rationalization.').

omega_variable(
    doctrine_restoration_possibility,
    'Will the church ever restore plural marriage practice if political constraints remain lifted, or is the doctrine permanently dead despite its formal dormancy?',
    'Longitudinal observation of church curriculum, temple sealing practices, and prophetic statements across generations; a formal retraction of D&C 132 or a restoration signal would resolve.',
    'If permanently dead, the constraint is a piton (atrophied function with theatrical maintenance of doctrine). If restoration remains structurally possible, the constraint remains a genuine tangled_rope coordinating present survival against future return.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_restoration_possibility, empirical, 'Whether the doctrinal suspension is truly temporary or permanently atrophied.').

omega_variable(
    reading_relation_ambiguity,
    'Does the temporal accommodation reading logically foreclose the immutable commandment reading, or can they coexist as paradoxical commitments within a single theological framework?',
    'Theological analysis of whether ''immutable requirement for exaltation'' is compatible with ''indefinitely suspended by civil law'' within the same doctrinal framework; historical study of whether the church''s own theology treats divine commands as contextually overridable.',
    'If logically foreclosed, the temporal reading structurally displaces the immutable reading, increasing the constraint''s extractiveness for traditionalists. If coexisting as paradox, the constraint''s classification retains its tangled character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_ambiguity, conceptual, 'Logical relationship between temporal accommodation and immutable commandment readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emcta_tr_t0, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(emcta_tr_t15, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(emcta_tr_t35, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 35, 0.48).
narrative_ontology:measurement(emcta_tr_t65, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 65, 0.55).
narrative_ontology:measurement(emcta_tr_t95, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 95, 0.6).
narrative_ontology:measurement(emcta_tr_t130, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 130, 0.63).

% Extraction over time
narrative_ontology:measurement(emcta_be_t0, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(emcta_be_t15, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(emcta_be_t35, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 35, 0.63).
narrative_ontology:measurement(emcta_be_t65, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 65, 0.67).
narrative_ontology:measurement(emcta_be_t95, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 95, 0.7).
narrative_ontology:measurement(emcta_be_t130, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 130, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(emcta_su_t0, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(emcta_su_t15, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(emcta_su_t35, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 35, 0.75).
narrative_ontology:measurement(emcta_su_t65, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 65, 0.7).
narrative_ontology:measurement(emcta_su_t95, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 95, 0.72).
narrative_ontology:measurement(emcta_su_t130, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 130, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the eternal_marriage_covenant kernel, decomposed from the colloquial label 'Mormon polygamy doctrine' into three structurally distinct claims. Each reading carries a different epsilon, beneficiary/victim structure, and classification. The upstream kernel (D&C 132) is treated as fixed text; the three readings are divergent interpretive responses to federal pressure and prophetic authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
