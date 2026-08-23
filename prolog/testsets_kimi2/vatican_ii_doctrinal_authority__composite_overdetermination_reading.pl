% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Doctrinal Authority â Composite Overdetermination Reading
 *   domain: ecclesiological/institutional/hermeneutics
 *
 * SUMMARY:
 *   This constraint story models Vatican II doctrinal authority under the
 *   composite overdetermination reading: the council was not a single
 *   coherent reform but a convergence of multiple distinct structural changes
 *   (liturgical, ecumenical, ecclesiological, political) that were packaged
 *   as a unified reform. The standing arrangement under contest is this
 *   unified packaging â the institutional and hermeneutic pressure to treat
 *   Vatican II as a single authoritative reform rather than a composite of
 *   independent shifts with different structural properties. The reading
 *   asserts that ambiguities are a structural feature, not a bug, and that
 *   the continuity versus rupture debate is a category error because
 *   different components exhibit different degrees of change.
 *
 * KEY AGENTS:
 *   - curial_hierarchy (agenda_setter, institutional power, constrained exit) â administers the unified reform narrative and retains interpretive control
 *   - progressive_reform_clergy (beneficiary, organized power, constrained exit) â advances reform agendas legitimized by the conciliar package
 *   - traditionalist_laity_clergy (payer, moderate power, identity-locked exit) â bears costs of liturgical and doctrinal displacement
 *   - ecumenical_partners (beneficiary, moderate power, mobile exit) â benefits from conciliar openings with voluntary participation
 *   - ecclesiastical_historians (observer, analytical power, analytical exit) â documents the composite drafting history without institutional power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.78).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Doctrinal Authority â Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiological/institutional/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'ae52787e-a53c-4ce1-9647-661fb845b874').
narrative_ontology:cs_kernel_codification('ae52787e-a53c-4ce1-9647-661fb845b874', fixed_text).
narrative_ontology:cs_authority_grounding('ae52787e-a53c-4ce1-9647-661fb845b874', lineage).
narrative_ontology:cs_interpretation_layer_present('ae52787e-a53c-4ce1-9647-661fb845b874').
narrative_ontology:cs_reading_relation('ae52787e-a53c-4ce1-9647-661fb845b874', vatican_ii_doctrinal_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('ae52787e-a53c-4ce1-9647-661fb845b874', vatican_ii_doctrinal_authority__rupture_progressive_reading, influences).
narrative_ontology:cs_reading_relation('ae52787e-a53c-4ce1-9647-661fb845b874', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, influences).
narrative_ontology:cs_axiom('ae52787e-a53c-4ce1-9647-661fb845b874', foundational, conciliar_composition_is_multi_track).
narrative_ontology:cs_axiom_status(conciliar_composition_is_multi_track, holdable).
narrative_ontology:cs_axiom_grounding('ae52787e-a53c-4ce1-9647-661fb845b874', conciliar_composition_is_multi_track, empirically_contingent).
narrative_ontology:cs_axiom('ae52787e-a53c-4ce1-9647-661fb845b874', foundational, unified_reform_frame_obscures_structural_divergence).
narrative_ontology:cs_axiom_status(unified_reform_frame_obscures_structural_divergence, holdable).
narrative_ontology:cs_axiom_grounding('ae52787e-a53c-4ce1-9647-661fb845b874', unified_reform_frame_obscures_structural_divergence, conventional).
narrative_ontology:cs_reference_frame('ae52787e-a53c-4ce1-9647-661fb845b874', conciliar_composite_convergence).
narrative_ontology:cs_drift_state('ae52787e-a53c-4ce1-9647-661fb845b874', post_conciliar_authoritative_implementation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ae52787e-a53c-4ce1-9647-661fb845b874', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_reform_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_laity_clergy).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, hermeneutic_of_reform).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliarism_as_authority_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the unified reform narrative through magisterial documents, episcopal appointments, and liturgical norms. Maintains that Vatican II represents a coherent reform while managing tensions between its components. Retains interpretive control and institutional flexibility by selectively emphasizing different conciliar documents in different contexts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, curial_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% Advance liturgical renewal, collegial governance, and pastoral adaptations under the authority of the conciliar package. Their reform agendas gain institutional legitimacy from the unified reform frame even when specific textual support is distributed unevenly across documents. They operate within the Church structure and depend on its sacramental system.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_reform_clergy, beneficiary,
    organized, generational, constrained, global).

% Bear the costs of liturgical disruption, doctrinal ambiguity, and the marginalization of pre-conciliar devotional practices and theological frameworks. Experience the unified reform as suppression of their spiritual and sacramental identity. Many remain within the Church because their religious identity is fused to its sacramental life and they cannot conceive of legitimate existence outside it, despite feeling targeted by reform implementation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_laity_clergy, payer,
    moderate, biographical, identity_locked, global).

% Engage in official theological dialogue and practical cooperation with the Catholic Church that was impossible before the conciliar opening. The unified reform frame provides institutional stability for these dialogues. Their participation is voluntary and they can disengage or downgrade relations if the Catholic side retracts conciliar commitments.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners, beneficiary,
    moderate, generational, mobile, global).

% Document the distinct drafting histories, competing theological currents, and political negotiations that produced the conciliar documents. Their research corroborates the composite multi-track nature of the council but they hold no institutional authority to alter the unified reform packaging.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecclesiastical_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates simultaneous institutional reforms across liturgy, ecumenical relations, ecclesiology, and political engagement under a single conciliar authority, preventing fragmentation into competing single-issue factions while allowing the Church to address multiple mid-20th-century crises within one institutional event.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional flexibility from a fixed pre-conciliar magisterial mode to a post-conciliar magisterium that can selectively emphasize different conciliar components in different contexts; moves the costs of liturgical disruption and doctrinal uncertainty onto traditionalist constituencies while opening institutional space for progressive and ecumenical advances.
% ABSENT_VOICES: Radical traditionalist theologians who read specific documents as outright rupture with prior teaching, progressive activists who interpret the spirit of the Council as mandating ongoing structural revolution beyond the texts, and separated Christian communities who might prefer unambiguous ecumenical clarity over diplomatic ambiguity. They are excluded from the unified-reform conversation by the institutional packaging that treats the council as a settled package.
% DISAPPEARANCE_RATIONALE: If the composite authority structure were fully exposed as a packaging of independent changes rather than a unified reform, the Church would likely reorganize into competing component-specific movements (liturgical traditionalists, ecumenical maximalists, political liberationists), each claiming conciliar mandate for their specific trajectory. The current institutional equilibrium depends on the unified frame suppressing this decomposition.
% FOUNDING_PROBLEM: The mid-20th-century Catholic Church faced simultaneous crises of liturgical participation, ecumenical isolation, excessive centralization, and declining political relevance in a secularizing Europe. Vatican II was convened to address all four crises within a single conciliar event.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical historians outside the progressive beneficiary camp, including traditionalist historians and academic historians of the Bologna School, attest the multiplicity of distinct agendas at the council. The curial hierarchy partially corroborates the founding problem's multiplicity but defends the unified packaging as necessary for institutional survival.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the unified packaging allows agenda-setters to exploit inter-document ambiguity to advance component-specific agendas while externalizing costs onto traditionalist constituencies. Suppression (0.78) is higher still because the constraint's persistence depends on actively suppressing both decompositional readings (that would break the council into separate components) and radical traditionalist rejection. Theater ratio (0.55) is elevated: maintaining the appearance of unified reform across incompatible components requires increasing performative effort. Measurements exhibit a cyclical pattern in suppression: relaxation under Benedict XVI (Summorum Pontificum, 2007) followed by re-intensification under Francis (Traditionis Custodes, 2021). The oscillation is itself an extraction mechanism â intermittent reinforcement of hope followed by restriction prevents stable exit and deepens identity lock-in. Accessibility collapse (0.62) reflects that once the unified reform frame is accepted, seeing the components separately becomes institutionally difficult. Resistance (0.74) is high due to sustained traditionalist opposition and progressive frustration with the limits of the unified packaging.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (curial hierarchy) experiences the unified reform as necessary institutional management of legitimate pluralism; the payer seat (traditionalists) experiences the same structure as suppression of their sacramental and spiritual identity. Progressive beneficiaries experience it as authorization for ongoing reform. The engine computes these divergences from the structural data â the composite reading does not resolve them but locates them in different conciliar components.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive clergy and ecumenical partners are structural beneficiaries (low directionality) because the unified packaging opens institutional space for their agendas. Traditionalists are structural targets (high directionality) because the same packaging suppresses pre-conciliar practices and marginalizes their hermeneutic. The curial hierarchy sits near the beneficiary end for the authority-flexibility dimension but bears coordination costs. Ecclesiastical historians sit at analytical distance with minimal directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The unified reform frame originated as a genuine coordination mechanism to prevent schism while addressing multiple crises. Over the 60-year interval, extraction has accumulated as the ambiguity of the composite has been progressively exploited by agenda-setters to advance component-specific agendas while suppressing others. The coordination function (preventing fragmentation) remains live, preventing pure snare classification, but the asymmetric extraction has grown substantially. The R5 genealogy interview shows contested founding-problem status: some components are solved, others transformed, yet the unified arrangement persists and enforces beyond its original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    component_extractiveness_variance,
    'Do the different components (liturgy, religious freedom, ecumenism, ecclesiology) exhibit sufficiently different extractiveness profiles that the composite constraint should be decomposed into separate stories per the epsilon-invariance principle?',
    'Independent epsilon assessment of each component against the framework, measuring whether liturgical, ecumenical, and ecclesiological changes produce divergent metric profiles under the same analytical lens.',
    'If variance is high, decomposition into separate constraint stories is warranted; the current composite would then become a constraint-family hub linking the component stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_extractiveness_variance, conceptual, 'Whether the composite constraint should decompose into component-specific stories').

omega_variable(
    ambiguity_as_feature_or_bug,
    'Are the ambiguities in conciliar documents a deliberately engineered structural feature enabling ongoing interpretation, or an accidental byproduct of political compromise between theological factions?',
    'Historical archival research on drafting processes, particularly the behind-the-scenes negotiations on Dignitatis Humanae, Lumen Gentium, and Sacrosanctum Concilium.',
    'If engineered, extraction is higher because ambiguity serves agenda-setters strategically; if accidental, extraction is lower because the coordination cost of managing unintended ambiguity is a genuine overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_feature_or_bug, empirical, 'Whether conciliar ambiguity was deliberate or accidental').

omega_variable(
    suppression_mechanism_ambiguity,
    'For the traditionalist payer group, is the measured suppression structural (canonical penalties, restricted access to the traditional liturgy, episcopal sanctions) or internalized (identity fusion with the pre-conciliar Church making exit psychologically and spiritually unthinkable)?',
    'Post-exit trajectory analysis of traditionalist communities that have left for canonically irregular jurisdictions (SSPX, sedevacantist groups) versus those who remain under restriction: does suppression persist after structural exit?',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because the target carries the suppression with them; this would raise computed extraction for identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for traditionalists').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2ca_codr_tr_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(v2ca_codr_tr_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(v2ca_codr_tr_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(v2ca_codr_tr_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(v2ca_codr_tr_t42, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 42, 0.48).
narrative_ontology:measurement(v2ca_codr_tr_t56, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 56, 0.52).
narrative_ontology:measurement(v2ca_codr_tr_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(v2ca_codr_be_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(v2ca_codr_be_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(v2ca_codr_be_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(v2ca_codr_be_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(v2ca_codr_be_t42, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 42, 0.65).
narrative_ontology:measurement(v2ca_codr_be_t56, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 56, 0.67).
narrative_ontology:measurement(v2ca_codr_be_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(v2ca_codr_su_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(v2ca_codr_su_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(v2ca_codr_su_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(v2ca_codr_su_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(v2ca_codr_su_t42, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 42, 0.7).
narrative_ontology:measurement(v2ca_codr_su_t56, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 56, 0.78).
narrative_ontology:measurement(v2ca_codr_su_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This composite constraint may decompose into separate component constraints (liturgical reform, ecumenical engagement, ecclesiological restructuring, political reorientation) per the epsilon-invariance principle. Each component likely carries a distinct extractiveness profile and may warrant its own constraint story linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
