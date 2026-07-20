% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   domain: ecclesiological/institutional/hermeneutic
 *
 * SUMMARY:
 *   Vatican II is commonly debated as either continuity or rupture. This
 *   constraint instantiates the composite_overdetermination reading: the
 *   Council is not one shift but a convergence of multiple distinct
 *   structural changes (liturgical, ecumenical, ecclesiological, political)
 *   packaged as unified reform. The constraint is the doctrinal authority
 *   framework that enforces this composite packaging. The 'hermeneutic of
 *   continuity' and post-conciliar magisterial interventions actively enforce
 *   the unified narrative, suppressing both traditionalist rejection and
 *   progressive rupture readings. This reading rejects single Îµ measurement
 *   because each conciliar component has independent extractiveness; the
 *   continuity/rupture debate is a category error. The constraint is authored
 *   as a kernel reading of vatican_ii_doctrinal_authority, with siblings
 *   continuity_reading, rupture_progressive_reading, and
 *   rupture_traditionalist_reading.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium: agenda-setter (institutional/constrained exit) â administers unified hermeneutic, cannot repudiate Council without self-undermining
 *   - progressive_constituency: beneficiary (organized/constrained) â invokes composite 'Council' for reforms beyond specific texts
 *   - traditionalist_communities: payer (moderate/constrained) â bear costs of suppressed document-specific continuity assessment
 *   - systematic_theologians: payer (moderate/constrained) â analytical independence suppressed by unified 'spirit' framing
 *   - ecumenical_partners: beneficiary (organized/mobile) â gain from ecumenical openings bundled into unified prestige
 *   - scholarly_historians: observer (analytical/analytical) â corroborate composite reading from outside institutional power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Doctrinal Authority â Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiological/institutional/hermeneutic").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '4f5586c9-7dff-4de7-9777-81a1040cabbb').
narrative_ontology:cs_kernel_codification('4f5586c9-7dff-4de7-9777-81a1040cabbb', fixed_text).
narrative_ontology:cs_authority_grounding('4f5586c9-7dff-4de7-9777-81a1040cabbb', lineage).
narrative_ontology:cs_interpretation_layer_present('4f5586c9-7dff-4de7-9777-81a1040cabbb').
narrative_ontology:cs_reading_relation('4f5586c9-7dff-4de7-9777-81a1040cabbb', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f5586c9-7dff-4de7-9777-81a1040cabbb', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f5586c9-7dff-4de7-9777-81a1040cabbb', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('4f5586c9-7dff-4de7-9777-81a1040cabbb', foundational, council_as_irreducible_composite).
narrative_ontology:cs_axiom_status(council_as_irreducible_composite, holdable).
narrative_ontology:cs_axiom_grounding('4f5586c9-7dff-4de7-9777-81a1040cabbb', council_as_irreducible_composite, conventional).
narrative_ontology:cs_axiom('4f5586c9-7dff-4de7-9777-81a1040cabbb', foundational, ambiguity_as_intentional_structure).
narrative_ontology:cs_axiom_status(ambiguity_as_intentional_structure, holdable).
narrative_ontology:cs_axiom_grounding('4f5586c9-7dff-4de7-9777-81a1040cabbb', ambiguity_as_intentional_structure, conventional).
narrative_ontology:cs_reference_frame('4f5586c9-7dff-4de7-9777-81a1040cabbb', conciliar_composite_authority).
narrative_ontology:cs_drift_state('4f5586c9-7dff-4de7-9777-81a1040cabbb', post_conciliar_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4f5586c9-7dff-4de7-9777-81a1040cabbb', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, post_conciliar_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_constituency).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, systematic_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, pre_conciliar_liturgical_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the unified hermeneutic of the Council across sixteen documents, using magisterial authority to prevent both traditionalist rejection and progressive excess. Bears the burden of maintaining communion while managing conflicting implementations. Cannot repudiate the Council without undermining its own authority, but can modulate interpretation through encyclicals, liturgical directives, and episcopal appointments.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Invokes 'the Council' or 'the spirit of Vatican II' as blanket authority for liturgical experimentation, ecumenical outreach, and collegial governance reforms. Benefits from the composite packaging because selective citation of diverse conciliar threads constructs reform mandates beyond any single document's explicit text. Exit is constrained by the need to remain within canonical communion.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_constituency, beneficiary,
    organized, generational, constrained, global).

% Benefit from the ecumenical and interfaith openings of Nostra Aetate and Unitatis Redintegratio, which gain force from being packaged with the Council's overall prestige rather than standing as isolated conciliar concessions. Their engagement with Rome is stabilized by the unified reform narrative. Can disengage but lose the institutional relationship.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners, beneficiary,
    organized, biographical, mobile, global).

% Seek document-by-document assessment of conciliar continuity, particularly for Dignitatis Humanae and Nostra Aetate. Bear costs when the composite packaging labels their demands as 'rejecting the Council' rather than engaging specific texts. Marginalized within institutional structures; constrained exit because schism carries heavy spiritual and communal costs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_communities, payer,
    moderate, generational, constrained, global).

% Attempt to assess each conciliar constitution's degree of novelty independently, but the composite framing treats the Council as a single authoritative event whose 'spirit' transcends individual texts. Their analytical work is suppressed when it questions the unified reform narrative. Career and institutional mobility depend on affirming the composite authority structure.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, systematic_theologians, payer,
    moderate, generational, constrained, global).

% Maintain that Sacrosanctum Concilium authorized limited liturgical reform, not the wholesale replacement that occurred under the umbrella of 'the Council.' Their specific textual arguments are overridden by appeals to the Council's general reforming authority. Constrained exit because traditional liturgy was suppressed institutionally.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, pre_conciliar_liturgical_practitioners, payer,
    moderate, generational, constrained, global).

% Observe from outside the magisterial beneficiary structure that Vatican II was historically a convergence of distinct political and theological movements packaged as unified by conciliar ecclesiology. Their research corroborates the composite reading but they exercise no institutional power over interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, scholarly_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, post_conciliar_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains ecclesial unity by providing a single hermeneutic framework for diverse conciliar documents, preventing fragmentation into competing 'councils' (one liturgical, one political, one ecclesiological) and the schisms that would follow from treating each document as having independent authority.
% TRANSFER_FUNCTION: Moves interpretive authority from discrete document-specific exegesis to a unified 'spirit of the Council' that can be invoked selectively across domains, concentrating hermeneutic control in the magisterium and progressive implementers while extracting analytical autonomy from systematic theologians and traditionalist communities.
% ABSENT_VOICES: Traditionalist interpreters who want document-by-document continuity assessment; systematic theologians who want to treat each conciliar text as having independent doctrinal weight; pre-conciliar liturgical practitioners who see their practices suppressed under blanket 'reform' authority; rival confessional readings that would assess documents independently of conciliar prestige.
% DISAPPEARANCE_RATIONALE: If the composite packaging dissolved, the Catholic Church would face immediate hermeneutic fragmentation: Lumen Gentium, Sacrosanctum Concilium, Dignitatis Humanae, and Nostra Aetate would be assessed independently for doctrinal continuity, progressive factions would lose blanket authority for changes not textually grounded, the magisterium would need to adjudicate each document's specific claims rather than managing a unified 'Council' aura, and traditionalist communities would gain leverage to argue for selective rather than global acceptance.
% FOUNDING_PROBLEM: The need to implement multiple conciliar reforms (liturgical, ecumenical, ecclesiological, political) simultaneously without provoking schism or appearing to contradict prior magisterial teaching, while accommodating the divergent theological and political factions that had converged at the Council.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium (Benedict XVI's 2005 hermeneutic of continuity speech, John Paul II's interpretive directives) attests the problem as managing diverse conciliar outputs. Traditionalist historians (Romano Amerio, Athanasius Schneider) and independent academic historians of the Council (Giuseppe Alberigo's documentary history) attest from outside the beneficiary seats that the problem was manufactured by conflating distinct political and theological changes under manufactured unity.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68) because the composite packaging concentrates hermeneutic authority in the unified 'Council' brand, extracting analytical autonomy from theologians and traditionalists who want document-specific assessment. Suppression is higher (0.72) because the constraint's persistence depends on actively marginalizing both rupture readings (progressive and traditionalist) and document-specific exegesis that would fracture the composite. Theater is moderate-high (0.48): much post-conciliar magisterial activity (hermeneutic of continuity speeches, liturgical enforcement, episcopal appointments) is performative maintenance of the unified narrative rather than implementation of specific conciliar texts. Resistance is moderate (0.55) due to sustained traditionalist critique and some scholarly dissent. Accessibility collapse (0.60) is partial because alternative readings exist but are institutionally marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The magisterium seat computes toward rope (genuine coordination preventing schism while managing diverse reforms). The traditionalist and systematic theologian seats compute toward snare (forced acceptance of ambiguous composite authority suppressing specific textual analysis). The progressive constituency sits nearer beneficiary but is constrained by canonical limits. The engine derives this divergence from the same structural data: beneficiary/victim declarations plus exit modulation (constrained for all parties inside the Church, but directionality differentiated by who gains from ambiguity management).
 *
 * DIRECTIONALITY LOGIC:
 *   The post_conciliar_magisterium and progressive_constituency are structural beneficiaries: they collect interpretive authority and reform leverage from the composite packaging, yielding low directionality. Traditionalist_communities, systematic_theologians, and pre_conciliar_liturgical_practitioners are structural targets: they bear costs when the unified 'Council' narrative suppresses their specific textual and continuity arguments, yielding high directionality. Ecumenical_partners are secondary beneficiaries with mobile exit. No override is needed because beneficiary/victim declarations plus the constrained exit of internal parties already capture the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving the coordination function (preventing schism through unified hermeneutic) alongside the extraction function (suppressing document-specific assessment to manage political convergence). A pure rope reading would ignore the asymmetric extraction of analytical autonomy; a pure snare reading would ignore the genuine coordination problem of implementing sixteen diverse documents without institutional fracture. The founding problem remains contested but live, so the constraint is not yet a piton, though theater_ratio elevation indicates some performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    component_extractiveness_independence,
    'Can the extractiveness of each conciliar component (liturgy, religious freedom, ecumenism) be measured independently without decomposing the constraint into separate stories?',
    'Historical-theological analysis assessing each document''s degree of novelty against pre-conciliar magisterial teaching, combined with sociological measurement of implementation coercion in each domain.',
    'If components have wildly divergent extractiveness, the composite reading is validated and the constraint should be decomposed per the Îµ-invariance principle; if uniformly similar, the composite reading collapses toward a single-Îµ continuity or rupture reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_extractiveness_independence, empirical, 'Whether conciliar documents can be assessed independently for extractiveness').

omega_variable(
    ambiguity_intentionality,
    'Are the textual ambiguities in conciliar documents deliberate structural features enabling convergence, or incidental compromises from conciliar drafting politics?',
    'Archival analysis of conciliar commission minutes, relator reports, and modi to determine whether ambiguity was strategically preserved or fought over.',
    'If deliberate, the constraint remains tangled_rope (coordination through managed ambiguity); if incidental, it shifts toward snare (extraction layered onto texts that never authorized it) or piton (inertial maintenance of unintended ambiguities).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_intentionality, empirical, 'Whether ambiguities were intentional or incidental').

omega_variable(
    unified_packaging_necessity,
    'Was the unified packaging of Vatican II as ''one reform'' structurally necessary to prevent schism, or a constructed mechanism to concentrate interpretive authority?',
    'Comparative analysis with other ecumenical councils (Trent, Vatican I) that maintained clearer single-Îµ profiles without provoking institutional fracture.',
    'If necessary, the coordination fraction of the tangled rope rises; if constructed, the extraction fraction rises and the constraint edges toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_packaging_necessity, conceptual, 'Whether unified packaging was necessary coordination or constructed extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(v2composite_tr_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(v2composite_tr_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(v2composite_tr_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(v2composite_tr_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(v2composite_tr_t40, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(v2composite_tr_t50, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 50, 0.46).
narrative_ontology:measurement(v2composite_tr_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(v2composite_be_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(v2composite_be_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(v2composite_be_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(v2composite_be_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(v2composite_be_t40, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(v2composite_be_t50, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(v2composite_be_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(v2composite_su_t0, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(v2composite_su_t10, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(v2composite_su_t20, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(v2composite_su_t30, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(v2composite_su_t40, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(v2composite_su_t50, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(v2composite_su_t60, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vatican_ii_doctrinal_authority kernel. The kernel decomposes into four structurally distinct constraints because each reading assigns different Îµ, different beneficiary/victim structures, and different directionality. The composite_overdetermination reading specifically treats the council as irreducibly multiple, making single-Îµ measurement a category error. See sibling constraints for continuity and rupture readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
