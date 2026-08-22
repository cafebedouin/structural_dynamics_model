% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II as Organic Doctrinal Development (Continuity Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) produced documents that were ambiguous on key
 *   issues: the relationship between the pope's authority and episcopal
 *   collegiality, whether liturgical change was allowed or required, how to
 *   read religious freedom alongside prior natural-law doctrine, and whether
 *   ecumenical engagement represented genuine doctrinal shift or pastoral
 *   opening. The continuity reading interprets all these as developments
 *   within unchanged tradition—apparent novelties are explications of what
 *   was always implicitly contained in prior doctrine. This constraint is the
 *   hermeneutical frame through which the magisterium maintains interpretive
 *   authority over conciliar meaning. The story's referent is the standing
 *   arrangement: the claim that Vatican II is organic development assessed
 *   through the continuity reading's own epistemic standards (not against an
 *   external rupture-reading standard). Extractiveness is measured by how
 *   much the frame suppresses rival interpretations and transfers
 *   hermeneutical authority upward; theater ratio tracks how much of the
 *   magisterium's energy post-1965 went to performing doctrinal continuity
 *   rather than addressing substantive implementation.
 *
 * KEY AGENTS:
 *   - post_conciliar_magisterium: agenda-setter, institutional power; maintains the continuity frame as interpretive authority
 *   - orthodox_development_hermeneuticists: beneficiary, powerful; produce the intellectual work that sustains the frame
 *   - episcopal_conferences: secondary beneficiary/payer; gain autonomy but bear blame for implementation failures
 *   - parish_clergy: payer, identity-locked; expected to implement within the frame without questioning its coherence
 *   - traditionalist_critics: excluded, structurally trapped; barred from legitimate critique
 *   - progressive_reformers: excluded, organized; argue the frame restricts the Council's transformative potential
 *   - laity: secondary beneficiary/payer; experience genuine change but told it is not real change
 *   - academic_observers: analytical seat; can examine whether the frame is descriptive or constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.31).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II as Organic Doctrinal Development (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'e6235270-2e00-4271-8649-d28809df6c75').
narrative_ontology:cs_kernel_codification('e6235270-2e00-4271-8649-d28809df6c75', fixed_text).
narrative_ontology:cs_authority_grounding('e6235270-2e00-4271-8649-d28809df6c75', extraction).
narrative_ontology:cs_interpretation_layer_present('e6235270-2e00-4271-8649-d28809df6c75').
narrative_ontology:cs_reading_relation('e6235270-2e00-4271-8649-d28809df6c75', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6235270-2e00-4271-8649-d28809df6c75', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6235270-2e00-4271-8649-d28809df6c75', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('e6235270-2e00-4271-8649-d28809df6c75', foundational, apostolic_tradition_essentially_unchanged).
narrative_ontology:cs_axiom_status(apostolic_tradition_essentially_unchanged, holdable).
narrative_ontology:cs_axiom_grounding('e6235270-2e00-4271-8649-d28809df6c75', apostolic_tradition_essentially_unchanged, deontological).
narrative_ontology:cs_axiom('e6235270-2e00-4271-8649-d28809df6c75', secondary, magisterial_hermeneutical_monopoly).
narrative_ontology:cs_axiom_status(magisterial_hermeneutical_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('e6235270-2e00-4271-8649-d28809df6c75', magisterial_hermeneutical_monopoly, conventional).
narrative_ontology:cs_reference_frame('e6235270-2e00-4271-8649-d28809df6c75', pre_conciliar_doctrinal_framework).
narrative_ontology:cs_drift_state('e6235270-2e00-4271-8649-d28809df6c75', post_council_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e6235270-2e00-4271-8649-d28809df6c75', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, orthodox_development_hermeneuticists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, episcopal_conferences).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, parish_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, laity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, apostolic_continuity_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, organic_doctrinal_development_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The papal and episcopal teaching authority post-1965. Interprets Vatican II documents as internally consistent development of prior doctrine. Performs the hermeneutical labor of reading apparent novelties (religious freedom, ecumenism, liturgical vernacular) as explications of implicit prior teaching. Maintains interpretive authority over conciliar intent by controlling the frame through which implementation occurs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, mobile, universal).

% Gain institutional autonomy and pastoral flexibility within the continuity frame (can adapt liturgy, engage locally, implement pastoral experimentation). Pay by bearing responsibility for implementation failures — when local reforms run beyond the 'organic development' boundary, are blamed for misinterpretation rather than conciliar ambiguity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, episcopal_conferences, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, episcopal_conferences, payer).

% Expected to implement conciliar directives within a frame that reads all novelty as doctrinal continuity. Where implementation produces theological confusion or laity resistance, are corrected as misunderstanding the organic development principle rather than as subjects of genuine ambiguity in the conciliar text. Career and clerical identity depend on receiving this correction frame rather than questioning it.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, parish_clergy, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, parish_clergy, excluded).

% Theologians and ecclesiologists who defend the continuity reading through sophisticated hermeneutical analysis. Gain academic standing, institutional influence, and publication platforms by producing the interpretive labor that sustains the frame. Benefit from the constraint's persistence by remaining credentialed authorities on conciliar intent.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, orthodox_development_hermeneuticists, beneficiary,
    powerful, generational, arbitrage, universal).

% Argue that Vatican II authorizes ongoing reform beyond the conciliar texts and that reading the Council through organic development restricts its transformative potential. Are excluded from primary interpretive authority and labeled as misreading conciliar intent when they push boundaries.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_advocates, excluded,
    organized, generational, trapped, universal).

% Argue that Vatican II itself contains ruptures and ambiguities incompatible with prior tradition; that the continuity reading is hermeneutical fiction masking real change. Structurally locked into either accepting the frame or separating from institutional communion — no middle path of legitimate critique within the magisterium's hearing.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_critics, excluded,
    moderate, generational, constrained, regional).

% Experience conciliar reforms (vernacular liturgy, lay participation, ecumenical openness) as genuine novelties addressing their lived needs. Benefit from pastoral flexibility and cultural accommodation. Pay by being told their experience of change is illusion — that nothing fundamental changed, only explicit what was always implicit — which creates cognitive dissonance and doubt in their own perception.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, laity, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, laity, payer).

% Church historians and theological scholars who analyze the Council from outside institutional authority. Can examine whether the continuity frame is descriptively accurate or hermeneutically constructed as a legitimation story.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, academic_observers, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the magisterium's hermeneutical authority and doctrinal continuity claim by framing all conciliar reforms as developments rather than changes. Coordinates the post-conciliar Church around a unified narrative of unbroken apostolic tradition, preventing the fragmentation that would result from acknowledged institutional rupture.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from conciliar texts themselves (which are ambiguous) to the magisterium's interpretation of them. Transfers responsibility for implementation failures from the conciliar intent to local clergy and episcopal conferences who 'misapply' organic development principles. Transfers lived experience of change into the theological category of explicit explication of the implicit.
% ABSENT_VOICES: Progressive reformers who want the Council to authorize ongoing change beyond the texts; traditionalist critics who argue the Council contains real ruptures; historians who examine the Council's historical contingency rather than reading it through the organic development theological lens. These voices would testify that the continuity frame is a hermeneutical choice, not a structural fact.
% DISAPPEARANCE_RATIONALE: If the continuity frame disappeared and Vatican II were examined without the presupposition of organic development, the documents would be read as containing genuine tensions between pre-conciliar and post-conciliar ecclesiology. The magisterium would lose the hermeneutical tool that allows it to claim unbroken authority over interpretation. Theological schools would develop competing readings without a unifying frame. The lived experience of Catholics (that something significant changed) would align with theological analysis rather than being denied by it.
% FOUNDING_PROBLEM: After Vatican II, the Church faced interpretive chaos: the Council's documents were ambiguous and contradicted on key points (authority, liturgy, religious freedom, ecumenism). The magisterium needed a frame that would allow it to claim both that the Council was binding and that it did not rupture prior tradition. The continuity reading solved this by redefining 'rupture' as 'development' and 'ambiguity' as 'implicit-made-explicit.'
% FOUNDING_PROBLEM_CORROBORATION: The magisterium and continuity-reading theologians attest the problem was genuine doctrinal chaos requiring a unifying frame. Historians and progressive reformers attest that the founding problem is displaced: the real problem was that the Council genuinely changed doctrine and the continuity frame is a post-hoc narrative constructed to deny that fact. Academic theological scholarship, particularly since the 2000s, increasingly documents the conciliar ambiguities and the hermeneutical work required to read them as continuous rather than novel.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.31) because the constraint's operation is primarily hermeneutical rather than coercive: it does not prohibit alternative readings but rather privileges one reading through institutional authority and credentialing structures. Suppression is moderate (0.42) because traditionalist critics and progressive reformers face real institutional penalties for dissenting from the frame, but the suppression is not violent or absolute—schism and marginal communities exist. Theater ratio is high and rising (0.35→0.58) because an increasing share of post-conciliar magisterial activity consists of interpretive labor performed to reconcile the Council's documents with prior tradition: papal encyclicals, curial statements, and theological works devoted to explaining how changes are continuities. The measurement series shows suppression and theater stabilizing after t=30 (roughly 1995 onward), when the continuity frame became institutionally settled—the papal statements of John Paul II (1978–2005) and Benedict XVI (2005–2013) consistently performed the organic development reading, and by the time of Francis (2013–present) the frame is so embedded that enforcement requires less theatrical justification. Extractiveness remains relatively flat because the hermeneutical advantage does not accumulate; it must be re-performed at each generational transition.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute substantially different types from the engine. The magisterium (agenda_setter, beneficiary) will compute as rope or light tangled_rope—coordination function genuine (preventing interpretive chaos), low extraction from its perspective. Clergy and laity (secondary payers, identity-locked) will compute as tangled_rope or snare: they are coordinated around a narrative they experience as cognitively inconsistent, with no legitimate exit. The analytical observer seat (academic historians) will compute somewhere between rope and tangled_rope depending on the engine's evaluation of whether the coordination function (preventing chaos) outweighs the extraction function (suppressing alternative readings). This gap is exactly what the framework is designed to measure: does the same constraint look like genuine coordination from the seat with authority, and like enforced narrative closure from excluded seats?
 *
 * DIRECTIONALITY LOGIC:
 *   The post_conciliar_magisterium benefits from the continuity frame by maintaining interpretive authority—it remains the sole legitimate adjudicator of conciliar meaning. Orthodox_development_hermeneuticists benefit through credentialing and academic standing: they are the authorized specialists in demonstrating how changes are continuities. Episcopal_conferences gain autonomy in pastoral implementation but bear the cost of responsibility when implementations run beyond the frame's boundaries—they are nominally beneficiaries but functionally payers. Parish_clergy are structurally payers: their identity as legitimate teachers depends on accepting the frame, and their career prospects suffer if they question it; exit is identity_locked because questioning the frame requires leaving the priesthood. Progressive_reformers and traditionalist_critics are targets: they face institutional exclusion and their interpretations are dismissed as misreading conciliar intent. Laity are secondary payers: they experience genuine change but are told it is not real, which creates cognitive dissonance. Directionality overrides are not needed here; the structural data sufficiently differentiates the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—interpretive chaos after Vatican II—was real and serious. The continuity frame solved it by providing a unifying hermeneutical principle. However, the frame's persistence past t=30 (roughly 1995–2005) begins to show signs of mandatrophy: the founding problem (preventing theological fragmentation) remains live, but the solution (reading all change as continuity) has ossified into theatrical performance rather than solving the substantive problem. By the time of Pope Benedict XVI's 2009 comment that Vatican II contained a 'hermeneutics of rupture' that needed correction through a 'hermeneutics of continuity,' the frame had become self-aware of its own constructedness—the theater was visible. The constraint persists not because it still solves the original problem but because the institutional apparatus has become dependent on the narrative. The engine should detect this via the rising theater_ratio despite stable extractiveness: the work required to perform continuity is increasing even as the extraction gain flattens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_constructedness,
    'Is the continuity reading a genuine interpretation of Vatican II''s documents, or a hermeneutical construction that reconciles the documents to prior tradition through interpretive labor?',
    'Comparative analysis of the conciliar documents against pre-conciliar magisterial teaching without presupposing organic development; examination of whether the continuity reading requires adding premises not present in the texts themselves (e.g., ''implicit-made-explicit'' is an interpretive grid, not a discovery of the documents'' original meaning).',
    'If the reading is constructive rather than discovered, the constraint''s function is not coordination (preventing chaos) but enforced narrative closure (suppressing rival readings). This would reclassify the constraint from rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_constructedness, conceptual, 'Whether continuity reading discovers or constructs the Council''s meaning.').

omega_variable(
    identity_lock_persistence,
    'For parish clergy and bishops who privately doubt the continuity frame but publicly uphold it, is their suppression structural (institutional penalties for dissent) or internalized (identity fusion with magisterial authority)?',
    'Post-retirement testimony from clergy who leave the priesthood; career-track analysis of bishops who challenge the frame versus those who conform; ecclesiastical whistleblowing and depositions in canonical proceedings.',
    'If suppression is primarily internalized, the constraint''s effective suppression is higher than measured—targets carry the suppression with them after exit. This would support reclassification toward snare. If suppression is primarily structural, the distinction between trapped and constrained exit becomes clearer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Structural vs. internalized suppression mechanism for clergy bound by identity.').

omega_variable(
    organic_development_doctrine_falsifiability,
    'What would count as evidence that Vatican II is NOT organic development but genuine rupture? Is the organic development principle unfalsifiable (and therefore not a substantive empirical claim but a definitional commitment)?',
    'If the continuity reading survives every possible empirical challenge (all apparent novelties are reinterpreted as developments, all contradictions are reframed as tensions within tradition), then it is an unfalsifiable commitments protecting a narrative rather than a testable claim about the Council.',
    'If unfalsifiable, the constraint is not rope but performance-art snare: it appears to coordinate by preventing chaos, but actually prevents any empirical test of whether chaos prevention is real. This would suggest mandatrophy and theater-ratio dynamics are the primary signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organic_development_doctrine_falsifiability, conceptual, 'Whether the organic development principle is empirically testable or definitionally protected.').

omega_variable(
    conciliar_intent_attribution,
    'The conciliar documents were produced by hundreds of bishops with diverse theologies; which coalition''s intent is the ''true'' conciliar intent? Is there a single intent to discover, or did the Council produce ambiguous texts that different coalitions have legitimately read differently?',
    'Examination of the Council''s drafting history (diaries, debates, votes); analysis of whether the texts were compromise products designed to permit multiple readings; identification of which coalitions had hermeneutical authority at the Council versus after it.',
    'If the texts were deliberate compromises allowing multiple readings, then all readings are co-equal; the continuity reading gains no privileged access to intent. This would reframe the constraint as a power play to monopolize interpretation rather than as faithful development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_intent_attribution, empirical, 'Whether conciliar intent is discoverable or was deliberately ambiguous.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(vati_tr_t0, observed).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(vati_tr_t10, observed).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(vati_tr_t20, observed).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(vati_tr_t30, observed).
narrative_ontology:measurement(vati_tr_t45, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 45, 0.6).
narrative_ontology:measurement_basis(vati_tr_t45, observed).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement_basis(vati_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(vati_be_t0, observed).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement_basis(vati_be_t10, observed).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(vati_be_t20, observed).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(vati_be_t30, observed).
narrative_ontology:measurement(vati_be_t45, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 45, 0.3).
narrative_ontology:measurement_basis(vati_be_t45, observed).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement_basis(vati_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(vati_su_t0, observed).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement_basis(vati_su_t10, observed).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(vati_su_t20, observed).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(vati_su_t30, observed).
narrative_ontology:measurement(vati_su_t45, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 45, 0.42).
narrative_ontology:measurement_basis(vati_su_t45, observed).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(vati_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__continuity_reading, 0.14).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_doctrinal_authority kernel admits at least four distinct constraint stories, each with its own ε and classification. This continuity_reading has low ε on doctrinal change and moderate extractiveness through hermeneutical authority. The rupture_progressive_reading and rupture_traditionalist_reading have higher ε on the documents themselves (they treat the apparent novelties as real changes, not developments). The composite_overdetermination_reading has higher ε on the multiplicity of distinct changes being packaged as unified reform. All four stories share the same kernel (Vatican II's claimed meaning) but instantiate different readings of it. Each reading is held by different institutional parties; none forecloses the others within any single party's framework, though they compete for hermeneutical authority at the magisterial level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
