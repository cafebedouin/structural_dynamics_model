% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Continuity Reading: Organic Development of Doctrine
 *   domain: religious/theological/ecclesiastical
 *
 * SUMMARY:
 *   Vatican II (1962–1965) promulgated 16 documents declaring pastoral and
 *   doctrinal reforms. This constraint story instantiates the continuity
 *   reading: Vatican II represents organic development of doctrine in
 *   unbroken continuity with tradition. The Council itself claimed this
 *   framing in its closing statements and in magisterial interpretation ever
 *   since (John Paul II's "hermeneutic of continuity," Benedict XVI's similar
 *   framing). This reading benefits progressive reformers by legitimating
 *   post-conciliar changes as faithful developments, and it serves the
 *   magisterial authority by preserving claims of doctrinal consistency. The
 *   constraint's operation is the institutional endorsement and propagation
 *   of this interpretive frame through teaching, textbooks, seminary
 *   formation, and the marginalization of competing readings. The
 *   claim/metric gap is deliberate and diagnostic: the continuity reading
 *   presents itself as a neutral description (rope: mere coordination of
 *   interpretation), but the authored metrics reflect substantial extractive
 *   operation—the suppression of alternative readings, the theater of
 *   'faithful interpretation' defending what amounts to interpretive
 *   authority capture.
 *
 * KEY AGENTS:
 *   - Progressive reformers (organized; constrained exit; beneficiary) — theology faculty, reform-aligned bishops, religious orders advocating pastoral modernization
 *   - Vatican II Commission members (institutional; analytical exit; agenda-setter) — authors of the 16 documents; set initial interpretive frame
 *   - Traditionalist clergy (organized; identity-locked exit; payer) — resistance to reforms; marginalized within official interpretation
 *   - Vatican doctrinal authority (institutional; analytical exit; agenda-setter/beneficiary) — maintains interpretive monopoly via magisterium
 *   - Lay faithful (powerless; trapped exit; beneficiary/payer) — experience reforms directly; no voice in interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.38).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.29).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Continuity Reading: Organic Development of Doctrine").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "religious/theological/ecclesiastical").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '7a2b0811-3760-4dbd-a344-2ce4594c113f').
narrative_ontology:cs_kernel_codification('7a2b0811-3760-4dbd-a344-2ce4594c113f', fixed_text).
narrative_ontology:cs_authority_grounding('7a2b0811-3760-4dbd-a344-2ce4594c113f', lineage).
narrative_ontology:cs_interpretation_layer_present('7a2b0811-3760-4dbd-a344-2ce4594c113f').
narrative_ontology:cs_reading_relation('7a2b0811-3760-4dbd-a344-2ce4594c113f', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('7a2b0811-3760-4dbd-a344-2ce4594c113f', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('7a2b0811-3760-4dbd-a344-2ce4594c113f', foundational, doctrine_unchanged_development_possible).
narrative_ontology:cs_axiom_status(doctrine_unchanged_development_possible, holdable).
narrative_ontology:cs_axiom_grounding('7a2b0811-3760-4dbd-a344-2ce4594c113f', doctrine_unchanged_development_possible, theological).
narrative_ontology:cs_axiom('7a2b0811-3760-4dbd-a344-2ce4594c113f', foundational, magisterial_fidelity_continuity_verifiable).
narrative_ontology:cs_axiom_status(magisterial_fidelity_continuity_verifiable, holdable).
narrative_ontology:cs_axiom_grounding('7a2b0811-3760-4dbd-a344-2ce4594c113f', magisterial_fidelity_continuity_verifiable, deontological).
narrative_ontology:cs_axiom('7a2b0811-3760-4dbd-a344-2ce4594c113f', secondary, hermeneutics_of_continuity_adequacy).
narrative_ontology:cs_axiom_status(hermeneutics_of_continuity_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('7a2b0811-3760-4dbd-a344-2ce4594c113f', hermeneutics_of_continuity_adequacy, instrumental).
narrative_ontology:cs_reference_frame('7a2b0811-3760-4dbd-a344-2ce4594c113f', organic_development_doctrine_framework).
narrative_ontology:cs_drift_state('7a2b0811-3760-4dbd-a344-2ce4594c113f', contemporary_post_2000, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a2b0811-3760-4dbd-a344-2ce4594c113f', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, vatican_ii_commission_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, vatican_doctrinal_authority).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, lay_faithful).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, lay_faithful).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of religious orders, theologians, clergy, and bishops who advocate for doctrinal development, pastoral adaptation, and engagement with modernity. They claim Vatican II validates their position: the Council's texts, read through the hermeneutic of continuity, permit and encourage the reforms they advocate. They benefit from a legitimating framework that allows change while claiming unbroken fidelity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers, beneficiary,
    organized, generational, constrained, global).

% The bishops, cardinals, and theologians who authored and negotiated the Council's 16 documents. They set the textual record and its initial interpretive frame. Their authority derives from magisterial office and the ecumenical council's formal status. They explicitly framed the Council as doctrinal development continuous with tradition, not rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, vatican_ii_commission_members, agenda_setter,
    institutional, civilizational, analytical, universal).

% Priests, bishops, and theologians who read Vatican II's texts as containing doctrinal errors, ambiguities, or substantive breaks with prior teaching. They resist the continuity framing as a rhetorical cover for rupture. They bear the cost of institutional conflict: their objections are marginalized as disobedience or lack of faith; their pastoral authority erodes as reform-aligned bishops advance.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_clergy, payer,
    organized, generational, identity_locked, global).

% The Congregation for the Doctrine of the Faith and papal teaching office. They maintain interpretive authority over the Council's meaning and application. The continuity reading preserves their authority as guardians of unchanging doctrine while permitting reform. The reading allows them to endorse post-conciliar changes as legitimate developments without claiming doctrinal novelty.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, vatican_doctrinal_authority, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, vatican_doctrinal_authority, beneficiary).

% Ordinary parish members experience the reforms directly: vernacular Mass, simplified rituals, new catechetical methods. They benefit from pastoral accessibility but lose (or gain, depending on perspective) traditional forms and certainty. They have no formal voice in doctrinal interpretation; their exit options are bounded by parish availability and family/cultural ties to Catholicism.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, lay_faithful, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, lay_faithful, payer).

% Scholars, theologians, and critics who argue Vatican II represents substantive doctrinal change or contains errors. Their position is marginalized within official Church teaching; they are excluded from shaping the received interpretation of the Council even though they command serious scholarly attention outside magisterial circles.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, rupture_reading_advocates, excluded,
    organized, generational, constrained, global).

% Academic historians and theologians who study Vatican II as a historical event. They analyze textual history, voting patterns, draft revisions, and context. Their work documents the tension between the Council's self-presentation (continuity) and the historical reality of substantial deliberation, compromise, and innovation in the texts.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, doctrinal_historians, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, vatican_doctrinal_authority).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a hermeneutic frame—the continuity reading—that permits the global Catholic Church to implement pastoral and doctrinal reforms while maintaining that doctrine itself is unchanged. Solves the coordination problem: how to reform practice without claiming doctrinal error in prior teaching, thus preserving institutional authority and Catholic identity across the transition.
% TRANSFER_FUNCTION: Moves interpretive authority from traditionalist voices to progressive interpreters: the continuity reading designates which reform proposals count as 'organic development' (legitimate) versus 'rupture' (illegitimate). Progressive reformers gain cultural and institutional legitimacy; traditionalists lose interpretive standing. The constraint transfers authority by framing the terms on which change is legible.
% ABSENT_VOICES: Scholars who read the Council as containing doctrinal errors or irreconcilable shifts are structurally excluded from shaping the received interpretation, despite their work being serious and scholarly. Traditionalist clergy and the lay faithful have input only through acceptance or resistance, not through formal voice in doctrinal interpretation.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished—if the Church acknowledged the Council as rupture or as composed of incompatible doctrinal claims—institutional authority would destabilize, post-conciliar reforms would lose their magisterial warrant, and the grounds for progressive change would shift from doctrinal legitimacy to pragmatic necessity or papal reinterpretation. The entire post-Vatican II reorganization of Catholic practice rests on this constraint's persistence.
% FOUNDING_PROBLEM: How to authorize pastoral and liturgical reforms (Mass in the vernacular, episcopal collegiality, engagement with modern thought, interfaith dialogue) without claiming the prior Church teaching was erroneous or that the magisterium contradicted itself—while preserving Catholic identity as a Church of unbroken tradition.
% FOUNDING_PROBLEM_CORROBORATION: The Vatican II Commission members and magisterial authority attest the founding problem is real and ongoing: every post-conciliar pope has invoked 'organic development' and the 'hermeneutic of continuity.' Independent scholars (Paul VI's correspondence, Archbishop Bugnini's diaries, draft-revision histories) document that this was indeed a central institutional concern. However, historians outside the benefiting parties (Faggioli, Ruffini, Alberigo) also document substantial doctrinal innovation during the Council's negotiations, suggesting the founding problem may be stated in way that presupposes the continuity answer.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) and rising early (peaks at t=30, then plateaus): the constraint's extractive force is the capture of interpretive authority. The continuity reading gives magisterial authority the power to designate which reforms are 'organic' and which are 'rupture,' which gives progressive reformers legitimacy provided they stay within the frame, and which margins traditionalists. Extractiveness peaks as the constraint hardens into institutional orthodoxy; it plateaus as resistance exhausts and the frame becomes hegemonic. Suppression is lower (0.29) because the constraint does not use crude coercion—it uses hermeneutics, teaching, institutional advancement, and the sheer weight of magisterial endorsement. Theater is low-to-moderate (0.22): the continuity reading does perform genuine interpretive work (bridging texts, finding doctrinal threads), but an increasing share of post-conciliar enforcement is theatrical—repeated invocation of 'fidelity to Vatican II' where fidelity is defined as accepting whatever reforms the interpreters say are faithful. The measurement grid is shared across all three metrics (one time series). Resistance (not measured in the scalar series but relevant to classification) is substantial (0.58): traditionalist clergy, some academic historians, and lay dissenters mount real intellectual and institutional resistance, which prevents the constraint from classifying as pure extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap is structural and vast. Progressive reformers and magisterial authority author this constraint as genuine coordination: the Council solved a real problem (how to modernize without breaking faith) and the continuity reading faithfully describes the result. Traditionalist clergy and scholarly historians outside the benefiting coalition author it as authority capture: the Council undertook substantive doctrinal shifts, and the continuity reading is a retrospective narrative that papers over the rupture to protect institutional authority. The engine computes per-seat type from power, exit, and beneficiary/victim data—it will classify this constraint differently for the agenda-setter seat than for the payer seat. That divergence is not an error; it is the measurement the story exists to yield.
 *
 * DIRECTIONALITY LOGIC:
 *   The progressive-reformer seat experiences d near beneficiary (d~0.2): they gain interpretive warrant and institutional advancement when their proposals align with the continuity frame. The traditionalist-clergy seat experiences d near target (d~0.75): their objections are redefined as disloyalty or hermeneutical error; they pay institutional cost (marginalization, career limitation, reduced pastoral authority) and gain no distributional benefit. The magisterial authority seat experiences d near the midpoint (d~0.5): it benefits from the frame (preserves its authority narrative) and pays a cost (must continuously reinterpret texts to maintain appearance of continuity while authorizing substantive change). The lay faithful sit near beneficiary-with-cost (d~0.35): they benefit from accessible liturgy and modern theology but lose traditional forms and pay indirectly through suppression of their traditionalist cohort's voice. This directionality divergence is the story's core: from the progressive seat this is rope (genuine coordination solving a real problem); from the traditionalist seat it is snare (a trap using doctrine as cover for authority capture).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is stated as a doctrinal question ('how to authorize change while claiming continuity'), but the actual institutional problem is an authority question ('how to preserve magisterial authority across a major shift'). The continuity reading answers both by designating doctrinal development as the mechanism—a claim about theology that also serves institutional power. Mandatrophy arises when the founding problem (authorization of change) outlives its solution (the need for continuity framing diminishes as post-conciliar reforms become normalized), but the constraint persists through institutional inertia and because the alternative (acknowledging rupture) would destabilize the Church's authority narrative. The measurement plateau at t=30-60 signals this: extractiveness and suppression hold steady rather than declining as one would expect if the problem were solved. The constraint persists beyond its functional life because abandoning the continuity reading would require the Church to renegotiate its fundamental authority claims—an exit cost prohibitive enough to sustain the constraint despite its marginal functional utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutic_adequacy_of_continuity_frame,
    'Is the continuity reading hermeneutically adequate to the Council''s actual textual content and historical deliberation, or does it require systematic downplaying of rupture signals in the texts?',
    'Close textual analysis of the 16 documents comparing final text to draft revisions, paired with analysis of bishops'' intervention records during the Council. Comparison with how other major councils (Trent, Vatican I) handled doctrinal clarification versus innovation.',
    'If continuity proves inadequate (forced readings required to maintain the frame), the reading reclassifies from rope (genuine coordination) toward snare (hermeneutic cover for authority capture). Adequate continuity would support the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_adequacy_of_continuity_frame, empirical, 'Whether continuity interpretation requires textual distortion or represents the texts'' plain sense.').

omega_variable(
    alternative_readings_institutional_exclusion,
    'Is the rupture reading and the composite reading excluded because they are hermeneutically inferior, or because they threaten institutional authority?',
    'Comparison of scholarly treatment: do magisterial sources engage the alternative readings on their merits and refute them, or do they marginalize them as disloyalty? Do academic historians (outside the benefiting coalition) find the alternatives intellectually defensible?',
    'If exclusion is authority-based rather than merit-based, suppression rises substantially and the constraint reclassifies toward snare. Merit-based exclusion would support rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_institutional_exclusion, empirical, 'Whether alternative readings are suppressed due to theological inferiority or institutional threat.').

omega_variable(
    doctrinal_content_actual_change,
    'Does the Church''s actual teaching on specific doctrines (collegial governance, liturgical language, other religions, marriage pastoral) represent organic development from prior teaching, or substantive change?',
    'Document-by-document comparison: pre-Vatican II magisterial teaching on collegiality, on vernacular liturgy, on non-Christian salvation, on contraception pastoral. Map whether Vatican II represents clarification, application of known principles, or novel theological content.',
    'Widespread substantive change would suggest the continuity frame is performing its function (authority capture via hermeneutic claim), raising extractiveness and theater. Genuine continuity would support the rope classification and lower theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_content_actual_change, empirical, 'Whether specific doctrinal content actually changed or developed.').

omega_variable(
    reading_as_kernel_or_interpretation,
    'Is this constraint properly understood as instantiating one reading of a kernel (the texts + their legitimacy), or is it really about the institutional capture of interpretation of a fixed kernel?',
    'Clarify the boundary: does the constraint''s persistence depend on one reading being true, or does it depend on one reading being endorsed by authority regardless of truth? If the latter, the constraint is better modeled as an institutional power structure than as a theological reading.',
    'If the constraint is reading-as-power rather than reading-as-truth, the classification should emphasize the snare/tangled-rope features of authority capture over the rope features of coordination. This affects how per-seat classification proceeds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_kernel_or_interpretation, conceptual, 'Whether the constraint is fundamentally about which reading is true or about who controls interpretation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of traditionalist clergy and alternative readings structural (institutional barriers, career consequences, exclusion from councils) or internalized (the clergy believe continuity is correct and suppress themselves)?',
    'Post-exit trajectories: do traditionalist clergy who leave or retire retain suppressive beliefs about interpretation, or do they become more open to alternative readings? Do they suppress out of conviction or institutional constraint?',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than the structural measure (the target carries the constraint''s logic with them). If structural, fixing it requires removing institutional barriers, not changing minds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of alternative readings is imposed structurally or internalized by targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__continuity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__continuity_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__continuity_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(vati_tr_t45, vatican_ii_authority__continuity_reading, theater_ratio, 45, 0.22).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__continuity_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__continuity_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__continuity_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__continuity_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(vati_be_t45, vatican_ii_authority__continuity_reading, base_extractiveness, 45, 0.37).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__continuity_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__continuity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__continuity_reading, suppression_requirement, 10, 0.21).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__continuity_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__continuity_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(vati_su_t45, vatican_ii_authority__continuity_reading, suppression_requirement, 45, 0.28).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__continuity_reading, suppression_requirement, 60, 0.29).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__continuity_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, post_conciliar_reform_legitimacy).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, catholic_authority_narrative).

% DUAL FORMULATION NOTE:
% vatican_ii_authority is a contested kernel with three structurally distinct readings instantiated in separate constraint stories. The continuity_reading (this file) frames Vatican II as doctrinal development in continuity with tradition. The rupture_reading frames it as substantive doctrinal break. The composite_overdetermination_reading frames it as an overdetermined composite unresolvable into either continuity or rupture. Each reading has different beneficiary structures, different ε values, different classifications. All three are linked via network.affects_constraints to signal that the dispute is about the SAME kernel (the Council's documents and their legitimacy) but that different parties read it differently and that institutional authority enforces one reading. The ε values are independent: continuity_reading's ε measures extraction when the continuity framing is the institutional norm; rupture_reading's ε would measure extraction when the rupture reading (or rejection of the Council) is institutional orthodoxy. They do not measure 'how much the Council extracted'; they measure 'how much extraction occurs when THIS reading is enforced as official doctrine.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, organized, 0.2).
constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, powerless, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
