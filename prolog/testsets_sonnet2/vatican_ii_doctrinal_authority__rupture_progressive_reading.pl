% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_progressive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_progressive_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_progressive_reading
 *   human_readable: Vatican II as Rupture Authorizing Ongoing Reform ('Spirit of the Council' Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   In the decades following the Second Vatican Council (1962-1965), a
 *   dominant strand of implementation held that the Council represented a
 *   decisive, necessary rupture with a rigid pre-conciliar Church, and that
 *   its true meaning ('the spirit of the Council') licensed reforms —
 *   liturgical, catechetical, disciplinary — beyond anything stated in the
 *   sixteen conciliar documents themselves. This reading became
 *   institutionally load-bearing: episcopal conferences, seminaries, and
 *   liturgical commissions built authority and career structures on the
 *   premise that the Council's intent exceeded its text, and that resisting
 *   further reform was resisting the Council itself. Traditionalist clergy,
 *   laity, and pre-existing religious orders bore the cost of implementation
 *   that outran explicit textual mandate, with limited institutional
 *   recourse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.58).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.52).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "Vatican II as Rupture Authorizing Ongoing Reform ('Spirit of the Council' Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'c7528160-01ca-4b90-9d15-0b7a16869fb3').
narrative_ontology:cs_kernel_codification('c7528160-01ca-4b90-9d15-0b7a16869fb3', fixed_text).
narrative_ontology:cs_authority_grounding('c7528160-01ca-4b90-9d15-0b7a16869fb3', extraction).
narrative_ontology:cs_interpretation_layer_present('c7528160-01ca-4b90-9d15-0b7a16869fb3').
narrative_ontology:cs_reading_relation('c7528160-01ca-4b90-9d15-0b7a16869fb3', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7528160-01ca-4b90-9d15-0b7a16869fb3', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, influences).
narrative_ontology:cs_axiom('c7528160-01ca-4b90-9d15-0b7a16869fb3', foundational, conciliar_intent_exceeds_conciliar_text).
narrative_ontology:cs_axiom_status(conciliar_intent_exceeds_conciliar_text, holdable).
narrative_ontology:cs_axiom_grounding('c7528160-01ca-4b90-9d15-0b7a16869fb3', conciliar_intent_exceeds_conciliar_text, conventional).
narrative_ontology:cs_axiom('c7528160-01ca-4b90-9d15-0b7a16869fb3', foundational, religious_freedom_declaration_constitutes_doctrinal_reversal_of_syllabus).
narrative_ontology:cs_axiom_status(religious_freedom_declaration_constitutes_doctrinal_reversal_of_syllabus, holdable).
narrative_ontology:cs_axiom_grounding('c7528160-01ca-4b90-9d15-0b7a16869fb3', religious_freedom_declaration_constitutes_doctrinal_reversal_of_syllabus, conventional).
narrative_ontology:cs_axiom('c7528160-01ca-4b90-9d15-0b7a16869fb3', secondary, ongoing_reform_beyond_text_is_authentically_conciliar).
narrative_ontology:cs_axiom_status(ongoing_reform_beyond_text_is_authentically_conciliar, holdable).
narrative_ontology:cs_axiom_grounding('c7528160-01ca-4b90-9d15-0b7a16869fb3', ongoing_reform_beyond_text_is_authentically_conciliar, instrumental).
narrative_ontology:cs_reference_frame('c7528160-01ca-4b90-9d15-0b7a16869fb3', immediate_post_conciliar_reform_consensus).
narrative_ontology:cs_drift_state('c7528160-01ca-4b90-9d15-0b7a16869fb3', contemporary_magisterial_re_anchoring, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c7528160-01ca-4b90-9d15-0b7a16869fb3', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_reform_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, liturgical_renewal_institutes).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_religious_orders).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, parishes_subject_to_mandated_liturgical_change).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, development_of_doctrine_as_open_ended_process).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, religious_freedom_as_theological_advance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National and regional bishops' conferences that implemented sweeping liturgical, catechetical, and disciplinary changes in the years after the Council, citing the 'spirit of the Council' to authorize reforms not explicit in the conciliar texts. They control seminary formation, liturgical translation commissions, and diocesan policy, and their authority to keep extending reform depends on the rupture reading remaining institutionally dominant.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_episcopal_conferences, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_episcopal_conferences, beneficiary).

% Academic theologians whose careers, journals, and institutional positions were built on reading Vatican II as a decisive break enabling further doctrinal development. They gain intellectual authority and appointments to the degree the rupture-progressive reading is treated as the Council's authentic self-understanding.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_reform_theologians, beneficiary,
    organized, biographical, mobile, global).

% Institutes and commissions formed to produce and defend the new liturgical books and vernacular translations. Their continued funding and relevance depend on the claim that pre-conciliar forms were rigid and needed superseding, rather than merely permitted alternate expressions.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, liturgical_renewal_institutes, beneficiary,
    organized, biographical, mobile, national).

% Priests, religious, and laypeople attached to pre-conciliar liturgical and doctrinal forms who experienced the post-conciliar period as forced discontinuity — restricted or suppressed older rites, altered catechesis, disciplinary pressure on clergy resisting change. Their canonical and pastoral options for continuing older practice were narrowed for decades; formal exit means schism or marginalization within diocesan structures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_clergy_and_laity, payer,
    moderate, biographical, constrained, national).

% Religious communities whose constitutions, habits, and observances were substantially altered under mandated 'renewal' processes justified by the rupture reading. Many orders experienced severe numerical decline they attribute to discontinuity being imposed as if it were the Council's clear textual mandate; internal dissent was treated as resistance to be overcome rather than a legitimate reading of the same documents.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_religious_orders, payer,
    moderate, generational, trapped, national).

% Ordinary parishioners who received top-down liturgical and catechetical change justified as flowing from the Council, with no meaningful say in the pace or extent of implementation. Their prior forms of worship and instruction were often withdrawn rather than offered alongside the new, on the authority of a 'spirit' invoked beyond what the texts themselves specify.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, parishes_subject_to_mandated_liturgical_change, payer,
    powerless, immediate, trapped, local).

% Curial bodies charged with doctrinal oversight repeatedly attempted to constrain 'spirit of the Council' implementations to the actual text (e.g., later magisterial interventions insisting on continuity), but during the primary implementation period their voice was often overridden by the momentum of conference-level reform claiming direct conciliar mandate.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, roman_curia_doctrinal_offices, excluded,
    institutional, civilizational, constrained, global).

% Historians and archivists who examine council debates, conciliar minutes (acta), and drafting history to assess whether specific post-conciliar changes were textually mandated, textually permitted, or extra-textual extrapolations attributed to an unwritten 'spirit.'
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying interpretive frame that let a global, doctrinally and liturgically diverse episcopate move in a coordinated direction after the Council without requiring case-by-case textual authorization for every local reform — a single narrative ('the Council changed everything') substitutes for slower text-by-text ratification.
% TRANSFER_FUNCTION: Moves interpretive and disciplinary authority from the literal conciliar texts (and from those attached to pre-conciliar forms) to reform-aligned bishops, theological faculties, and liturgical commissions empowered to extend 'the spirit' beyond what any document states; moves standing and institutional position toward those who champion ongoing reform and away from those who hold that the documents themselves, read plainly, license far less change.
% ABSENT_VOICES: The council fathers who voted for the actual texts, many of whom later objected publicly that post-conciliar implementation exceeded what they had approved, are treated retrospectively as having intended more than they wrote; their contemporaneous objections are largely absent from the rupture-progressive narrative's account of its own legitimacy.
% DISAPPEARANCE_RATIONALE: If the 'spirit of the Council' interpretive license were withdrawn and implementation were re-anchored strictly to conciliar text, a wide range of post-conciliar liturgical, catechetical, and disciplinary changes would lose their claimed conciliar warrant and would need independent justification (papal or curial authority in their own right) or reversal — significant institutional restructuring (seminaries, liturgical commissions, publishing houses) built on the rupture reading would lose its mandate.
% FOUNDING_PROBLEM: The Council was convened to address a perceived pastoral and institutional rigidity — a Church seen as poorly positioned to engage the modern world, evangelize effectively, and communicate across cultures — and to update pastoral practice without doctrinal innovation.
% FOUNDING_PROBLEM_CORROBORATION: Reform-aligned bishops and theologians attest the founding problem required exactly the scope of change implemented and treat the 'spirit' reading as the Council's authentic self-understanding. Independent historians working from the conciliar acta, several council fathers' own later memoirs, and subsequent papal interventions (which repeatedly sought to rein in implementation back toward the text) attest from outside the reform-beneficiary set that a substantial portion of post-conciliar change exceeded what the documents themselves specify — corroboration for the 'live problem, textually bounded solution' reading exists outside the benefiting parties; corroboration for 'the spirit licenses unlimited further reform' comes predominantly from the parties who exercised that license.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_progressive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε≈0.58) is authored as substantial but not extreme: the rupture-progressive reading did coordinate a genuine pastoral response to real institutional problems (declining catechetical effectiveness, liturgical language barriers, ecumenical isolation), but it also transferred interpretive authority away from the literal texts and toward reform-aligned administrators in ways that imposed real costs on non-aligned clergy, orders, and parishes without their consent or a clear textual mandate. Suppression (0.52) reflects the disciplinary and formation-level pressure applied to resist alternate (continuity or traditionalist) readings during peak implementation — curial correction attempts (documented in the suppression_requirement dip after the mid-interval peak) show this was neither static nor unopposed. Theater ratio (0.4) captures that a meaningful share of 'renewal' activity in this period was performative alignment with reformist rhetoric rather than substantive engagement with the actual pastoral problems the Council named. Accessibility collapse is moderate (0.35) — pre-conciliar alternatives were suppressed in ordinary diocesan life but never fully eliminated (traditionalist communities persisted, later regularized), so alternatives did not collapse completely. Resistance is high (0.72): the reading has always been met with sustained internal counter-argument, including from within the hierarchy.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting reform seat, this arrangement is the Council authentically realizing itself over time — a rope, not an imposition. From the payer seats (traditionalist clergy, suppressed orders, disrupted parishes), the identical structure operated as an enforced doctrinal and disciplinary transfer riding on a coordination claim (updating pastoral practice) that did not require the extent of change actually imposed. The engine's per-seat computation should register this asymmetry directly from the beneficiary/victim and exit-option data, independent of which side's narrative one starts from.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive episcopal conferences and reform theologians derive institutional and intellectual capital directly from the rupture-progressive reading's dominance — they are near the beneficiary end. Traditionalist clergy, pre-conciliar orders, and ordinary parishioners bear the disciplinary and disruption costs of implementation exceeding textual warrant, with limited exit short of canonical marginalization or schism — they sit near the target end. The Roman Curia's doctrinal offices are excluded rather than coordinated during peak implementation: their attempts to anchor interpretation to text were institutionally present but structurally overridden by conference-level momentum, which is why they are marked excluded rather than agenda_setter despite formal seniority.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'spirit of the Council' license is structurally suited to mandatrophy: its founding problem (pastoral and institutional stagnation circa 1962) was real and, by most accounts, substantially addressed within a decade or two of implementation, yet the license to invoke 'the spirit' beyond the text to justify further reform did not sunset — it became a standing interpretive authority independent of any specific unmet need. Classifying this as tangled_rope rather than snare or rope prevents two mislabelings: treating it as pure extraction would erase the real coordination problem the Council addressed and the legitimate reforms that followed; treating it as pure rope would erase the asymmetric costs imposed on non-aligned clergy, orders, and laity without commensurate voice in how far 'the spirit' would be extended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_intent_vs_spirit_extrapolation,
    'Where do the conciliar texts themselves license the changes attributed to ''the spirit of the Council,'' and where does that phrase function as cover for extrapolation the council fathers did not vote on?',
    'Systematic comparison of the sixteen conciliar documents'' actual language against specific post-conciliar changes (liturgical rubrics, catechetical content, disciplinary norms), cross-checked against the conciliar acta (drafting debates and voting records) and council fathers'' own later commentary on whether implementation matched their understanding at the time of the vote.',
    'If most contested changes trace to explicit or clearly implicit textual warrant, this reading collapses toward the continuity_reading and ε should be revised sharply downward. If most trace only to extra-textual ''spirit'' invocation with no clear textual anchor, the tangled_rope classification (or even snare) is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_intent_vs_spirit_extrapolation, empirical, 'Whether specific post-conciliar changes are textually grounded or extra-textual extrapolation.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the rupture-progressive reading a description of what a critical mass of council fathers actually intended, or a retrospective institutional narrative constructed by those who benefited from open-ended reform authority after the fact?',
    'Compare conciliar-era private correspondence, drafting-committee minutes, and immediate post-conciliar commentary (pre-1970) against later (1980s+) retrospective characterizations of ''what the Council really meant''; a shift in emphasis over time toward more expansive claims would support the retrospective-construction hypothesis.',
    'If retrospective, the reading''s legitimacy claim (that it recovers authentic conciliar intent) weakens considerably and the extraction reading strengthens; if contemporaneous, the coordination-function claim strengthens and ε might be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the rupture-progressive reading reflects contemporaneous intent or later institutional construction.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'For traditionalist clergy and laity who ceased resisting over time, was the decline in active resistance due to structural suppression (formation, discipline, canonical pressure) or internalized acceptance (genuine persuasion that the reform reading was correct)?',
    'Post-hoc interviews and memoir analysis of clergy formed under the reformed system versus those formed pre-conciliarly who later ceased public resistance; compare stated reasons for acquiescence against documented disciplinary pressure they experienced.',
    'If suppression was substantially internalized rather than purely structural, the effective suppression experienced by later cohorts is higher than the structural suppression metric alone indicates, since resistance was pre-empted rather than merely blocked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism among later clergy cohorts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vati_tr_t8, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(vati_tr_t16, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(vati_tr_t25, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(vati_tr_t35, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(vati_tr_t45, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(vati_tr_t55, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 55, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vati_be_t8, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(vati_be_t16, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(vati_be_t25, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(vati_be_t35, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 35, 0.57).
narrative_ontology:measurement(vati_be_t45, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(vati_be_t55, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 55, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vati_su_t8, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(vati_su_t16, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(vati_su_t25, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(vati_su_t35, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 35, 0.5).
narrative_ontology:measurement(vati_su_t45, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 45, 0.48).
narrative_ontology:measurement(vati_su_t55, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 55, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'Vatican II' per the ε-invariance principle. Each reading (continuity, rupture_progressive, rupture_traditionalist, composite_overdetermination) has a distinct beneficiary/victim structure and a distinct ε: continuity_reading treats change as organic development (low ε, near-mountain framing from within its own tradition); rupture_progressive_reading (this story) treats rupture as legitimate and licenses ongoing reform (moderate-high ε, tangled_rope); rupture_traditionalist_reading treats the same rupture as error enabling heterodoxy (high ε, likely snare from its own seat, with different victims — those who lost pre-conciliar forms framed as authoritative loss rather than pastoral update); composite_overdetermination_reading decomposes the singular 'Vatican II shift' into independently assessable structural changes rather than treating it as one coordinated rupture or continuity. All four share the same underlying kernel (the doctrinal-authority status of the Council's documents and their implementation) but are not measurements of one constraint — they are four constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
