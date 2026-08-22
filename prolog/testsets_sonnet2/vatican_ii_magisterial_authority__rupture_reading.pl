% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II as Rupture: New Ecclesiology Superseding Pre-Conciliar Magisterium
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the RUPTURE reading of the Vatican II magisterial
 *   authority kernel: the claim that the Council's texts encode a new
 *   ecclesiology structurally incompatible with pre-conciliar teaching, that
 *   this incompatibility is not merely apparent but real and acknowledged as
 *   doctrinal progress, and that implementation should proceed on that basis.
 *   This is not a story about the Council documents themselves or about
 *   whether they are 'really' continuous or ruptural in some
 *   observer-independent sense — that dispute is the kernel contest, carried
 *   across three sibling constraints (this one, continuity_reading,
 *   composite_overdetermination_reading). ε here is authored for the standing
 *   institutional arrangement AS THE RUPTURE READING ITSELF SEES IT: an
 *   arrangement that extracts institutional standing and disciplinary
 *   compliance from those who hold the older framework, justified by the
 *   claim that the old framework's authority has genuinely lapsed. A
 *   continuity reading of the identical historical events would author a very
 *   different ε (near-mountain: organic development, minimal extraction)
 *   because it denies the discontinuity that gives this reading its
 *   extractive lever in the first place.
 *
 * KEY AGENTS:
 *   - progressive_curial_reformers: agenda_setter, institutional/arbitrage — administers implementation and enforcement
 *   - post_conciliar_theological_establishment: beneficiary, organized/mobile — professional standing built on the rupture frame
 *   - national_bishops_conferences: beneficiary/agenda_setter, institutional/constrained — gained devolved authority under this reading
 *   - traditionalist_clergy: payer, moderate/trapped — disciplined for non-compliance
 *   - pre_conciliar_lay_associations: payer, powerless/trapped — dissolved institutional homes
 *   - sspx_aligned_communities: payer, powerless/trapped — driven into canonical irregularity
 *   - vatican_doctrinal_congregation: observer, institutional/analytical — issues ambiguous arbitration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.58).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II as Rupture: New Ecclesiology Superseding Pre-Conciliar Magisterium").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '2bb584d5-952e-4c90-8355-acc19d85cabd').
narrative_ontology:cs_kernel_codification('2bb584d5-952e-4c90-8355-acc19d85cabd', formalized).
narrative_ontology:cs_authority_grounding('2bb584d5-952e-4c90-8355-acc19d85cabd', lineage).
narrative_ontology:cs_interpretation_layer_present('2bb584d5-952e-4c90-8355-acc19d85cabd').
narrative_ontology:cs_reading_relation('2bb584d5-952e-4c90-8355-acc19d85cabd', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2bb584d5-952e-4c90-8355-acc19d85cabd', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('2bb584d5-952e-4c90-8355-acc19d85cabd', foundational, doctrinal_reversal_is_legitimate_progress).
narrative_ontology:cs_axiom_status(doctrinal_reversal_is_legitimate_progress, holdable).
narrative_ontology:cs_axiom_grounding('2bb584d5-952e-4c90-8355-acc19d85cabd', doctrinal_reversal_is_legitimate_progress, conventional).
narrative_ontology:cs_axiom('2bb584d5-952e-4c90-8355-acc19d85cabd', secondary, error_has_no_rights_doctrine_superseded).
narrative_ontology:cs_axiom_status(error_has_no_rights_doctrine_superseded, overridden).
narrative_ontology:cs_axiom_grounding('2bb584d5-952e-4c90-8355-acc19d85cabd', error_has_no_rights_doctrine_superseded, empirically_contingent).
narrative_ontology:cs_reference_frame('2bb584d5-952e-4c90-8355-acc19d85cabd', counter_reformation_defensive_magisterium).
narrative_ontology:cs_drift_state('2bb584d5-952e-4c90-8355-acc19d85cabd', post_conciliar_implementation_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('2bb584d5-952e-4c90-8355-acc19d85cabd', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_curial_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_theological_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_lay_associations).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, sspx_aligned_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer post-conciliar dicasteries, seminary formation, and liturgical implementation offices. They read the council texts as authorizing decisive breaks with pre-conciliar positions (religious liberty, ecclesiology of communion, vernacular liturgy) and enforce this reading through appointment power, curricular control, and disciplinary action against dissenting clergy.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_curial_reformers, agenda_setter,
    institutional, generational, arbitrage, global).

% Academic theologians whose careers, chairs, and publishing platforms are built on the rupture reading being the operative institutional truth. They gain professional standing, curricular dominance, and access to episcopal consultation precisely because the break-with-the-past framing legitimizes their post-conciliar specializations.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_theological_establishment, beneficiary,
    organized, generational, mobile, global).

% Gained substantially expanded collegial and administrative authority under a reading that treats the council as authorizing devolution from Roman centralization. Their institutional power is structurally tied to the rupture reading remaining operative; a continuity reading would narrow their discretionary latitude.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences, agenda_setter).

% Priests formed in or sympathetic to pre-conciliar liturgical and doctrinal formation who face canonical discipline, reassignment, or exclusion from ordinary ministry for refusing the rupture reading's implementation demands (e.g., mandatory vernacular liturgy, refusal to teach religious liberty as continuous with prior teaching). Exit means schism or marginalization within diocesan structures they cannot leave without losing faculties.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy, payer,
    moderate, biographical, trapped, national).

% Lay confraternities, devotional societies, and parish communities organized around pre-conciliar liturgical and catechetical practice who saw their institutional homes dissolved, suppressed, or reassigned during implementation. They had no standing to contest reclassification of their practices as superseded.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_lay_associations, payer,
    powerless, biographical, trapped, local).

% Communities that rejected the rupture reading's practical demands and were driven into canonically irregular or schismatic status. They bear the full cost of the rupture framing's disciplinary enforcement while having no voice within the structures that adjudicate their status.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, sspx_aligned_communities, payer,
    powerless, generational, trapped, global).

% Periodically issues clarifications (e.g., on the interpretation of Dignitatis Humanae or Lumen Gentium subsistit in) that neither fully ratify nor fully reject the rupture reading, functioning as an ambiguous arbiter whose statements are cited by all sides as support.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, vatican_doctrinal_congregation, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative frame for post-conciliar institutional reform, allowing dioceses, seminaries, and bishops' conferences to coordinate rapid, uniform implementation of liturgical and structural changes without renegotiating first principles at each level.
% TRANSFER_FUNCTION: Moves institutional authority, formation control, and disciplinary standing from pre-conciliar clergy, lay associations, and Roman curial centralization toward post-conciliar theological professionals, national conferences, and reform-aligned diocesan administrations.
% ABSENT_VOICES: Traditionalist clergy and lay associations who hold a continuity or composite reading are largely absent from the implementing bodies (theological faculties, liturgical commissions, episcopal conferences) that authored and enforce the rupture reading; their objections surface mainly in canonically irregular venues (SSPX communiques, traditionalist petitions) rather than in the deliberative structures themselves.
% DISAPPEARANCE_RATIONALE: If the rupture reading lost its institutional purchase overnight, seminary curricula, liturgical practice mandates, and the disciplinary basis for sanctioning traditionalist clergy would all require re-justification; national bishops' conferences would need to relitigate the basis of their expanded authority, and communities currently treated as schismatic (SSPX-aligned) would have a live claim to reintegration on continuity terms.
% FOUNDING_PROBLEM: The Council itself was convened to address a genuine problem: how the Church should engage a modern, pluralistic world (religious liberty, ecumenism, liturgical accessibility) after centuries of a defensive, Counter-Reformation posture. The rupture reading emerged as one answer to how radically that engagement should be construed.
% FOUNDING_PROBLEM_CORROBORATION: Progressive reformers and much of the post-conciliar theological establishment attest the rupture reading correctly resolves the founding problem and that continuity claims are apologetic retrofitting. Independent historians of the Council (e.g., scholars outside both curial and traditionalist camps, drawing on conciliar acta and periti diaries) and Pope Benedict XVI's own 2005 Curia address attest the rupture framing is itself a contested interpretive choice rather than the text's self-evident content — corroboration exists on both sides, which is why status is authored as contested rather than resolved.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that under the rupture reading, disciplinary and career consequences flow asymmetrically to those who maintain the superseded pre-conciliar positions — this is a real transfer of institutional standing, not a neutral doctrinal disagreement. Suppression (0.62) is authored high because enforcement of the rupture reading against traditionalist clergy and communities has historically relied on canonical discipline, removal of faculties, and (for SSPX-aligned communities) irregular/schismatic classification — genuine coercive machinery, not persuasion alone. Accessibility collapse is moderate (0.45), not high, because the continuity and composite readings remain institutionally live (cited by popes, taught in some faculties) — alternatives have not fully collapsed even where the rupture reading dominates administratively. Resistance is high (0.78) because traditionalist clergy, SSPX-aligned communities, and a substantial minority of theologians actively contest the rupture framing, sometimes at high personal cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive curial reformers and the post-conciliar theological establishment sit near the beneficiary end: the rupture reading is the ground of their institutional and professional standing, and they administer or interpret its implementation. National bishops' conferences are dual-positioned — they benefit from devolved authority under this reading AND help set its implementing agenda regionally. Traditionalist clergy, pre-conciliar lay associations, and SSPX-aligned communities sit near the full-target end: trapped exit options (leaving diocesan structures forfeits ministry or canonical standing), direct disciplinary costs, and no seat in the bodies that adjudicate the reading's correctness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how the Church engages modernity) is authored as contested rather than resolved specifically because the rupture reading's proponents and detractors give incompatible answers to whether that problem still requires the rupture-level solution or was ever correctly diagnosed as requiring one. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (a single interpretive frame lets large bureaucratic structures like national conferences and seminaries move in step) alongside the asymmetric extraction (traditionalist minorities pay disciplinary and institutional costs enforcement requires). A pure snare framing would deny any coordination value; a pure rope framing would deny the coercive machinery used against dissenters — tangled_rope is the honest structural read from inside this reading's own commitments.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_continuity_textual_fact,
    'Do the conciliar texts themselves (as opposed to their subsequent institutional implementation) actually assert doctrinal reversal, or do they use deliberately ambiguous compromise language compatible with both continuity and rupture readings?',
    'Close textual-critical analysis of the conciliar acta, drafting history, and periti commentary (e.g., the documented drafting disputes over Dignitatis Humanae and Lumen Gentium''s subsistit in) to determine whether the final texts contain unambiguous doctrinal reversal or negotiated ambiguity.',
    'If the texts are genuinely ambiguous compromise formulations, the rupture reading is itself a subsequent interpretive imposition rather than a textually mandated conclusion — this would support treating rupture as one contestable overlay among several rather than the texts'' inherent content, strengthening the composite_overdetermination_reading''s claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_continuity_textual_fact, empirical, 'Whether conciliar texts textually mandate rupture or merely permit a rupture reading among others.').

omega_variable(
    kernel_reading_selection_basis,
    'What decides which of the three kernel readings (rupture, continuity, composite) is authored as the operative institutional truth in any given diocese or era, if not the texts alone?',
    'Comparative institutional analysis of which reading dominates in which post-conciliar period and jurisdiction, correlated with which actors hold administrative power at that time and place (e.g., contrast the immediate post-conciliar implementation period against the Benedict XVI ''hermeneutic of continuity'' period).',
    'If reading dominance tracks who holds administrative power rather than any independent textual or theological resolution, this substantially strengthens the extraction reading of the enforcement apparatus (whichever reading currently holds power uses it to discipline dissent) rather than treating any reading as simply correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether reading dominance is power-contingent rather than textually determined.').

omega_variable(
    sspx_reintegration_counterfactual,
    'Would SSPX-aligned and similarly situated traditionalist communities have avoided canonical irregularity entirely under a consistently applied continuity reading?',
    'Trace the specific disciplinary actions taken against traditionalist clergy and communities and assess whether they were predicated on rupture-specific claims (e.g., mandatory acceptance of DH as doctrinal reversal) versus claims compatible with a continuity reading.',
    'If disciplinary actions specifically required acceptance of rupture-framed claims (not merely liturgical compliance), this confirms the rupture reading''s role as the operative extractive mechanism rather than a neutral byproduct of any implementation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sspx_reintegration_counterfactual, empirical, 'Whether traditionalist marginalization was rupture-specific or implementation-generic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.34).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1995, 0.36).
narrative_ontology:measurement(vati_tr_t2007, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2007, 0.38).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2013, 0.39).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(vati_be_t2007, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2007, 0.54).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2013, 0.56).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(vati_su_t2007, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the Vatican II rupture/continuity debate,' per the ε-invariance principle: the rupture, continuity, and composite_overdetermination readings each author their own ε, beneficiary/victim structure, and classification rather than sharing one story with a measurement parameter. The rupture reading authors ε=0.58 (tangled_rope) because it treats the discontinuity as real and institutionally actionable. The continuity reading is expected to author a near-mountain ε (organic development claim denies extractive leverage). The composite_overdetermination reading is expected to author its own distinct ε reflecting ambiguity-driven factional contest rather than either pole. All three link to each other bidirectionally as members of one kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
