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
 *   human_readable: 'Spirit of the Council' Progressive Reading of Vatican II Authority
 *   domain: religious/institutional/hermeneutical
 *
 * SUMMARY:
 *   This story authors the rupture-progressive reading of the Vatican II
 *   kernel: the claim that the Council marks a necessary and justified break
 *   with pre-conciliar rigidity, and that the 'spirit of the Council'
 *   legitimately authorizes ongoing reform beyond what the promulgated texts
 *   literally state. Under this reading, textual ambiguities in documents
 *   like Dignitatis Humanae and Sacrosanctum Concilium are read as
 *   intentional openings rather than unresolved compromises, and decades of
 *   post-conciliar implementation (vernacular liturgy expansion, ecumenical
 *   practice, altered seminary formation, expanded episcopal collegiality)
 *   are treated as the authentic realization of what the Council fathers
 *   actually intended, even where it exceeds the letter of the text. This is
 *   a kernel-reading story: ε is authored for the standing arrangement of
 *   authority-by-spirit as this reading's own proponents and its critics
 *   jointly observe it operating, not for an idealized end-state. The sibling
 *   readings — continuity (apparent novelty as organic development within
 *   unchanging tradition), rupture-traditionalist (rupture as institutional
 *   failure enabling heterodoxy), and composite-overdetermination (multiple
 *   independent structural shifts bundled as one reform) — are separate
 *   constraints, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.58).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_progressive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_progressive_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_progressive_reading, "'Spirit of the Council' Progressive Reading of Vatican II Authority").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_progressive_reading, "religious/institutional/hermeneutical").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_progressive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_progressive_reading, '2315eb50-f267-4d4f-87ed-30fbdd998a99').
narrative_ontology:cs_kernel_codification('2315eb50-f267-4d4f-87ed-30fbdd998a99', fixed_text).
narrative_ontology:cs_authority_grounding('2315eb50-f267-4d4f-87ed-30fbdd998a99', extraction).
narrative_ontology:cs_interpretation_layer_present('2315eb50-f267-4d4f-87ed-30fbdd998a99').
narrative_ontology:cs_reading_relation('2315eb50-f267-4d4f-87ed-30fbdd998a99', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2315eb50-f267-4d4f-87ed-30fbdd998a99', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2315eb50-f267-4d4f-87ed-30fbdd998a99', vatican_ii_doctrinal_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('2315eb50-f267-4d4f-87ed-30fbdd998a99', foundational, spirit_of_council_exceeds_textual_letter).
narrative_ontology:cs_axiom_status(spirit_of_council_exceeds_textual_letter, holdable).
narrative_ontology:cs_axiom_grounding('2315eb50-f267-4d4f-87ed-30fbdd998a99', spirit_of_council_exceeds_textual_letter, conventional).
narrative_ontology:cs_axiom('2315eb50-f267-4d4f-87ed-30fbdd998a99', foundational, pre_conciliar_rigidity_required_necessary_break).
narrative_ontology:cs_axiom_status(pre_conciliar_rigidity_required_necessary_break, holdable).
narrative_ontology:cs_axiom_grounding('2315eb50-f267-4d4f-87ed-30fbdd998a99', pre_conciliar_rigidity_required_necessary_break, empirically_contingent).
narrative_ontology:cs_axiom('2315eb50-f267-4d4f-87ed-30fbdd998a99', secondary, post_conciliar_implementation_is_authentic_realization).
narrative_ontology:cs_axiom_status(post_conciliar_implementation_is_authentic_realization, holdable).
narrative_ontology:cs_axiom_grounding('2315eb50-f267-4d4f-87ed-30fbdd998a99', post_conciliar_implementation_is_authentic_realization, instrumental).
narrative_ontology:cs_reference_frame('2315eb50-f267-4d4f-87ed-30fbdd998a99', pre_conciliar_neo_scholastic_settlement).
narrative_ontology:cs_drift_state('2315eb50-f267-4d4f-87ed-30fbdd998a99', contemporary_magisterial_recalibration, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2315eb50-f267-4d4f-87ed-30fbdd998a99', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_reform_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theological_faculties).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_progressive_reading, national_bishops_conferences).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_religious_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_liturgical_practitioners).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_seeking_doctrinal_stability).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, doctrine_develops_through_historical_consciousness).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__rupture_progressive_reading, council_intent_exceeds_textual_letter).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clergy and curial officials who implemented liturgical, catechetical, and structural reform after the Council by appeal to its 'spirit' rather than its promulgated text alone. They administer seminaries, liturgical commissions, and diocesan offices, and their institutional authority and career paths are built on the progressive reading being the authoritative one. They can revise catechesis and practice further without needing a new council to ratify each change.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_reform_clergy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, post_conciliar_reform_clergy, beneficiary).

% Academic theologians whose scholarly reputations, publishing careers, and faculty positions rest on treating the Council's ambiguities as intentional openings for doctrinal development. They shape seminary formation and gain intellectual authority from being interpreters of the Council's 'true' trajectory rather than its bare text.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, progressive_theological_faculties, beneficiary,
    organized, generational, mobile, continental).

% Episcopal conferences gained expanded collegial authority and local discretion under the progressive reading, allowing regional adaptation of liturgy and pastoral practice with less recourse to Rome. Their institutional power is enlarged by this reading; they administer implementation and can extend it, but a reversal to a strict-textual or continuity reading would shrink their discretionary authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, national_bishops_conferences, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__rupture_progressive_reading, national_bishops_conferences, agenda_setter).

% Communities and clergy attached to pre-conciliar liturgy and doctrinal formulations experienced the progressive implementation as displacement — suppressed rites, altered catechesis, restructured seminaries — justified by appeal to a 'spirit' that exceeds the documents they can point to and contest. Canonical and administrative exit costs are high; some groups face suspension or irregular status for resisting implementation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, traditionalist_religious_communities, payer,
    powerless, civilizational, trapped, global).

% Clergy and religious formed in pre-conciliar practice who found their liturgical and formational training rendered obsolete by reforms justified as the Council's authentic intent rather than its literal mandate. They bear the cost of retraining or marginalization within institutions they cannot leave without losing vocation and community.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, pre_conciliar_liturgical_practitioners, payer,
    powerless, biographical, trapped, national).

% Ordinary Catholics who experienced catechetical and liturgical discontinuity across a single generation, with authoritative teaching on religious freedom, ecumenism, and liturgy presented as flowing from a Council whose text they cannot independently verify against the 'spirit' invoked by implementers. Their options are compliance, quiet dissent, or departure to traditionalist or other communities.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, laity_seeking_doctrinal_stability, payer,
    powerless, biographical, constrained, global).

% Vatican officials and bishops who favor a strict-textual or continuity reading were structurally sidelined during peak post-conciliar implementation; their objections that 'spirit of the Council' arguments exceed textual warrant were treated as resistance to be managed rather than as a live interpretive claim requiring answer.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, curial_conservative_faction, excluded,
    organized, generational, constrained, national).

% Church historians and canonists who study conciliar documents, drafting history (the acta and relationes), and implementation record to assess whether post-conciliar practice matches the Council fathers' declared intent or departs from it under cover of 'spirit.'
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_progressive_reading, magisterial_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_progressive_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_progressive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretive frame allowing the institutional Church to adapt liturgy, ecumenical relations, and pastoral practice to modern conditions without requiring a new ecumenical council for every adjustment — genuine coordination value for an institution needing to act coherently across a changed world.
% TRANSFER_FUNCTION: Moves interpretive authority from the literal conciliar text (which requires no ongoing gatekeeper) to a class of implementers — reform clergy, theological faculties, bishops' conferences — who administer what the 'spirit' requires; moves liturgical and catechetical stability away from pre-conciliar communities and laity toward whichever authority currently controls the progressive interpretation.
% ABSENT_VOICES: Traditionalist clergy and laity who dispute that the documents authorize what was implemented in their name were largely outside the rooms where implementation decisions were made (national liturgical commissions, seminary reform committees); their objections were treated as pastoral problems to manage rather than interpretive claims requiring textual answer.
% DISAPPEARANCE_RATIONALE: If the 'spirit of the Council' interpretive license disappeared and authority reverted strictly to the promulgated texts read narrowly, large portions of post-conciliar liturgical reform, ecumenical practice, and pastoral discretion currently justified by appeal to conciliar intent beyond the letter would lose their warrant, forcing either a new council, a doctrinal restatement, or rollback of practices that currently rest on this reading's authority.
% FOUNDING_PROBLEM: The Council fathers left some documents (notably on religious freedom, ecumenism, and liturgy) with genuine ambiguity, compromise language, and unresolved tensions between reform-minded and conservative blocs; the progressive reading was built to resolve that ambiguity in a direction consistent with a hoped-for ongoing aggiornamento rather than to freeze the compromise text.
% FOUNDING_PROBLEM_CORROBORATION: Progressive implementers and their theological allies attest the founding problem (rigid pre-conciliar formation unsuited to the modern world) remains live and the spirit-reading is its authentic continuation. Independent historians working from the conciliar acta and non-aligned canonists note that some documents show deliberate ambiguity as a drafting compromise rather than an invitation to open-ended development, and that implementation in several areas (liturgy, seminary formation) went beyond what floor debate and the relationes support — corroboration from outside the beneficiary set is mixed, not absent, but does not uniformly support the progressive reading's self-account.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_progressive_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_progressive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_progressive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that the progressive reading functions as a durable transfer of interpretive authority to a specific implementer class (reform clergy, theological faculties, bishops' conferences) who administer what the 'spirit' requires, at the expense of communities attached to prior practice who have no textual recourse against an appeal to unwritten intent. Suppression (0.42) is moderate rather than extreme: post-conciliar implementation used real administrative pressure (seminary restructuring, liturgical mandate, canonical irregularity for resistant groups) but did not eliminate traditionalist practice outright — communities persisted, if marginalized. Theater ratio (0.3) captures that genuine pastoral adaptation occurred alongside performative invocations of 'the spirit' used to justify changes with weak textual warrant. Accessibility collapse (0.35) is moderate-low: alternative readings of the same documents remain available to scholars and traditionalist communities, unlike a genuine mountain where alternatives vanish entirely. Resistance (0.62) is substantial and organized (traditionalist orders, some episcopates, later magisterial correctives under subsequent popes), which is inconsistent with pure natural-law status and consistent with a contested, actively-defended interpretive regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform clergy, theological faculties, and bishops' conferences are declared beneficiaries: their institutional authority, career paths, and discretionary power expand under this reading, and their exit options are mobile-to-arbitrage (they can move within or across institutions the reading empowers). Traditionalist communities, pre-conciliar-formed practitioners, and stability-seeking laity are declared victims: they bear the cost of discontinuity, have trapped or constrained exit (leaving means losing vocation, community, or continuity of practice), and cannot contest the 'spirit' appeal on textual grounds because the reading's whole claim is that intent exceeds text. This is precisely why the constraint is tangled rope rather than pure snare: there is a genuine coordination function (adapting a global institution to modern conditions without a new council for every adjustment) bundled with asymmetric extraction (implementer classes gain durable authority at the direct cost of a defined, powerless victim class), and the reading requires active administrative enforcement (mandated implementation, suppression of independent pre-conciliar liturgical societies until later relaxations) to hold.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate here is genuinely contested rather than settled: this reading treats the founding problem (rigid pre-conciliar formation unsuited to modern conditions) as still live and the spirit-based interpretive license as its ongoing, legitimate instrument. It is not authored as mandatrophy-resolved, because — from this reading's own seat — the mandate has not outlived its function; it remains the operative justification for continued doctrinal and pastoral development. The tangled-rope classification, rather than snare, prevents mislabeling this as pure extraction: a real coordination need (a global institution navigating modernity) is present and would not vanish if the reading did, distinguishing it from an arrangement with no coordination content at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spirit_versus_letter_referent_ambiguity,
    'Does ''the spirit of the Council'' name a real, recoverable collective intent of the Council fathers (recoverable from the acta, relationes, and floor debates), or is it a construct that licenses whatever implementers subsequently wished to do, retroactively attributed to the Council?',
    'Systematic comparison of conciliar drafting history (acta synodalia, relationes, floor interventions) against specific post-conciliar implementation decisions, checking whether the ''spirit'' invoked in each case is traceable to documented floor intent or is a later gloss.',
    'If traceable, the progressive reading''s extraction is lower than authored (implementation tracks real intent); if largely untraceable, the reading functions closer to pure extraction dressed as fidelity, and ε would be higher than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spirit_versus_letter_referent_ambiguity, empirical, 'Whether ''spirit of the Council'' names recoverable intent or retroactive license.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the rupture-progressive reading held because it is the most textually and historically defensible account of the Council''s documents, or because it was structurally advantaged by controlling the institutions (seminaries, liturgical commissions, national conferences) that implemented and then narrated the Council''s legacy?',
    'Compare institutional control patterns (who ran seminary formation, liturgical commissions, and catechetical offices in the 1965-1985 window) against the content of the reading that prevailed in each region, looking for correlation between administrative control and interpretive outcome independent of textual argument.',
    'Strong correlation would support treating this reading partly as an artifact of institutional capture rather than pure hermeneutic conclusion, raising ε; weak correlation would support the reading''s own account of itself as the textually superior interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether reading dominance tracks textual merit or institutional control.').

omega_variable(
    religious_freedom_reversal_scope,
    'Does Dignitatis Humanae''s teaching on religious freedom constitute a doctrinal reversal of the Syllabus of Errors'' condemnations, or a development addressing a different question (civil/political right to non-coercion vs. the theological status of religious pluralism)?',
    'Close textual and historical-theological comparison of the object of each document''s condemnation/affirmation, cross-checked against how the drafting commission itself described the relationship to prior magisterial teaching.',
    'If genuine doctrinal reversal, this reading''s claim of ''necessary break'' is strongly vindicated and ε for the underlying doctrinal-change component is high; if the documents address distinct questions, the ''rupture'' claim is overstated relative to what the text supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_freedom_reversal_scope, conceptual, 'Whether religious freedom teaching reverses or is orthogonal to prior condemnations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_progressive_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 1995, 0.56).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2005, 0.57).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__rupture_progressive_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_progressive_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__rupture_progressive_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_progressive_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Part of the vatican_ii_doctrinal_authority kernel family (4 readings). This story (rupture_progressive_reading) claims the break was necessary and that conciliar 'spirit' legitimately licenses ongoing reform beyond text — directly forecloses continuity_reading's core premise that no real rupture occurred (both cannot be true within one framework). It coexists with rupture_traditionalist_reading (both affirm rupture occurred; they disagree only on whether it was legitimate, which is a live disagreement between factions, not a logical contradiction). It influences composite_overdetermination_reading by supplying the unifying 'necessary break' narrative that the overdetermination reading's decomposition explicitly denies is a single coherent shift, creating downstream pressure on how that reading's component changes get narrated. Each reading carries its own ε, beneficiaries, and victims per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
