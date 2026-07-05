% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality-Balancing Standard (Judicial Case-by-Case Reading)
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This story instantiates the proportionality-balancing reading of the
 *   humane_treatment_standard kernel: the claim that Common Article 3
 *   requires courts to weigh detainee dignity against security needs
 *   case-by-case, rather than applying an absolute prohibition or leaving the
 *   matter to unreviewed executive discretion. Under this reading, courts
 *   become the gatekeepers of what counts as lawful treatment, and the
 *   standard is defined through accumulated adjudication rather than a fixed
 *   textual bar. This is a distinct constraint from the sibling readings
 *   (absolute_prohibition, contextual_necessity), which are authored as
 *   separate stories with their own epsilon values, beneficiary/victim
 *   structures, and classifications; this file does not describe or average
 *   over them.
 *
 * KEY AGENTS:
 *   - reviewing_courts: agenda_setter (institutional/analytical) — sets the operative standard through case-by-case rulings
 *   - detaining_states: beneficiary/agenda_setter (institutional/constrained) — retains policy latitude subject to after-the-fact review
 *   - security_services_seeking_operational_flexibility: beneficiary (organized/constrained) — operates under continuous legal uncertainty at the boundary
 *   - interrogators_facing_retroactive_liability: payer (moderate/trapped) — bears personal liability risk for conduct authorized at the time
 *   - detainees_subject_to_borderline_techniques: payer (powerless/trapped) — bears the immediate cost of ambiguity before any ruling occurs
 *   - human_rights_monitors: excluded (organized/constrained) — argues the balancing frame itself legitimizes prohibited conduct
 *   - international_law_scholars: observer (analytical/analytical) — documents the gap between doctrine and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.52).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.44).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.52).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality-Balancing Standard (Judicial Case-by-Case Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '23c7ffff-83ff-421c-8059-5336db3a636d').
narrative_ontology:cs_kernel_codification('23c7ffff-83ff-421c-8059-5336db3a636d', fixed_text).
narrative_ontology:cs_authority_grounding('23c7ffff-83ff-421c-8059-5336db3a636d', practice).
narrative_ontology:cs_interpretation_layer_present('23c7ffff-83ff-421c-8059-5336db3a636d').
narrative_ontology:cs_reading_relation('23c7ffff-83ff-421c-8059-5336db3a636d', humane_treatment_standard__absolute_prohibition, influences).
narrative_ontology:cs_reading_relation('23c7ffff-83ff-421c-8059-5336db3a636d', humane_treatment_standard__contextual_necessity, coexists_with).
narrative_ontology:cs_axiom('23c7ffff-83ff-421c-8059-5336db3a636d', foundational, no_treatment_permissible_without_case_specific_review).
narrative_ontology:cs_axiom_status(no_treatment_permissible_without_case_specific_review, holdable).
narrative_ontology:cs_axiom_grounding('23c7ffff-83ff-421c-8059-5336db3a636d', no_treatment_permissible_without_case_specific_review, conventional).
narrative_ontology:cs_axiom('23c7ffff-83ff-421c-8059-5336db3a636d', secondary, security_necessity_can_shift_but_not_eliminate_dignity_floor).
narrative_ontology:cs_axiom_status(security_necessity_can_shift_but_not_eliminate_dignity_floor, holdable).
narrative_ontology:cs_axiom_grounding('23c7ffff-83ff-421c-8059-5336db3a636d', security_necessity_can_shift_but_not_eliminate_dignity_floor, instrumental).
narrative_ontology:cs_reference_frame('23c7ffff-83ff-421c-8059-5336db3a636d', judicial_case_by_case_adjudication_framework).
narrative_ontology:cs_drift_state('23c7ffff-83ff-421c-8059-5336db3a636d', post_war_on_terror_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23c7ffff-83ff-421c-8059-5336db3a636d', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, reviewing_courts).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, security_services_seeking_operational_flexibility).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, detaining_states).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees_subject_to_borderline_techniques).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, interrogators_facing_retroactive_liability).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, judicial_case_by_case_adjudication_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates, case by case, whether a given interrogation or detention practice crossed the line between permissible pressure and prohibited degradation. Sets the operative standard through accumulated rulings rather than a bright-line rule, and thereby controls what counts as compliant behavior after the fact.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, reviewing_courts, agenda_setter,
    institutional, generational, analytical, national).

% Retains latitude to authorize enhanced techniques when it can construct a security justification, subject only to after-the-fact judicial review rather than a categorical bar. Benefits from the ambiguity because it preserves policy discretion while still claiming compliance with the Convention.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detaining_states, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, detaining_states, agenda_setter).

% Wants a standard that permits stress techniques short of clear torture in high-stakes interrogations. The balancing test gives them room to argue necessity, but the boundary is set only after litigation, so they operate under continuous legal uncertainty about where the line actually sits.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, security_services_seeking_operational_flexibility, beneficiary,
    organized, biographical, constrained, national).

% Executes techniques authorized as lawful at the time of use, but the balancing standard means later courts may retroactively rule those same techniques disproportionate. Bears personal legal and reputational risk for decisions made under institutional pressure and unclear contemporaneous guidance; has no meaningful exit from the chain of command that ordered the conduct.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, interrogators_facing_retroactive_liability, payer,
    moderate, biographical, trapped, national).

% Experiences whatever technique the balancing test has not yet clearly foreclosed. Because the standard depends on proportionality assessed after the fact, detainees bear the cost of the ambiguity in real time — the harm occurs before any court determines whether it was lawful. Has no capacity to invoke the standard prospectively to prevent treatment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees_subject_to_borderline_techniques, payer,
    powerless, immediate, trapped, local).

% Argues the balancing framework itself is the problem — that any weighing of dignity against security legitimizes techniques the absolute-prohibition reading would foreclose outright. Their position is heard in commentary and amicus filings but does not set the operative legal test, which remains a judicial and executive prerogative.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_monitors, excluded,
    organized, generational, constrained, global).

% Analyzes how the proportionality reading functions across jurisdictions, tracks divergence between courts, and documents the gap between the doctrine's promise of principled balancing and its practical function as a post-hoc liability shield.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable adjudicative mechanism for the genuinely hard cases where security necessity and detainee dignity both carry real weight, avoiding both the rigidity of an absolute bar that courts might strain to evade and the lawlessness of unconstrained executive discretion.
% TRANSFER_FUNCTION: Moves the burden of legal uncertainty from states and security services (who retain ex ante latitude to act) onto detainees (who absorb the immediate physical and psychological cost of contested techniques) and onto individual interrogators (who absorb retroactive liability risk once courts later rule a technique disproportionate).
% ABSENT_VOICES: Human rights monitors and detainees themselves are structurally excluded from setting the operative standard — the balancing test is defined and applied by courts and executives, with affected detainees having no seat in the proportionality calculus performed on their treatment, and no capacity to invoke the standard before harm occurs.
% DISAPPEARANCE_RATIONALE: If the proportionality-balancing reading vanished, the kernel would default to one of its siblings: either the absolute-prohibition reading (foreclosing a large set of currently-litigated techniques outright) or the contextual-necessity reading (removing judicial gatekeeping entirely). Either shift would materially change what states can lawfully authorize and what detainees can expect, and would eliminate the current cottage industry of proportionality litigation, expert testimony, and doctrinal commentary built around the balancing test.
% FOUNDING_PROBLEM: Courts and drafters needed a standard flexible enough to handle detention and interrogation practices across enormously varied conflict contexts without either licensing abuse through unlimited discretion or producing an absolute rule that states would simply refuse to ratify or would systematically evade.
% FOUNDING_PROBLEM_CORROBORATION: Judges and government legal advisers attest the balancing problem remains live — genuine hard cases exist where categorical rules produce absurd results. Human rights monitors and several international law scholars, outside the beneficiary set of states and security services, attest that the 'hard cases' framing is itself doing the discretion-preserving work the absolute-prohibition reading was designed to close off, and that the founding problem as originally framed (preventing atrocity) has been substantially displaced by a narrower problem (managing state liability exposure).
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and theater_ratio (0.40) are moderate-high and rising over the interval because the balancing standard, absent a bright line, increasingly functions to legitimize post-hoc justification of techniques rather than to prevent harm prospectively — the case-by-case apparatus generates substantial adjudicative activity (litigation, expert testimony, doctrinal commentary) whose ratio to actual prevention of degrading treatment increases as the jurisprudence accumulates exceptions and distinctions. Suppression (0.44) is moderate: detainees have essentially no capacity to invoke the standard before harm occurs (near-total suppression at the point of use) but the framework does impose real ex post constraint on states, unlike the contextual_necessity reading. accessibility_collapse (0.35) is lower than a genuine mountain reading would show, because the proportionality test explicitly preserves multiple live outcomes (technique found lawful, found unlawful, found lawful-but-close) rather than collapsing to a single determinate answer. resistance (0.60) is comparatively high because human rights monitors and a portion of the scholarly community actively contest the legitimacy of the balancing frame itself, not merely its application.
 *
 * PERSPECTIVAL GAP:
 *   From the reviewing courts' seat, the standard looks like principled, careful adjudication balancing legitimate competing interests — exactly the coordination function it claims. From the detainee seat, the same standard looks like a mechanism that permits harm to occur and only afterward asks whether it was proportionate, which is functionally indistinguishable from unlimited discretion at the moment of the harm itself. The tangled_rope classification captures both: a genuine coordination function (avoiding both extremes) coexists with asymmetric extraction (states and security services get ex ante flexibility; detainees and individual interrogators absorb the ex post cost of getting the balance wrong).
 *
 * DIRECTIONALITY LOGIC:
 *   Reviewing courts sit as agenda_setter with analytical exit — they administer the standard without bearing its costs directly. Detaining states and security services derive low d (beneficiary end) because the standard preserves their ex ante operational latitude; their exit options are constrained rather than trapped because they retain substantial control over interpretation and framing in litigation. Interrogators derive a d closer to the target end than their formal 'moderate' power level would suggest on its own, because the retroactive-liability structure exposes them personally for institutionally-authorized conduct — this is the clearest case in the story where the derived directionality benefits from the trapped exit_options declaration rather than power alone. Detainees derive the highest d: powerless, trapped, immediate horizon, and structurally excluded from invoking the standard before harm occurs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing atrocity while accommodating genuinely hard cases — remains partially live, which is why this is not classified as pure extraction (snare) or pure performance (piton). But the corroboration record shows the problem has partially shifted: from 'how do we prevent abuse in hard cases' to 'how do we manage state and individual liability exposure after abuse has already occurred.' The proportionality-balancing reading prevents the mislabeling error in both directions — it is not pure coordination (courts do meaningfully constrain some conduct that contextual_necessity would permit) and it is not pure extraction (it is not merely theatrical; adverse rulings do occur and do change practice) — but the rising theater_ratio over the interval signals the balancing apparatus increasingly serves a liability-management function alongside its original protective function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_versus_absolute_bar_legitimacy,
    'Does treating Common Article 3''s ''humane treatment'' requirement as a proportionality test (rather than a non-derogable floor) already concede the substantive point the absolute_prohibition reading exists to foreclose — namely, that some conditions can justify degrading treatment?',
    'Comparative doctrinal analysis of whether jurisdictions that adopt the balancing test show measurably higher rates of authorized borderline techniques than jurisdictions applying an absolute-prohibition standard, controlling for security context.',
    'If balancing jurisdictions show systematically higher authorization rates, the proportionality reading functions as a partial capture of the absolute_prohibition kernel space rather than a genuinely distinct middle position, strengthening the tangled_rope classification''s extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_versus_absolute_bar_legitimacy, conceptual, 'Whether proportionality balancing is a distinct middle reading or a soft version of contextual_necessity.').

omega_variable(
    ex_post_review_adequacy,
    'Is after-the-fact judicial review of proportionality an adequate substitute for ex ante prevention, given that the harm to detainees occurs before any court rules on its lawfulness?',
    'Track whether adverse proportionality rulings produce prospective changes in interrogation doctrine and training (evidence of real prevention) versus only individual case remedies with no doctrinal update (evidence of pure liability management).',
    'If rulings rarely produce prospective doctrinal change, the coordination function claimed by this reading is substantially theatrical, pushing the classification toward snare; if rulings reliably update prospective practice, the coordination function is real and the tangled_rope classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ex_post_review_adequacy, empirical, 'Whether ex post proportionality review meaningfully prevents future harm or only manages liability after the fact.').

omega_variable(
    cs_framing_kernel_versus_legitimacy_layer,
    'Is the proportionality-balancing reading better modeled as a reading of the Common Article 3 TEXT (fixed_text kernel), or as a reading of the LEGITIMACY CLAIM that courts have authority to perform this balancing at all (a distinct, higher-order commitment about judicial competence in security matters)?',
    'Examine whether disputes in this domain center on what the text says (textual interpretation) or on whether courts should have this adjudicative role at all (institutional competence) — the former supports fixed_text/practice framing, the latter supports a distinct authority-legitimacy kernel.',
    'If the operative dispute is about judicial competence rather than textual meaning, the authority_grounding value and interpretation_layer_present declaration in cs_structure would shift from ''practice'' toward a framing centered on institutional legitimacy contest, potentially changing which drift dynamics are salient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_versus_legitimacy_layer, conceptual, 'Whether the contested kernel is the treaty text or the judicial-competence claim layered above it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.22).
narrative_ontology:measurement(huma_tr_t8, humane_treatment_standard__proportionality_balancing, theater_ratio, 8, 0.27).
narrative_ontology:measurement(huma_tr_t16, humane_treatment_standard__proportionality_balancing, theater_ratio, 16, 0.31).
narrative_ontology:measurement(huma_tr_t24, humane_treatment_standard__proportionality_balancing, theater_ratio, 24, 0.34).
narrative_ontology:measurement(huma_tr_t32, humane_treatment_standard__proportionality_balancing, theater_ratio, 32, 0.37).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__proportionality_balancing, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(huma_be_t8, humane_treatment_standard__proportionality_balancing, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(huma_be_t16, humane_treatment_standard__proportionality_balancing, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(huma_be_t24, humane_treatment_standard__proportionality_balancing, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(huma_be_t32, humane_treatment_standard__proportionality_balancing, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__proportionality_balancing, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(huma_su_t8, humane_treatment_standard__proportionality_balancing, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(huma_su_t16, humane_treatment_standard__proportionality_balancing, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(huma_su_t24, humane_treatment_standard__proportionality_balancing, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(huma_su_t32, humane_treatment_standard__proportionality_balancing, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__proportionality_balancing, suppression_requirement, 40, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% This is one of three readings of the humane_treatment_standard kernel (Common Article 3's 'humane treatment' requirement). The absolute_prohibition reading treats the standard as a non-derogable floor with near-mountain-level accessibility collapse and low extraction (Mountain-leaning). The contextual_necessity reading treats it as permitting enhanced interrogation under security necessity, with concentrated beneficiaries (security services) and identifiable victims and comparatively higher suppression (Snare-leaning). This proportionality_balancing reading sits structurally between them: it retains a genuine coordination function (courts as gatekeepers avoiding both extremes) while carrying real asymmetric extraction (states and security services get ex ante latitude; detainees and interrogators absorb ex post cost), which is why it is authored as tangled_rope rather than mountain or snare. Each reading has its own epsilon, stakeholder structure, and classification; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
