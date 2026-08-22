% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: 1951 Refugee Convention — Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story instantiates the procedural-integrity reading of the 1951
 *   Refugee Convention kernel: a reading that treats the Convention's core
 *   commitment as a guarantee of fair, individualized process rather than a
 *   fixed substantive protection threshold. States retain discretion to
 *   define 'well-founded fear' and 'particular social group' narrowly or
 *   broadly; what is non-negotiable is that whatever threshold a state
 *   adopts, it must be applied through a genuine individualized hearing with
 *   reasoned decision and appeal. Over the last several decades this reading
 *   has been used to legitimate offshore processing and accelerated
 *   procedures so long as procedural forms are present, even as those forms
 *   increasingly function as compliance theater around substantively
 *   predetermined outcomes.
 *
 * KEY AGENTS:
 *   - asylum_states_with_developed_tribunals: agenda_setter/beneficiary (institutional/arbitrage) — sets procedural architecture, gains legitimacy from formal compliance
 *   - asylum_seekers_in_offshore_processing: payer (powerless/trapped) — bears the gap between procedural form and substantive access
 *   - asylum_seekers_facing_accelerated_procedures: payer (powerless/constrained) — procedural floor satisfied nominally, substantively compressed
 *   - unhcr_and_procedural_monitors: observer/beneficiary (institutional/analytical) — gains a tractable audit metric but lacks enforcement power
 *   - domestic_courts_and_tribunals: agenda_setter/observer (organized/constrained) — reviews process, defers on substance
 *   - host_country_civil_society: excluded (moderate/mobile) — documents substantive failures without standing to compel reinterpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.58).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.62).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "1951 Refugee Convention — Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '93ddb078-af3e-466a-bfb0-e7da4c15c13d').
narrative_ontology:cs_kernel_codification('93ddb078-af3e-466a-bfb0-e7da4c15c13d', fixed_text).
narrative_ontology:cs_authority_grounding('93ddb078-af3e-466a-bfb0-e7da4c15c13d', practice).
narrative_ontology:cs_interpretation_layer_present('93ddb078-af3e-466a-bfb0-e7da4c15c13d').
narrative_ontology:cs_reading_relation('93ddb078-af3e-466a-bfb0-e7da4c15c13d', refugee_convention_text__restrictive_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('93ddb078-af3e-466a-bfb0-e7da4c15c13d', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('93ddb078-af3e-466a-bfb0-e7da4c15c13d', foundational, process_fairness_is_the_non_negotiable_core).
narrative_ontology:cs_axiom_status(process_fairness_is_the_non_negotiable_core, holdable).
narrative_ontology:cs_axiom_grounding('93ddb078-af3e-466a-bfb0-e7da4c15c13d', process_fairness_is_the_non_negotiable_core, conventional).
narrative_ontology:cs_axiom('93ddb078-af3e-466a-bfb0-e7da4c15c13d', secondary, substantive_threshold_is_state_discretionary_within_procedural_floor).
narrative_ontology:cs_axiom_status(substantive_threshold_is_state_discretionary_within_procedural_floor, holdable).
narrative_ontology:cs_axiom_grounding('93ddb078-af3e-466a-bfb0-e7da4c15c13d', substantive_threshold_is_state_discretionary_within_procedural_floor, instrumental).
narrative_ontology:cs_reference_frame('93ddb078-af3e-466a-bfb0-e7da4c15c13d', individualized_status_determination_norm).
narrative_ontology:cs_drift_state('93ddb078-af3e-466a-bfb0-e7da4c15c13d', post_offshore_processing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('93ddb078-af3e-466a-bfb0-e7da4c15c13d', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_states_with_developed_tribunals).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, unhcr_and_procedural_monitors).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_in_offshore_processing).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_facing_accelerated_procedures).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, procedural_due_process_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, individualized_assessment_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and operates the status-determination apparatus — interview protocols, appeal tribunals, evidentiary standards. Can narrow substantive definitions of who qualifies while still claiming compliance, so long as a hearing, a reasoned decision, and an appeal path exist. Gains international legitimacy and domestic political cover by pointing to its procedural machinery as proof of good faith, even where approval rates are low.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_states_with_developed_tribunals, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, asylum_states_with_developed_tribunals, beneficiary).

% Processed in a third country or extraterritorial facility where access to counsel, interpreters, and appeal is thin or absent. Under this reading, offshore processing is not itself prohibited — only processing that strips procedural guarantees is — so their fate turns entirely on whether the receiving arrangement replicates fair-hearing standards, which in practice it rarely does. They cannot litigate their way home; the tribunal that would vindicate their claim is the one they cannot reach.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_in_offshore_processing, payer,
    powerless, immediate, trapped, regional).

% Channeled into fast-track or 'manifestly unfounded' procedures with compressed timelines, limited evidence-gathering, and truncated appeal windows. The state can point to the existence of a hearing to satisfy the procedural floor even where the compression functionally forecloses a fair individualized assessment. Their outcome is nominally procedural but substantively predetermined by queue design.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_facing_accelerated_procedures, payer,
    powerless, immediate, constrained, national).

% Audits state procedures against fair-hearing benchmarks, publishes advisory opinions, and intervenes in litigation as amicus. Benefits from this reading because it gives monitoring bodies a tractable, justiciable metric (was there a fair process?) rather than an unmeasurable substantive one (was the fear well-founded?). Has no direct enforcement power — states can and do disregard advisory findings.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr_and_procedural_monitors, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, unhcr_and_procedural_monitors, beneficiary).

% Reviews individual claims and adjudicates whether procedure was followed. Under this reading their remit is bounded to process — they can quash a decision for procedural defect but are institutionally reluctant to substitute their own substantive judgment on who counts as a refugee, deferring to executive threshold-setting so long as the hearing was fair on its face.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, domestic_courts_and_tribunals, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, domestic_courts_and_tribunals, observer).

% Legal aid organizations and advocacy groups argue the procedural floor is being satisfied on paper while producing substantively unjust outcomes at scale. They document individual cases and lobby for reform but are not parties to status determination and cannot compel a state to interpret the Convention more generously.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, host_country_civil_society, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, asylum_states_with_developed_tribunals).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives states, courts, and monitoring bodies a shared, verifiable standard — a fair, individualized hearing — that can be audited and litigated even when the underlying substantive question (who genuinely fears persecution) resists consensus. It lets states with very different definitions of persecution still claim compliance with one Convention by pointing to common procedural architecture.
% TRANSFER_FUNCTION: Moves the burden of proof and the risk of erroneous rejection onto the individual claimant, in exchange for a guaranteed hearing; moves legitimacy and diplomatic cover to states that can demonstrate procedural compliance regardless of how narrowly they have drawn substantive eligibility.
% ABSENT_VOICES: Asylum seekers processed offshore or on accelerated tracks are the ones whose access to the procedural guarantees is thinnest, yet they have the least capacity to challenge how those guarantees were implemented in their specific case — by the time counsel or appeal review might help, removal or detention has often already occurred.
% DISAPPEARANCE_RATIONALE: If the procedural-integrity reading were abandoned, states would lose their principal compliance argument for narrow substantive definitions — either the restrictive reading would need its own independent legitimating apparatus, or courts would be forced toward substantive review of persecution claims directly, which would materially change approval rates and offshore/accelerated processing arrangements.
% FOUNDING_PROBLEM: The 1951 Convention needed to be administrable across dozens of legal systems with different capacities and different views of who deserves protection; a purely substantive standard ('genuine well-founded fear') is not self-executing and needs a procedure to operationalize it case by case.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR executive committee conclusions and refugee law scholarship (outside any single state's benefiting position) attest the procedural apparatus remains necessary to operationalize any substantive standard; but legal aid organizations and appellate dissents — also outside the state's own framing — attest that procedural compliance has increasingly become a substitute for, rather than a vehicle for, substantive fairness, particularly in offshore and accelerated contexts.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at T=40) reflects that under this reading, states can legally narrow substantive protection to near-zero for a given class of claimants while remaining in full Convention compliance, provided the hearing apparatus is intact — the 'extraction' is the gap between the promise of protection and the reality of narrow-plus-procedural outcomes. Suppression (0.62) captures the structural reality that once a state satisfies the procedural floor, there is very limited further recourse: courts largely will not re-litigate the substantive merits, so the hearing becomes the last real point of contest, and if that hearing is compressed or offshore-degraded, the claimant has functionally no exit. Theater ratio (0.44) tracks the rising divergence between the appearance of fair process (hearings held, appeals nominally available) and the functional erosion of fairness in accelerated and offshore contexts — this is the central drift this reading is built to detect and is exactly why it is authored as rising over the interval rather than flat.
 *
 * DIRECTIONALITY LOGIC:
 *   States with developed tribunal systems sit near the beneficiary end: they collect legitimacy and diplomatic cover from demonstrable procedural compliance, and their exit options (arbitrage between substantive strictness and procedural adequacy) are wide. Asylum seekers in offshore or accelerated tracks sit near the full-target end: trapped or heavily constrained, they cannot exit the procedure that determines their fate and cannot appeal to a substantive standard once process is nominally satisfied. UNHCR sits as an unusual near-beneficiary observer — it benefits from having a workable audit standard but has no coercive power to correct violations, so its 'benefit' is analytical traction, not extraction capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making a contestable substantive judgment (genuine fear of persecution) administrable across dozens of divergent legal systems — remains partly live: procedural architecture is still necessary to operationalize any substantive standard. But the classification prevents two opposite mislabelings: it does not let a state's offshore or accelerated procedure count as full coordination merely because forms exist (which the theater_ratio and suppression trend are designed to surface), and it does not treat the entire procedural apparatus as pure extraction merely because outcomes are sometimes restrictive, since the coordination function — a common, auditable standard usable across jurisdictions — remains genuinely served for claimants processed through developed, adequately resourced tribunals. The tangled_rope classification holds both truths at once rather than collapsing to either a clean rope or a pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_form_vs_substantive_capture,
    'At what point does a state''s procedural apparatus stop operationalizing genuine individualized assessment and start functioning as a compliance shield for predetermined substantive outcomes?',
    'Comparative empirical study of approval rates, average hearing duration, and appeal success rates across accelerated vs. standard tracks and offshore vs. domestic processing, controlling for underlying claim merit where independently verifiable (e.g. later resettlement outcomes).',
    'If accelerated/offshore tracks show approval rates far below claim-merit baselines while nominally satisfying procedural form, this reading''s tangled_rope classification would sharpen toward snare for those specific sub-arrangements even while the standard-track apparatus remains genuinely tangled_rope or even rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_form_vs_substantive_capture, empirical, 'Whether procedural compliance has become substantively decoupled from fair assessment in practice.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the procedural-integrity reading the Convention''s actual operative interpretation in international practice, or is it a contested third position that courts and states selectively invoke depending on which outcome (restrictive or expansive) it can be used to justify in a given case?',
    'Systematic review of state party reservations, UNHCR advisory opinions, and comparative jurisprudence to determine whether ''procedural integrity'' functions as an independent interpretive commitment or as a rhetorical bridge state actors invoke opportunistically between the restrictive and expansive readings.',
    'If procedural-integrity functions mainly as opportunistic bridging language rather than a stable independent commitment, this reading''s own claimed_type and metrics may themselves be less stable than authored here — the divergence would need to be captured by decomposing further rather than treating procedural-integrity as one clean reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether procedural-integrity is a genuine independent reading or an opportunistic hybrid invoked to legitimate either sibling reading''s outcomes.').

omega_variable(
    offshore_processing_guarantee_sufficiency,
    'What minimum bundle of procedural guarantees (counsel access, interpretation, appeal timeline, independent tribunal) must an offshore processing arrangement replicate before this reading treats it as Convention-compliant, and has any actual offshore arrangement met that bundle?',
    'Legal and empirical audit of specific offshore processing regimes (e.g. third-country transfer arrangements) against a codified procedural-adequacy checklist, cross-referenced with independent monitoring body findings.',
    'If no real-world offshore arrangement has ever met the full guarantee bundle, this reading''s tolerance of offshore processing ''in principle'' is doing no genuine limiting work in practice, which would push the victim-set for offshore-processed claimants toward a harder extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_guarantee_sufficiency, empirical, 'Whether the procedural bundle required for offshore processing legitimacy has ever been satisfied in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__procedural_integrity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(refu_tr_t8, refugee_convention_text__procedural_integrity_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(refu_tr_t16, refugee_convention_text__procedural_integrity_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(refu_tr_t24, refugee_convention_text__procedural_integrity_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(refu_tr_t32, refugee_convention_text__procedural_integrity_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(refu_tr_t40, refugee_convention_text__procedural_integrity_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(refu_be_t8, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(refu_be_t16, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(refu_be_t24, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(refu_be_t32, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(refu_be_t40, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(refu_su_t8, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(refu_su_t16, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(refu_su_t24, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(refu_su_t32, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(refu_su_t40, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% This story is the procedural-integrity reading within the refugee_convention_text kernel family. It shares the same textual kernel as restrictive_sovereignty_reading and expansive_humanitarian_reading but authors a structurally distinct beneficiary/victim set and a distinct extraction mechanism: extraction here arises specifically from the gap between procedural form and substantive access (offshore/accelerated processing), not from narrow substantive definitions per se (the restrictive reading's mechanism) or from denial of broad humanitarian categories (the expansive reading's mechanism). Each reading carries its own stable ε; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
