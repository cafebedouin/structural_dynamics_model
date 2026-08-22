% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Script Reform as Literacy/Administrative Efficiency Mandate (Instrumentalist Reading)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   This story authors the instrumentalist reading of a contested kernel
 *   about orthographic legitimacy: script reform is justified purely by
 *   measurable gains in literacy rate and administrative throughput. Under
 *   this reading the old script is a technical tool being replaced by a more
 *   efficient one, and the scribal/religious elite whose skills are devalued
 *   are a transition cost rather than a civilizational casualty. This is
 *   deliberately narrower than the modernist reading (which frames the same
 *   reform as rupture from an Ottoman/Islamic past) and the continuity
 *   reading (which treats the old script as irreplaceable access to religious
 *   and literary heritage) — those are separate constraints with their own ε
 *   and stakeholder structure, linked here only by network reference, not
 *   folded into this one.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: primary agenda-setter and beneficiary — designs and enforces the reform on efficiency grounds
 *   - newly_literate_population and rural_school_children: primary beneficiaries — gain literacy faster under the new script
 *   - arabic_literate_scribal_elite and religious_scholars_using_old_script: primary victims — professional and institutional capital devalued
 *   - older_generation_now_functionally_illiterate: secondary victims — lose functional literacy overnight
 *   - literacy_statisticians_and_planners: analytical observers — produce the evidentiary basis for the legitimacy claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.55).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Script Reform as Literacy/Administrative Efficiency Mandate (Instrumentalist Reading)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, 'cf0589ce-778f-42cb-b033-463fd3303bbd').
narrative_ontology:cs_kernel_codification('cf0589ce-778f-42cb-b033-463fd3303bbd', distributed).
narrative_ontology:cs_authority_grounding('cf0589ce-778f-42cb-b033-463fd3303bbd', extraction).
narrative_ontology:cs_interpretation_layer_present('cf0589ce-778f-42cb-b033-463fd3303bbd').
narrative_ontology:cs_reading_relation('cf0589ce-778f-42cb-b033-463fd3303bbd', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf0589ce-778f-42cb-b033-463fd3303bbd', orthographic_legitimacy_kernel__continuity_reading, influences).
narrative_ontology:cs_axiom('cf0589ce-778f-42cb-b033-463fd3303bbd', foundational, legitimacy_tracks_measurable_literacy_outcomes).
narrative_ontology:cs_axiom_status(legitimacy_tracks_measurable_literacy_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('cf0589ce-778f-42cb-b033-463fd3303bbd', legitimacy_tracks_measurable_literacy_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('cf0589ce-778f-42cb-b033-463fd3303bbd', foundational, script_is_a_neutral_instrument_not_an_identity_marker).
narrative_ontology:cs_axiom_status(script_is_a_neutral_instrument_not_an_identity_marker, holdable).
narrative_ontology:cs_axiom_grounding('cf0589ce-778f-42cb-b033-463fd3303bbd', script_is_a_neutral_instrument_not_an_identity_marker, instrumental).
narrative_ontology:cs_reference_frame('cf0589ce-778f-42cb-b033-463fd3303bbd', technocratic_efficiency_baseline).
narrative_ontology:cs_drift_state('cf0589ce-778f-42cb-b033-463fd3303bbd', post_literacy_plateau_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf0589ce-778f-42cb-b033-463fd3303bbd', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, rural_school_children).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_scribal_elite).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, religious_scholars_using_old_script).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, older_generation_now_functionally_illiterate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the new script's mandatory use in schools, government documents, and print, justifying the change through literacy statistics and administrative throughput rather than any claim about civilizational identity. Collects the benefit of a more easily taught, faster-to-typeset writing system and a population whose literacy is achieved on the state's terms and timetable.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus, beneficiary).

% Mostly rural and previously unschooled adults and children who can now learn to read and write within a much shorter instructional period than the old orthography required. They did not choose the script but gain functional literacy from it; their exit option is moot because they had no prior literacy to lose.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    powerless, biographical, constrained, national).

% Enter a school system built entirely around the new script's phonetic logic, acquiring literacy at a pace the old system could not match. They have no memory of the old script and no stake in the transition costs it imposed on their elders.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, rural_school_children, beneficiary,
    powerless, generational, trapped, national).

% Clerks, calligraphers, and administrative scribes whose years of training in the old orthography are abruptly devalued. Their professional capital does not transfer; the instrumentalist justification for reform treats their skill as a private sunk cost, not something the state owes compensation for.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_scribal_elite, payer,
    moderate, biographical, trapped, national).

% Continue operating in institutions (mosques, madrasas, religious courts) where the old script remains functionally necessary, but face shrinking state support and declining new entrants trained in it. Under this reading their loss is registered only as an administrative externality, not as a cultural harm — that framing is what the modernist and continuity readings dispute.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, religious_scholars_using_old_script, payer,
    moderate, civilizational, constrained, national).

% Adults who were literate in the old script wake up functionally illiterate in the new administrative and print environment overnight. They bear a sudden, uncompensated loss of access to newspapers, official forms, and public signage, justified by the state as a necessary transitional cost of raising aggregate literacy rates.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, older_generation_now_functionally_illiterate, payer,
    powerless, biographical, trapped, national).

% Measure literacy rate changes, printing costs, and administrative processing times before and after reform, producing the evidentiary basis on which the instrumentalist legitimacy claim rests. They have no stake in script choice beyond its measurable outcomes.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, literacy_statisticians_and_planners, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, phonetically efficient orthography lets the state teach basic literacy faster, print and process documents more cheaply, and administer a large population with fewer years of schooling required per literate citizen — a genuine coordination gain over a script requiring years of specialized training to master.
% TRANSFER_FUNCTION: Moves literacy access and administrative legibility from a narrow trained scribal class toward the general population, while transferring the sunk cost of prior script training from the state (which need not compensate it) onto the individuals who held it.
% ABSENT_VOICES: The Arabic-literate scribal elite and religious scholars are consulted, if at all, as a technical transition-cost line item rather than as parties whose professional and institutional standing is being restructured; under this reading their objections are treated as friction to be managed, not as legitimate counter-claims about what the script is for.
% DISAPPEARANCE_RATIONALE: If the instrumentalist orthographic mandate were withdrawn, mandatory instruction and processing would revert to whatever script commanded the most institutional inertia, literacy campaigns would lose their statistical justification, and print/administrative costs would rise — schools, presses, and civil registries are all built around the new script's assumed efficiency.
% FOUNDING_PROBLEM: Mass illiteracy and slow, error-prone administrative processing in a script whose complexity made basic reading and writing achievable only after years of specialized instruction, limiting both citizen literacy and state administrative capacity.
% FOUNDING_PROBLEM_CORROBORATION: Independent literacy researchers and UNESCO-style education statisticians outside the reforming state's own ministries have corroborated large literacy-rate gains attributable to orthographic simplification in comparable reforms; however, historians and linguists outside the state apparatus dispute whether the residual devaluation of prior-script literacy was a necessary cost of that gain or an avoidable byproduct of how the transition was administered.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and stabilizes rather than escalates: the reform delivers a genuine, measurable coordination gain (faster literacy acquisition, cheaper administration) and the cost falls on a bounded, aging cohort of prior-script specialists whose losses do not compound over time — this is structurally different from an open-ended extraction. Suppression starts higher (0.75) during the mandatory-enforcement rollout period (banning old-script instruction, replacing signage and print) and declines (0.55) as the new script becomes the unremarked default and active coercion is no longer needed to sustain it. Theater ratio stays low and rises only slightly (0.1 to 0.2) because the instrumentalist justification is falsifiable by literacy statistics themselves — there is little room for the mandate to become pure performance while the numbers keep being cited as its evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state administrative apparatus sits at the beneficiary end: it designs the constraint and captures the coordination gain of a cheaper, faster-to-teach system. Newly literate population and rural school children are also structural beneficiaries — the constraint subsidizes their literacy acquisition even though they had no say in script choice. The scribal elite and religious scholars are pushed toward the target end: their exit options are trapped or constrained (their capital does not transfer to any other line of work or institutional field), and the constraint's coordination story is read, in this instrumentalist frame, as adequately compensating them merely by the aggregate literacy gain — which is precisely the erasure the corroboration note flags.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass illiteracy, slow administration under the old orthography) is genuinely live in the transition period and can be independently corroborated by external literacy statistics — this blocks a premature 'zombie mandate' verdict. But once literacy rates plateau at a new steady state, the instrumentalist justification for continuing to actively suppress or disfavor the old script (rather than merely no longer mandating it) becomes harder to sustain on efficiency grounds alone; the declining suppression_requirement trajectory models exactly that handoff from active enforcement to unremarked default.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentalist_frame_as_cover_for_rupture,
    'Is the instrumentalist justification (literacy statistics, administrative efficiency) a genuinely separate legitimacy claim from the modernist rupture narrative, or is it the modernist reading''s preferred public-facing packaging — i.e., does the same reform get justified instrumentally in policy documents while functioning as civilizational rupture in political rhetoric?',
    'Comparative discourse analysis of the reform''s official justificatory documents (ministry reports, legislative debates) versus contemporaneous political speeches and party platforms; convergence would suggest the readings are analytically distinct rhetorical registers of one underlying political project rather than genuinely independent legitimacy claims.',
    'If the instrumentalist frame is shown to be a cover story for the modernist rupture project, this reading''s moderate ε understates the true extraction, since it would be laundering an identity-suppression agenda through efficiency metrics that are not actually the operative motive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalist_frame_as_cover_for_rupture, conceptual, 'Whether the instrumentalist reading is analytically independent of the modernist reading or its technocratic packaging.').

omega_variable(
    efficiency_metric_selection_bias,
    'Were the literacy and administrative-efficiency metrics used to justify the reform selected and measured by parties with a stake in the reform succeeding, and would alternative metrics (e.g., loss of access to the pre-reform textual corpus, transition-period literacy dip among adults) change the instrumentalist case?',
    'Independent retrospective analysis using metrics not selected by the reforming state itself — e.g., international literacy surveys conducted by third parties, or measures of documented-heritage accessibility before and after the transition.',
    'If the state''s own selected metrics overstate net efficiency gains relative to independently measured costs, the claimed rope-like coordination story weakens toward tangled_rope, since the coordination gain would be partly illusory relative to the extraction from the scribal elite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_metric_selection_bias, empirical, 'Whether the metrics grounding the instrumentalist legitimacy claim are independently verifiable or self-selected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(orth_tr_t4, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(orth_tr_t8, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(orth_tr_t12, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(orth_tr_t16, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(orth_be_t4, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(orth_be_t8, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(orth_be_t12, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(orth_be_t16, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(orth_su_t4, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(orth_su_t8, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(orth_su_t12, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(orth_su_t16, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__instrumentalist_reading, 0.05).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the orthographic_legitimacy_kernel, each authored as a structurally distinct constraint with its own ε per the ε-invariance principle: instrumentalist_reading (this file, moderate ε ~0.42, rope-claimed, beneficiary=newly literate population, victim=scribal/religious elite, justified by literacy statistics); modernist_reading (identity-rupture framing, expected higher ε and different victim salience — the old script becomes a marker of a rejected past rather than merely an obsolete tool); continuity_reading (frames the same script change as extraction from religious/literary tradition, expected to register victims — those cut off from historical/religious texts — as the central harm rather than a side effect). All three describe the same historical reform event but are analytically distinct legitimacy claims with different ε values, different beneficiary/victim framings, and different failure modes; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
