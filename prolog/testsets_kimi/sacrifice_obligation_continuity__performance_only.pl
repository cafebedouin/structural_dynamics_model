% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation: Performance-Only Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint instantiates the performance-only reading of the
 *   sacrifice-obligation continuity kernel: the divine commandment to offer
 *   physical sacrifices remains fully binding despite the destruction of the
 *   Temple, and textual study of sacrificial law is explicitly framed as
 *   preparation for future restoration rather than fulfillment. The current
 *   generation is thus locked into a state of permanent obligation without
 *   remedy, generating guilt and juridical subordination that accrues
 *   authority to the textual experts who administer the unperformable law.
 *   The reading actively suppresses competing interpretations â
 *   particularly study-as-performance, which would discharge the obligation
 *   through engagement with texts, and messianic suspension, which would
 *   remove present-tense liability.
 *
 * KEY AGENTS:
 *   - halakhic_authorities: Primary agenda-setter (institutional/identity_locked/global) â derive authority from administering the unfulfillable commandment
 *   - observant_community: Primary payer (organized/identity_locked/global) â bears unfulfillable obligation and attendant guilt
 *   - study_as_performance_advocates: Excluded reading-holders (moderate/constrained) â their interpretation is structurally barred from legitimacy
 *   - messianic_restoration_movement: Excluded activists (moderate/constrained) â literal path to discharge blocked by political and institutional realities
 *   - academic_textual_historians: Analytical observers (analytical/analytical) â document the reading's historical emergence without normative commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.78).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, 'd4842ceb-680c-4861-8957-829491c5796e').
narrative_ontology:cs_kernel_codification('d4842ceb-680c-4861-8957-829491c5796e', fixed_text).
narrative_ontology:cs_authority_grounding('d4842ceb-680c-4861-8957-829491c5796e', lineage).
narrative_ontology:cs_interpretation_layer_present('d4842ceb-680c-4861-8957-829491c5796e').
narrative_ontology:cs_reading_relation('d4842ceb-680c-4861-8957-829491c5796e', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('d4842ceb-680c-4861-8957-829491c5796e', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('d4842ceb-680c-4861-8957-829491c5796e', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('d4842ceb-680c-4861-8957-829491c5796e', foundational, physical_performance_categorically_required).
narrative_ontology:cs_axiom_status(physical_performance_categorically_required, holdable).
narrative_ontology:cs_axiom_grounding('d4842ceb-680c-4861-8957-829491c5796e', physical_performance_categorically_required, deontological).
narrative_ontology:cs_axiom('d4842ceb-680c-4861-8957-829491c5796e', foundational, study_cannot_substitute_for_action).
narrative_ontology:cs_axiom_status(study_cannot_substitute_for_action, holdable).
narrative_ontology:cs_axiom_grounding('d4842ceb-680c-4861-8957-829491c5796e', study_cannot_substitute_for_action, conventional).
narrative_ontology:cs_reference_frame('d4842ceb-680c-4861-8957-829491c5796e', temple_cult_operational).
narrative_ontology:cs_drift_state('d4842ceb-680c-4861-8957-829491c5796e', post_temple_exilic_continuity, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d4842ceb-680c-4861-8957-829491c5796e', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, halakhic_authorities).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, observant_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, physical_performance_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, textual_authority_over_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the ruling that physical sacrifice remains obligatory and that textual study cannot substitute for ritual performance. Their authority derives from maintaining the juridical continuity of an unperformable commandment; they train successors, adjudicate related laws, and enforce the boundary between preparation and fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, halakhic_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Bears the active obligation to bring sacrifices without access to a Temple or priesthood. Studies the laws as commanded but is taught that this study does not discharge the debt. Lives with the structural guilt of an unfulfillable commandment, marked by fasting and petitionary prayer as secondary responses.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, observant_community, payer,
    organized, biographical, identity_locked, global).

% Hold that intensive study of sacrificial law is itself a form of fulfillment. Their position is ruled out by the dominant interpretive tradition; they are treated as well-meaning but juridically mistaken, and their reading is kept outside the bounds of acceptable halakhic opinion.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, study_as_performance_advocates, excluded,
    moderate, biographical, constrained, regional).

% Advocates for rebuilding the Temple to resume physical sacrifice. Their agenda is tolerated as aspirational but is politically and practically blocked; they are excluded from mainstream institutional power yet remain vocal, representing the only literal path to obligation-discharge.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_restoration_movement, excluded,
    moderate, generational, constrained, national).

% Analyze the historical emergence of the performance-only reading against competing ancient interpretations. They document when and how the study-as-fulfillment option was suppressed but take no normative position on obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, academic_textual_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, halakhic_authorities).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains communal continuity across generations during the absence of central cultic infrastructure; keeps the textual tradition and practical knowledge of sacrifice alive for a future restoration.
% TRANSFER_FUNCTION: Moves obligation, guilt, and juridical authority from a dischargeable ritual frame into a permanent state of preparation administered by textual experts; the current generation transfers compliance-effort upward to authorities who define its limits.
% ABSENT_VOICES: The study-as-performance school and messianic-restoration activists are materially present but juridically excluded; the non-observant population is simply outside the conversation. Most absent are the biblical-era priests whose institutional extinction created the gap.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished, the community would reorganize around either study-as-fulfillment (lowering guilt and elevating textual engagement) or messianic suspension (removing present obligation); juridical authority would shift away from the gatekeepers of the unfulfillable commandment.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the physical site and priestly apparatus required for divine commandment-fulfillment, creating a crisis of continuity for a religion centered on sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of religion and archaeologists attest the Temple's destruction and the absence of sacrifice; the observant community itself marks the loss through annual mourning rituals (Tisha B'Av), corroborating from inside the beneficiary set that the founding crisis persists as absence. No external corroborator attests the obligation remains live â that claim is self-asserted by the halakhic authorities.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the obligation is unfulfillable yet fully binding, creating a permanent guilt-debt with no discharge mechanism. Suppression (0.78) is high because the reading's persistence depends on actively excluding study-as-performance and suspension alternatives; theater_ratio (0.42) reflects the partial performativity of intensive study that mimics fulfillment without achieving it. Accessibility_collapse (0.80) is high because once the performance-only frame is accepted, alternative readings appear as heretical or ignorant. Resistance (0.35) is moderate: competing readings exist but are marginalized, not overtly suppressed by violence.
 *
 * PERSPECTIVAL GAP:
 *   The halakhic-authority seat experiences the constraint as legitimate continuity and guardianship; the observant-community seat experiences it as an unpayable debt. The engine will compute these seats differently because the former is structurally a beneficiary (authority accrues) with identity-locked exit, while the latter is a victim (guilt without remedy) with equally identity-locked exit but opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities are declared beneficiaries because they collect institutional authority, deference, and a civilizational role from being the necessary administrators of the unfulfillable obligation. The observant community are declared victims because they bear the costs: permanent obligation, structural guilt, and the psychological burden of an unredeemable debt. Study-as-performance advocates are excluded rather than victims â they are not extracted from but silenced. Directionality derives from these declarations: d near 0.0 for authorities (subsidized by the constraint), d near 1.0 for the observant community (target of extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Temple destruction â is dead (the Temple has not stood for two millennia), yet the arrangement persists. This triggers the R5 mismatch flag: dead founding_problem + world_rearranges disappearance verdict signals a zombie constraint. It is classified as tangled_rope rather than snare because the coordination function (maintaining textual continuity and communal identity across generations) is structurally genuine, not merely cover; it is not a piton because there is a concentrated beneficiary (halakhic authorities) who actively maintains the constraint and would lose authority if it were reclassified or removed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unfulfillable_obligation_mental_health,
    'Does the performance-only reading produce measurable psychological harm (chronic guilt, anxiety, religious obsessive-compulsion) distinguishable from normal ritual discipline?',
    'Comparative mental-health studies across Jewish communities holding different readings (performance-only vs. study-as-performance vs. messianic-suspension) controlling for orthodoxy level.',
    'If harm is significant and specific, the extraction is not merely juridical but somatic, pushing classification toward snare; if not, the extraction remains symbolic/institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unfulfillable_obligation_mental_health, empirical, 'Whether the unfulfillable obligation produces distinctive psychological harm.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readings achieved through institutional authority alone, or through internalized identity fusion that makes exit psychologically unavailable?',
    'Post-exit trajectory study: do individuals leaving the performance-only framework retain guilt patterns, or does suppression evaporate with institutional removal?',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates partly as cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    kernel_reading_contest,
    'Does the performance-only reading represent a genuine continuity with the pre-destruction legal framework, or a retroactive construction by rabbinic authorities to secure institutional position?',
    'Text-critical and historical-legal analysis of the earliest rabbinic sources on sacrifice-in-absence.',
    'If retroactive construction, the false-summit dynamic applies â the constraint claims mountain-like continuity but is a constructed snare; if genuine continuity, the tangled-rope coordination component is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the reading is genuine continuity or retroactive construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perfonly_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perfonly_tr_t500, sacrifice_obligation_continuity__performance_only, theater_ratio, 500, 0.3).
narrative_ontology:measurement(perfonly_tr_t1000, sacrifice_obligation_continuity__performance_only, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(perfonly_tr_t1500, sacrifice_obligation_continuity__performance_only, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(perfonly_tr_t1800, sacrifice_obligation_continuity__performance_only, theater_ratio, 1800, 0.4).
narrative_ontology:measurement(perfonly_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.42).

% Extraction over time
narrative_ontology:measurement(perfonly_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(perfonly_be_t500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 500, 0.72).
narrative_ontology:measurement(perfonly_be_t1000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1000, 0.78).
narrative_ontology:measurement(perfonly_be_t1500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1500, 0.8).
narrative_ontology:measurement(perfonly_be_t1800, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1800, 0.83).
narrative_ontology:measurement(perfonly_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(perfonly_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perfonly_su_t500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 500, 0.65).
narrative_ontology:measurement(perfonly_su_t1000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1000, 0.72).
narrative_ontology:measurement(perfonly_su_t1500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1500, 0.75).
narrative_ontology:measurement(perfonly_su_t1800, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1800, 0.76).
narrative_ontology:measurement(perfonly_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
