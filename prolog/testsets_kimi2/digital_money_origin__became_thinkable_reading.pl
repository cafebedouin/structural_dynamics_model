% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin â Conceptual Conceivability Reading
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint instantiates the became_thinkable_reading of the
 *   contested kernel digital_money_origin. The kernel asks when digital money
 *   emerged; this reading locates emergence at the point when the concept
 *   became technically and institutionally conceivableâprior to widespread
 *   practical implementationâthereby privileging conceptual architects and
 *   institutional researchers over practical users. The constraint operates
 *   as an epistemic-historiographic arrangement that distributes credit,
 *   curricular centrality, and policy legitimacy asymmetrically.
 *
 * KEY AGENTS:
 *   - early_conceptual_architects: Primary beneficiary (moderate/global) â receive historical credit and academic prestige
 *   - central_bank_researchers: Secondary beneficiary (institutional/national) â lend institutional credibility to early-origin framing
 *   - academic_monetary_historians: Agenda-setter (institutional/global) â maintain the narrative through peer review and curricula
 *   - practical_early_users: Primary target (powerless/global) â bear erasure from canonical history
 *   - informal_value_communities: Secondary target (powerless/local) â excluded from archival and narrative record
 *   - economic_anthropologists: Analytical observer (organized/global) â contest the framing from a practice-based perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.48).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.4).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin â Conceptual Conceivability Reading").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '379f48b1-a7a0-405c-a6ae-8a5cd85ce839').
narrative_ontology:cs_kernel_codification('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', distributed).
narrative_ontology:cs_authority_grounding('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', expertise).
narrative_ontology:cs_interpretation_layer_present('379f48b1-a7a0-405c-a6ae-8a5cd85ce839').
narrative_ontology:cs_reading_relation('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', foundational, conceptual_priority_over_practice).
narrative_ontology:cs_axiom_status(conceptual_priority_over_practice, holdable).
narrative_ontology:cs_axiom_grounding('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', conceptual_priority_over_practice, conventional).
narrative_ontology:cs_reference_frame('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', intellectual_history_threshold).
narrative_ontology:cs_drift_state('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', contemporary_digital_money_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('379f48b1-a7a0-405c-a6ae-8a5cd85ce839', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_conceptual_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, central_bank_researchers).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, practical_early_users).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, informal_value_communities).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, technological_origins_are_conceptual).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, monetary_innovation_requires_institutional_forethought).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cryptographers and computer scientists who published foundational papers on digital cash and cryptographic protocols in the 1980s and 1990s; they are credited as the originators of digital money in standard narratives and receive academic prestige and historical priority.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_conceptual_architects, beneficiary,
    moderate, biographical, mobile, global).

% Economists and researchers inside monetary authorities who produced working papers and policy simulations on electronic money before public deployment; their institutional affiliation lends credibility to the early-origin framing and secures research budgets.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, central_bank_researchers, beneficiary,
    institutional, generational, constrained, national).

% University-based historians and science-and-technology-studies scholars who write textbooks, peer-reviewed articles, and conference papers locating digital money's origin in conceptual prefiguration; they control citation networks, curriculum design, and peer-review gatekeeping.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, academic_monetary_historians, agenda_setter,
    institutional, generational, constrained, global).

% Individuals who held and transacted non-physical value through systems like DigiCash, e-gold, or early game currencies before these were recognized in mainstream monetary history; their experience is absent from canonical accounts and they lack channels to correct the record.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, practical_early_users, payer,
    powerless, immediate, trapped, global).

% Groups using local digital scrip, virtual-world currencies, or peer-to-peer credit systems that functioned as money without formal institutional design; they lack archival presence and are omitted from the origin narrative.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, informal_value_communities, payer,
    powerless, immediate, trapped, local).

% Scholars who study money from practice-based and user-centered perspectives; they observe the constraint's operation and argue for alternative origin thresholds but hold limited sway over mainstream monetary historiography.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, economic_anthropologists, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, diffuse).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared chronological anchor for monetary history, aligning technologists, historians, and policymakers around a narrative that traces contemporary digital money to mid-20th-century conceptual breakthroughs in cryptography and central-bank research.
% TRANSFER_FUNCTION: Moves historical credit, curricular centrality, and policy legitimacy from practical early users and informal value communities to the conceptual architects and institutional researchers who articulated digital money before it was widely implemented.
% ABSENT_VOICES: Practical early users of e-cash, game-currency traders, and informal scrip communities are absent from the canonical origin story; they would object that their practice constituted digital money long before institutional recognition.
% DISAPPEARANCE_RATIONALE: If the conceptual-conceivability framing disappeared, monetary-history curricula would rewrite their opening chapters, credit would shift to practical user communities, and the institutional pedigree of current CBDC and cryptocurrency programs would lose its claimed pre-history.
% FOUNDING_PROBLEM: How to construct a coherent, authoritative pre-history for digital money that connects present-day cryptocurrency and central-bank digital currency to recognizable institutional and intellectual lineages.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of technology and cryptography corroborate the conceptual-origin narrative from their own disciplinary frameworks; economic anthropologists and user-centered historians contest it, arguing that practiceânot conceptionâshould mark origin. No neutral corroboration exists outside these competing expert communities.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate-high because the framing systematically diverts historiographic credit and epistemic standing from practical users to conceptual architects. Suppression (0.40) reflects the marginalization of practice-based origin stories in mainstream monetary history, enforced through peer review and curriculum design rather than overt coercion. Theater ratio (0.25) is low-moderate: citation of canonical texts performs disciplinary membership, but genuine intellectual work also occurs. Accessibility collapse (0.55) captures how field socialization makes alternative origin thresholds hard to perceive. Resistance (0.45) registers the live contestation from the first_held_reading and regulatory_recognition_reading siblings. Measurements share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats experience the constraint as legitimate intellectual history that correctly identifies conceptual foresight as the engine of innovation. The payer seats experience the same arrangement as enforced erasure: their practice is invisible, their contribution unarchived, and their objections inaudible within mainstream historiography. The engine computes this divergence from the structural asymmetry in power, exit, and beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (early_conceptual_architects, central_bank_researchers) sit near the low-d end because the constraint subsidizes their historical priority and institutional credibility. The agenda-setter (academic_monetary_historians) also benefits from disciplinary control. Payers (practical_early_users, informal_value_communities) sit near the high-d end because the constraint extracts their historical agency and epistemic standing; their trapped exit options amplify effective extraction. The observer (economic_anthropologists) sits near d=0.5 with analytical exit, neither subsidized nor extracted.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination functionâa field needs shared origin narratives to operateâwhile flagging the asymmetric extraction embedded in the same structure. A snare classification would overclaim by treating the coordination story as pure cover; a rope classification would underclaim by ignoring the systematic exclusion of practical users. The Tangled Rope gate requires both beneficiaries and victims plus active enforcement, which the schema enforces and which the narrative satisfies through peer-review gatekeeping and curricular reproduction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does digital money originate at conceptual conceivability, first practical holding, or regulatory recognition?',
    'Archival and ethnographic recovery of early practical digital value use; historiographic consensus conference or meta-analysis.',
    'If practical holding predates conceptual conceivability, the became_thinkable_reading''s beneficiary-victim structure inverts; if regulatory recognition is the true origin, the constraint shifts to a later interval with different architects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the digital_money_origin kernel is structurally correct').

omega_variable(
    conceptual_origin_constructedness,
    'Is the conceptual-origin threshold a natural feature of technological historiography, or a constructed narrative serving institutional architects?',
    'Comparative historiography of other technologies to test whether conceptual-conceivability framing systematically privileges institutional researchers.',
    'If constructed, the constraint''s epsilon rises and it moves toward snare; if natural, it moves toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_origin_constructedness, conceptual, 'Whether the origin threshold is natural or constructed').

omega_variable(
    excluded_practitioner_recovery,
    'Can excluded practical users and informal communities be recovered into the origin narrative without dissolving the constraint?',
    'Oral history and archival projects documenting pre-Bitcoin digital value practices.',
    'Recovery would reduce extractiveness by broadening the beneficiary set; complete recovery would dissolve asymmetric extraction and potentially reclassify as rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_practitioner_recovery, empirical, 'Whether practical users can be recovered into the narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digital_money_origin_btr_tr_t0, digital_money_origin__became_thinkable_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digital_money_origin_btr_tr_t5, digital_money_origin__became_thinkable_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(digital_money_origin_btr_tr_t10, digital_money_origin__became_thinkable_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(digital_money_origin_btr_tr_t15, digital_money_origin__became_thinkable_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(digital_money_origin_btr_tr_t20, digital_money_origin__became_thinkable_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(digital_money_origin_btr_tr_t25, digital_money_origin__became_thinkable_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(digital_money_origin_btr_tr_t30, digital_money_origin__became_thinkable_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(digital_money_origin_btr_be_t0, digital_money_origin__became_thinkable_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(digital_money_origin_btr_be_t5, digital_money_origin__became_thinkable_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(digital_money_origin_btr_be_t10, digital_money_origin__became_thinkable_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(digital_money_origin_btr_be_t15, digital_money_origin__became_thinkable_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(digital_money_origin_btr_be_t20, digital_money_origin__became_thinkable_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(digital_money_origin_btr_be_t25, digital_money_origin__became_thinkable_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(digital_money_origin_btr_be_t30, digital_money_origin__became_thinkable_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(digital_money_origin_btr_su_t0, digital_money_origin__became_thinkable_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(digital_money_origin_btr_su_t5, digital_money_origin__became_thinkable_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(digital_money_origin_btr_su_t10, digital_money_origin__became_thinkable_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(digital_money_origin_btr_su_t15, digital_money_origin__became_thinkable_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(digital_money_origin_btr_su_t20, digital_money_origin__became_thinkable_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(digital_money_origin_btr_su_t25, digital_money_origin__became_thinkable_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement(digital_money_origin_btr_su_t30, digital_money_origin__became_thinkable_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% The natural-language question 'when did digital money emerge' decomposes into three structurally distinct constraints: a conceptual-conceivability threshold (this reading), a first-practical-holding threshold, and a regulatory-recognition threshold. Each has distinct epsilon, beneficiaries, and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
