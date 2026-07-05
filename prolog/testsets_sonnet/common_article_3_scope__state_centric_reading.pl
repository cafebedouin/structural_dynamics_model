% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Applicability Threshold (State-Centric Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the state-centric reading of the Common Article 3
 *   scope kernel: CA3's humanitarian floor applies only when an internal
 *   conflict crosses intensity and organization thresholds (the ICTY
 *   Tadić-style test for 'protracted armed violence' by an 'organized armed
 *   group'), excluding riots, isolated acts of violence, and ordinary law
 *   enforcement. This reading gives the classifying state near-total
 *   discretion to determine whether its own internal violence qualifies, and
 *   functions as a genuine coordination device (distinguishing internal
 *   policing from armed conflict) that has, in documented state practice,
 *   drifted toward selective non-classification precisely where
 *   classification would be most legally costly to the state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.58).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.79).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Applicability Threshold (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '94ea1cf4-81cf-4a22-a733-65a920c822c4').
narrative_ontology:cs_kernel_codification('94ea1cf4-81cf-4a22-a733-65a920c822c4', fixed_text).
narrative_ontology:cs_authority_grounding('94ea1cf4-81cf-4a22-a733-65a920c822c4', extraction).
narrative_ontology:cs_interpretation_layer_present('94ea1cf4-81cf-4a22-a733-65a920c822c4').
narrative_ontology:cs_reading_relation('94ea1cf4-81cf-4a22-a733-65a920c822c4', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('94ea1cf4-81cf-4a22-a733-65a920c822c4', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('94ea1cf4-81cf-4a22-a733-65a920c822c4', foundational, sovereignty_preserving_threshold_necessity).
narrative_ontology:cs_axiom_status(sovereignty_preserving_threshold_necessity, holdable).
narrative_ontology:cs_axiom_grounding('94ea1cf4-81cf-4a22-a733-65a920c822c4', sovereignty_preserving_threshold_necessity, conventional).
narrative_ontology:cs_axiom('94ea1cf4-81cf-4a22-a733-65a920c822c4', foundational, state_classification_discretion_is_authoritative).
narrative_ontology:cs_axiom_status(state_classification_discretion_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('94ea1cf4-81cf-4a22-a733-65a920c822c4', state_classification_discretion_is_authoritative, conventional).
narrative_ontology:cs_reference_frame('94ea1cf4-81cf-4a22-a733-65a920c822c4', tadic_intensity_organization_test).
narrative_ontology:cs_drift_state('94ea1cf4-81cf-4a22-a733-65a920c822c4', post_war_on_terror_classification_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94ea1cf4-81cf-4a22-a733-65a920c822c4', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, counterinsurgency_commanders).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_zones).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, detainees_in_unclassified_operations).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, state_sovereignty_over_internal_security_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, intensity_organization_threshold_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determine, through military legal advisors and executive classification, whether a given internal conflict meets the intensity and organization thresholds that trigger CA3. Retain the authority to characterize violence as 'law enforcement,' 'riot,' or 'banditry' rather than armed conflict, which keeps the operation outside the CA3 framework entirely and under domestic criminal law with fewer humane-treatment guarantees.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Conduct counter-insurgency and internal security operations. When violence is classified below threshold, they operate under law-enforcement rules of engagement and domestic legal cover rather than IHL's minimum humane treatment obligations, giving commanders greater latitude in detention, interrogation, and use of force.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_militaries, beneficiary,
    institutional, biographical, mobile, national).

% Make operational and tactical classification calls in the field about whether an engagement counts as 'protracted armed violence' by an 'organized' group. Their threshold judgments shape which detainees and combatants fall under CA3 protections versus ordinary criminal process.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, counterinsurgency_commanders, beneficiary,
    powerful, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, counterinsurgency_commanders, agenda_setter).

% Fight in loosely organized, sporadic, or fragmented armed groups that a state can plausibly argue fail the organization or intensity test. When captured, they receive no CA3 floor — no guaranteed prohibition on cruel treatment, no fair-trial guarantee — because the state has classified the conflict as below the threshold. They have no forum to contest the classification before it is applied to them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, regional).

% Live in areas experiencing chronic but sub-threshold violence — recurring clashes, targeted killings, displacement — that never crosses the state's declared intensity bar. They experience conflict-like harms without the protective architecture that a formal armed-conflict classification would trigger, and depend entirely on the state's own threshold determination for any legal shield.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_zones, payer,
    powerless, immediate, trapped, local).

% Held by state forces in operations the government has declined to classify as reaching CA3 thresholds. Their treatment is governed by ordinary domestic detention law rather than the CA3 floor, which — depending on domestic legal quality — may permit prolonged incommunicado detention or diminished due process.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, detainees_in_unclassified_operations, payer,
    powerless, immediate, trapped, national).

% Argue publicly and in commentary that the intensity/organization thresholds are being manipulated to avoid CA3 obligations, but have no binding authority to override a state's classification of its own internal conflict. Their assessments carry normative weight but no compulsory force against the state-centric reading.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc_and_human_rights_monitors, excluded,
    organized, generational, constrained, global).

% Occasionally review threshold determinations after the fact, in war crimes tribunals or human rights bodies, applying tests like the ICTY's Tadić criteria. Their post-hoc jurisprudence can criticize a state's classification but rarely arrives in time to protect the detainees or combatants whose treatment already occurred under the narrower reading.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides states with a workable, administrable line between ordinary internal policing (governed by domestic law and human rights law alone) and non-international armed conflict (governed additionally by CA3's humanitarian floor), preventing every riot or gang skirmish from triggering full IHL machinery.
% TRANSFER_FUNCTION: Moves legal protection away from irregular combatants, low-intensity-zone civilians, and unclassified detainees, and moves operational discretion and legal insulation toward the state military and executive branch that controls the classification decision.
% ABSENT_VOICES: Irregular combatants and civilians in contested zones have no forum to contest a government's threshold determination before it is applied to them; the ICRC and human rights monitors object publicly but hold no binding authority to compel reclassification, and international tribunals typically rule only years after the harm.
% DISAPPEARANCE_RATIONALE: If the intensity/organization threshold vanished and CA3 applied to any organized armed violence (the expansive reading), governments would lose the discretionary classification lever entirely — internal security operations against loosely organized or sporadic armed groups would become subject to CA3's humane-treatment floor by default, materially constraining detention and use-of-force practices currently justified as ordinary law enforcement.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions drafters needed a formula that would extend minimum humanitarian protections to internal conflicts without treating every act of civil disorder, banditry, or isolated riot as a state's international armed conflict, which sovereign states would never have ratified.
% FOUNDING_PROBLEM_CORROBORATION: States and their military legal advisors attest the threshold remains necessary to prevent CA3 from swallowing ordinary policing. The ICRC's own commentaries, UN human rights treaty bodies, and scholars outside government legal offices (e.g. ICTY Tadić jurisprudence, ICRC 2016 Commentary) attest that the threshold is now routinely manipulated by classification avoidance rather than applied as originally intended — corroboration for the 'shifted function' reading comes from bodies structurally outside the beneficiary set (tribunals, the ICRC as custodian rather than party, independent monitors).
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 by interval end) reflects the growing pattern of states classifying protracted internal violence as 'law enforcement' specifically to avoid CA3 obligations — the threshold decision is not neutral fact-finding but a discretionary act with legal consequences the classifying party controls and benefits from. Suppression (0.79) is high because there is no independent, binding, ex ante forum where an affected combatant or civilian population can contest a state's classification before harm occurs; the mechanism that would check the discretion (international tribunal review) operates only after the fact. Theater ratio (0.42) reflects that legal advisors do apply real doctrinal tests (duration, intensity, organizational command structure) some of the time — the coordination function is not wholly fictional — but a rising share of classification activity serves to insulate operations from CA3 rather than genuinely sort armed conflict from ordinary disorder.
 *
 * PERSPECTIVAL GAP:
 *   From the state/military seat, the threshold is a principled and necessary line preventing IHL from displacing ordinary domestic law over minor disorder — a rope. From the seat of the below-threshold combatant or the chronically-violence-exposed civilian, the same threshold operates as an enforced gate that a more powerful party controls and manipulates to withhold protection — a tangled rope shading toward snare. The engine computes these divergent seat classifications from the same structural data; this story claims tangled_rope because both a genuine coordination function (sorting armed conflict from ordinary disorder) and asymmetric extraction (discretionary non-classification to avoid obligations) are present and actively enforced through state classification authority.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and the militaries and commanders who execute their classification decisions sit at the beneficiary end: they set the threshold test's application and capture the benefit of narrower classification (retained discretion, reduced legal exposure). Irregular combatants below the asserted threshold, civilians in chronically violent but sub-threshold zones, and detainees held under unclassified operations sit at the target end: high suppression, trapped exit, and no voice in the classification decision that determines their legal protection. The ICRC and human rights monitors are excluded rather than coordinated — they see the pattern clearly but cannot bind the classifying state.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing CA3 from swallowing ordinary internal policing — remains partially live (some sub-threshold violence genuinely is ordinary crime, not armed conflict), which is why this is authored 'contested' rather than 'dead.' But the corroboration record (ICTY jurisprudence, ICRC commentary, independent monitors) increasingly documents the threshold being invoked instrumentally to avoid obligations in cases that plausibly do meet the doctrinal test, which is the signature of mandatrophy: a device meant to solve a classification problem being repurposed as a discretion-preservation device for the party that controls the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_classification_authority_locus,
    'Should the intensity/organization threshold determination rest with the classifying state itself, or with an independent body (tribunal, fact-finding commission, ICRC) applying the Tadić-style test ex ante?',
    'Comparative study of cases where post-hoc tribunal review reversed a state''s non-classification, weighted against the harm that occurred during the interim; also track whether any state has accepted binding ex ante third-party classification review.',
    'If authority shifted to an independent body, the beneficiary/victim structure would substantially rebalance — irregular combatants and sub-threshold civilians would gain a contestation forum, moving this reading structurally toward the icrc_customary_reading or even the expansive_human_rights_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_classification_authority_locus, conceptual, 'Whether classification authority belongs with the state or an independent adjudicator — the central point of contest between this reading and its siblings.').

omega_variable(
    genuine_vs_manipulated_threshold_application,
    'In any given non-classification decision, is the state genuinely applying the Tadić intensity/organization test in good faith, or invoking the threshold instrumentally to avoid CA3 obligations it would otherwise incur?',
    'Case-by-case documentary and testimonial review of classification decisions against objective indicators (duration of hostilities, weapons used, territorial control, existence of a command structure) independently assessed by tribunals or fact-finding missions.',
    'A finding of predominantly good-faith application would support a rope/tangled-rope-toward-rope reading; a finding of systematic manipulation would support reclassification toward snare for this reading specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_manipulated_threshold_application, empirical, 'Whether the threshold''s actual application in practice is a neutral sorting device or a discretionary extraction mechanism.').

omega_variable(
    state_centric_reading_naturalization_risk,
    'Does presenting the intensity/organization threshold as the settled, technical, doctrinally required reading of CA3 (rather than one contested reading among three) obscure the sovereignty-preserving function it also performs for classifying states?',
    'Compare treaty drafting history and subsequent state practice against the doctrinal literature''s framing of the threshold as legally required versus policy-preferred.',
    'If the threshold is better understood as one policy-driven reading among viable alternatives rather than doctrinally compelled, its persistence looks more like state-beneficiary capture of interpretive authority than natural legal necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_centric_reading_naturalization_risk, conceptual, 'Whether the state-centric reading''s apparent doctrinal settledness masks a sovereignty-preserving policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_tr_t8, common_article_3_scope__state_centric_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(comm_tr_t16, common_article_3_scope__state_centric_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(comm_tr_t24, common_article_3_scope__state_centric_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(comm_tr_t32, common_article_3_scope__state_centric_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(comm_tr_t40, common_article_3_scope__state_centric_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comm_be_t8, common_article_3_scope__state_centric_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(comm_be_t16, common_article_3_scope__state_centric_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(comm_be_t24, common_article_3_scope__state_centric_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(comm_be_t32, common_article_3_scope__state_centric_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(comm_be_t40, common_article_3_scope__state_centric_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comm_su_t8, common_article_3_scope__state_centric_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(comm_su_t16, common_article_3_scope__state_centric_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(comm_su_t24, common_article_3_scope__state_centric_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(comm_su_t32, common_article_3_scope__state_centric_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(comm_su_t40, common_article_3_scope__state_centric_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language label 'CA3 scope' per the ε-invariance principle. state_centric_reading (this file) authors high suppression and state-favorable directionality; expansive_human_rights_reading authors low suppression and inclusive victim-set coverage; icrc_customary_reading authors an intermediate, practice-tracking ε that shifts with documented state opinio juris. Each carries its own ε, beneficiary/victim structure, and claimed_type; they are linked here and in the sibling files via network.affects_constraints because the state-centric reading's dominance in state legal practice structurally constrains how much room the customary-law reading has to evolve, and directly forecloses the expansive reading's core premise within any single state's operational legal framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
