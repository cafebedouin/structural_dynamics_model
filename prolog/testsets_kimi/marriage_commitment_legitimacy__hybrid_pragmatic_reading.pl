% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Hybrid Pragmatic Reading: Manifesto as Strategic Institutional Adaptation
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto issued by Wilford Woodruff ended plural marriage
 *   practice in the LDS Church under existential federal pressure. The hybrid
 *   pragmatic reading interprets this event not as pure prophetic revelation
 *   (endogenous) nor as pure federal coercion (exogenous), but as strategic
 *   institutional adaptation: prophetic authority deployed to manage an
 *   exogenous crisis while preserving core theological commitments through
 *   deliberate scope ambiguity. The reading treats the Manifesto as
 *   simultaneously sincere and strategic, producing a constraint that
 *   coordinates institutional survival while extracting interpretive labor
 *   from rank-and-file members.
 *
 * KEY AGENTS:
 *   - institutional_leadership (agenda_setter/institutional/arbitrage) â deploys prophetic authority to manage crisis and capture doctrinal flexibility
 *   - rank_and_file_members (payer/powerless/identity_locked) â bear interpretive uncertainty and practice abandonment costs
 *   - doctrinal_hardliners (excluded/organized/trapped) â reject ambiguity and persist in schism or clandestine practice
 *   - federal_authority (observer/institutional/analytical) â applied exogenous pressure and monitors compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.6).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.5).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Hybrid Pragmatic Reading: Manifesto as Strategic Institutional Adaptation").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '4ae70ce2-d949-4b8e-909b-321e5fee09fe').
narrative_ontology:cs_kernel_codification('4ae70ce2-d949-4b8e-909b-321e5fee09fe', fixed_text).
narrative_ontology:cs_authority_grounding('4ae70ce2-d949-4b8e-909b-321e5fee09fe', lineage).
narrative_ontology:cs_interpretation_layer_present('4ae70ce2-d949-4b8e-909b-321e5fee09fe').
narrative_ontology:cs_reading_relation('4ae70ce2-d949-4b8e-909b-321e5fee09fe', marriage_commitment_legitimacy__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('4ae70ce2-d949-4b8e-909b-321e5fee09fe', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_axiom('4ae70ce2-d949-4b8e-909b-321e5fee09fe', foundational, prophetic_pragmatism_legitimizes_scope_ambiguity).
narrative_ontology:cs_axiom_status(prophetic_pragmatism_legitimizes_scope_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('4ae70ce2-d949-4b8e-909b-321e5fee09fe', prophetic_pragmatism_legitimizes_scope_ambiguity, instrumental).
narrative_ontology:cs_axiom('4ae70ce2-d949-4b8e-909b-321e5fee09fe', secondary, institutional_survival_supersedes_doctrinal_clarity).
narrative_ontology:cs_axiom_status(institutional_survival_supersedes_doctrinal_clarity, holdable).
narrative_ontology:cs_axiom_grounding('4ae70ce2-d949-4b8e-909b-321e5fee09fe', institutional_survival_supersedes_doctrinal_clarity, instrumental).
narrative_ontology:cs_reference_frame('4ae70ce2-d949-4b8e-909b-321e5fee09fe', prophetic_pragmatic_adaptation).
narrative_ontology:cs_drift_state('4ae70ce2-d949-4b8e-909b-321e5fee09fe', post_crisis_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ae70ce2-d949-4b8e-909b-321e5fee09fe', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_hardliners).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prophetic_authority_legitimacy).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploys prophetic authority to issue the Manifesto, navigating existential federal pressure while preserving doctrinal claims to plural marriage as an eternal principle. Retains institutional control and doctrinal flexibility by embedding scope ambiguity in the pronouncement, allowing subsequent reinterpretation without formal reversal.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Must abandon plural marriage practice and accept the Manifesto's authority while living with unresolved ambiguity about whether the doctrine remains eternally valid. Bear the psychological, familial, and cosmological cost of interpretive uncertainty without access to theological resolution or institutional voice.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    powerless, biographical, identity_locked, national).

% Reject the Manifesto as either federal capitulation or incomplete revelation; continue plural marriage practice clandestinely or in schismatic communities. Structurally excluded from institutional discourse and subject to excommunication, yet unable to fully exit the theological framework that defines their identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_hardliners, excluded,
    organized, generational, trapped, regional).

% Applied anti-polygamy statutes and disincorporation threats that created the exogenous crisis. Monitors institutional compliance to determine whether enforcement can safely remain suspended, treating the Manifesto as a political instrument rather than a theological event.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves institutional continuity and collective religious identity under existential federal pressure by coordinating a practice shift (ending plural marriage) without requiring a formal doctrinal repudiation, thereby preventing immediate schism.
% TRANSFER_FUNCTION: Moves interpretive labor, legitimacy ambiguity, and practice-abandonment costs from institutional leadership to rank-and-file members, while transferring federal compliance credit and doctrinal flexibility to the institution.
% ABSENT_VOICES: Doctrinal hardliners who regard the Manifesto as capitulation or incomplete revelation; federal authorities who suspect compliance is performative; rank-and-file members without theological training who cannot parse scope ambiguity and were never consulted in its construction.
% DISAPPEARANCE_RATIONALE: If the hybrid pragmatic reading vanished as the operative institutional frame, either the endogenous revelatory reading or the exogenous coercion reading would become dominant, forcing a clearer doctrinal stance; schismatic pressures would intensify; the present equilibrium of ambiguity-managed unity would collapse.
% FOUNDING_PROBLEM: Federal anti-polygamy enforcement in the late nineteenth century threatening institutional destruction through disincorporation, property confiscation, and imprisonment of leadership, while the theological commitment to plural marriage as an eternal principle remained doctrinally fixed.
% FOUNDING_PROBLEM_CORROBORATION: Federal statutes and enforcement records (Edmunds-Tucker Act, etc.) corroborate the existential threat from outside the benefiting parties. Post-manifesto fundamentalist movements and institutional historians attest the crisis passed, while the ambiguity persists as an institutional feature.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.32 to 0.60 over the interval as the initial crisis response calcifies into a permanent institutional ambiguity. Theater ratio climbs from 0.15 to 0.55 as the performance of prophetic continuity and unified compliance increasingly substitutes for genuine doctrinal resolution. Suppression requirement declines from 0.70 to 0.50 as external federal pressure recedes and internalized obedience replaces overt enforcement, though excommunication for polygamists persists. Accessibility collapse is moderate-high (0.65) because theological alternatives (restoration of practice, full repudiation) are cognitively available but socially and cosmologically costly. Resistance is moderate (0.50) because hardliner dissent is persistent but marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership seat, the Manifesto is necessary prophetic pragmatism that saved the Church; from the rank-and-file seat, it is an unresolved theological wound that transfers interpretive labor downward; from the hardliner seat, it is illegitimate institutional capture. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the declared beneficiary with arbitrage-grade exit (can reinterpret, adjust, or issue subsequent clarifications), yielding a low directionality near the beneficiary pole. Rank-and-file members are declared victims with identity_locked exit (embedded in family and eternal cosmology), yielding high directionality near the target pole. Doctrinal hardliners are excluded with trapped exit, sitting at the extreme target end. Federal authority sits outside the constraint's internal directionality as an external observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal extinction) is dead, and the disappearance verdict is world_rearranges, which triggers the mandatrophy mismatch flag. However, the constraint is not a piton because the theater ratio, while rising, does not indicate pure performance, and there remains a concentrated beneficiary (institutional leadership) that actively maintains the ambiguity for ongoing institutional flexibility. The persistence is strategic, not merely inertial; the leadership could theoretically clarify the doctrine but chooses not to because the ambiguity continues to serve extractive-coordinative functions. Thus the mandatrophy signal is an investigative trigger, not an automatic reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_ambiguity_extraction_or_coordination,
    'Does the scope ambiguity in the Manifesto function as a necessary coordination device to preserve community unity, or as an extractive deferral that shifts theological labor to rank-and-file members?',
    'Comparative analysis of member exit rates and schismatic formation in jurisdictions with high versus low ambiguity tolerance; theological surveys of member interpretive burden.',
    'If ambiguity is found to be primarily extractive, the constraint shifts toward snare classification; if genuinely coordinative, it remains tangled rope with lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_ambiguity_extraction_or_coordination, conceptual, 'Whether ambiguity serves coordination or extraction.').

omega_variable(
    kernel_reading_indeterminacy,
    'This constraint is one reading of a contested kernel; would adopting the exogenous_override or endogenous_reinterpretation readings change the beneficiary/victim structure?',
    'Historical analysis of which reading was operative at different periods; correlation with institutional resource allocation and member compliance patterns.',
    'Exogenous_override reading would identify federal_authority as effective agenda-setter and eliminate institutional_leadership as beneficiary; endogenous_reinterpretation would eliminate victims by framing the Manifesto as pure divine command. This reading''s hybrid status depends on maintaining the ambiguity between these poles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Sibling reading structural deltas.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (federal law, excommunication) or internalized (members accepting ambiguity as faithful obedience)?',
    'Post-exit trajectory: if former members continue to feel bound by the Manifesto''s ambiguity after leaving the institution, suppression is partially internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests, raising extraction for identity_locked members.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 135).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcl_hpr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mcl_hpr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(mcl_hpr_tr_t45, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(mcl_hpr_tr_t75, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement(mcl_hpr_tr_t100, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(mcl_hpr_tr_t135, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 135, 0.55).

% Extraction over time
narrative_ontology:measurement(mcl_hpr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mcl_hpr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(mcl_hpr_be_t45, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 45, 0.45).
narrative_ontology:measurement(mcl_hpr_be_t75, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 75, 0.52).
narrative_ontology:measurement(mcl_hpr_be_t100, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 100, 0.56).
narrative_ontology:measurement(mcl_hpr_be_t135, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 135, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(mcl_hpr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(mcl_hpr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(mcl_hpr_su_t45, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(mcl_hpr_su_t75, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement(mcl_hpr_su_t100, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(mcl_hpr_su_t135, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 135, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
