% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy via Structural Injustice Threshold
 *   domain: political/economic/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the grievance_threshold_reading of the
 *   secession_legitimacy_boundary kernel. It posits that secession becomes
 *   legitimate when federal actions cross a threshold of structural
 *   injustice, regardless of constitutional text. The reading displaces
 *   constitutional supremacy with an objective justice standard, making
 *   legitimacy conditional on demonstrable federal overreach. Key agents
 *   include secessionist movements that gain a conditional path to
 *   recognition, federal governments that lose the absolute shield of
 *   constitutional text, constitutional interpreters who are structurally
 *   bypassed, and international recognition bodies that adjudicate threshold
 *   claims. Sibling readingsâconstitutional_impossibility,
 *   popular_sovereignty, and treaty_primacyâoccupy the same kernel but emit
 *   structurally distinct constraints.
 *
 * KEY AGENTS:
 *   - Secessionist movements: beneficiary (moderate/constrained) â gain conditional legitimacy path
 *   - Federal government: payer (institutional/constrained) â bears conditional legitimacy loss and territorial risk
 *   - Constitutional interpreters: excluded (organized/analytical) â interpretive authority bypassed by text-independent standard
 *   - International recognition regime: observer (institutional/analytical) â applies threshold inconsistently
 *   - Minority groups in seceding region: excluded (powerless/trapped) â affected but unrepresented in threshold calculus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.62).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.58).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy via Structural Injustice Threshold").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political/economic/federalism").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'cd637628-7e84-4702-b729-b0ae54a32632').
narrative_ontology:cs_kernel_codification('cd637628-7e84-4702-b729-b0ae54a32632', formalized).
narrative_ontology:cs_authority_grounding('cd637628-7e84-4702-b729-b0ae54a32632', diffuse_epistemic).
narrative_ontology:cs_reading_relation('cd637628-7e84-4702-b729-b0ae54a32632', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('cd637628-7e84-4702-b729-b0ae54a32632', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd637628-7e84-4702-b729-b0ae54a32632', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('cd637628-7e84-4702-b729-b0ae54a32632', foundational, legitimacy_conditional_on_demonstrable_overreach).
narrative_ontology:cs_axiom_status(legitimacy_conditional_on_demonstrable_overreach, holdable).
narrative_ontology:cs_axiom_grounding('cd637628-7e84-4702-b729-b0ae54a32632', legitimacy_conditional_on_demonstrable_overreach, empirically_contingent).
narrative_ontology:cs_axiom('cd637628-7e84-4702-b729-b0ae54a32632', foundational, constitutional_text_nonabsolute_under_injustice).
narrative_ontology:cs_axiom_status(constitutional_text_nonabsolute_under_injustice, holdable).
narrative_ontology:cs_axiom_grounding('cd637628-7e84-4702-b729-b0ae54a32632', constitutional_text_nonabsolute_under_injustice, deontological).
narrative_ontology:cs_reference_frame('cd637628-7e84-4702-b729-b0ae54a32632', remedial_secession_standard).
narrative_ontology:cs_drift_state('cd637628-7e84-4702-b729-b0ae54a32632', contemporary_great_power_abuse_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd637628-7e84-4702-b729-b0ae54a32632', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, secessionist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the structural injustice threshold to claim international legitimacy for independence. Must assemble objective evidence of federal overreachâhuman rights reports, economic discrimination data, political exclusion metricsâto meet the burden of proof. Without this reading, their claims rest solely on constitutional text (which usually prohibits unilateral secession) or raw majoritarian will (which lacks international traction).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, secessionist_movements, beneficiary,
    moderate, biographical, constrained, regional).

% Loses the absolute protection of constitutional supremacy over its territory. Faces a conditional threat to territorial integrity if its policies are demonstrated to cross the structural injustice threshold. Cannot simply dismiss secessionist claims by pointing to constitutional text; must instead contest the empirical evidence of overreach or risk legitimacy erosion.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Textualist and originalist jurists whose authority derives from constitutional interpretation are structurally bypassed by a reading that explicitly declares legitimacy regardless of constitutional text. Their interpretive frameworks, institutional roles, and professional identities are marginalized when secession claims are adjudicated through human rights metrics rather than constitutional procedure.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_interpreters, excluded,
    organized, generational, analytical, national).

% Acts as the de facto adjudicator of whether the threshold has been met. Grants or withholds state recognition based on assessments of federal overreach, setting precedents that shape subsequent secessionist strategies. Does not create the standard but applies it, often inconsistently, in response to great-power politics and human rights documentation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_recognition_regime, observer,
    institutional, generational, analytical, global).

% Would be directly affected by border changes and new citizenship regimes but are not party to the threshold calculus. Their future statusâwhether they become minorities in a new state or lose cross-border tiesâdepends on a legitimacy standard that does not formally require their consent or input.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, minority_groups_in_seceding_region, excluded,
    powerless, biographical, trapped, local).

% Produce the evidentiary basisâreports on discrimination, political exclusion, economic marginalizationâthat secessionist movements use to prove the threshold. Their findings become inputs to the legitimacy determination, though they do not control how states or international bodies interpret the threshold.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, human_rights_documentation_bodies, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, secessionist_movements).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled standard for distinguishing legitimate remedial secession from illegitimate territorial dissolution, coordinating international recognition and domestic political expectations around an objective justice threshold rather than arbitrary will or absolute constitutional prohibition.
% TRANSFER_FUNCTION: Moves legitimacy and territorial authority from the federal government to the secessionist region conditional on demonstrated structural injustice; moves the burden of proof onto secessionist claimants to objectively establish federal overreach.
% ABSENT_VOICES: Minority groups within the seceding region who would lose federal minority protections; future generations affected by border changes; constitutional textualists whose interpretive authority is bypassed by the regardless of constitutional text formulation.
% DISAPPEARANCE_RATIONALE: If the grievance threshold reading vanished, secessionist movements would lose their primary non-constitutional legitimacy framework; federal governments would regain the shield of constitutional absolutism; international recognition bodies would lose a standard for evaluating secession claimsâthe landscape of territorial legitimacy would reorganize around either pure constitutionalism or pure popular sovereignty.
% FOUNDING_PROBLEM: How to prevent both oppressive federal retention of territories that suffer structural injustice and chaotic fragmentation from unconditional secession.
% FOUNDING_PROBLEM_CORROBORATION: Human rights scholars and some international jurists attest the problem is live. Federal governments and constitutional scholars attest the problem is already solved by constitutional amendment procedures and that the grievance threshold creates more problems than it solves. Independent comparative federalism studies from outside both camps document mixed evidence.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the constraint reallocates territorial legitimacy and authority from federal governments to secessionist regions on a conditional basis. Suppression (0.58) is moderate: the reading suppresses unconditional secession (by requiring threshold proof) and suppresses constitutional absolutism (by declaring text irrelevant), while minority voices in seceding regions are excluded from the calculus. Theater ratio (0.40) reflects the increasing performativity of threshold debates in international forums, where states invoke structural injustice rhetorically without meeting genuine evidentiary standards. Accessibility collapse (0.48) is moderate because alternativesâconstitutional amendment, pure popular sovereignty, and treaty primacyâremain visible and actively defended. Resistance (0.75) is high because federal governments and constitutional scholars vigorously oppose any reading that bypasses constitutional text and threatens territorial integrity. The measurement series share one time grid to prevent temporal misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The secessionist seat experiences the constraint as a necessary shield against oppressive federal retention; the federal seat experiences it as an existential threat to territorial integrity and constitutional order; the constitutional interpreter seat experiences it as lawless bypassing of textual authority. The engine computes these divergences from the structural data rather than adjudicating which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Secessionist movements are structurally beneficiaries: they gain a conditional legitimacy path that does not depend on constitutional permission, though they must bear the burden of proof (exit: constrained, which damps their d toward the beneficiary end but not fully). The federal government is the structural victim: it loses the absolute protection of constitutional supremacy and faces a standing conditional threat to territorial integrity (exit: constrained, d near the target end). Constitutional interpreters are excluded and analytically positioned; their directionality reverts to the power atom fallback. Minority groups in the seceding region are excluded and trapped, sitting near the full-target end because the constraint operates on them without their input.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled rope prevents mislabeling the constraint as pure coordination (rope)âit is not merely a harmless standard because it actively reallocates legitimacy and territorial authority asymmetrically. It also prevents mislabeling as pure extraction (snare)âthe coordination function is genuine and active, solving the real problem of distinguishing oppressive unity from justified exit. It is not a mountain because it is constructed, contested, and beneficiary-dependent. It is not a piton because the coordination function has not atrophied; debates about thresholds are performative in part but the underlying justice standard remains operationally active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objective_threshold_demonstrability,
    'Can structural injustice be objectively demonstrated to a neutral standard, or is the threshold inherently political and contested?',
    'Comparative case analysis of secession claims adjudicated by international bodies and independent human rights monitors, measuring inter-rater reliability of threshold determinations.',
    'If the threshold cannot be objectively determined, the constraint''s empirical grounding collapses and its extraction profile rises toward snare (coordination story becomes cover for power politics). If objective determination is possible, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_threshold_demonstrability, empirical, 'Whether the structural injustice threshold is objectively measurable or inherently political.').

omega_variable(
    remedial_doctrine_abuse,
    'Is the grievance threshold reading primarily invoked to justify genuine remedial secession, or has it been captured by opportunistic territorial grabs?',
    'Historical inventory of secessionist invocations post-1990, coding each claim against independent human rights assessments of the invoking region''s actual grievances.',
    'If captured by opportunism, the constraint''s theater_ratio and extractiveness are higher than the coordination story suggests; international recognition bodies may need to tighten evidentiary standards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_doctrine_abuse, empirical, 'Whether the reading is used for genuine remedial secession or opportunistic territorial grabs.').

omega_variable(
    cs_framing_under_determination,
    'Is the authority structure best framed as diffuse_epistemic (spread across academic and legal discourse) or as practice-based (grounded in repeated state recognition behavior)?',
    'Citation and precedent analysis: measure whether international recognition decisions track scholarly human rights frameworks (diffuse_epistemic) or prior state practice and power politics (practice).',
    'If practice-based, the reference frame drifts toward state-interest capture and the reading''s normative force weakens. If diffuse_epistemic, the reading maintains autonomy from state practice but risks irrelevance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Whether the reading''s authority derives from scholarly expertise or state recognition practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sece_tr_t6, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(sece_tr_t18, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sece_be_t6, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(sece_be_t18, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sece_su_t6, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(sece_su_t18, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The secession_legitimacy_boundary kernel decomposes into four structurally distinct constraints because the colloquial label 'when is secession legitimate' conflates multiple incompatible standards: constitutional text, justice threshold, democratic will, and treaty rights. Each reading has a different epsilon, beneficiary/victim structure, and empirical status. This reading isolates the justice-threshold claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
