% ============================================================================
% CONSTRAINT STORY: genealogical_origin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genealogical_origin_reading, []).

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
 *   constraint_id: genealogical_origin_reading
 *   human_readable: Genealogical Fixity of Preference Authenticity (Causal-History Reading)
 *   domain: moral_psychology/philosophy_of_autonomy/political_theory
 *
 * SUMMARY:
 *   This constraint concerns the metaphysical claim, prior to any test, that
 *   whether a preference is autonomous or adaptively imposed was settled at
 *   its causal origin and remains a fact of the matter regardless of whether
 *   anyone — including the preference-holder — can ever access it. The claim
 *   underwrites adaptive-preferences critique in feminist and decolonial
 *   theory: it lets critics say a currently-endorsed preference is 'really'
 *   unfree, licensing paternalistic intervention, without needing to win an
 *   argument about the subject's present psychology. The cost falls on those
 *   classified: their sincere present testimony is disqualified by
 *   construction, and the classifying fact is one no party, including the
 *   classifier, can ever verify.
 *
 * KEY AGENTS:
 *   - autonomy_theorists: agenda_setter (institutional/analytical) — administer the genealogical framework
 *   - adaptive_preference_critics: beneficiary (organized/arbitrage) — gain metaphysical cover for critique immune to present-tense rebuttal
 *   - adapted_preference_holders: payer (powerless/identity_locked) — bear an unfalsifiable verdict about their own preferences
 *   - socialized_women_under_patriarchal_norms: payer (powerless/trapped) — the paradigm victim case
 *   - behavioral_scientists_and_therapists: excluded (moderate/constrained) — their entire evidentiary toolkit is declared irrelevant
 *   - philosophers_of_mind_observer: observer (analytical/universal) — traces the metaphysical commitments against sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genealogical_origin_reading, 0.42).
domain_priors:suppression_score(genealogical_origin_reading, 0.58).
domain_priors:theater_ratio(genealogical_origin_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genealogical_origin_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(genealogical_origin_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(genealogical_origin_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genealogical_origin_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(genealogical_origin_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genealogical_origin_reading, tangled_rope).
narrative_ontology:human_readable(genealogical_origin_reading, "Genealogical Fixity of Preference Authenticity (Causal-History Reading)").
narrative_ontology:topic_domain(genealogical_origin_reading, "moral_psychology/philosophy_of_autonomy/political_theory").

domain_priors:requires_active_enforcement(genealogical_origin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genealogical_origin_reading, '66494e5f-8773-494e-84e5-71264a05f32e').
narrative_ontology:cs_kernel_codification('66494e5f-8773-494e-84e5-71264a05f32e', distributed).
narrative_ontology:cs_authority_grounding('66494e5f-8773-494e-84e5-71264a05f32e', expertise).
narrative_ontology:cs_interpretation_layer_present('66494e5f-8773-494e-84e5-71264a05f32e').
narrative_ontology:cs_reading_relation('66494e5f-8773-494e-84e5-71264a05f32e', authentic_preference_boundary__behaviorist_counterfactual_reading, forecloses).
narrative_ontology:cs_reading_relation('66494e5f-8773-494e-84e5-71264a05f32e', authentic_preference_boundary__phenomenological_endorsement_reading, coexists_with).
narrative_ontology:cs_reading_relation('66494e5f-8773-494e-84e5-71264a05f32e', authentic_preference_boundary__capability_traction_reading, influences).
narrative_ontology:cs_axiom('66494e5f-8773-494e-84e5-71264a05f32e', foundational, determinacy_without_recoverability).
narrative_ontology:cs_axiom_status(determinacy_without_recoverability, holdable).
narrative_ontology:cs_axiom_grounding('66494e5f-8773-494e-84e5-71264a05f32e', determinacy_without_recoverability, deontological).
narrative_ontology:cs_axiom('66494e5f-8773-494e-84e5-71264a05f32e', secondary, present_endorsement_evidentially_inert_against_origin_fact).
narrative_ontology:cs_axiom_status(present_endorsement_evidentially_inert_against_origin_fact, holdable).
narrative_ontology:cs_axiom_grounding('66494e5f-8773-494e-84e5-71264a05f32e', present_endorsement_evidentially_inert_against_origin_fact, conventional).
narrative_ontology:cs_reference_frame('66494e5f-8773-494e-84e5-71264a05f32e', formation_moment_determinacy).
narrative_ontology:cs_drift_state('66494e5f-8773-494e-84e5-71264a05f32e', contemporary_adaptive_preference_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('66494e5f-8773-494e-84e5-71264a05f32e', '').
narrative_ontology:cs_kernel_id(genealogical_origin_reading, authentic_preference_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genealogical_origin_reading, autonomy_theorists).
narrative_ontology:constraint_beneficiary(genealogical_origin_reading, adaptive_preference_critics).
narrative_ontology:constraint_beneficiary(genealogical_origin_reading, liberatory_movements).
narrative_ontology:constraint_victim(genealogical_origin_reading, adapted_preference_holders).
narrative_ontology:constraint_victim(genealogical_origin_reading, socialized_women_under_patriarchal_norms).
narrative_ontology:constraint_victim(genealogical_origin_reading, formerly_colonized_subjects_with_internalized_norms).
narrative_ontology:constraint_vindicates(genealogical_origin_reading, determinacy_of_historical_fact_independent_of_epistemic_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and defend the theoretical apparatus that locates authenticity in causal history at formation. They administer the framework used in bioethics, political philosophy, and feminist theory to adjudicate which preferences count as genuinely the agent's own versus products of oppression. They do not bear the classificatory verdict themselves; they set the criteria others are measured against.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, autonomy_theorists, agenda_setter,
    institutional, civilizational, analytical, global).

% Feminist and liberation theorists who use the genealogical reading to name adaptive preferences (the 'happy slave,' the 'contented housewife') as inauthentic regardless of the subject's present endorsement. The reading gives their critique metaphysical backing — it lets them say the preference is unfree even when the person insists otherwise, without needing an accessible test to prove it.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, adaptive_preference_critics, beneficiary,
    organized, generational, arbitrage, global).

% Political movements (labor, feminist, decolonial) that invoke the genealogical fact of coerced formation to justify intervention against preferences the affected parties currently affirm. The framework licenses paternalistic policy on the ground that the subject's stated preference was formed under constraint, not on the ground that it currently fails some accessible test.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, liberatory_movements, beneficiary,
    organized, generational, mobile, national).

% Individuals whose preferences formed under constraint and who now affirm those preferences reflectively. Under this reading, their self-report and present psychological integration carry no evidential weight against the fixed historical fact of origin — they can be told, correctly by the framework's own lights, that their preference is inauthentic even though they can never themselves verify or contest this, and neither can anyone else. The verdict is unfalsifiable from their position and from every other position.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, adapted_preference_holders, payer,
    powerless, biographical, identity_locked, local).

% The paradigm case: a woman whose preference for domestic subordination formed inside a patriarchal socialization process. The genealogical reading treats her authentic/adapted status as a determinate fact fixed at some historical moment of formation, permanently unknowable to her, to observers, and to any future test — she is not merely un-testable, she is a fact no party can ever reach, and yet the fact is treated as real and as licensing others to act on her behalf.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, socialized_women_under_patriarchal_norms, payer,
    powerless, biographical, trapped, local).

% Populations whose values and preferences were shaped under colonial administration and internalized across generations. The genealogical reading assigns them a fixed historical authenticity status at the moment those values first formed — a moment often lost to record, memory, and even living witness — and treats present cultural continuity or repudiation as irrelevant evidence.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, formerly_colonized_subjects_with_internalized_norms, payer,
    powerless, generational, identity_locked, national).

% Clinicians and researchers who work only with present dispositions, counterfactual responses, and observable behavior. They would object that a fact permanently outside all possible evidence is not a fact any practice can use, and that their entire diagnostic toolkit is declared irrelevant to what 'really' happened by a framework that offers them nothing to test. They are not consulted in the philosophical adjudication of the boundary.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, behavioral_scientists_and_therapists, excluded,
    moderate, biographical, constrained, national).

% The people whose preferences are being classified have no standing in the classification — their present testimony, however sincere and reflective, is by construction incapable of settling the question the framework claims to answer about them. They are the subject of the fact and simultaneously the party least able to speak to it.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, the_subjects_themselves, excluded,
    powerless, biographical, trapped, local).

% Analyze the metaphysical commitments of the genealogical reading against its siblings, tracing what follows if determinate-but-irrecoverable facts are admitted into a theory whose entire practical payoff is licensing intervention on people's stated preferences.
narrative_ontology:constraint_stakeholder(genealogical_origin_reading, philosophers_of_mind_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genealogical_origin_reading, diffuse).
narrative_ontology:fixing_cost_class(genealogical_origin_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, principled way to distinguish preferences worth honoring from preferences that are products of oppression, coordinating critique and policy intervention around a single fact (causal history) rather than shifting present-tense proxies.
% TRANSFER_FUNCTION: Moves interpretive and political authority from the preference-holder (whose present endorsement is disqualified as evidence) to the theorist or movement that asserts knowledge of the formation history — enabling paternalistic intervention, reeducation, or policy override justified by a fact the holder cannot contest because no one can access it.
% ABSENT_VOICES: The subjects themselves and the clinicians who work only with observable, testable dispositions are structurally absent from the adjudication — the framework's central claim is precisely that their testimony and tests cannot settle the matter, which forecloses their standing by design rather than by oversight.
% DISAPPEARANCE_RATIONALE: If the genealogical-fixity claim were abandoned, adaptive-preference critique would lose its strongest metaphysical lever (the fact-no-one-can-reach argument) and would have to fall back to present-tense tests the behaviorist reading already offers; theorists and movements dispute whether this would weaken or merely relocate their critique, while those classified under the framework would, by definition, be unaffected in any way they could report.
% FOUNDING_PROBLEM: Built to answer the adaptive-preferences problem in autonomy theory: how can a preference be unfree even when the person holds it reflectively, sincerely, and without any felt coercion in the present? The genealogical move locates the answer in the past, not the present psychology.
% FOUNDING_PROBLEM_CORROBORATION: Autonomy theorists and liberatory movements (the framework's primary users) attest the problem remains live and requires exactly this metaphysical commitment. Outside corroboration is thin: philosophers of mind sympathetic to verificationism argue the problem is real but the genealogical answer manufactures an unfalsifiable fact rather than solving the practical problem; no adapted-preference holder has ever been able to corroborate or dispute the verdict rendered about them, which the framework itself predicts and treats as expected rather than as a defect.
narrative_ontology:disappearance_verdict(genealogical_origin_reading, contested).
narrative_ontology:founding_problem_status(genealogical_origin_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genealogical_origin_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(genealogical_origin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(genealogical_origin_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genealogical_origin_reading_tests).
:- end_tests(genealogical_origin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate: the framework does not extract material resources directly, but it extracts interpretive authority — the right to be believed about one's own preferences — from the classified party and transfers it to the theorist or movement invoking the genealogical fact. Suppression (0.58) is substantial because the mechanism by which the fact is enforced (disqualifying present testimony as evidence) is a structural, not incidental, feature: it is built into what the reading claims authenticity consists in. Theater ratio (0.31) is moderate-low: the genealogical inquiry does real philosophical work distinguishing formation histories, but an increasing share of its practical use is invoking the unfalsifiable-fact move to settle contested cases without argument, which drifts toward performative deployment over time — hence the rising trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the reading solves a genuine philosophical puzzle and imposes no cost on anyone — theorists bear none of the classificatory weight themselves. From the payer seat, the same structure is experienced as being told, unfalsifiably and permanently, that one's own reflective endorsement of one's life does not count as evidence about one's own life. The engine's per-seat computation should register this asymmetry directly from the declared power/exit data, not from any claim this commentary makes about which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomy theorists and liberatory movements sit near the beneficiary end: they gain a critique that cannot be defeated by the subject's own testimony, and they hold institutional or organized power with mobile or arbitrage exit — they are never themselves the object of classification. Adapted preference holders, socialized women, and formerly colonized subjects sit near the full-target end: they are powerless, their exit options are trapped or identity-locked (the preference in question is often constitutive of their self-concept, making 'exit' from the classification incoherent even in principle), and the constraint's core claim is specifically that their perspective cannot bear on their own case. Behavioral scientists are excluded rather than coordinated — their exclusion is structural to the reading's central metaphysical move, not an oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accounting for unfree-feeling-free preferences) remains philosophically live, which prevents outright dismissal as pure mandatrophy — the tangled_rope classification, not snare, reflects that a genuine coordination function (naming adaptive preferences as a real phenomenon) persists alongside the extraction (disqualifying the subject's own voice with an unfalsifiable historical fact). Where the framework is deployed to license concrete paternalistic policy against the stated wishes of the classified party, without independent corroboration of the formation history, the tangled coordination shades toward a captured critique — a distinct empirical question this story's omega on evidentiary practice is meant to track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irrecoverable_fact_vs_no_fact,
    'Is there really a determinate fact about causal formation that is permanently unknowable, or does ''permanently unknowable'' entail that there is no further fact to be right or wrong about (the behaviorist reading''s position)?',
    'This is not resolvable by any conceivable empirical test by the framework''s own construction — it is a conceptual dispute about whether determinacy requires even principled recoverability. Philosophical argument about the semantics of ''fact'' and the metaphysics of unwitnessed history is the only available resolution mechanism, and it may not converge.',
    'If no further fact exists once recovery is impossible, this entire reading collapses into the behaviorist_counterfactual_reading and the tangled_rope classification given here dissolves into whatever the counterfactual reading computes. If a determinate irrecoverable fact is genuinely coherent, the reading stands as its own constraint with its own victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irrecoverable_fact_vs_no_fact, conceptual, 'Whether determinate-but-permanently-unknowable facts are coherent, or whether unknowability collapses into non-existence of the fact.').

omega_variable(
    corroboration_standard_for_formation_claims,
    'When a critic invokes the genealogical fact to override a subject''s present endorsement, what independent evidence (historical record, third-party testimony, documented coercion) is actually required, versus how often is the claim asserted from theoretical necessity alone?',
    'Audit of adaptive-preference critique literature and policy interventions for whether formation-history claims are backed by independent historical corroboration or asserted purely from the present pattern of preference (i.e., inferring the coercive origin FROM the fact that the preference looks adaptive, which is circular).',
    'If formation claims are typically corroborated independently, the reading functions closer to genuine coordination (naming real, evidenced histories). If formation claims are typically inferred backward from the preference''s present shape, the ''genealogical fact'' is functioning as an unfalsifiable rhetorical device and the extraction score understates the actual capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corroboration_standard_for_formation_claims, empirical, 'Whether genealogical claims in practice rest on independent historical evidence or are inferred circularly from the preference being critiqued.').

omega_variable(
    identity_lock_of_the_classified,
    'For subjects whose classified preference is constitutive of their ongoing self-concept (e.g., a religious or domestic identity formed under constraint but now load-bearing for their sense of self), does ''exit'' from the classification even make sense as a concept, or does the framework impose a psychological cost with no corresponding exit path in principle?',
    'Longitudinal interviews with subjects classified as holding adaptive preferences, tracking whether the classification itself (independent of any material intervention) produces measurable psychological harm or alienation from self-narrative.',
    'If identity-lock is total and irreversible, the payer seats'' exit_options of identity_locked/trapped are structurally accurate and the effective extraction the engine computes for them should sit near the ceiling regardless of scope; if some subjects report the classification as liberating rather than harmful, the victim framing requires qualification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_the_classified, empirical, 'Whether the classified subjects experience the unfalsifiable verdict as harm, and whether exit from the classification is conceptually available at all.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genealogical_origin_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genealogical_origin_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gene_tr_t10, genealogical_origin_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(gene_tr_t20, genealogical_origin_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(gene_tr_t30, genealogical_origin_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(gene_tr_t40, genealogical_origin_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(gene_tr_t50, genealogical_origin_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(gene_tr_t60, genealogical_origin_reading, theater_ratio, 60, 0.31).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genealogical_origin_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gene_be_t10, genealogical_origin_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(gene_be_t20, genealogical_origin_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(gene_be_t30, genealogical_origin_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(gene_be_t40, genealogical_origin_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(gene_be_t50, genealogical_origin_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(gene_be_t60, genealogical_origin_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genealogical_origin_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gene_su_t10, genealogical_origin_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(gene_su_t20, genealogical_origin_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(gene_su_t30, genealogical_origin_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(gene_su_t40, genealogical_origin_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(gene_su_t50, genealogical_origin_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(gene_su_t60, genealogical_origin_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genealogical_origin_reading, identity_coordination).
narrative_ontology:affects_constraint(genealogical_origin_reading, behaviorist_counterfactual_reading).
narrative_ontology:affects_constraint(genealogical_origin_reading, phenomenological_endorsement_reading).
narrative_ontology:affects_constraint(genealogical_origin_reading, capability_traction_reading).

% DUAL FORMULATION NOTE:
% This is one of four sibling constraints decomposing the natural-language 'authentic preference boundary' kernel. Each reading locates authenticity in a different structural feature — causal history (this story), counterfactual/behavioral testability (behaviorist_counterfactual_reading), present phenomenological endorsement (phenomenological_endorsement_reading), or current capability to revise the preference (capability_traction_reading) — and each produces a different victim set and different epsilon. They are linked here per the ε-invariance principle rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
