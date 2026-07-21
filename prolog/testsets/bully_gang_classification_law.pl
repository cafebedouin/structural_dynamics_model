% ============================================================================
% CONSTRAINT STORY: bully_gang_classification_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bully_gang_classification_law, []).

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
 *   constraint_id: bully_gang_classification_law
 *   human_readable: Post-Hoc Target-Selection/Identity-Cost Categorization Defending the Boundedness-Pricing Law
 *   domain: social_epistemology/signaling_theory/conflict_economics
 *
 * SUMMARY:
 *   The boundedness-pricing law claims that no agent purchases unbounded
 *   individual risk at full marginal price — every apparent instance of
 *   someone escalating a feud into open-ended stakes turns out, on
 *   inspection, to be either a subsidized target selection (the escalator
 *   picked an opponent known in advance not to test the claim) or a pre-paid
 *   identity cost (the reputational price was sunk earlier, at initiation,
 *   not being spent fresh in the case at hand). This story is not about the
 *   horizon mechanism that makes bounded pricing rational in the first place
 *   — that is a separate constraint. This story is about the *discipline*
 *   that decides, case by case, whether an apparent counterexample gets
 *   absorbed into one of the two saving categories or stands as a live
 *   falsification. When that discipline operates by criteria fixed only after
 *   the triggering case appears, the law's universal claim is rescued at the
 *   cost of its own testability: it becomes very hard to specify, in advance,
 *   what a genuine counterexample would even look like.
 *
 * KEY AGENTS:
 *   - theory_defender_using_post_hoc_subsidy_pre_payment_categorization: sets and applies the rescue categories after outcomes are known
 *   - falsifiability_of_the_law_itself: the property being spent to keep the law's apparent universality intact
 *   - field_researchers_applying_the_law: want predictive, ex-ante criteria and don't get them
 *   - genuine_counterexample_agents: real test cases with no standing in their own adjudication
 *   - rival_theorists_of_feud_persistence: excluded from proposing competing sorting rules
 *   - methodological_observers: the seat positioned to actually check ex-ante vs. ex-post application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bully_gang_classification_law, 0.68).
domain_priors:suppression_score(bully_gang_classification_law, 0.58).
domain_priors:theater_ratio(bully_gang_classification_law, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bully_gang_classification_law, extractiveness, 0.68).
narrative_ontology:constraint_metric(bully_gang_classification_law, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bully_gang_classification_law, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bully_gang_classification_law, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(bully_gang_classification_law, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bully_gang_classification_law, snare).
narrative_ontology:human_readable(bully_gang_classification_law, "Post-Hoc Target-Selection/Identity-Cost Categorization Defending the Boundedness-Pricing Law").
narrative_ontology:topic_domain(bully_gang_classification_law, "social_epistemology/signaling_theory/conflict_economics").

domain_priors:requires_active_enforcement(bully_gang_classification_law).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bully_gang_classification_law, 'f15668ad-8a96-4b4c-a6a4-03d89cf161bf').
narrative_ontology:cs_kernel_codification('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', distributed).
narrative_ontology:cs_authority_grounding('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', distributed).
narrative_ontology:cs_reading_relation('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', bully_gang_classification_law__stance_reading, coexists_with).
narrative_ontology:cs_reading_relation('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', bully_gang_classification_law__register_reading, coexists_with).
narrative_ontology:cs_reading_relation('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', bully_gang_classification_law__drift_reading, coexists_with).
narrative_ontology:cs_reading_relation('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', bully_gang_classification_law__impression_management_reading, influences).
narrative_ontology:cs_axiom('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', foundational, truth_indifference_is_instrumental_to_sorting).
narrative_ontology:cs_axiom_status(truth_indifference_is_instrumental_to_sorting, holdable).
narrative_ontology:cs_axiom_grounding('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', truth_indifference_is_instrumental_to_sorting, instrumental).
narrative_ontology:cs_axiom('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', secondary, escalation_under_challenge_is_the_mechanisms_success_condition).
narrative_ontology:cs_axiom_status(escalation_under_challenge_is_the_mechanisms_success_condition, holdable).
narrative_ontology:cs_axiom_grounding('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', escalation_under_challenge_is_the_mechanisms_success_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', unaudited_categorization_discipline).
narrative_ontology:cs_drift_state('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', post_documented_rescue_pattern, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f15668ad-8a96-4b4c-a6a4-03d89cf161bf', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bully_gang_classification_law, theory_defender_using_post_hoc_subsidy_pre_payment_categorization).
narrative_ontology:constraint_victim(bully_gang_classification_law, falsifiability_of_the_law_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bully_gang_classification_law, field_researchers_applying_the_law).
narrative_ontology:constraint_vindicates(bully_gang_classification_law, boundedness_pricing_universal_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the theoretical apparatus that classifies any apparent counterexample to the boundedness-pricing law (an agent who repeatedly chooses open-ended feuds over comparable bounded settlements) as either a subsidized target selection or a pre-paid identity cost. Controls when a case counts as 'ex-ante documented' versus merely 'apparent.' Because the categorization is applied after observing the outcome that would otherwise falsify the law, the defender never has to concede a counterexample — every case can, in principle, be re-sorted into one of the two saving categories. This position costs nothing to occupy and is never itself tested against an independent criterion set in advance.
narrative_ontology:constraint_stakeholder(bully_gang_classification_law, theory_defender_using_post_hoc_subsidy_pre_payment_categorization, agenda_setter,
    analytical, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(bully_gang_classification_law, theory_defender_using_post_hoc_subsidy_pre_payment_categorization, beneficiary).

% The empirical content of the boundedness-pricing claim ('no one purchases unbounded individual risk at full marginal price') is what erodes each time a counterexample is absorbed by a post-hoc category rather than tested against ex-ante criteria. It has no voice, no advocate with standing independent of the theory's own defenders, and no exit — it either remains falsifiable or it doesn't, and the categorization discipline is the only thing that can protect or destroy that property. It cannot renegotiate the terms under which it is tested.
narrative_ontology:constraint_stakeholder(bully_gang_classification_law, falsifiability_of_the_law_itself, payer,
    analytical, civilizational, trapped, universal).

% Analysts and practitioners who want to use the boundedness-pricing law predictively — to identify, before an outcome occurs, whether an agent's open-ended feud-seeking reflects a genuine unbounded-risk purchase (a true counterexample) or a subsidized/pre-paid case. They bear the cost of a discipline that only supplies its categorization criteria after the fact, which means the law offers them no advance test they can rely on. Their exit is constrained: they can abandon the law's predictive use, but the field lacks an obvious rival account of feud persistence with comparable scope.
narrative_ontology:constraint_stakeholder(bully_gang_classification_law, field_researchers_applying_the_law, payer,
    moderate, biographical, constrained, global).

% Real agents who, if they exist, would demonstrate that some party does purchase unbounded individual risk at full marginal price without subsidy or pre-payment — the actual test case the law claims to survive. Whether any given such agent is recognized as a genuine counterexample or reclassified into a saving category is decided entirely by the theory's defenders; the agents themselves have no standing in that adjudication and their behavior is read retrospectively into whichever bucket keeps the law intact.
narrative_ontology:constraint_stakeholder(bully_gang_classification_law, genuine_counterexample_agents, excluded,
    powerless, biographical, trapped, local).

% Would propose alternative explanations for open-ended feud behavior (status economics, honor culture, pathological risk preference) that do not require the subsidy/pre-payment rescue apparatus. They are structurally excluded from adjudicating disputed cases because the categorization discipline that decides whether a case is a counterexample or a saved instance is controlled entirely by the law's own defenders, not by an independent tribunal that would hear rival accounts.
narrative_ontology:constraint_stakeholder(bully_gang_classification_law, rival_theorists_of_feud_persistence, excluded,
    moderate, generational, constrained, global).

% Philosophers of science and methodologists who can assess whether the target-selection/identity-cost categorization is applied ex ante (using criteria fixed before any case is examined) or ex post (fitted to each case after its outcome is known). Their assessment is what would actually resolve whether the law remains falsifiable or has been rescued into unfalsifiability.
narrative_ontology:constraint_stakeholder(bully_gang_classification_law, methodological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bully_gang_classification_law, theory_defender_using_post_hoc_subsidy_pre_payment_categorization).
narrative_ontology:fixing_cost_class(bully_gang_classification_law, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The categorization scheme genuinely solves a real classification problem: distinguishing agents whose apparent unbounded risk-taking is explained by an outside subsidy (someone else absorbs the tail risk) or an already-sunk identity cost (the price was paid upstream, at initiation, not at the moment of the current feud) from agents who are truly purchasing unbounded risk fresh, at full marginal price, in the case at hand. This distinction, applied prospectively, would meaningfully sharpen the boundedness-pricing law's predictive content.
% TRANSFER_FUNCTION: What moves is not money but evidentiary standing: every disputed case's status as 'counterexample' or 'consistent instance' is transferred from an independent, pre-specified test to the discretion of whoever is defending the law. The falsifiability of the claim is what is spent to purchase the appearance of the law's continued survival.
% ABSENT_VOICES: Genuine counterexample agents have no forum in which their case is adjudicated by criteria fixed before the fact; rival theorists of feud persistence are never given standing to propose an alternative sorting rule. Both would object that the categorization discipline as practiced is unfalsifiable in practice even if falsifiable in principle.
% DISAPPEARANCE_RATIONALE: If the post-hoc categorization discipline were withdrawn and replaced with an ex-ante criterion set (pre-registered target-selection patterns and pre-registered identity-cost documentation, fixed before any given case is examined), some fraction of currently 'saved' cases would become live counterexamples. The boundedness-pricing law's status would shift from apparently universal to empirically contested, and researchers who currently treat it as settled would need to re-open the question of what a self-assertive violence claim actually prices.
% FOUNDING_PROBLEM: The boundedness-pricing law was built to explain why almost no observed feuds escalate to truly unbounded stakes: most apparent counterexamples turn out, on inspection, to involve a subsidized target (someone chosen because they were known not to test the claim) or a pre-paid identity cost (a reputational stake already sunk before the current conflict, not being purchased fresh). The founding problem was distinguishing real tests of the law from apparent ones.
% FOUNDING_PROBLEM_CORROBORATION: The theory's defenders attest the categorization discipline is applied consistently and ex ante. Methodological observers and rival theorists of feud persistence — outside the beneficiary set — attest that in practice the subsidy/pre-payment criteria are typically articulated only after a candidate counterexample surfaces, which is the behavior the falsifiability concern is about; no documented case exists of the criteria being published and fixed before the triggering case was known.
narrative_ontology:disappearance_verdict(bully_gang_classification_law, world_rearranges).
narrative_ontology:founding_problem_status(bully_gang_classification_law, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bully_gang_classification_law, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(bully_gang_classification_law, 'none', 1).
narrative_ontology:epsilon_provenance(bully_gang_classification_law, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bully_gang_classification_law_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bully_gang_classification_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bully_gang_classification_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) tracks how much of the law's apparent empirical support is manufactured by discretionary post-hoc sorting rather than earned by surviving pre-specified tests. Suppression (0.58) reflects that dissenting sorts (a case the defender would rather not classify as saved) are not silenced by force but by the absence of any external tribunal empowered to contest the sort. Theater ratio (0.61) is high and rising because an increasing share of the defense's activity is the performance of methodological rigor — citing 'subsidy' or 'pre-payment' as if the criteria were independently specified — rather than genuine ex-ante prediction. Accessibility collapse is moderate (0.42): unlike a mountain, alternative explanatory frameworks for feud persistence do exist and are actively proposed by rival theorists, they are simply denied a forum. Resistance (0.55) is substantial because methodological observers and rival theorists do actively contest the discipline's practice, even though they lack the standing to force a resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the defender's seat, the categorization discipline looks like ordinary, healthy theory-refinement: apparent anomalies get explained by auxiliary hypotheses, as in any research program. From the seat of falsifiability-of-the-law-itself, and from field researchers who need predictive criteria, the same activity looks like an ad hoc rescue that immunizes the theory against any conceivable counterexample. The engine's per-seat computation should reflect that the defender occupies something close to a full-beneficiary position (controls the categorization, bears no cost when it is exercised) while the law's own empirical content occupies something close to a full-target position (it is what gets spent, has no recourse, cannot exit the test).
 *
 * DIRECTIONALITY LOGIC:
 *   The defender benefits directly: every successful post-hoc categorization preserves the theory they hold and are professionally invested in, at zero cost to them, so d sits near the beneficiary end. Falsifiability-of-the-law-itself is a non-agent abstraction that nonetheless functions as the clearest victim — it is what is eroded, and it has no exit whatsoever, so it is placed at the trapped/full-target end. Field researchers and rival theorists sit in between: they bear real costs (unusable predictive apparatus, exclusion from adjudication) but retain some capacity to walk away from the law's predictive claims, hence 'constrained' rather than 'trapped.'
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite errors. It does not let the law's genuine coordination value (a real, useful distinction between subsidized/pre-paid escalation and true unbounded-risk purchase) get erased just because its current defense is currently extractive — the coordination function is real and would be worth restoring under ex-ante criteria. But it also refuses to let 'the theory explains a real phenomenon' license treating the post-hoc rescue discipline as costless. The founding problem (telling real tests from apparent ones) is contested, not dead or fully live: the underlying distinction the law wants to draw remains a live scientific question, but the specific discipline currently used to draw it has drifted from ex-ante test into ex-post rescue, which is exactly the mandatrophy the classification is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ex_ante_vs_ex_post_categorization,
    'Are the subsidy/pre-payment categorization criteria ever published and fixed independently, before a candidate counterexample surfaces, or are they invariably articulated only in response to the triggering case?',
    'Systematic review of the categorization literature''s publication timeline: for each case classified as ''subsidized'' or ''pre-paid,'' check whether the criterion used was stated in a source predating the case''s public emergence.',
    'If criteria are consistently pre-published and independent, the law remains genuinely falsifiable and the constraint is closer to a rope with a real, testable coordination function. If criteria are consistently post-hoc, the constraint is a snare in which falsifiability itself is the extracted resource.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ex_ante_vs_ex_post_categorization, empirical, 'Whether the rescue categories are specified before or after the triggering case.').

omega_variable(
    genuine_counterexample_existence,
    'Do any documented cases exist of an agent with genuine exit options repeatedly choosing open-ended feuds over comparable bounded settlements, that cannot plausibly be sorted into either the subsidized-target or pre-paid-identity-cost category under criteria fixed in advance?',
    'Construct a pre-registered coding protocol for ''subsidized target'' and ''pre-paid identity cost'' before examining any specific case; apply it prospectively to a held-out sample of feud histories.',
    'A nonzero rate of uncategorizable genuine counterexamples would falsify the universal boundedness-pricing claim outright; a zero rate under a genuinely prospective protocol would strongly vindicate it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_counterexample_existence, empirical, 'Whether prospectively-coded genuine counterexamples exist at all.').

omega_variable(
    unsettled_claim_ontology_reading_choice,
    'This constraint concerns the discipline defending the boundedness-pricing law against counterexamples, which presupposes some reading of what an untested self-assertive claim represents beneath the surface. Which reading of the unsettled_claim_ontology kernel does the defended law itself commit to?',
    'Trace whether the boundedness-pricing law''s own defenders treat a feud-escalator''s claim as reflecting an intact self-model (register_reading), a sorting instrument (filter_reading), or a stance without underlying belief (stance_reading) — the categorization discipline analyzed here is largely agnostic on this point, but the underlying law it defends is not.',
    'If the law implicitly commits to filter_reading (claims are sorting instruments, truth-indifferent), the subsidy/pre-payment rescue looks more principled, since ''true test'' is defined structurally rather than by the speaker''s internal state. If it commits to register_reading, the rescue discipline needs an account of why the speaker''s intact self-knowledge doesn''t simply resolve the counterexample question directly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unsettled_claim_ontology_reading_choice, conceptual, 'Which sibling reading of the unsettled_claim_ontology kernel the defended law presupposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bully_gang_classification_law, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bull_tr_t0, bully_gang_classification_law, theater_ratio, 0, 0.3).
narrative_ontology:measurement(bull_tr_t4, bully_gang_classification_law, theater_ratio, 4, 0.38).
narrative_ontology:measurement(bull_tr_t8, bully_gang_classification_law, theater_ratio, 8, 0.45).
narrative_ontology:measurement(bull_tr_t12, bully_gang_classification_law, theater_ratio, 12, 0.5).
narrative_ontology:measurement(bull_tr_t16, bully_gang_classification_law, theater_ratio, 16, 0.55).
narrative_ontology:measurement(bull_tr_t20, bully_gang_classification_law, theater_ratio, 20, 0.58).
narrative_ontology:measurement(bull_tr_t24, bully_gang_classification_law, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(bull_be_t0, bully_gang_classification_law, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bull_be_t4, bully_gang_classification_law, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(bull_be_t8, bully_gang_classification_law, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(bull_be_t12, bully_gang_classification_law, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(bull_be_t16, bully_gang_classification_law, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(bull_be_t20, bully_gang_classification_law, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(bull_be_t24, bully_gang_classification_law, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bull_su_t0, bully_gang_classification_law, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bull_su_t4, bully_gang_classification_law, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(bull_su_t8, bully_gang_classification_law, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(bull_su_t12, bully_gang_classification_law, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(bull_su_t16, bully_gang_classification_law, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(bull_su_t20, bully_gang_classification_law, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(bull_su_t24, bully_gang_classification_law, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bully_gang_classification_law, identity_coordination).
narrative_ontology:affects_constraint(bully_gang_classification_law, boundedness_pricing_horizon_mechanism).

% DUAL FORMULATION NOTE:
% This story is deliberately decomposed from the horizon mechanism itself (why bounded pricing is individually rational given discount rates and exit options — a separate, largely coordination-flavored constraint) per the ε-invariance principle. That sibling constraint concerns the mechanism generating bounded pricing; this constraint concerns the methodological discipline that decides whether apparent violations of the resulting universal claim count as real. The two have different ε profiles: the horizon mechanism is substantially a rope (genuine, low-suppression coordination logic), while this categorization discipline, as currently practiced, functions as a snare on the law's falsifiability. Measuring 'the boundedness-pricing law' as a single observable conflates them; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
