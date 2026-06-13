% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Execution as Deterrent Signal Against Murder
 *   domain: political/legal/criminal_justice
 *
 * SUMMARY:
 *   The deterrence reading of capital punishment justifies state execution as
 *   a rational mechanism: if potential murderers believe execution is a
 *   certain consequence of murdering, the fear-induced calculation shifts the
 *   incentive structure enough to prevent murders. The condemned offender is
 *   instrumentalized as a bearer of a public message to potential future
 *   offenders. Beneficiaries are the abstract population of potential future
 *   murder victims whose lives are spared through the deterrent effect. The
 *   reading is ONE of three coherent framings of the legitimacy of state
 *   killing: the abolition reading rejects execution categorically as a
 *   violation of human dignity regardless of utility; the retributive reading
 *   justifies it through proportional desert (the murderer forfeits
 *   life-right), not future prevention. This JSON instantiates ONLY the
 *   deterrence reading as a self-contained constraint with its own ε,
 *   beneficiary/victim structure, and causal claim. The other readings are
 *   separate constraint stories, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - condemned_offender: powerless, trapped, immediate horizon — bears the extracted cost (execution) instrumentalized as a signal
 *   - potential_future_victims: organized (statistically), analytical exit — abstract beneficiaries of the deterrent effect
 *   - criminal_justice_authority: institutional, constrained exit — sets and enforces the rule, maintains credibility by executing
 *   - actual_murder_victims_families: moderate power, constrained — secondary beneficiaries; provide emotional foundation for the deterrent signal
 *   - incarcerated_potential_murderers: powerless, trapped — primary audience for the deterrent threat; must respond rationally
 *   - jurisdictions_without_execution: organized, excluded — their data complicate the reading; their non-inclusion is structural to how the reading is maintained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.62).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.71).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Execution as Deterrent Signal Against Murder").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "political/legal/criminal_justice").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'd703e9af-d07f-4951-81f6-636e2a08cb47').
narrative_ontology:cs_kernel_codification('d703e9af-d07f-4951-81f6-636e2a08cb47', formalized).
narrative_ontology:cs_authority_grounding('d703e9af-d07f-4951-81f6-636e2a08cb47', extraction).
narrative_ontology:cs_interpretation_layer_present('d703e9af-d07f-4951-81f6-636e2a08cb47').
narrative_ontology:cs_reading_relation('d703e9af-d07f-4951-81f6-636e2a08cb47', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('d703e9af-d07f-4951-81f6-636e2a08cb47', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('d703e9af-d07f-4951-81f6-636e2a08cb47', foundational, execution_prevents_future_murder_through_rational_deterrence).
narrative_ontology:cs_axiom_status(execution_prevents_future_murder_through_rational_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('d703e9af-d07f-4951-81f6-636e2a08cb47', execution_prevents_future_murder_through_rational_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('d703e9af-d07f-4951-81f6-636e2a08cb47', foundational, offender_instrumentalization_justified_by_aggregate_safety).
narrative_ontology:cs_axiom_status(offender_instrumentalization_justified_by_aggregate_safety, holdable).
narrative_ontology:cs_axiom_grounding('d703e9af-d07f-4951-81f6-636e2a08cb47', offender_instrumentalization_justified_by_aggregate_safety, instrumental).
narrative_ontology:cs_reference_frame('d703e9af-d07f-4951-81f6-636e2a08cb47', rational_actor_murder_prevention_framework).
narrative_ontology:cs_drift_state('d703e9af-d07f-4951-81f6-636e2a08cb47', contemporary_post_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d703e9af-d07f-4951-81f6-636e2a08cb47', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, general_public_safety_interest).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offender).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, actual_murder_victims_families).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, actual_murder_victims_families).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, incarcerated_potential_murderers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject of the execution. In this reading, the offender is instrumentalized as a bearer of a message to potential future murderers: 'this will happen to you.' The offender's individual circumstances, suffering, and humanity are compressed into a rational signal. Exit is impossible once the judgment is finalized; the constraint extracts the offender's life.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offender, payer,
    powerless, immediate, trapped, national).

% Abstract beneficiaries of the deterrent signal. They do not exist yet as identifiable persons; they are the statistical future population whose murders are prevented (in the deterrence reading's causal narrative) by the fear generated in potential murderers by the execution. The coordination problem solved is preventing future murders through rational fear-calculation.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    organized, generational, analytical, national).

% Legislates, adjudicates, and carries out executions. In the deterrence reading, the authority's mandate is to make the execution visible and certain enough to modify murderers' behavior. The authority must maintain enforcement credibility by actually executing rather than merely threatening. The institutional interest is in public safety measured by crime rates.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminal_justice_authority, agenda_setter,
    institutional, generational, constrained, national).

% In the deterrence reading, receive retributive closure (symbolic vindication of their loss) as a byproduct of the deterrent mechanism, not as the mechanism's justification. Their presence anchors the emotional reality of murder that the deterrent message must invoke. They are beneficiaries of the public safety benefit if deterrence works, but their satisfaction is not the causal engine the reading turns on. They also bear the cost of living in a society that uses execution as policy, confronting reminders of the violence that took their loved one.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, actual_murder_victims_families, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, actual_murder_victims_families, payer).

% Primary audience for the deterrent signal. The reading treats them as rational actors who calculate the costs of murdering: 'If I kill, I will be executed.' The constraint's function depends on their ability and willingness to perform this calculation and to have their behavior shaped by it. They are not beneficiaries; they bear the suppressive cost of the threat.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, incarcerated_potential_murderers, payer,
    powerless, biographical, trapped, national).

% Exist and operate without capital punishment, yet do not report proportionally higher murder rates (empirical fact contested in the deterrence reading). Their exclusion from the analysis is structural: the deterrence reading requires framing as a choice among available policy options, not as a fact of legal systems. Their data complicate the reading but are managed as insufficient-enforcement problems rather than falsifications.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, jurisdictions_without_execution, excluded,
    organized, generational, constrained, global).

% Articulate an alternative reading and lobby for policy change. They are not in the room where the deterrence logic is applied — they are structural witnesses to the reading's contestation. Their absence from enforcement authority is the reason they remain observers rather than agenda-setters.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolition_advocates, observer,
    moderate, generational, mobile, national).

% Support capital punishment on grounds of proportional desert rather than deterrence. They operate within the same institutional framework as the deterrence reading (both justify execution) but for different reasons. Their position is not excluded but orthogonal — they and the deterrence reading coexist in the same authority structure, sometimes in tension over justification.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, retributive_justice_advocates, observer,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__deterrence_reading, criminal_justice_authority).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents future murders by establishing rational fear in potential murderers: if the cost of committing murder is certain execution, the rational actor recalculates the act's utility. The coordination problem solved is aligning individual murderers' rational incentives with the public safety interest in preventing murder.
% TRANSFER_FUNCTION: Moves the condemned offender's life from their possession to the state's disposal, justified by the future benefit to potential future murder victims through the deterrent effect on murderers' behavior. The value transferred is the offender's existence; the recipient is the abstract statistical population of future murder victims; the intermediate carrier is the criminal justice authority's enforcement machinery.
% ABSENT_VOICES: Jurisdictions that abolished capital punishment and report no corresponding rise in murder rates are structurally excluded from the deterrence reading's framework — their data point to a different causal story. Condemned offenders' own testimony about their crime and motivation (are they rational calculators, or are they driven by passion, mental illness, or circumstances that rational fear does not reach?) is suppressed in the reading; if murderers are not rational calculators the deterrent mechanism does not function. Philosophers and legal theorists who argue that human dignity is inviolable regardless of utility are also excluded from the legitimacy frame the reading constructs.
% DISAPPEARANCE_RATIONALE: If capital punishment and its deterrent justification disappeared overnight, the criminal justice system would recalibrate to alternative incapacitation and rehabilitation modalities. Murder rates would either remain stable or rise depending on whether deterrence actually reduces murders (the empirical claim). The reading's framework depends on the execution being real and visible; absent that, the coordination problem the reading purports to solve would require solving through different mechanisms (life sentences, mandatory minimum penalties, enhanced policing, social investment in murder prevention, or acceptance of higher murder rates).
% FOUNDING_PROBLEM: Rational actors considering murder must be given sufficient incentive to refrain. The problem is that murder imposes uncompensated costs on victims' families and society (grief, trauma, loss of life) that the rational murderer does not internalize. The solution is to externalize a cost (execution) that outweighs the perceived benefit of the murder to the murderer.
% FOUNDING_PROBLEM_CORROBORATION: Criminologists and economists who model crime as rational choice behavior attest the founding problem is live: deterrence theory predicts murder rates should correlate with execution probability and severity. However, empirical criminologists outside the deterrence-committed faction point to longitudinal data (US states without capital punishment show no higher murder rates; countries that abolished execution show no subsequent murder increases) as evidence the founding problem is either dead or poorly specified. Legislative bodies and law-and-order constituencies in executing jurisdictions attest the problem is live and deterrence justified; human rights organizations, abolitionist jurisdictions, and meta-analytic reviews of deterrence research attest the problem is either a false premise (murder is not rational calculation) or solved without execution. The corroborating source outside the deterrence-benefiting faction is the comparative criminological literature and the natural experiment of jurisdictional variation.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.62: the constraint extracts the offender's life based on a causal claim about future prevention (moderate, contested evidence). Suppression measures 0.71: maintaining the constraint requires suppressing alternative framings (that murders are not rational-calculus-driven, that non-execution jurisdictions disprove deterrence, that dignity violations are categorically forbidden). Theater ratio measures 0.48: a significant but not dominant share of the constraint's operation is performative — the public visibility of execution as spectacle/message, the ceremonial character of capital trials, the rhetorical emphasis on deterrence in judicial opinions and state communications. The measurements track rising theater and suppression from 1930 to 2010 (as empirical challenges to deterrence accumulate, the reading must work harder to maintain itself through visibility and narrative framing), then stabilize as the constraint enters a mature defensive posture. Accessibility collapse (0.68) reflects the constraint's dual character: genuine alternatives to capital punishment exist and are used by other jurisdictions, but once a jurisdiction adopts the deterrence reading, alternatives become cognitively foreclosed (the fear-based framework makes non-execution look permissive).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (criminal justice authority) experiences this constraint as a coordination mechanism it chose and maintains for public safety. The condemned offender experiences it as arbitrary death dressed in rational language. Potential future victims (the beneficiary class) cannot exist as seated stakeholders with alternative perspectives because they are abstract and future. The actual murder victims' families experience mixed positioning: they benefit from the public vindication the execution provides, but their satisfaction is not what the reading turns on — the reading's function is deterrence, not retribution. This perspectival gap is structural to the constraint: the deterrence reading instrumentalizes individual offenders to serve a public good, which is why the offender's power atom is powerless and their exit options are trapped. A retributive reading would center the offender as a rights-bearer whose desert is at stake, shifting perspective; the deterrence reading deliberately subordinates individual justice to aggregate safety.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned offender is the full target (d=1.0): extraction of life for the deterrent signal's benefit, with no compensating benefit. Potential future victims sit at d=0.0 (full beneficiary analytically, though they are abstract and statistical). Criminal justice authority sits near d=0.3: they benefit from maintaining enforcement credibility and the appearance of public safety coordination, but they also bear the cost of managing the constraint against rising resistance and empirical challenges. Murder victims' families sit near d=0.4: they benefit from public vindication but are secondary to the reading's logic. No directionality override is needed; the structural data (powerless/trapped for offender, organized/analytical for future victims) derives the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading exhibits mandatrophy risk: the founding problem (rational actors must be given sufficient incentive to refrain from murder) is contested as live vs. dead. Empirical evidence from non-execution jurisdictions suggests the founding problem may be dead (murder rates do not rise without capital punishment), yet the reading persists through theatrical and rhetorical maintenance. The theater ratio's rise from 0.32 to 0.48 signals this atrophy: more of the constraint's operation becomes spectacle and narrative (judicial rhetoric emphasizing deterrence, public messaging, ceremonial trials) and less becomes actual causal mechanism (the evidence for deterrent effect is weak and contested). The suppression requirement rising from 0.54 to 0.71 indicates that holding the reading against empirical and ethical challenges requires more active suppression of alternatives. If the theater ratio continued rising above 0.60 and suppression continued above 0.75 while extractiveness remained static or declined, the constraint would reclassify toward piton (inertial, theatrically maintained, functionally atrophied). The current metrics suggest tangled_rope integrity but with visible wear: genuine coordination function (if deterrence works, future murders are prevented) yoked to asymmetric extraction (the condemned bear all cost), held together by enforcement and narrative work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_validity,
    'Does capital punishment actually deter murder more effectively than long-term incarceration, relative to the cost and risk of executing innocent people?',
    'Longitudinal comparative criminology: murder rate trajectories in execution and non-execution jurisdictions controlling for socioeconomic factors, policing intensity, and incarceration rates. Meta-analysis of criminological studies on deterrent effect sizes.',
    'If deterrence effect is zero or negligible relative to long-term incapacitation, the constraint reclassifies as purely extractive (snare), with the deterrence narrative as cover. If deterrence effect is substantial and robust, the tangled_rope classification holds with moderate extractiveness justified by coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_validity, empirical, 'The causal claim undergirding the deterrence reading: does execution actually prevent future murders?').

omega_variable(
    rationality_assumption_validity,
    'Are murderers in fact rational actors who calculate expected penalties, or are murders typically driven by passion, mental illness, circumstance, or cultural/social factors that rational fear does not reach?',
    'Psychological and sociological study of actual murderers'' decision-making: interviews, case analysis, behavioral analysis. Epidemiological study of murder causation factors (substance abuse, relationship conflict, mental health status, socioeconomic deprivation).',
    'If murderers are predominantly non-rational or irrational, the constraint''s causal mechanism fails entirely regardless of deterrent signaling: the reading collapses into pure extraction. If murderers are substantially rational in some contexts, deterrence works partially, and the constraint retains moderate extractiveness justified by partial coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationality_assumption_validity, empirical, 'Whether the assumed rational actor model matches the actual causation of murder.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the deterrence reading logically foreclose the abolition reading, or can both coexist as live positions held by different parties?',
    'Conceptual analysis: Does accepting deterrence as the justification for execution require denying that human dignity is inviolable? Or can one maintain that even utility-justified execution violates dignity, and therefore dignity constraints override utility? Can both framings be held simultaneously by different authorities without logical contradiction?',
    'If deterrence reading FORECLOSES abolition (one core premise directly contradicts the other), the relation is foreclosure in cs_structure.reading_relations. If both can be held simultaneously (different parties accept different premises), the relation is coexists_with. If deterrence reading changes the structural conditions for abolition (e.g., by establishing empirical claims about utility that abolition must then engage), the relation is influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'The logical relationship between the deterrence reading and the abolition reading.').

omega_variable(
    instrumental_vs_dignity_framing,
    'Is the offender''s instrumentalization as a signal-bearer compatible with frameworks that treat human dignity as inviolable, or does the deterrence reading necessarily require treating the offender as a mere means?',
    'Philosophical analysis of instrumental reason in criminal justice: can one justifiably use a person as a means to others'' safety while respecting their dignity? Is the distinction between forward-looking (deterrence) and backward-looking (retribution/dignity) justifications logically coherent or is it a false boundary?',
    'If instrumentalization of the offender is incompatible with dignity frameworks, the deterrence and abolition readings foreclose each other. If instrumentalization is defensible as a form of respecting equal citizenship (the offender''s crime forfeits dignity claims), the readings coexist. The resolution affects the reading_relations classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumental_vs_dignity_framing, conceptual, 'Whether deterrence reasoning requires treating the condemned as mere means, incompatible with dignity.').

omega_variable(
    suppression_mechanism_structural,
    'Is the measured suppression (0.71) primarily structural (empirical evidence against deterrence is actively excluded from policy discourse by institutional filters) or primarily internalized (those who encounter deterrence reasoning adopt it as their framework and suppress their own contrary evidence)?',
    'Post-exposure study: do criminal justice professionals, judges, and policymakers who encounter high-quality evidence against deterrence change their positions, or do they maintain the deterrence narrative despite evidence? Does exposure to abolitionist arguments change the suppression profile?',
    'If suppression is primarily structural, the constraint is maintained by institutional gatekeeping; if blocked, the constraint would likely be revised. If suppression is primarily internalized, the constraint is maintained by cognitive adherence; if structural filters loosened, internalized suppression might persist. The distinction informs whether remedies should target institutional rules or cultural narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural, empirical, 'Whether suppression is external institutional pressure or internalized cognitive framing.').

omega_variable(
    beneficiary_abstractness_extraction_validity,
    'Can an abstract, statistical population of future murder victims serve as a valid beneficiary class for extraction from a concrete, present offender, or does the asymmetry between present concrete cost and future abstract benefit render the extraction fundamentally asymmetric in a way that disqualifies tangled_rope classification?',
    'Philosophical and legal theory analysis: does the abstraction of the beneficiary class (future, statistical, non-identifiable individuals) change the validity of the constraint? Can present sacrifice be justified for abstract future benefit? Is this mechanism used elsewhere in legitimate policy (e.g., long-term environmental regulation)?',
    'If the abstraction is deemed illegitimate, the constraint reclassifies as snare (asymmetric extraction disguised as coordination). If the abstraction is acceptable (as it is in many policy domains), tangled_rope classification holds with the caveat that the coordination function is purely forward-looking and contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_abstractness_extraction_validity, conceptual, 'Whether abstract future beneficiaries can justify extraction from present agents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 1930, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1930, state_killing_legitimacy__deterrence_reading, theater_ratio, 1930, 0.32).
narrative_ontology:measurement_basis(stat_tr_t1930, observed).
narrative_ontology:measurement(stat_tr_t1960, state_killing_legitimacy__deterrence_reading, theater_ratio, 1960, 0.38).
narrative_ontology:measurement_basis(stat_tr_t1960, observed).
narrative_ontology:measurement(stat_tr_t1990, state_killing_legitimacy__deterrence_reading, theater_ratio, 1990, 0.43).
narrative_ontology:measurement_basis(stat_tr_t1990, observed).
narrative_ontology:measurement(stat_tr_t2010, state_killing_legitimacy__deterrence_reading, theater_ratio, 2010, 0.47).
narrative_ontology:measurement_basis(stat_tr_t2010, observed).
narrative_ontology:measurement(stat_tr_t2020, state_killing_legitimacy__deterrence_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement_basis(stat_tr_t2020, observed).
narrative_ontology:measurement(stat_tr_t2025, state_killing_legitimacy__deterrence_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(stat_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t1930, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1930, 0.48).
narrative_ontology:measurement_basis(stat_be_t1930, observed).
narrative_ontology:measurement(stat_be_t1960, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement_basis(stat_be_t1960, observed).
narrative_ontology:measurement(stat_be_t1990, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement_basis(stat_be_t1990, observed).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement_basis(stat_be_t2010, observed).
narrative_ontology:measurement(stat_be_t2020, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement_basis(stat_be_t2020, observed).
narrative_ontology:measurement(stat_be_t2025, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(stat_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1930, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1930, 0.54).
narrative_ontology:measurement_basis(stat_su_t1930, observed).
narrative_ontology:measurement(stat_su_t1960, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement_basis(stat_su_t1960, observed).
narrative_ontology:measurement(stat_su_t1990, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement_basis(stat_su_t1990, observed).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement_basis(stat_su_t2010, observed).
narrative_ontology:measurement(stat_su_t2020, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement_basis(stat_su_t2020, observed).
narrative_ontology:measurement(stat_su_t2025, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(stat_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% The state_killing_legitimacy kernel instantiates three structurally distinct constraints with different ε values, beneficiary/victim structures, and causal claims. The deterrence_reading (this file) justifies execution through prevention of future murders via rational fear. The retributive_reading justifies execution through proportional desert (the offender forfeits life-right), independent of future prevention. The abolition_reading denies that execution is ever legitimate, categorically rejecting state killing as a violation of human dignity regardless of utility. Each reading is a separate constraint with separate author-supplied metrics. They are linked as a kernel family: the deterrence reading INFLUENCES both siblings by establishing empirical claims about utility that they must then engage or refute. The abolition reading COEXISTS with both deterrence and retributive readings (all three are live positions held by different jurisdictions and constituencies). The retributive and deterrence readings COEXIST (some justifications invoke both, some invoke only one). No single reading forecloses both others within a coherent legal framework; they are competing premises rather than logically contradictory claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
