% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: Existential-Risk Reading of AI Alignment Priority
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'AI alignment
 *   priority' kernel: the existential-risk reading, which holds that
 *   alignment work should prioritize preventing catastrophic, irreversible
 *   loss of human control over advanced AI systems above addressing present
 *   discriminatory or extractive harms from deployed systems. Under this
 *   reading, ε is authored for the standing arrangement as the
 *   existential-risk reading itself assesses it — a real coordination
 *   function (pooling scarce safety talent and compute around control-failure
 *   prevention) riding alongside a substantial, growing extraction pattern in
 *   which frontier labs capture funding, self-governance latitude, and
 *   reputational benefit from a framing that also happens to justify
 *   continued capability racing and reduced external audit. This is not a
 *   story about which reading is correct; it is a story about the structural
 *   operation of this one reading, assessed by its own lights, as required by
 *   the ε-referent rule for kernel readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.62).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.48).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential-Risk Reading of AI Alignment Priority").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'b3688fc1-09ad-434b-9824-c25ee30951d2').
narrative_ontology:cs_kernel_codification('b3688fc1-09ad-434b-9824-c25ee30951d2', distributed).
narrative_ontology:cs_authority_grounding('b3688fc1-09ad-434b-9824-c25ee30951d2', distributed).
narrative_ontology:cs_reading_relation('b3688fc1-09ad-434b-9824-c25ee30951d2', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3688fc1-09ad-434b-9824-c25ee30951d2', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('b3688fc1-09ad-434b-9824-c25ee30951d2', foundational, irreversible_catastrophe_dominates_expected_value).
narrative_ontology:cs_axiom_status(irreversible_catastrophe_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('b3688fc1-09ad-434b-9824-c25ee30951d2', irreversible_catastrophe_dominates_expected_value, instrumental).
narrative_ontology:cs_axiom('b3688fc1-09ad-434b-9824-c25ee30951d2', secondary, future_population_moral_weight_equals_present).
narrative_ontology:cs_axiom_status(future_population_moral_weight_equals_present, holdable).
narrative_ontology:cs_axiom_grounding('b3688fc1-09ad-434b-9824-c25ee30951d2', future_population_moral_weight_equals_present, deontological).
narrative_ontology:cs_reference_frame('b3688fc1-09ad-434b-9824-c25ee30951d2', control_failure_prevention_primacy).
narrative_ontology:cs_drift_state('b3688fc1-09ad-434b-9824-c25ee30951d2', post_frontier_scaling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b3688fc1-09ad-434b-9824-c25ee30951d2', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, existential_risk_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_of_humanity).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_day_marginalized_users).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, global_south_ai_affected_communities).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, instrumental_convergence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research agendas, funding priorities, and public safety narratives around preventing catastrophic loss of control. Directs internal safety teams, alignment budgets, and publication norms toward long-horizon capability and control research. Simultaneously builds and races to deploy the very frontier capabilities the existential framing warns about, and uses the existential framing to justify continued scaling, restricted external audit access, and self-governance in place of external regulation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_ai_labs, beneficiary).

% Receive substantial philanthropic and lab funding predicated on the existential framing being the correct priority ordering. Career paths, publication venues, and institutional prestige are built around x-risk research; their continued relevance depends on the existential framing retaining primacy over nearterm-harms framings in funding and policy conversations.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_research_institutes, beneficiary,
    organized, civilizational, mobile, global).

% The undifferentiated future population invoked as the primary moral beneficiary of existential safety work. Cannot speak, vote, fund, or object; its interests are represented entirely by present-day advocates who claim to act on its behalf, with no mechanism for it to contest that representation.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, long_term_future_of_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, long_term_future_of_humanity).

% Experience discriminatory content moderation, biased automated decision systems, labor exploitation in data annotation, and surveillance harms from AI systems deployed today. Resources, research attention, and regulatory bandwidth that could address these harms are redirected toward speculative future-capability risks; these users have no practical exit from platforms that affect employment, benefits, and information access.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_day_marginalized_users, payer,
    powerless, immediate, trapped, global).

% Study algorithmic bias, labor exploitation, and discriminatory deployment harms but compete for the same funding pools, conference slots, and policy attention as existential-risk researchers. Their work is frequently characterized within the existential framing as a lower-priority distraction from 'real' safety work, which affects grant success, hiring, and standing in AI-safety policy circles.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers, excluded).

% Bear content-moderation labor costs, data-extraction costs, and deployment harms from AI systems built primarily for wealthy-market use cases, while having essentially no voice in the safety agenda-setting conversations occurring in a handful of well-resourced labs and institutes concentrated in a few countries.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, global_south_ai_affected_communities, excluded,
    powerless, generational, trapped, global).

% Conduct capability elicitation and control-failure testing on frontier models under the existential-risk methodology, which requires probing and sometimes advancing the very capabilities considered dangerous in order to study control failure modes. Their access and continued employment depend on the labs whose models they test.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, adversarial_red_teamers, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, adversarial_red_teamers, observer).

% Draft AI governance frameworks and take testimony largely shaped by lab-funded existential-risk narratives, which influence which harms get codified into binding rules versus voluntary commitments. Have formal authority to redirect priority but limited technical capacity independent of lab-supplied framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce technical safety talent, compute, and research attention around a single ordering principle — prevent catastrophic, irreversible loss of human control over AI systems before addressing distributional harms of current deployment — on the premise that catastrophic outcomes are lower-probability but foreclose all future correction, so they dominate expected-value calculations regardless of current deployment harms.
% TRANSFER_FUNCTION: Moves funding, researcher attention, regulatory bandwidth, and public narrative capital away from present discriminatory and extractive deployment harms and toward long-horizon capability-control research; the labs producing the risk also capture the resulting reputational and self-governance benefits of being seen as the responsible stewards of that risk.
% ABSENT_VOICES: Present-day harmed users (content moderators, gig-platform workers subject to automated management, Global South communities bearing data-labeling and moderation labor) are structurally distant from the funding bodies, elite research institutes, and lab safety teams that set the existential-risk agenda; the undifferentiated 'future humanity' beneficiary cannot object to or ratify how its interests are represented.
% DISAPPEARANCE_RATIONALE: Existential-risk advocates hold that if the priority ordering disappeared, resources would flow toward capability races without commensurate control research, materially raising catastrophic risk. Nearterm-harms advocates hold that its disappearance would simply reallocate already-scarce safety resources toward addressing measurable, ongoing discriminatory and extractive harms, and that the world would rearrange toward more accountable, more immediately verifiable safety work rather than collapse into unmanaged risk.
% FOUNDING_PROBLEM: Early AI-safety theorists identified that sufficiently capable optimization systems pursuing misspecified goals could produce irreversible, civilization-scale harm, and that no current governance or technical mechanism reliably prevents this outcome once systems cross a capability threshold.
% FOUNDING_PROBLEM_CORROBORATION: Some independent AI safety researchers outside the major labs (in academic control-theory and complex-systems communities) corroborate that loss-of-control risk from advanced optimization is a live, underexplored technical problem. However, nearterm-harms researchers, labor advocates, and several AI ethics scholars operating outside lab funding structures dispute that this framing, as currently resourced and prioritized, is proportionate to demonstrated present harm, and argue the existential framing has become partly self-serving for the labs whose products create both the speculative future risk and the documented present harm.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.42→0.62) as the existential framing becomes institutionally entrenched: more funding concentration in x-risk institutes, more lab self-governance justified by the framing, more displacement of nearterm-harms research from top venues and funding lines. Theater ratio also rises (0.22→0.40) as red-teaming and 'responsible scaling' commitments increasingly serve as publicly legible safety performance alongside continued capability advancement — some of the control research is genuine, but a growing share functions as license to keep scaling. Suppression is moderate and rising (0.30→0.48): the mechanism is not overt coercion but agenda-capture — nearterm-harms researchers are not banned from speaking, but structurally starved of resources, venues, and policy uptake relative to the existential framing's institutional weight. All three series share one time grid per the alignment rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs sit at the beneficiary end: institutional power, arbitrage-grade exit (they can reposition narratives, jurisdictions, and product lines at will), and direct capture of the reputational and governance benefits of the framing. Existential-risk institutes are secondary beneficiaries: their funding and prestige depend on the framing's continued primacy. Present-day marginalized users and Global South communities sit at the target end: powerless, trapped exit, immediate time horizon, bearing deployment harms that the framing's resource-allocation effects leave under-addressed. Nearterm-harm researchers are targets in a professional sense — constrained exit, moderate power, competing for displaced resources. 'Long-term future of humanity' is declared a non-agent beneficiary (agent: false) because it cannot ratify, contest, or benefit in any verifiable present sense from the arrangement made in its name — this is the crux of the framing's central legitimacy question.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (irreversible loss-of-control risk from advanced optimization) retains some corroboration from independent technical communities outside the labs, so this is not a pure zombie-mandate case — the founding_problem_status is authored 'contested' rather than 'dead'. The tangled_rope classification (rather than snare) reflects that a genuine coordination function persists: pooling safety talent around control-failure research is not manufactured. But the classification also registers that the beneficiary/victim asymmetry runs through the same structure that does the coordinating — the labs that create both the speculative future risk and the documented present harm are the same labs capturing the reputational and self-governance benefit of the existential framing. This prevents two mislabeling errors: treating the arrangement as pure cynical extraction (it is not; there is real technical coordination work), and treating it as pure disinterested coordination (it is not; resource capture and self-governance justification ride along).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_probability_calibration,
    'Is the probability of catastrophic loss-of-control from advanced AI systems within the timeframe assumed by this reading high enough to justify the resource reallocation away from documented present harms, or is the probability estimate itself contested and unfalsifiable within relevant timeframes?',
    'Would require either (a) technical consensus on capability thresholds and control-failure probability that does not currently exist, or (b) a track record of near-miss loss-of-control incidents that could calibrate base rates empirically — neither is currently available.',
    'If the probability is well-calibrated and non-negligible, the resource allocation is defensible expected-value reasoning; if the probability is unfalsifiable within any near-term horizon, the framing functions primarily as a resource-capture and reputational mechanism for the labs producing the risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_probability_calibration, empirical, 'Whether existential risk probability estimates are calibrated or unfalsifiable within relevant decision timeframes.').

omega_variable(
    kernel_framing_selection,
    'Is the existential-risk framing the correct primary lens for ''alignment,'' or does the label ''alignment'' itself conflate a technical control-failure claim with a resource-priority claim that should be evaluated separately from the nearterm-harms and integrated readings?',
    'This ambiguity is the reason the kernel was decomposed into three separate constraint stories (existential_risk_reading, nearterm_harms_reading, integrated_reading) per the ε-invariance principle; resolution is structural (decomposition) rather than empirical — each reading is authored as its own constraint with its own ε and beneficiary/victim set.',
    'Readers who treat ''alignment priority'' as a single unified claim risk averaging across readings with materially different ε values and victim sets, which would misrepresent all three. Keeping the readings separate and linked via network.affects_constraints preserves the distinct structural claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the natural-language label ''alignment'' improperly conflates three structurally distinct constraint claims.').

omega_variable(
    future_beneficiary_representation_legitimacy,
    'Can present-day institutions legitimately claim to represent the interests of an undifferentiated future population that cannot consent to, ratify, or contest that representation, or does this representation claim function as an unfalsifiable license for present resource allocation decisions?',
    'No empirical resolution is possible in principle (the represented party cannot testify); resolution is a values question about the legitimacy conditions for intergenerational moral representation absent consent mechanisms.',
    'If such representation is deemed illegitimate absent stronger accountability mechanisms, the beneficiary declaration for ''long_term_future_of_humanity'' should be read as primarily rhetorical cover for present institutional beneficiaries (frontier labs, x-risk institutes) rather than a genuine moral beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_beneficiary_representation_legitimacy, preference, 'Whether present institutions can legitimately claim representation of an undifferentiated future beneficiary with no consent mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__existential_risk_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__existential_risk_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__existential_risk_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__existential_risk_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__existential_risk_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__existential_risk_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__existential_risk_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__existential_risk_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__existential_risk_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(ai_a_be_t24, ai_alignment_priority__existential_risk_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_a_su_t4, ai_alignment_priority__existential_risk_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ai_a_su_t8, ai_alignment_priority__existential_risk_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(ai_a_su_t12, ai_alignment_priority__existential_risk_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(ai_a_su_t16, ai_alignment_priority__existential_risk_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(ai_a_su_t24, ai_alignment_priority__existential_risk_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'AI alignment priority' per the ε-invariance principle: existential_risk_reading (this story, high ε from resource capture and agenda displacement), nearterm_harms_reading (ε authored independently for present discriminatory/extractive deployment harms, with a narrower identifiable victim set), and integrated_reading (ε authored for a framing that treats both priorities as complementary, expected to show lower measured extraction if the integration is structurally genuine rather than rhetorical). Each reading has its own beneficiary/victim structure and its own claimed_type; they are linked here rather than averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
