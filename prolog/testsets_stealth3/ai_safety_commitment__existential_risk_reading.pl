% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__existential_risk_reading, []).

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
 *   constraint_id: ai_safety_commitment__existential_risk_reading
 *   human_readable: Existential-Risk Reading of the AI Safety Commitment
 *   domain: technology governance/risk assessment
 *
 * SUMMARY:
 *   This story instantiates the existential_risk_reading of the
 *   ai_safety_commitment kernel: the definitional commitment that AI safety
 *   means preventing extinction-level outcomes from misaligned
 *   superintelligent systems. The standing arrangement under contest — and
 *   the epsilon referent, assessed by this reading's own lights — is the
 *   x-risk-framed AI safety enterprise as it actually operates: funding
 *   portfolios, laboratory safety organizations, policy pipelines, and career
 *   structures ordered by the premise that loss-of-control risk dominates all
 *   other safety claims. The reading sincerely pursues protection of future
 *   humanity; the arrangement it sustains nonetheless transfers resources and
 *   moral standing away from documented present harms toward speculative
 *   pre-deployment interventions, producing identifiable payers alongside its
 *   conditional beneficiary. Claim and metrics are authored independently:
 *   the constraint is claimed as tangled_rope (genuine coordination around a
 *   real potential catastrophe, joined to asymmetric transfer sustained by
 *   active enforcement), while the metrics describe moderately high, slowly
 *   accumulating extraction. Sibling readings (near_term_harms_reading,
 *   dual_priority_reading) are separate constraints with their own epsilon
 *   values; the contest between them is recorded in omega variables, not
 *   averaged into this file. KEY AGENTS (by structural relationship): -
 *   alignment_research_community: Primary beneficiary
 *   (organized/identity_locked) — receives the transferred funding, talent,
 *   and field-defining status - frontier_deployment_labs: Secondary
 *   beneficiary and agenda co-setter (institutional/arbitrage) — collects
 *   reputational and regulatory positioning; shapes the frame it benefits
 *   from - xrisk_policy_advocates: Agenda setter (organized/identity_locked)
 *   — supplies the policy vocabulary and priority ordering -
 *   ai_safety_funders: Agenda setter (powerful/mobile) — allocates the
 *   portfolios that set which questions are askable -
 *   present_harm_affected_communities: Primary payer (powerless/constrained)
 *   — bears documented injuries the priority ordering renders secondary -
 *   fairness_accountability_researchers: Payer (moderate/constrained) — bears
 *   subfield marginalization and grant attrition - future_humanity: Nominal
 *   protectee and unconditional loss-bearer (powerless/trapped/universal) —
 *   the wagered party - independent_risk_assessors: Analytical observer —
 *   audits both literatures from outside either camp
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.55).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "Existential-Risk Reading of the AI Safety Commitment").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technology governance/risk assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, '85826993-b771-48e5-8596-c9e5e911fb3f').
narrative_ontology:cs_kernel_codification('85826993-b771-48e5-8596-c9e5e911fb3f', distributed).
narrative_ontology:cs_authority_grounding('85826993-b771-48e5-8596-c9e5e911fb3f', expertise).
narrative_ontology:cs_interpretation_layer_present('85826993-b771-48e5-8596-c9e5e911fb3f').
narrative_ontology:cs_reading_relation('85826993-b771-48e5-8596-c9e5e911fb3f', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('85826993-b771-48e5-8596-c9e5e911fb3f', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('85826993-b771-48e5-8596-c9e5e911fb3f', foundational, extinction_stakes_dominate_all_other_safety_claims).
narrative_ontology:cs_axiom_status(extinction_stakes_dominate_all_other_safety_claims, holdable).
narrative_ontology:cs_axiom_grounding('85826993-b771-48e5-8596-c9e5e911fb3f', extinction_stakes_dominate_all_other_safety_claims, deontological).
narrative_ontology:cs_axiom('85826993-b771-48e5-8596-c9e5e911fb3f', foundational, misaligned_superintelligence_is_the_operative_threat).
narrative_ontology:cs_axiom_status(misaligned_superintelligence_is_the_operative_threat, holdable).
narrative_ontology:cs_axiom_grounding('85826993-b771-48e5-8596-c9e5e911fb3f', misaligned_superintelligence_is_the_operative_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('85826993-b771-48e5-8596-c9e5e911fb3f', extinction_precedence_framework).
narrative_ontology:cs_drift_state('85826993-b771-48e5-8596-c9e5e911fb3f', post_deployed_capability_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('85826993-b771-48e5-8596-c9e5e911fb3f', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, alignment_research_community).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, frontier_deployment_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, xrisk_policy_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, present_harm_affected_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, fairness_accountability_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, future_humanity).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, superintelligence_arrival_premise).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, alignment_problem_centrality).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, longtermist_stakes_weighting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conducts mechanistic interpretability, scalable oversight, and related technical programs funded overwhelmingly under the civilization-scale risk premise. Careers, lab headcounts, and field status track the premise's acceptance; departure would mean abandoning both livelihood and the moral project that organizes their working lives.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, alignment_research_community, beneficiary,
    organized, generational, identity_locked, global).

% Builds and deploys large general-purpose models while operating safety teams and publishing responsible-scaling commitments framed around future loss-of-control scenarios. The framing positions them as responsible stewards of the technology, secures seats in government safety negotiations, and keeps present-deployment oversight lighter than civil-rights or product-liability enforcement would impose. They can shift messaging, jurisdiction, and product lines at will.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, frontier_deployment_labs, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, frontier_deployment_labs, agenda_setter).

% Runs campaigns for licensing regimes, compute thresholds, slowdown proposals, and international safety institutes; supplies the policy vocabulary that legislatures and summit processes adopt. Their organizations' relevance and fundraising depend on the priority ordering they advocate.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, xrisk_policy_advocates, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, xrisk_policy_advocates, beneficiary).

% Directs large philanthropic portfolios toward alignment research, field-building, and governance work according to the civilization-scale premise. Can rebalance or withdraw on grant-cycle timescales; their allocation choices effectively determine which questions the field can afford to ask.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_safety_funders, agenda_setter,
    powerful, generational, mobile, global).

% People subject to algorithmic decisions in hiring, lending, housing, policing, and content moderation today. Their documented injuries receive little attention in the forums where the field's priorities are set; they cannot opt out of the systems affecting them and lack standing in the venues where research agendas and policy frameworks are negotiated.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, present_harm_affected_communities, payer,
    powerless, immediate, constrained, national).

% Studies and documents discriminatory and exploitative behavior of deployed systems. Publishes in venues whose prestige and funding have shifted toward pre-deployment alignment topics; some have rebranded their work under safety terminology to retain support, others have lost grants and editorial influence. Leaving for adjacent fields carries real professional cost.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, fairness_accountability_researchers, payer,
    moderate, biographical, constrained, global).

% Does not yet exist. The entire arrangement is conducted on their behalf, committing their prospective existence and circumstances to the success of technical and governance interventions they cannot consent to, evaluate, or refuse. If the undertaking succeeds they inherit the benefit; if it fails they bear the entirety of the loss; nothing they could do changes their position.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Tracks both the alignment literature and the deployed-harms literature from outside either camp; audits claims about capability trajectories and intervention efficacy; holds no stake in which framing prevails.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, independent_risk_assessors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__existential_risk_reading, alignment_research_community).
narrative_ontology:fixing_cost_class(ai_safety_commitment__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives funders, laboratories, and governments a single prioritization criterion for an emerging technology whose possible failure modes are otherwise incomparable: allocate safety effort to reduce the probability of permanent, unrecoverable loss of human control. Without it, safety budgets fragment across every documented harm with no agreed way to rank them.
% TRANSFER_FUNCTION: Moves research funding, talent, media attention, and regulatory agenda space from accountability for already-deployed systems toward pre-deployment alignment research, capability forecasting, and compute-governance advocacy; correspondingly moves recognized moral urgency from people injured by systems operating today to people who may exist later.
% ABSENT_VOICES: Communities subjected to algorithmic hiring, lending, policing, and content-moderation decisions have no seat in the summits, laboratory advisory boards, or funder strategy sessions where this reading's priorities are set; global-majority populations affected by deployment are underrepresented in a field concentrated in a few Anglophone institutions. Their objections live in adjacent venues that this reading's resource flows bypass.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would strand funded research agendas, dissolve laboratory safety-team mandates built around loss-of-control scenarios, empty the policy pipeline of licensing and compute-threshold proposals, and force funders to re-rank every candidate harm on a common scale. Near-term accountability work would gain relative standing within months, and the field's center of gravity would migrate toward the sibling readings.
% FOUNDING_PROBLEM: In the early 2010s, machine-learning capability trends suggested systems could eventually exceed human oversight, and no research program, institution, or governance body was organized around preparing for that possibility. The reading was built to make loss-of-control preparation the field's defining obligation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by independent expert signatories of the 2023 Center for AI Safety statement who hold no alignment-funding ties, by governmental and intergovernmental risk assessments treating loss-of-control as a scenario requiring preparation, and by academic machine-learning figures critical of the field's sociology who nonetheless rate the tail risk as non-negligible. The status is nonetheless contested: deployed-harms researchers and affected-community advocates attest from outside the beneficiary set that the founding problem as prioritized remains speculative while documented injuries accumulate unaddressed.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65 at interval end) reflects a large, sustained transfer of field resources onto a premise that cannot currently be verified or falsified: the beneficiary side collects funding, talent, and agenda-setting power, while the payer side consists of present-harm constituencies whose claims lose standing and researchers whose subfield lost prestige and grant access. Suppression (0.55) is social and institutional rather than legal — funding gates, venue hierarchies, community norms that treat near-term focus as naivety, laboratory communications strategies — and is authored as a raw structural property, unscaled by power or scope. Theater (0.40) tracks the growth of voluntary-commitment and safety-framework performance lacking external verification alongside genuine technical work. Accessibility collapse (0.58) is partial: inside the frame, accepting the premise collapses alternatives, since any lesser allocation looks negligent; across frames the near-term and dual-priority alternatives remain live. Resistance (0.55) is sustained: FAccT-community critique, affected-community organizing, and innovation-first policy coalitions. All three measurement series share one seven-point grid (2014-2025). Base extractiveness rises monotonically — accumulation consistent with an abductive investigation trigger — theater accelerates after 2022 as voluntary commitments proliferate, and suppression_requirement rises with the post-2023 buildout of responsible-scaling policies, safety institutes, and licensing advocacy: enforcement capacity maturing, not merely extraction shifting. Identity-lock mechanism: alignment researchers and policy advocates fuse professional and ideological identity with the mission; exit would cost livelihood and moral self-concept simultaneously, which is why their seats classify differently from funders, who hold mobile exit and therefore milder positionalities despite equal agenda-setting power.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute divergent types from identical structural facts. From the alignment-research seat the arrangement is a sober insurance program against the largest conceivable loss; from the present-harm seat it is the erasure of documented injury by hypothetical catastrophe; from the laboratory seat it is a favorable bargain — existential-stewardship standing in exchange for lighter present-deployment accountability; from the observer seat both descriptions fit parts of the record. Same-level lateral differentiation: fairness/accountability researchers and alignment researchers hold comparable formal standing (credentialed experts at similar institutions), but the former face constrained exit (retraining, lost networks) while the latter are identity_locked (exit dissolves the mission self), so the same field treats them as differently positioned. Coalition potential among the powerless: present-harm communities, displaced workers, and accountability researchers could form a cross-domain coalition contesting the priority ordering, though their dispersed scopes and immediate time horizons make coordination costly. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the research community (collects the transfer directly), the laboratories (collect positioning and deferred accountability), and the advocates (collect relevance and agenda control). Victim declarations drive high d for present-harm communities (powerless, constrained exit, national scope), accountability researchers (moderate power, constrained exit), and future humanity — the structural paradox of this reading: the professed protectees hold the largest stake and the least agency. They are modeled as payers because the arrangement commits their entire prospective position to interventions they cannot consent to, refuse, or exit, at universal scope, which the engine amplifies; they are simultaneously the arrangement's intended beneficiaries, and that double position is documented here rather than resolved by override. No directionality_overrides entries were needed: the derivation from declarations, power, and exit reproduces the intended relationships. The one soft spot — accountability researchers who partially exited INTO the frame by rebranding — is noted qualitatively rather than overridden, since their net position remains payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (loss-of-control preparedness) is contested rather than dead: the parties dispute whether it is live, so the mismatch consumer finds no dead-problem-plus-world-rearranges signature and no zombie flag fires. Mandatrophy discipline still earns its keep here: without the type apparatus, the arrangement would be mislabeled in both directions — as pure coordination (hiding the crowding-out of present-harm remedies behind the insurance story) or as pure extraction (erasing the genuine tail-risk coordination a rational field might undertake under deep uncertainty). Tangled rope holds both halves: a real coordination function, a real asymmetric transfer, and active enforcement required to sustain the priority ordering. If the arrival premise resolved negatively, the mandate would die while the apparatus persisted — the pre-registered path to piton or snare runs through the arrival-premise and intervention-efficacy omegas, not through this story's static claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the ai_safety_commitment kernel; how would the near_term_harms_reading and dual_priority_reading restructure the victim set, the beneficiary set, and the distribution of extractiveness across interventions?',
    'Comparative classification of the sibling stories: near_term_harms_reading relocates victims to documented present persons and epsilon to deployed-system accountability; dual_priority_reading splits the referent into two linked constraints with separate epsilon values.',
    'Sibling adoption would collapse this reading''s potentially-infinite victim class into documented present victims, raise effective extraction on present-accountability neglect, and lower it on speculative interventions — changing the classification seat by seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the AI-safety kernel this constraint instantiates and what the sibling readings would change.').

omega_variable(
    superintelligence_arrival_premise,
    'Is misaligned superintelligence a physically and technically reachable outcome on planning-relevant timescales, or does the premise fail such that the arrangement coordinates effort toward an empty object?',
    'Capability-scaling evidence, convergence or divergence of expert forecasts, and adversarial evaluation of whether current training paradigms can produce goal-directed superhuman optimizers.',
    'If the premise fails, the coordination function evaporates while the resource capture persists — reclassification toward snare or piton; if it holds, the same capture is partially the price of insurance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superintelligence_arrival_premise, empirical, 'Whether the load-bearing empirical premise of the reading is true.').

omega_variable(
    intervention_efficacy_attribution,
    'Do the favored interventions (scalable oversight, interpretability, compute governance, pause proposals) actually reduce extinction probability, or are they unfalsifiable reassurance purchases?',
    'Adversarial red-team benchmarks, transfer studies of alignment techniques to stronger systems, and retrospective audit of whether governance interventions changed deployment decisions.',
    'Determines how much measured extraction is purchased risk reduction (rope-side residual) versus rent collected under uncertainty (snare-side residual).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_efficacy_attribution, empirical, 'Whether the speculative technical interventions deliver the risk reduction that justifies their resource share.').

omega_variable(
    crowding_out_counterfactual,
    'How much present-harm remediation is genuinely displaced by the x-risk resource capture, versus how much would have gone unfunded regardless?',
    'Funding-flow panel data across foundations and agencies; natural experiments where portfolios shifted toward or away from the frame.',
    'Sets the realized size of the present-victim classes'' loss; small counterfactual displacement leans the classification toward rope, large displacement toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_counterfactual, empirical, 'Magnitude of the crowding-out that constitutes the present-harm victims'' loss.').

omega_variable(
    future_persons_moral_standing,
    'Can not-yet-existent persons hold victim status, and what weight does their stake carry relative to documented present suffering?',
    'Not resolvable by data: turns on totalist versus person-affecting population ethics and on how discounting of future stakes is justified.',
    'Drives the potentially-infinite victim-set magnitude; a person-affecting resolution shrinks the victim class to present persons and moves the reading toward the near_term_harms sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_persons_moral_standing, conceptual, 'Population-ethics ambiguity underlying the victim-set declaration for future_humanity.').

omega_variable(
    frame_instrumentalization_by_labs,
    'To what extent do frontier developers endorse the civilization-scale frame sincerely versus instrumentally, as a substitute for present-deployment accountability?',
    'Revealed-preference comparison: lobbying positions on present-harm regulation versus x-risk regulation, willingness to accept third-party auditing, internal-document disclosure.',
    'Predominantly instrumental endorsement converts the coordination story into cover for deployment license and pushes the classification toward snare; sincere endorsement preserves tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frame_instrumentalization_by_labs, empirical, 'Sincerity versus instrumentality of laboratory endorsement of the frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 2014, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2014, ai_safety_commitment__existential_risk_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(ai_s_tr_t2016, ai_safety_commitment__existential_risk_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(ai_s_tr_t2018, ai_safety_commitment__existential_risk_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(ai_s_tr_t2020, ai_safety_commitment__existential_risk_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(ai_s_tr_t2022, ai_safety_commitment__existential_risk_reading, theater_ratio, 2022, 0.3).
narrative_ontology:measurement(ai_s_tr_t2023, ai_safety_commitment__existential_risk_reading, theater_ratio, 2023, 0.36).
narrative_ontology:measurement(ai_s_tr_t2025, ai_safety_commitment__existential_risk_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2014, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2014, 0.44).
narrative_ontology:measurement(ai_s_be_t2016, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2016, 0.49).
narrative_ontology:measurement(ai_s_be_t2018, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2018, 0.52).
narrative_ontology:measurement(ai_s_be_t2020, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(ai_s_be_t2022, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(ai_s_be_t2023, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2023, 0.63).
narrative_ontology:measurement(ai_s_be_t2025, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2014, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2014, 0.3).
narrative_ontology:measurement(ai_s_su_t2016, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2016, 0.33).
narrative_ontology:measurement(ai_s_su_t2018, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2018, 0.36).
narrative_ontology:measurement(ai_s_su_t2020, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(ai_s_su_t2022, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2022, 0.42).
narrative_ontology:measurement(ai_s_su_t2023, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2023, 0.5).
narrative_ontology:measurement(ai_s_su_t2025, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'AI safety' covers structurally distinct claims with different victim sets, evidentiary standards, and epsilon distributions, so the kernel emits three stories. This reading (existential_risk_reading) holds upstream structural position: its dominance logic and resource capture shape the operating environment of the dual_priority_reading (which must continuously defend non-competition against triage pressure) and competes for the same funding and legitimacy pool as the near_term_harms_reading. Each file carries its own epsilon, beneficiaries, and victims; the contest between them is carried in omega variables, not averaged into any single classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
