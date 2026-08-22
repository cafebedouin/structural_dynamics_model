% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential-Risk Reading of AI Risk Prioritization
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   This story authors the existential-risk reading of the contested 'AI risk
 *   prioritization' kernel: the claim that misaligned AGI poses an
 *   extinction-level threat and that alignment research and capability
 *   controls are therefore the paramount allocation of scarce safety
 *   attention. This is one of two structurally distinct readings sharing the
 *   same surface label ('AI risk'); the sibling near_term_harms_reading (not
 *   authored in this file) holds that documented present-day algorithmic
 *   harms — discrimination, displacement, surveillance — are the paramount
 *   concern. The two readings differ in victim set (hypothetical future
 *   humanity vs. presently harmed populations), beneficiary set (x-risk
 *   institutions/longtermist funders vs. near-term justice researchers and
 *   affected communities), timescale (10-100 years vs. immediate), and in
 *   which harms are treated as a 'distraction' from the other. Per the
 *   ε-invariance principle these are authored as separate constraints linked
 *   by network.affects_constraints, each with its own stable ε assessed by
 *   that reading's own lights.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: agenda-setting beneficiary (institutional/arbitrage) — administers the framing and its funding pipelines
 *   - longtermist_funders: beneficiary (institutional/arbitrage) — directs capital toward the framing's preferred interventions
 *   - agi_capability_labs_with_safety_teams: beneficiary/co-agenda-setter (institutional/arbitrage) — gains reputational cover from the framing while continuing capability development
 *   - algorithmically_harmed_present_populations: payer (powerless/trapped) — bears present harms displaced from priority attention
 *   - near_term_ai_justice_researchers: payer (moderate/constrained) — loses funding and legitimacy contest under the framing
 *   - future_humanity_hypothetical: non-agent payer (powerless/trapped, universal scope) — invoked stakeholder that cannot corroborate or contest the claims made in its name
 *   - policymakers_and_regulators: observer (institutional/analytical) — allocates attention between competing framings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.58).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.52).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential-Risk Reading of AI Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '02103174-b391-4963-bbbb-b39f1ffe35c3').
narrative_ontology:cs_kernel_codification('02103174-b391-4963-bbbb-b39f1ffe35c3', distributed).
narrative_ontology:cs_authority_grounding('02103174-b391-4963-bbbb-b39f1ffe35c3', expertise).
narrative_ontology:cs_interpretation_layer_present('02103174-b391-4963-bbbb-b39f1ffe35c3').
narrative_ontology:cs_reading_relation('02103174-b391-4963-bbbb-b39f1ffe35c3', ai_risk_prioritization__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('02103174-b391-4963-bbbb-b39f1ffe35c3', foundational, catastrophic_irreversibility_dominates_expected_value).
narrative_ontology:cs_axiom_status(catastrophic_irreversibility_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('02103174-b391-4963-bbbb-b39f1ffe35c3', catastrophic_irreversibility_dominates_expected_value, instrumental).
narrative_ontology:cs_axiom('02103174-b391-4963-bbbb-b39f1ffe35c3', foundational, future_persons_carry_comparable_moral_weight_to_present_persons).
narrative_ontology:cs_axiom_status(future_persons_carry_comparable_moral_weight_to_present_persons, holdable).
narrative_ontology:cs_axiom_grounding('02103174-b391-4963-bbbb-b39f1ffe35c3', future_persons_carry_comparable_moral_weight_to_present_persons, deontological).
narrative_ontology:cs_reference_frame('02103174-b391-4963-bbbb-b39f1ffe35c3', precautionary_longtermist_risk_calculus).
narrative_ontology:cs_drift_state('02103174-b391-4963-bbbb-b39f1ffe35c3', post_frontier_model_scaling_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('02103174-b391-4963-bbbb-b39f1ffe35c3', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, agi_capability_labs_with_safety_teams).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, algorithmically_harmed_present_populations).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_justice_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity_hypothetical).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, alignment_tractability_thesis).
narrative_ontology:constraint_vindicates(ai_risk_prioritization__existential_risk_reading, agi_timeline_urgency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research agendas, publishes risk frameworks, and advises governments and labs on what counts as the priority AI risk. Administers grant pipelines and advisory seats that channel funding and policy attention toward alignment work. Collects prestige, funding, and institutional legitimacy from the existential framing being accepted as the correct lens.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, beneficiary).

% Directs philanthropic and venture capital toward alignment research and capability-control initiatives, consistent with a worldview that weights vast numbers of future lives heavily. Benefits from having their giving framework treated as the responsible, rigorous one; can redirect funds at will and faces little accountability for opportunity costs to near-term interventions.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    institutional, civilizational, arbitrage, global).

% Builds frontier AI systems while maintaining internal safety/alignment teams that both do genuine technical work and provide reputational cover for continued capability development. Benefits from a framing that treats the risk of their own products as a distant civilizational question requiring their continued involvement, rather than a present regulatory target.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, agi_capability_labs_with_safety_teams, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, agi_capability_labs_with_safety_teams, agenda_setter).

% Experience discriminatory credit scoring, biased hiring algorithms, predictive policing, and surveillance systems deployed today. Their harms are documented and remediable but compete for research funding, regulatory attention, and public concern against a framing that treats extinction risk as the paramount concern. They cannot exit the systems that harm them and have little say in how AI risk research priorities are set.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, algorithmically_harmed_present_populations, payer,
    powerless, immediate, trapped, national).

% Study algorithmic bias, labor displacement, and surveillance harms occurring now. Compete for a shrinking share of AI safety funding and public/media attention against the existential framing, and are sometimes characterized within the field as addressing a 'distraction' from the paramount threat. Can continue working but with less funding, less prestige, and less policy traction than x-risk-aligned peers.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_justice_researchers, payer,
    moderate, immediate, constrained, national).

% The nonexistent persons and future generations whose potential existence and welfare are invoked as the primary stakes of this reading. Cannot represent themselves, cannot corroborate the claims made on their behalf, and bear the notional cost if the existential threat is real and under-addressed — but the framing also uses their invoked interests to justify present resource diversion, which is itself a cost borne (speculatively) by them if the diversion is later shown to have been misallocated.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity_hypothetical, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_prioritization__existential_risk_reading, future_humanity_hypothetical).

% Receive competing briefings from x-risk institutions and near-term harms researchers, must allocate regulatory attention and legislative capacity between capability controls and present-harm remediation, and are lobbied by both camps as well as by capability labs seeking to shape whichever framing prevails.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policymakers_and_regulators, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scarce technical and policy attention on a class of catastrophic, hard-to-reverse failure modes (loss of control over advanced AI systems) that individual actors have weak incentive to address alone, and that could in principle preempt harms too large for after-the-fact correction.
% TRANSFER_FUNCTION: Moves research funding, top technical talent, regulatory bandwidth, and media/public attention away from documented present-day algorithmic harms and toward alignment research, capability-control mechanisms, and long-horizon safety institutions — a transfer justified by appeal to future stakes that cannot be empirically verified in the present.
% ABSENT_VOICES: Populations currently harmed by deployed algorithmic systems (denied credit, misidentified by facial recognition, displaced from jobs) rarely sit on the AI safety advisory boards or grant committees that set priority; their advocates are present in the field but structurally out-funded and out-platformed relative to x-risk institutions with closer ties to capability labs and major funders.
% DISAPPEARANCE_RATIONALE: X-risk institutions and longtermist funders would say the world becomes catastrophically more exposed to unrecoverable AGI failure if this prioritization vanished. Near-term harms researchers and affected populations would say resources currently diverted to speculative long-horizon work would flow to remediable, verifiable present harms, and that little of practical value would be lost. The dispute is not resolvable from either seat alone.
% FOUNDING_PROBLEM: Advanced AI capabilities were advancing faster than institutional understanding of how to keep highly capable, goal-directed systems reliably under human control, raising the prospect of a failure mode with no opportunity for correction after the fact.
% FOUNDING_PROBLEM_CORROBORATION: Some corroboration exists outside the direct beneficiary set: independent AI researchers not affiliated with x-risk institutions (e.g., some academic ML safety researchers) acknowledge misalignment and loss-of-control as genuine open technical problems. However, the CLAIM that this is the primary or paramount risk relative to present harms is corroborated almost entirely by the same institutions, funders, and labs that benefit from the framing being accepted; independent social-harm researchers and affected communities dispute the priority ranking, not necessarily the underlying technical possibility.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (rising from 0.32 over the interval) because the reading's operation increasingly diverts funding, talent, and regulatory bandwidth from remediable present harms toward speculative long-horizon work, and that diversion compounds as the framing institutionalizes (major labs, funders, and government advisory bodies adopt it as the default lens). Suppression is authored at 0.52: the framing does not use hard coercion, but it actively de-legitimizes near-term-harms work as a 'distraction,' which is a real if softer suppressive mechanism operating through funding committees, conference agendas, and media framing rather than through law or force. Theater ratio (0.38, rising) reflects that a nontrivial and growing share of institutional 'alignment' activity is reputational signaling by capability labs rather than technical work with a plausible path to reducing catastrophic risk — this is authored as a genuine but partial phenomenon, not the whole of the field. Accessibility collapse is moderate (0.42): the near-term harms framing remains visible and actively contested, not fully suppressed, so alternatives have not collapsed as completely as they would under a mountain-like claim. Resistance is moderate-high (0.55): near-term harms researchers, affected communities, and some AI ethics scholars actively contest the priority ranking, which is exactly the resistance pattern expected of a tangled rope rather than an uncontested mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of x_risk_research_institutions and longtermist_funders, this arrangement looks like responsible, high-stakes coordination on the most consequential possible failure mode humanity could face. From the seat of algorithmically_harmed_present_populations and near_term_ai_justice_researchers, the identical structure looks like an extraction of attention and resources from remediable present harms, justified by reference to a future that cannot corroborate or contest the claims made in its name. The engine computes these divergent seat-level classifications from the same structural data; this story does not adjudicate which seat is 'right' — it authors the existential-risk reading's own ε and structural facts, per the ε-invariance and kernel-reading discipline.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions, longtermist funders, and capability labs with safety teams are authored as beneficiaries: the framing directs funding, prestige, and regulatory deference toward them, and their exit options are arbitrage-grade — they can move between institutions, jurisdictions, and funding sources at will, which the derivation chain correctly reads as low d (beneficiary end). Algorithmically harmed present populations and near-term AI justice researchers are authored as payers: harmed populations are trapped (cannot exit the algorithmic systems that harm them) and researchers are constrained (can continue working but under funding and legitimacy disadvantage), both of which push d toward the target end. Future_humanity_hypothetical is authored as agent:false and payer — it is a non-agent entity invoked in the reading's justificatory structure, included for narrative completeness but excluded from directionality computation per the schema's agent-hood gate, since it cannot itself collect, contest, or corroborate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (loss of control over highly capable AI systems) is authored as status: contested rather than flatly dead or live, because the underlying technical concern is not obviously resolved, but the CLAIM that it is paramount relative to present, verified harms is corroborated almost entirely by the same institutions that benefit from the prioritization holding. This is the diagnostic case the R5 genealogy interview is built to catch: a founding problem whose 'still live' status is asserted mainly from inside the beneficiary set. The tangled_rope classification (rather than snare) is chosen because a genuine coordination function is present — catastrophic, hard-to-reverse AI failure is a real category that individual labs have weak incentive to internalize alone — but the framing's operation as authored simultaneously imposes an asymmetric cost on present-harm researchers and populations through attention and funding diversion, which the mandatrophy analysis is designed to surface rather than let default to either a pure-coordination (rope) or pure-extraction (snare) read.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_probability_estimate_contestation,
    'Is the probability of extinction-level AGI misalignment within the relevant timeframe (10-100 years) high enough to justify the resource diversion this reading produces, or is the probability estimate itself an artifact of a self-selected research community with career and funding incentives to assert high risk?',
    'Independent, adversarially-structured elicitation of AGI risk probability estimates from technical AI researchers with no funding ties to x-risk institutions, compared against elicitation from the x-risk community itself, tracked over multiple forecasting cycles.',
    'If independent estimates converge with x-risk community estimates, the coordination function is strongly vindicated and the reading''s ε should be revised downward. If independent estimates diverge substantially lower, the reading''s extraction component is better evidenced and ε should be revised upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_probability_estimate_contestation, empirical, 'Whether the extinction-probability premise is independently corroborated or self-referential to the beneficiary community.').

omega_variable(
    genuine_vs_constructed_kernel_contest,
    'Is the choice between existential and near-term framings a genuine irreducible values disagreement about how to weight speculative future harm against certain present harm, or is the existential framing partly a constructed narrative that serves institutional and funding interests independent of its evidentiary merit?',
    'Trace funding flows and institutional formation history: examine whether x-risk framing adoption correlates more strongly with technical risk assessment updates or with funder preference and lab reputational needs.',
    'If institutional/funding correlation dominates over technical-assessment correlation, this supports authoring higher extractiveness and treating the framing as closer to a false-summit pattern; if technical correlation dominates, the coordination function is more clearly genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_constructed_kernel_contest, conceptual, 'Whether the existential framing tracks technical risk evidence or institutional interest.').

omega_variable(
    future_humanity_representation_problem,
    'Can any present institution legitimately claim to represent the interests of nonexistent future persons, and if not, what does that do to the legitimacy of resource claims made in their name?',
    'Philosophical and institutional analysis of proxy representation claims; comparison to other domains (e.g., environmental law''s treatment of future generations) where similar representation problems have been adjudicated.',
    'If proxy representation is judged illegitimate or unverifiable, the reading''s beneficiary structure looks more like extraction dressed in altruistic language; if judged a workable convention, the coordination framing is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_humanity_representation_problem, preference, 'Whether claims made on behalf of nonexistent future persons can ground legitimate present resource allocation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_prioritization__existential_risk_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_prioritization__existential_risk_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__existential_risk_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_prioritization__existential_risk_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__existential_risk_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, agi_capability_race_dynamics).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_safety_funding_allocation).

% DUAL FORMULATION NOTE:
% This constraint and near_term_harms_reading are two readings of the same kernel (ai_risk_prioritization), NOT the same constraint viewed from two angles. They differ in victim set (hypothetical future humanity vs. presently harmed populations), beneficiary set (x-risk institutions/longtermist funders vs. near-term justice researchers), timescale (10-100 years vs. immediate), and each treats the other's priority as a 'distraction.' Per the ε-invariance principle each reading is authored as a separate constraint with its own stable ε, its own stakeholders, and its own classification, linked here via affects_constraints rather than merged into one hedged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
