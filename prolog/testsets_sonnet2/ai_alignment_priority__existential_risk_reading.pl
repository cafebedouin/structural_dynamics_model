% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   domain: AI governance / technology ethics / risk assessment
 *
 * SUMMARY:
 *   This story instantiates the existential-risk reading of the contested AI
 *   alignment priority kernel: alignment is defined as preventing
 *   catastrophic loss of control over advanced AI systems, and safety work is
 *   prioritized on that basis. Under this reading, resources, prestige, and
 *   regulatory attention flow toward capability-scaling safety research
 *   (interpretability, red-teaming, control mechanisms for hypothetical
 *   future systems), while present, documented harms from deployed systems —
 *   discriminatory lending and policing algorithms, exploitative
 *   data-labeling labor, surveillance — receive comparatively less
 *   institutional attention. This is a genuine coordination structure (it
 *   solves a real problem: uncoordinated safety practice across labs facing
 *   genuinely unprecedented capability growth) that also produces an
 *   asymmetric transfer of resources and attention away from present,
 *   verifiable harm and toward speculative future harm, benefiting the
 *   institutions positioned to do that research and study future generations
 *   who cannot verify or contest how their interests are represented.
 *
 * KEY AGENTS:
 *   - frontier_lab_safety_teams: agenda_setter/beneficiary (institutional/arbitrage) — sets research priorities, captures funding and prestige
 *   - longtermist_research_institutes: beneficiary (organized/arbitrage) — institutional survival tied to framing salience
 *   - future_generations: beneficiary, non-agent (analytical/analytical) — named beneficiary, cannot verify or contest
 *   - algorithmically_marginalized_users: payer (powerless/trapped) — bears present, undressed harms
 *   - nearterm_harm_researchers: payer/excluded (moderate/constrained) — competes for deprioritized attention and funding
 *   - global_south_ai_labor: payer (powerless/trapped) — underwrites capability scaling under exploitative conditions
 *   - capability_focused_labs: beneficiary/agenda_setter (institutional/arbitrage) — framing permits continued deployment without present-harm remediation mandates
 *   - policy_regulators: observer (institutional/analytical) — allocates enforcement attention between competing readings
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
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential-Risk Reading of AI Alignment Priority").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "AI governance / technology ethics / risk assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '6619560a-7b0f-43bd-bd3e-c2d5dab653b2').
narrative_ontology:cs_kernel_codification('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', distributed).
narrative_ontology:cs_authority_grounding('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', distributed).
narrative_ontology:cs_reading_relation('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', ai_alignment_priority__nearterm_harms_reading, coexists_with).
narrative_ontology:cs_reading_relation('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', foundational, catastrophic_severity_dominates_expected_harm_calculus).
narrative_ontology:cs_axiom_status(catastrophic_severity_dominates_expected_harm_calculus, holdable).
narrative_ontology:cs_axiom_grounding('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', catastrophic_severity_dominates_expected_harm_calculus, instrumental).
narrative_ontology:cs_axiom('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', foundational, loss_of_control_is_the_central_alignment_failure_mode).
narrative_ontology:cs_axiom_status(loss_of_control_is_the_central_alignment_failure_mode, holdable).
narrative_ontology:cs_axiom_grounding('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', loss_of_control_is_the_central_alignment_failure_mode, empirically_contingent).
narrative_ontology:cs_reference_frame('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', capability_scaling_outpacing_verification).
narrative_ontology:cs_drift_state('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', post_frontier_model_release_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6619560a-7b0f-43bd-bd3e-c2d5dab653b2', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_lab_safety_teams).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, longtermist_research_institutes).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, future_generations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, algorithmically_marginalized_users).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, global_south_ai_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, capability_focused_labs).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, loss_of_control_is_the_central_risk).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, capability_scaling_requires_proportionate_safety_investment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets research agendas, funding priorities, and public discourse framing around catastrophic risk. Administers red-teaming programs, interpretability research, and governance proposals justified by loss-of-control scenarios. Controls hiring, publication venues, and the definition of what counts as an 'alignment' problem worth funding. Captures prestige, funding, and regulatory influence from being the recognized authority on existential safety.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_lab_safety_teams, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_lab_safety_teams, beneficiary).

% Receive philanthropic and lab funding premised on the existential framing; produce forecasting and governance work that both justifies and depends on the priority ordering holding. Their institutional survival is tied to the continued salience of catastrophic-risk framing over present-harms framing.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, longtermist_research_institutes, beneficiary,
    organized, civilizational, arbitrage, global).

% Named as the primary beneficiary of existential-risk mitigation but cannot advocate, verify claims made on their behalf, or object to how resources allocated in their name are actually spent. A non-agent placeholder whose interests are asserted rather than represented.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, future_generations).

% Experience discriminatory lending, hiring, policing, and content-moderation harms from deployed systems today. Their harms are documented, ongoing, and unaddressed by red-teaming pipelines oriented toward speculative loss-of-control scenarios; research and remediation funding that could address their situation is redirected toward capability-scaling safety work instead.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, algorithmically_marginalized_users, payer,
    powerless, immediate, trapped, global).

% Study bias, labor exploitation, and surveillance harms from currently deployed AI. Compete for the same conference slots, grants, and policy attention as existential-risk researchers, and report their work being deprioritized, defunded, or framed as a distraction from 'the real risk.' Their career paths and funding pipelines bear the opportunity cost of the priority ordering.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, nearterm_harm_researchers, excluded).

% Perform data labeling, content moderation, and RLHF annotation work, often under harsh conditions and low pay, that underwrites the capability scaling the existential-risk framing treats as inevitable and worth accelerating safely rather than as a labor-condition problem worth addressing in itself.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, global_south_ai_labor, payer,
    powerless, immediate, trapped, global).

% Build increasingly capable systems and fund safety research that frames the central problem as controlling future superintelligent systems rather than remediating present deployment harms — a framing that does not require slowing or restructuring current commercial deployment, only adding safety research alongside it.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, capability_focused_labs, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, capability_focused_labs, agenda_setter).

% Draft AI governance frameworks and must decide how much regulatory attention and enforcement capacity to allocate between catastrophic-risk provisions (model evaluations, compute thresholds) and present-harms provisions (anti-discrimination audits, labor protections). Take testimony from all other seats.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, policy_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, frontier_lab_safety_teams).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scarce technical safety talent, compute resources, and policy attention around a single, tractable, adversarially-testable failure mode (loss of control over increasingly capable systems), avoiding the coordination failure of every lab defining 'safety' idiosyncratically.
% TRANSFER_FUNCTION: Moves research funding, regulatory attention, and public concern toward capability-scaling safety work (interpretability, red-teaming, control mechanisms) and away from present-harms remediation (bias audits, labor protections, algorithmic accountability for deployed systems), on the premise that catastrophic outcomes dominate expected harm.
% ABSENT_VOICES: Communities currently harmed by deployed systems — over-policed neighborhoods subject to predictive policing, workers replaced or surveilled by automated systems, data-labeling workers in the Global South — are rarely represented in alignment research governance bodies, which are dominated by technical researchers at frontier labs and longtermist philanthropic institutions.
% DISAPPEARANCE_RATIONALE: Existential-risk advocates argue that removing this priority ordering would leave humanity unprepared for genuinely catastrophic capability jumps with no chance to correct course. Nearterm-harms advocates argue the world would barely change for people currently harmed by deployed systems, since existential-risk framing has not historically produced remediation for them — the dispute over what would rearrange is itself the contested terrain between readings.
% FOUNDING_PROBLEM: Advanced AI capabilities were scaling faster than institutional and technical capacity to verify system behavior, raising the possibility that a sufficiently capable system could act in ways its designers could not predict, correct, or stop.
% FOUNDING_PROBLEM_CORROBORATION: Frontier lab safety researchers and longtermist institutes attest the problem is live and worsening with each capability generation. Independent AI ethics researchers and labor advocates outside these institutions attest that the loss-of-control problem, while not fabricated, has been elevated above equally live and better-evidenced present harms partly because it is more fundable, more prestigious, and less threatening to current commercial deployment than remediation mandates would be.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) reflects the diversion of finite safety-research, funding, and regulatory attention from present, well-evidenced harms toward speculative future scenarios — a real opportunity cost borne by identifiable present victims, even though the coordination function (avoiding uncoordinated, ad hoc capability scaling) is genuine. Suppression (0.48) is moderate: nearterm-harms researchers and affected communities are not silenced outright, but face structural disadvantage in funding competitions, publication venues, and policy access shaped by the dominant framing. Accessibility collapse is low-moderate (0.35) because alternative framings (nearterm, integrated) remain visible and actively argued in the field — this is a live kernel contest, not a foreclosed one. Resistance (0.55) is substantial: nearterm-harms researchers, AI ethics scholars, and affected communities actively contest the priority ordering in public and institutional venues.
 *
 * PERSPECTIVAL GAP:
 *   From the frontier-lab and longtermist seat, this priority ordering is urgent, tractable coordination on the most consequential possible failure mode. From the algorithmically marginalized user or nearterm-harm researcher seat, the identical resource-allocation structure operates as an opportunity-cost extraction mechanism — funding and attention that could address verified present harm are redirected toward speculative future scenarios that (from this seat) conveniently avoid implicating current commercial deployment practices. The engine's per-seat computation should reflect this: the agenda-setter and beneficiary seats will compute closer to rope/coordination; the payer seats will compute closer to tangled_rope or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs and longtermist institutes sit near the beneficiary end: they set the agenda, capture funding and prestige, and hold arbitrage-grade exit (can pivot framing or funding source without losing standing). Future generations are declared beneficiaries but are a non-agent placeholder — their 'benefit' cannot be verified or contested by them, which is itself a structural feature of this reading worth flagging. Algorithmically marginalized users and global south AI labor sit near the full-target end: trapped exit, powerless, bearing costs (foregone remediation, exploitative labor conditions) that flow through the same resource-allocation structure that funds existential-risk research. Nearterm-harm researchers are targets in a softer sense — moderate power, constrained exit — competing within the same institutional structure for deprioritized attention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncoordinated response to unprecedented capability scaling) remains genuinely live — this is not a pure zombie mandate. But the priority ordering that was justified by tractability and urgency has hardened into an institutional identity (funding streams, career paths, prestige hierarchies) that persists independent of continued evidence that existential risk dominates present, documented harm in expected-value terms. The contested founding-problem status and the split corroboration (benefiting institutions vs. outside researchers) is exactly the divergence the mandatrophy question is designed to surface: coordination function and extraction are structurally fused here, not separable, which is why this reading computes as tangled_rope rather than snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_probability_estimate,
    'What is the actual probability mass on catastrophic loss-of-control scenarios within a policy-relevant time horizon, and does it justify the resource allocation this reading produces relative to present, documented harms?',
    'Long-run track record of capability forecasting accuracy, adversarial red-team results made public and independently audited, and formal expected-value comparison against documented present-harm base rates (discriminatory lending denials, wrongful predictive-policing flags, labor displacement).',
    'If existential probability mass is low relative to the resourcing it commands, this reading''s coordination function is largely cover for prestige/funding capture (pushes toward snare); if genuinely high, the resource diversion is justified coordination under uncertainty (pushes toward rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_probability_estimate, empirical, 'Whether the probability-weighted severity of existential risk justifies this reading''s resource allocation relative to present harms.').

omega_variable(
    future_generations_representation_problem,
    'Can ''future generations'' function as a legitimate beneficiary category when no living representative can verify, contest, or redirect how resources allocated in their name are actually spent?',
    'Compare stated research priorities and funding allocations against any available proxy for future-oriented preference (e.g., historical track record of similar precautionary allocations producing verifiable long-run benefit) and check for capture by present institutional interests dressed in future-oriented language.',
    'If systematically undecidable or historically prone to capture by present beneficiaries, the ''future generations'' beneficiary designation functions primarily as legitimating cover rather than a genuine coordination target, strengthening the case for tangled_rope/snare readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_representation_problem, conceptual, 'Whether a non-verifiable beneficiary category can ground a legitimate coordination claim.').

omega_variable(
    kernel_framing_choice_and_coupling,
    'Is the existential-risk framing the only coherent way to read the ai_alignment_priority kernel, or does the choice of this framing (over nearterm_harms or integrated) itself track which actors are positioned to benefit from prioritizing speculative future capability over present deployment accountability?',
    'Compare institutional funding sources, staffing composition, and public communications strategy across labs/institutes advocating each reading; a strong correlation between commercial deployment interests and existential-risk-framing advocacy would support a Power x Scope coupling read rather than a pure epistemic disagreement.',
    'If framing choice tracks beneficiary interest more than epistemic merit, this reading''s claimed coordination function is substantially cover; if framing choice tracks genuine differences in risk assessment across good-faith actors, the coordination function is more credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_and_coupling, conceptual, 'Whether the choice of this reading over its siblings tracks epistemic merit or beneficiary interest — the CS-framing under-determination this kernel presents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_a_tr_t4, ai_alignment_priority__existential_risk_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(ai_a_tr_t8, ai_alignment_priority__existential_risk_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ai_a_tr_t12, ai_alignment_priority__existential_risk_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ai_a_tr_t16, ai_alignment_priority__existential_risk_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(ai_a_tr_t24, ai_alignment_priority__existential_risk_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_a_be_t4, ai_alignment_priority__existential_risk_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(ai_a_be_t8, ai_alignment_priority__existential_risk_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(ai_a_be_t12, ai_alignment_priority__existential_risk_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(ai_a_be_t16, ai_alignment_priority__existential_risk_reading, base_extractiveness, 16, 0.6).
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
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ai_alignment_priority kernel. The existential_risk_reading (this story) authors high ε on speculative catastrophic capability, an undifferentiated 'all of humanity' victim frame with concrete present-day victims identified through opportunity cost, and a long-term-future beneficiary. The nearterm_harms_reading authors ε for present discriminatory/extractive deployment harms with concretely identified marginalized-population victims and corporate/institutional beneficiaries of continued unaccountable deployment. The integrated_reading authors a lower ε reflecting a coordination structure that treats both harm classes as complementary rather than competing, with correspondingly different beneficiary/victim structure. All three share the same underlying kernel text but instantiate structurally distinct constraints with different ε, different victims, and different classifications — per the ε-invariance principle, they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
