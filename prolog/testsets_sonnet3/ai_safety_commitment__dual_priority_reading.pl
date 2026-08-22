% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: AI Safety as Dual Priority: Existential Risk and Near-Term Harms as Non-Competing Commitments
 *   domain: AI Safety / Technology Governance / Risk Assessment
 *
 * SUMMARY:
 *   This story instantiates the 'dual priority' reading of the contested AI
 *   safety kernel: the claim that existential risk and near-term harms are
 *   non-competing priorities that AI safety must address jointly. This
 *   reading is structurally distinct from the existential_risk_reading (which
 *   treats extinction-level misalignment as the dominant stake) and the
 *   near_term_harms_reading (which treats documented present-day
 *   discriminatory and exploitative harms as the core mandate). The
 *   dual-priority reading's coordination function is real — it prevents a
 *   costly public schism between two research communities that both need
 *   institutional legitimacy and funding — but its extraction function is
 *   also real: under genuine resource scarcity, the claim that both
 *   priorities are 'non-competing' obscures an actual, ongoing allocation
 *   fight, and the party controlling the allocation (frontier labs and safety
 *   institutes) benefits from never having to make that fight explicit. As
 *   required by the ε-invariance principle, this file does not average across
 *   the sibling readings' ε values; it authors a single ε for the
 *   dual-priority arrangement as its own proponents and critics would assess
 *   the standing arrangement.
 *
 * KEY AGENTS:
 *   - frontier_ai_labs: primary agenda-setter and structural beneficiary (institutional/arbitrage) — controls allocation while claiming both priorities are served
 *   - ai_safety_institutes: secondary beneficiary/agenda-setter (institutional/constrained) — uses dual framing to claim broad jurisdiction and funding
 *   - algorithmically_harmed_users: primary payer (powerless/trapped) — bears opportunity cost when resources tilt toward existential-risk work
 *   - near_term_harm_researchers and long_termist_researchers: both payers (moderate/constrained) — each camp's work is diluted by forced resource-sharing with the other
 *   - excluded_affected_communities: absent voice — directly harmed populations not present in the framing's construction or allocation debates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.52).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.38).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "AI Safety as Dual Priority: Existential Risk and Near-Term Harms as Non-Competing Commitments").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "AI Safety / Technology Governance / Risk Assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '73447419-5ef0-4887-8643-e82cd0ce989a').
narrative_ontology:cs_kernel_codification('73447419-5ef0-4887-8643-e82cd0ce989a', distributed).
narrative_ontology:cs_authority_grounding('73447419-5ef0-4887-8643-e82cd0ce989a', distributed).
narrative_ontology:cs_reading_relation('73447419-5ef0-4887-8643-e82cd0ce989a', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('73447419-5ef0-4887-8643-e82cd0ce989a', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('73447419-5ef0-4887-8643-e82cd0ce989a', foundational, priorities_are_jointly_addressable_without_ranking).
narrative_ontology:cs_axiom_status(priorities_are_jointly_addressable_without_ranking, holdable).
narrative_ontology:cs_axiom_grounding('73447419-5ef0-4887-8643-e82cd0ce989a', priorities_are_jointly_addressable_without_ranking, instrumental).
narrative_ontology:cs_axiom('73447419-5ef0-4887-8643-e82cd0ce989a', secondary, resource_scarcity_does_not_force_explicit_tradeoff).
narrative_ontology:cs_axiom_status(resource_scarcity_does_not_force_explicit_tradeoff, holdable).
narrative_ontology:cs_axiom_grounding('73447419-5ef0-4887-8643-e82cd0ce989a', resource_scarcity_does_not_force_explicit_tradeoff, empirically_contingent).
narrative_ontology:cs_reference_frame('73447419-5ef0-4887-8643-e82cd0ce989a', unified_safety_field_founding_consensus).
narrative_ontology:cs_drift_state('73447419-5ef0-4887-8643-e82cd0ce989a', post_2023_capability_acceleration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73447419-5ef0-4887-8643-e82cd0ce989a', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, frontier_ai_labs).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_institutes).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, dual_priority_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, algorithmically_harmed_users).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harm_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, long_termist_researchers).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, safety_is_a_unified_field).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund and publicize both existential-risk research divisions and near-term harm/fairness teams under one 'safety' banner. Sets internal budget allocation between the two intervention types and controls the public narrative that they are complementary rather than competing. Benefits from the umbrella framing because it defuses pressure from either camp to prioritize the other, and lets the lab claim comprehensive responsibility while allocating resources as it sees fit.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, frontier_ai_labs, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, frontier_ai_labs, beneficiary).

% Government and quasi-government bodies that adopt the dual-priority framing to justify broad mandates covering both catastrophic risk assessment and consumer-protection style harm auditing. The framing lets them claim jurisdiction over the entire AI safety space and secure funding from multiple political constituencies simultaneously; exit would mean picking a narrower, more contestable mandate.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_institutes, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, ai_safety_institutes, agenda_setter).

% Researchers and institutes whose careers and grant portfolios are built on the claim that existential and near-term work are mutually reinforcing. They benefit from the coexistence claim directly: it legitimizes their bridging role, secures funding from both constituencies, and shields them from having to defend a single prioritization under scrutiny.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, dual_priority_researchers, beneficiary,
    moderate, biographical, mobile, global).

% People currently experiencing biased hiring algorithms, discriminatory content moderation, exploitative content-labeling labor, or misinformation amplification. Under the dual-priority framing their documented, present harms compete for the same finite safety budget and policy attention as speculative extinction scenarios; when institutes allocate resources toward existential-risk work, these users bear the opportunity cost as unaddressed, ongoing harm with no exit from the systems that harm them.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, algorithmically_harmed_users, payer,
    powerless, immediate, trapped, global).

% Researchers focused on measurable, present-day algorithmic harms who must now compete for funding, conference space, and institutional attention against existential-risk work under a shared 'safety' umbrella. The dual-priority framing formally elevates their work's status while, in practice, funding and staffing decisions frequently favor existential-risk framing when labs and institutes make discretionary allocation calls — leaving this group under-resourced relative to the framing's promise.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harm_researchers, payer,
    moderate, biographical, constrained, global).

% Researchers who believe extinction-level risk from misaligned superintelligence is the dominant moral priority given stakes and irreversibility. The dual-priority framing forces them to share scarce policy attention and lab resources with harms they see as orders of magnitude less consequential, diluting what they view as the only stakes that matter under time pressure they consider severe.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, long_termist_researchers, payer,
    moderate, civilizational, constrained, global).

% Legislators and regulators drafting AI governance frameworks who must decide, under the dual-priority claim, how to allocate regulatory attention and enforcement budgets between catastrophic-risk oversight and consumer-protection style harm mitigation. They receive competing testimony from both camps, each claiming the dual-priority framing supports their preferred allocation.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% Communities directly harmed by deployed systems (wrongful arrests from facial recognition, denied loans from biased credit models, displaced gig workers) who are rarely consulted in the safety-priority debates conducted primarily among researchers, lab staff, and policymakers. They would argue the dual-priority framing is an abstraction that delays concrete remedies they need now, but they are not seated at the table where resource allocation is decided.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, excluded_affected_communities, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, frontier_ai_labs).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real prioritization-avoidance problem: rather than forcing labs, funders, and policymakers to choose one safety framing and defend it against the other camp's objections, the dual-priority claim lets a single 'AI safety' umbrella hold funding, staff, and legitimacy for both existential-risk and near-term-harm work without an explicit, contestable ranking.
% TRANSFER_FUNCTION: Moves scarce safety budget, research attention, regulatory bandwidth, and public legitimacy between two claimant populations — existential-risk researchers and near-term-harm researchers/affected users — through discretionary allocation decisions made by labs and institutes that publicly claim both priorities are equally served.
% ABSENT_VOICES: Communities currently harmed by deployed algorithmic systems are almost never present in the rooms where the dual-priority framing is debated or where resulting budgets are set; their felt urgency is structurally different from the abstract, comparative-stakes debate conducted by researchers on both sides.
% DISAPPEARANCE_RATIONALE: If the dual-priority claim were abandoned overnight, labs and institutes would be forced to explicitly rank existential risk against near-term harms in budget and policy documents. This would likely trigger open conflict between the two research communities, force policymakers into explicit tradeoff votes, and could either concentrate resources sharply toward whichever priority wins the argument or fragment the field into openly competing factions — either way, the current arrangement of shared legitimacy and ambiguous allocation would not persist.
% FOUNDING_PROBLEM: As AI capabilities advanced rapidly circa 2020-2023, the field needed a way to hold together researchers alarmed by long-horizon catastrophic risk and researchers documenting immediate discriminatory and exploitative harms from deployed systems, without the two camps publicly undermining each other's legitimacy or splitting funding pools that both needed to grow.
% FOUNDING_PROBLEM_CORROBORATION: Frontier labs and dual-priority researchers attest the coexistence is genuine and mutually reinforcing, citing shared technical foundations (interpretability, evaluation methods) that serve both agendas. Independent policy analysts and several near-term-harm researchers, writing outside lab-funded venues, attest that in practice resource allocation systematically favors existential-risk framing when labs face public scrutiny or investor pressure, and that affected-community advocates were not consulted in the framing's construction — this corroboration comes from academic critiques and investigative journalism outside the benefiting institutions, not from the labs or institutes themselves.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the coordination benefit (avoiding an open, damaging priority war) is real and partially offsets the extraction that occurs when discretionary allocation systematically favors whichever priority best serves institutional legitimacy and funding optics at a given moment. Theater ratio is notably high and rising (0.30 to 0.58) because an increasing share of 'we address both' public commitments (joint safety teams, unified framing documents, conference programming that formally seats both camps) functions more as legitimacy-signaling than as evidence of genuinely balanced resource commitment — the substance of allocation decisions has not kept pace with the rhetoric of coexistence. Suppression is comparatively low and slow-rising (0.22 to 0.38) because this reading does not suppress either camp's existence or publication; it suppresses the explicit articulation of a ranking between them, which is a softer but real form of constraint on discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontier labs and safety institutes sit near the beneficiary end: they set the agenda, control the framing, and benefit from never being forced to publicly rank the two priorities against each other, which would expose them to attack from whichever camp loses. Algorithmically harmed users sit at the full-target end: trapped, powerless, and bearing the opportunity cost of any resource tilt toward existential-risk work, with no voice in how that tilt occurs. Near-term-harm researchers and long-termist researchers are both payers under this reading, structurally symmetric in one sense (both share scarce resources) but distinguished by time horizon — the near-term researchers' immediate, documentable-harm claims compete against the long-termist researchers' civilizational-stakes claims within the same shared pool, and the dual-priority framing's promise of non-competition papers over the fact that both groups experience real resource dilution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing to hold a fracturing field together during a period of rapid capability growth and funding competition — was genuinely live circa 2020-2023. Whether it remains live is contested: dual-priority proponents argue ongoing capability advances keep both stakes simultaneously real and rising; critics argue the framing has calcified into an allocation-avoidance mechanism that lets institutions claim comprehensive responsibility while making no binding commitment to either camp, particularly to the near-term-harm side whose victims are identifiable now rather than speculative. The tangled_rope classification captures this precisely: coordination function (preventing schism, enabling shared technical infrastructure like interpretability research that serves both agendas) coexists with asymmetric extraction (discretionary allocation power concentrated in labs/institutes, extraction from currently-harmed and under-resourced populations) requiring active enforcement (continuous public reaffirmation that both priorities are equally served, which is the theater-ratio-rising pattern observed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coexistence_vs_allocation_avoidance,
    'Is the dual-priority claim a structurally accurate description of a field where both risk types are genuinely addressable without tradeoff, or is it a legitimacy-preserving mechanism that avoids an actual, unavoidable resource allocation decision under scarcity?',
    'Track actual budget and staffing allocation ratios between existential-risk and near-term-harm teams within major labs and institutes over time, and compare against public statements of parity; a persistent, growing gap between stated parity and actual allocation would support the avoidance-mechanism reading.',
    'If allocation avoidance is confirmed, the coordination function is substantially cover for extraction (support snare/tangled_rope re-weighting toward higher extraction); if allocation tracks stated parity closely, the coordination function is closer to genuine and extraction is lower than currently authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coexistence_vs_allocation_avoidance, empirical, 'Whether dual-priority framing reflects real balanced practice or masks systematic allocation favoring one camp.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the disagreement between the three kernel readings live: is it a genuine disagreement about relative moral stakes (existential vs. near-term harm), or a disagreement about the correct time horizon for evaluating AI risk, or a disagreement about who counts as a legitimate current victim versus a statistical/future one?',
    'Structured elicitation of proponents from each reading, isolating whether their disagreement resolves once time horizon and population definitions are fixed, or persists as an irreducible values disagreement about stakes.',
    'If the disagreement is primarily about time horizon and population definition, the three readings could in principle converge on an allocation formula; if it is an irreducible values disagreement about stakes, the tangled_rope structure of this dual reading is a permanent feature, not a transitional one, and mandatrophy_resolved should likely be marked false indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the structural source of disagreement among the kernel''s sibling readings.').

omega_variable(
    resource_scarcity_severity,
    'Is the safety-research resource pool actually scarce enough that existential-risk and near-term-harm work meaningfully compete, or is the pool large and growing fast enough that both can be funded without zero-sum tradeoff?',
    'Analysis of total AI safety funding growth rates versus the marginal cost of adequately staffing both intervention types; if funding growth outpaces combined need, the competing-priorities framing understates genuine non-rivalry.',
    'If resources are not meaningfully scarce, the extraction this story authors is overstated and the dual-priority claim is closer to a genuine rope; if scarcity is severe and binding, the extraction is understated and the arrangement is closer to a snare on whichever population loses the allocation fight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_scarcity_severity, empirical, 'Whether the resource-scarcity premise underlying the tangled_rope classification actually holds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t4, ai_safety_commitment__dual_priority_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(ai_s_tr_t8, ai_safety_commitment__dual_priority_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__dual_priority_reading, theater_ratio, 12, 0.47).
narrative_ontology:measurement(ai_s_tr_t16, ai_safety_commitment__dual_priority_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__dual_priority_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(ai_s_tr_t24, ai_safety_commitment__dual_priority_reading, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(ai_s_be_t4, ai_safety_commitment__dual_priority_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(ai_s_be_t8, ai_safety_commitment__dual_priority_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__dual_priority_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(ai_s_be_t16, ai_safety_commitment__dual_priority_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__dual_priority_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(ai_s_be_t24, ai_safety_commitment__dual_priority_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(ai_s_su_t4, ai_safety_commitment__dual_priority_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(ai_s_su_t8, ai_safety_commitment__dual_priority_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__dual_priority_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(ai_s_su_t16, ai_safety_commitment__dual_priority_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__dual_priority_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(ai_s_su_t24, ai_safety_commitment__dual_priority_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is the dual_priority_reading member of the ai_safety_commitment kernel family (3 stories). existential_risk_reading treats extinction-level misalignment as the singular stake with its own victim set (future/statistical populations, civilization-scale). near_term_harms_reading treats documented present-day algorithmic harms as the singular mandate with its own victim set (currently affected users). This story's distinguishing structural feature is the union of both victim populations plus a resource-allocation coherence problem neither sibling reading faces in the same form, since each sibling treats its priority as non-competing by definition (there is only one priority to fund). ε differs across all three: this reading's ε (0.52) sits between what would be authored for a purely coordination-framed rope and a purely extraction-framed snare, reflecting the genuine-but-incomplete coordination function of holding two camps together under one legitimacy umbrella while allocation remains contested and non-transparent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
