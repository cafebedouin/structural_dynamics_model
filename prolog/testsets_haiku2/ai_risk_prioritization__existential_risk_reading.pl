% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: Existential AI Risk Prioritization (X-Risk Reading)
 *   domain: technology/governance/risk_assessment
 *
 * SUMMARY:
 *   The existential AI risk reading claims that misaligned artificial general
 *   intelligence (AGI) poses an extinction-level threat to humanity, and that
 *   alignment research is the paramount global priority. This reading
 *   institutionalizes a particular moral and epistemic frame: existential
 *   risk dominates the moral weight; uncertainty about tail scenarios demands
 *   precautionary resource allocation; the problem is fundamentally a
 *   technical research problem requiring decades of foundational work before
 *   AGI deployment. The reading shapes research agendas, funding flows, and
 *   policy priorities across AI governance. It is opposed by a near-term
 *   harms reading, which claims that current deployed AI systems cause
 *   measurable discrimination, worker displacement, and surveillance harms
 *   now, and that justice interventions are paramount. Both readings invoke
 *   the same domain—AI risk—but define 'the problem' differently, victimize
 *   different groups, and demand different resource transfers. This story
 *   instantiates ONLY the existential-risk reading as a clean ε-invariant
 *   constraint, following the kernel-reading discipline: one reading, one ε,
 *   one beneficiary/victim structure, no hedging.
 *
 * KEY AGENTS:
 *   - xrisk_research_institutions: Define the problem, set research agendas, receive concentrated funding and prestige (beneficiary + agenda_setter)
 *   - longtermist_funders: Control billions in philanthropic capital, allocate to x-risk priorities (beneficiary, powerful)
 *   - near_term_harms_advocates: Compete for same resources and policy attention, deprioritized as distracted from existential stakes (payer, moderate power)
 *   - marginalized_communities_subject_to_current_ai_systems: Present injury from deployed systems, excluded from x-risk strategy, reframed as lower priority (payer, powerless)
 *   - future_generations: Claimed beneficiaries of x-risk work, but nonexistent and identity-locked to researcher claims (payer, powerless, identity-locked)
 *   - capability_developers_racing_to_agi: Excluded from agenda-setting, treated as biased, unable to contest extinction narratives (excluded, institutional)
 *   - xrisk_research_skeptics: Questioned on empirical and mechanistic grounds, marginalized as uncertainty-mongering (excluded, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.72).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "Existential AI Risk Prioritization (X-Risk Reading)").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "technology/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '7cfc3c44-6ef7-4fb1-8fec-b4d41602c316').
narrative_ontology:cs_kernel_codification('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', distributed).
narrative_ontology:cs_authority_grounding('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', extraction).
narrative_ontology:cs_interpretation_layer_present('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316').
narrative_ontology:cs_reading_relation('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', ai_risk_prioritization__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', foundational, existential_risk_dominates_moral_weight).
narrative_ontology:cs_axiom_status(existential_risk_dominates_moral_weight, holdable).
narrative_ontology:cs_axiom_grounding('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', existential_risk_dominates_moral_weight, deontological).
narrative_ontology:cs_axiom('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', foundational, misaligned_agi_extinction_scenario_empirically_plausible).
narrative_ontology:cs_axiom_status(misaligned_agi_extinction_scenario_empirically_plausible, holdable).
narrative_ontology:cs_axiom_grounding('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', misaligned_agi_extinction_scenario_empirically_plausible, empirically_contingent).
narrative_ontology:cs_reference_frame('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', precautionary_alignment_obligation).
narrative_ontology:cs_drift_state('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', contemporary_ai_capabilities_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cfc3c44-6ef7-4fb1-8fec-b4d41602c316', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, marginalized_communities_subject_to_current_ai_systems).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, ai_safety_field_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations like MIRI, FHI, CHAI, and their research arms receive substantial funding and prestige within AI governance and philanthropic circles specifically because they adopt the existential-risk framing. They define the problem, set research agendas, and translate funding into research priorities. Their institutional health depends on the existential-risk narrative maintaining salience and funding.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions, agenda_setter).

% Major foundations (Open Philanthropy, Future of Humanity Institute donors, EA funders) allocate billions to longtermist causes, weighted heavily toward existential-risk mitigation. The existential-risk reading justifies their resource allocation strategy and shapes how they evaluate impact across all their grantmaking.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, mobile, global).

% Researchers, advocates, and policymakers focused on algorithmic discrimination, worker displacement, surveillance systems, and immediate harms from deployed AI. They compete for the same funding, policy attention, and institutional legitimacy as x-risk work, but the existential-risk framing systematically deprioritizes their concerns as near-term compared to civilizational stakes.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_harms_advocates, payer,
    moderate, biographical, constrained, global).

% Communities experiencing algorithmic bias in lending, hiring, criminal justice, healthcare, and benefit allocation systems. Their injuries are present and measurable but systematically redefined as a lower priority than existential x-risk. They have no seat at x-risk strategy tables; their advocacy for near-term justice is framed as a distraction from civilizational risk.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, marginalized_communities_subject_to_current_ai_systems, payer,
    powerless, biographical, trapped, global).

% Nonexistent persons whose existence and welfare depend on the choices made by current AGI developers and safety researchers. They cannot advocate, negotiate, or exit. The existential-risk reading claims to protect them, but the mechanism—deferring near-term justice to enable faster AI capabilities development or AGI race acceleration—is contested. Their interests are authoritatively spoken for by researchers they cannot cross-examine.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_generations, payer,
    powerless, civilizational, identity_locked, universal).

% AI companies pursuing AGI/frontier capabilities. They are structurally excluded from the existential-risk research agenda even though they are the primary agents whose behavior the agenda aims to influence. They would argue the existential-risk framing overestimates tail risk, underestimates the value of safety-through-capability, and creates perverse incentives for speed over carefulness. Their voice is treated as biased by their profit motive.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, capability_developers_racing_to_agi, excluded,
    institutional, biographical, trapped, global).

% Researchers and philosophers who doubt the empirical basis for extinction-level AGI scenarios or the mechanistic plausibility of the alignment problem as framed. They publish alternative models, but the existential-risk framing has achieved sufficient institutional dominance that their objections are sidelined as uncertainty-mongering or neglect of the precautionary principle.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, xrisk_research_skeptics, excluded,
    analytical, biographical, analytical, global).

% The academic and organizational infrastructure of AI safety research itself—journals, conferences, research groups, career paths—is constituted by and dependent upon the existential-risk framing. Career advancement, publication venues, funding eligibility, and institutional status are all indexed to how one's work positions relative to existential scenarios. The infrastructure would need to restructure if the reading were substantially challenged.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_safety_field_infrastructure, beneficiary,
    institutional, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, xrisk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes global AI research and governance infrastructure to develop AGI alignment techniques, capability controls, and international AGI governance frameworks before transformative AGI systems emerge. Solves a collective-action problem: competitive firms racing to AGI have incentives to cut corners on safety; nations have incentives to accelerate capability development; researchers have incentives to publish valuable insights that competitors can use. The existential-stakes framing coordinates around shared extinction risk, overriding competitive incentives.
% TRANSFER_FUNCTION: Redirects research talent, computational resources, and philanthropic capital from near-term algorithmic auditing, worker protection, and AI accountability work toward foundational alignment research, AGI safety governance, and long-term risk mitigation. Moves billions in longtermist philanthropic funding toward x-risk research institutions. Moves policy attention from deployed-system regulation toward AGI governance frameworks.
% ABSENT_VOICES: Capability developers (excluded as biased by profit motive, lack seat in x-risk strategy bodies), near-term harms advocates (marginalized as short-termist), researchers skeptical of extinction scenarios (dismissed as underweighting tail risk or ignoring precaution), affected communities (not seated in x-risk research, their present injuries reframed as lower priority), and future generations (absent by definition, spoken for only by x-risk researchers).
% DISAPPEARANCE_RATIONALE: If the existential-risk reading disappeared and the near-term-harms reading achieved institutional dominance: funding would shift to algorithmic auditing, anti-discrimination regulation, worker transition support, and AI transparency in deployed systems. X-risk research would contract sharply. AGI capability development would accelerate, unconstrained by safety-first institutional pressure. The near-term reading claims this world reduces present suffering. The x-risk reading claims this world increases extinction risk by removing alignment checks. The world rearranges, but its safety profile is contested.
% FOUNDING_PROBLEM: The possibility that artificial general intelligence (AGI), if developed without robust alignment between system objectives and human values, could pursue goals that lead to human extinction or permanent value destruction. The founding problem is: how do we develop AGI safely, given that misalignment is possible and the consequences are irreversible?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: a subset of AI researchers who model AGI scenarios (Nick Bostrom, Stuart Russell, Eliezer Yudkowsky); some academic alignment researchers who have published on scalable oversight and value learning; longtermist philosophers and effective altruists. NOT corroborated by: most AI practitioners and deployment researchers, who find extinction scenarios speculative; most near-term AI ethics and safety researchers, whose threat models center on present harms; AI skeptics and philosophers who question the empirical basis for fast takeoff scenarios; affected communities, whose observations center on current algorithmic discrimination; capability developers, who argue the extinction scenario overestimates risk and underestimates safety-through-capability. The corroboration base is narrow, insider to x-risk research circles, and does not include independent epistemic authority from practitioners, skeptics, or affected communities.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, contested).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68 at interval end) reflects that the reading systematically directs resources away from near-term interventions toward x-risk research; the mechanism is not coercion but moral reframing—what counts as 'actually important' shifts. Suppression (0.72) is high because maintaining the reading's dominance requires actively excluding and delegitimizing competing problem framings (near-term harms, skeptical positions); the suppression is primarily discursive (dismissing alternatives as distracted, short-termist, underweighting tail risk) rather than legal. Theater ratio (0.41, rising over time) indicates growing performative maintenance: as the extinction scenario remains unverified and AGI continues not to arrive within predicted windows, more institutional energy goes to rhetorical defense of the reading's authority than to empirical grounding. The measurement series shows extractiveness rising from 0.42 to 0.68 over 50 years: the reading becomes increasingly entrenched in funding, hiring, and policy. Theater ratio plateaus at 0.41, indicating a stable ratio of real work (alignment research) to theatrical justification, not a sharp turn toward pure performance. All measurements share one time grid: t ∈ {0, 5, 10, 20, 30, 50}. Basis is 'projected' for t ∈ {0, 5, 50} (counterfactual or future), 'observed' for t ∈ {10, 20, 30} (historical data on funding flows, research output, institutional entrenchment). The interval endpoint (t=50) represents the approximate point at which AGI arrival predictions in the core literature were supposed to become relevant (2070s); metrics are projected flat or slightly rising, reflecting institutional inertia and continued funding dominance despite prediction-miss.
 *
 * PERSPECTIVAL GAP:
 *   From the x-risk research seat, this is genuine coordination: mobilizing global cooperation on AGI safety against competitive incentives is a hard coordination problem, and the extinction-stakes framing solves it. Researchers see themselves as building essential knowledge. From the payer seats (near-term advocates, marginalized communities), this is extraction: the reading justifies redirecting resources from present injury to speculative future risk, and the exclusion of alternative problem-definitions prevents any democratic arbitration of priorities. From the analytical seat, this is a tangled rope: real coordination function (global AGI governance is a genuine problem) wound with asymmetric extraction (near-term harms deprioritized, communities silenced) and active suppression (alternative framings delegitimized). The engine computes each seat's perception from the structural data; the authored claim does not decide it.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions sit as beneficiaries (d ≈ 0.1): they collect funding, prestige, and research authority specifically because they adopt the existential-risk reading. Their power is institutional and their exit is arbitrage—they can shift to different technical domains but not out of research-institution structure. Longtermist funders sit as beneficiaries (d ≈ 0.15): they deploy capital according to the reading's logic and their influence over resource allocation is amplified by the reading. Near-term harms advocates sit as payers (d ≈ 0.75): they lose funding and policy attention to x-risk work; their power is moderate and their exit is constrained—they cannot stop doing near-term harm research even if it receives less support. Marginalized communities sit as payers (d ≈ 0.95): their harms are present and measurable but the reading redefines them as lower moral weight; they are powerless and identity-locked to the systems harming them (cannot exit AI deployment). Future generations sit as payers (d ≈ 0.90): they are claimed as beneficiaries of x-risk work but are nonexistent and identity-locked to researchers' speech acts about their interests. Capability developers sit as excluded (d ≈ 0.70): they would benefit from capability-first development unconstrained by safety-first institutional pressure, but are systematically excluded from agenda-setting and treated as biased. X-risk skeptics sit as excluded (d ≈ 0.65): their alternative models lose salience and funding, but they retain some analytical standing to publish dissent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is: misaligned AGI poses extinction risk. The founding-problem status is contested—some in the field believe the scenario remains live; others believe it was overstated and has gradually become zombified by institutional inertia. The disappearance verdict is contested—if the x-risk reading disappeared, would arrangements rearrange or would the same research simply reorganize under a different framing? The classification as tangled_rope rests on: (1) beneficiaries (x-risk institutions, longtermist funders) explicitly identified and extracting concentrated benefit; (2) victims (near-term advocates, marginalized communities) bearing deprioritization cost; (3) active suppression (discursive delegitimization of alternatives). A piton is possible if the founding problem dies—if AGI predictions continue to miss and the field shifts to performative defense of x-risk relevance without genuine coordination function. The theater ratio (0.41) is not yet high enough to call it piton, but the rising trajectory and institutional entrenchment despite non-arrival of AGI (prediction-miss visible by 2045) suggest drift toward piton structure. An omega variable addresses this: if the founding problem dies, does the constraint invert to pure institutional inertia with diffuse cost and no capturer (piton topology), or does it restructure into a new reading (near-term harms becomes dominant)? The answer determines whether mandatrophy has been resolved or merely deferred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agi_timeline_uncertainty,
    'When, if ever, will transformative AGI systems emerge? What is the empirical basis for the extinction-risk scenario''s timeline assumptions?',
    'Historical calibration of AI capabilities progress against predictions; survey of AI researcher forecasts; longitudinal tracking of capability milestones. If AGI does not emerge within the predicted windows (e.g., by 2050-2075), what does continued funding on unchanged assumptions indicate about the reading''s empirical grounding?',
    'If AGI timeline is dramatically later than assumed, the present urgency of x-risk work is overstated and the reading shifts toward piton topology (inertial maintenance of an atrophied function). If AGI arrives early, the reading''s claims about preparation time compress, potentially triggering rapid restructuring of AI governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agi_timeline_uncertainty, empirical, 'Whether the empirical timeline assumptions of the existential-risk reading hold, or whether prediction-miss indicates institutional drift.').

omega_variable(
    alignment_problem_mechanistic_closure,
    'Is the alignment problem (ensuring AGI values align with human values) mechanistically well-defined enough for research progress, or is it underspecified in ways that make ''solving'' it inherently uncertain?',
    'Longitudinal tracking of alignment research output and its integration into capability development; survey of AI practitioners on whether alignment techniques are actionable in training pipelines; empirical trials of alignment mechanisms under realistic competitive pressure.',
    'If alignment research remains speculative and unintegrated into practice, the reading''s claim that ''research is paramount'' loses its empirical mooring and becomes purely institutional commitment. This would support reclassification toward piton or indicate that the actual constraint is ''maintain x-risk research institutions'' rather than ''solve AGI alignment.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_problem_mechanistic_closure, empirical, 'Whether the foundational alignment problem has been mechanistically solved or remains in the research phase.').

omega_variable(
    moral_weight_of_future_existence,
    'What moral weight should be assigned to the existence and welfare of future, nonexistent generations relative to present, measurable harms? Does existential-risk prioritization rest on a contestable moral framework that cannot be adjudicated on empirical grounds?',
    'Philosophical analysis of longtermist ethics; survey of moral philosophers across traditions on how to weight present vs. future harms; empirical study of how communities affected by present AI harms view the trade-off.',
    'If the moral weight assigned to existential risk is revealed as reading-dependent rather than discovered as fact, the constraint''s claim to prioritization becomes transparent as a values choice, not a technical conclusion. This would reframe the constraint as preference-based rather than evidence-based and potentially destabilize the suppression of near-term alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_weight_of_future_existence, preference, 'Whether x-risk prioritization rests on a universal moral framework or on a contestable reading of what counts as risk.').

omega_variable(
    structural_alignment_between_xrisk_research_and_safety_in_practice,
    'Does the knowledge produced by x-risk research institutions (alignment techniques, safety frameworks, governance models) actually transfer into practice at AI capability companies, or does it circulate primarily within academic and philosophy circles while capability development proceeds unconstrained?',
    'Empirical audit of alignment research adoption at major AI companies; tracking of safety research citations and implementation in production systems; interviews with practitioners about what research they actually use.',
    'If x-risk research remains academically isolated and unintegrated into deployed systems, the constraint''s claim to solve the ''real problem'' fails and the function inverts: instead of protecting humanity from AGI risk, the reading operates primarily to extract resources and prestige for research institutions while capability development accelerates independently. This is a suppression-dependent mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_alignment_between_xrisk_research_and_safety_in_practice, empirical, 'Whether x-risk research effectively influences AGI safety in practice or circulates primarily as institutional discourse.').

omega_variable(
    kernel_reading_contested_boundary,
    'Is the contestation between the existential-risk reading and the near-term-harms reading a genuine logical foreclousure (one reading''s core premise rules out the other), a coexistence of incompatible positions held by different parties, or a causal influence (one reading''s validity changes the operating conditions for the other)?',
    'Philosophical analysis of the logical relationship between the readings'' core axioms; empirical study of whether parties holding one reading explicitly reject the other or rather assign it lower priority; historical analysis of whether these readings emerged from a common kernel or represent distinct problem framings.',
    'If the readings foreclose each other, one will eventually dominate and the other will be suppressed; the constraint will resolve toward one type (snare if x-risk dominates, the inverse if near-term dominates). If they coexist, both readings will persist as competing problem framings, and the constraint will remain tangled_rope indefinitely. If they influence each other, the constraint''s topology will evolve as the readings'' relative institutional power shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contested_boundary, conceptual, 'The logical structure of the kernel reading contest: whether readings foreclose, coexist, or influence.').

omega_variable(
    suppression_mechanism_discursive_vs_structural,
    'Is the suppression of near-term harms reading primarily a result of discursive delegitimation (near-term work framed as short-termist and naive) that could be reversed by rhetorical counter-framing, or is it structurally entrenched (funding institutions controlled by x-risk believers, career paths rewarding x-risk research, institutional infrastructure built on x-risk premises)?',
    'Institutional audit of funding sources and allocation patterns; analysis of hiring and promotion practices in AI safety research; tracking of whether near-term advocates can gain institutional standing without adopting x-risk framing; historical study of whether rhetorical challenges to x-risk dominance have shifted funding or prestige.',
    'If suppression is primarily discursive, it is reversible through effective counter-messaging and epistemic challenges. If it is structurally entrenched, reversing it would require institutional restructuring (new funding bodies, new career paths, new research agendas), not just better arguments. This determines whether the constraint''s suppression is a symptom or a mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_discursive_vs_structural, empirical, 'Whether the suppression of near-term harms reading is primarily discursive (rhetorically vulnerable) or structurally entrenched (institutionally stable).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_prioritization__existential_risk_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_prioritization__existential_risk_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_prioritization__existential_risk_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ai_r_tr_t30, ai_risk_prioritization__existential_risk_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(ai_r_tr_t50, ai_risk_prioritization__existential_risk_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(ai_r_be_t30, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(ai_r_be_t50, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ai_r_su_t30, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(ai_r_su_t50, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_prioritization__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The ai_risk_prioritization kernel instantiates two structurally distinct constraints via incompatible readings. The existential-risk reading (this file) defines the problem as misaligned AGI posing extinction-level threat; the near-term-harms reading (sibling file) defines the problem as present discrimination and displacement from deployed systems. Both readings invoke 'AI risk' but differ on: victim set (future humanity vs. present communities), timescale (10-100 years vs. now), resource flows (x-risk research vs. near-term regulation/remediation), and suppression mechanisms (delegitimation of near-term work vs. marginalization of x-risk as speculative). The readings coexist in public discourse held by different institutional factions. The ε values differ substantially: existential-risk reading extracts primarily through resource redirection and suppression of alternatives (ε=0.68); near-term-harms reading extracts primarily through exclusion and deprioritization of present injury (ε would be similarly high, measuring from the near-term reading's seat). Neither reading should hedge across both; each is a clean, ε-invariant constraint. The network edge signals that the readings influence each other: as one gains institutional dominance, it creates downstream pressure on the other's legitimacy and funding access. A future analysis integrating both readings would track how effective articulation of the near-term reading affects the existential-risk reading's suppression requirement and theater ratio.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_prioritization__existential_risk_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
