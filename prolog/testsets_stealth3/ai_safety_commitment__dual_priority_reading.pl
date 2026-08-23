% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Dual-Priority Commitment in AI Safety (Non-Competing Harms Reading)
 *   domain: technological/governance
 *
 * SUMMARY:
 *   Since the mid-2010s, AI safety institutions have operated under a shared
 *   commitment that existential-scale risk and present-day harms from
 *   deployed systems must both be addressed and that neither may be
 *   subordinated to the other. The commitment holds a single field together
 *   across two research traditions with divergent time horizons, methods, and
 *   vocabularies. This story instantiates ONE reading of that commitment —
 *   the dual_priority_reading — as a clean, epsilon-invariant constraint over
 *   the standing arrangement it governs: the dual-mandate allocation regime
 *   itself. Epsilon's referent is that standing regime as this reading's own
 *   lights appraise it: the reading affirms both mandates in principle while
 *   acknowledging that under scarcity the non-competing premise strains, and
 *   that the strain is currently managed by not adjudicating it. The sibling
 *   readings (existential_risk_reading, near_term_harms_reading) are separate
 *   constraint files linked through the network block; their structural
 *   deltas differ — each refuses half of this reading's victim set in
 *   opposite directions. Claim and metrics are authored independently: the
 *   claimed type reflects the hybrid structure I believe is true (real
 *   coalition coordination carrying real distributive asymmetry), and the
 *   metrics describe the regime's actual operation without being tuned to any
 *   predicted verdict.
 *
 * KEY AGENTS:
 *   - ai_safety_funders: Primary agenda-setter (powerful/arbitrage) — controls the split between streams and is structurally shielded from ever publishing its rationale
 *   - safety_field_institutions: Administrative enforcer (institutional/identity_locked) — administers what counts as safety work; their broker position has become who they are
 *   - alignment_research_community: Protected beneficiary (organized/constrained) — holds a guaranteed share; cannot advocate concentration without marking itself a traitor to the field
 *   - applied_harms_research_community: Protected beneficiary (organized/constrained) — holds the complementary share under the same advocacy bound
 *   - near_term_harm_affected_populations: Primary target (powerless/trapped) — bears diluted mitigation today and has no seat where the split is decided
 *   - frontier_deployment_exposed_public: Primary target (powerless/trapped) — bears diluted protective effort against frontier externalities; diffuse and unorganized
 *   - policy_bodies_regulating_ai: Incidental beneficiary (institutional/mobile) — signals comprehensive coverage without ranking
 *   - grassroots_harm_organizers: Excluded voice (moderate/constrained) — would press for documented-harm priority but lacks standing in governance forums
 *   - independent_field_ethnographers: Analytical observer (analytical/analytical) — documents the gap between stated commitment and budget behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.45).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.55).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "Dual-Priority Commitment in AI Safety (Non-Competing Harms Reading)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technological/governance").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, '162e2014-d72f-4d50-a350-e34081718db5').
narrative_ontology:cs_kernel_codification('162e2014-d72f-4d50-a350-e34081718db5', distributed).
narrative_ontology:cs_authority_grounding('162e2014-d72f-4d50-a350-e34081718db5', distributed).
narrative_ontology:cs_reading_relation('162e2014-d72f-4d50-a350-e34081718db5', ai_safety_commitment__existential_risk_reading, influences).
narrative_ontology:cs_reading_relation('162e2014-d72f-4d50-a350-e34081718db5', ai_safety_commitment__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('162e2014-d72f-4d50-a350-e34081718db5', foundational, joint_obligation_to_present_and_future_bearers).
narrative_ontology:cs_axiom_status(joint_obligation_to_present_and_future_bearers, holdable).
narrative_ontology:cs_axiom_grounding('162e2014-d72f-4d50-a350-e34081718db5', joint_obligation_to_present_and_future_bearers, deontological).
narrative_ontology:cs_axiom('162e2014-d72f-4d50-a350-e34081718db5', secondary, hedged_portfolio_under_deep_uncertainty).
narrative_ontology:cs_axiom_status(hedged_portfolio_under_deep_uncertainty, holdable).
narrative_ontology:cs_axiom_grounding('162e2014-d72f-4d50-a350-e34081718db5', hedged_portfolio_under_deep_uncertainty, instrumental).
narrative_ontology:cs_reference_frame('162e2014-d72f-4d50-a350-e34081718db5', unified_dual_mandate_field).
narrative_ontology:cs_drift_state('162e2014-d72f-4d50-a350-e34081718db5', contemporary_funding_tightening, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('162e2014-d72f-4d50-a350-e34081718db5', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, alignment_research_community).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, applied_harms_research_community).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, ai_safety_funders).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, safety_field_institutions).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, policy_bodies_regulating_ai).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, near_term_harm_affected_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, frontier_deployment_exposed_public).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, moral_pluralism_in_risk_prioritization).
narrative_ontology:constraint_vindicates(ai_safety_commitment__dual_priority_reading, portfolio_hedging_under_uncertainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Philanthropic foundations and strategic grantmakers split their portfolios between long-horizon alignment research and applied-harm mitigation. The shared commitment lets them fund both streams without publishing an explicit ranking; when budgets contract they adjust quietly rather than adjudicate publicly. They answer to boards and donor communities rather than to either research camp, and they can rebalance across portfolios faster than either camp can reconstitute itself.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_safety_funders, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, ai_safety_funders, beneficiary).

% Safety institutes, laboratory safety teams, and flagship venues administer the field's shared agenda: workshop themes, review criteria, and what counts as safety research. Their standing rests on representing the whole field to funders and governments; recasting themselves as partisan to one stream would dissolve the broker position they have become. Absorbing criticism from both flanks is a permanent operating cost they cannot shed without changing what the organization is.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, safety_field_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, safety_field_institutions, beneficiary).

% Researchers working on long-horizon catastrophic risk from advanced systems. They receive a protected share of funding, venue space, and talent under the shared commitment. Pressing openly for concentrating resources on their stream would mark them as abandoning the other half of the field, so they advocate within bounds; leaving the frame entirely would mean founding separate institutions and surrendering access to the pooled funding and legitimacy the unified field provides.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, alignment_research_community, beneficiary,
    organized, generational, constrained, global).

% Researchers documenting and mitigating present-day harms: biased screening systems, discriminatory model outputs, displaced workers, misinformation at scale. They receive the complementary share of the pool. Pushing for full dedication to deployed-system harms would read as dismissing existential stakes, so they likewise advocate within bounds; their career capital and publication venues are invested in the unified field's categories.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, applied_harms_research_community, beneficiary,
    organized, biographical, constrained, global).

% National and supranational regulators adopt frameworks claiming coverage of both catastrophic and present-day risks. The shared commitment lets them signal comprehensiveness without ranking the two, and they can shift emphasis between risk classes across legislative cycles as political salience moves. They collect the coverage signal while bearing implementation burden and criticism from both directions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_bodies_regulating_ai, beneficiary,
    institutional, generational, mobile, continental).

% People subject to algorithmic credit scoring, automated hiring filters, wrongful content moderation, and workplace automation today. Mitigation of their harms competes for the same pool of money and attention as long-horizon programs, and the official denial that the two compete means their advocates must share the podium and the budget rather than press the full case. They hold no seat where the split is decided and cannot opt out of the systems affecting them.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harm_affected_populations, payer,
    powerless, immediate, trapped, global).

% The broad public exposed to frontier-model externalities: misinformation at scale, autonomous-system accidents, and the tail possibility of losing control of advanced systems. Protective effort on their behalf is one of the two streams sharing the pool, so their safeguarding arrives diluted. They are diffuse, unorganized, and cannot exit an AI-saturated information and infrastructure environment; their interest is voiced by proxy and contested by the other stream's proxies.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, frontier_deployment_exposed_public, payer,
    powerless, generational, trapped, global).

% Community organizers working directly with people harmed by deployed systems. They lack standing in AI safety governance spaces, which route participation through credentialed research institutions. They would press for prioritizing documented, verifiable harms over speculative programs, and their exclusion from the rooms where portfolios are set is part of how the unified framing holds.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, grassroots_harm_organizers, excluded,
    moderate, immediate, constrained, national).

% Science-and-technology-studies scholars and meta-researchers studying how the field allocates attention and money. They take no side between the streams, publish analyses of the gap between stated commitments and budget behavior, and supply much of the outside corroboration for the field's own internal disputes.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, independent_field_ethnographers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, ai_safety_funders).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a single AI safety coalition together across two research traditions with divergent time horizons, methods, and vocabularies; preserves funding breadth and option value under uncertainty about which harm class will dominate; prevents open internecine resource conflict that would erode funder and government confidence in the field.
% TRANSFER_FUNCTION: Moves research funding, venue space, policy attention, and junior talent between long-horizon alignment work and deployed-harm mitigation according to an implicit split that the non-competing commitment exempts from explicit defense; the practical effect is that each at-risk population receives partial rather than dedicated protective effort, while the allocator seats are spared publishing a ranking.
% ABSENT_VOICES: People directly harmed by deployed systems appear only through researcher proxies; grassroots organizers have no standing seat in governance forums. Future people exposed to tail risk are necessarily unrepresented. Researchers who want to rank priorities openly are present but sanctioned when they speak, so the conversation's apparent unanimity partly reflects who was admitted and what they are permitted to say.
% DISAPPEARANCE_RATIONALE: Overnight removal would force the two camps into open budgetary contest: funders would have to publish allocation rationales or pick sides, joint venues would split or renegotiate their charters, and both at-risk populations would see protective effort re-concentrate toward whichever camp prevails — with a real chance that total field funding shrinks during the fight, leaving both populations worse off during the transition.
% FOUNDING_PROBLEM: In the mid-2010s the field was splitting: a long-horizon alignment tradition and a deployed-harms tradition were building separate venues, vocabularies, and funding channels, and mutual dismissal between them threatened the field's credibility with funders and governments just as commercial AI investment surged.
% FOUNDING_PROBLEM_CORROBORATION: Independent science-and-technology-studies scholarship and the meta-research literature document the fragmentation history; governmental science-advisory assessments note that a splintered safety field complicates regulatory engagement; dissident wings inside both camps attest the underlying tension remains unresolved even while disputing this reading's solution. Corroboration exists from outside the benefiting parties.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).
:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at interval end) because the regime's distributive effect is dilution rather than confiscation: each at-risk population receives partial protective effort where its dedicated reading would deliver concentrated effort, and the gap is never defended because the frame declares the tradeoff nonexistent. Suppression (0.55) is a raw structural property and is deliberately NOT scaled by power or scope — it measures the normative enforcement apparatus itself: review norms, funding gatekeeping, and the social cost of framing violation. Theater (0.38) tracks the widening gap between commitment statements and actual budget behavior. Accessibility_collapse is low-moderate (0.4) because the alternatives — the two dedicated readings — remain live positions held by real factions; the kernel structure guarantees they persist, so the frame marginalizes rather than eliminates them. Resistance (0.6) is sustained and bidirectional: both flanks periodically revolt, x-risk maximalists against distraction, harms advocates against speculative crowding-out. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the frame began as an aspirational statement and matured into administered machinery (program criteria, portfolio review conventions, public commitment letters), a hardening a static scalar would miss. All three metric series run on one shared seven-point grid so no row borrows an end-state value from another metric's timeline; the 2026 endpoints are marked projected.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From the harm-affected publics' position the regime is diluted protection plus an unspeakable grievance: their mitigation visibly competes with speculative programs, but the competition is officially denied, so they cannot even lodge the complaint in the field's own terms. From the researcher communities' position the same regime is belonging and a protected funding floor — the frame is what makes them one field instead of two rival sects. From the funders' and institutions' position it is decision-shielding: the hardest question in the field (how much for which) is structurally exempted from public answer. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real subsidies: both researcher communities hold protected shares (derived d near the beneficiary end — left at derivation). Victim declarations map to the union population the expected structural delta specifies: present-day harm sufferers and the frontier-exposed public, both trapped and unorganized (derived d near the full-target end — left at derivation). Two overrides correct derivations that would misread the agenda-setting seats. The funders are listed among beneficiaries because they genuinely collect the avoidance benefit, but the derivation would place them near-full beneficiary (d ~0.1); they actually sit nearer symmetric (override 0.30) because every budget contraction forces them to choose silently and they absorb criticism from both flanks. The institutional seats (field institutions, policy bodies) likewise collect legitimacy but carry enforcement labor and two-front exposure (override 0.36 rather than derived near-beneficiary). Without these overrides the engine would read the regime's administrators as pure subsidized riders, understating the coherence burden that is the regime's central operating cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the regime as pure coordination would erase the distributive asymmetry: two populations receive systematically diluted protection while the allocation question goes undefended, and that asymmetry is load-bearing, not incidental. Reading it as pure extraction would erase the genuine coalition function and generate a false remedial prediction — dissolving the frame overnight would not return dedicated protection to either population; it would trigger open resource war between the camps with a real probability of shrinking total protective effort for both. The tangled_rope claim preserves both halves and directs scrutiny at the seam: the coordination is real, the refusal to adjudicate is the extraction vector, and the two must be evaluated together. The founding problem (field fragmentation threatening credibility) remains live, so no mandatrophy resolution is declared; the regime has not outlived its mandate, it has under-delivered on it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates the dual_priority_reading of the ai_safety_commitment kernel; how would either sibling reading restructure the arrangement?',
    'Adopting existential_risk_reading collapses the protected set to tail-risk-exposed populations and concentrates allocation on long-horizon alignment work; adopting near_term_harms_reading restricts it to documented present-day sufferers. Each sibling is a separate constraint file authoring its own victim set and epsilon over the same standing arrangement.',
    'The victim set, the allocation rule, and every per-seat classification change wholesale under a sibling reading; the numbers in this file are valid only for the dual reading and must not be averaged across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three coexisting readings of the ai_safety_commitment kernel.').

omega_variable(
    scarcity_coherence_challenge,
    'Does the non-competing premise survive binding resource scarcity, or does the frame become a covert zero-sum ranking once budgets tighten?',
    'Observe allocation behavior across funding contractions: if cuts fall disproportionately, and without published rationale, on one stream, the non-competing claim is operating as cover for an implicit ranking.',
    'If scarcity forces implicit ranking, effective extraction on whichever stream''s population is disfavored rises sharply and the arrangement drifts toward pure extraction from that seat''s vantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scarcity_coherence_challenge, empirical, 'Whether the dual mandate is allocatively coherent under scarcity or masks an unstated ranking.').

omega_variable(
    synergy_vs_coalition_construct,
    'Is the two-stream portfolio joined by genuine technical synergy (shared evaluation infrastructure, interpretability serving both streams) or primarily by coalition maintenance?',
    'Audit cross-stream dependency: measure how much of each stream''s output the other actually consumes. High mutual consumption supports a substantive joint basis; thin consumption indicates the joint mandate is a constructed coalition norm.',
    'Genuine synergy pushes the arrangement toward pure coordination with modest inherent cost; thin synergy leaves the non-competing claim carrying distributive consequences it cannot justify on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synergy_vs_coalition_construct, empirical, 'Naturalness of the joint mandate: discovered synergy versus constructed coalition norm.').

omega_variable(
    suppression_internalization_in_ranking_discourse,
    'Is the silence around priority-ranking maintained by external gatekeeping (review norms, funding signals, venue politics) or internalized (researchers pre-filtering arguments they no longer register as sayable)?',
    'Compare publication and grant-seeking behavior of researchers before and after moving between institutions with different frame intensity; persistence of self-restriction after leaving high-enforcement environments indicates internalization.',
    'Internalized suppression outlasts the gatekeeping that produced it and would persist even if funders relaxed the frame, meaning true suppression exceeds the structural measure and the omega resolves the mechanism split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_ranking_discourse, empirical, 'Structural versus internalized enforcement of the non-competing norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2014, ai_safety_commitment__dual_priority_reading, theater_ratio, 2014, 0.14).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t2014, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2016, ai_safety_commitment__dual_priority_reading, theater_ratio, 2016, 0.17).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t2016, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2018, ai_safety_commitment__dual_priority_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t2018, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2020, ai_safety_commitment__dual_priority_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t2020, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2022, ai_safety_commitment__dual_priority_reading, theater_ratio, 2022, 0.3).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t2022, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2024, ai_safety_commitment__dual_priority_reading, theater_ratio, 2024, 0.34).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t2024, observed).
narrative_ontology:measurement(ai_safety_dual_priority_tr_t2026, ai_safety_commitment__dual_priority_reading, theater_ratio, 2026, 0.38).
narrative_ontology:measurement_basis(ai_safety_dual_priority_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(ai_safety_dual_priority_be_t2014, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2014, 0.28).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t2014, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2016, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2016, 0.31).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t2016, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2018, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2018, 0.34).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t2018, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2020, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t2020, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2022, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2022, 0.4).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t2022, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2024, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2024, 0.43).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t2024, observed).
narrative_ontology:measurement(ai_safety_dual_priority_be_t2026, ai_safety_commitment__dual_priority_reading, base_extractiveness, 2026, 0.45).
narrative_ontology:measurement_basis(ai_safety_dual_priority_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_safety_dual_priority_su_t2014, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2014, 0.22).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t2014, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2016, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2016, 0.27).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t2016, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2018, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2018, 0.33).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t2018, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2020, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2020, 0.39).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t2020, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2022, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2022, 0.45).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t2022, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2024, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2024, 0.5).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t2024, observed).
narrative_ontology:measurement(ai_safety_dual_priority_su_t2026, ai_safety_commitment__dual_priority_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(ai_safety_dual_priority_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'AI safety' decomposes into three reading-files of the ai_safety_commitment kernel because the label conflates structurally distinct commitments with different victim sets and different epsilon values. This file (dual_priority_reading) is the umbrella reading and sits upstream of both dedicated siblings: it enables them (each camp draws its protected share through the dual frame) and constrains them (neither may claim the whole field). Each file carries its own stable epsilon over the same standing arrangement; no file hedges across readings. The dedicated readings are expected to author narrower victim sets and correspondingly different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, powerful, 0.3).
constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, institutional, 0.36).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
