% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Existential Risk Alignment Priority
 *   domain: technological/philosophical
 *
 * SUMMARY:
 *   The existential risk reading of AI alignment priority structures the
 *   entire field's resource allocation, research agenda, and governance
 *   imagination around preventing catastrophic loss of control over advanced
 *   AI systems. It presents itself as the only coherent response to the
 *   stakes (astronomical value at risk) and the technical problem (alignment
 *   is hard, capabilities generalize, misalignment is the default). This
 *   reading captures the 'alignment' label and directs billions toward
 *   capability-adjacent safety work (interpretability, scalable oversight,
 *   evals, red-teaming) while treating present harms as distractions or
 *   lower-order concerns. The constraint operates as a tangled rope: it
 *   genuinely coordinates massive resources toward a real coordination
 *   problem (how to align systems smarter than us) while asymmetrically
 *   extracting from present-harm mitigation, marginalized communities, and
 *   alternative research paradigms — all of which are suppressed by the
 *   framing's dominance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.42).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "Existential Risk Alignment Priority").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "technological/philosophical").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '64c32556-249c-4bf3-86bf-19823767eb9a').
narrative_ontology:cs_kernel_codification('64c32556-249c-4bf3-86bf-19823767eb9a', distributed).
narrative_ontology:cs_authority_grounding('64c32556-249c-4bf3-86bf-19823767eb9a', distributed).
narrative_ontology:cs_reading_relation('64c32556-249c-4bf3-86bf-19823767eb9a', ai_alignment_priority__nearterm_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('64c32556-249c-4bf3-86bf-19823767eb9a', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('64c32556-249c-4bf3-86bf-19823767eb9a', foundational, existential_risk_dominates_expected_value).
narrative_ontology:cs_axiom_status(existential_risk_dominates_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('64c32556-249c-4bf3-86bf-19823767eb9a', existential_risk_dominates_expected_value, empirically_contingent).
narrative_ontology:cs_axiom('64c32556-249c-4bf3-86bf-19823767eb9a', foundational, alignment_is_loss_of_control_prevention).
narrative_ontology:cs_axiom_status(alignment_is_loss_of_control_prevention, holdable).
narrative_ontology:cs_axiom_grounding('64c32556-249c-4bf3-86bf-19823767eb9a', alignment_is_loss_of_control_prevention, deontological).
narrative_ontology:cs_axiom('64c32556-249c-4bf3-86bf-19823767eb9a', secondary, present_harms_are_distraction_from_core_problem).
narrative_ontology:cs_axiom_status(present_harms_are_distraction_from_core_problem, holdable).
narrative_ontology:cs_axiom_grounding('64c32556-249c-4bf3-86bf-19823767eb9a', present_harms_are_distraction_from_core_problem, instrumental).
narrative_ontology:cs_reference_frame('64c32556-249c-4bf3-86bf-19823767eb9a', pre_deep_learning_alignment_theory).
narrative_ontology:cs_drift_state('64c32556-249c-4bf3-86bf-19823767eb9a', post_gpt4_capability_surge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('64c32556-249c-4bf3-86bf-19823767eb9a', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, longtermist_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, frontier_labs).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, alignment_infrastructure_funders).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, global_humanity_undifferentiated).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, near_term_ai_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_harm_regulators).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, existential_risk_orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, instrumental_convergence_hypothesis).
narrative_ontology:constraint_vindicates(ai_alignment_priority__existential_risk_reading, sharp_left_turn_capability_gain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the alignment research agenda around catastrophic loss-of-control scenarios. Their professional identity, funding streams, and epistemic communities are fused with the existential risk framing. Exit means abandoning the research paradigm that constitutes their career and intellectual community.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, longtermist_researchers, agenda_setter,
    organized, civilizational, identity_locked, global).

% Receive massive resource allocation for capability-adjacent alignment work (interpretability, scalable oversight, red-teaming). The existential framing justifies their continued scaling while capturing the safety narrative. They can pivot framing if political winds shift.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, frontier_labs, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, frontier_labs, agenda_setter).

% Direct philanthropic and governmental funding toward longtermist priorities (compute, talent, field-building). The existential risk narrative unlocks capital at civilizational scale. They hold portfolio optionality across cause areas.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, alignment_infrastructure_funders, beneficiary,
    institutional, generational, arbitrage, global).

% Bears the diffuse opportunity cost of alignment resources flowing to speculative future scenarios rather than present harms. No organized representation in the governance conversations that allocate these resources. Cannot exit the planetary-scale consequences of either misaligned AI or misallocated safety effort.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, global_humanity_undifferentiated, payer,
    powerless, immediate, trapped, universal).

% Work on bias, fairness, labor displacement, and deployment harms. Their research is structurally marginalized by the existential framing's resource capture. Exit means leaving the field or reframing work in longtermist terms — many cannot do either without career loss.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, near_term_ai_ethics_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, near_term_ai_ethics_researchers, excluded).

% Experience deployed AI harms today (algorithmic discrimination, surveillance, economic displacement). The existential framing treats these as distractions from the 'real' problem. They have no voice in the resource allocation that treats their suffering as lower priority.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, marginalized_populations, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, marginalized_populations, excluded).

% Build regulatory frameworks for current AI harms (EU AI Act, US executive orders, etc.). The existential framing diverts political attention and technical talent toward speculative scenarios, making their enforcement job harder. They cannot easily pivot because their mandate is present-tense.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_harm_regulators, excluded,
    organized, biographical, constrained, national).

% Argue that catastrophic and present harms are complementary (shared causes: opacity, misaligned incentives, power concentration). They see the framing war itself as a coordination failure. Can engage across camps but hold no decisive leverage.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, integrated_governance_advocates, observer,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates fragmented AI safety concern onto a single, legible threat model (loss of control) that can mobilize civilizational-scale resources, coordinate research talent, and justify unprecedented compute governance.
% TRANSFER_FUNCTION: Moves research funding, talent, compute access, and regulatory attention from present-harm mitigation (bias, fairness, labor, surveillance) toward speculative capability-adjacent alignment work (interpretability, scalable oversight, red-teaming, evals).
% ABSENT_VOICES: Directly affected communities experiencing algorithmic harm today — workers displaced by automation, communities under algorithmic surveillance, populations subject to biased decision systems. They are not in the room where existential risk priorities are set; their representatives (civil society, near-term ethics researchers) are structurally excluded from the resource allocation table.
% DISAPPEARANCE_RATIONALE: If the existential risk framing vanished overnight, billions in committed funding would reallocate, research careers would pivot, compute governance proposals would lose their primary justification, and the entire 'AI safety' field would restructure around present-harm priorities. The field's institutional topology is built around this constraint.
% FOUNDING_PROBLEM: Early AI safety work (Yudkowsky, Bostrom, MIRI) identified that superintelligent systems could pursue goals catastrophically misaligned with human values, and that this risk dominates expected value calculations due to the astronomical stakes. The founding problem: how to ensure advanced AI systems remain controllable and aligned as capabilities scale beyond human oversight.
% FOUNDING_PROBLEM_CORROBORATION: Longtermist originators (Bostrom, Yudkowsky, MIRI) attest the problem remains live and worsening with capabilities progress. Near-term harm advocates (Buolamwini, Gebru, Noble, Crawford) and integrated governance scholars (Dafoe, Whittlestone, Cave) attest the founding problem has been superseded by deployed harms and that the framing now functions as resource capture. Independent forecasters and meta-researchers (Grace, Sandkühler, AI Index) document the diverging trajectories.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the framing captures the vast majority of 'AI safety' funding, talent, and political capital while delivering speculative returns on existential risk reduction — the opportunity cost is measurable present harm reduction foregone. Suppression (0.42) is moderate but structural: the framing doesn't ban near-term work but makes it illegible to the dominant funding and prestige channels. Theater ratio (0.51) exceeds 0.5 because a majority of 'alignment' activity (evals, red-teaming, capability benchmarks) is continuous with capabilities advancement rather than orthogonal safety research — the coordination function is real but increasingly performed by the same actors building the risk. Accessibility collapse (0.55) reflects that the framing has become the default in policy rooms; alternatives require rebuilding entire epistemic communities. Resistance (0.38) is growing but fragmented across disconnected communities (ethics, policy, labor, civil society).
 *
 * PERSPECTIVAL GAP:
 *   From the longtermist seat, this is a rope (genuine coordination against extinction). From the near-term harm seat, it's a snare (extraction via framing dominance). From the frontier lab seat, it's a beneficial scaffold (justifies scaling while buying legitimacy). From the marginalized population seat, it's an invisible constraint (their harms are defined out of the priority frame). The engine computes this divergence from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Longtermist researchers are identity-locked agenda-setters: their professional self-concept is constituted by the existential framing. Frontier labs and funders are institutional beneficiaries with arbitrage-grade exit — they capture the resource flow but can pivot. Global humanity is the universal trapped payer: bears opportunity cost with zero exit. Near-term ethics researchers and marginalized populations are constrained/trapped payers whose work is de-legitimized by the framing. Present-harm regulators are excluded organized actors whose mandate becomes harder to execute. The integrated advocates are mobile observers who see the structural trap but lack leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (controlling superintelligence) remains technically live per the originators, but the constraint has accumulated massive extractive overhead: the field now primarily produces capability-adjacent work that accelerates the very risk it claims to mitigate. The mandate has not atrophied — the threat model is arguably more credible — but the operationalization has become extractive. This is not a piton (the function is not vestigial) but a tangled rope where the coordination function is real and the extraction is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_probability_uncertainty,
    'What is the actual probability of catastrophic loss-of-control scenarios on relevant timescales, and how sensitive is the reading''s resource claim to this probability?',
    'Structured expert elicitation with diverse forecasters (not only longtermist-affiliated), tracking forecast accuracy on intermediate milestones. If probability estimates collapse toward zero, the reading''s extraction becomes indefensible.',
    'If existential risk probability is low (<1% this century), the reading''s resource capture is massively disproportionate — reclassification toward snare. If probability is high (>10%), the coordination function justifies the extraction — remains tangled_rope. The reading''s ε is structurally sensitive to this empirical question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_probability_uncertainty, empirical, 'Whether the existential threat model''s probability justifies its resource claim.').

omega_variable(
    capability_adjacent_safety_extraction,
    'What fraction of ''alignment'' research funding and talent actually reduces existential risk versus advancing capabilities under a safety label?',
    'Retrospective portfolio analysis of major alignment funding streams (Open Philanthropy, government AI safety institutes, lab safety teams) categorizing grants by: (a) directly targets loss-of-control, (b) advances capabilities with safety framing, (c) addresses present harms. Track citation and deployment patterns.',
    'If >60% of ''alignment'' resources fall in category (b), the theater_ratio is understated and the reading functions as capability acceleration with a safety veneer — reclassification toward snare. If >60% in (a), the coordination function is genuine — remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_adjacent_safety_extraction, empirical, 'Whether the reading''s operationalization matches its stated coordination function.').

omega_variable(
    framing_foreclosure_mechanism,
    'Does the existential reading logically foreclose the near-term harm reading within any single governance framework, or do they merely compete for scarce attention?',
    'Analyze actual governance documents (EU AI Act, US EO, UK AI Safety Summit, UN Global Digital Compact) for whether they treat the two framings as mutually exclusive budget/priority allocations or as complementary workstreams. Track rhetorical moves that frame present-harm work as ''distraction'' vs. ''foundation''.',
    'If governance structures treat them as zero-sum (forecloses), the kernel has a structural fault line that will produce institutional schism. If they are budget-competitive but logically compatible (coexists_with), the contest is political not structural. This determines whether the kernel can hold a stable integrated position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_foreclosure_mechanism, conceptual, 'Whether the sibling readings are structurally incompatible or politically competitive.').

omega_variable(
    identity_lock_mechanism_longtermist,
    'What specific identity-fusion mechanism binds longtermist researchers to the existential framing — professional identity, epistemic community, moral commitment, or funding dependence?',
    'Sociological study of career trajectories: what fraction of researchers who entered the field 2015-2020 have pivoted framing vs. doubled down? Correlate with funding source, publication venue, and public commitment depth.',
    'If identity_lock is primarily funding-dependent, it is fragile to political shifts. If it is professional/epistemic/moral, it is durable and the reading''s agenda-setter seat will resist evidence updates. This affects whether the constraint''s extraction can be reduced from within.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_longtermist, empirical, 'Mechanism of identity lock for the constraint''s primary agenda-setters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2014, ai_alignment_priority__existential_risk_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(ai_a_tr_t2016, ai_alignment_priority__existential_risk_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(ai_a_tr_t2018, ai_alignment_priority__existential_risk_reading, theater_ratio, 2018, 0.31).
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_priority__existential_risk_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(ai_a_tr_t2022, ai_alignment_priority__existential_risk_reading, theater_ratio, 2022, 0.45).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_priority__existential_risk_reading, theater_ratio, 2024, 0.51).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2014, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2014, 0.25).
narrative_ontology:measurement(ai_a_be_t2016, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2016, 0.32).
narrative_ontology:measurement(ai_a_be_t2018, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2018, 0.41).
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(ai_a_be_t2022, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2014, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2014, 0.18).
narrative_ontology:measurement(ai_a_su_t2016, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2016, 0.25).
narrative_ontology:measurement(ai_a_su_t2018, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2018, 0.32).
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement(ai_a_su_t2022, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2022, 0.41).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_alignment_priority__existential_risk_reading, 0.12).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_governance_compute_control).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_safety_funding_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, frontier_model_licensing_regime).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, interpretability_research_mandates).

% DUAL FORMULATION NOTE:
% Part of the ai_alignment_priority constraint family with nearterm_harms_reading and integrated_reading. This reading claims the 'alignment' label and captures the resource flow; the nearterm reading claims the justice frame and the affected populations; the integrated reading claims the synthesis. All three decompose the colloquial 'AI alignment' into structurally distinct constraints with different ε, different victim sets, different beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_alignment_priority__existential_risk_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
