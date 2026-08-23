% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: AI Safety as Existential Risk Prevention (Existential Risk Reading)
 *   domain: technology_governance/risk_assessment
 *
 * SUMMARY:
 *   The existential risk reading defines AI safety exclusively as preventing
 *   extinction-level outcomes from misaligned superintelligence. This framing
 *   emerged from the MIRI/LessWrong community (2000s), gained institutional
 *   dominance through longtermist philanthropy (Open Philanthropy, FTX Future
 *   Fund) and AI lab safety teams (2015-present), and now structures
 *   government policy (AISIs, voluntary commitments, evals). The constraint
 *   operates by defining the field: what counts as "AI safety work," who gets
 *   funded, what policies are pursued. It coordinates genuine effort on a
 *   speculative threat while extracting from near-term harm work and
 *   present-day communities — a tangled rope with real coordination function
 *   (if the threat is real) and asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__existential_risk_reading, 0.55).
domain_priors:theater_ratio(ai_safety_commitment__existential_risk_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_safety_commitment__existential_risk_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__existential_risk_reading, "AI Safety as Existential Risk Prevention (Existential Risk Reading)").
narrative_ontology:topic_domain(ai_safety_commitment__existential_risk_reading, "technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__existential_risk_reading, '16007c1e-006d-48c6-9b39-0eedf8a4d381').
narrative_ontology:cs_kernel_codification('16007c1e-006d-48c6-9b39-0eedf8a4d381', distributed).
narrative_ontology:cs_authority_grounding('16007c1e-006d-48c6-9b39-0eedf8a4d381', extraction).
narrative_ontology:cs_interpretation_layer_present('16007c1e-006d-48c6-9b39-0eedf8a4d381').
narrative_ontology:cs_reading_relation('16007c1e-006d-48c6-9b39-0eedf8a4d381', ai_safety_commitment__near_term_harms_reading, forecloses).
narrative_ontology:cs_reading_relation('16007c1e-006d-48c6-9b39-0eedf8a4d381', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('16007c1e-006d-48c6-9b39-0eedf8a4d381', foundational, existential_risk_prevention_defines_ai_safety).
narrative_ontology:cs_axiom_status(existential_risk_prevention_defines_ai_safety, holdable).
narrative_ontology:cs_axiom_grounding('16007c1e-006d-48c6-9b39-0eedf8a4d381', existential_risk_prevention_defines_ai_safety, instrumental).
narrative_ontology:cs_axiom('16007c1e-006d-48c6-9b39-0eedf8a4d381', foundational, alignment_difficulty_justifies_resource_concentration).
narrative_ontology:cs_axiom_status(alignment_difficulty_justifies_resource_concentration, holdable).
narrative_ontology:cs_axiom_grounding('16007c1e-006d-48c6-9b39-0eedf8a4d381', alignment_difficulty_justifies_resource_concentration, empirically_contingent).
narrative_ontology:cs_reference_frame('16007c1e-006d-48c6-9b39-0eedf8a4d381', civilizational_survival_priority).
narrative_ontology:cs_drift_state('16007c1e-006d-48c6-9b39-0eedf8a4d381', post_chatgpt_policy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('16007c1e-006d-48c6-9b39-0eedf8a4d381', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__existential_risk_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, alignment_researchers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, longtermist_institutions).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__existential_risk_reading, humanity_future_conditional).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, near_term_harm_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, present_affected_communities).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, alignment_junior_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_safety_commitment__existential_risk_reading, government_ai_policy_offices).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, orthogonality_thesis).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, instrumental_convergence).
narrative_ontology:constraint_vindicates(ai_safety_commitment__existential_risk_reading, alignment_difficulty_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive concentrated funding, status, and institutional support under this framing. Their research agenda (RLHF, interpretability, scalable oversight) becomes the primary definition of AI safety work. However, they face extreme pressure to produce results on speculative timelines, with career risk if progress stalls.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, alignment_researchers, beneficiary,
    organized, biographical, constrained, global).

% Set the research priorities and funding allocation for AI safety (Open Philanthropy, FTX Future Fund historically, major AI labs' safety teams). They benefit from controlling the definition of the field and capturing talent pipelines. Their exit options are high — they can pivot funding strategies.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, longtermist_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, longtermist_institutions, beneficiary).

% The abstract beneficiary: all future humans who exist conditional on alignment success. They cannot advocate for themselves. Their interests are represented by longtermist institutions. If alignment succeeds, they gain everything; if it fails, they never exist. No exit possible.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, humanity_future_conditional, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_safety_commitment__existential_risk_reading, humanity_future_conditional).

% Work on bias, discrimination, labor exploitation, misinformation, and other deployed-system harms. Under this reading, their work is excluded from "AI safety" proper, leading to funding marginalization, conference exclusion, and career disincentives. They can pivot to industry ethics roles but lose field-defining recognition.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, near_term_harm_researchers, payer,
    moderate, biographical, constrained, global).

% Communities currently harmed by deployed AI systems (algorithmic discrimination, gig worker exploitation, misinformation victims, creative workers displaced). Their harms are treated as "not AI safety" under this reading, delaying remediation and policy response. No meaningful exit from AI-deployed systems.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, present_affected_communities, payer,
    powerless, immediate, trapped, global).

% PhD students, postdocs, and early-career researchers entering alignment. They face extreme publish-or-perish pressure on problems that may be fundamentally unsolvable on relevant timelines. Professional identity fuses with the mission ("saving humanity"), making exit psychologically costly. High burnout rates documented.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, alignment_junior_researchers, payer,
    powerless, biographical, identity_locked, global).

% OpenAI, Anthropic, DeepMind leadership. They fund and direct alignment work while racing to build AGI. The existential risk framing legitimizes their scale of investment and provides regulatory cover ("we're taking safety seriously"). They can pivot between safety framing and acceleration as needed.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_lab_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% NIST AI Safety Institute, UK AISI, EU AI Office. They adopt the existential risk framing in policy (voluntary commitments, evals, potential licensing). They pay opportunity cost: regulatory bandwidth spent on speculative evals instead of present harms enforcement. Constrained by legislative mandates and inter-agency politics.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, government_ai_policy_offices, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__existential_risk_reading, government_ai_policy_offices, payer).

% Researchers who question alignment difficulty, timelines, or the framing itself (e.g., skeptics of fast takeoff, critics of longtermism). Their views are structurally excluded from mainline AI safety venues and funding. They can publish in ML venues but lose influence on the "safety" definition.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, ai_safety_critics_skeptics, excluded,
    moderate, biographical, mobile, global).

% Sees the full structural asymmetry: a definitional constraint that channels billions in resources and thousands of careers toward a speculative technical agenda while deprioritizing documented harms. The observer notes the identity_lock on junior researchers and the resource capture by institutional agenda-setters.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__existential_risk_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates civilization-scale research effort and resource allocation toward preventing human extinction from misaligned superintelligence — a genuine coordination problem if the threat model is correct, requiring unified technical agenda, talent concentration, and policy alignment.
% TRANSFER_FUNCTION: Moves funding, talent, regulatory attention, and field-defining authority from near-term harm research and present-day affected communities toward alignment research (RLHF, interpretability, governance for pause/slowdown) and longtermist institutions. Junior alignment researchers pay with career capital and psychological burden.
% ABSENT_VOICES: Present-day harmed communities (algorithmic discrimination victims, gig workers, misinformation targets, displaced creatives) and near-term harm researchers are structurally excluded from the "AI safety" table. They would object to the definitional exclusion of their harms but lack institutional access to the safety funding/policy apparatus.
% DISAPPEARANCE_RATIONALE: If the existential risk definition vanished overnight, billions in funding would reallocate, thousands of researchers would pivot, policy agendas would shift to present harms, and the longtermist institutional architecture would lose its central organizing premise. The AI safety field would restructure around dual-priority or near-term framings.
% FOUNDING_PROBLEM: The perceived gap between rapidly advancing AI capabilities and the absence of any credible technical solution for aligning superintelligent systems with human values — identified circa 2000-2010 by Yudkowsky, Bostrom, and early LessWrong/MIRI community as the dominant civilizational risk.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (alignment difficulty + fast takeoff) is attested by the originating community (MIRI, FHI, early LessWrong). It is contested by: (1) ML researchers who dispute fast takeoff likelihood (e.g., LeCun, Ng, many at NeurIPS/ICML), (2) near-term harm researchers who argue present harms are the real founding problem, (3) dual-priority advocates who say both are live. No consensus outside the originating tradition.
narrative_ontology:disappearance_verdict(ai_safety_commitment__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_safety_commitment__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__existential_risk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects high resource capture by alignment agenda: ~80% of AI safety funding goes to alignment/ex-risk work per 2023-24 analyses. Suppression (0.55) is moderate: the framing doesn't legally ban near-term work but structurally marginalizes it through funding, hiring, and venue control. Theater (0.42) is rising: growing share of activity is performative (evals that don't bind, commitments without enforcement, interpretability demos on toy models) while core alignment problems (deceptive alignment, scalable oversight) remain unsolved. Accessibility collapse (0.62): once you accept the framing, alternatives (near-term priority, dual priority) appear to miss the point entirely. Resistance (0.48): significant pushback from ML mainstream, near-term harm advocates, and dual-priority coalitions.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (longtermist institutions, lab leadership), this is a rope: genuine coordination on the most important problem. From the payer seats (near-term researchers, present communities, junior alignment researchers), it operates as a snare: extraction without consent, suppression of alternatives. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) acknowledges both coordination and extraction are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Longtermist institutions and AI lab leadership are agenda_setters with arbitrage exit — they set the frame and can pivot. Alignment researchers are beneficiaries but constrained (career-locked to the agenda). Humanity_future_conditional is the ultimate beneficiary but trapped (nonexistent, represented by others). Near-term harm researchers and present communities are payers with constrained/trapped exit — they lose resources and recognition. Junior alignment researchers are payers with identity_locked exit — professional identity fuses with the mission. Critics are excluded but mobile (can work elsewhere). The observer sees the full asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (prevent extinction) remains live per the reading's own lights. But the coordination function has accumulated extraction: funding concentration, career pipeline capture, policy bandwidth diversion. The mandatrophy risk is that the field persists even if the threat model proves exaggerated — the institutional architecture now depends on the framing. The founding_problem_status=contested captures this: the problem the arrangement was built for is disputed by relevant experts outside the benefiting tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_model_validity,
    'Is the core threat model (fast takeoff + alignment difficulty + extinction likelihood) empirically well-founded, or is it a speculative philosophical argument that has captured a field?',
    'Track record of AI progress vs. takeover scenarios; emergence of deceptive alignment or situational awareness in frontier models; expert surveys on timelines and risk with calibrated forecasting.',
    'If threat model is well-founded, the coordination function is genuine and extraction is the price of civilizational survival. If speculative, the constraint is a snare masquerading as a rope — extraction without commensurate coordination value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_model_validity, empirical, 'Whether the existential risk threat model justifies the resource capture.').

omega_variable(
    coordination_extraction_boundary,
    'Is the marginal extraction from near-term harm work and junior researchers necessary for the coordination function, or is it incidental capture by institutional agenda-setters?',
    'Counterfactual analysis: would alignment progress be slower if near-term harm work were equally funded? Do junior researcher burnout rates correlate with output? Compare to fields with similar coordination challenges but less extraction (e.g., nuclear safety, climate science).',
    'If extraction is necessary, tangled_rope classification holds. If incidental, the constraint trends toward snare — the coordination story becomes cover for institutional self-dealing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the asymmetric extraction is structurally coupled to the coordination function.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the existential_risk_reading logically foreclose the near_term_harms_reading within a single commitment framework, or do they merely compete for resources?',
    'Analyze the definitional logic: if "AI safety" is defined as X, can it also be defined as not-X within the same framework? Test by asking proponents: could a single institution legitimately adopt both definitions simultaneously?',
    'If forecloses, reading_relations=forecloses is correct and the kernel has genuine structural fracture. If merely competes, coexists_with would be more accurate and the kernel is a resource allocation dispute, not a logical incompatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between existential_risk_reading and near_term_harms_reading within one framework.').

omega_variable(
    identity_lock_mechanism_juniors,
    'What specific identity-fusion mechanism binds junior alignment researchers — professional identity (career path dependence), ideological identity (longtermist worldview), relational identity (mentor/community bonds), or institutional identity (lab/organization culture)?',
    'Longitudinal surveys of alignment PhD cohorts tracking identity markers, exit intentions, and actual exits. Compare to other high-pressure academic fields (theoretical physics, pure math) to isolate AI-safety-specific mechanisms.',
    'If ideological/relational, exit is psychologically harder than career-path dependence alone — the constraint''s effective suppression is higher than structural measures suggest. If primarily professional, the lock may weaken as alternative career paths (AI safety in industry, policy) proliferate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_juniors, empirical, 'Mechanism of identity_lock for junior alignment researchers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__existential_risk_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_safety_existential_tr_t2010, ai_safety_commitment__existential_risk_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(ai_safety_existential_tr_t2014, ai_safety_commitment__existential_risk_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(ai_safety_existential_tr_t2017, ai_safety_commitment__existential_risk_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(ai_safety_existential_tr_t2020, ai_safety_commitment__existential_risk_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(ai_safety_existential_tr_t2022, ai_safety_commitment__existential_risk_reading, theater_ratio, 2022, 0.39).
narrative_ontology:measurement(ai_safety_existential_tr_t2025, ai_safety_commitment__existential_risk_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_safety_existential_be_t2010, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2010, 0.35).
narrative_ontology:measurement(ai_safety_existential_be_t2014, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2014, 0.42).
narrative_ontology:measurement(ai_safety_existential_be_t2017, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2017, 0.51).
narrative_ontology:measurement(ai_safety_existential_be_t2020, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(ai_safety_existential_be_t2022, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement(ai_safety_existential_be_t2025, ai_safety_commitment__existential_risk_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_safety_existential_su_t2010, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(ai_safety_existential_su_t2014, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2014, 0.32).
narrative_ontology:measurement(ai_safety_existential_su_t2017, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2017, 0.41).
narrative_ontology:measurement(ai_safety_existential_su_t2020, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(ai_safety_existential_su_t2022, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement(ai_safety_existential_su_t2025, ai_safety_commitment__existential_risk_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__existential_risk_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__existential_risk_reading, 0.18).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_safety_commitment__dual_priority_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, ai_governance_pause_proposals).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, interpretability_research_funding).
narrative_ontology:affects_constraint(ai_safety_commitment__existential_risk_reading, rlhf_standardization).

% DUAL FORMULATION NOTE:
% This constraint (existential_risk_reading) and near_term_harms_reading are distinct ε-invariant constraints decomposed from the single label 'AI safety.' The existential reading has high ε on speculative alignment interventions (0.68) and low ε on present accountability; the near-term harms reading has the inverse profile. They are linked as a constraint family via affects_constraints. The dual_priority_reading attempts to synthesize both but structurally inherits the resource allocation tension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__existential_risk_reading, institutional, 0.15).
constraint_indexing:directionality_override(ai_safety_commitment__existential_risk_reading, organized, 0.25).
constraint_indexing:directionality_override(ai_safety_commitment__existential_risk_reading, powerless, 0.85).
constraint_indexing:directionality_override(ai_safety_commitment__existential_risk_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
