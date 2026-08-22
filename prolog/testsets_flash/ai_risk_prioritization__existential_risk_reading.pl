% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: AI Risk Prioritization: Existential Risk Focus
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the prioritization of AI existential risk
 *   (x-risk) over near-term harms within the broader AI safety discourse. It
 *   asserts that misaligned AGI poses an extinction-level threat, making
 *   alignment research and capability controls paramount. This reading frames
 *   near-term harms as secondary or even distracting from the 'real' problem.
 *   The constraint operates by directing funding, research agendas, and
 *   policy attention towards x-risk, often at the expense of other AI safety
 *   concerns. The claimed type is 'tangled_rope' because it genuinely
 *   coordinates a complex, global problem (preventing extinction) but does so
 *   with significant asymmetric extraction from those concerned with
 *   immediate harms.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda-setter (institutional/constrained) – defines the problem and directs resources.
 *   - longtermist_funders: Primary beneficiary (powerful/mobile) – provides funding, validates worldview.
 *   - near_term_ai_harms_advocates: Primary payer (organized/constrained) – bears costs of deprioritization.
 *   - future_humanity: Ultimate victim (powerless/trapped) – the theoretical beneficiary of x-risk prevention, but also the ultimate target of the threat.
 *   - policy_makers: Agenda-setter (institutional/constrained) – balances competing narratives, allocates resources.
 *   - ai_developers: Beneficiary/Payer (powerful/mobile) – benefits from legitimacy, bears potential regulatory costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "AI Risk Prioritization: Existential Risk Focus").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'a48a5621-08ba-4591-8110-2d119e27b6ee').
narrative_ontology:cs_kernel_codification('a48a5621-08ba-4591-8110-2d119e27b6ee', distributed).
narrative_ontology:cs_authority_grounding('a48a5621-08ba-4591-8110-2d119e27b6ee', expertise).
narrative_ontology:cs_interpretation_layer_present('a48a5621-08ba-4591-8110-2d119e27b6ee').
narrative_ontology:cs_reading_relation('a48a5621-08ba-4591-8110-2d119e27b6ee', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('a48a5621-08ba-4591-8110-2d119e27b6ee', foundational, agi_poses_existential_threat).
narrative_ontology:cs_axiom_status(agi_poses_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('a48a5621-08ba-4591-8110-2d119e27b6ee', agi_poses_existential_threat, empirically_contingent).
narrative_ontology:cs_axiom('a48a5621-08ba-4591-8110-2d119e27b6ee', foundational, longtermism_is_moral_priority).
narrative_ontology:cs_axiom_status(longtermism_is_moral_priority, holdable).
narrative_ontology:cs_axiom_grounding('a48a5621-08ba-4591-8110-2d119e27b6ee', longtermism_is_moral_priority, deontological).
narrative_ontology:cs_reference_frame('a48a5621-08ba-4591-8110-2d119e27b6ee', agi_alignment_as_primary_safety_goal).
narrative_ontology:cs_drift_state('a48a5621-08ba-4591-8110-2d119e27b6ee', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('a48a5621-08ba-4591-8110-2d119e27b6ee', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, ai_developers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the problem of AGI existential risk, conduct alignment research, and advocate for policy interventions. They receive significant funding based on this prioritization and shape the discourse around AI safety. Their influence is tied to the perceived urgency and severity of x-risk.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Provide substantial financial resources to x-risk research and advocacy. Their philanthropic and investment strategies are explicitly aligned with safeguarding the long-term future of humanity, making AGI x-risk a primary concern. They benefit from the validation and prioritization of their worldview.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, mobile, global).

% Advocate for addressing immediate and observable harms of AI, such as bias, discrimination, labor displacement, and surveillance. They find their concerns deprioritized and underfunded in favor of speculative future risks, bearing the cost of diverted attention and resources.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates, payer,
    organized, biographical, constrained, global).

% The ultimate potential victim of unaligned AGI, bearing the existential threat. This group includes non-existent persons whose welfare is projected into the future. Their interests are represented by x-risk advocates, but they have no direct agency or exit options.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Responsible for regulating AI development and deployment. They are influenced by both x-risk and near-term harms narratives, often struggling to balance competing priorities and allocate resources effectively. Their decisions shape the regulatory landscape.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Develop and deploy AI systems. While they benefit from the focus on long-term safety (which can legitimize their work), they also bear the costs of potential regulatory burdens and the moral hazard of focusing on distant risks over present ones. Some actively engage in alignment research.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_developers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, ai_developers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts and resources towards a singular, long-term goal of preventing human extinction from misaligned AGI, by focusing research, funding, and policy on AGI alignment and capability controls.
% TRANSFER_FUNCTION: Transfers significant intellectual, financial, and political capital from addressing near-term, tangible AI harms to speculative, long-term existential risks. This includes diverting research grants, policy attention, and public discourse.
% ABSENT_VOICES: Communities disproportionately affected by current AI harms (e.g., those facing algorithmic bias, job displacement, or surveillance) are often marginalized in the x-risk-dominated discourse. Their immediate suffering is framed as a lesser priority compared to future extinction.
% DISAPPEARANCE_RATIONALE: If the existential risk prioritization vanished, the AI safety field would immediately reorient towards near-term harms, algorithmic justice, and ethical deployment. Funding would shift, research agendas would change, and policy efforts would focus on present-day impacts, fundamentally altering the landscape of AI governance.
% FOUNDING_PROBLEM: The potential for advanced artificial intelligence to develop goals misaligned with human values, leading to an uncontrollable intelligence explosion and catastrophic outcomes for humanity.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by leading AI researchers, philosophers, and public intellectuals who have articulated the technical and philosophical challenges of AGI alignment. While the probability and timeline are debated, the conceptual possibility of the problem is widely acknowledged across various expert communities, including those not directly benefiting from x-risk funding.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is high because resources and attention are significantly diverted from addressing present, measurable harms to a speculative, albeit severe, future risk. Suppression (0.70) is also high, as alternative framings (e.g., algorithmic justice) are actively marginalized or dismissed as less important. The theater ratio (0.20) is relatively low, indicating that the core alignment research is genuine, but there's a performative aspect in how other concerns are downplayed. The increasing extractiveness and suppression over time reflect the growing dominance of the x-risk narrative and the associated institutionalization of its priorities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk institutions and longtermist funders, this constraint is a necessary coordination mechanism to prevent a catastrophic future. From the perspective of near-term harms advocates, it is an extractive mechanism that diverts resources from urgent, tangible problems, effectively making them 'pay' for a distant, uncertain one. Future humanity, as the ultimate victim, has no agency in this gap.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and longtermist funders are beneficiaries, as the constraint directs resources and legitimacy towards their agendas. Near-term harms advocates and future humanity are payers/victims, as their concerns are deprioritized or they bear the ultimate, albeit theoretical, cost of the threat. AI developers and policymakers have mixed directionality, benefiting from some aspects while bearing costs in others.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing AGI extinction) is still 'live' and widely attested, preventing it from being a piton. However, the 'tangled_rope' classification highlights that while the coordination function is real, it comes with significant asymmetric extraction, preventing it from being a pure rope. The contestation around the 'founding_problem_status' (live vs. solved/shifted) is key to understanding its current operation as a tangled rope rather than a snare, as the coordination function is still genuinely perceived by many as necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_of_agi_x_risk,
    'What is the actual probability and timeline of AGI existential risk, and how does it compare to the certainty and scale of near-term AI harms?',
    'Further empirical and theoretical research on AGI capabilities, alignment techniques, and societal impact, combined with robust, independent risk assessments that compare different classes of AI risk.',
    'If the probability of AGI x-risk is lower or further in the future than currently prioritized, resources would shift towards near-term harms, potentially reclassifying this constraint as a snare or piton. If higher, it would reinforce the current prioritization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(probability_of_agi_x_risk, empirical, 'Uncertainty regarding the likelihood and timing of AGI existential threats.').

omega_variable(
    resource_allocation_efficiency,
    'Is the current allocation of resources to AGI alignment research the most effective way to mitigate overall AI risk, considering both existential and near-term threats?',
    'Comprehensive cost-benefit analysis and impact assessments of different AI safety interventions, comparing the effectiveness of x-risk-focused vs. near-term-focused strategies.',
    'If current allocation is found inefficient, it would suggest the constraint is more extractive than coordinative, potentially shifting its classification towards a snare. If efficient, it would strengthen the ''rope'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation in mitigating AI risk.').

omega_variable(
    framing_of_near_term_harms,
    'To what extent is the framing of near-term AI harms as ''distractions'' a genuine assessment of their relative importance, versus a rhetorical strategy to maintain x-risk prioritization?',
    'Discourse analysis, stakeholder interviews, and examination of funding patterns to identify explicit or implicit suppression of alternative narratives. Analysis of whether x-risk solutions genuinely address near-term harms as a byproduct.',
    'If primarily rhetorical, the suppression metric is higher, and the constraint leans more towards a snare. If a genuine assessment, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_near_term_harms, conceptual, 'Ambiguity in the framing of near-term harms relative to x-risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2015, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ai_r_tr_t2018, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(ai_r_tr_t2021, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(ai_r_tr_t2024, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2015, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(ai_r_be_t2018, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(ai_r_be_t2021, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement(ai_r_be_t2024, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2015, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(ai_r_su_t2018, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(ai_r_su_t2021, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(ai_r_su_t2024, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, global_infrastructure).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_ethics_funding_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_governance_regulatory_focus).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
