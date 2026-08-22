% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: AI Risk Governance: Existential Risk Priority
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'existential risk' reading of AI
 *   governance, where preventing superintelligence scenarios that could
 *   annihilate or permanently curtail humanity's potential is the paramount
 *   priority. It is a reading of the 'ai_risk_governance_priority' kernel.
 *   This prioritization directs significant resources and policy attention
 *   towards highly speculative, long-term risks, often at the expense of
 *   addressing immediate, demonstrable harms. The constraint is claimed as a
 *   Tangled Rope by its proponents, suggesting a necessary coordination
 *   function for humanity's survival, but its operation exhibits high
 *   extraction and suppression, particularly from those advocating for
 *   near-term harms.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda-setter (institutional/constrained)
 *   - ai_labs_claiming_safety_leadership: Primary beneficiary (powerful/mobile)
 *   - future_humanity: Primary victim (powerless/trapped)
 *   - near_term_harms_advocates: Payer (organized/constrained)
 *   - global_south_populations: Payer (powerless/trapped)
 *   - policy_makers: Agenda-setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.68).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.75).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "AI Risk Governance: Existential Risk Priority").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'd914ca17-fc00-4d29-a54f-4936a4313d3e').
narrative_ontology:cs_kernel_codification('d914ca17-fc00-4d29-a54f-4936a4313d3e', distributed).
narrative_ontology:cs_authority_grounding('d914ca17-fc00-4d29-a54f-4936a4313d3e', expertise).
narrative_ontology:cs_interpretation_layer_present('d914ca17-fc00-4d29-a54f-4936a4313d3e').
narrative_ontology:cs_reading_relation('d914ca17-fc00-4d29-a54f-4936a4313d3e', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('d914ca17-fc00-4d29-a54f-4936a4313d3e', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('d914ca17-fc00-4d29-a54f-4936a4313d3e', foundational, existential_risk_is_paramount).
narrative_ontology:cs_axiom_status(existential_risk_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('d914ca17-fc00-4d29-a54f-4936a4313d3e', existential_risk_is_paramount, deontological).
narrative_ontology:cs_axiom('d914ca17-fc00-4d29-a54f-4936a4313d3e', foundational, superintelligence_is_imminent_and_uncontrollable).
narrative_ontology:cs_axiom_status(superintelligence_is_imminent_and_uncontrollable, holdable).
narrative_ontology:cs_axiom_grounding('d914ca17-fc00-4d29-a54f-4936a4313d3e', superintelligence_is_imminent_and_uncontrollable, empirically_contingent).
narrative_ontology:cs_reference_frame('d914ca17-fc00-4d29-a54f-4936a4313d3e', humanity_at_risk_from_agi).
narrative_ontology:cs_drift_state('d914ca17-fc00-4d29-a54f-4936a4313d3e', contemporary_ai_acceleration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d914ca17-fc00-4d29-a54f-4936a4313d3e', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the scope of 'existential risk' and advocate for policy priorities, directing funding and research towards long-term alignment and control problems. They benefit from the prioritization of these risks, securing funding and influence.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Major AI development labs that publicly align with existential risk narratives, often using it to justify slower deployment or internal safety research, which can also serve as a competitive advantage or regulatory moat. They benefit from reduced scrutiny on present-day harms and increased legitimacy for their long-term research agendas.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary,
    powerful, biographical, mobile, global).

% The ultimate 'victim' of existential risks, whose potential is either annihilated or permanently curtailed. They bear the hypothetical costs of inaction on superintelligence, but also the opportunity costs of resources diverted from present-day problems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Advocates for addressing immediate harms like bias, misinformation, and labor displacement. They bear the cost of diverted attention and resources, finding their concerns deprioritized or framed as secondary to speculative future risks.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harms_advocates, payer,
    organized, immediate, constrained, global).

% Populations disproportionately affected by present-day AI harms (e.g., surveillance, algorithmic bias in resource allocation, labor exploitation). They bear the direct costs of unmitigated harms while resources are directed towards distant, speculative risks.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, global_south_populations, payer,
    powerless, immediate, trapped, global).

% Government bodies and international organizations tasked with developing AI policy. They are influenced by the dominant narratives and funding streams, often balancing competing priorities but leaning towards those with strong institutional backing.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global attention and resources towards a specific class of highly speculative, high-impact risks associated with advanced AI, aiming to prevent catastrophic outcomes for humanity.
% TRANSFER_FUNCTION: Transfers significant research funding, policy attention, and regulatory focus from immediate, demonstrable AI harms to long-term, speculative existential risks, primarily benefiting institutions and labs focused on these long-term scenarios.
% ABSENT_VOICES: Future generations, who are the primary beneficiaries/victims of this constraint, are inherently absent. Additionally, many voices from marginalized communities experiencing present-day AI harms are often excluded or marginalized in high-level AI governance discussions, which tend to be dominated by technical experts and industry representatives.
% DISAPPEARANCE_RATIONALE: If the prioritization of existential risk vanished, the AI governance landscape would immediately reorient. Resources would likely flow more towards mitigating present-day harms, and the framing of AI safety would shift from 'alignment' to 'fairness' and 'accountability'. Research institutions and labs currently benefiting from this prioritization would need to adapt or lose influence.
% FOUNDING_PROBLEM: The potential for advanced artificial intelligence to develop capabilities beyond human control, leading to unforeseen and catastrophic outcomes for humanity, including extinction or permanent disempowerment.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by a significant portion of the AI research community, prominent public intellectuals, and some government bodies, who cite theoretical arguments and accelerating AI capabilities. Critics (outside the benefiting parties) argue the problem is speculative and distracts from more immediate, empirically verifiable harms, thus contesting its 'live' status as a primary concern.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the diversion of resources and attention from present-day problems to speculative future ones. Suppression (0.75) is high due to the framing of existential risk as an 'all-of-humanity' problem, which can suppress dissent or alternative priorities by framing them as secondary or even irresponsible. The theater ratio (0.45) indicates that while some genuine safety work occurs, a significant portion of 'safety leadership' serves to legitimize the current power structures and deflect criticism regarding immediate harms. Accessibility collapse (0.6) is moderate because alternative framings (like near-term harms) are not entirely eliminated but are significantly marginalized. Resistance (0.55) is present from near-term harms advocates but is often outmatched by the institutional power of x-risk proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk research institutions and AI labs, this constraint is a vital coordination mechanism for humanity's survival, justifying the allocation of resources. From the perspective of near-term harms advocates and global south populations, it operates as a snare, extracting resources and attention from their immediate suffering under the guise of a distant, speculative threat.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and AI labs are beneficiaries (low d) as they gain funding, influence, and a favorable regulatory environment. Future humanity is a theoretical beneficiary but also a payer (high d) due to opportunity costs. Near-term harms advocates and global south populations are clear payers (high d) as their concerns are deprioritized. Policy makers are agenda-setters, balancing influence but often swayed by powerful narratives and institutional lobbying.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it claims a genuine coordination function (preventing human extinction) but exhibits significant asymmetric extraction (diverting resources from present harms) and requires active enforcement (suppressing alternative risk framings). It prevents mislabeling by highlighting that the coordination story is intertwined with a clear extractive dynamic, rather than being pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_risk_speculation_vs_empirical_harm,
    'Is the prioritization of speculative existential risks over empirically demonstrable near-term harms justified by the probability and severity of the former, or does it reflect a bias towards abstract, technical problems?',
    'Development of robust, empirically grounded methodologies for assessing the probability and impact of superintelligence scenarios, and comparative risk analysis against the cumulative impact of present-day harms.',
    'If speculative risks are found to be significantly overblown or less probable than claimed, the extractiveness of this constraint would be reclassified as higher, and its coordination function diminished, potentially shifting its type towards a Snare. If robustly justified, its coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_risk_speculation_vs_empirical_harm, empirical, 'Ambiguity regarding the empirical grounding of existential risk claims versus present harms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative risk framings structural (institutional power, funding flows) or internalized (cognitive patterns that lead to self-censorship or dismissal of ''lesser'' risks)?',
    'Analysis of discourse patterns, funding allocation mechanisms, and the career trajectories of researchers who challenge the existential risk prioritization. If suppression persists even when structural barriers are nominally removed, it suggests internalized mechanisms.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as it operates through self-reinforcing cognitive biases within the field. This would amplify the extractive nature of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative risk framings.').

omega_variable(
    framing_under_determination_ai_risk,
    'Is the ''existential_risk_reading'' the only defensible framing for AI risk governance, or do alternative framings (e.g., ''near_term_harms_reading'') offer equally coherent, albeit different, structural analyses?',
    'Comparative analysis of the problem-solution fit, stakeholder inclusion, and resource allocation outcomes across different risk framings. Resolution would involve identifying which framing best accounts for the full spectrum of AI''s societal impacts and stakeholder experiences.',
    'If alternative framings are found to be equally or more coherent, it would challenge the legitimacy of this reading''s prioritization, potentially reclassifying it as a Snare by exposing its coordination story as cover for extraction. If this reading is uniquely coherent, its Tangled Rope classification would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_ai_risk, conceptual, 'The choice of framing for AI risk governance (existential vs. near-term harms) is under-determined by the evidence, leading to different structural classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2015, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ai_r_tr_t2018, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(ai_r_tr_t2021, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(ai_r_tr_t2024, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2015, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(ai_r_be_t2018, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(ai_r_be_t2021, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(ai_r_be_t2024, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2015, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(ai_r_su_t2018, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2018, 0.5).
narrative_ontology:measurement(ai_r_su_t2021, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(ai_r_su_t2024, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, global_infrastructure).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_governance_priority' kernel. It focuses on existential risks, while 'near_term_harms_reading' focuses on immediate impacts, and 'bridge_reading' attempts to integrate both. These are distinct constraints arising from different interpretations of the same underlying problem space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
