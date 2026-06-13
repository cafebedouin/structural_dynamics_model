% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance Prioritization: Near-Term Harms
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'near-term harms' reading of AI risk
 *   governance prioritization. It asserts that mitigating present,
 *   demonstrated harms (bias, misinformation, labor displacement,
 *   surveillance) affecting marginalized populations should be the primary
 *   focus. This reading is in active contest with 'existential risk' and
 *   'bridge' framings. The constraint operates as a Tangled Rope: it
 *   genuinely coordinates attention and resources towards real problems, but
 *   also extracts from affected populations by often providing insufficient
 *   or performative mitigation, while benefiting technology companies by
 *   diverting attention from more fundamental structural changes.
 *
 * KEY AGENTS:
 *   - ai_ethics_advocates: Agenda-setter (organized/constrained) — pushes for this prioritization.
 *   - marginalized_populations: Primary payer (powerless/trapped) — bears the direct harms.
 *   - displaced_workers: Payer (powerless/constrained) — bears economic costs of automation.
 *   - global_south_communities: Payer (powerless/trapped) — targets of untested AI systems.
 *   - technology_companies: Beneficiary (institutional/arbitrage) — benefits from diverted regulatory attention.
 *   - ai_ethics_consultants: Beneficiary (moderate/mobile) — profits from demand for ethical AI solutions.
 *   - existential_risk_researchers: Excluded (organized/constrained) — advocates for a different prioritization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.65).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.7).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance Prioritization: Near-Term Harms").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, 'fc35d750-e511-4a2e-85f7-fc082d43d316').
narrative_ontology:cs_kernel_codification('fc35d750-e511-4a2e-85f7-fc082d43d316', distributed).
narrative_ontology:cs_authority_grounding('fc35d750-e511-4a2e-85f7-fc082d43d316', distributed).
narrative_ontology:cs_reading_relation('fc35d750-e511-4a2e-85f7-fc082d43d316', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc35d750-e511-4a2e-85f7-fc082d43d316', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('fc35d750-e511-4a2e-85f7-fc082d43d316', foundational, present_suffering_demands_priority).
narrative_ontology:cs_axiom_status(present_suffering_demands_priority, holdable).
narrative_ontology:cs_axiom_grounding('fc35d750-e511-4a2e-85f7-fc082d43d316', present_suffering_demands_priority, deontological).
narrative_ontology:cs_axiom('fc35d750-e511-4a2e-85f7-fc082d43d316', secondary, demonstrated_harms_are_tractable).
narrative_ontology:cs_axiom_status(demonstrated_harms_are_tractable, holdable).
narrative_ontology:cs_axiom_grounding('fc35d750-e511-4a2e-85f7-fc082d43d316', demonstrated_harms_are_tractable, empirically_contingent).
narrative_ontology:cs_reference_frame('fc35d750-e511-4a2e-85f7-fc082d43d316', human_rights_first_framework).
narrative_ontology:cs_drift_state('fc35d750-e511-4a2e-85f7-fc082d43d316', contemporary_ai_policy_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('fc35d750-e511-4a2e-85f7-fc082d43d316', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_ethics_consultants).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively push for regulatory frameworks and corporate policies that address present, demonstrable harms of AI. They conduct research, publish reports, and lobby policymakers, often facing resistance from industry and those focused on long-term speculative risks. Their influence is growing but often diluted by competing narratives.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_ethics_advocates, agenda_setter,
    organized, biographical, constrained, global).

% Bear the direct and immediate costs of biased algorithms, surveillance technologies, and automated decision-making systems. They often lack the means to contest these systems or exit their reach, experiencing discrimination in housing, employment, credit, and justice systems. Their voices are often unheard in high-level policy discussions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations, payer,
    powerless, immediate, trapped, local).

% Face job displacement due to automation without adequate retraining or social safety nets. They bear the economic and social costs of technological advancement, often with limited collective bargaining power or political representation to influence AI deployment policies.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, displaced_workers, payer,
    powerless, immediate, constrained, national).

% Are often targets for the deployment of untested or ethically questionable AI systems, experiencing data colonialism, surveillance, and the exacerbation of existing inequalities. They have minimal agency in shaping global AI governance norms and are structurally vulnerable to extractive practices.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_communities, payer,
    powerless, generational, trapped, global).

% Benefit from a regulatory environment that, while acknowledging present harms, often struggles to implement effective, binding controls. They can leverage 'AI ethics' as a public relations tool while continuing to deploy systems with known issues, often diverting resources to long-term speculative risks that are less immediately threatening to their business models.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Profit from the demand for 'ethical AI' solutions, offering services like bias audits, fairness frameworks, and responsible AI strategy. While some genuinely push for change, the industry can also serve to legitimize existing practices without fundamentally altering power dynamics or extractive structures.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_ethics_consultants, beneficiary,
    moderate, biographical, mobile, global).

% Focus on preventing catastrophic or existential risks from advanced AI. While they acknowledge present harms, their prioritization framework often leads them to advocate for different policy interventions and resource allocations, sometimes at the expense of immediate concerns. They are excluded from the 'near-term harms' framing as the primary agenda-setter.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, existential_risk_researchers, excluded,
    organized, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts to identify, measure, and mitigate specific, observable harms arising from AI deployment, such as algorithmic bias, privacy violations, and job displacement, by focusing regulatory and research attention on these issues.
% TRANSFER_FUNCTION: Transfers resources (funding, policy attention, research effort) from speculative, long-term AI risks to immediate, demonstrable harms affecting vulnerable populations. It also transfers the burden of harm from technology companies to affected communities when mitigation efforts are insufficient.
% ABSENT_VOICES: Those advocating for a primary focus on existential AI risks are often marginalized in this framing, as are the voices of those who benefit from the status quo of unchecked AI deployment. The most directly affected marginalized populations often lack direct representation in policy-making bodies.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, the focus would likely shift back to more abstract or speculative risks, or to industry self-regulation, leading to an exacerbation of present harms for marginalized populations and a reduction in accountability for technology companies. The current (albeit imperfect) mechanisms for addressing bias and displacement would atrophy.
% FOUNDING_PROBLEM: The rapid deployment of AI systems led to documented instances of algorithmic discrimination, job displacement, and privacy violations, disproportionately affecting vulnerable and marginalized communities, with insufficient regulatory or ethical oversight.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected communities and civil society organizations consistently corroborate the ongoing nature and severity of these harms. International human rights bodies and some government agencies also attest to the problem's persistence, from outside the direct beneficiaries of the current AI development paradigm.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because while this framing directs attention to harms, the actual mitigation often falls short, leaving affected populations to bear significant costs. Suppression (0.70) is also high, as the structural power of technology companies and the complexity of AI systems make it difficult for victims to resist or exit. The theater ratio (0.20) is moderate, reflecting that while genuine efforts are made, some 'ethical AI' initiatives are performative, serving to manage public perception rather than enact deep structural change. Resistance (0.80) is high, reflecting the active and ongoing advocacy from civil society and affected communities against these harms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized populations, this prioritization is a necessary but often insufficient response, still leaving them as payers. From the perspective of technology companies, it's a manageable regulatory burden that allows continued innovation. AI ethics advocates see it as a crucial step towards justice. The engine will compute these divergent experiences based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology companies and AI ethics consultants are beneficiaries (low d) as this framing, while addressing harms, often allows for continued business models and creates new consulting opportunities. Marginalized populations, displaced workers, and Global South communities are victims (high d) as they bear the direct and indirect costs of the harms and the often-insufficient mitigation. AI ethics advocates are agenda-setters, pushing for this framing, but their position is complex as they also seek to alleviate the burden on victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophic; the problem it addresses (near-term AI harms) is demonstrably live and growing. The challenge is not obsolescence but effectiveness and the potential for the 'coordination' function (addressing harms) to be co-opted for 'extraction' (maintaining industry power while appearing to address concerns). The classification as Tangled Rope reflects this hybrid nature, preventing it from being mislabeled as a pure Rope (overlooking extraction) or a Snare (overlooking genuine coordination efforts).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_effectiveness,
    'Are the resources and policy attention directed by this prioritization framework genuinely effective in mitigating harms for marginalized populations, or are they primarily absorbed by compliance overhead and performative measures?',
    'Independent, longitudinal impact assessments of specific mitigation policies on affected communities, measuring actual reduction in harm metrics (e.g., bias rates, job security, surveillance exposure).',
    'If resources are largely ineffective, the constraint''s effective extractiveness is higher than measured, as the coordination function is largely theatrical. If highly effective, extractiveness is lower, moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_effectiveness, empirical, 'Effectiveness of harm mitigation efforts vs. performative compliance.').

omega_variable(
    framing_diversion_effect,
    'To what extent does the strong emphasis on ''near-term harms'' divert regulatory and public attention from more fundamental structural issues in AI development (e.g., data ownership, power concentration, economic models of surveillance capitalism)?',
    'Content analysis of policy documents and media coverage, tracking the proportion of attention given to mitigation vs. structural reform, alongside interviews with policymakers and industry leaders regarding their perceived regulatory pressures.',
    'If diversion is substantial, the constraint''s effective extractiveness for technology companies is lower (as it protects their core business models), and higher for victims (as deeper issues remain unaddressed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_diversion_effect, conceptual, 'Whether the framing diverts attention from deeper structural issues.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''near-term harms'' reading fundamentally compatible with the ''existential risk'' reading within a single, coherent AI governance framework, or do their core premises lead to irreconcilable policy prescriptions?',
    'Analysis of proposed policy interventions from both camps: if a policy addressing one reading''s priority actively undermines the other''s, they are foreclosing. If they can coexist or influence each other without direct contradiction, they are compatible.',
    'If foreclosing, the contest is zero-sum, increasing the effective suppression of the excluded reading. If coexisting or influencing, a ''bridge'' reading becomes more viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Compatibility of near-term vs. existential risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
