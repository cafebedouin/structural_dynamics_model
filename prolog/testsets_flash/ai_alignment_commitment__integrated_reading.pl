% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__integrated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__integrated_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_commitment__integrated_reading
 *   human_readable: Integrated AI Alignment Commitment
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint represents the commitment within the AI governance
 *   community that 'alignment' must simultaneously address both 'control
 *   problems' (e.g., preventing catastrophic misuse or loss of control of
 *   advanced AI) and 'justice problems' (e.g., preventing the reproduction of
 *   social biases, ensuring equitable access, and mitigating present-day
 *   harms). It rejects a false dichotomy between these concerns, arguing that
 *   fragmenting them leads to incomplete and potentially harmful solutions.
 *   The constraint actively enforces this integrated perspective, extracting
 *   from and suppressing purely siloed approaches.
 *
 * KEY AGENTS:
 *   - ai_governance_researchers: Agenda setter (institutional/analytical) — defines the integrated agenda
 *   - future_humanity: Beneficiary (civilizational) — protected from catastrophic risks
 *   - marginalized_populations: Beneficiary (generational) — protected from present and future harms
 *   - siloed_ai_safety_researchers: Payer (powerful) — pressured to broaden scope
 *   - siloed_ai_ethics_researchers: Payer (powerful) — pressured to integrate control concerns
 *   - ai_developers_prioritizing_speed: Payer (institutional) — faces increased regulatory and ethical overhead
 *   - funding_bodies: Agenda setter (institutional) — directs resources towards integrated approaches
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, 0.65).
domain_priors:suppression_score(ai_alignment_commitment__integrated_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_commitment__integrated_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__integrated_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__integrated_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__integrated_reading, "Integrated AI Alignment Commitment").
narrative_ontology:topic_domain(ai_alignment_commitment__integrated_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__integrated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__integrated_reading, 'f8def7ba-abcd-4171-b949-98f220b4e512').
narrative_ontology:cs_kernel_codification('f8def7ba-abcd-4171-b949-98f220b4e512', distributed).
narrative_ontology:cs_authority_grounding('f8def7ba-abcd-4171-b949-98f220b4e512', expertise).
narrative_ontology:cs_interpretation_layer_present('f8def7ba-abcd-4171-b949-98f220b4e512').
narrative_ontology:cs_reading_relation('f8def7ba-abcd-4171-b949-98f220b4e512', ai_alignment_commitment__safety_control_reading, influences).
narrative_ontology:cs_reading_relation('f8def7ba-abcd-4171-b949-98f220b4e512', ai_alignment_commitment__ethics_justice_reading, influences).
narrative_ontology:cs_axiom('f8def7ba-abcd-4171-b949-98f220b4e512', foundational, alignment_is_holistic_risk_management).
narrative_ontology:cs_axiom_status(alignment_is_holistic_risk_management, holdable).
narrative_ontology:cs_axiom_grounding('f8def7ba-abcd-4171-b949-98f220b4e512', alignment_is_holistic_risk_management, deontological).
narrative_ontology:cs_axiom('f8def7ba-abcd-4171-b949-98f220b4e512', secondary, fragmentation_leads_to_incomplete_solutions).
narrative_ontology:cs_axiom_status(fragmentation_leads_to_incomplete_solutions, holdable).
narrative_ontology:cs_axiom_grounding('f8def7ba-abcd-4171-b949-98f220b4e512', fragmentation_leads_to_incomplete_solutions, empirically_contingent).
narrative_ontology:cs_reference_frame('f8def7ba-abcd-4171-b949-98f220b4e512', unified_risk_framework).
narrative_ontology:cs_drift_state('f8def7ba-abcd-4171-b949-98f220b4e512', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f8def7ba-abcd-4171-b949-98f220b4e512', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__integrated_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__integrated_reading, ai_governance_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_ai_safety_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, siloed_ai_ethics_researchers).
narrative_ontology:constraint_victim(ai_alignment_commitment__integrated_reading, ai_developers_prioritizing_speed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively advocate for and define the integrated approach to AI alignment, shaping research agendas, policy recommendations, and public discourse. They benefit from a more holistic and robust framework but bear the cost of intellectual labor to synthesize disparate fields.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_governance_researchers, agenda_setter,
    institutional, generational, constrained, global).

% The ultimate beneficiary, protected from existential risks posed by uncontrolled advanced AI systems, as well as from the perpetuation of systemic injustices by such systems. Their interests are represented by current advocates.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_commitment__integrated_reading, future_humanity).

% Benefit from the explicit inclusion of justice problems in alignment, aiming to prevent AI systems from reproducing or amplifying existing social biases and harms. They are often the primary victims of unaligned or ethically flawed AI systems.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, marginalized_populations, beneficiary,
    powerless, generational, trapped, global).

% Historically focused on technical control problems (e.g., corrigibility, inner alignment). They are pressured to broaden their scope to include ethical and justice considerations, which can feel like a dilution of their expertise or a diversion of resources from urgent technical risks. They pay in terms of intellectual re-tooling and expanded research scope.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_ai_safety_researchers, payer,
    powerful, biographical, constrained, global).

% Historically focused on fairness, accountability, and transparency (FAT) in AI. They are pressured to integrate technical control problems and existential risk considerations, which can feel like a shift away from immediate social justice concerns or an overemphasis on speculative future risks. They pay in terms of expanded conceptual frameworks and interdisciplinary collaboration.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, siloed_ai_ethics_researchers, payer,
    powerful, biographical, constrained, global).

% Face increased regulatory and ethical overhead due to the integrated alignment agenda. They are pressured to slow down development, implement more rigorous testing for both safety and fairness, and engage with a broader range of stakeholders, incurring costs in time and resources.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, ai_developers_prioritizing_speed, payer,
    institutional, immediate, constrained, global).

% Direct research grants and institutional support towards integrated alignment initiatives, effectively enforcing the constraint through resource allocation. They benefit from funding more comprehensive and impactful research but bear the cost of managing complex interdisciplinary programs.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__integrated_reading, funding_bodies, agenda_setter,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_commitment__integrated_reading, ai_governance_researchers).
narrative_ontology:fixing_cost_class(ai_alignment_commitment__integrated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the diverse efforts within the AI alignment community by establishing a unified framework that addresses both control and justice problems, preventing fragmentation and ensuring comprehensive risk mitigation.
% TRANSFER_FUNCTION: Transfers intellectual and financial resources from purely siloed research efforts towards integrated, interdisciplinary approaches. It also transfers the burden of comprehensive risk assessment onto AI developers and researchers.
% ABSENT_VOICES: Purely technical optimists who believe that 'solving' control problems will automatically resolve justice issues, and purely social justice advocates who dismiss 'existential risk' as a distraction, are marginalized. They would argue for simpler, more focused approaches, but their perspectives are actively de-emphasized by the integrated agenda.
% DISAPPEARANCE_RATIONALE: If this integrated commitment vanished, the AI alignment field would likely revert to fragmented, siloed efforts, with safety researchers focusing solely on control and ethics researchers on justice. This would lead to incomplete solutions, increased risk of catastrophic outcomes, and exacerbated social harms, requiring a significant reorganization of research and policy.
% FOUNDING_PROBLEM: The initial fragmentation of AI alignment into distinct 'AI safety' (control) and 'AI ethics' (justice) communities, leading to incomplete solutions and a false dichotomy that hindered comprehensive risk mitigation.
% FOUNDING_PROBLEM_CORROBORATION: Leading interdisciplinary AI governance organizations and independent academic reviews attest that the problem of fragmentation, while mitigated, remains live. They cite ongoing challenges in fostering genuine interdisciplinary collaboration and resource allocation, corroborating the need for an integrated approach from outside the immediate beneficiaries of siloed research.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__integrated_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__integrated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__integrated_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_commitment__integrated_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__integrated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__integrated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__integrated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) comes from the overhead and re-prioritization required to adopt an integrated approach, especially for those accustomed to siloed work. Suppression (0.70) is high because funding, publication, and institutional recognition are increasingly directed away from purely specialized efforts. Theater ratio (0.20) is low, indicating that the commitment is largely genuine, though some performative 'integration' may occur without deep change. Accessibility collapse (0.40) is moderate, as siloed work is still possible but increasingly marginalized. Resistance (0.55) is present from those who prefer to focus on their specialized areas.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'ai_governance_researchers' and 'funding_bodies', this is a necessary coordination to achieve comprehensive alignment. From the perspective of 'siloed_ai_safety_researchers' and 'siloed_ai_ethics_researchers', it can feel like an extractive imposition that dilutes their specialized expertise and forces them into unfamiliar domains, increasing their workload and potentially slowing progress in their core areas.
 *
 * DIRECTIONALITY LOGIC:
 *   'AI governance researchers' and 'funding_bodies' act as agenda setters, benefiting from a more robust and legitimate alignment discourse. 'Future humanity' and 'marginalized populations' are the ultimate beneficiaries, as their interests are explicitly integrated. 'Siloed AI safety researchers' and 'siloed AI ethics researchers' are payers, as they bear the cost of adapting their methodologies and expanding their scope. 'AI developers prioritizing speed' are also payers, facing increased scrutiny and requirements.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents the mislabeling of fragmented efforts as 'alignment'. By actively integrating control and justice problems, it ensures that the mandate of alignment remains comprehensive and does not atrophy into a narrow technical problem or a purely social one. The active enforcement prevents the coordination function from being undermined by siloed incentives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine integrated approach to AI alignment, or is it merely a rhetorical framing to bridge otherwise siloed efforts?',
    'Empirical analysis of resource allocation and research program design: if funding and effort are genuinely integrated across control and justice problems, it supports the integrated reading. If resources remain siloed, it suggests a rhetorical bridge.',
    'If rhetorical, the constraint''s effective extractiveness from fragmented efforts is higher, as the ''integration'' serves as cover for continued specialization, potentially reclassifying towards a Snare for those genuinely seeking integration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''integrated_reading'' of the ''ai_alignment_commitment'' kernel. Sibling readings (''safety_control_reading'', ''ethics_justice_reading'') would emphasize one aspect over the other, leading to different victim sets and extractiveness profiles. The disagreement is located in the scope and definition of ''alignment''.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of siloed approaches structural (e.g., funding mandates) or internalized (e.g., social pressure within the research community)?',
    'Post-funding-mandate trajectory: if siloed approaches persist after funding structures are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — researchers carry the suppression with them after external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for siloed research.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__integrated_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_commitment__integrated_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_commitment__integrated_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_commitment__integrated_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_commitment__integrated_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_commitment__integrated_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_commitment__integrated_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_commitment__integrated_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_commitment__integrated_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_commitment__integrated_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_commitment__integrated_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_commitment__integrated_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_commitment__integrated_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__integrated_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__safety_control_reading).
narrative_ontology:affects_constraint(ai_alignment_commitment__integrated_reading, ai_alignment_commitment__ethics_justice_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'integrated_reading' of the 'ai_alignment_commitment' kernel, which also has 'safety_control_reading' and 'ethics_justice_reading' as sibling constraints. This integrated reading aims to bridge and supersede the perceived dichotomy between the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
