% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Reformation as Technologically Mediated Mass Movement (Printing Press Reading)
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the Reformation as a fundamentally technological
 *   event, where the printing press acts as the primary enabling force. It
 *   transforms localized theological dissent into a continental mass movement
 *   by rapidly disseminating texts, increasing literacy, and fostering public
 *   debate. The printing press itself is treated as a Mountain due to its
 *   inherent physical properties and the irreversible changes it brought to
 *   information diffusion, which then enabled other social and political
 *   dynamics.
 *
 * KEY AGENTS:
 *   - printing_press_technology: Primary enabler (Mountain)
 *   - protestant_reformers: Primary beneficiaries (institutional/arbitrage) — leverage the technology
 *   - catholic_church: Primary target (institutional/constrained) — struggles to suppress information
 *   - literate_populace: Secondary beneficiaries (moderate/mobile) — gains access to information
 *   - printers_and_publishers: Secondary beneficiaries (organized/mobile) — profit from dissemination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.05).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.02).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Reformation as Technologically Mediated Mass Movement (Printing Press Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, 'b93a1b33-3c0b-43ae-8173-3695a2e63d29').
narrative_ontology:cs_kernel_codification('b93a1b33-3c0b-43ae-8173-3695a2e63d29', implicit).
narrative_ontology:cs_authority_grounding('b93a1b33-3c0b-43ae-8173-3695a2e63d29', diffuse_epistemic).
narrative_ontology:cs_reading_relation('b93a1b33-3c0b-43ae-8173-3695a2e63d29', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('b93a1b33-3c0b-43ae-8173-3695a2e63d29', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('b93a1b33-3c0b-43ae-8173-3695a2e63d29', foundational, information_diffusion_is_primary_driver).
narrative_ontology:cs_axiom_status(information_diffusion_is_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('b93a1b33-3c0b-43ae-8173-3695a2e63d29', information_diffusion_is_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('b93a1b33-3c0b-43ae-8173-3695a2e63d29', foundational, technology_shapes_social_movements).
narrative_ontology:cs_axiom_status(technology_shapes_social_movements, holdable).
narrative_ontology:cs_axiom_grounding('b93a1b33-3c0b-43ae-8173-3695a2e63d29', technology_shapes_social_movements, empirically_contingent).
narrative_ontology:cs_reference_frame('b93a1b33-3c0b-43ae-8173-3695a2e63d29', pre_print_information_economy).
narrative_ontology:cs_drift_state('b93a1b33-3c0b-43ae-8173-3695a2e63d29', post_gutenberg_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b93a1b33-3c0b-43ae-8173-3695a2e63d29', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, printers_and_publishers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, catholic_church).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, technological_determinism_hypothesis).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, information_diffusion_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The physical technology itself, enabling rapid, low-cost reproduction of texts. Its inherent properties define the constraint.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printing_press_technology, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(reformation_composite__technological_mediation_reading, printing_press_technology).

% Leveraged the printing press to disseminate their theological arguments, pamphlets, and Bibles, transforming local dissent into a widespread movement. They gained immense reach and influence.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, protestant_reformers, beneficiary,
    institutional, generational, arbitrage, continental).

% Lost its monopoly on information dissemination and interpretation. Its attempts at censorship and control were largely overwhelmed by the volume and speed of printed materials, leading to a loss of authority and revenue.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, catholic_church, payer,
    institutional, civilizational, constrained, global).

% Gained unprecedented access to religious texts, vernacular Bibles, and theological debates, fostering individual interpretation and reducing reliance on clerical intermediaries. This increased literacy and engagement.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_populace, beneficiary,
    moderate, biographical, mobile, local).

% Profited significantly from the increased demand for printed materials, particularly religious texts and polemics. They became key actors in the information ecosystem.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printers_and_publishers, beneficiary,
    organized, biographical, mobile, regional).

% Observed the power of the printing press to shape public opinion and sometimes leveraged it for their own political ends, either to support or suppress religious movements within their territories.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, secular_rulers, observer,
    powerful, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled the rapid, standardized, and widespread dissemination of complex theological and political ideas across vast geographical areas, coordinating a mass movement of thought and action.
% TRANSFER_FUNCTION: Transferred the power of information control from centralized ecclesiastical authority to a decentralized network of printers, authors, and readers, shifting intellectual and spiritual capital.
% ABSENT_VOICES: Those who could not read or afford printed materials remained excluded from direct participation in the textual debates, relying on oral transmission or interpretation by others. Their voices were mediated or absent from the direct print-driven discourse.
% DISAPPEARANCE_RATIONALE: If the printing press had never existed, the Reformation as a continental mass movement would not have occurred in the same form or scale. Theological dissent would have remained localized, and the power dynamics between church, state, and populace would have evolved very differently, likely without the rapid, widespread challenge to established authority.
% FOUNDING_PROBLEM: The problem of slow, expensive, and error-prone manual copying of texts, which limited the spread of knowledge and centralized control over information.
% FOUNDING_PROBLEM_CORROBORATION: The problem of manual copying is unequivocally dead, superseded by printing. Historians and technological analysts universally corroborate that the printing press fundamentally altered information economics, making the original problem obsolete. The Catholic Church's initial attempts to control print output also attest to the perceived threat to their prior information monopoly.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The printing press, as a technology, has negligible extractiveness or suppression in itself; its impact is in enabling the rapid, low-cost replication and distribution of information, which is a Mountain-like property. The 'extraction' (0.05) and 'suppression' (0.02) metrics reflect the minimal inherent friction of the technology and the initial attempts to control its output, which were largely ineffective against the underlying technological shift. The accessibility collapse is high (0.95) because once printing exists, the alternative (scribal copying) becomes almost entirely obsolete for mass communication. Resistance is low (0.01) because the technology itself is not resisted, only its outputs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the printing press itself, it is a neutral technological force. However, from the perspective of the Catholic Church, its operation was highly disruptive and extractive, as it undermined their control over information and doctrine. Protestant reformers, conversely, experienced it as a powerful enabling force. The engine will compute these divergent experiences from the declared stakeholder positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The printing press itself has no 'directionality' in the human sense. However, its *effects* are directional. Protestant reformers and the literate populace are beneficiaries (d near 0.0) as the technology amplifies their reach and access. The Catholic Church is a target (d near 1.0) as the technology undermines its authority and control. Printers and publishers are also beneficiaries, profiting from the new medium.
 *
 * MANDATROPHY ANALYSIS:
 *   The concept of mandatrophy does not directly apply to a technological constraint like the printing press, as its 'mandate' is inherent in its function. However, the *social structures* built around information control (e.g., censorship by the Catholic Church) did experience mandatrophy as the printing press rendered their original function obsolete. This constraint focuses on the enabling technology, not the decaying control structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the printing press''s role in the Reformation a genuine natural law of information diffusion, or a constructed constraint whose benefits were captured by specific agents?',
    'Comparative historical analysis of other information technologies and social movements; counterfactual analysis of the Reformation without the printing press.',
    'If purely natural, its classification as Mountain is robust. If constructed, it might be reclassified as a Rope or even Tangled Rope, with beneficiaries capturing the ''natural'' diffusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between technological inevitability and agentic capture of diffusion.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''technological_mediation_reading'' of the ''reformation_composite'' kernel. What would change if a sibling reading were adopted?',
    'Analyzing the ''theological_fragmentation_reading'' or ''political_realignment_reading'' as primary: this would shift focus from print technology to doctrinal disputes or state power dynamics, altering the core causal mechanism and stakeholder set.',
    'Adopting a sibling reading would lead to a different constraint_id, different primary observables, and potentially a different claimed_type and metric profile, as the core mechanism of the Reformation would be re-framed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Impact of alternative readings of the Reformation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1517, 1547).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__technological_mediation_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(refo_tr_t10, reformation_composite__technological_mediation_reading, theater_ratio, 10, 0.01).
narrative_ontology:measurement(refo_tr_t20, reformation_composite__technological_mediation_reading, theater_ratio, 20, 0.01).
narrative_ontology:measurement(refo_tr_t30, reformation_composite__technological_mediation_reading, theater_ratio, 30, 0.01).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__technological_mediation_reading, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(refo_be_t10, reformation_composite__technological_mediation_reading, base_extractiveness, 10, 0.02).
narrative_ontology:measurement(refo_be_t20, reformation_composite__technological_mediation_reading, base_extractiveness, 20, 0.03).
narrative_ontology:measurement(refo_be_t30, reformation_composite__technological_mediation_reading, base_extractiveness, 30, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__technological_mediation_reading, suppression_requirement, 0, 0.01).
narrative_ontology:measurement(refo_su_t10, reformation_composite__technological_mediation_reading, suppression_requirement, 10, 0.01).
narrative_ontology:measurement(refo_su_t20, reformation_composite__technological_mediation_reading, suppression_requirement, 20, 0.02).
narrative_ontology:measurement(refo_su_t30, reformation_composite__technological_mediation_reading, suppression_requirement, 30, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reformation_composite' kernel, focusing on the technological mediation of the printing press. It structurally influences the theological and political dimensions of the Reformation by enabling mass communication.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
