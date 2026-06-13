% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Deterministic Force in Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the 'technological determinism' reading of the
 *   printing press's role in the Reformation. In this view, the printing
 *   press is an autonomous enabling technology whose inherent properties
 *   (speed, reproducibility, cost reduction) made the spread of vernacular
 *   scripture and the success of the Reformation inevitable. Human agency,
 *   strategic choices by reformers and printers, and the co-constitutive
 *   relationship between technology and society are downplayed or absent. The
 *   technology itself is treated as a 'mountain' — an unchangeable, fixed
 *   force that dictated historical outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.02).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Deterministic Force in Reformation").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'b4ccf396-323b-4988-98fb-449c1a6b1620').
narrative_ontology:cs_kernel_codification('b4ccf396-323b-4988-98fb-449c1a6b1620', implicit).
narrative_ontology:cs_authority_grounding('b4ccf396-323b-4988-98fb-449c1a6b1620', diffuse_epistemic).
narrative_ontology:cs_reading_relation('b4ccf396-323b-4988-98fb-449c1a6b1620', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('b4ccf396-323b-4988-98fb-449c1a6b1620', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('b4ccf396-323b-4988-98fb-449c1a6b1620', foundational, technology_as_primary_causal_agent).
narrative_ontology:cs_axiom_status(technology_as_primary_causal_agent, holdable).
narrative_ontology:cs_axiom_grounding('b4ccf396-323b-4988-98fb-449c1a6b1620', technology_as_primary_causal_agent, empirically_contingent).
narrative_ontology:cs_axiom('b4ccf396-323b-4988-98fb-449c1a6b1620', foundational, historical_outcomes_as_inevitable).
narrative_ontology:cs_axiom_status(historical_outcomes_as_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('b4ccf396-323b-4988-98fb-449c1a6b1620', historical_outcomes_as_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('b4ccf396-323b-4988-98fb-449c1a6b1620', pure_technological_causality).
narrative_ontology:cs_drift_state('b4ccf396-323b-4988-98fb-449c1a6b1620', contemporary_socio_technical_studies, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b4ccf396-323b-4988-98fb-449c1a6b1620', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, vernacular_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The printing press itself, as an autonomous technological force, dictated the spread of ideas and the inevitability of the Reformation. Its inherent properties (speed, reproducibility, cost reduction) are seen as the primary drivers.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printing_press_technology, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, printing_press_technology).

% Benefited from the press's inherent capacity to disseminate their ideas and vernacular Bibles widely and rapidly, which this reading sees as an inevitable outcome of the technology's existence, rather than a strategic choice.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, protestant_reformers, beneficiary,
    powerful, generational, mobile, continental).

% Gained access to scripture and religious texts in their native languages, a development seen as a direct and unavoidable consequence of the printing press's capabilities, leading to increased literacy and religious autonomy.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, vernacular_readers, beneficiary,
    organized, biographical, mobile, continental).

% Suffered a loss of control over information dissemination and religious interpretation due to the unstoppable spread of printed materials. This reading views their attempts to suppress vernacular texts as ultimately futile against the technological tide.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_church, payer,
    institutional, generational, constrained, global).

% Analyze the long-term impacts of technological innovations, often seeking to identify deterministic causal pathways between technology and societal change. This reading aligns with a particular school of thought within this field.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, historians_of_technology, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press inherently coordinated the rapid, widespread, and standardized dissemination of information across diverse linguistic and geographic regions, enabling a collective shift in religious understanding.
% TRANSFER_FUNCTION: Transferred the means of knowledge production and religious authority from centralized ecclesiastical institutions to a decentralized network of printers and readers, driven by the technology's intrinsic properties.
% ABSENT_VOICES: Printers and reformers who made conscious, strategic decisions about what to print, how to distribute it, and how to frame its message; their agency is downplayed or absent in this deterministic account.
% DISAPPEARANCE_RATIONALE: If the deterministic effect of the printing press on the Reformation vanished, the historical outcome of the Reformation would remain, but the causal explanation for it would shift from technological inevitability to human agency and strategic action. The 'world' (historical events) would not rearrange, only its interpretation.
% FOUNDING_PROBLEM: The problem of slow, expensive, and centrally controlled information dissemination, which limited access to knowledge and religious texts.
% FOUNDING_PROBLEM_CORROBORATION: The problem of slow, expensive, and centrally controlled information dissemination was indeed a historical reality prior to the printing press. However, the claim that the press *inevitably* led to the Reformation is contested by historians who emphasize human agency and social context. Corroboration for the 'inevitability' aspect is primarily from proponents of technological determinism, not from independent historical consensus.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because, from this deterministic perspective, the printing press's effects were inherent and unavoidable, akin to natural law. Extractiveness, suppression, and theater ratio are all very low because the technology itself is not seen as extracting from or suppressing agents, but rather as an impersonal force. Accessibility collapse is high (0.95) because the technology fundamentally altered the landscape of information access, making previous methods obsolete. Resistance is low (0.01) because, in this view, resistance to the press's effects was ultimately futile against its inherent power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this technological determinism, there is no significant perspectival gap; the effects of the press are seen as universally inevitable. However, from other readings (e.g., strategic deployment or co-constitution), the deterministic view itself creates a gap by obscuring human agency and strategic choices.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'printing_press_technology' itself is framed as the agenda_setter, dictating outcomes. Protestant reformers and vernacular readers are beneficiaries, as the technology's inherent properties served their interests. The Catholic Church is a payer, as its authority was undermined by the technology's unstoppable force. Directionality for the technology itself is analytical, as it's not a human agent. For human actors, directionality is derived from their structural position relative to the 'inevitable' technological force.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a complex socio-technical phenomenon as a simple coordination problem or pure extraction. By classifying it as a 'mountain' (with beneficiaries), the framework highlights the claim of inevitability while still allowing for the detection of who benefits from such a framing. The FSM mechanism would flag this if the 'naturalness' claim were challenged by the presence of beneficiaries, prompting an omega variable to explore the ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_autonomy_vs_agency,
    'Is the printing press truly an autonomous force, or were its effects mediated and shaped by human agency and strategic choices?',
    'Historical analysis focusing on specific decisions by printers, reformers, and political actors regarding content, distribution, and suppression; counterfactual history exploring alternative outcomes if different choices were made.',
    'If human agency is found to be significant, the ''mountain'' classification would be challenged, potentially shifting towards a ''tangled_rope'' or ''rope'' if coordination or extraction by human actors is revealed. The ''emerges_naturally'' claim would be undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_autonomy_vs_agency, empirical, 'Ambiguity regarding the degree of technological determinism versus human agency in historical outcomes.').

omega_variable(
    natural_law_vs_social_construct,
    'Is the ''inevitability'' of the Reformation due to the printing press a natural law-like phenomenon, or a retrospective social construct that benefits certain narratives?',
    'Critical historiography examining the origins and persistence of the technological determinism narrative itself, and identifying who benefits from its propagation.',
    'If it''s a social construct, the ''mountain'' classification is a false summit, and the constraint would reclassify to a ''tangled_rope'' or ''snare'' depending on the degree of extraction by those who benefit from the deterministic framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Whether the deterministic claim is an objective truth or a constructed narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__technological_determinism, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.01).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.01).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__technological_determinism, theater_ratio, 1650, 0.01).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__technological_determinism, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.05).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__technological_determinism, base_extractiveness, 1650, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__technological_determinism, suppression_requirement, 1450, 0.02).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__technological_determinism, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causality__technological_determinism, suppression_requirement, 1550, 0.02).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__technological_determinism, suppression_requirement, 1600, 0.02).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causality__technological_determinism, suppression_requirement, 1650, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
