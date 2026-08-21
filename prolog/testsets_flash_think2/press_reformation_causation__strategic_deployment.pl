% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Strategic Deployment of the Printing Press in the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'strategic_deployment' reading of
 *   the 'press_reformation_causation' kernel. It posits that the printing
 *   press was a neutral technological capacity that reformers and printers
 *   deliberately and purposefully exploited to achieve their goals during the
 *   Reformation. The technology itself did not 'cause' the Reformation, but
 *   rather served as an effective tool in the hands of agents with specific
 *   intentions. The constraint is classified as a Rope, reflecting its
 *   function as a coordination tool that primarily benefited its users.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.25).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.1).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.25).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Deployment of the Printing Press in the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, 'c425f8ac-c4e7-4b44-b926-d567950581a3').
narrative_ontology:cs_kernel_codification('c425f8ac-c4e7-4b44-b926-d567950581a3', fixed_text).
narrative_ontology:cs_authority_grounding('c425f8ac-c4e7-4b44-b926-d567950581a3', practice).
narrative_ontology:cs_reading_relation('c425f8ac-c4e7-4b44-b926-d567950581a3', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('c425f8ac-c4e7-4b44-b926-d567950581a3', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('c425f8ac-c4e7-4b44-b926-d567950581a3', foundational, human_agency_is_primary_driver).
narrative_ontology:cs_axiom_status(human_agency_is_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('c425f8ac-c4e7-4b44-b926-d567950581a3', human_agency_is_primary_driver, conventional).
narrative_ontology:cs_axiom('c425f8ac-c4e7-4b44-b926-d567950581a3', foundational, technology_is_a_neutral_tool).
narrative_ontology:cs_axiom_status(technology_is_a_neutral_tool, holdable).
narrative_ontology:cs_axiom_grounding('c425f8ac-c4e7-4b44-b926-d567950581a3', technology_is_a_neutral_tool, conventional).
narrative_ontology:cs_reference_frame('c425f8ac-c4e7-4b44-b926-d567950581a3', agency_driven_innovation).
narrative_ontology:cs_drift_state('c425f8ac-c4e7-4b44-b926-d567950581a3', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c425f8ac-c4e7-4b44-b926-d567950581a3', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively chose to utilize the printing press to disseminate their theological arguments, critiques of the Catholic Church, and vernacular Bibles, thereby gaining influence and challenging existing religious authority. They saw the press as a powerful, neutral tool for their cause.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, reformers, agenda_setter,
    organized, generational, mobile, continental).

% Profited significantly from the demand for printed materials generated by the Reformation. They strategically invested in presses and distributed texts, expanding their businesses and influence by aligning with reform movements. They viewed the press as a commercial opportunity.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, printers, beneficiary,
    moderate, biographical, mobile, regional).

% Faced a profound challenge to its authority and control over information dissemination. Its attempts at censorship and suppression were largely reactive and often ineffective against the strategic, decentralized use of the press by reformers. It bore the cost of losing its monopoly on information.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_church, payer,
    institutional, civilizational, constrained, global).

% While not directly interacting with the press, their collective situation was the target of the reformers' strategic efforts. Their access to information remained largely mediated, but the content they received was increasingly shaped by the printed word, even if read aloud by others. They were excluded from the strategic decisions but impacted by the outcomes.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, illiterate_populace, excluded,
    powerless, biographical, trapped, local).

% Analyze the historical evidence to argue for the primacy of human agency and deliberate choices in the adoption and impact of the printing press during the Reformation, emphasizing the press as a tool rather than a determinant force.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, historians_strategic_deployment, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the rapid and widespread dissemination of religious and political ideas across Europe, enabling synchronized reform movements and the formation of new intellectual communities.
% TRANSFER_FUNCTION: Transferred information, influence, and eventually power from centralized religious authorities to decentralized reform movements and individual readers. It also transferred wealth and market share to printers.
% ABSENT_VOICES: Historians and theorists who emphasize the inherent transformative power of technology (technological determinists) or those who argue for a more complex co-evolutionary process (mutual shaping) are structurally absent from this reading's internal logic, which prioritizes human agency.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the printing press had not occurred, the Reformation would have unfolded very differently, likely slower, less widespread, and potentially less impactful, or perhaps not at all in its historical form. The rapid and coordinated spread of ideas was crucial to its success.
% FOUNDING_PROBLEM: The need for reformers to rapidly disseminate their theological arguments and critiques to a broad audience, and for printers to find profitable markets for their new technology beyond traditional elite texts.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and media studies, distinct from the direct beneficiaries (reformers/printers), corroborate the strategic choices, profit motives, and deliberate actions involved in the press's adoption and impact. Primary sources from the period also attest to the intentional use of the press by reformers.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).
:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the press primarily served as a coordination tool, facilitating the spread of ideas rather than coercively extracting from its users. The 'profit/power' mentioned in the prompt refers to the gains from successful coordination and market activity, not asymmetric extraction. Suppression is low (0.1) as the technology itself did not inherently suppress alternatives, though its widespread adoption made older forms of communication less competitive. Theater ratio is negligible (0.05) because the use of the press was direct and functional. Accessibility collapse is moderate (0.3) as alternatives for communication existed, but the press offered a vastly more efficient means. Resistance is low (0.15) from the perspective of those deploying it, as they were actively adopting and benefiting from it. The slight increase in suppression_requirement over time reflects the Catholic Church's reactive attempts at censorship, which were largely ineffective against the decentralized strategic deployment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformers and printers, the press was an empowering tool, a 'rope' that facilitated their goals and generated profit. From the perspective of the Catholic Church, it was a disruptive force that undermined its authority and control over information, effectively acting as a 'snare' to its established order. This story focuses on the 'rope' aspect from the perspective of the strategic deployers.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are clear beneficiaries (low directionality) as they actively used the press to achieve their objectives and gain influence/profit. The Catholic Church is the primary 'payer' or target (high directionality) as it bore the costs of the press's disruptive impact on its authority and struggled to counter its effects. The illiterate populace is excluded from the strategic decision-making but indirectly impacted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_ambiguity,
    'To what extent did the inherent properties of the printing press (e.g., speed, reproducibility, cost-effectiveness) exert a deterministic influence on the Reformation, independent of human agency?',
    'Comparative historical analysis of other societies with similar technological innovations but different social/religious contexts, or counterfactual historical modeling.',
    'If a strong deterministic influence is found, the ''strategic deployment'' reading would be weakened, and the ''technological_determinism'' reading would gain credence, potentially reclassifying the press as a more ''mountain-like'' or ''snare-like'' force due to its inherent, unavoidable effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_ambiguity, conceptual, 'Ambiguity regarding the press''s inherent causal power versus its role as a neutral tool.').

omega_variable(
    mutual_shaping_vs_agency_primacy,
    'Was the relationship between the printing press and the Reformation purely one of strategic deployment, or did the technology and human agency mutually shape each other in a co-evolutionary process?',
    'Detailed micro-historical studies tracing the iterative feedback loops between printing innovations, textual content, and social reception, rather than linear cause-and-effect narratives.',
    'If mutual shaping is found to be dominant, the ''strategic deployment'' reading''s emphasis on agency primacy would be challenged, leading to a more nuanced classification that acknowledges the technology''s active role in shaping possibilities and constraints for human action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_shaping_vs_agency_primacy, conceptual, 'Ambiguity between agency-driven deployment and co-evolutionary mutual shaping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__strategic_deployment, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__strategic_deployment, theater_ratio, 1480, 0.05).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causation__strategic_deployment, theater_ratio, 1510, 0.05).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__strategic_deployment, theater_ratio, 1540, 0.05).
narrative_ontology:measurement(pres_tr_t1570, press_reformation_causation__strategic_deployment, theater_ratio, 1570, 0.05).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__strategic_deployment, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__strategic_deployment, theater_ratio, 1650, 0.05).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__strategic_deployment, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__strategic_deployment, base_extractiveness, 1480, 0.2).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causation__strategic_deployment, base_extractiveness, 1510, 0.23).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__strategic_deployment, base_extractiveness, 1540, 0.25).
narrative_ontology:measurement(pres_be_t1570, press_reformation_causation__strategic_deployment, base_extractiveness, 1570, 0.24).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__strategic_deployment, base_extractiveness, 1600, 0.23).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__strategic_deployment, base_extractiveness, 1650, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__strategic_deployment, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causation__strategic_deployment, suppression_requirement, 1480, 0.08).
narrative_ontology:measurement(pres_su_t1510, press_reformation_causation__strategic_deployment, suppression_requirement, 1510, 0.12).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causation__strategic_deployment, suppression_requirement, 1540, 0.18).
narrative_ontology:measurement(pres_su_t1570, press_reformation_causation__strategic_deployment, suppression_requirement, 1570, 0.15).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__strategic_deployment, suppression_requirement, 1600, 0.12).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causation__strategic_deployment, suppression_requirement, 1650, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, reformation_theological_disputes).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, rise_of_vernacular_languages).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'press_reformation_causation' kernel, emphasizing strategic human agency. It is linked to sibling readings that offer alternative causal explanations for the press's role in the Reformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
