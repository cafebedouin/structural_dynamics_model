% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Deployment of Printing Press in Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint describes the strategic deployment of printing technology
 *   by reformers and printers during the Reformation era (1517-1648). It
 *   emphasizes human agency in weaponizing the press to achieve religious and
 *   economic goals, leading to a significant challenge to the Catholic
 *   Church's authority. The constraint functions as a coordination mechanism
 *   for reformers and printers, while simultaneously acting as an extractive
 *   force (a 'snare') against the Church's control over information and
 *   doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.78).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.85).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.78).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Deployment of Printing Press in Reformation").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'b8783378-4a86-4878-a72b-b6d0032a1484').
narrative_ontology:cs_kernel_codification('b8783378-4a86-4878-a72b-b6d0032a1484', implicit).
narrative_ontology:cs_authority_grounding('b8783378-4a86-4878-a72b-b6d0032a1484', distributed).
narrative_ontology:cs_reading_relation('b8783378-4a86-4878-a72b-b6d0032a1484', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('b8783378-4a86-4878-a72b-b6d0032a1484', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('b8783378-4a86-4878-a72b-b6d0032a1484', foundational, human_agency_primary_driver).
narrative_ontology:cs_axiom_status(human_agency_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('b8783378-4a86-4878-a72b-b6d0032a1484', human_agency_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('b8783378-4a86-4878-a72b-b6d0032a1484', foundational, technology_as_neutral_tool).
narrative_ontology:cs_axiom_status(technology_as_neutral_tool, holdable).
narrative_ontology:cs_axiom_grounding('b8783378-4a86-4878-a72b-b6d0032a1484', technology_as_neutral_tool, conventional).
narrative_ontology:cs_reference_frame('b8783378-4a86-4878-a72b-b6d0032a1484', agent_driven_historical_change).
narrative_ontology:cs_drift_state('b8783378-4a86-4878-a72b-b6d0032a1484', contemporary_historiography, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b8783378-4a86-4878-a72b-b6d0032a1484', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, secular_rulers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_church_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively used the printing press to disseminate their theological arguments, critiques of the Catholic Church, and vernacular Bibles. They coordinated printing efforts and distribution networks to maximize reach and impact, directly challenging established religious authority.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformation_reformers, agenda_setter,
    organized, generational, constrained, regional).

% Profited significantly from the demand for Reformation texts. They often aligned with reformers for economic gain, sometimes taking risks to print controversial material. Their business model was directly enhanced by the strategic deployment of the press.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printers, beneficiary,
    moderate, biographical, mobile, local).

% Suffered a severe erosion of its monopoly on information and religious interpretation. Its efforts to suppress dissenting texts through censorship and the Index Librorum Prohibitorum were largely ineffective against the decentralized and rapid spread of printed material, leading to loss of authority and revenue.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_church_authority, payer,
    institutional, civilizational, trapped, universal).

% Often supported reformers and printers, seeing an opportunity to weaken the power of the Catholic Church and consolidate their own political and economic control within their territories. They benefited from the shift in power dynamics enabled by the press.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, secular_rulers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, secular_rulers, agenda_setter).

% Were largely excluded from direct engagement with printed texts due to illiteracy, but were indirectly affected by the spread of Reformation ideas through sermons, pamphlets read aloud, and visual propaganda. Their worldview was reshaped by the conflict, but they were not active agents in the strategic deployment.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, illiterate_populace, excluded,
    powerless, immediate, trapped, local).

% An analytical perspective that views the printing press as an autonomous force that inevitably led to the Reformation, rather than a tool strategically wielded by agents. This perspective is a sibling reading of the kernel.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, technological_determinists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, reformation_reformers).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled reformers to coordinate the rapid, widespread dissemination of their ideas and critiques, bypassing traditional channels of information control and reaching a mass audience across diverse geographic regions.
% TRANSFER_FUNCTION: Transferred control over religious discourse and information dissemination from the centralized Catholic Church hierarchy to a decentralized network of reformers and printers, along with associated economic and political power.
% ABSENT_VOICES: Those who wished to maintain the pre-Reformation religious and social order, particularly within the Catholic hierarchy, were present but increasingly marginalized in the public discourse shaped by print. Their attempts at suppression were outmaneuvered by the speed and scale of print dissemination.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the printing press had not occurred, the Reformation would likely have remained a localized theological dispute, lacking the means to challenge the Church's authority on a continental scale. The religious, political, and social landscape of early modern Europe would have been fundamentally different.
% FOUNDING_PROBLEM: The Catholic Church held a near-monopoly on religious interpretation and information dissemination, which reformers sought to break to advance their theological and social agendas.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historians and media scholars widely corroborate that the Church's information monopoly was decisively broken by the Reformation, rendering the original problem 'dead' in its initial form. While religious authority remains, its structure and contestability are fundamentally altered from the pre-print era.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because the deployment fundamentally undermined the Church's power and diverted significant resources (e.g., tithes, indulgences) and influence. Suppression (0.85) is also high, reflecting the Church's inability to effectively counter the decentralized print networks, despite active enforcement efforts like censorship and the Index. The theater ratio is low (0.15) because the deployment was highly functional and effective in achieving its goals, with little performative maintenance. Accessibility collapse (0.65) reflects the partial but significant collapse of the Church's monopoly on information, while resistance (0.70) indicates the strong, though ultimately outmaneuvered, opposition from the Church.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformers and printers, the strategic deployment was a necessary and effective tool for liberation and truth-telling (a 'rope'). From the perspective of the Catholic Church, it was a destructive, illegitimate force (a 'snare'). This reading acknowledges both the coordination function for some and the extractive function for others, leading to a 'tangled_rope' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformation reformers and printers are clear beneficiaries and agenda-setters, actively shaping and profiting from the constraint's operation. Secular rulers also benefited by leveraging the religious upheaval to consolidate their own power. The Catholic Church authority is the primary victim, experiencing a profound loss of control and influence. The illiterate populace is largely excluded from direct agency but bears the indirect costs and benefits of the societal upheaval.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to challenge and dismantle the Church's information monopoly. While the specific 'problem' of a single, unchallenged religious authority is 'dead' in its original form, the strategic deployment was highly successful in achieving its goals, preventing it from becoming a 'piton'. The ongoing contestation over the 'founding problem status' reflects the enduring impact and differing interpretations of the Reformation's legacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinction,
    'How much of the Reformation''s impact was due to the strategic deployment of the press by agents, versus the press''s inherent technological properties (technological_determinism) or a co-constitutive feedback loop (co_constitution)?',
    'Comparative historical analysis across different technological adoptions and social contexts, and counterfactual historical modeling.',
    'If technological determinism were primary, the press would be classified closer to a ''mountain'' or ''rope'' with less emphasis on agent-driven extraction. If co-constitution were primary, the classification would emphasize dynamic feedback loops rather than unidirectional strategic deployment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing the causal role of agency vs. technology in the Reformation.').

omega_variable(
    extent_of_strategic_intent,
    'To what extent were the actions of reformers and printers truly ''strategic'' and coordinated, versus emergent properties of a new media ecosystem?',
    'Detailed archival research into communication networks, explicit planning documents, and financial records of printing operations.',
    'If less strategic intent is found, the ''tangled_rope'' classification might shift towards a ''rope'' (more emergent coordination) or even a ''piton'' (if the coordination function atrophied into mere inertia), with lower suppression and extractiveness attributed to deliberate action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_strategic_intent, empirical, 'Assessing the degree of conscious strategic planning in press deployment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__strategic_deployment, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__strategic_deployment, theater_ratio, 1540, 0.12).
narrative_ontology:measurement(pres_tr_t1565, press_reformation_causality__strategic_deployment, theater_ratio, 1565, 0.14).
narrative_ontology:measurement(pres_tr_t1590, press_reformation_causality__strategic_deployment, theater_ratio, 1590, 0.15).
narrative_ontology:measurement(pres_tr_t1615, press_reformation_causality__strategic_deployment, theater_ratio, 1615, 0.15).
narrative_ontology:measurement(pres_tr_t1648, press_reformation_causality__strategic_deployment, theater_ratio, 1648, 0.15).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.4).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__strategic_deployment, base_extractiveness, 1540, 0.6).
narrative_ontology:measurement(pres_be_t1565, press_reformation_causality__strategic_deployment, base_extractiveness, 1565, 0.7).
narrative_ontology:measurement(pres_be_t1590, press_reformation_causality__strategic_deployment, base_extractiveness, 1590, 0.75).
narrative_ontology:measurement(pres_be_t1615, press_reformation_causality__strategic_deployment, base_extractiveness, 1615, 0.77).
narrative_ontology:measurement(pres_be_t1648, press_reformation_causality__strategic_deployment, base_extractiveness, 1648, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.5).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__strategic_deployment, suppression_requirement, 1540, 0.68).
narrative_ontology:measurement(pres_su_t1565, press_reformation_causality__strategic_deployment, suppression_requirement, 1565, 0.78).
narrative_ontology:measurement(pres_su_t1590, press_reformation_causality__strategic_deployment, suppression_requirement, 1590, 0.82).
narrative_ontology:measurement(pres_su_t1615, press_reformation_causality__strategic_deployment, suppression_requirement, 1615, 0.84).
narrative_ontology:measurement(pres_su_t1648, press_reformation_causality__strategic_deployment, suppression_requirement, 1648, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, enforcement_mechanism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('strategic_deployment') of the 'press_reformation_causality' kernel, emphasizing human agency. It is linked to sibling readings that offer alternative causal explanations for the Reformation's relationship with the printing press.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
