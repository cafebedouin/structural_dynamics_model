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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Deployment of the Printing Press in the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint describes the strategic deployment of printing technology
 *   by reformers and printers during the early Reformation (1517-1560) to
 *   achieve religious and economic goals. It focuses on the agentic choices
 *   and coordinated actions that weaponized the press, rather than viewing
 *   the technology as an autonomous force. This deployment simultaneously
 *   served as a coordination tool for reformers and an extractive/suppressive
 *   force against the established Catholic Church's authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.78).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.85).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.78).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Deployment of the Printing Press in the Reformation").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, '35395e64-d530-419a-afa3-618743521c25').
narrative_ontology:cs_kernel_codification('35395e64-d530-419a-afa3-618743521c25', implicit).
narrative_ontology:cs_authority_grounding('35395e64-d530-419a-afa3-618743521c25', practice).
narrative_ontology:cs_interpretation_layer_present('35395e64-d530-419a-afa3-618743521c25').
narrative_ontology:cs_reading_relation('35395e64-d530-419a-afa3-618743521c25', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('35395e64-d530-419a-afa3-618743521c25', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('35395e64-d530-419a-afa3-618743521c25', foundational, human_agency_primary_driver).
narrative_ontology:cs_axiom_status(human_agency_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('35395e64-d530-419a-afa3-618743521c25', human_agency_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('35395e64-d530-419a-afa3-618743521c25', foundational, technology_as_instrument).
narrative_ontology:cs_axiom_status(technology_as_instrument, holdable).
narrative_ontology:cs_axiom_grounding('35395e64-d530-419a-afa3-618743521c25', technology_as_instrument, conventional).
narrative_ontology:cs_reference_frame('35395e64-d530-419a-afa3-618743521c25', agent_driven_historical_change).
narrative_ontology:cs_drift_state('35395e64-d530-419a-afa3-618743521c25', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('35395e64-d530-419a-afa3-618743521c25', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printers_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, vernacular_readers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, traditional_clergy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, secular_rulers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively used the printing press to disseminate their theological arguments, critiques of the Catholic Church, and vernacular Bibles. They coordinated their efforts through print, bypassing traditional Church control over information. They saw the press as a tool for liberation and truth.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformation_reformers, agenda_setter,
    organized, generational, mobile, continental).

% Profited significantly from the demand for Reformation texts, pamphlets, and Bibles. They formed alliances with reformers, often taking risks to print controversial material, but gaining economic advantage and influence. They were key enablers of the strategic deployment.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printers_publishers, beneficiary,
    organized, biographical, arbitrage, regional).

% Gained unprecedented access to religious texts and theological debates in their native languages, fostering individual interpretation and challenging clerical authority. Their participation was crucial for the spread of Reformation ideas, but their access was still mediated by literacy and availability.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, vernacular_readers, beneficiary,
    moderate, biographical, constrained, local).

% Suffered a severe loss of control over religious discourse and doctrine. They actively resisted the spread of Reformation ideas through censorship, book burning, and persecution, but their traditional mechanisms of control were overwhelmed by the volume and speed of print. They bore the costs of this strategic deployment.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, catholic_church_hierarchy, agenda_setter).

% Lost authority and influence as laypeople gained direct access to scripture and new theological interpretations. They were often caught between the demands of the Church hierarchy and the rising tide of Reformation sentiment among their parishioners. Their role was undermined by the strategic use of the press.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, traditional_clergy, payer,
    moderate, biographical, constrained, local).

% Observed the religious upheaval and often leveraged it to consolidate their own power against the Holy Roman Empire and the Papacy. Some supported reformers for political gain, while others suppressed them to maintain order. They were not direct agents of the press's strategic deployment but were profoundly affected by its outcomes.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, secular_rulers, observer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, secular_rulers, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled reformers to coordinate their theological arguments, disseminate their messages rapidly across vast distances, and mobilize public opinion against the established Church, creating a coherent movement out of disparate local efforts.
% TRANSFER_FUNCTION: Transferred control over religious narrative and interpretation from the centralized Catholic Church hierarchy to a decentralized network of reformers, printers, and vernacular readers, along with the economic gains from book production.
% ABSENT_VOICES: The illiterate populace, while indirectly benefiting from the spread of ideas, lacked direct agency in the strategic deployment itself. Their voices were largely mediated by literate reformers and printers. Also, those who wished for a more moderate, less schismatic reform were often drowned out by the polemical nature of print.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the printing press had not occurred, the Reformation as we know it would not have taken hold. The Catholic Church would have maintained its near-monopoly on information, theological dissent would have remained localized and easily suppressed, and the political and social landscape of Europe would have been fundamentally different.
% FOUNDING_PROBLEM: The Catholic Church's centralized control over religious doctrine, interpretation, and access to scripture, which reformers viewed as corrupt and unbiblical, preventing individual spiritual autonomy and reform.
% FOUNDING_PROBLEM_CORROBORATION: Historians widely corroborate that the problem of centralized Church control was the primary driver for the Reformation. While some contemporary religious issues persist, the specific problem of a monolithic, unchallengeable Church authority over information was fundamentally broken by the strategic use of print. Independent historical analyses and contemporary accounts from outside the benefiting parties (e.g., records of Church councils attempting to reassert control) support this shifted-function reading.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because the strategic deployment fundamentally undermined the Church's monopoly on information and its associated power, effectively extracting its authority and transferring it to new actors. Suppression is very high as the Church's attempts to counter this information flow through censorship and persecution were largely ineffective against the decentralized and rapid spread of print. Resistance is also very high, reflecting the intense conflict between the Church and the reformers. Theater ratio is low because the deployment was highly functional and effective in achieving its goals, with little performative maintenance. Accessibility collapse is moderate overall: while it collapsed the Church's traditional alternatives for information control, it simultaneously opened new avenues for reformers and readers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformers and printers, the printing press was a 'rope'—a powerful coordination tool for spreading truth and building a movement. From the perspective of the Catholic Church, the strategic deployment of the press was a 'snare'—a coercive mechanism that undermined its authority and extracted its power. This constraint, 'strategic_deployment', captures both aspects, classifying it as a Tangled Rope due to its dual function of coordination for some and extraction/suppression for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformation reformers and printers were clear beneficiaries, actively using the press to their advantage (low d). Vernacular readers also benefited from increased access to information. The Catholic Church hierarchy and traditional clergy were the primary targets, experiencing significant extraction of authority and control (high d). Secular rulers were observers who could choose to benefit or suppress, depending on their political goals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_autonomy_of_technology,
    'To what extent was the printing press an autonomous force shaping the Reformation, versus a tool strategically deployed by human agents?',
    'Further historical analysis comparing the impact of the press in other contexts where strategic agency was less pronounced, or counterfactual historical modeling.',
    'If the press''s autonomous influence was greater, this constraint''s classification might shift towards a more ''mountain-like'' or ''rope-like'' character for the technology itself, with less emphasis on the ''tangled'' aspects of strategic deployment. If agency is confirmed as primary, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_autonomy_of_technology, empirical, 'Ambiguity regarding the balance of technological determinism versus human agency in historical outcomes.').

omega_variable(
    strategic_vs_emergent_outcomes,
    'How much of the Reformation''s outcome was due to deliberate strategic planning by reformers, versus emergent properties of a complex system involving technology, politics, and religion?',
    'Detailed micro-historical studies tracing specific decisions and their immediate and long-term consequences, compared against broader systemic analyses.',
    'If emergent properties played a larger role, the ''strategic deployment'' framing might be seen as overstating agency, potentially leading to a re-evaluation of the constraint''s extractiveness and suppression as less ''intentional'' and more ''systemic''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_emergent_outcomes, empirical, 'Distinguishing planned strategic outcomes from unforeseen emergent effects in historical processes.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''strategic_deployment'' reading of the ''press_reformation_causality'' kernel. What would change if a sibling reading (e.g., ''technological_determinism'' or ''co_constitution'') were adopted?',
    'Adopting a different interpretive framework for historical causality.',
    'The ''technological_determinism'' reading would likely classify the press itself as a Mountain or Rope, with lower extractiveness attributed to human agents. The ''co_constitution'' reading would emphasize feedback loops, potentially leading to a more nuanced Tangled Rope classification with distributed agency and extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative readings of the press''s role in the Reformation on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1517, 1560).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__strategic_deployment, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__strategic_deployment, theater_ratio, 1525, 0.12).
narrative_ontology:measurement(pres_tr_t1535, press_reformation_causality__strategic_deployment, theater_ratio, 1535, 0.1).
narrative_ontology:measurement(pres_tr_t1545, press_reformation_causality__strategic_deployment, theater_ratio, 1545, 0.09).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causality__strategic_deployment, theater_ratio, 1560, 0.1).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.5).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__strategic_deployment, base_extractiveness, 1525, 0.65).
narrative_ontology:measurement(pres_be_t1535, press_reformation_causality__strategic_deployment, base_extractiveness, 1535, 0.72).
narrative_ontology:measurement(pres_be_t1545, press_reformation_causality__strategic_deployment, base_extractiveness, 1545, 0.76).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causality__strategic_deployment, base_extractiveness, 1560, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.6).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causality__strategic_deployment, suppression_requirement, 1525, 0.7).
narrative_ontology:measurement(pres_su_t1535, press_reformation_causality__strategic_deployment, suppression_requirement, 1535, 0.78).
narrative_ontology:measurement(pres_su_t1545, press_reformation_causality__strategic_deployment, suppression_requirement, 1545, 0.82).
narrative_ontology:measurement(pres_su_t1560, press_reformation_causality__strategic_deployment, suppression_requirement, 1560, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, church_doctrinal_authority).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, vernacular_literacy_spread).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, rise_of_nation_states).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causality' kernel, focusing on the agentic, strategic deployment of the printing press. It is linked to its sibling readings ('technological_determinism' and 'co_constitution') through the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
