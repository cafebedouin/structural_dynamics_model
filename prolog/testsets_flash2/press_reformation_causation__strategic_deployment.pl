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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Printing Press as Strategic Deployment Tool (Reformation)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'strategic_deployment' reading of
 *   the 'press_reformation_causation' kernel. It posits that the printing
 *   press was a neutral technology, a 'tool' whose impact on the Reformation
 *   was primarily determined by the conscious, strategic choices of reformers
 *   and printers. The press itself is classified as a Rope, facilitating
 *   coordination and information transfer with minimal inherent extraction,
 *   but its *deployment* was a strategic act by agents seeking to achieve
 *   their goals. This reading emphasizes human agency as the upstream driver
 *   of historical change, with technology serving as a capacity amplifier.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.15).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.1).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Printing Press as Strategic Deployment Tool (Reformation)").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '6f4143f5-6db1-40e2-aacd-9b4788caeb12').
narrative_ontology:cs_kernel_codification('6f4143f5-6db1-40e2-aacd-9b4788caeb12', implicit).
narrative_ontology:cs_authority_grounding('6f4143f5-6db1-40e2-aacd-9b4788caeb12', expertise).
narrative_ontology:cs_interpretation_layer_present('6f4143f5-6db1-40e2-aacd-9b4788caeb12').
narrative_ontology:cs_reading_relation('6f4143f5-6db1-40e2-aacd-9b4788caeb12', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('6f4143f5-6db1-40e2-aacd-9b4788caeb12', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('6f4143f5-6db1-40e2-aacd-9b4788caeb12', foundational, technology_is_neutral_tool).
narrative_ontology:cs_axiom_status(technology_is_neutral_tool, holdable).
narrative_ontology:cs_axiom_grounding('6f4143f5-6db1-40e2-aacd-9b4788caeb12', technology_is_neutral_tool, empirically_contingent).
narrative_ontology:cs_axiom('6f4143f5-6db1-40e2-aacd-9b4788caeb12', foundational, human_agency_is_primary_causal_driver).
narrative_ontology:cs_axiom_status(human_agency_is_primary_causal_driver, holdable).
narrative_ontology:cs_axiom_grounding('6f4143f5-6db1-40e2-aacd-9b4788caeb12', human_agency_is_primary_causal_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('6f4143f5-6db1-40e2-aacd-9b4788caeb12', pre_existing_reform_agendas).
narrative_ontology:cs_drift_state('6f4143f5-6db1-40e2-aacd-9b4788caeb12', contemporary_historical_scholarship, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6f4143f5-6db1-40e2-aacd-9b4788caeb12', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, printers_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively used the printing press to disseminate their theological arguments, vernacular Bibles, and polemics. They saw the press as a neutral, powerful instrument to achieve their pre-existing goals, benefiting from its capacity to reach a mass audience and coordinate their movement.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Saw the Reformation as a massive new market for their products. They actively sought out and published reformist texts, profiting from the demand for printed materials. Their strategic choices in content and distribution amplified the Reformation's reach.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, printers_publishers, beneficiary,
    moderate, biographical, arbitrage, regional).

% Was the target of much of the reformist printing. While they also used the press, their existing institutional structures and slower adaptation meant they bore the cost of a rapidly changing information environment that undermined their authority and control over religious discourse. They were forced to react to a dynamic they did not initiate.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_church, payer,
    institutional, civilizational, constrained, global).

% Did not directly interact with the printed word but were influenced by its downstream effects through sermons, public readings, and visual media derived from printed texts. Their access to information was mediated, and they had no direct agency in the strategic deployment of the press.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, illiterate_populace, excluded,
    powerless, immediate, trapped, local).

% Analyze the role of the printing press in historical events, debating the extent of technological determinism versus human agency. This reading emphasizes the latter, viewing the press as a tool whose impact was shaped by the intentions and actions of its users.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, historians_of_technology, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press provided a highly efficient means for reformers to coordinate their message, mobilize support, and disseminate ideas across vast distances, enabling a collective action problem of religious reform to be solved.
% TRANSFER_FUNCTION: Transferred information, theological arguments, and religious authority from a centralized, elite-controlled system (Catholic Church) to a decentralized, mass-accessible system (Protestant reformers and printers), enabling a transfer of influence and power.
% ABSENT_VOICES: Those who believed the press inherently shaped society (technological determinists) or that technology and society co-evolved (mutual shapers) are absent from this reading's core assertion of neutral capacity and strategic use. They would argue that the press was not merely a tool but an active force or co-creator of the Reformation.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the press had not occurred, the Reformation would have been a vastly different, likely much slower and less widespread, phenomenon. The rapid spread of ideas and coordination of reformers depended critically on this active, purposeful use of the technology.
% FOUNDING_PROBLEM: The problem of disseminating complex theological arguments and coordinating a large-scale religious movement across a continent with existing communication limitations and institutional resistance.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reformation widely corroborate that the problem of mass communication and coordination was acute. However, the 'strategic deployment' reading emphasizes that the *solution* was not inherent in the technology but in the deliberate choices of reformers and printers, a view supported by numerous historical analyses of agency in technological adoption.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because the press itself, in this reading, is a neutral capacity; any 'extraction' is from the efficiency gains of coordination, not from the technology itself. Suppression is also low (0.1) as the press's power came from its widespread adoption and the inability of existing powers to fully control its use, rather than active suppression by the technology. Theater ratio is negligible (0.05) as the press was a highly functional tool. The claimed type is Rope, reflecting its role as a coordination mechanism that primarily benefited its users through efficiency and reach.
 *
 * PERSPECTIVAL GAP:
 *   This reading's emphasis on agency and technological neutrality creates a significant perspectival gap with deterministic readings, which would see the press as an active, causal force. From the perspective of reformers and printers, the press was a powerful, enabling tool. From the perspective of the Catholic Church, it was a disruptive force that undermined their established order, regardless of specific strategic choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers and printers are clear beneficiaries (d near 0.0) as they actively exploited the press for their goals, gaining influence and profit. The Catholic Church is positioned as a payer (d near 1.0) as it bore the costs of adapting to a new information environment that challenged its authority, without initiating the change. The illiterate populace is excluded, as their interaction with the press was indirect and mediated.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a Rope, is not subject to mandatrophy in the same way as an extractive constraint. Its function as a coordination tool for strategic deployment remained live throughout the period. The question is not whether its mandate atrophied, but whether its 'neutral tool' framing accurately captures its historical role, which is addressed by the kernel's sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint truly an ''ε-invariant'' reading of the press_reformation_causation kernel, or does it conflate distinct claims?',
    'Detailed historical analysis of primary sources to identify whether reformers/printers consistently articulated the press as a ''neutral tool'' versus a ''co-shaping force'' or ''deterministic agent''.',
    'If conflated, the story would need decomposition into further, more granular constraints, each with its own ε and classification. If confirmed as a distinct reading, it strengthens the corpus''s ability to model perspectival divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this story as a distinct reading within the press_reformation_causation kernel.').

omega_variable(
    technological_neutrality_ambiguity,
    'Was the printing press truly a ''neutral capacity'' awaiting purposeful use, or did its inherent characteristics (e.g., reproducibility, fixed text) subtly shape its deployment and impact in ways not fully controlled by human agency?',
    'Comparative historical analysis with other communication technologies and their societal impacts, or counterfactual history exploring alternative deployments.',
    'If the press had inherent shaping properties, the ''strategic_deployment'' reading''s claim of neutrality would be weakened, potentially shifting its classification closer to a ''mutual_shaping'' perspective, where the technology itself contributes to the coordination and extraction dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_neutrality_ambiguity, empirical, 'Examines the extent of the press''s ''neutrality'' as a tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__strategic_deployment, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__strategic_deployment, theater_ratio, 1500, 0.03).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__strategic_deployment, theater_ratio, 1550, 0.05).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__strategic_deployment, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__strategic_deployment, theater_ratio, 1650, 0.05).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__strategic_deployment, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__strategic_deployment, base_extractiveness, 1500, 0.1).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__strategic_deployment, base_extractiveness, 1550, 0.15).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__strategic_deployment, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__strategic_deployment, base_extractiveness, 1650, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__strategic_deployment, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__strategic_deployment, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__strategic_deployment, suppression_requirement, 1550, 0.1).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__strategic_deployment, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causation__strategic_deployment, suppression_requirement, 1650, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causation' kernel, alongside 'technological_determinism' and 'mutual_shaping'. Each reading offers a distinct causal account of the press's role in the Reformation, with different structural implications for the technology itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
