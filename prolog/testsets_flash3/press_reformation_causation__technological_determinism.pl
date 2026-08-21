% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint represents the 'technological determinism' reading of the
 *   printing press's role in the Reformation. In this view, the printing
 *   press is an autonomous technological force (a Mountain) whose inherent
 *   properties (mass production, decentralization) made the Reformation's
 *   outcomes (censorship failure, vernacular scripture) inevitable. The
 *   technology itself is the 'agenda-setter,' and its 'beneficiaries'
 *   (reformers, vernacular readers) simply ride its wave. The Catholic
 *   Church, as the 'payer,' is depicted as futilely resisting an unstoppable
 *   technological tide. The low extractiveness and theater ratio, combined
 *   with high suppression (of prior information control) and accessibility
 *   collapse, reflect the view that the technology's impact was a natural,
 *   unmediated force.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.95).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'fb099056-1a1b-4934-910f-64f0e179038d').
narrative_ontology:cs_kernel_codification('fb099056-1a1b-4934-910f-64f0e179038d', implicit).
narrative_ontology:cs_authority_grounding('fb099056-1a1b-4934-910f-64f0e179038d', diffuse_epistemic).
narrative_ontology:cs_reading_relation('fb099056-1a1b-4934-910f-64f0e179038d', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('fb099056-1a1b-4934-910f-64f0e179038d', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('fb099056-1a1b-4934-910f-64f0e179038d', foundational, technology_as_autonomous_cause).
narrative_ontology:cs_axiom_status(technology_as_autonomous_cause, holdable).
narrative_ontology:cs_axiom_grounding('fb099056-1a1b-4934-910f-64f0e179038d', technology_as_autonomous_cause, empirically_contingent).
narrative_ontology:cs_axiom('fb099056-1a1b-4934-910f-64f0e179038d', foundational, social_change_as_technologically_determined).
narrative_ontology:cs_axiom_status(social_change_as_technologically_determined, holdable).
narrative_ontology:cs_axiom_grounding('fb099056-1a1b-4934-910f-64f0e179038d', social_change_as_technologically_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('fb099056-1a1b-4934-910f-64f0e179038d', technological_autonomy_paradigm).
narrative_ontology:cs_drift_state('fb099056-1a1b-4934-910f-64f0e179038d', contemporary_social_construction_of_technology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fb099056-1a1b-4934-910f-64f0e179038d', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The technology itself, acting as an autonomous force that dictates social outcomes. It inherently enables mass production of texts and decentralizes information dissemination, making previous control mechanisms obsolete.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, printing_press_technology).

% Benefited from the press's inherent capacity to rapidly disseminate their ideas and vernacular translations of scripture, bypassing traditional gatekeepers. Their success is seen as a direct consequence of the technology's affordances.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, biographical, arbitrage, regional).

% Its traditional authority over information and interpretation was fundamentally undermined by the printing press. Efforts to censor or control the spread of dissenting ideas were rendered futile by the technology's inherent decentralizing power, leading to a loss of control and influence.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church, payer,
    institutional, civilizational, trapped, global).

% Gained unprecedented access to religious texts in their own languages, fostering individual interpretation and reducing reliance on clerical intermediaries. This access was an inevitable outcome of the press's operation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the causal links between technological innovation and societal change, often emphasizing the autonomous power of technology to shape historical trajectories.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, historians_of_technology, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press inherently coordinated the rapid, widespread dissemination of information and ideas across Europe, enabling a new form of public discourse that transcended previous geographical and social barriers.
% TRANSFER_FUNCTION: Transferred the power of information control from centralized religious and political authorities to a more distributed network of printers, authors, and readers, making knowledge more accessible and less subject to gatekeeping.
% ABSENT_VOICES: Those who believed in the inherent neutrality of technology, or the primacy of human agency in shaping its impact, are absent from this deterministic framing. They would argue that the press was a tool whose effects depended on how it was used, not an autonomous cause.
% DISAPPEARANCE_RATIONALE: If the deterministic causal link vanished, the historical events of the Reformation would still have occurred, but their explanation would shift from technological inevitability to a more complex interplay of social, political, and religious factors. The 'world' of historical outcomes would remain, but its 'explanation' would rearrange.
% FOUNDING_PROBLEM: The problem of slow, expensive, and centrally controlled information dissemination in pre-modern Europe.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and media studies scholars, particularly those aligned with deterministic views, attest that the problem of information scarcity was fundamentally 'solved' by the press, leading to the inevitable outcomes observed. Critics from other schools of thought contest this, arguing the problem was reframed, not simply solved, and that agency remained paramount.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects the idea that the technology itself doesn't 'extract' in a human-centric sense, but rather imposes its own logic. The high suppression (0.95) signifies the complete and inevitable suppression of previous, centralized information control mechanisms by the press's inherent capabilities. The near-zero theater ratio (0.01) indicates that the press's effects are seen as direct and functional, with no performative or symbolic overlay. Accessibility collapse (0.98) is near total, as the press fundamentally altered the landscape of information access, making alternatives to mass-produced texts nearly unthinkable. Resistance (0.02) is negligible because, in this deterministic view, resistance to the press's fundamental impact was largely futile.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the technology itself (as an 'agent' in this deterministic reading), its operation is simply a natural unfolding of its capabilities. From the perspective of the reformers, it is an unalloyed benefit. From the perspective of the Catholic Church, it is an unstoppable force that extracts its authority. The engine's classification will highlight how this 'Mountain' of technology is experienced as a force that benefits some and extracts from others, despite its 'natural' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The printing press technology itself is framed as the ultimate 'agenda-setter,' dictating the terms of information flow. Protestant reformers and vernacular readers are 'beneficiaries' because the technology's inherent properties directly enabled their goals. The Catholic Church is the 'payer' because its traditional power structure was directly undermined and forced to bear the costs of a new information regime it could not control. This reading emphasizes the technology's autonomous power, making its beneficiaries passive recipients of its effects.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by asserting the press's impact as a 'natural' technological force, rather than a constructed constraint. The low extractiveness and theater ratio, combined with high suppression of prior systems, argue against it being a Snare or Tangled Rope. The 'emerges_naturally: true' flag, coupled with beneficiaries, triggers the False Summit Mountain detection, which will highlight the tension between the claimed naturalness and the identifiable beneficiaries of this 'natural' process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_determinism,
    'To what extent was the printing press an autonomous cause of the Reformation, versus a tool strategically deployed by human agents?',
    'Comparative historical analysis of regions with similar printing access but different Reformation outcomes, or detailed studies of reformers'' strategic choices in utilizing the press.',
    'If agency is found to be primary, the constraint shifts from a Mountain (autonomous technology) to a Rope or Tangled Rope (technology as a coordination mechanism or tool), with extractiveness tied to the strategic choices of its users. This would align with the ''strategic_deployment'' sibling reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agency_vs_determinism, conceptual, 'Ambiguity between technological determinism and human agency in historical causation.').

omega_variable(
    inevitability_of_vernacular_scripture,
    'Was the widespread adoption of vernacular scripture an inevitable outcome of the printing press, or a contingent development shaped by theological and political choices?',
    'Examination of historical counterfactuals or regions where printing existed but vernacular scripture did not achieve similar dominance due to active suppression or different cultural priorities.',
    'If contingent, the ''inevitability'' claim of this reading weakens, suggesting that the ''suppression'' of Latin-only scripture was not purely technological but also a result of active human choices, potentially reclassifying the constraint as a Tangled Rope or Snare depending on the nature of those choices. This would align with the ''mutual_shaping'' sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_of_vernacular_scripture, empirical, 'Whether vernacular scripture was a technologically determined outcome or a contingent historical development.').

omega_variable(
    natural_law_vs_constructed_causality,
    'Is the causal link between the printing press and the Reformation a ''natural law'' of technological impact, or a constructed historical narrative that benefits identifiable agents (e.g., those who champion technological progress as an autonomous force)?',
    'Critical historiographical analysis of the origins and persistence of technological deterministic narratives, identifying the intellectual and social contexts in which they gained prominence.',
    'If constructed, the ''mountain'' classification is a ''false summit,'' and the constraint would be reclassified as a Tangled Rope or Snare, with the ''beneficiaries'' of the narrative (e.g., certain schools of historical thought) being the primary extractors of its legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_causality, conceptual, 'Whether the technological determinism claim is a natural law or a constructed narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(pres_tr_t1475, press_reformation_causation__technological_determinism, theater_ratio, 1475, 0.01).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causation__technological_determinism, theater_ratio, 1525, 0.01).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.01).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.01).
narrative_ontology:measurement(pres_be_t1475, press_reformation_causation__technological_determinism, base_extractiveness, 1475, 0.02).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causation__technological_determinism, base_extractiveness, 1525, 0.04).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.99).
narrative_ontology:measurement(pres_su_t1475, press_reformation_causation__technological_determinism, suppression_requirement, 1475, 0.98).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.97).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__technological_determinism, suppression_requirement, 1525, 0.96).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causation' kernel. This 'technological_determinism' reading emphasizes the autonomous power of the printing press. It is linked to the 'strategic_deployment' and 'mutual_shaping' readings, which offer alternative explanations for the press's role in the Reformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
