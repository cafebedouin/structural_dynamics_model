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
 *   printing press's role in the Reformation. It posits the printing press as
 *   an autonomous technological force (a 'mountain') whose inherent
 *   properties made the Reformation's success, the breakdown of censorship,
 *   and the rise of vernacular scripture inevitable. Reformers and readers
 *   are beneficiaries of this exogenous technological capacity, while the
 *   Catholic Church is a victim, unable to resist the press's inherent power.
 *   The metrics reflect this deterministic view: low extractiveness and
 *   suppression, high accessibility collapse (alternatives to the press's
 *   impact were inherently limited).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.02).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Inevitable Cause of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '0de388fd-a942-4432-ae5d-7400cc266ca4').
narrative_ontology:cs_kernel_codification('0de388fd-a942-4432-ae5d-7400cc266ca4', implicit).
narrative_ontology:cs_authority_grounding('0de388fd-a942-4432-ae5d-7400cc266ca4', diffuse_epistemic).
narrative_ontology:cs_reading_relation('0de388fd-a942-4432-ae5d-7400cc266ca4', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('0de388fd-a942-4432-ae5d-7400cc266ca4', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('0de388fd-a942-4432-ae5d-7400cc266ca4', foundational, technology_as_autonomous_force).
narrative_ontology:cs_axiom_status(technology_as_autonomous_force, holdable).
narrative_ontology:cs_axiom_grounding('0de388fd-a942-4432-ae5d-7400cc266ca4', technology_as_autonomous_force, empirically_contingent).
narrative_ontology:cs_axiom('0de388fd-a942-4432-ae5d-7400cc266ca4', foundational, social_change_as_technologically_determined).
narrative_ontology:cs_axiom_status(social_change_as_technologically_determined, holdable).
narrative_ontology:cs_axiom_grounding('0de388fd-a942-4432-ae5d-7400cc266ca4', social_change_as_technologically_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('0de388fd-a942-4432-ae5d-7400cc266ca4', gutenberg_revolution_inevitability).
narrative_ontology:cs_drift_state('0de388fd-a942-4432-ae5d-7400cc266ca4', contemporary_sts_scholarship, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('0de388fd-a942-4432-ae5d-7400cc266ca4', '').
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

% The printing press itself, as an autonomous technological force, is seen as the primary driver, making certain outcomes inevitable regardless of human agency. It 'benefits' by being the unchallengeable cause.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_press_technology, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, printing_press_technology).

% Benefited from the press's inherent capacity to disseminate their ideas and vernacular Bibles widely and rapidly, overcoming traditional censorship mechanisms. They were downstream recipients of an exogenous technological shift.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, generational, arbitrage, continental).

% Was unable to effectively suppress the spread of dissenting ideas and vernacular texts due to the printing press's inherent properties. Their traditional control over information was rendered obsolete by the technology itself, leading to a loss of authority and fragmentation of religious unity.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church, payer,
    institutional, civilizational, trapped, global).

% Gained unprecedented access to religious texts in their own languages, fostering individual interpretation and undermining the Church's monopoly on scripture. This access was an inevitable consequence of the press's operation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the historical relationship between technology and social change, often interpreting the printing press as a deterministic force in the Reformation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press inherently coordinated the rapid, widespread, and decentralized dissemination of information, enabling a new form of public discourse that bypassed traditional gatekeepers.
% TRANSFER_FUNCTION: Transferred the power of information control from centralized religious and political authorities to a more distributed network of printers, authors, and readers, making censorship ineffective and vernacular literacy inevitable.
% ABSENT_VOICES: Those who believed in the inherent neutrality of technology, or the primacy of human agency in shaping technological outcomes, are absent from this deterministic account. They would argue that the press was a tool whose impact depended on how it was strategically deployed.
% DISAPPEARANCE_RATIONALE: If the deterministic causal link vanished, the historical fact of the Reformation would remain, but its explanation would shift from technological inevitability to one emphasizing human agency, strategic choices, or mutual shaping. The 'world' of historical interpretation would rearrange, but the events themselves would not disappear.
% FOUNDING_PROBLEM: The problem of how to explain the rapid and widespread success of the Reformation, particularly its ability to overcome entrenched institutional resistance.
% FOUNDING_PROBLEM_CORROBORATION: Historians and media theorists continue to debate the extent of technological determinism in historical events. While some scholars still emphasize the press's inherent power, others (outside the deterministic camp) offer alternative explanations, indicating the problem is still actively contested in academic discourse.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

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
 *   The low extractiveness (0.05) and suppression (0.02) reflect the view that the press itself was not 'extracting' from anyone in a coercive sense, but rather was an unstoppable force whose effects were inherent. The high accessibility collapse (0.95) signifies that once the press existed, the alternatives (e.g., maintaining traditional censorship) became almost impossible. Resistance is low (0.05) because, in this reading, attempts to resist the press's effects were largely futile. The claimed type is 'mountain' because the technology is presented as an unchangeable, natural-law-like force in this specific reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Catholic Church, the constraint was a devastating, unavoidable force. From the reformers' perspective, it was a providential tool. The deterministic reading emphasizes the inevitability of the outcome, minimizing the role of human choice or strategic action in shaping the press's impact.
 *
 * DIRECTIONALITY LOGIC:
 *   The printing_press_technology itself is framed as the ultimate beneficiary, as its inherent properties are vindicated. Protestant reformers and vernacular readers are direct beneficiaries, as the press enabled their goals. The Catholic Church is the primary payer/victim, as its authority and control were undermined by the press's inherent capabilities. Historical observers are analytical, seeking to understand this deterministic causal link.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a 'mountain' of technological determinism, is not subject to mandatrophy in the traditional sense, as it describes an inherent causal link. However, the 'false summit mountain' detection would be relevant if the 'naturalness' of this deterministic link were challenged, revealing underlying human choices or power dynamics that shaped the technology's impact. The presence of beneficiaries on a claimed mountain triggers this FSM analysis, prompting an examination of whether the 'inevitability' is truly natural or a constructed narrative benefiting certain groups (e.g., those who champion technological progress as an autonomous force).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_autonomy_vs_agency,
    'Is the printing press truly an autonomous force with inherent, deterministic effects, or was its impact mediated and shaped by human agency and strategic choices?',
    'Historical analysis focusing on specific instances of printers'' and reformers'' strategic deployment of the press, and the varying effectiveness of censorship efforts across different regions and times.',
    'If human agency is found to be primary, the constraint shifts from a ''mountain'' (technological inevitability) to a ''rope'' or ''tangled_rope'' (coordination or extraction based on strategic use), with extractiveness and suppression metrics rising to reflect the costs of strategic action and resistance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_autonomy_vs_agency, conceptual, 'The core ambiguity of technological determinism versus social construction of technology.').

omega_variable(
    natural_law_vs_constructed_narrative,
    'Is the ''inevitability'' of the printing press''s impact a genuine natural law of technological diffusion, or a constructed narrative that benefits those who champion technological progress as an unchallengeable force?',
    'Examination of the historical context in which this deterministic narrative emerged and its ideological functions, particularly in justifying certain outcomes as ''progress'' or ''inevitable''.',
    'If a constructed narrative, the constraint''s ''mountain'' classification is a false summit, reclassifying to a ''tangled_rope'' or ''snare'' that extracts from those who challenge the narrative of technological inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_narrative, conceptual, 'Whether the deterministic claim is a natural truth or a legitimizing story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.01).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__technological_determinism, theater_ratio, 1600, 0.01).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__technological_determinism, theater_ratio, 1650, 0.01).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.01).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.03).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__technological_determinism, base_extractiveness, 1600, 0.04).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__technological_determinism, base_extractiveness, 1650, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.01).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.01).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.02).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__technological_determinism, suppression_requirement, 1600, 0.02).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causation__technological_determinism, suppression_requirement, 1650, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'printing press caused the Reformation' kernel, emphasizing technological determinism. Sibling readings (strategic deployment, mutual shaping) offer alternative causal accounts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
