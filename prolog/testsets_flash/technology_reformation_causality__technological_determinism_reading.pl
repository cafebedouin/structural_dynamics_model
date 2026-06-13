% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press as Determinant of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the 'technological determinism' reading of the
 *   printing press's role in the Reformation. In this view, the printing
 *   press, as a powerful and inherently transformative technology, made the
 *   Reformation inevitable by enabling the mass distribution of vernacular
 *   scripture, thereby undermining the Catholic Church's control over
 *   religious information. The technology itself is seen as the primary
 *   causal agent, a 'mountain' that reshaped social structures regardless of
 *   human agency. The low extractiveness and suppression reflect the idea
 *   that the technology itself does not 'extract' in a human sense, but
 *   rather imposes a new reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.05).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.02).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press as Determinant of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, 'bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa').
narrative_ontology:cs_kernel_codification('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', implicit).
narrative_ontology:cs_authority_grounding('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', self_enforcing).
narrative_ontology:cs_reading_relation('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', technology_reformation_causality__co_constitution_reading, forecloses).
narrative_ontology:cs_axiom('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', foundational, technology_as_primary_causal_agent).
narrative_ontology:cs_axiom_status(technology_as_primary_causal_agent, holdable).
narrative_ontology:cs_axiom_grounding('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', technology_as_primary_causal_agent, empirically_contingent).
narrative_ontology:cs_axiom('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', secondary, information_dissemination_drives_social_change).
narrative_ontology:cs_axiom_status(information_dissemination_drives_social_change, holdable).
narrative_ontology:cs_axiom_grounding('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', information_dissemination_drives_social_change, empirically_contingent).
narrative_ontology:cs_reference_frame('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', technological_imperative_framework).
narrative_ontology:cs_drift_state('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', contemporary_media_studies_critique, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bca10cc4-b098-4a15-b70e-d9dd3bfe7ffa', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_church_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The printing press, as a technological system, fundamentally altered the economics and logistics of information dissemination, making mass production of texts feasible and cheap. Its inherent capabilities set the agenda for what was possible in communication.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, printing_press_technology, agenda_setter,
    institutional, generational, analytical, global).

% Benefited immensely from the printing press, which enabled them to rapidly produce and distribute vernacular Bibles, pamphlets, and theological tracts, bypassing traditional Church control over information. They were downstream adapters to the technological imperative.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Suffered a loss of control over religious discourse as the printing press democratized access to scripture and theological debate. Their traditional mechanisms of information control (scribal copying, Latin Vulgate) were rendered obsolete by the technology, forcing them into a reactive position.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, constrained, global).

% Gained unprecedented access to religious texts in their native languages, fostering individual interpretation and reducing reliance on clerical intermediaries. The technology directly enabled their access.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_readers, beneficiary,
    moderate, biographical, mobile, regional).

% Their profession was largely rendered obsolete by the efficiency of the printing press, leading to economic displacement and loss of social status. They were structurally excluded from the new information economy.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, scribal_copyists, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press coordinated the mass production and distribution of identical texts across wide geographies, enabling a shared information environment for religious and political ideas.
% TRANSFER_FUNCTION: Transferred the power of information dissemination from centralized, elite institutions (Church, monasteries) to decentralized, commercial enterprises (printers) and individual authors, enabling a rapid transfer of ideas to a mass audience.
% ABSENT_VOICES: Those who would have preferred a slower, more controlled dissemination of religious knowledge, or who benefited from the scarcity of texts, were structurally sidelined by the technological shift. Their objections were overwhelmed by the new reality of mass production.
% DISAPPEARANCE_RATIONALE: If the causal link between the printing press and the Reformation were to vanish, the historical fact of the Reformation would remain, but its explanation would require a complete re-evaluation, shifting focus to social, political, and theological factors independent of media technology.
% FOUNDING_PROBLEM: The problem of slow, expensive, and error-prone manual copying of texts, which limited access to knowledge and centralized control over information.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and media studies widely corroborate that the manual copying problem was definitively solved by the printing press. The Catholic Church hierarchy, while initially resisting, eventually adapted to the new reality, implicitly acknowledging the problem's resolution by adopting printing themselves.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_unchanged).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and suppression (0.02) reflect the view that the printing press, as a technology, is a 'mountain' — an unchangeable physical/logical limit that imposes its reality rather than extracting from or suppressing agents in a coercive human sense. Its 'power' derives from its inherent capacity to reduce costs and increase speed of information dissemination. The high accessibility collapse (0.95) and low resistance (0.01) further support the mountain classification, as the technology fundamentally altered the landscape of information access, making alternatives to mass printing largely unviable and meeting little effective resistance from those who wished to maintain the old order.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the printing press (as an analytical construct representing the technology's inherent capabilities), its operation is a neutral, inevitable force. From the perspective of the Protestant reformers, it was a powerful tool for liberation. From the perspective of the Catholic Church, it was a disruptive force that eroded their authority. This reading emphasizes the technology's inherent, mountain-like nature, making these differing perspectives consequences of the technology's deterministic impact.
 *
 * DIRECTIONALITY LOGIC:
 *   The printing press itself is modeled as the 'agenda_setter' due to its inherent technological capabilities driving the change. Protestant reformers and vernacular readers are beneficiaries, as the technology directly enabled their goals and access. The Catholic Church hierarchy and scribal copyists are payers/victims, as the technology undermined their established roles and power. The directionality for the technology itself is near 0.0 (full beneficiary) as it 'benefits' from its own operation by fulfilling its inherent function, while the Church hierarchy is near 1.0 (full target) as it bears the costs of the technological shift.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a 'mountain' (claimed type), is not subject to mandatrophy in the same way human-constructed constraints are. Its 'mandate' is its inherent technological function, which does not 'atrophy' but rather continues to operate. The classification prevents mislabeling a fundamental technological shift as a human-designed extractive system. The beneficiaries (reformers) are not 'extracting' from the technology, but rather leveraging its inherent capabilities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_vs_social_causality,
    'To what extent was the Reformation a direct, inevitable consequence of the printing press''s capabilities, versus a contingent outcome shaped by social, political, and theological agency?',
    'Comparative historical analysis of other societies with printing technology but without similar religious upheavals, or counterfactual history exploring alternative social responses to the press.',
    'If social agency is found to be dominant, this constraint would be reclassified from a ''mountain'' (inevitable) to a ''rope'' or ''tangled_rope'' (a tool whose effects depend on how it is wielded), with higher extractiveness reflecting the choices made by agents. If technological determinism holds, the mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_vs_social_causality, conceptual, 'Ambiguity between technological inevitability and human agency in historical outcomes.').

omega_variable(
    mountain_vs_tool,
    'Is the printing press best understood as a ''mountain'' (an unchangeable, deterministic force) or as a ''tool'' (an instrument whose impact is shaped by its users)?',
    'Analysis of the degree of freedom available to historical actors in deploying or resisting the technology. If actors had significant choice in how to use or suppress it, it leans towards ''tool''.',
    'If reclassified as a ''tool'', the constraint would likely shift to a ''rope'' or ''tangled_rope'', with higher suppression and extractiveness reflecting the choices and power dynamics of its users. The ''mountain'' classification implies minimal human choice in its fundamental impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_tool, conceptual, 'Whether technology is a deterministic force or a malleable instrument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(tech_tr_t1475, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1475, 0.01).
narrative_ontology:measurement(tech_tr_t1500, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(tech_tr_t1525, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1525, 0.01).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1550, 0.01).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1450, 0.01).
narrative_ontology:measurement(tech_be_t1475, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1475, 0.02).
narrative_ontology:measurement(tech_be_t1500, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement(tech_be_t1525, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1525, 0.04).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1550, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1450, 0.01).
narrative_ontology:measurement(tech_su_t1475, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1475, 0.01).
narrative_ontology:measurement(tech_su_t1500, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(tech_su_t1525, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1525, 0.02).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1550, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
