% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Mutual Shaping of Printing Press and Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story describes the 'mutual shaping' reading of the
 *   relationship between the printing press and the Reformation. It posits a
 *   bidirectional causal link: the printing press created new possibilities
 *   for information dissemination, which reformers actively exploited, and
 *   this exploitation, in turn, influenced the technological and commercial
 *   development of printing. The press is seen as a scaffold, enabling a
 *   profound societal transition rather than merely causing it or being a
 *   neutral tool.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.35).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.45).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Mutual Shaping of Printing Press and Reformation").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causation__mutual_shaping).
narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'f619f078-cf1b-4f14-a1bb-a206144c485f').
narrative_ontology:cs_kernel_codification('f619f078-cf1b-4f14-a1bb-a206144c485f', formalized).
narrative_ontology:cs_authority_grounding('f619f078-cf1b-4f14-a1bb-a206144c485f', practice).
narrative_ontology:cs_reading_relation('f619f078-cf1b-4f14-a1bb-a206144c485f', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('f619f078-cf1b-4f14-a1bb-a206144c485f', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('f619f078-cf1b-4f14-a1bb-a206144c485f', foundational, technology_is_socially_constructed).
narrative_ontology:cs_axiom_status(technology_is_socially_constructed, holdable).
narrative_ontology:cs_axiom_grounding('f619f078-cf1b-4f14-a1bb-a206144c485f', technology_is_socially_constructed, empirically_contingent).
narrative_ontology:cs_axiom('f619f078-cf1b-4f14-a1bb-a206144c485f', foundational, social_change_is_mediated_by_technology).
narrative_ontology:cs_axiom_status(social_change_is_mediated_by_technology, holdable).
narrative_ontology:cs_axiom_grounding('f619f078-cf1b-4f14-a1bb-a206144c485f', social_change_is_mediated_by_technology, empirically_contingent).
narrative_ontology:cs_reference_frame('f619f078-cf1b-4f14-a1bb-a206144c485f', dynamic_co_evolutionary_system).
narrative_ontology:cs_drift_state('f619f078-cf1b-4f14-a1bb-a206144c485f', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f619f078-cf1b-4f14-a1bb-a206144c485f', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printers_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, literate_public).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, traditional_scribes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited the printing press to rapidly disseminate their ideas, bypassing traditional gatekeepers. Their agency in content creation and distribution, in turn, drove demand for printing technology and shaped its development (e.g., demand for vernacular texts).
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformation_reformers, beneficiary,
    organized, biographical, constrained, continental).

% Invested in and operated the printing technology, profiting from the demand for printed materials. Their commercial decisions (e.g., what to print, how to market) influenced the spread of ideas and the technological evolution of the press itself. They benefited from the increased demand generated by reformers.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printers_publishers, agenda_setter,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, printers_publishers, beneficiary).

% Initially bore the cost of losing control over information dissemination. They attempted to enforce censorship and control the press, but their efforts were often outpaced by the technology's spread and the reformers' agility. Their resistance, however, also shaped the development of printing (e.g., clandestine presses, legal frameworks).
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_church_hierarchy, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, catholic_church_hierarchy, agenda_setter).

% Saw their livelihood and cultural role diminish as printed books became cheaper and more widespread. They were largely excluded from the new information economy, bearing the direct cost of technological displacement.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, traditional_scribes, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, traditional_scribes, excluded).

% Gained unprecedented access to a wider range of texts, including religious and political tracts, at lower costs. Their demand for specific content (e.g., vernacular Bibles) further fueled the printing industry and the Reformation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, literate_public, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the complex interplay between the printing press and the Reformation, seeking to understand the bidirectional causal links and co-evolutionary dynamics. They are outside the historical events but interpret their structure.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, historians_media_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, printers_publishers).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled the rapid, widespread, and relatively standardized dissemination of information and ideas across Europe, coordinating the spread of new religious and political movements.
% TRANSFER_FUNCTION: Transferred information, religious doctrine, and political ideas from authors and printers to a mass public, while also transferring economic value to printers and publishers.
% ABSENT_VOICES: Those who wished to maintain a centralized, controlled information environment (e.g., segments of the Catholic Church hierarchy) were increasingly marginalized from the new, decentralized communication channels. Their attempts at suppression were often reactive rather than proactive.
% DISAPPEARANCE_RATIONALE: If the mutual shaping process had not occurred, the Reformation would have unfolded very differently, likely remaining a more localized and less impactful movement. The development of printing technology itself would have followed a different trajectory, potentially remaining more focused on elite or administrative texts. The entire early modern information landscape would be fundamentally altered.
% FOUNDING_PROBLEM: The problem was the slow, expensive, and centralized dissemination of information, which limited the spread of new ideas and maintained existing power structures.
% FOUNDING_PROBLEM_CORROBORATION: Historians widely corroborate that the pre-press information environment was indeed slow and centralized. While the specific problems of the 15th century were addressed, the underlying challenge of information dissemination and its societal impact remains a 'live' problem, albeit in new forms. Contemporary accounts from reformers and printers also attest to the transformative power of the press.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is moderate (0.35) because while the press enabled new forms of profit for printers and influence for reformers, it also imposed costs (e.g., capital investment, new forms of intellectual property). Suppression is moderate (0.45) reflecting the ongoing attempts by authorities (like the Catholic Church) to control and censor printed materials, which were met with resistance and innovation (e.g., clandestine printing). The theater ratio is low (0.10) as the press was a highly functional technology, with little performative maintenance. Accessibility collapse is moderate (0.60) because while the press opened new avenues for communication, it also rendered older, slower methods (like hand-copying) less viable. Resistance is moderate (0.50) due to the significant efforts by established powers to counter the spread of 'heretical' or challenging ideas.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformers and printers, the press was a powerful enabler and a source of opportunity. From the perspective of the Catholic Church, it was a disruptive force requiring active suppression. The 'mutual shaping' reading attempts to integrate these perspectives by showing how both proactive use and reactive suppression contributed to the co-evolution of the technology and the social landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformation reformers and printers/publishers were primary beneficiaries, gaining influence and economic advantage respectively. The literate public also benefited from increased access to information. The Catholic Church hierarchy, while attempting to exert control, ultimately bore significant costs in terms of lost authority and control over information, making them a payer. Traditional scribes were clear victims, losing their livelihoods. The 'mutual shaping' aspect means that even those who bore costs (like the Church) inadvertently influenced the trajectory of the technology through their reactions.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Scaffold prevents mislabeling it as a Mountain (pure technological determinism) or a Snare (pure extraction). It acknowledges the press's role as a temporary, enabling structure that facilitated a transition to a new information order. The 'sunset' of this scaffold is the point at which the new media landscape and its associated social structures became established, and the co-evolutionary dynamic shifted to a new phase. The constraint's function was to support this transition, not to maintain a steady state indefinitely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_ambiguity,
    'To what extent did the printing press primarily enable pre-existing reformist impulses, versus actively creating new possibilities that would not otherwise have emerged?',
    'Counterfactual historical analysis comparing regions with early press adoption to those without, or detailed micro-historical studies of specific reform movements'' origins and their interaction with printing technology.',
    'If the press primarily enabled pre-existing impulses, the ''strategic_deployment'' reading gains strength. If it created genuinely new possibilities, the ''mutual_shaping'' reading is further reinforced, highlighting the generative capacity of technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_ambiguity, empirical, 'Ambiguity regarding the precise balance of enabling vs. generative causality.').

omega_variable(
    technological_autonomy_vs_social_shaping,
    'How much of the printing press''s development (e.g., standardization, efficiency) was driven by its internal technological logic, versus external social demands from reformers and markets?',
    'Detailed historical studies of printing workshops, patents, and commercial records, tracing innovations back to their primary drivers (e.g., engineers'' insights vs. market demand for specific book formats).',
    'If internal technological logic was dominant, the ''technological_determinism'' reading gains some ground regarding the technology''s own evolution. If social demands were primary, the ''mutual_shaping'' reading is strengthened, emphasizing the social construction of technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_autonomy_vs_social_shaping, empirical, 'The balance between internal technological drivers and external social shaping in the press''s development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__mutual_shaping, theater_ratio, 1480, 0.08).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causation__mutual_shaping, theater_ratio, 1520, 0.1).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causation__mutual_shaping, theater_ratio, 1560, 0.12).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__mutual_shaping, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__mutual_shaping, theater_ratio, 1650, 0.1).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.2).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__mutual_shaping, base_extractiveness, 1480, 0.25).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causation__mutual_shaping, base_extractiveness, 1520, 0.3).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causation__mutual_shaping, base_extractiveness, 1560, 0.35).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__mutual_shaping, base_extractiveness, 1600, 0.38).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__mutual_shaping, base_extractiveness, 1650, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.2).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causation__mutual_shaping, suppression_requirement, 1480, 0.3).
narrative_ontology:measurement(pres_su_t1520, press_reformation_causation__mutual_shaping, suppression_requirement, 1520, 0.4).
narrative_ontology:measurement(pres_su_t1560, press_reformation_causation__mutual_shaping, suppression_requirement, 1560, 0.5).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__mutual_shaping, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causation__mutual_shaping, suppression_requirement, 1650, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, vernacular_bible_translation).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, rise_of_scientific_societies).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('mutual_shaping') of the 'press_reformation_causation' kernel, which also includes 'technological_determinism' and 'strategic_deployment' as sibling readings. Each reading offers a distinct structural interpretation of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
