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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press as Deterministic Force in Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'technological determinism'
 *   reading of the relationship between the printing press and the
 *   Reformation. It posits the printing press as a fundamental, mountain-like
 *   technological force whose inherent efficiency and capacity for mass
 *   distribution made the Reformation inevitable, primarily by enabling
 *   widespread vernacular scripture access. The constraint's 'extraction' is
 *   low from the perspective of the new system it enables (due to cost
 *   reduction), but its 'suppression' is high against the old order it
 *   displaces.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.15).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.85).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press as Deterministic Force in Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '8b881625-0637-400d-9f5f-adaf62fa7bf7').
narrative_ontology:cs_kernel_codification('8b881625-0637-400d-9f5f-adaf62fa7bf7', formalized).
narrative_ontology:cs_authority_grounding('8b881625-0637-400d-9f5f-adaf62fa7bf7', self_enforcing).
narrative_ontology:cs_reading_relation('8b881625-0637-400d-9f5f-adaf62fa7bf7', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('8b881625-0637-400d-9f5f-adaf62fa7bf7', technology_reformation_causality__co_constitution_reading, forecloses).
narrative_ontology:cs_axiom('8b881625-0637-400d-9f5f-adaf62fa7bf7', foundational, technological_imperative_drives_social_change).
narrative_ontology:cs_axiom_status(technological_imperative_drives_social_change, holdable).
narrative_ontology:cs_axiom_grounding('8b881625-0637-400d-9f5f-adaf62fa7bf7', technological_imperative_drives_social_change, empirically_contingent).
narrative_ontology:cs_axiom('8b881625-0637-400d-9f5f-adaf62fa7bf7', foundational, information_diffusion_inevitably_undermines_centralized_control).
narrative_ontology:cs_axiom_status(information_diffusion_inevitably_undermines_centralized_control, holdable).
narrative_ontology:cs_axiom_grounding('8b881625-0637-400d-9f5f-adaf62fa7bf7', information_diffusion_inevitably_undermines_centralized_control, empirically_contingent).
narrative_ontology:cs_reference_frame('8b881625-0637-400d-9f5f-adaf62fa7bf7', gutenberg_revolution_paradigm).
narrative_ontology:cs_drift_state('8b881625-0637-400d-9f5f-adaf62fa7bf7', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8b881625-0637-400d-9f5f-adaf62fa7bf7', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_readers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, printers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, scribes_and_copyists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leveraged the printing press to rapidly disseminate their theological arguments and vernacular Bibles, bypassing traditional Church control and accelerating the Reformation. They benefited immensely from the reduced cost and increased reach of printed materials.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, protestant_reformers, beneficiary,
    powerful, generational, mobile, continental).

% Gained unprecedented access to religious texts in their native languages, fostering personal interpretation and reducing reliance on clerical intermediaries. This access was a direct result of the press's efficiency.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_readers, beneficiary,
    moderate, biographical, mobile, local).

% Operated the printing presses, profiting from the mass production of books, pamphlets, and Bibles. They were key agents in the distribution network, often aligning with reformers for commercial and ideological reasons, and their technology set the pace of information flow.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, printers, agenda_setter,
    organized, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__technological_determinism_reading, printers, beneficiary).

% Suffered a profound loss of control over religious discourse and information dissemination. Their traditional authority, based on scarcity and controlled access to texts, was directly undermined by the press's efficiency and reach. They resisted through censorship but ultimately could not stop the tide.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).

% Their traditional craft of manual text reproduction was rendered largely obsolete by the speed and cost-effectiveness of the printing press, leading to a collapse of their livelihood and social role.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, scribes_and_copyists, payer,
    powerless, immediate, trapped, local).

% Study the causal links between technological innovation and societal change, often debating the extent to which technology determines historical outcomes versus being merely an enabling factor. This reading represents one side of that analytical debate.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled the mass production and distribution of standardized texts, coordinating the spread of ideas and information across vast distances and diverse populations more efficiently than ever before.
% TRANSFER_FUNCTION: Transferred control over information production and dissemination from a centralized, elite clerical authority to a decentralized network of printers and readers. It also transferred economic value from manual labor to mechanized production.
% ABSENT_VOICES: Those who would have preferred a slower, more controlled evolution of religious thought, or those whose livelihoods (e.g., scribes) were directly destroyed by the new technology without adequate alternatives. Their voices were largely drowned out by the efficiency of the press.
% DISAPPEARANCE_RATIONALE: If the printing press had not been invented or had vanished, the Reformation as we know it would not have occurred. The rapid, widespread dissemination of vernacular Bibles and reformist tracts was critical to its success, and without it, the religious and political landscape of Europe would have developed very differently.
% FOUNDING_PROBLEM: The slow, expensive, and error-prone manual copying of texts, which limited access to knowledge and centralized control of information, particularly religious scripture, within the Catholic Church.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology, media, and the Reformation widely corroborate the problem of slow, expensive manual copying and the Church's control over information prior to the press. Independent academic research, not tied to religious institutions, supports this historical assessment.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The claimed type is 'mountain' because the printing press, as a technology, operates according to physical principles and inherent efficiencies that are unchangeable. Its 'emerges_naturally' is true in the sense that its operational principles are physical, not socially constructed. The 'extractiveness' is low (0.15) because the core function of the press is to *reduce* the cost of information production, making it a net enabler for its beneficiaries. However, the 'suppression' is very high (0.85) because the press fundamentally undermined and suppressed the Catholic Church's monopoly on information and the traditional role of scribes. 'Accessibility collapse' is high (0.90) as manual copying became economically unviable. 'Resistance' is also high (0.70) from the Church's efforts to censor and control the press. The low theater ratio (0.05) reflects the press's purely functional nature.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (reformers, readers, printers), the press was a liberating force, a pure enabler. From the perspective of the victims (Church, scribes), it was a destructive, suppressive force. The engine will compute these divergent classifications based on the structural roles and metrics, even though the underlying technology is claimed as a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers, vernacular readers, and printers are clear beneficiaries, as the press directly enabled their goals and livelihoods. The Catholic Church hierarchy and scribes/copyists are victims, as their power and economic roles were directly undermined and suppressed by the press's operation. The directionality for beneficiaries is low (subsidized by efficiency), and for victims, it is high (extracted from by displacement).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    determinism_vs_agency_ambiguity,
    'To what extent did the printing press *determine* the Reformation, versus merely *enabling* agents (reformers, printers) to achieve their goals?',
    'Counterfactual historical analysis: detailed examination of alternative historical paths or the impact of similar technologies in different social contexts where a Reformation did not occur.',
    'If agency is found to be the primary driver, this constraint would reclassify from a Mountain (of inevitability) to a Rope or Tangled Rope (a tool used by agents), with higher extractiveness from the agents who strategically deployed it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(determinism_vs_agency_ambiguity, conceptual, 'Ambiguity between technological determinism and human agency in historical causation.').

omega_variable(
    technology_as_neutral_tool_vs_inherent_force,
    'Is the printing press a neutral tool whose impact depends entirely on its users, or does it possess inherent properties that exert a deterministic force on society?',
    'Philosophical and sociological analysis of technology''s role in history, examining whether technologies consistently produce similar societal outcomes regardless of cultural context.',
    'If found to be a neutral tool, the ''mountain'' classification for the technology''s inherent force would weaken, potentially shifting to a ''rope'' or ''tangled_rope'' that reflects the coordination or extraction enacted *through* the tool by its users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_as_neutral_tool_vs_inherent_force, conceptual, 'Debate over technology''s inherent causal power versus its instrumental use.').

omega_variable(
    false_summit_natural_law_vs_constructed_benefit,
    'Is the efficiency and disruptive power of the printing press a genuine natural law (a mountain), or is its ''inevitability'' claim a constructed narrative that primarily benefits identifiable agents (reformers, printers) by legitimizing their actions?',
    'Analysis of the historical discourse surrounding the press: who promoted the ''inevitability'' narrative, and what were their interests? Examination of the counter-narratives and resistance from those negatively impacted.',
    'If the ''inevitability'' is found to be a constructed narrative serving specific interests, the constraint would reclassify from a Mountain to a Tangled Rope or Snare, reflecting the active enforcement of a beneficial narrative rather than a natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_vs_constructed_benefit, conceptual, 'Whether the ''mountain'' claim for the press''s inevitability is a genuine natural law or a constructed justification for its beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(tech_tr_t1470, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1470, 0.02).
narrative_ontology:measurement(tech_tr_t1490, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1490, 0.03).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1510, 0.04).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1530, 0.05).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1550, 0.05).
narrative_ontology:measurement(tech_tr_t1570, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1570, 0.05).
narrative_ontology:measurement(tech_tr_t1590, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1590, 0.05).
narrative_ontology:measurement(tech_tr_t1610, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1610, 0.05).
narrative_ontology:measurement(tech_tr_t1630, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1630, 0.05).
narrative_ontology:measurement(tech_tr_t1650, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1650, 0.05).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(tech_be_t1470, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1470, 0.08).
narrative_ontology:measurement(tech_be_t1490, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1490, 0.1).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1510, 0.12).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1530, 0.13).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1550, 0.14).
narrative_ontology:measurement(tech_be_t1570, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1570, 0.14).
narrative_ontology:measurement(tech_be_t1590, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1590, 0.15).
narrative_ontology:measurement(tech_be_t1610, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1610, 0.15).
narrative_ontology:measurement(tech_be_t1630, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1630, 0.15).
narrative_ontology:measurement(tech_be_t1650, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1650, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement(tech_su_t1470, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1470, 0.25).
narrative_ontology:measurement(tech_su_t1490, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1490, 0.4).
narrative_ontology:measurement(tech_su_t1510, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1510, 0.6).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1530, 0.75).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1550, 0.8).
narrative_ontology:measurement(tech_su_t1570, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1570, 0.83).
narrative_ontology:measurement(tech_su_t1590, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1590, 0.85).
narrative_ontology:measurement(tech_su_t1610, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1610, 0.85).
narrative_ontology:measurement(tech_su_t1630, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1630, 0.85).
narrative_ontology:measurement(tech_su_t1650, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1650, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, information_standard).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'technology_reformation_causality' kernel. This 'technological_determinism_reading' emphasizes the press's inherent causal power, while 'beneficiary_agency_reading' focuses on strategic use by actors, and 'co_constitution_reading' on co-evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
