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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Inevitable Driver of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint represents the 'technological determinism' reading of the
 *   printing press's role in the Reformation. It posits the printing press as
 *   an autonomous, enabling technology whose inherent properties made the
 *   spread of vernacular scripture and the success of the Reformation
 *   inevitable. In this reading, the technology itself is a 'mountain' — an
 *   unchangeable physical and logical limit that dictates outcomes, with
 *   human agency largely suppressed or relegated to a reactive role. The
 *   'beneficiary' is the narrative of technological progress itself, which
 *   gains explanatory power from this deterministic framing. This is one
 *   reading of the 'press_reformation_causality' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.05).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.95).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.05).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Inevitable Driver of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'bc82b241-17c9-44d2-8b66-794fc07347fd').
narrative_ontology:cs_kernel_codification('bc82b241-17c9-44d2-8b66-794fc07347fd', implicit).
narrative_ontology:cs_authority_grounding('bc82b241-17c9-44d2-8b66-794fc07347fd', diffuse_epistemic).
narrative_ontology:cs_reading_relation('bc82b241-17c9-44d2-8b66-794fc07347fd', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('bc82b241-17c9-44d2-8b66-794fc07347fd', press_reformation_causality__co_constitution, forecloses).
narrative_ontology:cs_axiom('bc82b241-17c9-44d2-8b66-794fc07347fd', foundational, technology_as_autonomous_causal_agent).
narrative_ontology:cs_axiom_status(technology_as_autonomous_causal_agent, holdable).
narrative_ontology:cs_axiom_grounding('bc82b241-17c9-44d2-8b66-794fc07347fd', technology_as_autonomous_causal_agent, empirically_contingent).
narrative_ontology:cs_axiom('bc82b241-17c9-44d2-8b66-794fc07347fd', foundational, social_change_as_technologically_determined).
narrative_ontology:cs_axiom_status(social_change_as_technologically_determined, holdable).
narrative_ontology:cs_axiom_grounding('bc82b241-17c9-44d2-8b66-794fc07347fd', social_change_as_technologically_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('bc82b241-17c9-44d2-8b66-794fc07347fd', gutenberg_era_technological_revolution).
narrative_ontology:cs_drift_state('bc82b241-17c9-44d2-8b66-794fc07347fd', contemporary_media_studies_critique, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bc82b241-17c9-44d2-8b66-794fc07347fd', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, historical_narratives_of_technological_progress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a simplified, linear causal explanation where technology is the primary driver of social change, reinforcing a deterministic view of history. This narrative gains explanatory power by attributing inevitability to the press's role.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, historical_narratives_of_technological_progress, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, historical_narratives_of_technological_progress).

% The physical and logical capabilities of the printing press itself, which this reading posits as the autonomous force. It 'sets the agenda' by making certain outcomes (like widespread scripture dissemination) physically inevitable once invented.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printing_press_technology, agenda_setter,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, printing_press_technology).

% The choices, strategies, and contingent actions of reformers, printers, and political leaders are downplayed or rendered epiphenomenal, seen as merely responding to the technological imperative rather than shaping it. Their active role is excluded from the primary causal account.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, human_agency_in_reformation, excluded,
    powerless, biographical, identity_locked, regional).

% Analyze the role of technology in historical change. This reading offers a clear, if simplified, causal model that can be appealing for its explanatory parsimony, though many historians contest its determinism.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, historians_of_technology, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press, as a physical technology, inherently coordinates the rapid, standardized, and widespread reproduction of text, enabling a scale of information dissemination previously impossible.
% TRANSFER_FUNCTION: Transfers information (vernacular scripture) from a limited, elite sphere to a mass audience, and transfers causal agency from human actors to the technology itself within this historical narrative.
% ABSENT_VOICES: Historians emphasizing human agency, strategic deployment of technology, or the co-constitutive relationship between technology and society would object, arguing that this reading oversimplifies complex historical processes and ignores the contingent choices that shaped the Reformation.
% DISAPPEARANCE_RATIONALE: If this specific deterministic reading of the printing press's role vanished, the historical facts of the Reformation and the existence of the printing press would remain unchanged. Only the interpretive framework would be lost, leading to a more nuanced, less deterministic understanding of the period.
% FOUNDING_PROBLEM: To explain the rapid and widespread success of the Reformation, particularly the dissemination of vernacular scripture, in a way that highlights the transformative power of a new technology.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of technological determinism in historical analysis continue to attest to the problem's live status, seeking parsimonious explanations for large-scale social change. Critics (e.g., media historians, social historians) argue the problem is framed too narrowly, ignoring social and political contingencies.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is very low (0.05) because the printing press, as a physical technology, does not 'extract' from human actors in this deterministic reading; it simply enables. Suppression is very high (0.95) because the technological imperative is seen as overriding or 'suppressing' alternative historical trajectories or the significance of human choice. Theater ratio is zero (0.0) as the technology's function is purely instrumental and direct, with no performative aspect. Accessibility collapse is high (0.9) because, once the press exists, the 'alternatives' to widespread dissemination (e.g., continued manuscript culture) are seen as collapsing under its inherent force. Resistance is low (0.05) because, within this deterministic frame, the technology's impact is seen as an unstoppable force, not something that can be meaningfully resisted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'printing_press_technology' (as an abstract force), its operation is simply a matter of its inherent capabilities. From the perspective of 'human_agency_in_reformation', their active role is diminished, making the constraint appear as an overwhelming, unchallengeable force. The 'historians_of_technology' observer seat can analyze this framing, noting its explanatory power but also its limitations.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'printing_press_technology' itself is the agenda-setter, dictating the course of events. 'Historical_narratives_of_technological_progress' are the beneficiaries, as this reading reinforces their explanatory power. 'Human_agency_in_reformation' is effectively excluded or suppressed, bearing the cost of having its causal role diminished. There are no direct 'victims' in the sense of extraction, as the constraint is framed as a natural, inevitable force.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by clearly defining the printing press as a 'mountain' of technological inevitability, rather than a 'snare' or 'tangled rope' of human design or extraction. The low extractiveness and high suppression are consistent with a force that dictates outcomes rather than collecting rents. The challenge is to avoid reifying the narrative itself as a natural law, which is addressed by the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_autonomy_vs_social_shaping,
    'Is the printing press truly an autonomous enabling technology, or was its impact shaped by social, political, and religious choices?',
    'Detailed historical analysis of specific instances of print deployment, examining the choices made by reformers, printers, and authorities regarding content, distribution, and suppression.',
    'If its impact was socially shaped, the ''mountain'' classification would be a misattribution, and the constraint would reclassify towards a ''tangled_rope'' (strategic deployment) or ''rope'' (co-constitution) reflecting human agency and contingent outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_autonomy_vs_social_shaping, empirical, 'Whether technology''s role is autonomous or socially mediated.').

omega_variable(
    causal_agency_attribution,
    'Is the ''inevitability'' of the Reformation''s success truly a property of the printing press, or an interpretive choice within a deterministic historical narrative?',
    'Comparative historical studies of other societies with printing technology that did not experience similar religious upheavals, or counterfactual analysis exploring alternative historical paths.',
    'If it''s an interpretive choice, the ''emerges_naturally: true'' claim would be false, and the constraint would be reclassified as a ''snare'' (of narrative construction) or ''tangled_rope'' (of academic discourse), extracting explanatory power by suppressing nuance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_agency_attribution, conceptual, 'Distinguishing technological inevitability from narrative construction.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''press_reformation_causality'' kernel. What would a sibling reading (e.g., ''strategic_deployment'' or ''co_constitution'') change structurally?',
    'Comparing this story''s structural properties (especially extractiveness, suppression, and beneficiary/victim declarations) with those of the sibling readings.',
    'A ''strategic_deployment'' reading would likely show higher extractiveness (from those who weaponized the press) and lower suppression (as agency is restored), reclassifying as a ''tangled_rope''. A ''co_constitution'' reading would likely show moderate extractiveness and suppression, reflecting feedback loops, possibly classifying as a ''rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Structural differences between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.0).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__technological_determinism, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.0).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.0).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__technological_determinism, theater_ratio, 1650, 0.0).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__technological_determinism, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.05).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__technological_determinism, base_extractiveness, 1650, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__technological_determinism, suppression_requirement, 1450, 0.95).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__technological_determinism, suppression_requirement, 1500, 0.95).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causality__technological_determinism, suppression_requirement, 1550, 0.95).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__technological_determinism, suppression_requirement, 1600, 0.95).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causality__technological_determinism, suppression_requirement, 1650, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'press_reformation_causality' kernel. This 'technological_determinism' reading emphasizes the autonomous, inevitable impact of the printing press. Sibling readings ('strategic_deployment' and 'co_constitution') offer alternative causal accounts, emphasizing human agency and feedback loops, respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
