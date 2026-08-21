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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   domain: history/technology/religion
 *
 * SUMMARY:
 *   This constraint describes the historical process where the printing press
 *   and the Protestant Reformation mutually shaped each other's development.
 *   The press created new possibilities for reformers to disseminate their
 *   ideas, which in turn drove innovation and demand within the printing
 *   industry. This reading emphasizes a bidirectional causality, where
 *   neither technology nor human agency was solely determinant. It is
 *   classified as a Scaffold because the press provided transitional support
 *   for a period of profound social and religious change, and its own
 *   development was scaffolded by the demands of the Reformation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.35).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.25).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Mutual Shaping of Printing Press and Reformation").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history/technology/religion").

narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, '95b26532-f4bc-4bc8-bdf6-1503ba1f5e73').
narrative_ontology:cs_kernel_codification('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', implicit).
narrative_ontology:cs_authority_grounding('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', practice).
narrative_ontology:cs_reading_relation('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', foundational, sociotechnical_systems_are_co_constitutive).
narrative_ontology:cs_axiom_status(sociotechnical_systems_are_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', sociotechnical_systems_are_co_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', foundational, historical_change_is_emergent_not_unilinear).
narrative_ontology:cs_axiom_status(historical_change_is_emergent_not_unilinear, holdable).
narrative_ontology:cs_axiom_grounding('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', historical_change_is_emergent_not_unilinear, empirically_contingent).
narrative_ontology:cs_reference_frame('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', complex_adaptive_historical_systems).
narrative_ontology:cs_drift_state('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('95b26532-f4bc-4bc8-bdf6-1503ba1f5e73', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printing_industry).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, secular_rulers).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, traditional_scribal_culture).
narrative_ontology:constraint_vindicates(press_reformation_causation__mutual_shaping, sociotechnical_co_evolution_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively used the printing press to disseminate their ideas, translating texts into vernacular languages and producing pamphlets. They benefited from the press's ability to rapidly spread their message, which in turn shaped the demand for and content of printed materials.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformation_reformers, beneficiary,
    organized, biographical, constrained, regional).

% Printers and publishers adapted their technology and business models to meet the demands of the Reformation, producing vast quantities of religious and polemical texts. They profited from the increased demand and their innovations in printing techniques were influenced by the reformers' needs.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printing_industry, beneficiary,
    organized, biographical, mobile, regional).

% Experienced a significant challenge to its authority and control over religious discourse. Its attempts to suppress printing and reformist ideas were largely ineffective, leading to a loss of power and influence, and forcing internal reforms.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_church_hierarchy, payer,
    institutional, generational, constrained, global).

% The network of scribes, copyists, and monastic scriptoria saw their role in knowledge production and dissemination rapidly diminish. Their skills became less central, and their economic model was disrupted by the efficiency of the press.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, traditional_scribal_culture, payer,
    organized, biographical, trapped, local).

% Regulated the printing trade, influencing standards, training, and distribution. Their practices and rules evolved in response to the explosion of demand and the new types of content driven by the Reformation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, printers_guilds, agenda_setter,
    organized, biographical, constrained, local).

% Often found their power enhanced by the weakening of the Catholic Church's authority, and sometimes strategically supported reformers or controlled printing to consolidate their own power. They benefited from the new information flows but also faced challenges in managing religious dissent.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, secular_rulers, beneficiary,
    institutional, generational, constrained, national).

% Study the complex interplay between technological innovation and social change, analyzing how the printing press and the Reformation mutually influenced each other's development and outcomes.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitated the rapid and widespread dissemination of religious and political ideas, enabling new forms of collective action and intellectual exchange that transcended traditional boundaries.
% TRANSFER_FUNCTION: Transferred information, authority, and cultural influence from centralized, elite institutions (like the Church) to a broader, more decentralized public, while also transferring economic value to the nascent printing industry.
% ABSENT_VOICES: Those who benefited from the pre-printing information monopolies, such as traditional scribes and monastic copyists, were rapidly marginalized and their perspectives on the 'benefits' of the new order were largely unheard in the dominant narratives of progress.
% DISAPPEARANCE_RATIONALE: If the mutual shaping between the printing press and the Reformation had not occurred, the course of European history, religious development, and the very nature of public discourse would have been fundamentally different. The Reformation would likely have remained a localized academic dispute, and the printing press's development would have followed a different trajectory, perhaps remaining a tool primarily for elite communication.
% FOUNDING_PROBLEM: The problem of efficiently disseminating complex ideas and challenging established authority in a pre-mass media society, coupled with the nascent printing industry's need for content and market expansion.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and media studies widely corroborate that the dynamic of technology shaping and being shaped by social movements is an ongoing and fundamental aspect of historical change, even if the specific historical context of the Reformation is unique. Independent academic research consistently highlights this co-evolutionary pattern.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.35) because while the co-evolution led to significant losses for established powers (Church, scribal culture), the press itself was not primarily an extractive mechanism in this reading, but an enabling one. Suppression is moderate-low (0.25) as attempts by authorities to control the press were met with significant resistance and were ultimately ineffective against the broader co-evolutionary dynamic. Theater ratio is low (0.10) because the functional impact of the press was immense and undeniable. Accessibility collapse is moderate (0.40) as the press opened new avenues for information dissemination, even as it diminished older ones. Resistance is high (0.60) reflecting the intense conflicts and challenges to authority that characterized the Reformation era. The 'has_sunset_clause: true' reflects that the specific historical period of the Reformation's emergence and the press's role in that transition had a historical end, even if printing continued.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the reformers and printers, the co-evolution was a dynamic of liberation and innovation. From the perspective of the Catholic Church and scribal culture, it was a destructive force leading to loss and chaos. This reading attempts to capture the mutual shaping without fully endorsing either extreme, acknowledging both the enabling and disruptive aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformation reformers, the printing industry, and secular rulers were beneficiaries, gaining new avenues for influence, profit, and power. The Catholic Church hierarchy and traditional scribal culture were payers, experiencing significant losses of authority, control, and economic relevance. Analytical historians serve as observers, studying the complex dynamics without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Scaffold prevents mislabeling it as a pure Rope (which would understate the disruptive extraction from existing powers) or a Snare (which would overstate the press's inherent extractive intent). It highlights its role as a transitional, enabling structure that was itself transformed by the process it facilitated, rather than a static mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_dominance_ambiguity,
    'Is the relationship between the printing press and the Reformation truly one of mutual shaping, or does one factor (technology or agency) hold a dominant causal role?',
    'Further historical research comparing counterfactual scenarios or cross-cultural studies of similar technological introductions without corresponding social movements.',
    'If one factor is found to be dominant, the classification might shift towards a more deterministic (Mountain-like) or purely agentic (Rope/Snare-like) interpretation, altering the perceived agency and responsibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_dominance_ambiguity, empirical, 'Ambiguity regarding the precise balance of causal influence in the co-evolutionary process.').

omega_variable(
    scaffold_permanence_ambiguity,
    'Was the press''s role truly transitional support for the Reformation, or did it represent a permanent, non-transitional shift in communication infrastructure that merely manifested during the Reformation?',
    'Analysis of post-Reformation communication patterns: if the fundamental structures of information dissemination remained unchanged after the Reformation''s ''transition,'' it suggests a permanent shift rather than a temporary scaffold.',
    'If the shift was permanent rather than transitional, the ''scaffold'' classification might be less appropriate, potentially leaning towards a ''rope'' (if coordination benefits are primary) or even a ''mountain'' (if the new communication structure became an irreducible feature of the landscape).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_permanence_ambiguity, conceptual, 'Whether the ''scaffold'' function was truly transitional or indicative of a permanent, underlying change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causation__mutual_shaping, theater_ratio, 1490, 0.05).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__mutual_shaping, theater_ratio, 1530, 0.08).
narrative_ontology:measurement(pres_tr_t1570, press_reformation_causation__mutual_shaping, theater_ratio, 1570, 0.1).
narrative_ontology:measurement(pres_tr_t1610, press_reformation_causation__mutual_shaping, theater_ratio, 1610, 0.1).
narrative_ontology:measurement(pres_tr_t1648, press_reformation_causation__mutual_shaping, theater_ratio, 1648, 0.1).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causation__mutual_shaping, base_extractiveness, 1490, 0.25).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__mutual_shaping, base_extractiveness, 1530, 0.35).
narrative_ontology:measurement(pres_be_t1570, press_reformation_causation__mutual_shaping, base_extractiveness, 1570, 0.4).
narrative_ontology:measurement(pres_be_t1610, press_reformation_causation__mutual_shaping, base_extractiveness, 1610, 0.38).
narrative_ontology:measurement(pres_be_t1648, press_reformation_causation__mutual_shaping, base_extractiveness, 1648, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement(pres_su_t1490, press_reformation_causation__mutual_shaping, suppression_requirement, 1490, 0.2).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causation__mutual_shaping, suppression_requirement, 1530, 0.4).
narrative_ontology:measurement(pres_su_t1570, press_reformation_causation__mutual_shaping, suppression_requirement, 1570, 0.5).
narrative_ontology:measurement(pres_su_t1610, press_reformation_causation__mutual_shaping, suppression_requirement, 1610, 0.45).
narrative_ontology:measurement(pres_su_t1648, press_reformation_causation__mutual_shaping, suppression_requirement, 1648, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, rise_of_vernacular_languages).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, state_censorship_mechanisms).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('mutual_shaping') of the 'press_reformation_causation' kernel, which also includes 'technological_determinism' and 'strategic_deployment' readings. Each reading offers a distinct causal account of the relationship between the printing press and the Reformation, with differing implications for agency and determinism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
