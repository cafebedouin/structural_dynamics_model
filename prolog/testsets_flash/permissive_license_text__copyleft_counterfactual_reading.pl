% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Text (Copyleft Counterfactual Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'copyleft counterfactual' reading of
 *   permissive license texts, arguing that without a reciprocity requirement,
 *   such licenses structurally enable exploitation. It posits that viral
 *   reciprocity (like the GPL) is a necessary alternative to prevent
 *   proprietary developers from taking open-source contributions without
 *   giving back. This reading views permissive licenses as a 'tangled rope'
 *   where initial coordination (sharing code) is undermined by asymmetric
 *   extraction (proprietary enclosure).
 *
 * KEY AGENTS:
 *   - copyleft_advocates: Primary beneficiary (institutional/arbitrage) — benefit from the enforcement of reciprocity.
 *   - open_source_community: Secondary beneficiary (organized/mobile) — benefits from the continued 'freeness' of derivative works.
 *   - proprietary_software_developers: Primary victim (powerful/constrained) — bear the cost of having to reciprocate or avoid permissive code.
 *   - corporate_integrators: Secondary victim (institutional/constrained) — face legal obligations to open-source their modifications.
 *   - permissive_license_authors: Agenda setter (moderate/mobile) — set the initial terms, but their intent is reinterpreted by this reading.
 *   - legal_scholars: Observer (analytical/analytical) — analyze the implications of different licensing models.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.7).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.6).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Text (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'f68b17c2-61b5-431e-9512-6daf48d7989a').
narrative_ontology:cs_kernel_codification('f68b17c2-61b5-431e-9512-6daf48d7989a', fixed_text).
narrative_ontology:cs_authority_grounding('f68b17c2-61b5-431e-9512-6daf48d7989a', practice).
narrative_ontology:cs_interpretation_layer_present('f68b17c2-61b5-431e-9512-6daf48d7989a').
narrative_ontology:cs_reading_relation('f68b17c2-61b5-431e-9512-6daf48d7989a', permissive_license_text__commons_coordination_reading, influences).
narrative_ontology:cs_reading_relation('f68b17c2-61b5-431e-9512-6daf48d7989a', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('f68b17c2-61b5-431e-9512-6daf48d7989a', foundational, reciprocity_is_essential_for_fairness).
narrative_ontology:cs_axiom_status(reciprocity_is_essential_for_fairness, holdable).
narrative_ontology:cs_axiom_grounding('f68b17c2-61b5-431e-9512-6daf48d7989a', reciprocity_is_essential_for_fairness, deontological).
narrative_ontology:cs_axiom('f68b17c2-61b5-431e-9512-6daf48d7989a', foundational, unrestricted_reuse_enables_exploitation).
narrative_ontology:cs_axiom_status(unrestricted_reuse_enables_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('f68b17c2-61b5-431e-9512-6daf48d7989a', unrestricted_reuse_enables_exploitation, empirically_contingent).
narrative_ontology:cs_reference_frame('f68b17c2-61b5-431e-9512-6daf48d7989a', copyleft_as_necessary_alternative).
narrative_ontology:cs_drift_state('f68b17c2-61b5-431e-9512-6daf48d7989a', contemporary_licensing_landscape, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f68b17c2-61b5-431e-9512-6daf48d7989a', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, open_source_community).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_developers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, corporate_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and defend copyleft licensing models (e.g., GPL) as a necessary countermeasure to the perceived exploitation enabled by permissive licenses. They benefit from the structural pressure this reading exerts on proprietary developers to reciprocate.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, beneficiary,
    institutional, generational, arbitrage, global).

% Comprises developers and users who contribute to and rely on open-source software. They benefit from the 'viral' nature of copyleft, which ensures that derivative works remain open, preventing the enclosure of the digital commons.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_community, beneficiary,
    organized, generational, mobile, global).

% Develop and sell closed-source software. They are 'victims' in this reading because they are either forced to avoid permissively licensed code (to prevent copyleft 'infection') or to open-source their modifications, which they view as a cost to their business model.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_developers, payer,
    powerful, biographical, constrained, global).

% Integrate various software components, including open-source, into larger commercial products. They face legal and strategic challenges in managing licenses, particularly when permissive and copyleft components interact, leading to costs or limitations on their product development.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, corporate_integrators, payer,
    institutional, biographical, constrained, global).

% Draft and publish permissive licenses (e.g., MIT, Apache) with the intent of maximizing freedom of use. In this counterfactual reading, their original intent is seen as inadvertently enabling exploitation, making them the 'setters' of the initial conditions that lead to the problem.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, permissive_license_authors, agenda_setter,
    moderate, generational, mobile, global).

% Analyze the legal and economic implications of different software licenses. They provide the theoretical framework for understanding the 'exploitation' argument and the necessity of copyleft as an alternative.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates the initial sharing and reuse of software code by minimizing legal friction, allowing developers to build upon existing work without extensive negotiation.
% TRANSFER_FUNCTION: Transfers the right to use, modify, and distribute software code from authors to users. In this reading, it also implicitly transfers the potential for proprietary enclosure from the open-source commons to proprietary developers.
% ABSENT_VOICES: Developers who would prefer a purely public domain approach (no copyright at all) are absent, as this debate is framed within the existing copyright system. Also, users who are indifferent to the licensing model, only caring about functionality, are not actively represented in this ideological conflict.
% DISAPPEARANCE_RATIONALE: If permissive licenses vanished, the software ecosystem would fundamentally change. Proprietary developers would lose a major source of reusable components, forcing them to either develop everything in-house or rely solely on copyleft, which would require them to open-source their own work. The open-source community would see a shift towards more copyleft, but also potentially less initial sharing due to increased friction.
% FOUNDING_PROBLEM: The problem of enabling widespread software reuse and collaboration while respecting authorial rights, without creating excessive legal burdens.
% FOUNDING_PROBLEM_CORROBORATION: Permissive license authors and some developers argue the problem is live, as maximizing freedom of use remains a goal. Copyleft advocates and some legal scholars argue that the original problem has been superseded by the problem of proprietary enclosure, and that permissive licenses now exacerbate this new problem; their arguments are supported by economic analyses of value capture in software ecosystems.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because proprietary developers can leverage permissively licensed code to build closed-source products, capturing value without contributing back to the commons. Suppression (0.6) is present because the 'freedom' offered by permissive licenses can suppress the development of truly reciprocal open-source alternatives by making it easier to 'take' without 'giving'. The theater ratio is low (0.1) as the constraint's function is genuinely about enabling or preventing specific forms of software reuse, not mere performance. Resistance (0.75) is high, reflecting the ongoing ideological and legal battles between copyleft and permissive licensing camps.
 *
 * PERSPECTIVAL GAP:
 *   Copyleft advocates experience this as a necessary constraint to ensure fairness and prevent enclosure, thus a beneficial 'tangled rope' that protects the commons. Proprietary developers, however, experience it as an extractive 'snare' that limits their ability to innovate and profit from open-source components without legal encumbrance. The engine's classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyleft advocates and the broader open-source community are beneficiaries (d near 0.0) as this reading champions their core principles of reciprocity and prevents enclosure. Proprietary software developers and corporate integrators are victims (d near 1.0) as they are forced to either reciprocate or avoid permissively licensed code, incurring costs or limiting their options. Permissive license authors are agenda-setters, but their original intent is reinterpreted through this counterfactual lens.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling permissive licenses as a simple 'rope' (pure coordination) by highlighting the asymmetric extraction inherent in their lack of reciprocity, as argued by copyleft proponents. It acknowledges the coordination function (initial sharing) but emphasizes the extractive outcome for the open-source commons when reciprocity is absent. The 'mandatrophy resolved' flag is not set because the debate over the 'mandate' of permissive licenses (pure freedom vs. anti-exploitation) is still very much alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''copyleft counterfactual'' reading of permissive license text, or is it better understood as a different reading?',
    'Analysis of developer intent and community discourse around specific permissive licenses, focusing on whether the ''exploitation'' narrative is dominant.',
    'If this reading is not dominant, the constraint''s extractiveness and suppression might be lower, potentially reclassifying it as a Rope or even a Mountain (if the ''commons coordination'' reading prevails).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''copyleft counterfactual'' reading of the ''permissive_license_text'' kernel. Sibling readings include ''commons_coordination_reading'' and ''corporate_moat_reading''.').

omega_variable(
    exploitation_definition_ambiguity,
    'What constitutes ''exploitation'' in the context of permissive licensing, and is the lack of reciprocity inherently exploitative?',
    'Empirical studies on value capture in permissive vs. copyleft ecosystems, and philosophical analysis of ''fairness'' in software reuse.',
    'If ''exploitation'' is narrowly defined or reciprocity is not deemed essential for fairness, the extractiveness metric would decrease, potentially shifting the classification towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploitation_definition_ambiguity, preference, 'Ambiguity in defining exploitation and the necessity of reciprocity in software licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'permissive_license_text' kernel, focusing on the counterfactual argument for copyleft's necessity. It highlights the extractive potential of permissive licenses without reciprocity, contrasting with readings that emphasize pure coordination or corporate advantage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
