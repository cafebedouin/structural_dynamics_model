% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Authority (Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty reading' of border authority,
 *   asserting the state's legitimate right to exclude based on territorial
 *   sovereignty. It is one reading of the broader 'border_legitimacy' kernel,
 *   which is contested by alternative readings emphasizing freedom of
 *   movement or humanitarian obligations. This reading frames state
 *   enforcement as a legitimate exercise of power, with excluded migrants as
 *   the primary victims of this extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.85).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.9).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, snare).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Authority (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, 'f39d836f-47f7-4ce1-82d4-d7510a23c324').
narrative_ontology:cs_kernel_codification('f39d836f-47f7-4ce1-82d4-d7510a23c324', formalized).
narrative_ontology:cs_authority_grounding('f39d836f-47f7-4ce1-82d4-d7510a23c324', lineage).
narrative_ontology:cs_interpretation_layer_present('f39d836f-47f7-4ce1-82d4-d7510a23c324').
narrative_ontology:cs_reading_relation('f39d836f-47f7-4ce1-82d4-d7510a23c324', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('f39d836f-47f7-4ce1-82d4-d7510a23c324', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('f39d836f-47f7-4ce1-82d4-d7510a23c324', foundational, territorial_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f39d836f-47f7-4ce1-82d4-d7510a23c324', territorial_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('f39d836f-47f7-4ce1-82d4-d7510a23c324', foundational, state_has_right_to_exclude).
narrative_ontology:cs_axiom_status(state_has_right_to_exclude, holdable).
narrative_ontology:cs_axiom_grounding('f39d836f-47f7-4ce1-82d4-d7510a23c324', state_has_right_to_exclude, conventional).
narrative_ontology:cs_reference_frame('f39d836f-47f7-4ce1-82d4-d7510a23c324', westphalian_state_system).
narrative_ontology:cs_drift_state('f39d836f-47f7-4ce1-82d4-d7510a23c324', contemporary_globalization_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f39d836f-47f7-4ce1-82d4-d7510a23c324', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, sovereign_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizenry).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces its right to control its borders and determine who enters its territory, based on the principle of territorial sovereignty. Benefits from perceived stability and control over resources and social order.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, sovereign_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the state's control over borders through perceived national security, cultural cohesion, and protection of domestic labor markets. Supports the state's right to exclude as a means of preserving their way of life and economic interests.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizenry, beneficiary,
    organized, biographical, mobile, national).

% Are denied entry to desired territories, often facing severe economic hardship, danger, or statelessness. Bear the direct costs of exclusion, including separation from family, loss of opportunity, and risk of refoulement.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% While international law grants a right to seek asylum, this reading prioritizes state sovereignty, often leading to prolonged detention, denial of due process, or rejection at the border, despite credible fears of persecution. They bear the costs of legal and physical barriers.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Monitor state border practices and advocate for the rights of migrants and asylum seekers, often challenging the absolute nature of state sovereignty in favor of universal human rights. They provide legal aid and public awareness.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear framework for state control over its territory and population, coordinating national security, resource allocation, and social planning within defined geographical limits.
% TRANSFER_FUNCTION: Transfers the right to reside and access resources from excluded individuals to the existing citizenry, enforced by the state's monopoly on legitimate force at its borders.
% ABSENT_VOICES: The voices of those seeking entry, particularly economic migrants and those without clear asylum claims, are systematically excluded from the policy-making process, despite being the primary targets of the constraint. Their perspectives on global inequality and the right to seek a better life are not formally considered.
% DISAPPEARANCE_RATIONALE: If the state's right to exclude vanished overnight, global migration patterns would fundamentally shift, leading to massive population movements, rapid demographic changes in many nations, and a complete re-evaluation of national identity and resource distribution. The concept of the 'nation-state' as currently understood would be profoundly altered.
% FOUNDING_PROBLEM: The need for states to define and defend their territorial integrity, manage their populations, and secure resources against external threats or uncontrolled influxes, establishing a stable basis for political and social order.
% FOUNDING_PROBLEM_CORROBORATION: The sovereign state and its citizenry consistently attest that the founding problem of territorial integrity and national security remains live, citing ongoing geopolitical instability, economic pressures, and the need to maintain social cohesion. This is corroborated by historical precedents in international law and the continued existence of state borders globally, though challenged by human rights advocates.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the constraint fundamentally denies a basic human desire (to move and seek opportunity) to a large population, imposing severe costs. Suppression is very high (0.90) due to the state's monopoly on force and the active deployment of physical and legal barriers to prevent entry. Theater ratio is low (0.10) as the enforcement is direct and functional, not performative; the state genuinely intends to exclude. Accessibility collapse is high (0.75) as legal alternatives for entry are severely limited for many. Resistance is also high (0.70) from migrants themselves and their advocates, reflecting the severity of the extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the sovereign state and its citizenry, this constraint is a legitimate and necessary exercise of self-determination, ensuring security and stability. From the perspective of excluded migrants and human rights advocates, it is a highly extractive and suppressive mechanism that denies fundamental rights and opportunities. The engine's classification will reflect this divergence based on the declared roles and structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign state and its citizenry are clear beneficiaries, as the constraint directly serves their interests in control and security (low d). Excluded migrants and asylum seekers are direct targets, bearing the full cost of exclusion (high d). International human rights advocates act as analytical observers, challenging the constraint's legitimacy and impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy in this reading; the founding problem of territorial control and national security is considered 'live' by its beneficiaries. The high extractiveness and suppression are actively maintained, indicating it is a snare, not a decaying piton. The classification prevents mislabeling a robustly enforced, extractive system as merely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of state sovereignty, or is it primarily a mechanism for economic protectionism and social control?',
    'Analysis of state policies and rhetoric: if exclusion criteria primarily target economic migrants while allowing high-skilled labor, it suggests economic protectionism rather than pure sovereignty. If it targets specific ethnic or religious groups, it suggests social control.',
    'If primarily economic protectionism or social control, the ''sovereignty'' framing is a cover story, increasing the effective extractiveness and potentially reclassifying it as a more explicit snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between genuine sovereignty and instrumental use of the concept.').

omega_variable(
    sovereignty_vs_humanitarian_obligation,
    'To what extent does the principle of territorial sovereignty genuinely conflict with international humanitarian obligations, particularly regarding asylum seekers?',
    'Legal analysis of international treaties and customary law, and judicial rulings on the hierarchy of norms. Empirical observation of state practice in balancing these claims.',
    'If humanitarian obligations are found to supersede absolute sovereignty in specific contexts, the state''s ''right to exclude'' would be constrained, reducing extractiveness for asylum seekers and potentially shifting the constraint towards a tangled_rope or even a rope for that specific group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_humanitarian_obligation, empirical, 'The tension between state sovereignty and international human rights law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_legitimacy__sovereignty_reading, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(bord_be_t1965, border_legitimacy__sovereignty_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(bord_be_t1985, border_legitimacy__sovereignty_reading, base_extractiveness, 1985, 0.8).
narrative_ontology:measurement(bord_be_t2005, border_legitimacy__sovereignty_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_legitimacy__sovereignty_reading, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(bord_su_t1965, border_legitimacy__sovereignty_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(bord_su_t1985, border_legitimacy__sovereignty_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(bord_su_t2005, border_legitimacy__sovereignty_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__sovereignty_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
