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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: State Border Authority (Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty_reading' of the
 *   'border_legitimacy' kernel. It posits that a state's authority over its
 *   borders derives from its territorial sovereignty, granting it a
 *   legitimate right to exclude non-citizens. This reading emphasizes the
 *   state's prerogative to define its population and control entry, often
 *   prioritizing national security and resource management. The metrics
 *   reflect the high extraction and suppression inherent in actively
 *   enforcing this right against those seeking entry.
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
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "State Border Authority (Sovereignty Reading)").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '079910b4-a771-482d-b216-cb5f65dbc279').
narrative_ontology:cs_kernel_codification('079910b4-a771-482d-b216-cb5f65dbc279', formalized).
narrative_ontology:cs_authority_grounding('079910b4-a771-482d-b216-cb5f65dbc279', lineage).
narrative_ontology:cs_interpretation_layer_present('079910b4-a771-482d-b216-cb5f65dbc279').
narrative_ontology:cs_reading_relation('079910b4-a771-482d-b216-cb5f65dbc279', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('079910b4-a771-482d-b216-cb5f65dbc279', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('079910b4-a771-482d-b216-cb5f65dbc279', foundational, state_territorial_integrity).
narrative_ontology:cs_axiom_status(state_territorial_integrity, holdable).
narrative_ontology:cs_axiom_grounding('079910b4-a771-482d-b216-cb5f65dbc279', state_territorial_integrity, conventional).
narrative_ontology:cs_axiom('079910b4-a771-482d-b216-cb5f65dbc279', foundational, right_to_exclude_non_citizens).
narrative_ontology:cs_axiom_status(right_to_exclude_non_citizens, holdable).
narrative_ontology:cs_axiom_grounding('079910b4-a771-482d-b216-cb5f65dbc279', right_to_exclude_non_citizens, conventional).
narrative_ontology:cs_reference_frame('079910b4-a771-482d-b216-cb5f65dbc279', westphalian_state_system).
narrative_ontology:cs_drift_state('079910b4-a771-482d-b216-cb5f65dbc279', contemporary_globalization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('079910b4-a771-482d-b216-cb5f65dbc279', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizens_of_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers_denied).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The government and its agencies (border patrol, immigration services) that define, enforce, and interpret border laws. They derive legitimacy and control from the concept of territorial sovereignty and manage the entry and exit of people and goods.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from perceived security, controlled resource access, and the preservation of national identity and social cohesion. They are granted rights and protections within the state's borders that are denied to non-citizens.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizens_of_state, beneficiary,
    organized, biographical, mobile, national).

% Individuals seeking entry who are denied based on the state's sovereign right to exclude. They bear the costs of exclusion, including loss of opportunity, separation from family, danger during attempts to cross, and potential death. Their options are limited to illegal entry, seeking asylum elsewhere, or returning to their origin.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Individuals fleeing persecution or disaster who are denied asylum or protection by a state asserting its sovereign right to control its borders, even if humanitarian claims are present. They face severe risks upon return to their country of origin or prolonged limbo.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers_denied, payer,
    powerless, immediate, trapped, global).

% Organizations and individuals who monitor border practices and advocate for the rights of migrants and asylum seekers, often challenging the absolute nature of state sovereignty in favor of universal human rights. They influence public opinion and legal discourse.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Organizations like the UN and international courts that interpret and apply international law, including conventions on refugees and human rights. They provide frameworks that can challenge or constrain state sovereignty, but often lack direct enforcement power.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_law_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the entry and exit of people, goods, and capital, ensuring security, resource allocation, and the definition of a distinct political community for its citizens.
% TRANSFER_FUNCTION: Transfers security, resource control, and national identity benefits to citizens and the state apparatus, while imposing severe costs of exclusion (loss of life, liberty, opportunity) on excluded migrants.
% ABSENT_VOICES: Excluded migrants and those advocating for universal freedom of movement are structurally excluded from the decision-making processes that define border policy. Their perspectives are often mediated through advocates or international bodies.
% DISAPPEARANCE_RATIONALE: If border authority vanished overnight, the concept of national citizenship and territorial control would collapse. This would lead to massive, uncontrolled population movements, a reordering of global governance, and a fundamental redefinition of political communities.
% FOUNDING_PROBLEM: To define and protect a distinct political community, manage its resources, ensure internal order, and provide security against external threats, thereby establishing the modern nation-state.
% FOUNDING_PROBLEM_CORROBORATION: Most states and their citizens continue to attest to the ongoing need for border control for security, economic stability, and cultural preservation. International relations theory and historical accounts of state formation largely corroborate the foundational role of border authority.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extraction is high (0.85) because the constraint imposes severe costs on excluded migrants, including denial of basic rights, economic opportunity, and safety, often leading to dangerous journeys or prolonged detention. Suppression is very high (0.90) due to the active and often militarized enforcement mechanisms (walls, patrols, surveillance, legal barriers) employed by states to prevent unauthorized entry. Theater ratio is low (0.10) because the enforcement is genuinely functional in preventing entry, not merely performative. Accessibility collapse is high (0.80) as legal and safe alternatives for entry are severely limited for many, forcing them into perilous routes. Resistance is high (0.70) from migrants themselves and their advocates, reflecting the profound impact of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and its citizens, this constraint is a legitimate and necessary coordination mechanism for national self-preservation. From the perspective of excluded migrants, it is a highly extractive and suppressive force that denies fundamental human dignity and opportunity. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and its citizens are the primary beneficiaries, gaining security, resource control, and identity preservation (low directionality). Excluded migrants and denied asylum seekers are the clear targets, bearing the full costs of exclusion (high directionality). Human rights advocates and international law bodies act as observers, challenging the constraint's operation from an analytical distance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_balance,
    'What is the appropriate balance between state territorial sovereignty and universal human rights, particularly the right to seek asylum and freedom of movement?',
    'International legal consensus, evolving customary international law, and the outcomes of landmark cases in international courts that re-evaluate the scope of state discretion at borders.',
    'If human rights are deemed to significantly constrain sovereignty, the constraint''s legitimacy and effective suppression would decrease, potentially reclassifying it towards a Snare or Tangled Rope with lower extraction for migrants. If sovereignty remains paramount, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_balance, conceptual, 'The fundamental tension between state rights and individual rights at borders.').

omega_variable(
    effectiveness_of_exclusion_costs,
    'Does the high cost of exclusion (humanitarian, economic, and social) outweigh the perceived benefits of border control for the state and its citizens in the long run?',
    'Comprehensive, long-term economic and social impact studies that account for the full costs of enforcement, lost labor, demographic shifts, and the social integration challenges of undocumented populations.',
    'If the costs are found to outweigh benefits, the justification for high extraction and suppression would weaken, potentially leading to policy changes that reduce extractiveness and suppression, shifting the constraint towards a Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_exclusion_costs, empirical, 'The net utility of border exclusion for all affected parties.').

omega_variable(
    suppression_mechanism_ambiguity,
    'For excluded migrants, is the measured suppression primarily structural (physical barriers, legal penalties) or does it also involve internalized elements (despair, loss of agency, fear of return)?',
    'Post-exit support programs and psychological assessments for migrants who gain legal status: if psychological suppression persists after structural barriers are removed, it indicates an internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as migrants carry the suppression with them even if they overcome physical barriers. This would amplify the effective extraction for these individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1950, border_legitimacy__sovereignty_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(bord_tr_t1970, border_legitimacy__sovereignty_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(bord_tr_t1990, border_legitimacy__sovereignty_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__sovereignty_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1950, border_legitimacy__sovereignty_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(bord_be_t1970, border_legitimacy__sovereignty_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(bord_be_t1990, border_legitimacy__sovereignty_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__sovereignty_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1950, border_legitimacy__sovereignty_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(bord_su_t1970, border_legitimacy__sovereignty_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(bord_su_t1990, border_legitimacy__sovereignty_reading, suppression_requirement, 1990, 0.83).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__sovereignty_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__sovereignty_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, national_identity_formation).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, citizenship_rights_definition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
