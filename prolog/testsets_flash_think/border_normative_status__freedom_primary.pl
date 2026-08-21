% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Borders as Impermissible Restrictions on Freedom of Movement
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story analyzes the normative status of borders from the
 *   perspective that freedom of movement is a fundamental human right, and
 *   borders constitute an impermissible restriction requiring extraordinary
 *   justification. From this 'freedom primary' reading, borders are highly
 *   extractive and suppressive, functioning as a snare that benefits states
 *   and their citizens at the expense of those seeking to move. The analysis
 *   focuses on the structural illegitimacy of exclusion when a universal
 *   right is asserted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.9).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.95).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.9).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Borders as Impermissible Restrictions on Freedom of Movement").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, 'e7f15f26-6e35-460c-bc96-f0c1d261c6ce').
narrative_ontology:cs_kernel_codification('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', formalized).
narrative_ontology:cs_authority_grounding('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', extraction).
narrative_ontology:cs_interpretation_layer_present('e7f15f26-6e35-460c-bc96-f0c1d261c6ce').
narrative_ontology:cs_reading_relation('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', border_normative_status__qualified_sovereignty, forecloses).
narrative_ontology:cs_axiom('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', foundational, freedom_of_movement_is_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', freedom_of_movement_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', foundational, state_exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(state_exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', state_exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', universal_human_rights_framework).
narrative_ontology:cs_drift_state('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', contemporary_global_migration_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e7f15f26-6e35-460c-bc96-f0c1d261c6ce', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, citizens_of_destination_states).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, states_maintaining_control).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, individuals_seeking_economic_opportunity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the sovereign states that define and enforce their borders, controlling entry and exit. From this reading's perspective, they illegitimately assert authority over a fundamental human right, benefiting from the control over labor markets, social services, and national identity that borders afford.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, states_maintaining_control, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizens who benefit from the restriction of movement, perceiving advantages in terms of controlled labor markets, social welfare systems, and cultural homogeneity. They are beneficiaries of the constraint's extractive function, which limits competition and preserves existing social structures.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, citizens_of_destination_states, beneficiary,
    organized, biographical, mobile, national).

% Individuals who are denied entry or residence by border controls, despite seeking to exercise their fundamental right to movement. They bear the direct costs of the constraint through lost opportunities, separation from family, and often dangerous attempts to circumvent restrictions. From this reading's perspective, they are victims of an illegitimate system.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Individuals seeking protection who are often trapped at borders or in transit, facing legal and physical barriers to entry. Their right to seek asylum is curtailed by the border constraint, making them direct victims of its enforcement.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% People seeking to improve their economic situation by moving across borders, whose opportunities are severely limited by the constraint. They are victims of the border's function in maintaining global economic inequalities by restricting labor mobility.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, individuals_seeking_economic_opportunity, payer,
    powerless, biographical, constrained, global).

% Organizations and individuals who monitor border practices, document human rights violations, and advocate for policies that uphold freedom of movement. They analyze the constraint's operation and challenge its legitimacy from a universal rights perspective.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, states_maintaining_control).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint primarily coordinates the exclusion of non-citizens, rather than solving a legitimate collective action problem for all humanity. It coordinates the maintenance of state sovereignty and national identity at the expense of universal rights.
% TRANSFER_FUNCTION: Transfers security, economic stability, and cultural homogeneity to citizens of destination states, from the freedom, opportunity, and often safety of excluded non-citizens.
% ABSENT_VOICES: Excluded migrants, asylum seekers, and individuals seeking economic opportunity are often denied a voice in the policy-making processes that determine border regimes. Their perspectives, which would highlight the rights violations and human costs, are systematically marginalized.
% DISAPPEARANCE_RATIONALE: If borders and their enforcement vanished overnight, there would be massive global migration flows, leading to significant demographic, economic, and social reorganization worldwide. Labor markets would rebalance, and the concept of national citizenship would be fundamentally altered.
% FOUNDING_PROBLEM: The assertion of state territoriality and the right to exclude non-members, prioritizing national interests and collective self-determination over universal human rights.
% FOUNDING_PROBLEM_CORROBORATION: States and many of their citizens continue to assert the legitimacy and necessity of border controls, citing national security, economic stability, and cultural preservation. However, human rights organizations, international legal scholars, and migrant advocacy groups consistently challenge this framing, arguing that the 'problem' borders solve is secondary to the fundamental right to movement, and that the current system is a violation.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because borders fundamentally deny a claimed universal right, imposing immense costs on those excluded. Suppression is also very high (0.95) due to the active, often militarized, enforcement mechanisms employed by states to prevent unauthorized entry. Theater ratio is low (0.1) because border enforcement is a very real and functional activity, though the justifications for it may be increasingly performative in the face of human rights claims. Resistance is high (0.7) from migrants themselves, advocacy groups, and legal challenges. Accessibility collapse is high (0.88) as legal alternatives to crossing borders are severely limited for many.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states and many citizens, borders are legitimate instruments of sovereignty and security. From this 'freedom primary' reading, however, the same structures are illegitimate snares. The engine's computation of per-seat classification will highlight this divergence, showing a Snare for those whose rights are denied, and potentially a Rope or even Mountain for those who benefit from or enforce the system.
 *
 * DIRECTIONALITY LOGIC:
 *   States maintaining control are clear agenda-setters and beneficiaries, as they directly control and profit from the border regime. Citizens of destination states are also beneficiaries, enjoying the perceived benefits of controlled migration. Excluded migrants, asylum seekers, and individuals seeking economic opportunity are the primary targets and victims, bearing the full weight of the constraint's extractiveness and suppression. Human rights advocates serve as analytical observers, documenting the constraint's impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_exclusion_ambiguity,
    'Is the exclusion of non-citizens by state borders a legitimate exercise of sovereignty or an impermissible restriction of a fundamental human right?',
    'International legal consensus shift, or a global referendum on the normative priority of freedom of movement versus state territorial control.',
    'If exclusion is deemed legitimate, the constraint''s extractiveness would be re-evaluated downward, potentially reclassifying it from Snare to Tangled Rope or even Rope. If deemed impermissible, the Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_exclusion_ambiguity, conceptual, 'The fundamental conceptual disagreement over the normative status of borders.').

omega_variable(
    sovereignty_primary_classification_delta,
    'How would the classification change under the ''sovereignty_primary'' reading, which asserts foundational state authority to exclude non-members?',
    'Applying the ''sovereignty_primary'' reading to the same structural facts and re-evaluating extractiveness and victim status.',
    'Under ''sovereignty_primary'', extractiveness would be significantly lower (as exclusion is legitimate), and ''excluded_migrants'' would likely exit the victim set, potentially reclassifying the constraint as a Rope or even a Mountain (from the state''s perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_primary_classification_delta, conceptual, 'Impact of the ''sovereignty_primary'' sibling reading on classification.').

omega_variable(
    qualified_sovereignty_classification_delta,
    'How would the classification change under the ''qualified_sovereignty'' reading, which balances state control with human rights obligations?',
    'Applying the ''qualified_sovereignty'' reading to the same structural facts and re-evaluating extractiveness and the scope of legitimate enforcement.',
    'Under ''qualified_sovereignty'', extractiveness would be lower than ''freedom_primary'' but higher than ''sovereignty_primary'', and the constraint might be classified as a Tangled Rope, acknowledging both coordination (for states) and extraction (for migrants, but with limits).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qualified_sovereignty_classification_delta, conceptual, 'Impact of the ''qualified_sovereignty'' sibling reading on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__freedom_primary, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(bord_tr_t1960, border_normative_status__freedom_primary, theater_ratio, 1960, 0.06).
narrative_ontology:measurement(bord_tr_t1980, border_normative_status__freedom_primary, theater_ratio, 1980, 0.07).
narrative_ontology:measurement(bord_tr_t2000, border_normative_status__freedom_primary, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__freedom_primary, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__freedom_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_normative_status__freedom_primary, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(bord_be_t1960, border_normative_status__freedom_primary, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(bord_be_t1980, border_normative_status__freedom_primary, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(bord_be_t2000, border_normative_status__freedom_primary, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__freedom_primary, base_extractiveness, 2010, 0.88).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__freedom_primary, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_normative_status__freedom_primary, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(bord_su_t1960, border_normative_status__freedom_primary, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(bord_su_t1980, border_normative_status__freedom_primary, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(bord_su_t2000, border_normative_status__freedom_primary, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__freedom_primary, suppression_requirement, 2010, 0.93).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__freedom_primary, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, global_inequality).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, refugee_status_determination).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'border_normative_status' kernel. Each reading yields a different structural classification due to differing normative premises regarding the legitimacy of state borders and the priority of human rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
