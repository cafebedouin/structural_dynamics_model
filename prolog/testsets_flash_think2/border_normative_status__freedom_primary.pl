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
 *   human_readable: Border Normative Status: Freedom of Movement Primary Reading
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom_primary' reading of the
 *   'border_normative_status' kernel. From this perspective, freedom of
 *   movement is a fundamental human right, and existing state borders
 *   constitute an impermissible restriction requiring extraordinary
 *   justification. The current border regime is thus interpreted as a snare,
 *   extracting freedom and imposing costs on individuals seeking movement,
 *   while benefiting nation-states and their citizens through illegitimate
 *   means. The structural delta for this reading means that 'excluded
 *   migrants' are not a legitimate category but rather victims of a rights
 *   violation, and 'displaced domestic workers' are recognized as victims of
 *   the border regime's indirect effects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.9).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.95).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.9).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Normative Status: Freedom of Movement Primary Reading").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '752b2f40-a3bd-4204-94b7-ac7ba7249bb3').
narrative_ontology:cs_kernel_codification('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', formalized).
narrative_ontology:cs_authority_grounding('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', lineage).
narrative_ontology:cs_interpretation_layer_present('752b2f40-a3bd-4204-94b7-ac7ba7249bb3').
narrative_ontology:cs_reading_relation('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', border_normative_status__qualified_sovereignty, forecloses).
narrative_ontology:cs_axiom('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', foundational, freedom_of_movement_is_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', freedom_of_movement_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', foundational, state_exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(state_exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', state_exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', universal_human_rights_framework).
narrative_ontology:cs_drift_state('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', contemporary_global_mobility_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('752b2f40-a3bd-4204-94b7-ac7ba7249bb3', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, nation_states).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, citizens_of_states).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, individuals_seeking_movement).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities that claim and enforce territorial borders, asserting the right to control entry and exit. From this reading's perspective, they illegitimately restrict fundamental human rights for perceived national interests.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, nation_states, agenda_setter,
    institutional, generational, constrained, global).

% Individuals whose fundamental human right to freedom of movement is restricted or denied by state borders. They bear the direct costs of exclusion, detention, and dangerous irregular migration routes.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, individuals_seeking_movement, payer,
    powerless, immediate, trapped, global).

% Members of nation-states who benefit from the perceived security, cultural homogeneity, and economic stability that border restrictions are claimed to provide. Their freedom of movement is generally unhindered within their own state's borders and often privileged across others.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, citizens_of_states, beneficiary,
    organized, biographical, mobile, national).

% Workers within a state whose economic position is negatively impacted by the specific patterns of migration allowed or restricted by border policies. This reading identifies them as victims of the border regime's indirect effects, which can create unfair labor competition or depress wages.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    powerless, biographical, constrained, local).

% Organizations and individuals who champion the universal right to freedom of movement and challenge state-centric justifications for border restrictions. They analyze the ethical and legal implications of border regimes.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Government bodies tasked with implementing and enforcing border controls, including surveillance, detention, and deportation. From this reading's perspective, their actions constitute rights violations requiring extraordinary justification.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, nation_states).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the border regime illegitimately 'coordinates' the exclusion of individuals, thereby allocating perceived national resources and security benefits to citizens at the expense of universal human rights.
% TRANSFER_FUNCTION: Transfers the fundamental right to freedom of movement from individuals to the discretionary control of nation-states, enabling states to manage populations and labor markets, and transferring perceived security and economic benefits to citizens.
% ABSENT_VOICES: Those whose movement is restricted or denied, and those who would benefit from open borders (e.g., certain industries, families separated by borders). Their perspectives are systematically marginalized or silenced by the state-centric framing of border control.
% DISAPPEARANCE_RATIONALE: If borders and their enforcement vanished overnight, global society would undergo a fundamental reorganization. Massive population shifts, economic rebalancing, and the redefinition of national identity would occur, leading to a radically different global political and social landscape.
% FOUNDING_PROBLEM: The problem of managing populations, resources, and national identity within defined territories, and the perceived need to protect existing communities from external threats or economic competition.
% FOUNDING_PROBLEM_CORROBORATION: Nation-states and many citizens attest that the founding problems (security, resource management, cultural preservation) are still live. Human rights advocates and some economists, from outside the benefiting parties, argue that these problems are often pretexts for exclusion and that the existing regime creates more problems than it solves, rendering the 'founding problem' largely a cover story.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.90) and suppression (0.95) are very high because this reading views borders as fundamentally illegitimate restrictions on a universal right, maintained through active coercion. The coordination function (e.g., national security, economic management) is seen as a cover story for this extraction. Theater ratio is low (0.20) because the enforcement is genuinely aimed at preventing movement, even if the justifications are contested. The increasing metrics over time reflect the growing intensity of border enforcement and the philosophical challenge to its legitimacy since the Universal Declaration of Human Rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nation-states, borders are legitimate instruments of sovereignty and security. From the 'freedom_primary' reading, these same borders are illegitimate snares. The engine's classification will highlight this divergence by computing a snare classification from the authored metrics, contrasting with the 'sovereignty_primary' reading's likely rope or tangled_rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Nation-states and their citizens are beneficiaries (d near 0.0) as they gain control and perceived benefits from restricting movement. Individuals seeking movement and displaced domestic workers are targets (d near 1.0) as their rights are violated and they bear the direct and indirect costs of border enforcement. Border enforcement agencies are agenda-setters, actively implementing the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_justification_ambiguity,
    'What constitutes ''extraordinary justification'' for exclusion, and who adjudicates it?',
    'Development of international legal precedents and a robust, independent international tribunal with jurisdiction over border claims, or a global consensus on specific, narrowly defined threats that would justify temporary exclusion.',
    'If ''extraordinary justification'' is defined too broadly or adjudicated by states themselves, the constraint remains highly extractive; if narrowly defined and externally adjudicated, it could shift towards a scaffold or rope for specific, temporary circumstances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_ambiguity, conceptual, 'Ambiguity in the standard for legitimate border exclusion.').

omega_variable(
    empirical_impact_of_open_borders,
    'What would be the actual empirical impact of open borders on domestic labor markets, social cohesion, and resource distribution?',
    'Large-scale, long-term empirical studies or natural experiments in regions with highly porous or open borders, analyzing economic, social, and demographic shifts.',
    'If empirical studies show net positive or manageable impacts, it would weaken the ''founding problem'' justifications for borders, reinforcing the snare classification. If severe negative impacts are consistently demonstrated, it could lend some (though not ''extraordinary'') justification to certain forms of border management, potentially shifting the constraint''s perceived extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_impact_of_open_borders, empirical, 'Uncertainty about the real-world consequences of unrestricted movement.').

omega_variable(
    state_sovereignty_vs_individual_rights_priority,
    'Is state sovereignty a foundational principle that inherently includes border control, or is it a derivative concept subordinate to universal individual human rights?',
    'A global philosophical and legal consensus on the hierarchy of normative claims, potentially through a new international convention or a landmark ruling by a universally recognized court of human rights.',
    'If individual rights are universally recognized as primary, the ''freedom_primary'' reading''s snare classification of existing borders is reinforced. If state sovereignty is affirmed as foundational, the ''sovereignty_primary'' reading gains legitimacy, and the current border regime might be reclassified as a rope or tangled_rope from a different perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_sovereignty_vs_individual_rights_priority, conceptual, 'Fundamental conceptual conflict between state authority and individual rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__freedom_primary, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(bord_tr_t1960, border_normative_status__freedom_primary, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(bord_tr_t1975, border_normative_status__freedom_primary, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__freedom_primary, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(bord_tr_t2005, border_normative_status__freedom_primary, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__freedom_primary, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_normative_status__freedom_primary, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(bord_be_t1960, border_normative_status__freedom_primary, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(bord_be_t1975, border_normative_status__freedom_primary, base_extractiveness, 1975, 0.8).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__freedom_primary, base_extractiveness, 1990, 0.85).
narrative_ontology:measurement(bord_be_t2005, border_normative_status__freedom_primary, base_extractiveness, 2005, 0.88).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__freedom_primary, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_normative_status__freedom_primary, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(bord_su_t1960, border_normative_status__freedom_primary, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(bord_su_t1975, border_normative_status__freedom_primary, suppression_requirement, 1975, 0.85).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__freedom_primary, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(bord_su_t2005, border_normative_status__freedom_primary, suppression_requirement, 2005, 0.93).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__freedom_primary, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
