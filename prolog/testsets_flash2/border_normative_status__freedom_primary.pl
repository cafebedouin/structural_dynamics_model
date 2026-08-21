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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Freedom of Movement as Primary Right (Border Normative Status Kernel)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom_primary' reading of the
 *   'border_normative_status' kernel. It posits that freedom of movement is a
 *   fundamental human right, and therefore, state borders, by restricting
 *   this right, are impermissible unless justified by extraordinary
 *   circumstances. From this perspective, border enforcement is a coercive
 *   mechanism that extracts liberty and opportunity from excluded
 *   individuals, while benefiting those within the state who gain from
 *   reduced competition. The structural delta for this reading is that
 *   excluded migrants are victims, and domestic workers who benefit from
 *   restricted labor markets are also victims of the border regime's
 *   restriction on their own movement to seek better opportunities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.95).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.98).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.95).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Freedom of Movement as Primary Right (Border Normative Status Kernel)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c').
narrative_ontology:cs_kernel_codification('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', distributed).
narrative_ontology:cs_authority_grounding('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', distributed).
narrative_ontology:cs_reading_relation('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', border_normative_status__qualified_sovereignty, forecloses).
narrative_ontology:cs_axiom('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', foundational, freedom_of_movement_is_a_fundamental_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_a_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', freedom_of_movement_is_a_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', foundational, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', universal_human_rights_framework).
narrative_ontology:cs_drift_state('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', contemporary_state_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6f5bb0db-4687-4ceb-9924-44aa4f4e3a9c', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, domestic_workers_in_destination_states).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, refugees).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers_in_destination_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denied entry or residency based on national borders, facing severe restrictions on their liberty, economic opportunity, and family reunification. This reading views their exclusion as an impermissible rights violation.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Seeking protection but often detained or denied safe passage at borders, their claims for fundamental rights are systematically suppressed by border regimes. This reading sees their exclusion as a direct violation of the right to seek asylum.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Forced to flee their home countries, they face physical and legal barriers at borders, often in violation of international protection principles. This reading views their forced immobility as a profound rights deprivation.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, refugees, payer,
    powerless, immediate, trapped, global).

% Benefit from reduced competition in the labor market due to border restrictions on foreign workers, potentially leading to higher wages or better working conditions. This reading identifies them as beneficiaries of the restriction on movement.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, domestic_workers_in_destination_states, beneficiary,
    moderate, biographical, constrained, national).

% Those domestic workers who, under a regime of open borders, would face increased competition and potentially lower wages or displacement. This reading identifies them as victims of the current border regime's restriction on movement, as it prevents them from seeking better opportunities elsewhere.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers_in_destination_states, payer,
    powerless, immediate, trapped, national).

% Actively enforce border restrictions, including surveillance, detention, and deportation. Their existence and operations are predicated on the legitimacy of borders as instruments of exclusion, which this reading fundamentally challenges.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Champion the universal right to freedom of movement and challenge state sovereignty claims that restrict it. They provide legal and moral arguments against border enforcement as a violation of fundamental human rights.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint primarily coordinates the exclusion of non-citizens, maintaining a specific demographic and labor market composition within states, rather than solving a legitimate collective action problem.
% TRANSFER_FUNCTION: Transfers the right to free movement and associated economic/social opportunities from excluded migrants, asylum seekers, and refugees to citizens and residents of destination states, particularly those in sectors that would face competition from new arrivals.
% ABSENT_VOICES: The voices of future generations, who might inherit a world with more fluid movement and less nationalistic exclusion, are absent. Also, the voices of those who would benefit from global labor mobility and cultural exchange are suppressed by the current regime.
% DISAPPEARANCE_RATIONALE: If borders ceased to restrict movement overnight, global migration patterns would fundamentally shift, labor markets would rebalance, and the concept of national citizenship would be profoundly altered. The world would rearrange around a new understanding of human mobility.
% FOUNDING_PROBLEM: The constraint of borders was established to define and protect national communities, manage resources, and ensure security within defined territories.
% FOUNDING_PROBLEM_CORROBORATION: States and their citizens often attest that the founding problems of national security and resource management remain live. However, human rights advocates and some economists argue that these problems are either exaggerated, solvable through alternative means, or that the current border regime creates more problems than it solves, rendering the founding problem 'dead' in its original justification.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is very high (0.95) because the constraint fundamentally denies a basic human right, imposing immense costs on excluded individuals. Suppression is also very high (0.98) due to the coercive power of states, including physical barriers, legal penalties, and the lack of viable alternatives for those seeking to cross borders. Theater ratio is low (0.05) because border enforcement is a highly functional, albeit extractive, operation; there is little performative maintenance without real effect. Resistance is high (0.85) reflecting ongoing efforts by migrants, activists, and legal challenges against border regimes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of excluded migrants, the border is a pure snare, denying fundamental rights. From the perspective of states, it is often framed as a necessary coordination mechanism for national security and resource management. This story adopts the migrant-centric, rights-based perspective, classifying the constraint as a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants, asylum seekers, and refugees are full targets (d=1.0) as they bear the direct and severe costs of the constraint. Domestic workers in destination states are beneficiaries (d=0.0) as they gain from reduced labor competition. However, this reading also identifies them as victims of the border regime's restriction on their own movement, as it prevents them from seeking better opportunities elsewhere. Border enforcement agencies are agenda-setters, actively maintaining the constraint. Human rights advocates are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling border controls as legitimate coordination (a rope) by highlighting the fundamental rights violation and the coercive nature of exclusion. It emphasizes that the coordination story (national security, resource management) is cover for extraction from those denied entry, and that the original mandate for borders is superseded by the primary right to freedom of movement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_exclusion_justification,
    'What constitutes ''extraordinary justification'' for exclusion, and who adjudicates it?',
    'Development of international legal frameworks and precedents that define and enforce strict criteria for legitimate exclusion, with independent judicial oversight.',
    'If criteria are too broad or state-defined, the constraint remains highly extractive. If criteria are narrow and externally adjudicated, extractiveness would decrease significantly, potentially reclassifying towards a scaffold or even a rope for truly exceptional cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_exclusion_justification, conceptual, 'Ambiguity in what justifies border restrictions under a primary right to movement.').

omega_variable(
    internalized_border_legitimacy,
    'To what extent is the suppression of free movement internalized by individuals, leading them to accept border legitimacy even when it harms them?',
    'Sociological studies on migrant aspirations and perceptions of border justice, and analysis of ''brain drain'' vs. ''brain circulation'' in open vs. closed border regimes.',
    'If internalized suppression is high, the effective extractiveness is higher than structural measures suggest, as individuals self-regulate their mobility even in the absence of direct enforcement. This would make the snare more insidious.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_border_legitimacy, empirical, 'Structural vs. internalized suppression mechanism for border legitimacy.').

omega_variable(
    displaced_domestic_workers_victim_status,
    'Is the displacement of domestic workers due to increased competition from open borders a legitimate ''victim'' outcome, or a necessary adjustment to a more just global labor market?',
    'Economic modeling of labor market adjustments under open borders, combined with ethical analysis of distributive justice in a global context.',
    'If their displacement is considered a legitimate victim outcome, it reinforces the snare classification by highlighting broader harms. If it''s seen as a necessary adjustment, it might reduce the perceived victim set, but the core rights violation for excluded migrants remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_domestic_workers_victim_status, preference, 'Whether labor market competition from open borders constitutes a ''victim'' outcome.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__freedom_primary, theater_ratio, 10, 0.08).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__freedom_primary, theater_ratio, 20, 0.07).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__freedom_primary, theater_ratio, 30, 0.06).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__freedom_primary, theater_ratio, 40, 0.05).
narrative_ontology:measurement(bord_tr_t50, border_normative_status__freedom_primary, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(bord_be_t10, border_normative_status__freedom_primary, base_extractiveness, 10, 0.88).
narrative_ontology:measurement(bord_be_t20, border_normative_status__freedom_primary, base_extractiveness, 20, 0.91).
narrative_ontology:measurement(bord_be_t30, border_normative_status__freedom_primary, base_extractiveness, 30, 0.93).
narrative_ontology:measurement(bord_be_t40, border_normative_status__freedom_primary, base_extractiveness, 40, 0.94).
narrative_ontology:measurement(bord_be_t50, border_normative_status__freedom_primary, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(bord_su_t10, border_normative_status__freedom_primary, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(bord_su_t20, border_normative_status__freedom_primary, suppression_requirement, 20, 0.94).
narrative_ontology:measurement(bord_su_t30, border_normative_status__freedom_primary, suppression_requirement, 30, 0.96).
narrative_ontology:measurement(bord_su_t40, border_normative_status__freedom_primary, suppression_requirement, 40, 0.97).
narrative_ontology:measurement(bord_su_t50, border_normative_status__freedom_primary, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'freedom_primary' reading of the 'border_normative_status' kernel. It is structurally distinct from the 'sovereignty_primary' and 'qualified_sovereignty' readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
