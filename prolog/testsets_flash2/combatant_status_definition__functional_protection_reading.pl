% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Status-Independent Humane Treatment and Fair Trial Rights (Functional Protection Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the 'functional protection' reading of
 *   combatant status definition within International Humanitarian Law (IHL).
 *   It asserts that all persons detained in armed conflict are entitled to a
 *   baseline of humane treatment and fair trial rights under Common Article 3
 *   of the Geneva Conventions, irrespective of their formal combatant status.
 *   This reading aims to close protection gaps that arise from restrictive
 *   interpretations of status, particularly concerning non-state armed
 *   groups. The metrics reflect a relatively low extractiveness, as the
 *   constraint primarily provides protections rather than extracting from
 *   beneficiaries, though some states perceive an 'extraction' of operational
 *   flexibility.
 *
 * KEY AGENTS:
 *   - all_detained_persons: Primary beneficiary (powerless/trapped)
 *   - state_parties_to_geneva_conventions: Agenda-setter (institutional/constrained)
 *   - humanitarian_organizations: Beneficiary (organized/mobile)
 *   - military_commanders: Payer (powerful/constrained)
 *   - legal_scholars_and_advocates: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.15).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.25).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Status-Independent Humane Treatment and Fair Trial Rights (Functional Protection Reading)").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, 'c7df5691-1ba3-43a5-a35b-139830093296').
narrative_ontology:cs_kernel_codification('c7df5691-1ba3-43a5-a35b-139830093296', formalized).
narrative_ontology:cs_authority_grounding('c7df5691-1ba3-43a5-a35b-139830093296', lineage).
narrative_ontology:cs_interpretation_layer_present('c7df5691-1ba3-43a5-a35b-139830093296').
narrative_ontology:cs_reading_relation('c7df5691-1ba3-43a5-a35b-139830093296', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7df5691-1ba3-43a5-a35b-139830093296', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('c7df5691-1ba3-43a5-a35b-139830093296', foundational, universal_human_dignity_in_conflict).
narrative_ontology:cs_axiom_status(universal_human_dignity_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('c7df5691-1ba3-43a5-a35b-139830093296', universal_human_dignity_in_conflict, deontological).
narrative_ontology:cs_axiom('c7df5691-1ba3-43a5-a35b-139830093296', foundational, status_independent_minimum_protections).
narrative_ontology:cs_axiom_status(status_independent_minimum_protections, holdable).
narrative_ontology:cs_axiom_grounding('c7df5691-1ba3-43a5-a35b-139830093296', status_independent_minimum_protections, conventional).
narrative_ontology:cs_reference_frame('c7df5691-1ba3-43a5-a35b-139830093296', common_article_3_universal_application).
narrative_ontology:cs_drift_state('c7df5691-1ba3-43a5-a35b-139830093296', post_9_11_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c7df5691-1ba3-43a5-a35b-139830093296', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, humanitarian_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, military_commanders).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, universal_human_dignity).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, rule_of_law_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives minimum protections under Common Article 3, including humane treatment and fair trial rights, regardless of their combatant status. Their situation is one of extreme vulnerability, with no exit options from detention.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% Are bound by Common Article 3 to provide minimum protections. They administer detention facilities and are responsible for implementing these rights, though some states resist broad application to non-state actors.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% Advocate for the universal application of Common Article 3 protections and monitor compliance. This reading strengthens their mandate and provides a clear legal basis for their interventions.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, humanitarian_organizations, beneficiary,
    organized, biographical, mobile, global).

% Bear the operational burden of ensuring humane treatment and fair trial processes for all detainees, regardless of status. This can be perceived as complicating battlefield operations and intelligence gathering.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, military_commanders, payer,
    powerful, immediate, constrained, local).

% Analyze and interpret international humanitarian law, promoting the functional protection reading as essential for upholding human dignity and the rule of law in conflict. They influence judicial and policy debates.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, legal_scholars_and_advocates, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal baseline of humane treatment and due process for all persons deprived of liberty in armed conflict, preventing a legal vacuum based on status determination and ensuring a minimum standard of conduct for detaining powers.
% TRANSFER_FUNCTION: Transfers the obligation to provide humane treatment and fair trial rights from a status-dependent framework to a universal one, placing a consistent burden on detaining powers and granting consistent rights to all detainees.
% ABSENT_VOICES: Those who advocate for a more restrictive, state-centric interpretation of combatant status, which would limit protections to formally recognized combatants, are often excluded from the functional protection discourse, as their premise is directly challenged by this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for detainees would revert to a more status-dependent and potentially arbitrary system, leading to significant legal vacuums, increased human rights abuses, and greater operational uncertainty for detaining powers regarding non-state actors.
% FOUNDING_PROBLEM: The original Geneva Conventions left gaps in protection for persons not clearly falling into defined combatant categories, particularly in non-international armed conflicts, leading to arbitrary treatment and abuses.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian organizations, international courts, and a broad consensus among legal scholars attest that the problem of arbitrary detention and treatment based on status ambiguity remains live, particularly with the rise of non-state armed groups and asymmetric conflicts. This is corroborated by numerous reports from conflict zones and judicial decisions.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint's primary function is to provide a floor of protection, not to extract resources. Any 'cost' is borne by detaining powers in terms of operational constraints, which is minimal compared to the benefit of universal protection. Suppression (0.25) is moderate, reflecting ongoing resistance from some states to fully implement this broad interpretation, requiring active enforcement through international pressure and legal mechanisms. Theater ratio (0.1) is low, as the commitment to humane treatment is generally genuine, though some performative compliance exists. The slight increase in extractiveness and suppression around 2001 reflects the post-9/11 'war on terror' context, where some states sought to limit protections for alleged terrorists.
 *
 * PERSPECTIVAL GAP:
 *   Detained persons experience this as a vital lifeline, while some military commanders may perceive it as an undue burden on operations. State parties navigate between their legal obligations and perceived national security interests. The engine's classification will reflect these divergent experiences based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   All detained persons are full beneficiaries (d=0.0) as the constraint directly grants them rights. Humanitarian organizations are also beneficiaries (d low) as it strengthens their advocacy. State parties are agenda-setters (d symmetric to slightly beneficiary) as they are the primary duty-bearers but also benefit from a stable legal framework. Military commanders are payers (d high) as they bear the direct operational costs of implementation. Legal scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the provision of universal protections as an extractive mechanism. While some states may argue it 'extracts' operational flexibility, the core function is coordination around a humanitarian baseline. The constraint's mandate (preventing arbitrary treatment) remains live and highly relevant in contemporary conflicts, preventing mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_non_international_armed_conflict,
    'How broadly should ''non-international armed conflict'' be interpreted, as this directly impacts the applicability of Common Article 3 to various non-state actors?',
    'Further jurisprudence from international courts (e.g., ICTY, ICC) and state practice, clarifying the thresholds for intensity and organization of non-state groups.',
    'A broader interpretation would expand the scope of beneficiaries and increase the perceived ''burden'' on detaining states, potentially increasing resistance. A narrower interpretation would limit protections, creating new legal gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_non_international_armed_conflict, empirical, 'Ambiguity in the definition of non-international armed conflict and its impact on Common Article 3 applicability.').

omega_variable(
    status_determination_precondition,
    'Is the functional protection reading truly status-independent, or do states still implicitly or explicitly use status determination as a precondition for full application of rights?',
    'Empirical studies of detention practices in various conflicts, comparing treatment of different categories of detainees, and analysis of national legal frameworks for IHL implementation.',
    'If status determination remains a de facto precondition, the effective extractiveness (from detainees) and suppression (of their rights) would be higher than currently assessed, indicating a gap between the legal claim and actual practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_determination_precondition, empirical, 'Gap between de jure status-independence and de facto status-dependent application of protections.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine functional protection reading of the combatant status kernel, or is it a strategic framing to achieve other policy goals?',
    'Analysis of state adherence to international court rulings and consistent application of Common Article 3 in diverse conflict scenarios, even when politically inconvenient.',
    'If it''s a strategic framing, the underlying extractiveness from detainees might be higher, and the claimed coordination function might be a cover for selective application of IHL.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''combatant_status_definition'' kernel. This ''functional_protection_reading'' emphasizes universal minimum protections. Sibling readings include ''state_centric_reading'' (combatant status requires formal state military organization) and ''national_liberation_reading'' (combatant status extends to non-state groups fighting specific regimes). The disagreement is located in the scope and preconditions for IHL protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comb_tr_t1970, combatant_status_definition__functional_protection_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__functional_protection_reading, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__functional_protection_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__functional_protection_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__functional_protection_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comb_be_t1970, combatant_status_definition__functional_protection_reading, base_extractiveness, 1970, 0.12).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__functional_protection_reading, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__functional_protection_reading, base_extractiveness, 2001, 0.18).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__functional_protection_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__functional_protection_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(comb_su_t1970, combatant_status_definition__functional_protection_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__functional_protection_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__functional_protection_reading, suppression_requirement, 2001, 0.3).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__functional_protection_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__functional_protection_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, prohibition_of_torture).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'combatant_status_definition' kernel. This 'functional_protection_reading' emphasizes universal minimum protections, contrasting with the 'state_centric_reading' (formal state military organization) and 'national_liberation_reading' (status for non-state groups fighting specific regimes). Each reading represents a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
