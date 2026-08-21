% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Deference to Congressional Copyright Term Length
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'judicial ambiguity' reading of the
 *   constitutional mandate for copyright. It describes the judicial practice
 *   of deferring to Congress on copyright term length, primarily through
 *   rational basis review. This deference allows Congress significant
 *   latitude to extend terms, effectively shifting the 'limited times' clause
 *   from a strict constitutional constraint to a zone of legislative
 *   discretion. This reading enables the gradual enclosure of the public
 *   domain without direct constitutional invalidation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.35).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.6).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference to Congressional Copyright Term Length").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, 'd52a5475-90bd-40fc-90e3-92d8bffb2e94').
narrative_ontology:cs_kernel_codification('d52a5475-90bd-40fc-90e3-92d8bffb2e94', fixed_text).
narrative_ontology:cs_authority_grounding('d52a5475-90bd-40fc-90e3-92d8bffb2e94', lineage).
narrative_ontology:cs_interpretation_layer_present('d52a5475-90bd-40fc-90e3-92d8bffb2e94').
narrative_ontology:cs_reading_relation('d52a5475-90bd-40fc-90e3-92d8bffb2e94', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('d52a5475-90bd-40fc-90e3-92d8bffb2e94', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_axiom('d52a5475-90bd-40fc-90e3-92d8bffb2e94', foundational, legislative_discretion_in_limited_times).
narrative_ontology:cs_axiom_status(legislative_discretion_in_limited_times, holdable).
narrative_ontology:cs_axiom_grounding('d52a5475-90bd-40fc-90e3-92d8bffb2e94', legislative_discretion_in_limited_times, conventional).
narrative_ontology:cs_axiom('d52a5475-90bd-40fc-90e3-92d8bffb2e94', secondary, rational_basis_review_for_economic_legislation).
narrative_ontology:cs_axiom_status(rational_basis_review_for_economic_legislation, holdable).
narrative_ontology:cs_axiom_grounding('d52a5475-90bd-40fc-90e3-92d8bffb2e94', rational_basis_review_for_economic_legislation, conventional).
narrative_ontology:cs_reference_frame('d52a5475-90bd-40fc-90e3-92d8bffb2e94', judicial_deference_to_congress).
narrative_ontology:cs_drift_state('d52a5475-90bd-40fc-90e3-92d8bffb2e94', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d52a5475-90bd-40fc-90e3-92d8bffb2e94', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress holds the power to set copyright terms, with courts largely deferring to its judgment. This reading grants Congress broad discretion, allowing it to extend terms without significant judicial challenge, effectively expanding its legislative power in this domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Courts, particularly the Supreme Court, apply rational basis review to copyright term extensions, upholding them as long as there is a conceivable rational basis, even if not explicitly articulated by Congress. This defers to legislative power, limiting judicial intervention.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from extended copyright terms, which prolong their exclusive rights and revenue streams. They actively lobby Congress for extensions, relying on judicial deference to ensure these extensions are not overturned.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of delayed entry of works into the public domain, limiting access to cultural heritage and raw material for new creation. They challenge term extensions but face an uphill battle due to judicial deference.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates, payer,
    moderate, generational, constrained, national).

% Are implicitly harmed by the shrinking public domain, as they have fewer works to build upon without licensing fees or permission. Their interests are diffuse and not directly represented in legislative or judicial processes.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators, payer,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit flexible, boundary for copyright protection, providing certainty for creators and users within the framework of legislative action. It coordinates the roles of Congress and the Judiciary in defining intellectual property rights.
% TRANSFER_FUNCTION: Transfers the power to define the 'limited times' of copyright from a potentially strict constitutional interpretation to broad legislative discretion, effectively extending the duration of exclusive rights from the public domain to copyright holders.
% ABSENT_VOICES: The framers' original intent regarding 'limited times' as a strict constraint on monopoly duration is largely absent from contemporary judicial review, replaced by a deference to legislative judgment. Future generations, who would benefit from a richer public domain, also lack direct representation.
% DISAPPEARANCE_RATIONALE: If judicial deference to Congress on copyright term length vanished, courts would likely scrutinize extensions more rigorously, potentially invalidating past extensions and forcing Congress to justify future ones with a higher standard. This would significantly alter the landscape of intellectual property law and the public domain.
% FOUNDING_PROBLEM: The U.S. Constitution grants Congress the power to promote the progress of science and useful arts by securing for 'limited times' to authors and inventors the exclusive right to their respective writings and discoveries. The founding problem was to balance incentives for creation with public access.
% FOUNDING_PROBLEM_CORROBORATION: Congressional proponents and copyright holders argue the problem is live, requiring flexible terms to incentivize creation in new media. Public domain advocates and some legal scholars argue the original problem of incentivizing creation is largely solved, and the current system over-incentivizes, leading to enclosure. Historical analysis of copyright legislation and economic studies of innovation provide corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the indirect nature of extraction through legislative extensions rather than direct judicial action. Suppression (0.6) is higher because judicial deference actively suppresses challenges to legislative power in this area, making it difficult for public domain advocates to succeed. Theater ratio (0.1) is low, as the judicial review process, while deferential, is still a genuine legal function, not mere performance. The metrics reflect a system where the constitutional 'limited times' clause is interpreted flexibly, allowing for legislative expansion of copyright terms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of congressional authority and copyright holders, this constraint is a legitimate exercise of legislative power and a necessary incentive for creation. From the perspective of public domain advocates and future creators, it represents a gradual erosion of the public domain and a constitutional mandate that has been reinterpreted to favor private interests. The judiciary's role is seen as either upholding the constitutional balance (by its own lights) or enabling legislative overreach (by critics).
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority and copyright holders are clear beneficiaries, as judicial deference empowers Congress to grant longer monopolies, which directly benefits copyright holders. Public domain advocates and future creators are victims, as their access to cultural works is restricted for longer periods. The federal judiciary, while an agenda-setter, also acts as a beneficiary of its own institutional power, as deference simplifies its role and avoids politically charged constitutional confrontations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_deference,
    'To what extent does judicial deference align with the original intent of the ''limited times'' clause in the U.S. Constitution?',
    'Historical and legal scholarship analyzing the framers'' debates, early copyright statutes, and contemporary understandings of ''limited times'' in the 18th century.',
    'If original intent strongly suggests a stricter limitation, the current deference would be reclassified as a significant departure, potentially strengthening arguments for judicial intervention. If original intent is genuinely ambiguous, the deference is more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_deference, conceptual, 'Ambiguity regarding the constitutional framers'' intent on copyright term limits.').

omega_variable(
    rational_basis_review_rigor,
    'Is the rational basis review applied to copyright term extensions genuinely rigorous, or is it effectively a rubber stamp for legislative action?',
    'Comparative legal analysis of rational basis review in other constitutional contexts (e.g., economic regulation) to assess the level of scrutiny applied specifically to copyright cases. Examination of dissenting judicial opinions.',
    'If the review is found to be a mere formality, the ''suppression'' metric would increase, as judicial oversight is more theatrical than substantive. If it demonstrates genuine, albeit low-bar, scrutiny, the current metrics hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_basis_review_rigor, empirical, 'The actual rigor of rational basis review in copyright cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1976, 0.05).
narrative_ontology:measurement(copy_tr_t1990, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.25).
narrative_ontology:measurement(copy_be_t1990, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1998, 0.35).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(copy_su_t1990, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1998, 0.6).
narrative_ontology:measurement(copy_su_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_access_rules).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, intellectual_property_lobbying_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'copyright_constitutional_mandate' kernel. This 'judicial_ambiguity_reading' focuses on the judiciary's role in interpreting the 'limited times' clause, influencing both the 'public_scaffold_reading' (by limiting its scope) and the 'corporate_enclosure_reading' (by enabling its legislative goals).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
