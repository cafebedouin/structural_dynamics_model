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
 *   human_readable: Judicial Deference to Copyright Term Length (Ambiguity Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint describes the judicial interpretation that copyright term
 *   length is primarily a matter of legislative discretion, with courts
 *   applying a highly deferential rational basis review. This reading,
 *   instantiated as 'judicial_ambiguity_reading' of the
 *   'copyright_constitutional_mandate' kernel, allows Congress significant
 *   latitude in extending copyright terms, effectively enabling a gradual
 *   shift from a public-good-oriented 'scaffold' to a more extractive
 *   'corporate enclosure' without direct constitutional challenge. The other
 *   readings, 'public_scaffold_reading' and 'corporate_enclosure_reading',
 *   represent alternative interpretations of the same constitutional kernel.
 *
 * KEY AGENTS:
 *   - congressional_authority: Primary beneficiary (institutional/arbitrage) — benefits from broad discretion
 *   - copyright_holders: Beneficiary (organized/mobile) — benefits from extended terms
 *   - public_domain_advocates: Payer (organized/constrained) — bears costs of reduced public domain
 *   - future_creators: Payer (moderate/constrained) — bears costs of limited access to prior works
 *   - federal_judiciary: Agenda-setter (institutional/analytical) — administers the deferential review standard
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
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference to Copyright Term Length (Ambiguity Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '65e9d4c7-e305-49cf-a71a-de51c2467a43').
narrative_ontology:cs_kernel_codification('65e9d4c7-e305-49cf-a71a-de51c2467a43', fixed_text).
narrative_ontology:cs_authority_grounding('65e9d4c7-e305-49cf-a71a-de51c2467a43', lineage).
narrative_ontology:cs_interpretation_layer_present('65e9d4c7-e305-49cf-a71a-de51c2467a43').
narrative_ontology:cs_reading_relation('65e9d4c7-e305-49cf-a71a-de51c2467a43', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('65e9d4c7-e305-49cf-a71a-de51c2467a43', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('65e9d4c7-e305-49cf-a71a-de51c2467a43', foundational, legislative_discretion_primary).
narrative_ontology:cs_axiom_status(legislative_discretion_primary, holdable).
narrative_ontology:cs_axiom_grounding('65e9d4c7-e305-49cf-a71a-de51c2467a43', legislative_discretion_primary, conventional).
narrative_ontology:cs_axiom('65e9d4c7-e305-49cf-a71a-de51c2467a43', foundational, rational_basis_review_sufficient).
narrative_ontology:cs_axiom_status(rational_basis_review_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('65e9d4c7-e305-49cf-a71a-de51c2467a43', rational_basis_review_sufficient, conventional).
narrative_ontology:cs_reference_frame('65e9d4c7-e305-49cf-a71a-de51c2467a43', judicial_deference_framework).
narrative_ontology:cs_drift_state('65e9d4c7-e305-49cf-a71a-de51c2467a43', contemporary_legal_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('65e9d4c7-e305-49cf-a71a-de51c2467a43', '').
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

% The legislative branch, which benefits from broad discretion in setting copyright terms without significant judicial interference. This allows them to respond to lobbying efforts and extend terms.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, beneficiary,
    institutional, generational, arbitrage, national).

% Entities (e.g., corporations, estates) that own copyrights and benefit directly from extended terms, which prolong their exclusive rights and revenue streams. They actively lobby Congress for extensions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders, beneficiary,
    organized, biographical, mobile, global).

% Organizations and individuals who argue for a robust public domain and oppose copyright term extensions. They bear the cost of reduced access to creative works and diminished public commons.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates, payer,
    organized, generational, constrained, national).

% Artists, writers, and innovators who rely on access to existing works to create new ones. Extended copyright terms limit their source material and increase licensing costs, bearing the cost of enclosure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators, payer,
    moderate, generational, constrained, global).

% The courts, particularly the Supreme Court, which apply rational basis review to copyright legislation, thereby deferring to Congress's judgment on term length. They administer the constraint by upholding legislative acts.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legislative and judicial branches by establishing a clear, albeit deferential, standard for judicial review of copyright term legislation, ensuring legal predictability for copyright holders and creators.
% TRANSFER_FUNCTION: Transfers the power to define the scope of copyright from a potentially strict constitutional interpretation to broad legislative discretion, indirectly transferring economic value from the public domain to copyright holders.
% ABSENT_VOICES: Scholars advocating for a more robust 'limited times' interpretation based on original intent or economic efficiency are largely absent from the judicial decision-making process, as the current framework prioritizes legislative deference.
% DISAPPEARANCE_RATIONALE: If judicial deference vanished, courts would likely adopt a stricter standard of review, potentially invalidating past and future copyright term extensions. This would fundamentally alter the balance of power between Congress and the judiciary on IP matters, and significantly impact the economic models of copyright-dependent industries.
% FOUNDING_PROBLEM: The U.S. Constitution's 'limited times' clause for copyright and patent grants required a mechanism to reconcile legislative power with constitutional limits, ensuring a balance between creator incentives and public access.
% FOUNDING_PROBLEM_CORROBORATION: The federal judiciary and congressional authority assert the problem is live, requiring ongoing legislative flexibility. Public domain advocates and legal scholars, from outside the benefiting parties, argue that the problem of balancing incentives and access has been distorted, with the 'limited times' clause effectively rendered meaningless by legislative extensions and judicial deference.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.35) is low-to-moderate because the constraint itself is the judicial deference, not the copyright term. The deference enables extraction by Congress but does not directly perform it. Suppression (0.6) is moderate because while there are legal avenues to challenge term extensions, the high bar of rational basis review effectively suppresses most challenges. Theater ratio is low (0.1) as the judicial review process is genuinely applied, even if deferentially. The values reflect the current period of judicial interpretation, which has largely upheld legislative extensions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of congressional authority and copyright holders, this constraint is a legitimate 'rope' that provides necessary flexibility for IP policy. From the perspective of public domain advocates and future creators, it functions more like a 'tangled rope' or even a 'snare' by enabling continuous enclosure of the public domain, with the judiciary acting as a permissive gatekeeper rather than a constitutional check.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority is a beneficiary (d=0.0) as the constraint grants it broad power. Copyright holders are also beneficiaries (d=0.1) as they benefit from the legislative discretion to extend terms. Public domain advocates and future creators are payers (d=0.9) as they bear the costs of extended monopolies. The federal judiciary, as the agenda-setter, maintains the framework of deference (d=0.5), balancing its institutional role with the outcomes of its review.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, framed as a 'rope' (coordination of legislative and judicial branches), prevents mislabeling the judicial deference itself as pure extraction. However, the analysis highlights how this 'rope' enables the potential for mandatrophy in the broader copyright system, as the original public-good mandate of copyright can be eroded by legislative extensions, facilitated by judicial deference. The 'judicial_ambiguity_reading' is crucial for understanding how a seemingly benign constraint can facilitate extractive drift in a larger system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_fixity_ambiguity,
    'Is the ''limited times'' clause a substantive constraint on legislative power, or merely a procedural directive for Congress to act?',
    'A Supreme Court ruling explicitly defining the outer bounds of ''limited times'' or adopting a stricter standard of review for copyright term extensions.',
    'If a substantive constraint, the current reading''s low extractiveness would be reclassified upward as it enables legislative overreach; if procedural, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_fixity_ambiguity, conceptual, 'Ambiguity of ''limited times'' clause in US Constitution.').

omega_variable(
    reading_of_kernel,
    'This constraint is the ''judicial_ambiguity_reading'' of the ''copyright_constitutional_mandate'' kernel. How would the classification change under sibling readings?',
    'Adoption of ''public_scaffold_reading'' would lower extractiveness and suppression, shifting to a more pure Rope or Scaffold. Adoption of ''corporate_enclosure_reading'' would increase extractiveness and suppression, shifting towards a Snare.',
    'The current reading enables a ''scaffold-to-enclosure'' transition without constitutional invalidation, acting as a permissive mechanism for legislative drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_kernel, conceptual, 'Impact of alternative readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(copy_be_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(copy_su_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'copyright_constitutional_mandate' kernel. This 'judicial_ambiguity_reading' focuses on the role of judicial deference, which enables the 'corporate_enclosure_reading' and contrasts with the 'public_scaffold_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
