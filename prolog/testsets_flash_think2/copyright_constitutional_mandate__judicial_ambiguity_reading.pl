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
 *   human_readable: Copyright Term Length: Judicial Deference Reading
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint describes the judicial interpretation that copyright term
 *   length falls within Congress's legislative discretion, with courts
 *   applying rational basis review rather than strict scrutiny. This reading
 *   of the 'limited times' clause in the Constitution enables Congress to
 *   extend copyright terms, often in response to lobbying by copyright
 *   holders. It is one reading of the broader
 *   'copyright_constitutional_mandate' kernel, focusing on the role of
 *   judicial deference in shaping IP law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.6).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term Length: Judicial Deference Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '72ffadc7-a826-4443-a90a-fc5b6e443ff3').
narrative_ontology:cs_kernel_codification('72ffadc7-a826-4443-a90a-fc5b6e443ff3', fixed_text).
narrative_ontology:cs_authority_grounding('72ffadc7-a826-4443-a90a-fc5b6e443ff3', lineage).
narrative_ontology:cs_interpretation_layer_present('72ffadc7-a826-4443-a90a-fc5b6e443ff3').
narrative_ontology:cs_reading_relation('72ffadc7-a826-4443-a90a-fc5b6e443ff3', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('72ffadc7-a826-4443-a90a-fc5b6e443ff3', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('72ffadc7-a826-4443-a90a-fc5b6e443ff3', foundational, legislative_discretion_in_ip).
narrative_ontology:cs_axiom_status(legislative_discretion_in_ip, holdable).
narrative_ontology:cs_axiom_grounding('72ffadc7-a826-4443-a90a-fc5b6e443ff3', legislative_discretion_in_ip, conventional).
narrative_ontology:cs_axiom('72ffadc7-a826-4443-a90a-fc5b6e443ff3', foundational, rational_basis_review_for_economic_legislation).
narrative_ontology:cs_axiom_status(rational_basis_review_for_economic_legislation, holdable).
narrative_ontology:cs_axiom_grounding('72ffadc7-a826-4443-a90a-fc5b6e443ff3', rational_basis_review_for_economic_legislation, conventional).
narrative_ontology:cs_reference_frame('72ffadc7-a826-4443-a90a-fc5b6e443ff3', judicial_deference_tradition).
narrative_ontology:cs_drift_state('72ffadc7-a826-4443-a90a-fc5b6e443ff3', contemporary_ip_landscape, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('72ffadc7-a826-4443-a90a-fc5b6e443ff3', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, the_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from broad discretion to set copyright term lengths without strict judicial oversight, allowing it to respond to lobbying efforts and perceived economic needs. This deference enhances its legislative power in IP matters.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, generational, mobile, national).

% Administer the rational basis review standard, which involves deferring to Congress's judgment on copyright term length. While they set the standard of review, their role is largely to uphold legislative acts unless they are arbitrary or irrational, which is a low bar.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the judicial deference that enables Congress to extend copyright terms, thereby prolonging their exclusive rights and revenue streams. They actively lobby Congress for such extensions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders, beneficiary,
    organized, generational, arbitrage, global).

% Bear the cost of extended copyright terms, as works remain out of the public domain for longer. They face an uphill battle in challenging legislative extensions due to the high bar of rational basis review.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates, payer,
    organized, generational, constrained, national).

% Collectively bears the cost of reduced access to cultural and informational works that remain under copyright for extended periods. Their interests are diffuse and often unrepresented in the legislative process, making them indirect victims of the deference.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, the_public, payer,
    powerless, generational, trapped, national).

% Analyze the legal and historical implications of judicial deference to Congress on copyright, often critiquing its impact on constitutional principles and the public domain, but without direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Are the ultimate beneficiaries of a robust public domain, but are entirely absent from the current legislative and judicial processes that determine copyright term length. Their interests are implicitly represented by public domain advocates but have no direct voice.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legislative and judicial branches by establishing a clear standard for judicial review of copyright term length, allowing Congress to adapt intellectual property law to evolving economic and technological landscapes.
% TRANSFER_FUNCTION: Transfers effective authority over the interpretation of the 'limited times' clause from strict constitutional interpretation to broad legislative discretion, thereby enabling the transfer of potential value from the public domain to copyright holders through extended terms.
% ABSENT_VOICES: Future generations and the abstract concept of the public domain are structurally excluded from directly advocating for shorter terms or stricter constitutional interpretation. Their interests are largely represented by public domain advocacy groups, who face significant institutional hurdles.
% DISAPPEARANCE_RATIONALE: If judicial deference to Congress on copyright term length vanished overnight, courts would likely apply stricter scrutiny to legislative extensions, potentially invalidating past acts and forcing Congress to fundamentally reconsider its approach to IP law. This would significantly alter the balance of power between branches and reshape economic incentives in the IP ecosystem, leading to a reorganization of the mobile software economy around open payment routing.
% FOUNDING_PROBLEM: To define the appropriate balance of power between the legislative and judicial branches regarding the interpretation of the 'limited times' clause for copyright, particularly concerning the extent of congressional discretion.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, constitutional historians, and ongoing debates in legal journals corroborate the persistent tension between legislative power and judicial review in intellectual property, especially as economic interests push for longer terms. This corroboration comes from outside the direct beneficiaries of the deference.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.45) is moderate because while the deference itself isn't pure extraction, it enables legislative actions that lead to significant transfers of value (longer copyright terms). Suppression (0.6) is moderate because it actively suppresses constitutional challenges to term extensions by setting a low bar for judicial review. Theater ratio (0.2) is low, as the principle of judicial deference is a genuine, long-standing legal doctrine, not mere performance. The metrics show a slight increase over time, reflecting the gradual solidification of this deference and its consequences.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of congressional authority and copyright holders, this constraint represents a proper balance of power and a flexible approach to IP law. From the perspective of public domain advocates and constitutional scholars, it represents an erosion of constitutional limits and a mechanism for private enclosure of public goods. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority is a primary beneficiary (low d) as it gains broad discretion. Copyright holders are also beneficiaries (low d) as they directly profit from the extended terms enabled by this deference. Public domain advocates and the public are victims (high d) as they bear the cost of reduced access to works. Federal courts, while administering the review, act as agenda-setters who enable the legislative discretion, placing them closer to the beneficiary side of the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the judicial deference as a pure Rope (simple coordination) or a Snare (pure extraction). It acknowledges the genuine coordination function between branches (allowing legislative adaptation) while recognizing the asymmetric extraction that results from this deference, enabling the extension of private monopolies at the expense of the public domain. The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, suggests the constraint is still functional but its benefits are unevenly distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    limited_times_interpretation_ambiguity,
    'What is the true constitutional meaning of ''limited times'' in the Copyright Clause, and does rational basis review adequately uphold this limit?',
    'A Supreme Court ruling that reinterprets ''limited times'' with a stricter standard, or a constitutional amendment clarifying the clause''s intent.',
    'If ''limited times'' is interpreted more strictly, the constraint''s extractiveness would decrease, and its suppression of challenges would weaken, potentially reclassifying it towards a Rope or even a Scaffold (if a sunset is imposed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(limited_times_interpretation_ambiguity, conceptual, 'Ambiguity in the constitutional limit on copyright term length.').

omega_variable(
    deference_impact_on_public_domain,
    'What is the quantifiable economic and cultural impact of judicial deference on the growth and accessibility of the public domain?',
    'Comprehensive economic and cultural studies comparing jurisdictions with different levels of judicial scrutiny on copyright term extensions.',
    'If the impact is shown to be severely detrimental, it would strengthen the argument for reclassifying the constraint as more extractive (Snare-like) due to its long-term societal costs, even if the immediate legal mechanism is one of deference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_impact_on_public_domain, empirical, 'Quantifying the societal cost of extended copyright terms due to judicial deference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1976, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(copy_tr_t1985, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement(copy_tr_t1994, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1994, 0.17).
narrative_ontology:measurement(copy_tr_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2003, 0.18).
narrative_ontology:measurement(copy_tr_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2012, 0.19).
narrative_ontology:measurement(copy_tr_t2023, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(copy_be_t1985, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement(copy_be_t1994, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1994, 0.41).
narrative_ontology:measurement(copy_be_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2003, 0.43).
narrative_ontology:measurement(copy_be_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2012, 0.44).
narrative_ontology:measurement(copy_be_t2023, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement(copy_su_t1985, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1985, 0.53).
narrative_ontology:measurement(copy_su_t1994, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1994, 0.56).
narrative_ontology:measurement(copy_su_t2003, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2003, 0.58).
narrative_ontology:measurement(copy_su_t2012, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2012, 0.59).
narrative_ontology:measurement(copy_su_t2023, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2023, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'copyright_constitutional_mandate' kernel. This 'judicial_ambiguity_reading' focuses on the role of judicial deference, which enables the 'corporate_enclosure_reading' and structurally forecloses the 'public_scaffold_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
