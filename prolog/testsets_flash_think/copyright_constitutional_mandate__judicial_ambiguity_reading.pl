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
 *   human_readable: Judicial Deference to Copyright Term Length Legislation
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint describes the judicial reading of the Copyright Clause
 *   (Article I, Section 8, Clause 8) of the U.S. Constitution, specifically
 *   the interpretation that grants Congress broad legislative discretion over
 *   copyright term length, with courts deferring to congressional judgment
 *   via rational basis review. This reading enables the legislative branch to
 *   extend copyright terms significantly, often in response to lobbying,
 *   without facing strict constitutional invalidation. It is a reading of the
 *   'copyright_constitutional_mandate' kernel.
 *
 * KEY AGENTS:
 *   - congressional_authority: Primary beneficiary (institutional/arbitrage) — benefits from discretion.
 *   - federal_courts: Agenda setter (institutional/constrained) — enforces deference.
 *   - copyright_holders: Beneficiary (organized/mobile) — benefits from extended terms.
 *   - public_domain_advocates: Payer (organized/constrained) — bears costs of enclosure.
 *   - future_creators: Payer (moderate/constrained) — bears costs of reduced public domain.
 *   - constitutional_scholars: Observer (analytical/analytical) — analyzes legal implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.6).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference to Copyright Term Length Legislation").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, 'af66ba1f-d06c-469b-b625-8def878fef12').
narrative_ontology:cs_kernel_codification('af66ba1f-d06c-469b-b625-8def878fef12', fixed_text).
narrative_ontology:cs_authority_grounding('af66ba1f-d06c-469b-b625-8def878fef12', lineage).
narrative_ontology:cs_interpretation_layer_present('af66ba1f-d06c-469b-b625-8def878fef12').
narrative_ontology:cs_reading_relation('af66ba1f-d06c-469b-b625-8def878fef12', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('af66ba1f-d06c-469b-b625-8def878fef12', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_axiom('af66ba1f-d06c-469b-b625-8def878fef12', foundational, legislative_discretion_in_copyright_terms).
narrative_ontology:cs_axiom_status(legislative_discretion_in_copyright_terms, holdable).
narrative_ontology:cs_axiom_grounding('af66ba1f-d06c-469b-b625-8def878fef12', legislative_discretion_in_copyright_terms, conventional).
narrative_ontology:cs_axiom('af66ba1f-d06c-469b-b625-8def878fef12', foundational, rational_basis_review_is_appropriate_standard).
narrative_ontology:cs_axiom_status(rational_basis_review_is_appropriate_standard, holdable).
narrative_ontology:cs_axiom_grounding('af66ba1f-d06c-469b-b625-8def878fef12', rational_basis_review_is_appropriate_standard, conventional).
narrative_ontology:cs_reference_frame('af66ba1f-d06c-469b-b625-8def878fef12', rational_basis_review_doctrine).
narrative_ontology:cs_drift_state('af66ba1f-d06c-469b-b625-8def878fef12', contemporary_copyright_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af66ba1f-d06c-469b-b625-8def878fef12', '').
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

% Benefits from broad discretion to set copyright terms without strict judicial oversight, allowing it to respond to lobbying efforts and policy shifts. Its power is enhanced by judicial deference.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Apply rational basis review to copyright term legislation, deferring to Congress's judgment. This maintains judicial restraint and separation of powers, but enables legislative drift towards longer terms.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the extended and stable copyright terms enabled by judicial deference, allowing them to monetize their intellectual property for longer periods. They actively lobby Congress for extensions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders, beneficiary,
    organized, generational, mobile, global).

% Bear the cost of reduced public domain access and delayed entry of works into the public commons. They challenge term extensions in courts and lobby Congress, but face an uphill battle due to judicial deference.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates, payer,
    organized, generational, constrained, national).

% Are indirectly harmed by the shrinking public domain, as their ability to build upon existing works without licensing fees is diminished. They are a diffuse group with limited direct influence.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators, payer,
    moderate, generational, constrained, global).

% Analyze the constitutional implications of judicial deference to copyright term length, often critiquing the rational basis standard as insufficient to uphold the 'limited times' clause.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal framework for legislative action on copyright, ensuring that courts do not frequently overturn congressional decisions on term length, thereby maintaining separation of powers and legal certainty for creators.
% TRANSFER_FUNCTION: Transfers the effective power to define the 'limited times' of copyright from strict constitutional interpretation to legislative policy, enabling the extension of private monopolies over intellectual property at the expense of the public domain.
% ABSENT_VOICES: The original intent of the 'limited times' clause framers, which arguably envisioned a more constrained legislative power, is absent from direct advocacy. The general public, as the ultimate beneficiary of the public domain, is a diffuse voice whose interests are often underrepresented.
% DISAPPEARANCE_RATIONALE: If courts suddenly abandoned rational basis review and strictly scrutinized copyright term extensions, Congress would be forced to justify terms more rigorously, potentially leading to shorter terms and a rebalancing of public/private interests. The entire intellectual property landscape would reorganize.
% FOUNDING_PROBLEM: To balance the incentive for authors and inventors to create with the public's right to access and build upon knowledge, granting Congress the power to secure exclusive rights for 'limited times'.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and Congress argue that extended terms are necessary to incentivize creation in a global digital economy. Public domain advocates and many constitutional scholars argue that the original problem is largely solved, and current terms exceed any reasonable incentive, serving primarily as rent collection. Legislative hearing testimony and independent economic analyses from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Base extractiveness (0.7 at end) is high because judicial deference allows Congress to enact term extensions that disproportionately benefit copyright holders, effectively extracting value from the public domain. Suppression (0.8 at end) is high because the rational basis standard makes it extremely difficult to successfully challenge term extensions in court, effectively suppressing constitutional challenges. Theater ratio remains low (0.2 at end) because judicial review, even under a deferential standard, is a genuine function, not mere performance. The increasing extractiveness and suppression over the interval reflect the historical trend of copyright term extensions (e.g., 1976 and 1998 acts) and the judiciary's consistent upholding of these laws.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal courts, this constraint upholds the separation of powers and judicial restraint, ensuring legislative flexibility. From the perspective of public domain advocates and future creators, the same constraint operates as an enabling mechanism for private enclosure, undermining the constitutional balance. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority and copyright holders are beneficiaries, as the constraint grants Congress discretion which copyright holders then leverage for longer terms. Federal courts are agenda setters, as they define and apply the standard of review. Public domain advocates and future creators are victims, as they bear the costs of reduced access to cultural works. Constitutional scholars are observers, analyzing the system without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This judicial reading prevents the detection of mandatrophy in copyright legislation by consistently validating term extensions. The original mandate of copyright as a 'scaffold' to incentivize creation for a 'limited time' has arguably drifted, but judicial deference allows the 'enclosure' function to persist and expand without being deemed unconstitutional. The constraint itself is not mandatrophic, but it enables mandatrophy in the underlying copyright laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is ''limited times'' in the Copyright Clause a substantive constitutional limit on legislative power, or merely a formal requirement that Congress must specify a duration?',
    'A Supreme Court ruling that explicitly defines ''limited times'' as a substantive constraint, or a constitutional amendment clarifying the scope of congressional power.',
    'If ''limited times'' is a substantive limit, judicial deference would be reclassified as an abdication of duty, potentially leading to shorter copyright terms and a rebalancing of public/private interests. If it''s merely formal, the current deference is constitutionally sound.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity regarding the substantive meaning of ''limited times'' in the Copyright Clause.').

omega_variable(
    judicial_role_ambiguity,
    'Is judicial deference via rational basis review a neutral application of established constitutional doctrine, or an abdication of the judiciary''s role in upholding constitutional limits against legislative overreach?',
    'A shift in judicial philosophy or a landmark case that re-evaluates the application of rational basis review to intellectual property, potentially adopting a higher standard of scrutiny.',
    'If deemed an abdication, the constraint''s suppression and extractiveness would be seen as enabled by judicial inaction, leading to calls for more active judicial oversight. If neutral, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_role_ambiguity, conceptual, 'Ambiguity regarding the appropriate level of judicial scrutiny for copyright legislation.').

omega_variable(
    mandatrophy_of_public_good_mandate,
    'Has the original public good mandate of copyright (to promote progress by incentivizing creation) been superseded by private property interests (maximal protection for existing works)?',
    'Empirical studies demonstrating that current copyright terms no longer serve their original incentive function, or a legislative re-evaluation that explicitly prioritizes public domain enrichment.',
    'If the public good mandate is found to be superseded, the constraint''s role in enabling term extensions would be seen as facilitating a ''snare'' rather than a ''scaffold'', increasing its effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_public_good_mandate, empirical, 'Whether copyright''s original public good mandate has atrophied in favor of private interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(copy_tr_t1980, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1980, 0.16).
narrative_ontology:measurement(copy_tr_t1990, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(copy_tr_t2000, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(copy_tr_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(copy_tr_t2020, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(copy_be_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(copy_be_t1980, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(copy_be_t1990, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(copy_be_t2000, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(copy_be_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(copy_be_t2020, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1970, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(copy_su_t1980, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(copy_su_t1990, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(copy_su_t2000, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(copy_su_t2010, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(copy_su_t2020, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_term_extension_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'copyright_constitutional_mandate' kernel. This 'judicial_ambiguity_reading' focuses on the courts' deferential stance, which influences the legislative outcomes and the balance between public and private interests. It differs from the 'public_scaffold_reading' (which emphasizes the public good) and the 'corporate_enclosure_reading' (which emphasizes maximal private property rights) by focusing on the judicial mechanism that enables the drift between these two poles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
