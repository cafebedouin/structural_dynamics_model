% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Copyleft Scope (Narrow Interpretation)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents a 'narrow' interpretation of GPL Section 2(b),
 *   asserting that copyleft obligations only apply to direct derivative
 *   works, excluding mere aggregation, plugin architectures, or certain
 *   dynamic linking forms. This reading aligns with traditional copyright
 *   doctrine and is widely adopted in commercial contexts to allow
 *   integration of GPL components with proprietary software. It is one
 *   reading of the 'gpl_copyleft_scope' kernel, alongside
 *   'strong_copyleft_reading' and 'enforcement_vacuum_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.25).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Copyleft Scope (Narrow Interpretation)").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, 'a3aea734-3272-4964-ad36-5652823f1210').
narrative_ontology:cs_kernel_codification('a3aea734-3272-4964-ad36-5652823f1210', fixed_text).
narrative_ontology:cs_authority_grounding('a3aea734-3272-4964-ad36-5652823f1210', practice).
narrative_ontology:cs_interpretation_layer_present('a3aea734-3272-4964-ad36-5652823f1210').
narrative_ontology:cs_reading_relation('a3aea734-3272-4964-ad36-5652823f1210', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('a3aea734-3272-4964-ad36-5652823f1210', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('a3aea734-3272-4964-ad36-5652823f1210', foundational, derivative_work_follows_traditional_copyright).
narrative_ontology:cs_axiom_status(derivative_work_follows_traditional_copyright, holdable).
narrative_ontology:cs_axiom_grounding('a3aea734-3272-4964-ad36-5652823f1210', derivative_work_follows_traditional_copyright, conventional).
narrative_ontology:cs_axiom('a3aea734-3272-4964-ad36-5652823f1210', secondary, mere_aggregation_is_not_derivative).
narrative_ontology:cs_axiom_status(mere_aggregation_is_not_derivative, holdable).
narrative_ontology:cs_axiom_grounding('a3aea734-3272-4964-ad36-5652823f1210', mere_aggregation_is_not_derivative, conventional).
narrative_ontology:cs_reference_frame('a3aea734-3272-4964-ad36-5652823f1210', traditional_copyright_framework).
narrative_ontology:cs_drift_state('a3aea734-3272-4964-ad36-5652823f1210', contemporary_software_development_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a3aea734-3272-4964-ad36-5652823f1210', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, open_source_developers_using_gpl_components).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the clarity and flexibility this interpretation provides, allowing them to integrate GPL-licensed components into proprietary software without being forced to open-source their entire product. They can choose to use GPL components in ways that avoid triggering copyleft obligations on their proprietary code.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Benefit from the legal certainty that their use of GPL components, particularly in aggregation or dynamic linking, does not automatically extend copyleft to their non-GPL code. This encourages broader adoption of GPL libraries in mixed-license projects.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, open_source_developers_using_gpl_components, beneficiary,
    moderate, biographical, constrained, global).

% Administer GPL-licensed projects and interpret the license's scope. While some may prefer a broader interpretation, this reading provides a workable framework for community contributions and industry engagement, even if it doesn't fully align with maximalist copyleft goals.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_project_maintainers, agenda_setter,
    organized, generational, constrained, global).

% Bear the cost of their expectations for universal code-sharing being structurally weakened. This interpretation limits the 'viral' effect of copyleft, meaning less proprietary code is compelled into the open-source ecosystem than they might desire. Their efforts are focused on promoting stronger interpretations or alternative licenses.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates, payer,
    organized, generational, constrained, global).

% Analyze and advise on the legal implications of GPL licensing. This reading provides a practical, if not universally accepted, framework for legal compliance and risk management in software development.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, legal_scholars_and_practitioners, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies the legal boundary of 'derivative work' under GPL Section 2(b), enabling developers and firms to integrate GPL-licensed components with proprietary codebases with reduced legal uncertainty, fostering interoperability and mixed-license software ecosystems.
% TRANSFER_FUNCTION: Transfers legal flexibility and reduced compliance burden to commercial software firms and developers, allowing them to retain proprietary layers. It implicitly transfers a reduced scope of copyleft enforcement to copyleft advocates, limiting the 'viral' effect of the license.
% ABSENT_VOICES: While copyleft advocates are present as 'payers' in this story, a more radical 'free software maximalist' voice, which might reject any proprietary integration as inherently harmful, is largely excluded from mainstream legal and commercial discourse around GPL interpretation.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished, the legal ambiguity around derivative works would become severe. Commercial firms would likely cease using GPL components in mixed-license projects due to unacceptable legal risk, fragmenting the software ecosystem and hindering collaboration between open-source and proprietary domains.
% FOUNDING_PROBLEM: The original GPL aimed to ensure software freedom, but the legal concept of 'derivative work' in copyright law was ambiguous, especially concerning software linking and aggregation, leading to uncertainty for developers wishing to combine GPL and non-GPL code.
% FOUNDING_PROBLEM_CORROBORATION: Legal commentary, industry licensing practices, and ongoing debates among open-source communities and legal experts attest that the ambiguity around derivative works in software licensing remains a live issue, even with this interpretation providing a common working understanding. No single, universally accepted judicial precedent has fully resolved it.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.38, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it primarily serves a coordination function by providing legal clarity and reducing transaction costs for developers combining GPL and proprietary code. Its extractiveness (0.38) is moderate, reflecting the 'cost' to copyleft advocates of not achieving maximal code sharing, but it's not a direct extraction from victims. Suppression (0.25) is low, as it's an interpretation that many parties willingly adopt for its practical benefits, though it requires active enforcement (e.g., through legal challenges) to maintain its boundaries against broader interpretations. Theater ratio is low (0.10) as the interpretation is genuinely functional.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of commercial firms, this is a highly beneficial coordination mechanism. From the perspective of strong copyleft advocates, it represents a 'watering down' of the GPL's intent, allowing proprietary interests to 'free-ride' on open-source contributions. The engine's classification as a Rope reflects its functional coordination role, while acknowledging the 'cost' to those with maximalist copyleft goals.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial software firms and open-source developers using GPL components are beneficiaries, as this reading grants them flexibility and reduces legal risk (low directionality). GPL project maintainers also benefit from broader adoption of their code, even if some prefer a stronger copyleft (moderate directionality). Copyleft advocates are 'payers' in the sense that their ideal of universal code freedom is not fully realized by this interpretation (higher directionality, but not 'victim' level as no direct extraction occurs).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_kernel_reading_identity,
    'Is this constraint a genuine, stable interpretation of GPL Section 2(b), or is its ''narrow scope'' merely a temporary, commercially convenient reading within a broader, unresolved legal ambiguity?',
    'Definitive judicial precedent from a high court ruling on dynamic linking or aggregation under GPL, or a formal amendment/clarification to the GPL itself.',
    'If a definitive ruling upholds a broader scope, this ''narrow_scope_reading'' would be reclassified as an ''overridden'' axiom within the kernel, and its classification would shift towards a Snare for commercial firms. If upheld, its Rope classification would be further solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_kernel_reading_identity, empirical, 'Ambiguity regarding the long-term legal stability of the narrow GPL interpretation.').

omega_variable(
    derivative_work_definition_ambiguity,
    'Does ''traditional copyright doctrine'' adequately define ''derivative work'' for complex software interactions like dynamic linking and plugin architectures, or is the concept inherently underspecified in this domain?',
    'Development of new legal frameworks or industry standards specifically addressing software coupling, or a consensus among legal scholars on a software-specific definition.',
    'If traditional doctrine is found inadequate, the ''conventional'' grounding of the axioms would weaken, potentially increasing the constraint''s extractiveness as parties exploit definitional gaps. If found robust, the Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'The conceptual fit of traditional copyright law to modern software.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t2000, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(gpl__tr_t2004, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2004, 0.1).
narrative_ontology:measurement(gpl__tr_t2008, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(gpl__tr_t2012, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(gpl__tr_t2016, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(gpl__tr_t2020, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t2000, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(gpl__be_t2004, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2004, 0.36).
narrative_ontology:measurement(gpl__be_t2008, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2008, 0.37).
narrative_ontology:measurement(gpl__be_t2012, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(gpl__be_t2016, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement(gpl__be_t2020, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2020, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t2000, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2000, 0.22).
narrative_ontology:measurement(gpl__su_t2004, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2004, 0.23).
narrative_ontology:measurement(gpl__su_t2008, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2008, 0.24).
narrative_ontology:measurement(gpl__su_t2012, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2012, 0.25).
narrative_ontology:measurement(gpl__su_t2016, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2016, 0.25).
narrative_ontology:measurement(gpl__su_t2020, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2020, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_licensing_terms).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel. This 'narrow_scope_reading' provides a specific interpretation of GPL Section 2(b) that allows for greater flexibility in combining GPL and proprietary code, contrasting with the 'strong_copyleft_reading' and operating within the 'enforcement_vacuum_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
