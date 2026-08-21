% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Pragmatic Development Methodology
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic development' reading of
 *   software source status, where open source is valued primarily for its
 *   instrumental benefits to software quality, security, and innovation. It
 *   asserts that open development methodologies lead to superior outcomes,
 *   making 'freedom' a means to an end rather than an end in itself.
 *   Proprietary software is not inherently illegitimate, but its development
 *   model is often seen as less efficient or robust. This reading coexists
 *   with, rather than forecloses, other perspectives on software licensing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.15).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.1).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Pragmatic Development Methodology").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, '6e2f53a3-790e-4f1e-9c85-48c142ccb24a').
narrative_ontology:cs_kernel_codification('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', distributed).
narrative_ontology:cs_authority_grounding('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', expertise).
narrative_ontology:cs_interpretation_layer_present('6e2f53a3-790e-4f1e-9c85-48c142ccb24a').
narrative_ontology:cs_reading_relation('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', foundational, open_collaboration_improves_quality).
narrative_ontology:cs_axiom_status(open_collaboration_improves_quality, holdable).
narrative_ontology:cs_axiom_grounding('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', open_collaboration_improves_quality, empirically_contingent).
narrative_ontology:cs_axiom('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', foundational, permissive_licensing_enables_innovation).
narrative_ontology:cs_axiom_status(permissive_licensing_enables_innovation, holdable).
narrative_ontology:cs_axiom_grounding('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', permissive_licensing_enables_innovation, empirically_contingent).
narrative_ontology:cs_reference_frame('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', meritocratic_development_outcomes).
narrative_ontology:cs_drift_state('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6e2f53a3-790e-4f1e-9c85-48c142ccb24a', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, technology_companies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, proprietary_software_vendors).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, peer_review_enhances_quality).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, open_collaboration_drives_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from higher quality, more secure, and often free software. Can choose between open and proprietary options based on features and cost, without ideological commitment.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_users, beneficiary,
    organized, biographical, mobile, global).

% Benefits from collaborative development, peer review, and access to a vast ecosystem of tools and libraries. Can contribute to open source projects or work on proprietary ones, valuing the practical advantages of open methods.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_developers, beneficiary,
    moderate, biographical, mobile, global).

% Leverages open source for cost savings, faster development cycles, and access to community-driven innovation. Often contributes back to open source projects while maintaining proprietary products, seeing open source as a strategic tool.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Faces competition from high-quality open-source alternatives, which can depress prices or force innovation. Must adapt business models to compete with the pragmatic advantages of open development.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Would argue that the emphasis on pragmatic benefits of open source undervalues the fundamental right to control intellectual property. Their concerns are acknowledged but not central to this reading's focus on development efficacy.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, intellectual_property_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates large-scale, distributed software development by enabling transparent collaboration, peer review, and shared innovation, leading to more robust and adaptable software.
% TRANSFER_FUNCTION: Facilitates the transfer of knowledge, code, and development effort across a broad community, resulting in higher quality software that is often freely available, shifting value from proprietary licenses to shared utility.
% ABSENT_VOICES: Hardline intellectual property advocates and those who view software freedom as an absolute moral imperative are less central to this pragmatic discussion, which prioritizes development outcomes over rights or ethics.
% DISAPPEARANCE_RATIONALE: If the pragmatic benefits of open source were no longer recognized, development methodologies would revert to more closed, proprietary models, slowing innovation, increasing costs, and reducing software quality and security across the industry.
% FOUNDING_PROBLEM: Proprietary software development often suffered from 'not invented here' syndrome, limited peer review, and vendor lock-in, leading to slower innovation and less robust products.
% FOUNDING_PROBLEM_CORROBORATION: Software engineering research, industry reports, and numerous case studies from diverse technology companies corroborate the ongoing benefits of open source practices for quality, security, and innovation velocity, independent of ideological positions.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).
:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because this reading emphasizes shared benefits and efficiency gains, not extraction. Suppression is low as the 'superiority' is demonstrated through results and adoption, not coercion. Theater ratio is minimal as the claims are largely functional and empirically verifiable. The decreasing extractiveness and suppression over time reflect the growing empirical evidence and industry adoption of open source practices, making the 'pragmatic' case stronger and less contested.
 *
 * PERSPECTIVAL GAP:
 *   While this reading focuses on practical benefits, other readings (e.g., 'freedom imperative') view open source as a moral or ethical necessity. The gap lies in the foundational justification: utility vs. rights. This constraint's classification as a Rope reflects its coordination function and broad benefits within its own pragmatic frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Software users, developers, and technology companies are beneficiaries, gaining from the quality and efficiency of open source. Proprietary software vendors are payers, facing competitive pressure to adapt. Intellectual property advocates are excluded, as their foundational claims are not central to this pragmatic, outcome-focused reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_strength,
    'How robust is the empirical evidence for open source''s superiority in quality and security across all software domains?',
    'Ongoing meta-analysis of software engineering studies, security audits, and long-term maintenance data comparing open and proprietary projects across diverse contexts.',
    'Stronger evidence reinforces this reading''s ''Rope'' classification by solidifying its coordination function. Weaker or mixed evidence might shift it towards ''Tangled Rope'' if the ''superiority'' claim becomes a rhetorical cover for other interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_evidence_strength, empirical, 'The degree to which open source demonstrably outperforms proprietary models in key metrics.').

omega_variable(
    boundary_with_utilitarian_hybrid,
    'What is the precise boundary between ''pragmatic development'' (instrumental quality) and ''utilitarian hybrid'' (aggregate welfare maximization) readings?',
    'Conceptual analysis of specific licensing decisions and their stated justifications: does the justification prioritize development outcomes (quality, security) or broader societal welfare (economic impact, access)?',
    'A clearer boundary would help disambiguate cases where a ''pragmatic'' claim is actually a subset of a broader ''utilitarian'' calculation, potentially leading to reclassification or refinement of the ''utilitarian_hybrid_reading'' constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_with_utilitarian_hybrid, conceptual, 'Distinguishing instrumental quality from broader welfare maximization.').

omega_variable(
    suppression_of_alternative_methods,
    'Does the widespread adoption and perceived ''superiority'' of open source pragmatically suppress the development or funding of alternative, potentially innovative, proprietary methods?',
    'Market analysis of venture capital funding trends, developer career paths, and innovation rates in proprietary vs. open source sectors, looking for evidence of ''crowding out'' effects.',
    'If significant suppression is found, the ''suppression'' metric for this reading would need to be adjusted upward, potentially shifting the classification towards ''Tangled Rope'' by revealing an unacknowledged cost of the ''superior'' methodology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternative_methods, empirical, 'Whether the success of open source inadvertently limits other development approaches.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1990, software_source_status__pragmatic_development_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__pragmatic_development_reading, theater_ratio, 2000, 0.06).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__pragmatic_development_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__pragmatic_development_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(soft_be_t1990, software_source_status__pragmatic_development_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(soft_be_t2000, software_source_status__pragmatic_development_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(soft_be_t2010, software_source_status__pragmatic_development_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(soft_be_t2024, software_source_status__pragmatic_development_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1990, software_source_status__pragmatic_development_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(soft_su_t2000, software_source_status__pragmatic_development_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(soft_su_t2010, software_source_status__pragmatic_development_reading, suppression_requirement, 2010, 0.11).
narrative_ontology:measurement(soft_su_t2024, software_source_status__pragmatic_development_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, information_standard).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_source_status' kernel, focusing on the pragmatic benefits of open source for development quality. It is linked to its sibling readings, which offer alternative justifications or critiques of open/proprietary software.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
