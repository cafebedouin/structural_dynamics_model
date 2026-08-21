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
 *   human_readable: Open Source as Superior Development Methodology (Pragmatic Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic development' reading of the
 *   software_source_status kernel. It asserts that open source is a superior
 *   development methodology primarily due to its instrumental benefits: peer
 *   review, bug detection, innovation velocity, and overall quality. Unlike
 *   the 'freedom imperative' reading, it does not view proprietary software
 *   as inherently illegitimate, and it accepts permissive licensing. The
 *   constraint's low extractiveness and suppression reflect its nature as a
 *   widely adopted, beneficial practice rather than a coercive structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.15).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.05).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Development Methodology (Pragmatic Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, '7806ad7e-61e4-4b35-9aed-c1046622bbd4').
narrative_ontology:cs_kernel_codification('7806ad7e-61e4-4b35-9aed-c1046622bbd4', distributed).
narrative_ontology:cs_authority_grounding('7806ad7e-61e4-4b35-9aed-c1046622bbd4', expertise).
narrative_ontology:cs_interpretation_layer_present('7806ad7e-61e4-4b35-9aed-c1046622bbd4').
narrative_ontology:cs_reading_relation('7806ad7e-61e4-4b35-9aed-c1046622bbd4', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('7806ad7e-61e4-4b35-9aed-c1046622bbd4', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('7806ad7e-61e4-4b35-9aed-c1046622bbd4', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('7806ad7e-61e4-4b35-9aed-c1046622bbd4', foundational, openness_improves_quality).
narrative_ontology:cs_axiom_status(openness_improves_quality, holdable).
narrative_ontology:cs_axiom_grounding('7806ad7e-61e4-4b35-9aed-c1046622bbd4', openness_improves_quality, empirically_contingent).
narrative_ontology:cs_axiom('7806ad7e-61e4-4b35-9aed-c1046622bbd4', secondary, permissive_licensing_is_acceptable).
narrative_ontology:cs_axiom_status(permissive_licensing_is_acceptable, holdable).
narrative_ontology:cs_axiom_grounding('7806ad7e-61e4-4b35-9aed-c1046622bbd4', permissive_licensing_is_acceptable, conventional).
narrative_ontology:cs_reference_frame('7806ad7e-61e4-4b35-9aed-c1046622bbd4', meritocratic_development_paradigm).
narrative_ontology:cs_drift_state('7806ad7e-61e4-4b35-9aed-c1046622bbd4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7806ad7e-61e4-4b35-9aed-c1046622bbd4', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_developers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, tech_companies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, proprietary_software_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from collaborative development, peer review, and access to a vast codebase. They can choose to work on open or proprietary projects, but often find open source more efficient and rewarding for certain types of problems.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from higher quality, more secure, and often free software. They value the transparency and community support, but their choices are ultimately dictated by available software options.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_users, beneficiary,
    moderate, biographical, constrained, global).

% Leverage open source for faster innovation, reduced development costs, and access to talent. They often contribute to open source while maintaining proprietary products, seeing open development as a strategic advantage rather than an ideological imperative.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, tech_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Face competition from high-quality open-source alternatives, which can depress prices or force them to adopt open-source practices. They are not seen as illegitimate, but must adapt to the market's preference for open development where it offers clear advantages.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Would argue that the emphasis on open development undervalues proprietary rights and could disincentivize innovation driven by exclusive ownership. Their concerns are acknowledged but not central to this reading's pragmatic focus on development quality.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, intellectual_property_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates software development efforts by promoting transparency, peer review, and modularity, leading to more robust and higher-quality software through collective intelligence.
% TRANSFER_FUNCTION: Facilitates the transfer of knowledge, code, and development effort across a broad community, leading to shared improvements and reduced redundant work. It also transfers market pressure to proprietary vendors to improve quality or lower prices.
% ABSENT_VOICES: Strict intellectual property advocates, who prioritize exclusive ownership above all else, are largely absent from the core discourse of this pragmatic reading, which focuses on development efficacy.
% DISAPPEARANCE_RATIONALE: If the belief in open source as a superior methodology vanished, software development would likely revert to more siloed, proprietary models, potentially slowing innovation, increasing bug rates, and raising costs for users. The collaborative ecosystem would fragment.
% FOUNDING_PROBLEM: Proprietary software development often suffered from 'not invented here' syndrome, slow bug fixes, security vulnerabilities due to closed review, and vendor lock-in, hindering overall technological progress.
% FOUNDING_PROBLEM_CORROBORATION: Software engineering research, numerous case studies of successful open-source projects (e.g., Linux, Apache), and the widespread adoption of open-source components by major tech companies corroborate the ongoing benefits and the persistence of the problems open source addresses.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.15) reflects that while open source creates market pressure on proprietary vendors, it primarily generates value rather than extracting it. Suppression (0.05) is minimal, as adoption is driven by demonstrated benefits, not coercion. Accessibility collapse (0.8) is high because once the benefits of open development are understood, alternatives (closed, siloed development) are seen as less effective. Resistance (0.1) is low because the methodology is widely accepted for its practical advantages. The claimed type is 'rope' because it's a coordination mechanism that benefits participants without significant extraction.
 *
 * PERSPECTIVAL GAP:
 *   This reading's focus on instrumental benefits means that while it acknowledges the existence of proprietary software, it implicitly frames it as a less efficient or less robust development model in many contexts. This contrasts sharply with the 'property rights' reading, which would see proprietary models as the default and open source as a deviation requiring justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers, users, and tech companies are beneficiaries, gaining from improved quality, collaboration, and efficiency. Proprietary software vendors are 'payers' in the sense that they face competitive pressure and may need to adapt their models, but they are not 'victims' of extraction. Intellectual property advocates are 'excluded' as their primary concern (exclusive ownership) is secondary to the pragmatic focus on development quality in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_strength,
    'How robust is the empirical evidence for open source''s superiority in all development contexts, particularly for highly specialized or niche software?',
    'Further comparative studies across diverse software domains, measuring quality, security, and development velocity for open vs. proprietary models.',
    'If evidence for superiority is weaker in certain contexts, the ''pragmatic development'' reading might be refined to acknowledge contextual limitations, potentially shifting its classification towards a more ''utilitarian hybrid'' perspective for those specific cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_strength, empirical, 'The extent to which open source''s pragmatic benefits hold universally.').

omega_variable(
    definition_of_quality,
    'Does ''quality'' in this reading primarily refer to technical robustness (bugs, security) or also include user experience, feature completeness, and long-term support, which proprietary models sometimes excel at?',
    'Clarification of the specific metrics and criteria used to define ''superiority'' in software development within this reading''s framework.',
    'If ''quality'' is narrowly defined, the reading''s claim of superiority might be less comprehensive than implied, potentially strengthening the ''utilitarian hybrid'' or even ''property rights'' readings for aspects of quality not covered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_quality, conceptual, 'The scope and definition of ''quality'' in the pragmatic argument for open source.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine reading of the ''software_source_status'' kernel, or does it merely describe a set of best practices that could apply to any software development, regardless of source status?',
    'Analysis of whether the ''superiority'' claim inherently relies on the ''openness'' of the source code (e.g., for peer review, community contribution) or if similar benefits could be achieved in a closed but well-managed proprietary environment.',
    'If the benefits are not intrinsically tied to ''openness'', this constraint might be reclassified as a ''best_practices_methodology'' rather than a reading of the ''software_source_status'' kernel, affecting its network relationships and CS structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether the ''pragmatic development'' claim is truly about source status or general development practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1990, software_source_status__pragmatic_development_reading, theater_ratio, 1990, 0.01).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__pragmatic_development_reading, theater_ratio, 2000, 0.01).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__pragmatic_development_reading, theater_ratio, 2010, 0.02).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__pragmatic_development_reading, theater_ratio, 2024, 0.02).

% Extraction over time
narrative_ontology:measurement(soft_be_t1990, software_source_status__pragmatic_development_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(soft_be_t2000, software_source_status__pragmatic_development_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement(soft_be_t2010, software_source_status__pragmatic_development_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(soft_be_t2024, software_source_status__pragmatic_development_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1990, software_source_status__pragmatic_development_reading, suppression_requirement, 1990, 0.03).
narrative_ontology:measurement(soft_su_t2000, software_source_status__pragmatic_development_reading, suppression_requirement, 2000, 0.04).
narrative_ontology:measurement(soft_su_t2010, software_source_status__pragmatic_development_reading, suppression_requirement, 2010, 0.04).
narrative_ontology:measurement(soft_su_t2024, software_source_status__pragmatic_development_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, information_standard).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_source_status' kernel. Each reading represents a distinct structural claim about the nature and value of open vs. proprietary software.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
