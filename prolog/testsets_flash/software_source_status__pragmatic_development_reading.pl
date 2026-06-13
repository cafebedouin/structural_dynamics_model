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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Pragmatic Development Superiority
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'pragmatic development' reading of
 *   software source status, where open source is valued for its instrumental
 *   benefits to quality, security, and innovation. It is distinct from a
 *   purely ethical 'freedom imperative' or a 'property rights' defense. This
 *   reading acknowledges that proprietary software is not inherently
 *   illegitimate, but argues that open development practices lead to superior
 *   outcomes. Permissive licensing (e.g., MIT, Apache) is acceptable as it
 *   allows for broader adoption and integration, even into proprietary
 *   products, as long as the benefits of open collaboration are realized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.2).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.1).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Pragmatic Development Superiority").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'e45c830c-16f4-4a36-b9e1-dac882effc9f').
narrative_ontology:cs_kernel_codification('e45c830c-16f4-4a36-b9e1-dac882effc9f', distributed).
narrative_ontology:cs_authority_grounding('e45c830c-16f4-4a36-b9e1-dac882effc9f', practice).
narrative_ontology:cs_interpretation_layer_present('e45c830c-16f4-4a36-b9e1-dac882effc9f').
narrative_ontology:cs_reading_relation('e45c830c-16f4-4a36-b9e1-dac882effc9f', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('e45c830c-16f4-4a36-b9e1-dac882effc9f', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('e45c830c-16f4-4a36-b9e1-dac882effc9f', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('e45c830c-16f4-4a36-b9e1-dac882effc9f', foundational, open_collaboration_yields_superior_software).
narrative_ontology:cs_axiom_status(open_collaboration_yields_superior_software, holdable).
narrative_ontology:cs_axiom_grounding('e45c830c-16f4-4a36-b9e1-dac882effc9f', open_collaboration_yields_superior_software, empirically_contingent).
narrative_ontology:cs_axiom('e45c830c-16f4-4a36-b9e1-dac882effc9f', secondary, permissive_licensing_maximizes_adoption_and_impact).
narrative_ontology:cs_axiom_status(permissive_licensing_maximizes_adoption_and_impact, holdable).
narrative_ontology:cs_axiom_grounding('e45c830c-16f4-4a36-b9e1-dac882effc9f', permissive_licensing_maximizes_adoption_and_impact, instrumental).
narrative_ontology:cs_reference_frame('e45c830c-16f4-4a36-b9e1-dac882effc9f', meritocratic_open_development).
narrative_ontology:cs_drift_state('e45c830c-16f4-4a36-b9e1-dac882effc9f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e45c830c-16f4-4a36-b9e1-dac882effc9f', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_developers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, tech_companies_using_oss).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, proprietary_software_vendors).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, peer_review_enhances_quality).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, transparency_improves_security).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, distributed_innovation_accelerates_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the collaborative environment, peer review, and reputation building inherent in open source. They contribute to projects, gain skills, and often find employment opportunities based on their contributions. They are not bound to any single project or platform.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from higher quality, more secure, and often free software. They can inspect the code, contribute bug reports, and sometimes even fund features. Their options are to use open source or proprietary alternatives.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_users, beneficiary,
    moderate, biographical, mobile, global).

% Leverage open source software to build products and services, reducing development costs and accelerating time to market. They contribute back to projects where it aligns with their business interests, but also maintain proprietary components. They have significant flexibility in their technology stack choices.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, tech_companies_using_oss, beneficiary,
    institutional, generational, arbitrage, global).

% Face competitive pressure from high-quality, free, and transparent open source alternatives. They must innovate faster, offer superior features, or compete on support and services. Their business model is challenged by the 'superiority' claim of open source.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_vendors, payer,
    powerful, generational, constrained, global).

% Their traditional role in defending proprietary software rights is diminished by the open-source ethos. They would argue for stronger enforcement of copyright and patent protections, but their voice is often marginalized in open-source communities.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, intellectual_property_lawyers, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global community of developers and users around shared codebases, enabling rapid iteration, peer review, and collective problem-solving for complex software projects.
% TRANSFER_FUNCTION: Transfers knowledge, code, and reputation among participants, leading to higher quality software that is often freely available, challenging traditional proprietary software markets.
% ABSENT_VOICES: Strict intellectual property advocates and proprietary software vendors who prioritize exclusive control over collaborative development are often excluded from the core discourse, as their foundational premises conflict with the open-source ethos.
% DISAPPEARANCE_RATIONALE: If the belief in open source's pragmatic superiority vanished, the incentives for collaborative development would erode, leading to a fragmentation of projects, increased proprietary lock-in, and a slower pace of innovation across the software industry. The current software ecosystem relies heavily on this paradigm.
% FOUNDING_PROBLEM: Proprietary software development was often slow, buggy, and opaque, leading to vendor lock-in and limited innovation. The founding problem was to find a more efficient, transparent, and collaborative way to build high-quality software.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing success of major open-source projects (Linux, Apache, Kubernetes) and their adoption by major tech companies corroborates the continued relevance of this problem and the effectiveness of the open-source approach. Independent academic studies on software quality and security also provide corroboration.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).

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
 *   The extractiveness is low (0.2) because the constraint primarily promotes a development methodology rather than directly extracting rents. Any 'extraction' is indirect, e.g., proprietary vendors needing to adapt or compete. Suppression is also low (0.1) as it relies on cultural adoption and demonstrated success, not coercion. Theater ratio is minimal (0.05) as the claims of quality and security are largely borne out by empirical evidence. Accessibility collapse is moderate (0.7) because while open source is widely available, adopting its full development methodology requires significant cultural and organizational shifts. Resistance is low (0.15) because the pragmatic benefits are widely recognized, even by many proprietary vendors who use OSS components.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of open source advocates within this reading, the constraint is a 'rope' that coordinates superior development. From the perspective of proprietary software vendors, it's a 'tangled rope' or 'snare' that extracts market share and forces them to compete on unfavorable terms, even if they acknowledge some of its benefits. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Open source developers, users, and tech companies leveraging OSS are beneficiaries, as they directly gain from the collaborative model and the resulting software quality. Proprietary software vendors are payers, as they face competitive pressure and must adapt their strategies. Intellectual property lawyers, whose business model is tied to proprietary rights, are largely excluded from the core discourse of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_superiority_durability,
    'Will the empirical evidence for open source''s superior quality and security remain robust against future proprietary innovations or new development paradigms?',
    'Ongoing comparative studies of software quality, security vulnerabilities, and innovation rates between open source and proprietary projects over the next decade.',
    'If proprietary models consistently outperform open source on key metrics, the ''pragmatic development'' claim would weaken, potentially shifting this constraint towards a ''piton'' (if maintained by inertia) or ''snare'' (if used to extract from those still believing the claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_superiority_durability, empirical, 'Durability of open source''s empirical superiority claims.').

omega_variable(
    permissive_licensing_boundary,
    'At what point does permissive licensing (e.g., MIT, Apache) cease to embody the ''pragmatic development'' ethos by enabling excessive proprietary enclosure of open contributions?',
    'Analysis of licensing trends and the proportion of open-source contributions that are subsequently re-enclosed in proprietary products, alongside community consensus on acceptable enclosure levels.',
    'If permissive licensing is found to enable significant enclosure that undermines the collaborative benefits, this reading might shift towards favoring stronger copyleft licenses, or be reclassified as a ''tangled_rope'' where some benefit from the open contribution while others extract by enclosing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissive_licensing_boundary, conceptual, 'The boundary between permissive licensing and proprietary enclosure within the pragmatic development framework.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the ''software_source_status'' kernel primarily an empirical question about development efficacy, or a normative question about rights and ethics?',
    'Analysis of legal precedents, philosophical arguments, and industry discourse to determine which framing (empirical vs. normative) dominates in different contexts and for different stakeholders.',
    'If the normative framing (e.g., ''freedom_imperative_reading'' or ''property_rights_reading'') gains dominance, the ''pragmatic_development_reading'' would be seen as a secondary, instrumental concern, potentially reducing its influence and shifting its classification towards a ''piton'' if its core claims are no longer central.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the fundamental framing of the software source status kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1990, software_source_status__pragmatic_development_reading, theater_ratio, 1990, 0.02).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__pragmatic_development_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__pragmatic_development_reading, theater_ratio, 2010, 0.04).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__pragmatic_development_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(soft_be_t1990, software_source_status__pragmatic_development_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(soft_be_t2000, software_source_status__pragmatic_development_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(soft_be_t2010, software_source_status__pragmatic_development_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(soft_be_t2024, software_source_status__pragmatic_development_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1990, software_source_status__pragmatic_development_reading, suppression_requirement, 1990, 0.05).
narrative_ontology:measurement(soft_su_t2000, software_source_status__pragmatic_development_reading, suppression_requirement, 2000, 0.08).
narrative_ontology:measurement(soft_su_t2010, software_source_status__pragmatic_development_reading, suppression_requirement, 2010, 0.09).
narrative_ontology:measurement(soft_su_t2024, software_source_status__pragmatic_development_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, information_standard).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'software_source_status' kernel, focusing on the pragmatic benefits of open development. It influences and coexists with other readings that emphasize ethical freedom, property rights, or aggregate welfare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
