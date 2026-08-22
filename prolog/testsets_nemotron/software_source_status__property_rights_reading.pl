% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Proprietary Software Source Code Restrictions as Property Rights
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   The property rights reading of software source status treats source code
 *   as a proprietary asset analogous to other forms of intellectual property.
 *   Creators (individual or corporate) hold legitimate ownership rights that
 *   include the right to restrict access, modification, and redistribution
 *   through licensing. Users are consumers who acquire contractual rights
 *   only — typically a limited, non-exclusive, non-transferable license to
 *   use the software in object-code form. This reading underpins the entire
 *   commercial proprietary software industry, including shrink-wrap licenses,
 *   EULAs, SaaS terms, and the legal architecture of copyright, trade secret,
 *   and anti-circumvention law (DMCA 1201, EUCD Article 6) as applied to
 *   software.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.62).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.58).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Proprietary Software Source Code Restrictions as Property Rights").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "economic/technological/social").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, '8894fb07-df5c-4f53-850c-22bfec20c071').
narrative_ontology:cs_kernel_codification('8894fb07-df5c-4f53-850c-22bfec20c071', formalized).
narrative_ontology:cs_authority_grounding('8894fb07-df5c-4f53-850c-22bfec20c071', extraction).
narrative_ontology:cs_interpretation_layer_present('8894fb07-df5c-4f53-850c-22bfec20c071').
narrative_ontology:cs_reading_relation('8894fb07-df5c-4f53-850c-22bfec20c071', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('8894fb07-df5c-4f53-850c-22bfec20c071', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('8894fb07-df5c-4f53-850c-22bfec20c071', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('8894fb07-df5c-4f53-850c-22bfec20c071', foundational, source_code_is_proprietary_asset).
narrative_ontology:cs_axiom_status(source_code_is_proprietary_asset, holdable).
narrative_ontology:cs_axiom_grounding('8894fb07-df5c-4f53-850c-22bfec20c071', source_code_is_proprietary_asset, conventional).
narrative_ontology:cs_axiom('8894fb07-df5c-4f53-850c-22bfec20c071', foundational, licensing_restrictions_are_legitimate_ownership_exercise).
narrative_ontology:cs_axiom_status(licensing_restrictions_are_legitimate_ownership_exercise, holdable).
narrative_ontology:cs_axiom_grounding('8894fb07-df5c-4f53-850c-22bfec20c071', licensing_restrictions_are_legitimate_ownership_exercise, deontological).
narrative_ontology:cs_axiom('8894fb07-df5c-4f53-850c-22bfec20c071', secondary, users_are_consumers_with_contractual_rights_only).
narrative_ontology:cs_axiom_status(users_are_consumers_with_contractual_rights_only, holdable).
narrative_ontology:cs_axiom_grounding('8894fb07-df5c-4f53-850c-22bfec20c071', users_are_consumers_with_contractual_rights_only, conventional).
narrative_ontology:cs_reference_frame('8894fb07-df5c-4f53-850c-22bfec20c071', classical_copyright_software_regime).
narrative_ontology:cs_drift_state('8894fb07-df5c-4f53-850c-22bfec20c071', contemporary_saas_cloud_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8894fb07-df5c-4f53-850c-22bfec20c071', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, software_publishers).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, platform_owners).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, ip_law_firms).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, end_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, security_researchers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, educational_institutions).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, intellectual_property_natural_rights).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, contractual_freedom_of_licensing).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, creator_control_over_distribution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and distribute proprietary software under restrictive licenses. Control source code access, modification rights, and redistribution terms through EULAs and license enforcement. Collect license revenue and maintain competitive advantage through source secrecy. Lobby for stronger IP enforcement and anti-circumvention laws.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate app stores and software distribution platforms that enforce proprietary licensing terms. Collect platform fees (15-30%) on proprietary software transactions. Benefit from the ecosystem lock-in that proprietary licensing creates. Set platform policies that reinforce proprietary models.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, platform_owners, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, platform_owners, agenda_setter).

% Specialize in software licensing, IP enforcement, and anti-piracy litigation. Represent publishers in license compliance audits, DMCA takedowns, and trade secret protection. Revenue scales with the complexity and enforceability of proprietary licensing regimes.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, ip_law_firms, beneficiary,
    organized, biographical, mobile, global).

% Purchase licenses for proprietary software but cannot inspect, modify, or redistribute the source code. Dependent on vendors for bug fixes, security patches, and feature updates. Face vendor lock-in, forced upgrades, and discontinuation risk. Pay recurring license fees without ownership of the software artifacts they rely on.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, end_users, payer,
    organized, biographical, constrained, global).

% Cannot build upon or learn from proprietary source code. Must reimplement functionality from scratch or work within vendor-controlled APIs and SDKs. Face legal barriers to interoperability (reverse engineering restrictions, anti-circumvention). Limited ability to serve niche markets that vendors ignore.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, independent_developers, excluded).

% Legally restricted from analyzing proprietary binaries for vulnerabilities (DMCA 1201, CFAA). Cannot publish proof-of-concept exploits or detailed vulnerability analyses without vendor permission. Coordinated disclosure programs are vendor-controlled and can suppress findings. Research that benefits the public is chilled by legal risk.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, security_researchers, excluded,
    moderate, biographical, constrained, global).

% Pay site licenses for proprietary software used in teaching and research. Cannot adapt software for pedagogical needs or share modified versions with students. Curriculum constrained by vendor feature sets and licensing terms. Long-term dependency on vendors whose educational pricing may change.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, educational_institutions, payer,
    organized, generational, constrained, global).

% Develops and maintains free software alternatives. Provides empirical evidence that collaborative development produces high-quality software without proprietary restrictions. Advocates for user freedom and against software patents and anti-circumvention laws. Competes with proprietary vendors in some markets.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, open_source_community, observer,
    organized, generational, analytical, global).

% Investigate whether proprietary licensing practices constitute anti-competitive behavior (tying, refusal to deal, essential facilities). Evaluate interoperability mandates and right-to-repair legislation. Can impose remedies that modify the enforcement landscape for proprietary software.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal and economic framework for software creators to recoup development investment through controlled distribution, enabling commercial software markets that fund large-scale, coordinated engineering efforts.
% TRANSFER_FUNCTION: Moves licensing revenue and control over software artifacts from users and downstream developers to publishers and platform owners, in exchange for the right to use the software under vendor-defined terms.
% ABSENT_VOICES: Users in the Global South who cannot afford proprietary licenses but are excluded from the policy conversation; future generations who inherit a software commons depleted by enclosure; whistleblowers and auditors who need source access to verify claims about critical infrastructure.
% DISAPPEARANCE_RATIONALE: If proprietary source restrictions vanished overnight, the commercial software industry would reorganize around service/support/open-core models; users would gain inspection and modification rights; security research would accelerate; but the investment calculus for certain categories of high-upfront-cost software (e.g., specialized CAD, game engines) would shift dramatically.
% FOUNDING_PROBLEM: Early commercial software (1970s-80s) faced rampant unauthorized copying with no technical protection; creators had no reliable way to monetize development effort, threatening the viability of professional software production.
% FOUNDING_PROBLEM_CORROBORATION: Software publishers and trade associations (BSA, SIIA) attest the problem remains live, citing ongoing piracy and the need for IP protection to fund R&D. Economic historians and open source advocates attest the founding problem was substantially solved by alternative models (SaaS, open core, crowdfunding) and that current enforcement exceeds the original justification; academic studies of open source sustainability (e.g., Nagappan et al., 2013; Eghbal, 2020) corroborate the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial: the constraint transfers both economic value (license fees, platform commissions) and autonomy (inspection, modification, repair rights) from a broad user base to a concentrated publisher/platform tier. Suppression (0.58) is significant: the constraint's persistence depends on active legal enforcement (copyright, trade secret, anti-circumvention), technical measures (obfuscation, DRM, hardware locks), and contractual terms that restrict reverse engineering and benchmarking. Theater ratio (0.31) reflects that the coordination function (funding software development) is real but increasingly decoupled from the extraction mechanism — SaaS and open-core models demonstrate development funding without source restriction. Accessibility collapse (0.45) is moderate: alternatives exist (free software, open source) but face network effects, format lock-in, and ecosystem barriers. Resistance (0.48) is substantial and growing: right-to-repair movements, security research advocacy, government open source mandates, and competitive pressure from open alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher seat, the constraint is genuine coordination: it solves the public goods problem of software funding. From the user/developer seats, the same structure operates as enforced extraction: they pay for software they cannot control, audit, or adapt. The engine computes this seat divergence from the structural data — the property rights claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Software publishers and platform owners are structural beneficiaries (d near 0.0-0.2): they collect the economic rents and control the rules. IP law firms are secondary beneficiaries (d ~ 0.2-0.3): they profit from enforcement complexity. End users, independent developers, security researchers, and educational institutions are structural targets (d near 0.7-0.9): they bear the costs in autonomy, money, and foregone innovation. The open source community and competition authorities are analytical observers (d = 0.5): they analyze the structure without directly collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (monetizing software development in an environment of costless copying) was real but has been substantially solved by alternative models. The constraint persists with expanded scope (anti-circumvention, SaaS lock-in, hardware-enforced restrictions) that exceeds the original justification. This is mandatrophy: the mandate (protect creator revenue) has atrophied into a broader control regime (prevent user autonomy, suppress competition, enforce ecosystem lock-in). The classification as tangled_rope captures this dual nature — genuine coordination function layered with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the property_rights_reading a distinct constraint with its own stable ε, or does it collapse into the utilitarian_hybrid_reading under empirical scrutiny?',
    'Test whether the property rights reading''s ε remains stable when measured against the standing arrangement (proprietary software regime) versus when measured against a counterfactual open regime. If ε varies with the measurement basis, the reading is not ε-invariant and must be decomposed.',
    'If the reading is not ε-invariant, it is not a single constraint but a family — the engine would need separate stories for ''proprietary licensing as investment recovery'' and ''proprietary licensing as ecosystem control'' per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this kernel reading satisfies ε-invariance or conceals multiple constraints').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (funding software development) be separated from the extraction mechanism (source restriction) in practice?',
    'Natural experiments: SaaS models, open-core businesses, public funding of open source, and government mandates for open source in procurement. Measure whether development velocity and quality are maintained when source restrictions are removed.',
    'If separable, the extraction component is pure rent and the constraint is more snare-like; if inseparable, part of measured extraction is genuine coordination cost and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable').

omega_variable(
    reading_foreclosure_relations,
    'Does the property_rights_reading logically foreclose the freedom_imperative_reading within a single legal framework, or do they coexist as competing policy positions?',
    'Analyze whether a jurisdiction can simultaneously recognize software as property (with full exclusion rights) AND recognize a user right to inspect/modify (freedom imperative) without contradiction. Test in constitutional and human rights frameworks.',
    'If forecloses, the kernel has a genuine logical fracture; if coexists_with, the kernel hosts a persistent political contest that the engine should model as parallel constraint families.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_relations, conceptual, 'Structural relationship between this reading and the freedom_imperative_reading').

omega_variable(
    anti_circumvention_scope_creep,
    'Has anti-circumvention law (DMCA 1201, EUCD Art 6) expanded the constraint''s extraction beyond the original copyright justification into a general-purpose control right?',
    'Trace litigation and legislative history: count cases where anti-circumvention was used for non-copyright purposes (printer cartridges, garage door openers, tractor repair, medical devices). Measure the ratio of copyright-related to control-related invocations over time.',
    'If scope creep is documented, the constraint''s extraction has accumulated beyond its founding mandate — supports mandatrophy classification and T17 drift detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(anti_circumvention_scope_creep, empirical, 'Whether anti-circumvention provisions have become a general control right beyond copyright').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1976, software_source_status__property_rights_reading, theater_ratio, 1976, 0.12).
narrative_ontology:measurement(soft_tr_t1985, software_source_status__property_rights_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(soft_tr_t1995, software_source_status__property_rights_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(soft_tr_t2000, software_source_status__property_rights_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__property_rights_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(soft_tr_t2020, software_source_status__property_rights_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__property_rights_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(soft_be_t1976, software_source_status__property_rights_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(soft_be_t1985, software_source_status__property_rights_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(soft_be_t1995, software_source_status__property_rights_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(soft_be_t2000, software_source_status__property_rights_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(soft_be_t2010, software_source_status__property_rights_reading, base_extractiveness, 2010, 0.59).
narrative_ontology:measurement(soft_be_t2020, software_source_status__property_rights_reading, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(soft_be_t2024, software_source_status__property_rights_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1976, software_source_status__property_rights_reading, suppression_requirement, 1976, 0.25).
narrative_ontology:measurement(soft_su_t1985, software_source_status__property_rights_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(soft_su_t1995, software_source_status__property_rights_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(soft_su_t2000, software_source_status__property_rights_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(soft_su_t2010, software_source_status__property_rights_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(soft_su_t2020, software_source_status__property_rights_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement(soft_su_t2024, software_source_status__property_rights_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.15).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, dmca_1201_anti_circumvention).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, software_patent_regime).
narrative_ontology:affects_constraint(software_source_status__property_rights_reading, right_to_repair_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the software_source_status kernel. The freedom_imperative_reading treats proprietary restriction as injustice (high extraction, snare classification expected). The pragmatic_development_reading treats open source as methodological superior (coordination function with low extraction, rope classification expected). The utilitarian_hybrid_reading treats licensing as context-dependent welfare optimization (scaffold or tangled_rope depending on domain). All four readings share the referent (the standing proprietary software regime) but author different ε, beneficiary/victim structures, and claimed types — per ε-invariance, they are distinct constraints linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_source_status__property_rights_reading, institutional, 0.15).
constraint_indexing:directionality_override(software_source_status__property_rights_reading, organized, 0.75).
constraint_indexing:directionality_override(software_source_status__property_rights_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
