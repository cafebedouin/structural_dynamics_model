% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive Licensing Without Reciprocity Enables Proprietary Enclosure
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the copyleft_counterfactual_reading of
 *   the permissive_license_text kernel. It analyzes permissive licensing
 *   (MIT, BSD, Apache) as a high-extraction tangled rope: the coordination
 *   function (frictionless reuse) is genuine but the same structure enables
 *   asymmetric extraction by proprietary actors who incorporate community
 *   code without reciprocating. The reading argues viral reciprocity (GPL) is
 *   the necessary alternative to prevent enclosure. The claimed type
 *   (tangled_rope) and metrics are authored independently — the engine
 *   computes per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.78).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.65).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive Licensing Without Reciprocity Enables Proprietary Enclosure").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'feb3508a-25ad-4fbd-8755-2460b6641a22').
narrative_ontology:cs_kernel_codification('feb3508a-25ad-4fbd-8755-2460b6641a22', formalized).
narrative_ontology:cs_authority_grounding('feb3508a-25ad-4fbd-8755-2460b6641a22', lineage).
narrative_ontology:cs_interpretation_layer_present('feb3508a-25ad-4fbd-8755-2460b6641a22').
narrative_ontology:cs_reading_relation('feb3508a-25ad-4fbd-8755-2460b6641a22', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('feb3508a-25ad-4fbd-8755-2460b6641a22', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('feb3508a-25ad-4fbd-8755-2460b6641a22', foundational, reciprocity_required_for_software_freedom).
narrative_ontology:cs_axiom_status(reciprocity_required_for_software_freedom, holdable).
narrative_ontology:cs_axiom_grounding('feb3508a-25ad-4fbd-8755-2460b6641a22', reciprocity_required_for_software_freedom, deontological).
narrative_ontology:cs_axiom('feb3508a-25ad-4fbd-8755-2460b6641a22', foundational, permissive_licensing_enables_enclosure).
narrative_ontology:cs_axiom_status(permissive_licensing_enables_enclosure, holdable).
narrative_ontology:cs_axiom_grounding('feb3508a-25ad-4fbd-8755-2460b6641a22', permissive_licensing_enables_enclosure, empirically_contingent).
narrative_ontology:cs_reference_frame('feb3508a-25ad-4fbd-8755-2460b6641a22', copyleft_ethical_framework).
narrative_ontology:cs_drift_state('feb3508a-25ad-4fbd-8755-2460b6641a22', contemporary_cloud_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('feb3508a-25ad-4fbd-8755-2460b6641a22', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, cloud_infrastructure_providers).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, commercial_redistributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, original_authors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, free_software_commons).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, downstream_users_denied_freedom).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, reciprocity_required_for_software_freedom).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, permissive_licensing_enables_enclosure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Incorporate permissively-licensed code into proprietary products without contributing modifications back. Capture the value of community labor while maintaining exclusive control over derivative works. Exit is arbitrage-grade: they can switch license strategies, acquire companies, or lobby for favorable IP regimes.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Run permissively-licensed software (databases, orchestration, runtimes) as managed services without sharing improvements. Extract rental value from operating software they did not write and need not improve publicly. Exit options include vertically integrating up the stack or shifting to proprietary alternatives.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, cloud_infrastructure_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Rebundle permissively-licensed components into commercial distributions with proprietary extensions. Capture value through packaging, support, and proprietary add-ons while the core remains free. Can exit by pivoting to different component stacks.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, commercial_redistributors, beneficiary,
    organized, biographical, mobile, global).

% Release code under permissive licenses hoping for adoption, then watch corporations build proprietary empires on their work without contribution. Exit is constrained: once released permissively, they cannot retroactively impose reciprocity; relicensing requires contributor agreement which is often impractical.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, original_authors, payer,
    moderate, biographical, constrained, global).

% The shared pool of free code that grows through reciprocity. Permissive licensing drains value from the commons into proprietary silos. Exit is constrained: the commons cannot prevent permissive licensing, only counter it with copyleft alternatives.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, free_software_commons, payer,
    organized, generational, constrained, global).

% Analyze and advocate against permissive licensing's extraction dynamics. Bear costs of maintaining copyleft infrastructure (GPL compliance, legal defense, education). Their analytical seat sees the full structure; their payer role reflects resource expenditure defending the commons.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, observer,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, payer).

% End users of proprietary products built on permissive code who cannot inspect, modify, or redistribute the software they depend on. Trapped by vendor lock-in, network effects, and lack of alternatives. The freedom the original license promised is lost in the proprietary derivative.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, downstream_users_denied_freedom, payer,
    powerless, biographical, trapped, global).

% Competition authorities and standards bodies evaluating whether permissive licensing enables anti-competitive enclosure. They observe from outside the licensing bargain but can impose remedies (interoperability mandates, essential facility doctrines) that alter the constraint's operation.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, regulatory_bodies, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licenses solve the coordination problem of frictionless code reuse: they minimize legal transaction costs so developers can incorporate, modify, and distribute code without license compatibility analysis or contribution obligations.
% TRANSFER_FUNCTION: Moves the value of authored code and community maintenance labor from original authors and the free software commons to proprietary entities who capture it in closed products and managed services without reciprocating improvements.
% ABSENT_VOICES: Authors who chose permissive licenses for adoption but expected community norms to ensure contribution (violated by corporate actors); users in proprietary ecosystems who never consented to freedom loss; maintainers of permissive projects who burn out while cloud providers profit from their labor.
% DISAPPEARANCE_RATIONALE: If permissive licensing without reciprocity vanished overnight, proprietary enclosure of community code would collapse. Companies would either adopt copyleft (contributing back) or invest in proprietary alternatives. The software economy would reorganize around reciprocal licensing or closed-source development — the current extraction model would be structurally impossible.
% FOUNDING_PROBLEM: Early open source (late 1990s) needed broad adoption to prove the collaborative development model. Permissive licenses lowered barriers for corporate participation, making open source palatable to legal departments wary of GPL's viral terms.
% FOUNDING_PROBLEM_CORROBORATION: Eric Raymond's 'The Cathedral and the Bazaar' (1999) attests the adoption motive from within the open source movement. Richard Stallman and the Free Software Foundation (1985–present) attest from outside the beneficiary set that the founding problem (adoption) was achieved but the arrangement persists as extraction. Independent economic analyses (e.g., Nagle 2019 on cloud extraction) corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the value transfer from commons to proprietary enclosure is substantial and growing — cloud providers capture rental value from operating permissive software at scale. Suppression (0.65) reflects copyright law enforcing the permissive terms (which forbid reciprocity requirements) and the practical impossibility of relicensing once code is widely adopted. Theater ratio (0.42) captures the 'open source' branding that obscures extraction: the coordination function is real but increasingly performs as cover for enclosure. The measurement grid shows extractiveness rising sharply post-2010 with cloud adoption, theater rising as 'open core' and 'source available' labels proliferate, suppression hardening as relicensing becomes legally infeasible.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary beneficiary seats, the arrangement is genuine coordination they helped build (low χ). From the payer seats (authors, commons, users), the same structure operates as enforced extraction (high χ). Copyleft advocates' observer seat sees the full asymmetry; their secondary payer role reflects resource costs of defending the commons. The engine computes this divergence — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary companies, cloud providers, and redistributors are structural beneficiaries (d near 0.0) — they collect extraction, control the rules, have arbitrage-grade exit. Original authors, the commons, copyleft advocates, and downstream users are targets (d near 1.0) — they bear costs, have constrained or trapped exit. Regulatory bodies sit at analytical (d=0.5). The derivation chain: beneficiaries declared → low d; victims declared → high d; exit options modulate (arbitrage → lower d, trapped → higher d).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adoption) is contested: beneficiaries claim it's live (ongoing need for broad adoption); payers and observers argue it's dead (adoption achieved, extraction persists). This mismatch (status=contested + verdict=world_rearranges) flags capture/zombie dynamics — the arrangement persists beyond its coordination justification because beneficiaries capture the extraction. The mandatrophy_resolved flag is not declared; the R5 mismatch feeds the engine's capture detection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the permissive_license_text kernel one constraint with multiple readings, or are these structurally distinct constraints that happen to share license text?',
    'Apply ε-invariance test: if measuring permissive licensing via adoption metrics yields low ε but measuring via enclosure metrics yields high ε, they are distinct constraints. This reading asserts the latter — the commons_coordination_reading and corporate_moat_reading measure different structural realities.',
    'If distinct constraints, each gets its own ε and classification; the kernel label ''permissive license'' is a false summit conflating coordination and extraction. If one constraint, the readings are perspectival variants and ε must be invariant — requiring decomposition per DP-001.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel''s readings instantiate one constraint or multiple ε-distinct constraints.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function of permissive licenses (frictionless reuse) structurally separable from the extraction function (enclosure without reciprocity), or are they inseparable features of the same license terms?',
    'Natural experiment: jurisdictions or projects that add reciprocity requirements (e.g., GPL, Parity License, Ethical Source licenses) — if adoption collapses, coordination depends on non-reciprocity; if adoption holds with reciprocity, the functions are separable.',
    'If inseparable, the measured extraction is the price of coordination (rope-like); if separable, the extraction is pure overhead riding on coordination (tangled_rope or snare). This reading asserts separability — copyleft proves coordination works with reciprocity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether permissive licensing''s coordination and extraction components can be decoupled.').

omega_variable(
    copyleft_effectiveness,
    'Does viral reciprocity (GPL) actually prevent enclosure, or does it merely shift extraction to other layers (SaaS loophole, cloud hosting, hardware locking)?',
    'Longitudinal study of GPL-licensed projects: measure contribution rates, fork dynamics, and proprietary enclosure attempts over 20+ years. Compare to permissive-licensed counterparts in similar domains.',
    'If copyleft fails to prevent enclosure at scale, this reading''s ''necessary alternative'' claim is falsified — the constraint family may have no non-extractive member. If copyleft succeeds, the tangled_rope classification holds with copyleft as the coordination-only (rope) alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_effectiveness, empirical, 'Whether the proposed alternative (copyleft) structurally resolves the extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t1998, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(perm_tr_t2004, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(perm_tr_t2010, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(perm_tr_t2015, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(perm_tr_t2020, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(perm_tr_t2024, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(perm_be_t1998, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 1998, 0.25).
narrative_ontology:measurement(perm_be_t2004, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(perm_be_t2010, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(perm_be_t2015, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(perm_be_t2020, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(perm_be_t2024, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t1998, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement(perm_su_t2004, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2004, 0.3).
narrative_ontology:measurement(perm_su_t2010, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(perm_su_t2015, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(perm_su_t2020, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(perm_su_t2024, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.03).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, gpl_enforcement_dynamics).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, cloud_saas_loophole).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, ethical_source_licensing).

% DUAL FORMULATION NOTE:
% This story decomposes the permissive_license_text kernel into three ε-distinct constraints per the ε-invariance principle. The commons_coordination_reading measures adoption coordination (low ε, rope); this reading measures enclosure extraction (high ε, tangled_rope); corporate_moat_reading measures strategic moat-building (high ε, snare). All three share the same license text but instantiate different constraints because their ε referents differ: coordination friction vs. enclosure dynamics vs. moat economics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, organized, 0.15).
constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
