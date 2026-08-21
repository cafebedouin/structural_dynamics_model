% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Restriction Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'copyleft as restriction' reading
 *   of the GPL reciprocity obligation. From this perspective, the GPL, by
 *   imposing a 'viral' reciprocity requirement, paradoxically constrains
 *   open-source development and enables proprietary vendors to create forks
 *   or integrate code in ways that ultimately extract value from the commons.
 *   The title 'Viral licensing constrains business models by prohibiting
 *   proprietary integration' is interpreted here as the constraint on the
 *   *open-source* business model, which is undermined by proprietary
 *   exploitation enabled by this reading, rather than a constraint on
 *   proprietary businesses themselves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.85).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.8).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation (Copyleft as Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e329e987-a360-4f85-98dd-afec766ecc0a').
narrative_ontology:cs_kernel_codification('e329e987-a360-4f85-98dd-afec766ecc0a', fixed_text).
narrative_ontology:cs_authority_grounding('e329e987-a360-4f85-98dd-afec766ecc0a', lineage).
narrative_ontology:cs_interpretation_layer_present('e329e987-a360-4f85-98dd-afec766ecc0a').
narrative_ontology:cs_reading_relation('e329e987-a360-4f85-98dd-afec766ecc0a', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('e329e987-a360-4f85-98dd-afec766ecc0a', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('e329e987-a360-4f85-98dd-afec766ecc0a', foundational, proprietary_integration_is_a_fundamental_business_right).
narrative_ontology:cs_axiom_status(proprietary_integration_is_a_fundamental_business_right, holdable).
narrative_ontology:cs_axiom_grounding('e329e987-a360-4f85-98dd-afec766ecc0a', proprietary_integration_is_a_fundamental_business_right, conventional).
narrative_ontology:cs_axiom('e329e987-a360-4f85-98dd-afec766ecc0a', secondary, copyleft_creates_unnecessary_barriers_to_commercialization).
narrative_ontology:cs_axiom_status(copyleft_creates_unnecessary_barriers_to_commercialization, holdable).
narrative_ontology:cs_axiom_grounding('e329e987-a360-4f85-98dd-afec766ecc0a', copyleft_creates_unnecessary_barriers_to_commercialization, empirically_contingent).
narrative_ontology:cs_reference_frame('e329e987-a360-4f85-98dd-afec766ecc0a', unrestricted_software_development).
narrative_ontology:cs_drift_state('e329e987-a360-4f85-98dd-afec766ecc0a', contemporary_software_licensing_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e329e987-a360-4f85-98dd-afec766ecc0a', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities benefit from the GPL's 'restriction' by finding ways to integrate GPL-licensed code into proprietary products or create proprietary forks, effectively leveraging open-source contributions without fully reciprocating, thus enabling their business models despite (or because of) the copyleft terms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Individuals and groups who contribute to open-source projects under GPL. From this reading's perspective, they are victims because their contributions, intended to build a shared commons, are siphoned off into proprietary forks or integrated into proprietary products without full reciprocity, undermining the goal of a truly open ecosystem.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, payer,
    moderate, generational, identity_locked, global).

% Organizations responsible for promoting and defending open-source principles and licenses. They administer the GPL and advocate for its interpretation, but from this reading's perspective, their efforts are constrained by interpretations that enable proprietary exploitation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, open_source_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Legal experts who analyze and interpret software licenses. Their interpretations can influence how the GPL is understood and enforced, contributing to or challenging the 'restriction' reading.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, legal_scholars_and_practitioners, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The GPL aims to coordinate contributions to a shared software commons by mandating reciprocity for derivative works.
% TRANSFER_FUNCTION: This reading posits a transfer of value (code, innovation, community effort) from commons contributors to proprietary software vendors, who leverage it for private gain by exploiting interpretations of the GPL that allow proprietary forks or integration without full reciprocity.
% ABSENT_VOICES: Users who are ultimately locked into proprietary ecosystems that benefit from open-source work without contributing back, and who might prefer a truly open and reciprocal software landscape.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, the dynamics of open-source contribution and proprietary integration would fundamentally shift. Proprietary vendors would have fewer legal constraints on using open-source code, potentially leading to a greater enclosure of the software commons, while commons contributors would need new mechanisms to protect their work.
% FOUNDING_PROBLEM: The original GPL was created to prevent the enclosure of software by proprietary interests, ensuring that software would remain free for users to run, study, modify, and distribute.
% FOUNDING_PROBLEM_CORROBORATION: While open-source advocates attest the problem is live and the GPL is a solution, this reading, supported by some critical legal analyses and observations of market behavior, argues that the GPL's 'restriction' aspect paradoxically enables proprietary exploitation, thus failing to fully solve the enclosure problem. This perspective is often found in critiques of 'open core' models or 'permissive' interpretations of copyleft.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the value siphoned from commons contributors to proprietary vendors. Suppression (0.8) indicates the legal and market mechanisms that make it difficult for commons contributors to prevent this exploitation. The low theater ratio (0.1) suggests the constraint operates as a functional legal mechanism, albeit one interpreted to enable extraction. The metrics reflect the impact on the 'victims' (commons contributors) and the 'beneficiaries' (proprietary vendors) as defined by this specific reading.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies in whether copyleft is seen as a mechanism for freedom and commons building (as in sibling readings) or as a restriction that, when interpreted in certain ways, enables proprietary exploitation. This reading highlights the latter, showing how the same legal text can be interpreted to produce vastly different structural outcomes for different parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software vendors are beneficiaries (low d) as this reading enables them to leverage open-source work for private gain. Commons contributors are targets/victims (high d) as their efforts are extracted without full reciprocity. Open-source foundations, while nominally agenda-setters, are constrained by this interpretation, making them indirect payers in terms of their mission.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gpl_kernel_reading_identity,
    'Is this constraint a genuine restriction on proprietary integration, or an interpretation that enables proprietary exploitation of the commons?',
    'Analysis of legal precedents and market outcomes: if proprietary forks consistently thrive while commons contributions diminish, it supports the ''exploitation'' interpretation of the restriction.',
    'If it''s primarily an exploitation mechanism, the classification as Snare is strengthened. If it''s a genuine restriction on proprietary models, the beneficiary/victim structure would need to be inverted, likely leading to a Tangled Rope or Snare for proprietary vendors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_kernel_reading_identity, conceptual, 'This constraint is the ''copyleft as restriction'' reading of the ''gpl_reciprocity_obligation'' kernel. Sibling readings (''copyleft_as_freedom_reading'', ''copyleft_as_commons_reading'') offer alternative interpretations of the GPL''s structural effects.').

omega_variable(
    reciprocity_interpretation_ambiguity,
    'How strictly is ''reciprocity'' defined and enforced in practice, and what constitutes ''integration'' that triggers the copyleft obligation?',
    'Judicial rulings on specific cases of GPL compliance and industry standard practices for linking and distribution.',
    'A loose interpretation of reciprocity or a narrow definition of integration would amplify the extraction from commons contributors, strengthening the Snare classification. A strict interpretation would reduce it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_interpretation_ambiguity, empirical, 'Ambiguity in the practical interpretation of GPL''s reciprocity and integration clauses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1989, 0.1).
narrative_ontology:measurement(gpl__tr_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1999, 0.1).
narrative_ontology:measurement(gpl__tr_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1989, 0.6).
narrative_ontology:measurement(gpl__be_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1999, 0.7).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2009, 0.8).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2019, 0.83).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1989, 0.5).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1999, 0.65).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2009, 0.75).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2019, 0.78).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel. This 'restriction' reading focuses on how copyleft can be interpreted to enable proprietary exploitation, contrasting with readings that emphasize freedom or commons building.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
