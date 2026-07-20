% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Equality Clause Scope
 *   domain: constitutional law / political philosophy / civil rights history
 *
 * SUMMARY:
 *   This constraint instantiates the restrictive originalist reading of the
 *   equality_clause_scope kernel: that equality within the 18th-century
 *   social contract framework applied only to propertied white males as
 *   political actors. The constraint operated through constitutional text,
 *   legal interpretation, and political philosophy to restrict legal and
 *   political standing to a narrow beneficiary class while extracting labor,
 *   liberty, and civic personhood from excluded groups. It is authored as a
 *   snare because the universalist equality language functions as cover for a
 *   racialized, gendered, and property-based extraction of political power;
 *   the coordination story (republican government among the virtuous)
 *   legitimates what active enforcement of slavery, coverture, and
 *   disenfranchisement maintains.
 *
 * KEY AGENTS:
 *   - propertied_white_males: Primary beneficiary and political actor class â holds franchise and legal equality, mobile within the system.
 *   - enslaved_persons: Primary target â total exclusion from personhood, labor extracted under state violence, trapped.
 *   - women: Primary target â exclusion from franchise and legal independence under coverture, trapped.
 *   - non_propertied_whites: Secondary target â partial exclusion from political equality, constrained exit through property acquisition.
 *   - indigenous_peoples: Primary target â excluded from political community and sovereignty, trapped.
 *   - state_judiciary: Agenda setter â interprets and enforces the narrow scope through constitutional doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.88).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.91).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.88).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, snare).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional law / political philosophy / civil rights history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '91c3c4b8-0eb3-4478-969d-17ab18bf3e7e').
narrative_ontology:cs_kernel_codification('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', fixed_text).
narrative_ontology:cs_authority_grounding('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', lineage).
narrative_ontology:cs_interpretation_layer_present('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e').
narrative_ontology:cs_reading_relation('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_reading_relation('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', foundational, original_equality_scope_limited_to_propertied_white_males).
narrative_ontology:cs_axiom_status(original_equality_scope_limited_to_propertied_white_males, holdable).
narrative_ontology:cs_axiom_grounding('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', original_equality_scope_limited_to_propertied_white_males, empirically_contingent).
narrative_ontology:cs_axiom('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', foundational, social_contract_requires_economic_independence_for_political_personhood).
narrative_ontology:cs_axiom_status(social_contract_requires_economic_independence_for_political_personhood, holdable).
narrative_ontology:cs_axiom_grounding('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', social_contract_requires_economic_independence_for_political_personhood, deontological).
narrative_ontology:cs_reference_frame('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', eighteenth_social_contract_compact).
narrative_ontology:cs_drift_state('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', post_reconstruction_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('91c3c4b8-0eb3-4478-969d-17ab18bf3e7e', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_whites).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive franchise, legal standing, and political equality within the republic; participate in representative government and commercial society as full civic persons; their political identity is constituted through the social contract framework that recognizes them as the sole bearers of reasoned consent.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_males, beneficiary,
    powerful, generational, mobile, national).

% Excluded from all legal equality claims and held in chattel slavery; labor, liberty, and kinship are extracted under state-sanctioned coercion; escape is legally barred and violently punished; the social contract framework explicitly denies them political personhood and treats them as property.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_persons, payer,
    powerless, immediate, trapped, national).

% Excluded from political equality and franchise; under coverture, legal identity is subsumed by male relatives; excluded from property ownership and contract in many jurisdictions; the social contract framework treats them as represented by male household heads rather than as independent political agents.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    powerless, biographical, trapped, national).

% Free but excluded from franchise and full political equality in many jurisdictions; lack of property disqualifies them from the social contract's benefits; serve as labor force and potential future beneficiaries if they acquire property, but remain politically subordinate.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_whites, payer,
    moderate, biographical, constrained, national).

% Excluded from the political community by the social contract framework; land and sovereignty systematically removed through treaty and violence; framed as outside the scope of equality and consent-based governance, with cultural and political alternatives suppressed.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, payer,
    powerless, generational, trapped, national).

% Interprets and enforces the constitutional framework, applying social contract theory to determine standing and rights; their decisions consistently restrict equality claims to the propertied white male class and invalidate claims from excluded groups.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, state_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_males).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates republican government and political order among the propertied white male class by establishing consent-based governance, property-qualified franchise, and legal equality within that restricted circle.
% TRANSFER_FUNCTION: Transfers political power, legal personhood, and civic standing from enslaved persons, women, non-propertied whites, and indigenous peoples to propertied white males, enforced through constitutional text and social contract theory.
% ABSENT_VOICES: Enslaved persons, women, and indigenous peoples are structurally excluded from the constitutional convention, franchise, and legal standing to challenge the narrow scope; abolitionist and feminist voices are marginalized in the interpretive community.
% DISAPPEARANCE_RATIONALE: If the equality clause were interpreted to include all persons rather than restricted to propertied white males, the franchise would expand immediately, slavery would lose its constitutional cover, coverture would dissolve, and the social order would reconstitute around universal political equality.
% FOUNDING_PROBLEM: Establishing legitimate republican government after revolutionary separation from Britain; determining who possesses the rational independence and property stake requisite for political participation and consent.
% FOUNDING_PROBLEM_CORROBORATION: Federalist Papers and constitutional convention records from the beneficiary class attest the problem. Outside the beneficiary set, abolitionists like Frederick Douglass and feminist conventions like Seneca Falls contested the framing, arguing the 'founding problem' was a cover for oligarchy and that universal equality was the true revolutionary principle.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.88) and suppression (0.91) are near-maximum because the constraint denied legal personhood to the majority of the population and enforced this denial through state violence, slave codes, and coverture. Theater ratio (0.35) is moderate: Lockean social contract theory was genuinely believed by beneficiaries, but its restrictive application required increasingly performative maintenance as contradictions became visible. Accessibility collapse (0.92) is very high because alternatives like universal suffrage or abolition were legally and violently suppressed. Resistance (0.78) is high due to slave revolts, abolitionism, and women's organizing, though structurally crushed. The measurement series tracks intensifying extraction and suppression alongside rising theater from 1790 to 1860 as the constraint hardened toward its antebellum peak.
 *
 * PERSPECTIVAL GAP:
 *   The propertied white male beneficiary seat experiences the constraint as the legitimate foundation of republican liberty and self-government; the enslaved, women, and indigenous payer seats experience it as a system of terror and exclusion justified by philosophy. The state judiciary seat experiences it as interpretive fidelity. These divergences are structurally encoded by the beneficiary/victim declarations and the differential exit options (mobile vs trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied_white_males are declared beneficiaries with mobile exit options, placing their directionality near the subsidy end. Enslaved_persons, women, and indigenous_peoples are declared victims with trapped exit, placing their directionality near full target. Non_propertied_whites are intermediate (constrained exit, partial exclusion). The state_judiciary administers the constraint but does not personally collect the extraction; their directionality is moderate but structurally aligned with beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive originalist reading prevents mandatrophy mislabeling by requiring us to ask: was the coordination function (republican order among propertied males) separable from the extraction (enslavement and dispossession of others)? If the republican order required the extraction to survive, the coordination story is cover and the constraint is snare. If republican order was genuinely achieved among the beneficiaries and merely coincided with extraction, it would be tangled_rope. The authored metrics (theater rising, suppression near-maximum) suggest the former: the constraint's persistence depended on coercion against the excluded, not on the autonomous preference of the excluded to remain outside the social contract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_hierarchy,
    'Is the 18th-century social contract framework a discovered natural law of political equality, or a constructed justification for propertied white male supremacy?',
    'Comparative historical analysis of social contract theory across cultures; examination of whether the property and race qualifications were logically derivable from the equality principle or historically contingent exclusions.',
    'If constructed, the constraint is a false summit mountain or snare rather than a rope; if natural law, the exclusion of non-propertied groups follows from the theory''s internal logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_hierarchy, conceptual, 'Natural law status of social contract equality framework').

omega_variable(
    coordination_among_beneficiaries,
    'Did the social contract framework solve a genuine collective action problem among propertied white males, or was their political cohesion entirely dependent on the exclusion and extraction from subordinate groups?',
    'Counterfactual analysis of whether republican government could have been stable among propertied white males without slave economy and patriarchal household structure; examination of post-emancipation political order.',
    'If genuine coordination existed independently, the constraint is tangled_rope; if cohesion required extraction, it is snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_among_beneficiaries, empirical, 'Whether beneficiary-class coordination was independent of extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal bars, state violence) or internalized (the excluded groups accepted the hierarchy as natural)?',
    'Examination of resistance frequency and slave narrative testimony; measurement of suppression persistence after legal barrier removal during Reconstruction.',
    'If internalized, effective suppression exceeds structural measure; if purely structural, constraint is more vulnerable to legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eq_restrict_orig_tr_t0, equality_clause_scope__restrictive_originalist, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eq_restrict_orig_tr_t10, equality_clause_scope__restrictive_originalist, theater_ratio, 10, 0.22).
narrative_ontology:measurement(eq_restrict_orig_tr_t20, equality_clause_scope__restrictive_originalist, theater_ratio, 20, 0.25).
narrative_ontology:measurement(eq_restrict_orig_tr_t30, equality_clause_scope__restrictive_originalist, theater_ratio, 30, 0.28).
narrative_ontology:measurement(eq_restrict_orig_tr_t40, equality_clause_scope__restrictive_originalist, theater_ratio, 40, 0.32).
narrative_ontology:measurement(eq_restrict_orig_tr_t50, equality_clause_scope__restrictive_originalist, theater_ratio, 50, 0.36).
narrative_ontology:measurement(eq_restrict_orig_tr_t60, equality_clause_scope__restrictive_originalist, theater_ratio, 60, 0.4).
narrative_ontology:measurement(eq_restrict_orig_tr_t70, equality_clause_scope__restrictive_originalist, theater_ratio, 70, 0.45).
narrative_ontology:measurement(eq_restrict_orig_tr_t80, equality_clause_scope__restrictive_originalist, theater_ratio, 80, 0.5).

% Extraction over time
narrative_ontology:measurement(eq_restrict_orig_be_t0, equality_clause_scope__restrictive_originalist, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(eq_restrict_orig_be_t10, equality_clause_scope__restrictive_originalist, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(eq_restrict_orig_be_t20, equality_clause_scope__restrictive_originalist, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(eq_restrict_orig_be_t30, equality_clause_scope__restrictive_originalist, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(eq_restrict_orig_be_t40, equality_clause_scope__restrictive_originalist, base_extractiveness, 40, 0.87).
narrative_ontology:measurement(eq_restrict_orig_be_t50, equality_clause_scope__restrictive_originalist, base_extractiveness, 50, 0.88).
narrative_ontology:measurement(eq_restrict_orig_be_t60, equality_clause_scope__restrictive_originalist, base_extractiveness, 60, 0.89).
narrative_ontology:measurement(eq_restrict_orig_be_t70, equality_clause_scope__restrictive_originalist, base_extractiveness, 70, 0.9).
narrative_ontology:measurement(eq_restrict_orig_be_t80, equality_clause_scope__restrictive_originalist, base_extractiveness, 80, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(eq_restrict_orig_su_t0, equality_clause_scope__restrictive_originalist, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(eq_restrict_orig_su_t10, equality_clause_scope__restrictive_originalist, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(eq_restrict_orig_su_t20, equality_clause_scope__restrictive_originalist, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(eq_restrict_orig_su_t30, equality_clause_scope__restrictive_originalist, suppression_requirement, 30, 0.86).
narrative_ontology:measurement(eq_restrict_orig_su_t40, equality_clause_scope__restrictive_originalist, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(eq_restrict_orig_su_t50, equality_clause_scope__restrictive_originalist, suppression_requirement, 50, 0.89).
narrative_ontology:measurement(eq_restrict_orig_su_t60, equality_clause_scope__restrictive_originalist, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(eq_restrict_orig_su_t70, equality_clause_scope__restrictive_originalist, suppression_requirement, 70, 0.91).
narrative_ontology:measurement(eq_restrict_orig_su_t80, equality_clause_scope__restrictive_originalist, suppression_requirement, 80, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equality_clause_scope kernel. Sibling readings (expansive_universalist, progressive_textualist) instantiate structurally distinct constraints from the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
