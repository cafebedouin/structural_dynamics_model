% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership Grounded in Sustained Social Performance and Recognition (Social Role Reading)
 *   domain: social_ontology/bioethics/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the social-role reading of the contested
 *   gendered-category-membership kernel: membership in gendered categories is
 *   grounded not in biology (sibling reading) or self-declared identity
 *   (sibling reading) but in sustained social performance recognized by
 *   others in everyday interaction — passing. Under this reading, trans women
 *   gain conditional inclusion in women's categories to the degree their
 *   performance is read as convincing by peers and institutions; the
 *   gatekeeping is not centralized in a single rule but distributed across
 *   countless informal recognition events (bathrooms, sports leagues,
 *   shelters, everyday address). This produces a distinctive and ambiguous
 *   victim structure: both non-passing trans women AND gender-nonconforming
 *   cis women can fail the same performance test and be excluded from
 *   women's-category membership, while trans women who perform convincingly
 *   and cis women who perform conventionally both benefit. This is a
 *   structurally different constraint from the biological-sex reading (fixed,
 *   binary, non-performative) and the identity reading (self-declared, not
 *   contingent on others' recognition) — it has its own ε, its own
 *   beneficiary/victim structure, and its own classification, linked to its
 *   siblings only via network edges, per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - passing_trans_women: conditional beneficiary who bears continuous performance cost
 *   - non_passing_trans_women: excluded regardless of identity or legal status
 *   - gender_nonconforming_cis_women: excluded despite biological/legal cis status
 *   - gatekeeping_social_institutions: administer the de facto recognition standard without bearing its costs
 *   - cis_conforming_individuals: pass effortlessly, benefit from low-friction default membership
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.42).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.55).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership Grounded in Sustained Social Performance and Recognition (Social Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/bioethics/political_philosophy").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '9ee48157-1f6a-4167-93cb-641e0026b9b4').
narrative_ontology:cs_kernel_codification('9ee48157-1f6a-4167-93cb-641e0026b9b4', distributed).
narrative_ontology:cs_authority_grounding('9ee48157-1f6a-4167-93cb-641e0026b9b4', practice).
narrative_ontology:cs_interpretation_layer_present('9ee48157-1f6a-4167-93cb-641e0026b9b4').
narrative_ontology:cs_reading_relation('9ee48157-1f6a-4167-93cb-641e0026b9b4', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ee48157-1f6a-4167-93cb-641e0026b9b4', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('9ee48157-1f6a-4167-93cb-641e0026b9b4', foundational, membership_constituted_by_recognized_performance).
narrative_ontology:cs_axiom_status(membership_constituted_by_recognized_performance, holdable).
narrative_ontology:cs_axiom_grounding('9ee48157-1f6a-4167-93cb-641e0026b9b4', membership_constituted_by_recognized_performance, conventional).
narrative_ontology:cs_axiom('9ee48157-1f6a-4167-93cb-641e0026b9b4', secondary, recognition_is_necessarily_social_not_unilateral).
narrative_ontology:cs_axiom_status(recognition_is_necessarily_social_not_unilateral, holdable).
narrative_ontology:cs_axiom_grounding('9ee48157-1f6a-4167-93cb-641e0026b9b4', recognition_is_necessarily_social_not_unilateral, conventional).
narrative_ontology:cs_reference_frame('9ee48157-1f6a-4167-93cb-641e0026b9b4', tacit_recognition_convention).
narrative_ontology:cs_drift_state('9ee48157-1f6a-4167-93cb-641e0026b9b4', contemporary_gender_politics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ee48157-1f6a-4167-93cb-641e0026b9b4', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_conforming_individuals).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, gatekeeping_social_institutions).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, passing_trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, non_passing_trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_cis_men).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, passing_trans_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Achieve conditional inclusion in women's social spaces, categories, and recognition by sustaining a continuous performance of legible femininity — voice, dress, mannerism, social affect — that others read as convincing. Inclusion is real but revocable: a single moment of non-recognition (a slip in voice, disclosure of trans status, an unconvincing performance) can retract standing that was never permanently secured. The performance cost (money, vigilance, time, psychological labor of never relaxing) is continuous and falls only on them, not on cis women.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, passing_trans_women, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, passing_trans_women, payer).

% Cannot produce a performance that peers, institutions, or strangers recognize as sufficiently convincing, and are excluded from women's category membership regardless of internal identity or legal status. Their exclusion is enforced informally through denied recognition in bathrooms, changing rooms, sports leagues, social groups, and everyday address rather than through a single formal rule, making it diffuse, deniable, and hard to contest.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, non_passing_trans_women, payer,
    powerless, biographical, trapped, local).

% Are cis women by any biological or legal measure but fail the social-performance test — through appearance, voice, dress, or affect read as insufficiently feminine — and are subjected to the same recognition-based gatekeeping mechanism: misgendering, exclusion from women's spaces, suspicion, and demands to prove membership. The performance standard extracts from them even though they were never its intended target.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women, payer,
    powerless, biographical, trapped, local).

% Face parallel but asymmetric costs: failure to perform legible masculinity attracts ridicule, exclusion, and social sanction under the same recognition logic, though with different stakes (ostracism rather than exclusion from protected spaces). Included here for completeness of the performance-gatekeeping mechanism, though the story's primary victim analysis centers women's-category access.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_cis_men, payer,
    powerless, biographical, trapped, local).

% Sports leagues, changing-room operators, women's shelters, some social and professional women's organizations, and informal peer networks administer the recognition test in practice — setting the de facto bar for what counts as sufficiently convincing performance, adjudicating edge cases, and bearing no direct cost themselves from however the line is drawn. They can tighten or loosen the standard institutionally without personally absorbing the consequences either way.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gatekeeping_social_institutions, agenda_setter,
    institutional, generational, mobile, national).

% Pass the recognition test effortlessly by default, without performing anything they would recognize as performance, and never encounter the gatekeeping mechanism as a cost. They benefit from a stable, low-friction default membership that non-conforming people of every category must actively work to approximate.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_conforming_individuals, beneficiary,
    moderate, biographical, arbitrage, local).

% Include voices on multiple sides — some arguing the social-role reading itself performs an oppressive femininity standard that harms all women, others arguing it offers workable inclusion pending fuller social change. Both critiques are largely absent from the informal, decentralized enforcement contexts (a locker room, a shelter intake, a sports league committee) where the actual gatekeeping decisions get made moment to moment.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, feminist_theorists_and_activists, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, non-bureaucratic way to sort ambiguous cases in everyday social contexts (who belongs in this space, this category, this address) without requiring legal documentation, medical verification, or a tribunal — relying instead on the same tacit recognition cues humans already use to navigate social categories generally.
% TRANSFER_FUNCTION: Moves social standing, access to gendered spaces, and freedom from continuous vigilance from those who fail the performance test (regardless of trans or cis status) to those who pass it, and moves the cost of maintaining passing status onto trans women specifically as continuous labor rather than a one-time achievement.
% ABSENT_VOICES: Non-passing trans women and gender-nonconforming cis women are rarely consulted when informal institutions set or adjust the de facto recognition bar; the standard is set by aggregate perception, not by negotiation with those it excludes. Feminist critics who reject performance-based inclusion as reinstating a beauty/femininity hierarchy are also largely absent from the on-the-ground moments where gatekeeping actually happens.
% DISAPPEARANCE_RATIONALE: If social-performance-based recognition vanished as the operative standard overnight, non-passing trans women and gender-nonconforming cis women would gain uncontested access to categories currently denied them — a real rearrangement. But institutions would need to replace it with some other sorting mechanism (self-declaration, legal documentation, biological criteria), each of which reintroduces its own gatekeeping logic; parties dispute whether the underlying problem (how to sort ambiguous membership claims) would actually disappear or merely relocate.
% FOUNDING_PROBLEM: Social groups have long needed a low-cost, non-bureaucratic way to sort category membership in everyday interaction (who uses this space, who is addressed which way) without invoking formal documentation or biology in every single encounter; social performance and recognition filled that role tacitly, long before contested gender categories became a live political question.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists studying gender performance (outside any advocacy position on trans inclusion) attest the tacit-recognition mechanism predates and operates independently of the current political dispute; however, both trans-rights advocates and gender-critical feminists — otherwise opposed — separately attest that its current operation functions as a political gatekeeping tool rather than a neutral sorting convenience, which is a rare point of external corroboration from opposing outside seats.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, contested).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).
:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.42 at interval end) because the cost is performance labor and revocable social standing, not material dispossession — but it is real and continuous, distinguishing it from a mountain. Suppression (0.55) reflects that the mechanism operates through diffuse, decentralized social sanction rather than formal coercive enforcement, making it harder to resist or appeal than a codified rule, yet less totalizing than an institutional gate. Theater ratio (0.48) is comparatively high because a substantial share of what functions as 'coordination' — informal social sorting of who belongs where — has drifted into policing gender conformity as an end in itself, well beyond whatever functional need originally justified tacit recognition-based sorting. Resistance (0.68) is high because contemporary gender politics actively contests this reading from multiple directions (trans-inclusive critics reject performance gatekeeping as regressive; gender-critical critics reject it as insufficiently grounded in biology), so the constraint faces sustained challenge rather than quiet acceptance.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a passing trans woman, the constraint can appear as functioning, if costly, inclusion — a real coordination achievement under difficult conditions. From the seat of a non-passing trans woman or a gender-nonconforming cis woman, the identical mechanism appears as pure exclusion with no coordination benefit at all, since they receive none of its purported benefits and bear only its costs. The engine computing divergent per-seat types from this same structural data is the intended signal, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Passing trans women and gender-conforming cis people sit near the beneficiary end: they receive category access and social ease respectively, though the former pays continuously to sustain it (hence the dual role). Non-passing trans women and gender-nonconforming cis women sit near the full-target end: trapped exit options because there is no alternative social category that would grant them the recognition they are denied, and the exclusion operates through everyday interaction rather than a rule they could appeal. Gatekeeping institutions sit as agenda-setters with mobile exit — they administer the standard but are not personally exposed to the consequences of where they set the bar.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a low-cost tacit sorting mechanism for ambiguous social categories — plausibly predates and is narrower than the constraint's current politicized operation. Corroboration from both trans-rights advocates and gender-critical feminists (opposing camps, external to the mechanism's own defenders) that the current operation functions as active political gatekeeping rather than neutral convenience is a meaningful signal against treating this as settled coordination. The tangled_rope classification (rather than snare) is warranted by genuine residual coordination value in low-stakes, low-conflict everyday interactions, but the requires_active_enforcement flag and dual beneficiary/victim structure register that this coordination now rides substantial and contested extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_standard_source_ambiguity,
    'Is the social-performance recognition standard a naturally emergent, low-cost sorting convention, or is it a constructed and actively defended standard whose current strictness serves the interests of institutions that administer gendered spaces?',
    'Cross-cultural and historical comparison of how strictly performance-based recognition has been enforced across societies and eras; if strictness tracks institutional interest (e.g., liability concerns, membership control) rather than any functional sorting need, the constructed reading is favored.',
    'If constructed and institutionally self-serving, the tangled_rope classification undersells the extraction and the constraint drifts toward snare; if genuinely emergent and functionally minimal, closer to a scaffold pending broader social/legal resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_standard_source_ambiguity, conceptual, 'Whether the performance standard is emergent convention or constructed institutional gatekeeping.').

omega_variable(
    victim_symmetry_ambiguity,
    'Are trans women and gender-nonconforming cis women genuinely symmetric victims of the same mechanism, or does one group bear categorically worse consequences (e.g., violence risk, legal jeopardy) that the shared ''fails the performance test'' framing obscures?',
    'Comparative outcome data: violence rates, legal exposure, and social sanction severity for non-passing trans women versus gender-nonconforming cis women when excluded from the same spaces.',
    'If outcomes are starkly asymmetric, the story''s flat victim list understates a hierarchy of harm within the same mechanism, which would refine but not overturn the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_symmetry_ambiguity, empirical, 'Whether victim groups bear comparable or asymmetric consequences from the same exclusion mechanism.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the three sibling readings (biological, identity, social-role) of gendered category membership are mutually incompatible as a SINGLE operative standard, which reading a given institution or interaction actually applies is often undetermined until a specific dispute forces the question — meaning many real-world interactions may be governed by an unstable mixture rather than a clean instance of any one reading.',
    'Discourse analysis of how institutions justify specific admission/exclusion decisions — do they cite biology, self-declaration, or passing? Consistency of citation would indicate a stable reading; inconsistency would indicate ad hoc mixture.',
    'If institutions apply readings inconsistently case-by-case, this story''s clean social-role framing is an idealization of a messier empirical reality, and the network edges to sibling readings should be read as describing simultaneous competing pressures on the same institution rather than cleanly separated domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether real institutions apply a single stable reading or an unstable mixture of the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__social_role_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__social_role_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__social_role_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__social_role_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__social_role_reading, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__social_role_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__social_role_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__social_role_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__social_role_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__social_role_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__social_role_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__social_role_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__social_role_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__social_role_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(gend_su_t24, gendered_category_membership__social_role_reading, suppression_requirement, 24, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__social_role_reading, 0.08).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gendered_category_membership kernel. The biological_sex_reading produces a categorical, non-performative exclusion structure (trans women excluded regardless of performance or identity); the gender_identity_reading produces unconditional inclusion based on self-declaration (no performance test, no third-party recognition gate); this social_role_reading produces conditional, performance-contingent inclusion with a distributed, informal gatekeeping mechanism and an ambiguous victim set spanning both trans and cis populations. Each has a distinct ε, distinct beneficiary/victim structure, and distinct classification — they are not the same constraint measured differently, per the ε-invariance principle, and are linked here only as structurally related siblings within one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
