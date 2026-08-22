% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Social-Role Reading of Gendered Category Membership
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates the social-role reading of the gendered category
 *   membership kernel: gender category membership as grounded not in
 *   chromosomes or self-declaration but in sustained social performance
 *   recognized by others in ongoing interaction. Under this reading, trans
 *   women are conditionally included when they achieve durable recognition
 *   ('passing'), and the gatekeeping function is distributed across countless
 *   informal social interactions rather than housed in any single formal
 *   authority. This produces a genuinely ambiguous victim structure: both
 *   trans individuals who cannot achieve or sustain the performance and
 *   cisgender women who fail to perform normative femininity convincingly can
 *   be excluded by the same mechanism, even though their relationship to the
 *   category is otherwise entirely different. This is a distinct constraint
 *   from the biological_sex_reading (fixed at birth, no performance required,
 *   no conditional inclusion) and the gender_identity_reading (membership by
 *   declaration alone, no ongoing performance test) — those are separate
 *   stories with separate epsilon values, linked here via
 *   network.affects_constraints, not folded into this one.
 *
 * KEY AGENTS:
 *   - passing_trans_women: primary conditional beneficiary (moderate/constrained) — gains inclusion through continuous performance labor
 *   - non_passing_trans_women: primary excluded party (powerless/trapped) — cannot meet the performance standard regardless of effort
 *   - gender_nonconforming_cis_women: secondary victim (powerless/trapped) — subjected to a test never meant to apply to them
 *   - gatekeeping_social_networks: distributed agenda-setter (organized/arbitrage) — administers the standard informally with no accountability
 *   - cis_normative_institutions: institutional beneficiary (institutional/analytical) — adopts the standard for low-friction discretionary control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.42).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.55).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Social-Role Reading of Gendered Category Membership").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, 'fba42aca-88a8-474c-94cf-ed77400b877d').
narrative_ontology:cs_kernel_codification('fba42aca-88a8-474c-94cf-ed77400b877d', distributed).
narrative_ontology:cs_authority_grounding('fba42aca-88a8-474c-94cf-ed77400b877d', practice).
narrative_ontology:cs_interpretation_layer_present('fba42aca-88a8-474c-94cf-ed77400b877d').
narrative_ontology:cs_reading_relation('fba42aca-88a8-474c-94cf-ed77400b877d', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('fba42aca-88a8-474c-94cf-ed77400b877d', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('fba42aca-88a8-474c-94cf-ed77400b877d', foundational, membership_conditioned_on_sustained_recognition).
narrative_ontology:cs_axiom_status(membership_conditioned_on_sustained_recognition, holdable).
narrative_ontology:cs_axiom_grounding('fba42aca-88a8-474c-94cf-ed77400b877d', membership_conditioned_on_sustained_recognition, conventional).
narrative_ontology:cs_axiom('fba42aca-88a8-474c-94cf-ed77400b877d', secondary, performance_labor_is_legitimate_basis_for_inclusion).
narrative_ontology:cs_axiom_status(performance_labor_is_legitimate_basis_for_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('fba42aca-88a8-474c-94cf-ed77400b877d', performance_labor_is_legitimate_basis_for_inclusion, instrumental).
narrative_ontology:cs_reference_frame('fba42aca-88a8-474c-94cf-ed77400b877d', recognition_through_sustained_social_performance).
narrative_ontology:cs_drift_state('fba42aca-88a8-474c-94cf-ed77400b877d', contemporary_institutional_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fba42aca-88a8-474c-94cf-ed77400b877d', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, passing_trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, gatekeeping_social_networks).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_normative_institutions).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, non_passing_trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women_early_transition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, passing_trans_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sustain continuous performance of legible femininity — voice, dress, mannerism, social affect — that is read and re-certified by others in each interaction. When recognition is granted, they gain conditional inclusion in womanhood's social spaces (bathrooms, changing rooms, women's groups, dating pools). The inclusion is never final; it must be re-earned in every new social context, and any lapse (a misread voice on the phone, an ID mismatch, a moment of stress) can trigger exclusion. They pay in constant vigilance and performance labor for a membership that is always provisional.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, passing_trans_women, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, passing_trans_women, payer).

% Cannot achieve the sustained recognition the category demands — whether due to transition timing, economic inability to access voice training or surgery, or simply not being read as female by strangers. They are excluded from the category not by declaration but by the accumulated verdict of thousands of small social interactions they cannot control. Exit from the constraint means either continuing to seek recognition indefinitely or abandoning claim to the category altogether; neither is a real choice.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, non_passing_trans_women, payer,
    powerless, biographical, trapped, local).

% Cisgender women whose appearance, voice, or mannerisms do not perform normative femininity are also subjected to the same social recognition gate — misgendered, challenged for using women's spaces, or accused of being trans. They bear a cost from a category-membership test they never opted into and cannot exit, since their sex was never in question and yet the performance test is applied to them anyway.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women, payer,
    powerless, biographical, trapped, local).

% Informal networks — other women in a workplace, a gym, a friend group — collectively administer the recognition test through daily interaction: who gets invited, who gets corrected, who gets accepted without comment. They set no formal rule but jointly enforce the standard through countless small acts of inclusion or correction, and they can shift the bar without accountability to anyone it affects.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gatekeeping_social_networks, agenda_setter,
    organized, generational, arbitrage, local).

% Institutions that administer sex-segregated spaces (prisons, sports federations, some shelters) can adopt the social-role standard as a flexible, low-litigation-risk compromise: it lets them claim inclusivity while retaining discretionary gatekeeping through 'reasonable belief' and 'passing' criteria, avoiding the harder commitments either a strict biological or strict self-declaration standard would require of them.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_normative_institutions, beneficiary,
    institutional, generational, analytical, national).

% Are in the period where physical and social markers are ambiguous by definition — the performance the category demands has not yet stabilized. They experience the sharpest edge of the standard: excluded from women's spaces as not-yet-passing, but also not treated as men, leaving them structurally outside both categories during the period they most need social support.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women_early_transition, payer,
    powerless, biographical, trapped, local).

% Advocates for the biological-sex reading (who would exclude all trans women regardless of performance) and advocates for the gender-identity reading (who would include all trans women regardless of performance) both object to the social-role standard as illegitimate on principle — one because it concedes any inclusion at all, the other because it makes inclusion conditional on a discriminatory performance test. Neither is inside the rooms where the informal gatekeeping actually happens.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, biological_sex_and_gender_identity_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, non-bureaucratic way for existing sex-segregated social spaces and relationships to decide, interaction by interaction, who is treated as a woman, without requiring either a legal document check or an unfalsifiable identity declaration — it substitutes distributed social judgment for a centralized rule.
% TRANSFER_FUNCTION: Moves social standing and access to gendered spaces from those who cannot perform recognizable femininity (non-passing trans women, gender-nonconforming cis women, early-transition trans women) to those who can (passing trans women, normatively feminine cis women), and moves discretionary gatekeeping power to whoever is present in the interaction rather than to any accountable authority.
% ABSENT_VOICES: Non-passing trans women and gender-nonconforming cis women rarely get a say in how the informal standard is set — the standard is set by the aggregate behavior of gatekeeping networks who are not accountable to those they exclude. Biological-sex and gender-identity advocates are also absent from the rooms where the social test is actually administered day to day; the standard operates below the level of any forum where it could be formally contested.
% DISAPPEARANCE_RATIONALE: If the social-role standard vanished, gatekeeping practices in informal single-sex spaces would not disappear — they would likely default to either stricter biological gatekeeping or looser self-identification, both already live alternatives. Passing trans women who currently benefit from conditional inclusion would lose their main pathway to it under a strict biological reading, while gender-nonconforming cis women would be relieved of the passing test but might face new scrutiny under either alternative. Whether the world 'rearranges' or 'stays the same' depends on which sibling reading would fill the vacuum, which is itself contested between the parties.
% FOUNDING_PROBLEM: Sex-segregated social spaces long predate any legal or medical framework for trans identity; ordinary people needed some way to decide, in the absence of documentation, who belonged in a given space, and social recognition through appearance and behavior was the pre-existing default mechanism repurposed to handle trans inclusion questions.
% FOUNDING_PROBLEM_CORROBORATION: Sociological research on 'passing' and stigma (Goffman-derived literature, independent of any advocacy group) corroborates that social recognition has always been the operative mechanism in informal single-sex spaces, predating any formal trans-rights framework. Trans advocacy organizations dispute that this makes the standard legitimate going forward, arguing the founding problem (no formal alternative existed) is now dead because legal and medical frameworks exist; gatekeeping networks themselves assert the problem remains live because those frameworks do not reach into everyday informal interaction.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, contested).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low-to-moderate (0.42 at interval end) because the primary cost this reading imposes is performance labor and psychological vigilance rather than material transfer — closer to a coordination tax than outright extraction. Suppression sits at a moderate 0.55: there is no single enforcing authority, so no one entity can be resisted directly, but the aggregate effect of distributed informal gatekeeping is a real barrier that is hard to challenge precisely because it is diffuse. Theater ratio rises modestly (0.25 to 0.38) as institutions increasingly adopt 'passing' language as a compliance gesture that substitutes for harder policy commitments. Accessibility collapse is moderate (0.4): the standard leaves real routes to inclusion open (transition progress, social integration over time) unlike a hard biological bar, but those routes are not available to everyone equally. Resistance is comparatively high (0.6) because this reading is actively contested by advocates on both flanks of the kernel.
 *
 * PERSPECTIVAL GAP:
 *   From inside a gatekeeping social network, the standard looks like ordinary, low-stakes social judgment — 'we just go by how people present.' From a non-passing trans woman's seat, the identical mechanism computes as a trap with no formal appeal and no fixed criteria to meet. From a gender-nonconforming cis woman's seat, it computes as an unearned burden imposed by a dispute she is not party to. The engine should register these as different computed types from the same structural data, not as a single agreed-upon verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Passing trans women sit closest to the beneficiary end but carry an unusually large performance cost baked into that benefit — their directionality is not a clean subsidy, it is inclusion purchased with continuous labor, which is why they carry a secondary payer role. Non-passing trans women and early-transition trans women sit at the target end: trapped, powerless, bearing the exclusion the standard produces regardless of intent. Gender-nonconforming cis women are a genuinely distinct victim class — their directionality toward this constraint is high despite having no claim dispute at all, because the social-role test is applied to them as a side effect of policing trans inclusion. Gatekeeping networks and cis-normative institutions benefit from the discretion the standard preserves, without bearing its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — informal single-sex spaces needing some practical basis for inclusion decisions absent legal or medical documentation — plausibly remains partially live wherever formal frameworks do not reach (bathrooms, changing rooms, friend groups), which argues against treating this as pure mandatrophy. But the increasing theater ratio and the institutional adoption pattern (cis_normative_institutions using 'passing' language to avoid firmer commitments) suggest the standard is increasingly doing discretion-preservation work unrelated to its founding coordination function. This is not resolved cleanly to either 'still functional' or 'purely vestigial' — it is exactly the contested middle the founding_problem_status: contested captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_standard_or_disguised_biology,
    'Is the social-role recognition standard genuinely independent of biological markers, or does it function as a proxy measure for biological sex that is simply harder to name directly (voice pitch, bone structure, hairline being read as biological tells dressed up as ''performance'')?',
    'Compare exclusion patterns against post-medical-transition trans women whose biological markers have shifted substantially versus early-transition trans women whose performance is strong but biological markers have not shifted; if exclusion tracks biology more than performance, the reading collapses toward the biological_sex_reading in practice.',
    'If the standard is substantially a biology-proxy, this constraint and biological_sex_reading may not be as structurally distinct as claimed, undermining the ε-invariance claim that they are separate constraints with separate victim structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_standard_or_disguised_biology, conceptual, 'Whether social-role recognition is doing independent work or covertly re-encoding biological criteria.').

omega_variable(
    gatekeeping_network_accountability_gap,
    'Because the gatekeeping function is distributed across countless informal interactions rather than housed in one authority, is there any mechanism by which its aggregate standard could be contested or reformed, or is diffusion itself an unaccountability mechanism?',
    'Track whether organized advocacy (from either flank) has historically produced measurable shifts in aggregate informal recognition standards over multi-year periods, versus whether the standard is empirically static regardless of advocacy pressure.',
    'If the standard is genuinely unresponsive to organized pressure, the diffusion of the gatekeeping function functions as a suppression mechanism in itself, which would raise the effective suppression score independent of any single actor''s intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeping_network_accountability_gap, empirical, 'Whether distributed gatekeeping is reformable or is structurally insulated from contestation by its diffusion.').

omega_variable(
    cis_woman_victim_class_framing,
    'Is it structurally accurate to describe gender-nonconforming cis women as victims of THIS constraint, or are they victims of a separate, older constraint (gender-normativity policing of cis women generally) that merely shares a mechanism with this one?',
    'Determine whether gender-nonconforming cis women''s exclusion predates and is independent of the trans-inclusion question (i.e., would exist even absent any trans people in the social space) versus whether the specific form of scrutiny they face has intensified because of trans-inclusion disputes.',
    'If the mechanism is genuinely prior and independent, this story may be conflating two constraints under one victim list, which the ε-invariance principle would require decomposing into a separate ''gender-normativity policing of cis women'' story linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_woman_victim_class_framing, conceptual, 'Whether cis-woman exclusion is native to this constraint or borrowed from an adjacent, older one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__social_role_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__social_role_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__social_role_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__social_role_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__social_role_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__social_role_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__social_role_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__social_role_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__social_role_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__social_role_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__social_role_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__social_role_reading, suppression_requirement, 8, 0.5).
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
% This story is one of three sibling readings of the gendered_category_membership kernel. biological_sex_reading grounds membership in immutable birth markers (near-zero conditional inclusion, sharpest exclusion of trans individuals, no performance cost). gender_identity_reading grounds membership in self-declaration (near-total inclusion by declaration, minimal performance cost, but contested legitimacy from those who reject self-declaration as sufficient). This social_role_reading sits structurally between them: conditional inclusion via sustained recognition, moderate performance-cost extraction, and a uniquely ambiguous victim structure spanning both trans individuals and gender-nonconforming cis women. Each reading has its own ε, its own beneficiary/victim structure, and its own claimed type; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
