% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__identity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Category Membership by Subjective Gender Identity (Self-ID)
 *   domain: social/political
 *
 * SUMMARY:
 *   Category membership for 'woman' (and related sex categories) determined
 *   solely by subjective gender identity through self-declaration, without
 *   medical or biological gatekeeping. This constraint story instantiates the
 *   identity_reading of the sex_gender_category kernel. Under this reading,
 *   trans women are included in the 'woman' category by their own
 *   declaration, which expands the set of those recognized as victims of
 *   misogyny but simultaneously dissolves the exclusive boundaries that
 *   underpinned sex-based protections for cis women. The constraint
 *   coordinates social and legal recognition for trans people while
 *   asymmetrically extracting bounded protections from cis women. It requires
 *   active social and institutional enforcement to maintain against competing
 *   readings (biology_reading and hybrid_reading).
 *
 * KEY AGENTS:
 *   - trans_women: Primary beneficiary and secondary payer (moderate/identity_locked) â gain recognition but bear social conflict and misogyny costs through categorical inclusion
 *   - cis_women: Primary payer and victim (organized/constrained) â lose exclusive sex-based protections and cannot exit the biologically fixed category
 *   - trans_inclusion_advocates: Agenda-setter (institutional/arbitrage) â design and enforce self-ID norms across jurisdictions
 *   - state_legal_systems: Agenda-setter (institutional/constrained) â administer statutory self-declaration bound by legislative mandate
 *   - medical_gatekeepers: Excluded voice (organized/constrained) â professional gatekeeping role eliminated under self-ID
 *   - gender_critical_feminists: Excluded voice (moderate/constrained) â object to boundary dissolution but silenced in institutional contexts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.62).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.7).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Category Membership by Subjective Gender Identity (Self-ID)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social/political").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '56054f6e-412a-4c63-82ce-5acad2efaea4').
narrative_ontology:cs_kernel_codification('56054f6e-412a-4c63-82ce-5acad2efaea4', distributed).
narrative_ontology:cs_authority_grounding('56054f6e-412a-4c63-82ce-5acad2efaea4', distributed).
narrative_ontology:cs_reading_relation('56054f6e-412a-4c63-82ce-5acad2efaea4', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('56054f6e-412a-4c63-82ce-5acad2efaea4', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('56054f6e-412a-4c63-82ce-5acad2efaea4', foundational, gender_self_determination_right).
narrative_ontology:cs_axiom_status(gender_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('56054f6e-412a-4c63-82ce-5acad2efaea4', gender_self_determination_right, deontological).
narrative_ontology:cs_axiom('56054f6e-412a-4c63-82ce-5acad2efaea4', foundational, medical_gatekeeping_unnecessary).
narrative_ontology:cs_axiom_status(medical_gatekeeping_unnecessary, holdable).
narrative_ontology:cs_axiom_grounding('56054f6e-412a-4c63-82ce-5acad2efaea4', medical_gatekeeping_unnecessary, empirically_contingent).
narrative_ontology:cs_reference_frame('56054f6e-412a-4c63-82ce-5acad2efaea4', gender_identity_authenticity).
narrative_ontology:cs_drift_state('56054f6e-412a-4c63-82ce-5acad2efaea4', contemporary_policy_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56054f6e-412a-4c63-82ce-5acad2efaea4', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, trans_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal and social inclusion in the category 'woman' through self-declaration, without medical gatekeeping. Obtain access to sex-protected spaces, sports categories, and legal documentation aligned with gender identity. Simultaneously bear costs of social conflict, backlash, and misogyny directed at them through the same categorical inclusion.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, trans_women, payer).

% Lose exclusive claim to sex-based protections and single-sex spaces as the category 'woman' expands to include anyone who self-declares. Bear the costs of boundary dissolution in shelters, prisons, sports, and data collection. Exit is impossible because their category membership is biologically fixed and the protective infrastructure they relied on is being repurposed.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women, payer,
    organized, generational, constrained, national).

% Design and enforce the self-identification framework through policy proposals, institutional training, and social norm enforcement. Set the interpretive standards for what constitutes valid identity claims and what constitutes transphobic exclusion. Move between jurisdictions and institutional contexts to advance the framework and defend it against competing readings.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_inclusion_advocates, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer legal sex classification through statutory self-declaration procedures. Process gender recognition certificates and statutory declarations. Bound by legislative mandate; exit from the policy framework requires political reversal or judicial intervention.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, state_legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Previously controlled access to category membership through psychiatric diagnosis and medical transition requirements. Under self-ID, their gatekeeping role is bypassed and their professional authority over gender classification is eliminated. Would argue for clinical assessment but are excluded from the policy conversation in self-ID jurisdictions.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, medical_gatekeepers, excluded,
    organized, biographical, constrained, national).

% Argue that sex-based protections require biological boundary maintenance and that self-ID erases the material reality of sex. Object to the constraint's redefinition of 'woman' but are increasingly excluded from institutional policy-making, academic discourse, and civil-society spaces through no-platforming and disciplinary norms.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables transgender people to obtain legal and social recognition of their gender identity without invasive medical gatekeeping, reducing administrative barriers and respecting individual self-determination.
% TRANSFER_FUNCTION: Moves category membership, legal documentation, and space access from biologically bounded criteria to self-declared criteria; transfers the costs of dissolved boundaries to cis women who lose sex-exclusive protections, shelters, sports categories, and crime statistics specificity.
% ABSENT_VOICES: Medical gatekeepers whose professional authority is eliminated by self-ID; gender-critical feminists who argue sex is immutable and who are excluded from institutional policy-making through no-platforming and disciplinary norms; cis women in vulnerable institutional settings (prisons, shelters, rehabilitation) who are underrepresented in advocacy discourse.
% DISAPPEARANCE_RATIONALE: If self-ID category assignment vanished overnight, legal sex classification would revert to biological or medical-gatekeeping criteria. Single-sex spaces would reconstitute biological boundaries. Sports categories would reorganize around physiology. The advocacy and institutional infrastructure built around unconditional self-declaration would lose its organizing principle, and conflict over space access would shift to different grounds.
% FOUNDING_PROBLEM: Transgender people faced exclusion from legal recognition and social participation due to expensive, stigmatizing, and inaccessible medical gatekeeping requirements for gender recognition.
% FOUNDING_PROBLEM_CORROBORATION: Medical and psychological professional bodies historically attested to the harms of gatekeeping. However, the current expansion from legal recognition to full categorical equivalence (spaces, sports, data) is primarily attested by advocacy organizations. Gender-critical feminist groups and some detransitioners from outside the beneficiary set dispute that the current model solves the founding problem without creating new exclusions. State equality bodies provide mixed corroboration but are often aligned with advocacy networks.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the constraint transfers sex-based protections and space access from a bounded biological category to a self-declared one, imposing material costs on cis women in shelters, prisons, sports, and data collection. Suppression (0.70) is high because the reading's persistence depends on active suppression of dissent: no-platforming, disciplinary action for misgendering, and institutional exclusion of gender-critical voices. Theater_ratio (0.40) reflects moderate performative enforcement â much institutional activity around pronouns and declarations is symbolic, while the material boundary dissolution is real. Accessibility_collapse (0.50) captures that alternative readings (biology, hybrid) remain thinkable but are increasingly institutionally inaccessible. Resistance (0.72) is high due to ongoing feminist opposition, legal challenges, and political mobilization against self-ID policies.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (trans inclusion advocates, state systems) experiences the constraint as progressive coordination expanding human rights. The payer seat (cis women) experiences it as the demolition of protective boundaries they depended on. The beneficiary seat (trans women) experiences it as recognition and inclusion, though with secondary costs from social backlash and misogyny. These divergences are structurally expected and computed by the engine from the role and exit data.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women are declared beneficiaries because the constraint grants them category membership and access (low d, subsidy). They are simultaneously declared victims because the same inclusion exposes them to misogyny through the category (high d element), yielding a dual-role that sits near symmetric. Cis women are declared victims (high d, amplified extraction) because the constraint removes their exclusive protections and forces them to absorb boundary dissolution costs without exit. Agenda-setters sit near the beneficiary end because they administer and collect legitimacy from the constraint's operation. Excluded voices are governed by the constraint's suppression machinery but do not collect or pay through the primary transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it genuinely coordinates recognition for trans people â there is a real problem (gatekeeping exclusion) that this arrangement addresses. However, it is not a pure Rope because the coordination is coupled with asymmetric extraction from cis women and requires active suppression of the biology and hybrid readings to persist. If the founding problem were entirely solved and only the extraction remained, it would drift toward Snare; if the extraction were shown to be illusory (no real harm to cis women), it would be Rope. The current evidence supports the hybrid classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_operational_content,
    'Does the category ''woman'' under self-ID retain enough operational content to perform the protective functions (shelter placement, sports fairness, health data) it was built for, or does it become an empty signifier?',
    'Comparative outcome analysis across jurisdictions: measure shelter safety incidents, sports fairness metrics, and epidemiological data validity under self-ID versus gatekeeping regimes.',
    'If empty, the constraint is primarily extractive (snare-like) using coordination language as cover; if substantive, the extraction is the necessary cost of inclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_operational_content, conceptual, 'Whether the category retains operational meaning under self-ID.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of dissent structural (institutional policies, legal penalties) or internalized (self-censorship by academics, clinicians, and civil servants)?',
    'Post-exit trajectory analysis: if dissenters continue to self-censor after leaving institutional environments, reclassify as partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates more like ideological capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    kernel_decomposition_validity,
    'Is sex_gender_category correctly decomposed into three distinct constraints, or do the readings collapse into one under political or empirical pressure?',
    'Observe whether jurisdictions adopting one reading show convergence toward another under pressure (e.g., self-ID jurisdictions reintroducing medical criteria).',
    'If readings collapse, the epsilon-invariance decomposition fails and the kernel should be modeled as a single constraint with high internal variance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_validity, conceptual, 'Whether the kernel decomposition is structurally stable.').

omega_variable(
    trans_women_dual_position,
    'Are trans women under self-ID primarily beneficiaries of category inclusion, or do they constitute a victim seat due to misogyny directed at them through the category?',
    'Disaggregate costs: measure whether harms to trans women arise from the category assignment itself (constraint-induced) or from external transphobia independent of classification rules.',
    'If constraint-induced, they should be classified as payer/beneficiary dual-role, raising the effective extraction for their seat; if external, they remain pure beneficiaries and extraction concentrates on cis women.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trans_women_dual_position, conceptual, 'Whether trans women bear costs from the constraint itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sex__tr_t4, sex_gender_category__identity_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__identity_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__identity_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__identity_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sex__be_t4, sex_gender_category__identity_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__identity_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__identity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__identity_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sex__su_t4, sex_gender_category__identity_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__identity_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__identity_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__identity_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sex_gender_category__identity_reading, biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sex_gender_category kernel, decomposed per epsilon-invariance. The biology_reading and hybrid_reading are structurally distinct constraints with different epsilon values, beneficiary/victim structures, and enforcement requirements. They compete for institutional adoption as sibling readings of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
