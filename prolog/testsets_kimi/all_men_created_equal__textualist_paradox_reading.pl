% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: Textualist Paradox of Founding Universalism
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The phrase 'all men are created equal' appears in founding-era documents
 *   as universalist language, yet it was applied in practice to a narrow
 *   18th-century social taxonomy. The textualist-paradox reading treats this
 *   gap not as a historical curiosity but as a structural bind: any
 *   interpretive methodology that claims fidelity to the literal text while
 *   restricting its application enters a performative contradiction. This
 *   constraint extracts methodological credibility from originalist jurists
 *   who are professionally committed to textual fidelity, while providing
 *   argumentative leverage to universalist advocates who can point to the
 *   words themselves. The kernel is the founding text; this is one of three
 *   readings, distinguished by its stabilization of the contradiction as a
 *   feature rather than resolving it via historical intent or expansive
 *   reinterpretation.
 *
 * KEY AGENTS:
 *   - originalist_jurists: Primary payer (institutional/generational/identity_locked) â bears the extraction of methodological credibility
 *   - universalist_advocates: Primary beneficiary (organized/biographical/mobile) â collects argumentative leverage from the contradiction
 *   - historically_excluded_groups: Excluded seat (powerless/generational/trapped) â supplies the empirical evidence of restricted application but is absent from originalist interpretive frameworks
 *   - constitutional_historians: Analytical observer (organized/generational/analytical) â documents the gap without aligning with either legal project
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.62).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.55).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist Paradox of Founding Universalism").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional/political").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '8e062799-e7f5-40cb-877a-c4387ed79d89').
narrative_ontology:cs_kernel_codification('8e062799-e7f5-40cb-877a-c4387ed79d89', fixed_text).
narrative_ontology:cs_authority_grounding('8e062799-e7f5-40cb-877a-c4387ed79d89', lineage).
narrative_ontology:cs_interpretation_layer_present('8e062799-e7f5-40cb-877a-c4387ed79d89').
narrative_ontology:cs_reading_relation('8e062799-e7f5-40cb-877a-c4387ed79d89', all_men_created_equal__originalist_reading, influences).
narrative_ontology:cs_reading_relation('8e062799-e7f5-40cb-877a-c4387ed79d89', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('8e062799-e7f5-40cb-877a-c4387ed79d89', foundational, universal_text_precludes_restricted_practice).
narrative_ontology:cs_axiom_status(universal_text_precludes_restricted_practice, holdable).
narrative_ontology:cs_axiom_grounding('8e062799-e7f5-40cb-877a-c4387ed79d89', universal_text_precludes_restricted_practice, conventional).
narrative_ontology:cs_axiom('8e062799-e7f5-40cb-877a-c4387ed79d89', secondary, contradiction_erodes_methodological_authority).
narrative_ontology:cs_axiom_status(contradiction_erodes_methodological_authority, holdable).
narrative_ontology:cs_axiom_grounding('8e062799-e7f5-40cb-877a-c4387ed79d89', contradiction_erodes_methodological_authority, instrumental).
narrative_ontology:cs_reference_frame('8e062799-e7f5-40cb-877a-c4387ed79d89', literal_universal_commitment).
narrative_ontology:cs_drift_state('8e062799-e7f5-40cb-877a-c4387ed79d89', contemporary_originalist_jurisprudence, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8e062799-e7f5-40cb-877a-c4387ed79d89', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, universalist_advocates).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Committed to interpreting the Constitution and founding documents according to their original public meaning. Are bound by the literal universality of phrases such as 'all men are created equal' yet seek to confine application to 18th-century social taxonomies. Must perform increasingly elaborate interpretive work to prevent the universal language from expanding equality protections beyond historically intended bounds. Their methodological credibility is strained by the visible gap between the words and the restricted outcomes they defend.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_jurists, payer,
    institutional, generational, identity_locked, national).

% Invoke the literal universal language of founding texts to press for expanded equality protections. Gain argumentative leverage when originalist methodologies are shown to conflict with their own textual premises. Collect institutional and rhetorical advantage when courts are forced to confront the gap between universal words and restricted historical practices.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_advocates, beneficiary,
    organized, biographical, mobile, national).

% Descendants of those excluded from the 'all men' formulation in the founding era. Their ancestors' exclusion is the historical evidence that the universal language was applied restrictively. Are not represented in originalist interpretive frameworks that claim to recover founding-era meaning, yet their history is the material that makes the contradiction visible.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, historically_excluded_groups, excluded,
    powerless, generational, trapped, national).

% Document the gap between founding-era universalist rhetoric and the actual practices of slavery, dispossession, and exclusion. Provide empirical evidence about what the founders did and wrote without being aligned with either the originalist or universalist legal project.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_historians, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, universalist_advocates).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Forces interpretive consistency between canonical text and applied practice; prevents constitutional interpreters from simultaneously claiming textual fidelity and restricting the scope of universal equality language without accounting for the logical gap. The universal text provides a shared anchor that coordinates equality claims across generations.
% TRANSFER_FUNCTION: Moves methodological credibility and institutional authority from originalist jurists to universalist advocates whenever the gap between universal founding language and restricted application is exposed. The currency is constitutional legitimacy and interpretive precedence.
% ABSENT_VOICES: Historically excluded populationsâenslaved persons, women, Indigenous peoplesâwhose exclusion in the founding era constitutes the restrictive application that creates the contradiction. They are absent from originalist interpretive frameworks that claim to recover founding-era meaning, yet their history is the material evidence of the paradox.
% DISAPPEARANCE_RATIONALE: If the paradox vanishedâeither because originalists abandoned textual fidelity or because the universal language was no longer treated as bindingâoriginalist constitutional methodology would lose its central tension or its central anchor. The current equilibrium of restricted textualism depends on the contradiction being actively managed; its disappearance would force a methodological reorganization toward historicism without textual constraint or toward open universalism.
% FOUNDING_PROBLEM: How to ground constitutional authority in fixed founding-era texts while accommodating social and moral change across generations, particularly when those texts contain universalist language that conflicts with the founders' own restricted practices.
% FOUNDING_PROBLEM_CORROBORATION: Universalist jurists and critical legal scholars attest that the founding problem is structurally unsolved and manifests as performative contradiction. Originalist jurists attest that the problem is solved by proper historical understanding that reveals restricted original meaning. Historians of slavery and republicanism, situated outside legal advocacy, corroborate that founders' practices sharply contradicted their universal language, documenting the gap without resolving it.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects the systematic transfer of credibility from originalists to universalists when the contradiction is invoked. Suppression at 0.55 is enforced primarily through professional identity: originalist jurists maintain textual fidelity as a core methodological commitment, which prevents them from simply abandoning the universal language. Theater_ratio at 0.60 captures the increasingly elaborate interpretive moves originalists must perform to restrict the scope of 'all men' without appearing to reject the text. Accessibility_collapse at 0.70 is high because, once inside a textualist frame, the fixed words cannot be rewritten; the only alternatives are to accept their universal force or abandon textualism altogether. Resistance at 0.45 reflects the active counter-mobilization of historical-intent arguments that seek to absorb the contradiction.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist seat, the arrangement appears as a methodological commitment to textual fidelity that creates manageable tension. From the universalist seat, the same structure is an argumentative engine that extracts credibility from originalism whenever the literal text is pressed against restricted outcomes. The engine computes this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist jurists sit near the full-target end: their institutional identity is fused to textual fidelity, yet their preferred outcomes require restricting the same text that binds them. Universalist advocates sit near the beneficiary end: they invoke the identical textual commitment without needing to restrict it, so the constraint subsidizes their arguments. Historically excluded groups are not direct parties to the transfer but their history constitutes the material evidence of the restricted application. The directionality derives from beneficiary/victim declarations plus the identity-locked exit of the originalist seat versus the mobile exit of the universalist seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it preserves both the genuine coordination function and the asymmetric extraction. The universal language genuinely coordinates equality claims across generations by providing a shared textual anchor that cannot be erased; without this coordination component, the constraint would be a pure snare on originalists. Conversely, the extraction from originalists is real and structural, not incidental; without acknowledging it, the constraint would be misread as a rope of shared meaning. The tangled-rope classification captures the hybrid: the same textual commitment that coordinates also extracts, and the extraction persists only because originalists actively enforce their textual fidelity upon themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contingency,
    'Is the performative contradiction an inherent structural feature of the founding text, or an artifact generated by the textualist-paradox reading?',
    'Cross-reading comparison: the originalist reading dissolves the contradiction via historical intent; the universalist reading dissolves it via expansive application; only this reading stabilizes the contradiction as a feature. Corpus-level analysis of whether the paradox reproduces across other kernel readings would indicate inherence.',
    'If the contradiction is reading-contingent, the constraint''s extractiveness is local to this interpretive frame and may not generalize. If it is inherent, the kernel itself is unstable across all readings and the constraint belongs to a broader family of textual paradoxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency, conceptual, 'Whether the paradox is inherent to the kernel or an artifact of this reading.').

omega_variable(
    delegitimization_mechanism,
    'Does exposure of the performative contradiction actually erode originalist institutional authority, or is the contradiction absorbed as a tolerable tension within originalist practice?',
    'Longitudinal analysis of originalist jurisprudential output and citation patterns: does invocation of the paradox correlate with methodological shift, defensive adaptation, or no measurable change in institutional authority?',
    'If originalism absorbs the tension, the constraint''s effective extraction is lower than the base metric suggests and the theater_ratio may be higher (performative management without structural change). If authority erodes, extraction is higher and may trend toward institutional piton as the method becomes inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegitimization_mechanism, empirical, 'Whether the paradox delegitimizes or is absorbed by originalism.').

omega_variable(
    kernel_instability_scope,
    'Does the instability exposed by this reading propagate to other fixed-text commitments in the constitutional kernel, or is it localized to equality jurisprudence?',
    'Examination of other universalist or aspirational phrases in founding documents (e.g., ''We the People'', ''perfect Union'', ''Blessings of Liberty'') for analogous gaps between literal text and restricted historical application.',
    'If propagation occurs, the constraint is part of a broader constraint family and the textualist-paradox reading may generalize into a systemic critique. If localized, the instability is specific to equality doctrine and does not threaten the broader originalist framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_instability_scope, conceptual, 'Scope of kernel instability beyond equality language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(textualist_paradox_tr_t0, all_men_created_equal__textualist_paradox_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(textualist_paradox_tr_t10, all_men_created_equal__textualist_paradox_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(textualist_paradox_tr_t20, all_men_created_equal__textualist_paradox_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(textualist_paradox_tr_t30, all_men_created_equal__textualist_paradox_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(textualist_paradox_tr_t40, all_men_created_equal__textualist_paradox_reading, theater_ratio, 40, 0.6).

% Extraction over time
narrative_ontology:measurement(textualist_paradox_be_t0, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(textualist_paradox_be_t10, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(textualist_paradox_be_t20, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(textualist_paradox_be_t30, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(textualist_paradox_be_t40, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 40, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(all_men_created_equal__textualist_paradox_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the all_men_created_equal kernel family. The originalist reading and universalist reading are structurally distinct constraints with different epsilon values and stakeholder configurations. This reading focuses on the performative contradiction rather than the scope of equality itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
