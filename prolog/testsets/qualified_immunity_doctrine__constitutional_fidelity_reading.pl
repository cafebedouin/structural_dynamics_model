% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity as Judicially Fabricated Doctrine Without Constitutional Authorization
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   Qualified immunity is a judicially created doctrine that shields law
 *   enforcement officers from civil liability under 42 U.S.C. § 1983 unless
 *   they violated a 'clearly established' constitutional right at the time of
 *   the violation. This reading frames the doctrine as illegitimate on
 *   constitutional grounds: it lacks textual authorization in the
 *   Constitution or § 1983, contradicts the statutory remedy Congress
 *   created, and represents a judicial usurpation of legislative power to
 *   define rights and remedies. The doctrine systematically denies victims of
 *   constitutional violations any legal recourse, while shielding officers
 *   from accountability regardless of the egregious­ness of the violation.
 *   Under this reading, the doctrine is a snare: it operates to extract legal
 *   immunity for officers while suppressing the constitutional remedy
 *   Congress authorized, and it persists through institutional inertia and
 *   judicial self-interest in preserving its own power, not through
 *   legitimate legal authority.
 *
 * KEY AGENTS:
 *   - constitution_fidelity_litigants — powerless victims of constitutional violations denied remedy by judicial doctrine
 *   - federal_judiciary — institutional agenda-setter that created and maintains the doctrine, beneficiary of expanded power to define constitutional accountability
 *   - law_enforcement_officers — organized beneficiaries who collect immunity from civil liability
 *   - congress — institutional observer, authorized § 1983 without mentioning qualified immunity
 *   - civil_rights_advocates — excluded voices that would object to the doctrine's legitimacy but lack standing in the judicial process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.68).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.71).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity as Judicially Fabricated Doctrine Without Constitutional Authorization").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'b7a9d06b-356d-421c-baac-c893730338d1').
narrative_ontology:cs_kernel_codification('b7a9d06b-356d-421c-baac-c893730338d1', fixed_text).
narrative_ontology:cs_authority_grounding('b7a9d06b-356d-421c-baac-c893730338d1', extraction).
narrative_ontology:cs_interpretation_layer_present('b7a9d06b-356d-421c-baac-c893730338d1').
narrative_ontology:cs_reading_relation('b7a9d06b-356d-421c-baac-c893730338d1', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7a9d06b-356d-421c-baac-c893730338d1', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('b7a9d06b-356d-421c-baac-c893730338d1', foundational, judicial_authority_requires_constitutional_text).
narrative_ontology:cs_axiom_status(judicial_authority_requires_constitutional_text, holdable).
narrative_ontology:cs_axiom_grounding('b7a9d06b-356d-421c-baac-c893730338d1', judicial_authority_requires_constitutional_text, deontological).
narrative_ontology:cs_axiom('b7a9d06b-356d-421c-baac-c893730338d1', foundational, section_1983_text_does_not_contain_qualified_immunity).
narrative_ontology:cs_axiom_status(section_1983_text_does_not_contain_qualified_immunity, holdable).
narrative_ontology:cs_axiom_grounding('b7a9d06b-356d-421c-baac-c893730338d1', section_1983_text_does_not_contain_qualified_immunity, empirically_contingent).
narrative_ontology:cs_reference_frame('b7a9d06b-356d-421c-baac-c893730338d1', constitutional_textual_fidelity).
narrative_ontology:cs_drift_state('b7a9d06b-356d-421c-baac-c893730338d1', contemporary_empirical_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7a9d06b-356d-421c-baac-c893730338d1', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary_institutional_power).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_violation_victims).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_plaintiffs).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (interval endpoint) because the doctrine systematically bars remedies for constitutional violations; the extraction is not quantified in money but in legal immunity and accountability-denial. Suppression is higher (0.71) because the doctrine's persistence requires active judicial maintenance: courts must continually apply the 'clearly established' test, reject cases as barred, and resist legislative reform. Theater ratio rises from 0.38 to 0.52 over the interval because the doctrine's stated justification (enabling vigorous law enforcement) increasingly diverges from its actual operation (providing categorical immunity regardless of constitutional magnitude). The measurement series tracks the growing mismatch between the doctrine's legitimacy narrative and its functional operation: as empirical research accumulates showing qualified immunity does not serve its stated purpose, the performative ratio rises. Resistance is high (0.78) because civil rights advocates, constitutional scholars, and victims mount continuous legal, legislative, and public challenges to the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary's position and the victim's position should compute dramatically differently. From the judiciary's seat, the doctrine is a necessary management tool that prevents courts from being paralyzed by frivolous § 1983 suits and allows law enforcement to operate effectively. From the victim's seat, the same structure is a categorical bar to remedy for constitutional violations. The federal judiciary benefits from the doctrine's preservation (expanded power to define accountability boundaries); law enforcement officers benefit from immunity; victims bear the extraction (denied remedy). The engine should compute the judiciary and officers as beneficiaries (low d) and victims as targets (high d → high χ). This reading asserts that the beneficiary framing itself is illegitimate because the doctrine lacks constitutional authorization.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims of constitutional violations are trapped (exit_options=trapped): they cannot leave the jurisdiction to escape the constitutional violation, and the doctrine blocks legal redress. Their directionality is at the target end (d near 1.0). Law enforcement officers hold institutional power and can exit the profession, but within the profession they benefit from the immunity the doctrine provides; their directionality is mixed (moderate d, perhaps 0.3-0.4, because they are protected beneficiaries but organized enough to have some exit). The federal judiciary has no real exit from its own creation; it benefits from the power the doctrine grants to define constitutional meaning without appellate review. Under this reading, the judiciary is the true institutional beneficiary (d near 0.0, beneficiary), even if law enforcement appears as the nominal beneficiary. No directionality override is necessary if the beneficiary/victim declarations are clear: victims=[constitutional_violation_victims, section_1983_plaintiffs], beneficiaries=[judiciary_institutional_power]. The engine will derive d from these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's founding problem (addressing litigation risk and enabling vigorous law enforcement) has been superseded by empirical evidence that it does not serve this purpose, yet the doctrine persists. This is a classic mandatrophy case: the mandate (protect officers from frivolous suits) is dead, but the constraint (immunity from liability) remains active. The constitutional fidelity reading asserts that even if the founding problem were still live, the doctrine would be illegitimate because it lacks constitutional authorization. Mandatrophy resolution is strong under this reading: the doctrine is illegitimate regardless of outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicially_fabricated_vs_implied_authority,
    'Does Article III of the Constitution, read in isolation, authorize judges to create immunity doctrines that bar statutory remedies like § 1983, or does such doctrine-creation exceed judicial power and invade legislative territory?',
    'Originalist and textualist constitutional analysis examining Article III''s text, the Framers'' understanding of judicial power, and the constitutional status of implied doctrines created without statutory or constitutional text. Compare to constitutional doctrines with explicit textual grounding (e.g., privileges and immunities, equal protection).',
    'If judicially fabricated authority is deemed ultra vires, the entire doctrine is illegitimate on constitutional grounds, not merely poorly reasoned. If judges have implied authority to create such doctrines, the reading''s core premise forecloses on its own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicially_fabricated_vs_implied_authority, conceptual, 'Whether the judiciary''s authority to create qualified immunity extends from constitutional text or is a pure judicial invention.').

omega_variable(
    statutory_construction_versus_judicial_override,
    'Does § 1983''s text (§ 1983 creates liability for anyone acting under color of law who deprives a person of constitutional rights) contain any exception for ''not clearly established'' rights, or has the judiciary engrafted an exception that contradicts the statutory text?',
    'Strict textual interpretation of § 1983 as written in 1871 and as Congress intended, compared to the ''clearly established law'' gloss the judiciary added. Legislative history examination to determine whether Congress contemplated qualified immunity when drafting § 1983.',
    'If the judiciary has overridden statutory text by creating an unauthorized exception, the doctrine violates the separation of powers and usurps legislative authority. The doctrine would be illegitimate as statutory interpretation. If Congress implicitly authorized the gloss, the reading''s core premise is weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_construction_versus_judicial_override, empirical, 'Whether § 1983 contains textual room for qualified immunity or whether the doctrine contradicts statutory text.').

omega_variable(
    institutional_power_expansion_beneficiary,
    'Who truly benefits from qualified immunity''s persistence: law enforcement (who collect immunity from accountability), or the judiciary (which expanded its own power to define constitutional boundaries and shield enforceers)?',
    'Examination of which institution''s structural position changed most over the doctrine''s history. If courts gained power to control constitutional meaning without accountability review, the beneficiary is the judiciary. If courts merely gave officers discretion, the beneficiary is law enforcement. Comparative analysis of how other democracies allocate accountability authority.',
    'If the judiciary is the true institutional beneficiary, the doctrine functions as self-serving power expansion, not legitimate policy. This reshapes the classification: a snare where the official beneficiary (law enforcement) masks the true beneficiary (judicial power). If law enforcement is the true beneficiary, the extraction target is clear and the doctrine remains a snare aimed at victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_power_expansion_beneficiary, conceptual, 'Whether qualified immunity primarily benefits individual officers or the judiciary''s institutional power.').

omega_variable(
    constitutional_fidelity_reading_versus_protective_scaffold_reading,
    'Is qualified immunity a judicially fabricated overreach (constitutional fidelity reading) or a necessary but time-limited protection enabling law enforcement without paralyzing fear of liability (protective scaffold reading)?',
    'Empirical comparison of jurisdictions with and without qualified immunity, measuring: (1) frivolous litigation rates, (2) officer morale and recruitment, (3) constitutional compliance, (4) victim remedy rates. Comparative study of other democracies'' accountability mechanisms. Temporal analysis: if officers'' fear of liability is genuine and measurable in jurisdictions without immunity, the scaffold reading gains force; if absent and officer performance remains high, the constitutional fidelity reading is supported.',
    'This omega documents the reading relation to the protective_scaffold_reading sibling. If the scaffold reading is correct, immunity is justified as temporary support for a legitimate function. If the constitutional fidelity reading is correct, immunity is illegitimate regardless of protective effects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_fidelity_reading_versus_protective_scaffold_reading, empirical, 'Kernel contest: constitutional fidelity vs. protective scaffold framing of the same doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(qual_tr_t5, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(qual_tr_t10, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(qual_tr_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(qual_tr_t20, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(qual_tr_t25, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(qual_be_t5, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(qual_be_t10, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(qual_be_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(qual_be_t20, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(qual_be_t25, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(qual_su_t5, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(qual_su_t10, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(qual_su_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(qual_su_t20, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(qual_su_t25, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).

% DUAL FORMULATION NOTE:
% Qualified immunity as doctrine decomposes into three structurally distinct constraint stories based on how the authority structure and legitimacy basis are framed. The constitutional_fidelity_reading treats the doctrine's legitimacy as the contested variable; the accountability_void_reading treats extraction as the contested variable; the protective_scaffold_reading treats necessity as the contested variable. Each reading assigns a different ε and beneficiary set based on its core premise. All three are linked as kernel readings of the same judicial doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
