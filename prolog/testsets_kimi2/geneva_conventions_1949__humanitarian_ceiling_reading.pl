% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions Humanitarian Ceiling Reading
 *   domain: international law / armed conflict / political philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the humanitarian_ceiling_reading of the
 *   geneva_conventions_1949 kernel: the interpretation that the Conventions
 *   establish absolute, non-derogable humanitarian minimums constraining
 *   state violence regardless of adversary compliance or reciprocity. The
 *   reading suppresses security maximization rationales, extends expansive
 *   protections to civilians and detainees, and requires state militaries to
 *   bear an asymmetric compliance burden even when facing irregular forces
 *   who systematically violate the laws of war. Sibling readings â
 *   conditional_reciprocity_reading and security_maximization_reading â are
 *   structurally excluded from this normative framework.
 *
 * KEY AGENTS:
 *   - State militaries: primary target (institutional/constrained) â bear operational extraction and asymmetric compliance costs
 *   - Protected persons: primary beneficiary (powerless/trapped) â receive non-reciprocal protections
 *   - International criminal justice system: agenda setter (institutional/analytical) â administers enforcement and interpretation
 *   - Security maximization advocates: excluded voice (organized/constrained) â structurally suppressed by the reading
 *   - Conditional reciprocity proponents: excluded voice (organized/constrained) â legally excluded from the operative framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.72).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.85).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions Humanitarian Ceiling Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international law / armed conflict / political philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, 'b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78').
narrative_ontology:cs_kernel_codification('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', formalized).
narrative_ontology:cs_authority_grounding('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', lineage).
narrative_ontology:cs_interpretation_layer_present('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78').
narrative_ontology:cs_reading_relation('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', foundational, humanitarian_obligations_non_reciprocal).
narrative_ontology:cs_axiom_status(humanitarian_obligations_non_reciprocal, holdable).
narrative_ontology:cs_axiom_grounding('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', humanitarian_obligations_non_reciprocal, deontological).
narrative_ontology:cs_axiom('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', foundational, security_necessity_subordinate_to_humanitarian_minimums).
narrative_ontology:cs_axiom_status(security_necessity_subordinate_to_humanitarian_minimums, holdable).
narrative_ontology:cs_axiom_grounding('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', security_necessity_subordinate_to_humanitarian_minimums, deontological).
narrative_ontology:cs_reference_frame('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', absolute_humanitarian_minimums_framework).
narrative_ontology:cs_drift_state('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', contemporary_asymmetric_conflict_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b0bbc8fd-c40d-4bc2-992f-aa5cd9a99c78', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear asymmetric operational burdens including positive obligations to distinguish combatants from civilians, maintain detainee protections, and refrain from tactics even when adversaries are irregular forces who do not reciprocate. Security rationales and claims of operational necessity are structurally subordinated to humanitarian minimums. Exit is constrained by treaty ratification, customary international law status, and domestic incorporation of the conventions.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, immediate, constrained, global).

% Civilians, detainees, and irregular combatants who receive legal protections against torture, indiscriminate attack, and inhumane treatment regardless of whether their adversary or captor complies with reciprocity obligations. They cannot opt out of the protection framework but benefit from its existence.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons, beneficiary,
    powerless, immediate, trapped, global).

% Administers the humanitarian ceiling through war crimes prosecutions, treaty interpretation, and jurisprudence that rejects security maximization and reciprocity as legal defenses. Sets the enforceable meaning of absolute minimums and invests institutional authority in maintaining the reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_justice_system, agenda_setter,
    institutional, generational, analytical, global).

% Argue that operational necessity and asymmetric threats require suspending or degrading humanitarian constraints. Their position is structurally suppressed within the humanitarian ceiling reading, which treats security rationales as legally inadmissible to override core protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_maximization_advocates, excluded,
    organized, biographical, constrained, global).

% Hold that humanitarian obligations apply fully only when adversaries comply and that non-compliance by irregular forces permits proportional degradation of protections. The humanitarian ceiling reading explicitly rejects this framework, rendering their position legally excluded from the operative normative structure.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, conditional_reciprocity_proponents, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the conduct of hostilities among states and non-state actors to establish minimum humane standards in war, reducing indiscriminate violence against civilians, detainees, and hors de combat combatants through a shared legal baseline that functions regardless of adversary compliance.
% TRANSFER_FUNCTION: Transfers operational discretion, tactical flexibility, and security latitude from state militaries to protected persons in armed conflict, requiring state forces to bear the compliance costs regardless of adversary reciprocity or operational inconvenience.
% ABSENT_VOICES: Security maximization advocates who argue operational necessity should override humanitarian constraints, and conditional reciprocity proponents who hold that protections apply only to compliant adversaries. Both are structurally marginalized within the humanitarian ceiling framework.
% DISAPPEARANCE_RATIONALE: If the humanitarian ceiling vanished, state militaries would regain broad discretion to invoke operational necessity and reciprocity to degrade protections; civilian and detainee safeguards would contract to whatever adversary behavior permitted; the architecture of international criminal law would lose its normative foundation.
% FOUNDING_PROBLEM: The unrestrained brutality of early twentieth-century total war, including indiscriminate aerial bombing, torture of prisoners, and collective punishment, which demonstrated that reciprocity-based and security-maximizing frameworks failed to limit suffering.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and humanitarian organizations attest the founding problem was real; however, state security agencies and some military legal scholars attest the problem has been substantially addressed for interstate wars while persisting in asymmetric conflicts, and that the humanitarian ceiling reading now extracts excessive operational costs without corresponding reciprocal restraint from irregular adversaries.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint removes operational discretion from state militaries without reciprocity, forcing them to absorb tactical costs. Suppression is higher (0.85) because the reading structurally excludes security and reciprocity rationales from legal admissibility. Theater ratio is moderate-high (0.55): legal compliance rituals and treaty signatures abound, but battlefield conduct frequently departs from the absolute standard, generating performative adherence. Accessibility collapse is very high (0.80) because once inside the IHL framework, total war and reciprocity-based degradation become legally and morally unthinkable. Resistance is high (0.70) from state militaries and security actors who continually push for operational necessity exceptions.
 *
 * PERSPECTIVAL GAP:
 *   The protected-person seat should compute toward rope-like coordination: they receive survival protections without bearing compliance costs. The state-military seat should compute toward heavy extraction or snare-like coercion: they pay regardless of adversary conduct, with exit blocked by treaty architecture and customary law. The divergence is structurally wide because the same legal arrangement coordinates protection for the powerless while extracting operational discretion from the institutional actor.
 *
 * DIRECTIONALITY LOGIC:
 *   Protected persons sit near the full-beneficiary end: the constraint subsidizes their survival and bodily integrity by forcibly subordinating military utility. State militaries sit near the full-target end: they bear operational costs, foregone tactical options, and legal exposure. The international criminal justice system sits at low directionality because it draws authority from administering the constraint rather than paying its costs. Excluded voices are positioned outside the directionality derivation because their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â limiting total war brutality â is contested as to whether it remains live in asymmetric conflict. The humanitarian ceiling reading prevents mislabeling by maintaining a genuine coordination function (civilian protection, detainee safeguards) alongside asymmetric extraction (military operational burden without reciprocity). If the coordination function were merely cover, the reading would be a snare; if the extraction were merely the cost of coordination, it would be a rope. The tangled rope classification captures the coexistence of both. Mandatrophy is not declared resolved because security actors argue the problem has mutated while the constraint has not, whereas humanitarian actors argue the problem persists precisely because the constraint is violated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_ceiling_kernel_location,
    'Is the absolute humanitarian ceiling a structurally stable reading of the 1949 Geneva kernel, or does the kernel''s text and negotiating history more naturally accommodate conditional reciprocity?',
    'Historical treaty negotiation analysis, travaux prÃ©paratoires review, and subsequent state practice assessment to determine whether the absolute reading is an interpretation or a normative innovation.',
    'If the kernel naturally supports reciprocity, the humanitarian ceiling reading extracts compliance costs beyond what the underlying commitment system structurally supports; if the kernel supports absolutism, the conditional reciprocity reading is the extraction mechanism allowing states to escape obligations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_ceiling_kernel_location, conceptual, 'Structural location of the humanitarian ceiling reading within the contested Geneva kernel').

omega_variable(
    irregular_combatant_protection_balance,
    'Does extending full humanitarian protections to irregular combatants who systematically violate the laws of war generate net coordination benefit or net extraction cost?',
    'Empirical assessment of conflict outcomes, detainee treatment rates, and compliance cascades across irregular armed groups under varying levels of reciprocal restraint.',
    'If net extraction, the humanitarian ceiling reading functions as a tangled rope with significant asymmetric transfer to non-compliant adversaries; if net coordination, the asymmetric burden is the necessary price of maintaining the normative framework''s integrity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irregular_combatant_protection_balance, empirical, 'Coordination-extraction balance of irregular combatant protections').

omega_variable(
    enforcement_hardening_or_theater,
    'Does the rising suppression requirement reflect genuine enforcement hardening, or increased performative compliance without behavioral change?',
    'Comparative analysis of war crime prosecution rates, military training curriculums, and battlefield conduct data across the measurement interval.',
    'If theater dominates, the constraint''s effective extractiveness is lower than measured and the classification may drift toward piton; if genuine hardening, the tangled rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_hardening_or_theater, empirical, 'Whether rising suppression reflects enforcement or performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 45, 0.46).
narrative_ontology:measurement(gene_tr_t60, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 60, 0.51).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 75, 0.55).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 45, 0.66).
narrative_ontology:measurement(gene_be_t60, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 45, 0.74).
narrative_ontology:measurement(gene_su_t60, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 75, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
