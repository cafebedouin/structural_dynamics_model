% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Originalist Reading of the U.S. Constitution (1787)
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   This constraint story isolates the originalist reading of the U.S.
 *   Constitution (kernel_id: us_constitution_1787). Under this reading,
 *   constitutional meaning was fixed at ratification and remains binding
 *   unless altered by formal amendment. The constraint coordinates a
 *   community of jurists and scholars around a historically bounded
 *   interpretive method while asymmetrically extracting from modern rights
 *   claimants and non-originalist scholars whose claims and methods are
 *   delegitimized. Sibling readings (living constitutionalism, positivist
 *   textualism) are structurally distinct and recorded as separate
 *   constraints.
 *
 * KEY AGENTS:
 *   - originalist_jurists (institutional/analytical): Agenda-setter and beneficiary â enforces the interpretive method and derives professional authority from it.
 *   - modern_rights_claimants (organized/constrained): Primary payer â bears the cost of having constitutional claims excluded for lack of 1787 provenance.
 *   - living_constitutionalist_scholars (institutional/mobile): Secondary payer â methodology marginalized, institutional influence reduced.
 *   - structural_conservatives (powerful/mobile): Beneficiary â gains constitutional inertia and blockage of rapid reform.
 *   - constitutional_historians (institutional/analytical): Observer â supplies evidence but is selectively heard.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.72).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.6).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Reading of the U.S. Constitution (1787)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, 'f6268a96-d831-447b-9f1d-b09c13cdc36b').
narrative_ontology:cs_kernel_codification('f6268a96-d831-447b-9f1d-b09c13cdc36b', fixed_text).
narrative_ontology:cs_authority_grounding('f6268a96-d831-447b-9f1d-b09c13cdc36b', lineage).
narrative_ontology:cs_interpretation_layer_present('f6268a96-d831-447b-9f1d-b09c13cdc36b').
narrative_ontology:cs_reading_relation('f6268a96-d831-447b-9f1d-b09c13cdc36b', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('f6268a96-d831-447b-9f1d-b09c13cdc36b', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('f6268a96-d831-447b-9f1d-b09c13cdc36b', foundational, original_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(original_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('f6268a96-d831-447b-9f1d-b09c13cdc36b', original_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('f6268a96-d831-447b-9f1d-b09c13cdc36b', foundational, judicial_fidelity_to_ratification_meaning).
narrative_ontology:cs_axiom_status(judicial_fidelity_to_ratification_meaning, holdable).
narrative_ontology:cs_axiom_grounding('f6268a96-d831-447b-9f1d-b09c13cdc36b', judicial_fidelity_to_ratification_meaning, deontological).
narrative_ontology:cs_reference_frame('f6268a96-d831-447b-9f1d-b09c13cdc36b', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('f6268a96-d831-447b-9f1d-b09c13cdc36b', contemporary_constitutional_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f6268a96-d831-447b-9f1d-b09c13cdc36b', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_jurists).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, structural_conservatives).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, living_constitutionalist_scholars).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, framers_intent_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, fixed_meaning_thesis).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, historical_fidelity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret constitutional provisions by recovering the original public meaning understood at ratification in 1787. Their professional authority, appointment prospects, and clerkship networks depend on maintaining this historical method as a legitimate and binding mode of constitutional argumentation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_jurists, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, originalist_jurists, beneficiary).

% Political and institutional actors who favor stable, limited government and resist rapid constitutional innovation. They benefit from an interpretive method that privileges the political and economic arrangements familiar to the founding generation and blocks modern reform movements from securing constitutional recognition.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, structural_conservatives, beneficiary,
    powerful, generational, mobile, national).

% Groups and individuals who seek constitutional protection for rightsâsuch as privacy, equality, or due-process expansionsâthat lack a clear basis in the historical record of 1787. Their constitutional claims are systematically ruled inadmissible under this interpretive framework, forcing reliance on statutory or political channels.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, modern_rights_claimants, payer,
    organized, biographical, constrained, national).

% Legal academics who argue that constitutional meaning evolves through precedent, social movements, and moral progress. Under this constraint, their methodology is treated as outside the bounds of legitimate judicial argument, reducing their capacity to influence case outcomes and secure academic appointments in top appellate clinics.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalist_scholars, payer,
    institutional, generational, mobile, national).

% Professional historians who research the documentary record of the founding era. Their findings are selectively enlisted by originalist jurists to support fixed-meaning claims, while historiographical conclusions that undermine originalist certainty are often sidelined or treated as irrelevant in constitutional argument.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_historians, observer,
    institutional, generational, analytical, national).

narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, historically anchored method of constitutional interpretation intended to constrain judicial discretion, prevent arbitrary rulings, and stabilize expectations about fundamental law across time.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary democratic majorities, evolving social movements, and progressive jurists to the historical understandings of the 1787 ratifiers; it also transfers epistemic burdens onto modern claimants to produce historical evidence of original meaning.
% ABSENT_VOICES: Enslaved persons, women, and indigenous nations excluded from the 1787 ratification process; future generations whose conditions were not anticipated; non-originalist historians whose findings complicate the originalist historical record; and litigants asserting rights without demonstrable 1787 provenance.
% DISAPPEARANCE_RATIONALE: Constitutional doctrine would shift toward living or positivist methods; previously excluded modern rights claims would become justiciable; the interpretive industry and appointment infrastructure organized around historical recovery would lose its constitutional foothold; and the political coalition that relies on constitutional inertia would face accelerated change.
% FOUNDING_PROBLEM: How to constrain unelected judges from imposing their own preferences under the guise of constitutional interpretation, and how to ground judicial review in a democratically legitimate fixed text rather than evolving judicial majorities.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and affiliated professional networks attest the problem remains live. Critical legal scholars, historians outside the beneficiary set, and political scientists attest that originalism itself functions as a mechanism for judicial selection politics and outcome-oriented reasoning; empirical studies of judicial behavior corroborate that methodological constraints are often outcome-dependent. No neutral corroboration is unanimous.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading imposes severe epistemic burdens and excludes whole categories of modern rights claims from constitutional protection. Suppression (0.60) reflects that alternative interpretive methods, while intellectually present, are institutionally disadvantaged in federal judicial selection and appellate argumentation. Theater ratio (0.50) captures the contested sincerity of originalist reasoningâsignificant performative investment in historical argument that is sometimes instrumentally deployed to justify predetermined outcomes. Accessibility collapse (0.60) is moderate: alternatives are intellectually available but practically foreclosed within the dominant appellate forum. Resistance (0.65) is high because the constraint faces sustained opposition from social movements, legal academia, and dissenting justices.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist jurist's seat, the constraint is genuine coordination against judicial tyrannyâa rope that binds judges to law rather than will. From the modern rights claimant's seat, the same structure operates as extraction that locks them out of the constitutional forum. The engine computes this divergence from the structural asymmetry in exit options and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist jurists and structural conservatives sit near the beneficiary end (low d): the constraint subsidizes their authority and preferred political arrangements. Modern rights claimants sit near the target end (high d): they bear the cost of exclusion. Living constitutionalist scholars sit mid-to-high d: they are not materially trapped but pay professional and epistemic costs. Constitutional historians occupy an analytical seat with d near 0.5 but are selectively instrumentalized.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids snare classification because it supplies a recognizable coordination functionâjudicial restraint and predictability. It avoids rope classification because the coordination is not symmetric: modern claimants and non-originalist scholars pay substantial costs that originalist jurists and conservatives do not share. The tangled rope classification captures this hybrid: genuine coordination plus asymmetric extraction maintained by active institutional enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the originalist_reading of the us_constitution_1787 kernel. How would classification change if the living_reading or positivist_reading were adopted instead?',
    'Comparative analysis of the sibling constraint stories and their respective beneficiary/victim structures.',
    'The originalist reading produces high extraction for modern rights claimants; sibling readings would shift the victim/beneficiary structure and lower extraction for those claimants while raising it for other seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame uncertainty about kernel reading choice and structural delta.').

omega_variable(
    historical_indeterminacy,
    'Are the epistemic demands of originalism (recovering 1787 public meaning) satisfiable, or do they function as a procedural barrier that shields outcomes from contestation?',
    'Cross-methodological consensus among historians on key constitutional provisions; comparison of originalist and non-originalist historical briefs in landmark cases.',
    'If original meaning is largely indeterminate, the constraint functions more as a snare (extraction through epistemic barrier) than a tangled rope; if determinate, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_indeterminacy, empirical, 'Whether originalism''s epistemic demands are genuine or instrumental.').

omega_variable(
    enforcement_mobilization_source,
    'Does the constraint''s active enforcement derive from internal professional norms of fidelity, or from external political mobilization (judicial appointment infrastructure, funding networks)?',
    'Sociological study of judicial hiring and clerkship networks; tracking funding, citation, and amicus patterns in originalist scholarship over the interval.',
    'If enforcement is primarily political, the coordination function is thinner and extraction more dominant; if professional, the rope element is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mobilization_source, empirical, 'Institutional capture versus genuine normative enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orig_1787_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(orig_1787_tr_t8, us_constitution_1787__originalist_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(orig_1787_tr_t16, us_constitution_1787__originalist_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(orig_1787_tr_t24, us_constitution_1787__originalist_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(orig_1787_tr_t32, us_constitution_1787__originalist_reading, theater_ratio, 32, 0.45).
narrative_ontology:measurement(orig_1787_tr_t40, us_constitution_1787__originalist_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(orig_1787_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(orig_1787_be_t8, us_constitution_1787__originalist_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(orig_1787_be_t16, us_constitution_1787__originalist_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(orig_1787_be_t24, us_constitution_1787__originalist_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(orig_1787_be_t32, us_constitution_1787__originalist_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(orig_1787_be_t40, us_constitution_1787__originalist_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(orig_1787_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(orig_1787_su_t8, us_constitution_1787__originalist_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(orig_1787_su_t16, us_constitution_1787__originalist_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(orig_1787_su_t24, us_constitution_1787__originalist_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(orig_1787_su_t32, us_constitution_1787__originalist_reading, suppression_requirement, 32, 0.63).
narrative_ontology:measurement(orig_1787_su_t40, us_constitution_1787__originalist_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'U.S. Constitution' conflates three structurally distinct interpretive constraints. This file isolates the originalist reading; siblings isolate living and positivist readings. Each has distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
