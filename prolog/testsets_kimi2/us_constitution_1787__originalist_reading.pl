% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Originalist Reading of the U.S. Constitution (Fixed Meaning at Ratification)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution holds that
 *   constitutional meaning was fixed at the moment of ratification
 *   (1787â1791) and that interpreters are bound by the original public
 *   meaning or the framers' intent. This constraint operates through a
 *   network of institutionsâprincipally the federal judiciary and the
 *   conservative legal movementâto block constitutional evolution and
 *   modern rights claims that lack a demonstrable basis in founding-era
 *   understandings. It presents itself as a neutral coordination mechanism
 *   (binding judges to historical facts) but functions asymmetrically: it
 *   legitimates pre-1787 practices and conservative political outcomes while
 *   extracting from progressive democratic majorities and modern rights
 *   claimants. The constraint requires active enforcement through judicial
 *   selection, clerkship pipelines, and law-school credentialing.
 *
 * KEY AGENTS:
 *   - federalist_society_network: Primary agenda-setter (institutional/arbitrage) â administers the interpretive pipeline and collects institutional prestige and political influence
 *   - conservative_judiciary: Secondary agenda-setter/beneficiary (institutional/arbitrage) â applies the constraint in binding decisions and benefits from interpretive stability
 *   - modern_rights_claimants: Primary target (powerless/constrained) â bears exclusion from constitutional protection
 *   - progressive_legislators: Secondary target (organized/constrained) â bears judicial override of democratic legislation
 *   - living_constitutionalist_scholars: Excluded voice (moderate/constrained) â alternative interpretive method suppressed in dominant venues
 *   - constitutional_historians: Analytical observer (moderate/analytical) â supplies evidence whose epistemic limits are selectively ignored
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.75).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.82).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Reading of the U.S. Constitution (Fixed Meaning at Ratification)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '36b92afd-6cea-4718-b02c-9e0979522d62').
narrative_ontology:cs_kernel_codification('36b92afd-6cea-4718-b02c-9e0979522d62', fixed_text).
narrative_ontology:cs_authority_grounding('36b92afd-6cea-4718-b02c-9e0979522d62', lineage).
narrative_ontology:cs_interpretation_layer_present('36b92afd-6cea-4718-b02c-9e0979522d62').
narrative_ontology:cs_reading_relation('36b92afd-6cea-4718-b02c-9e0979522d62', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('36b92afd-6cea-4718-b02c-9e0979522d62', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('36b92afd-6cea-4718-b02c-9e0979522d62', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('36b92afd-6cea-4718-b02c-9e0979522d62', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('36b92afd-6cea-4718-b02c-9e0979522d62', foundational, framers_intent_is_binding).
narrative_ontology:cs_axiom_status(framers_intent_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('36b92afd-6cea-4718-b02c-9e0979522d62', framers_intent_is_binding, deontological).
narrative_ontology:cs_reference_frame('36b92afd-6cea-4718-b02c-9e0979522d62', fixed_original_meaning_framework).
narrative_ontology:cs_drift_state('36b92afd-6cea-4718-b02c-9e0979522d62', contemporary_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('36b92afd-6cea-4718-b02c-9e0979522d62', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, federalist_society_network).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_judiciary).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, modern_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, progressive_legislators).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, original_public_meaning_thesis).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, framers_intent_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Selects and promotes judicial candidates, controls clerkship pipelines, and defines the boundaries of legitimate constitutional argument in elite legal institutions to maintain originalism as the governing interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, federalist_society_network, agenda_setter,
    institutional, generational, arbitrage, national).

% Issues decisions striking down modern legislation and denying rights claims based on asserted original meaning; their institutional authority and interpretive stability depend on the originalist frame.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, conservative_judiciary, beneficiary).

% Seek constitutional protection for rights not recognized or imagined in 1787; their claims are systematically ruled outside the constitutional boundary by originalist interpretation and they cannot obtain the protections they seek through ordinary democratic or judicial channels.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, modern_rights_claimants, payer,
    powerless, biographical, constrained, national).

% Enact modern regulatory and social welfare legislation that is structurally vulnerable to originalist judicial review; their democratic mandates are constrained by a constitutional framework frozen to pre-1787 practices and understandings.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, progressive_legislators, payer,
    organized, biographical, constrained, national).

% Advance interpretive frameworks based on evolving social norms and moral philosophy; their arguments are treated as illegitimate in originalist-controlled courts and are rarely adopted in binding opinions.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalist_scholars, excluded,
    moderate, generational, constrained, national).

% Produce historical evidence about founding-era meaning; many acknowledge the epistemic limitations and indeterminacy of historical recovery, but their findings are selectively instrumentalized by originalist institutions.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_historians, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, federalist_society_network).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded interpretive framework that constrains judicial discretion and coordinates constitutional interpretation across time and institutions by tethering meaning to the fixed point of ratification.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary democratic majorities and evolving moral consensus to historical evidence about 1787 understandings and to the institutions that control access to and deployment of that evidence.
% ABSENT_VOICES: Living constitutionalist jurists and modern rights claimants are structurally excluded from the interpretive frame; their normative arguments are ruled inadmissible in originalist-controlled courts. Progressive legislators are present but their constitutional claims are treated as illegitimate.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight as the governing interpretive method, constitutional doctrine would shift rapidly: modern rights claims would be re-evaluated under evolving-standards frameworks, progressive legislation would face reduced judicial vulnerability, and the conservative legal movement's institutional pipeline would lose its primary credentialing function.
% FOUNDING_PROBLEM: How to constrain unelected judges from imposing their own moral and political preferences under the guise of constitutional interpretation; how to provide a stable, neutral baseline for constitutional law that transcends contemporary political conflict.
% FOUNDING_PROBLEM_CORROBORATION: Originalist institutions and conservative jurists attest the problem is still live, citing judicial activism. Living constitutionalist scholars and many historians attest the problem has shifted: originalism now functions as a mechanism for modern political control dressed in historical garb, and the neutrality claim is contested. Empirical studies of originalist judicial behavior and historical indeterminacy from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.75, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.75) because the constraint systematically blocks modern democratic self-governance and rights claims by tethering constitutional legitimacy to an 18th-century baseline. Suppression is higher (0.82) because the persistence of this arrangement depends on actively excluding rival interpretive methods from courts and elite legal institutions, not on the spontaneous preference of constitutional interpreters. Theater is elevated (0.55): historical claims are frequently instrumentalized to reach modern conservative outcomes, and historians routinely contest the evidentiary basis of originalist assertions. Accessibility collapse is high (0.82) because once originalism is accepted as the legitimate frame, living constitutionalist alternatives collapse within the institutional space. Resistance is moderate (0.55): living constitutionalist scholars and progressive legislators push back via academia, litigation, and court-reform proposals, but they lack institutional control.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (Federalist Society network, conservative judiciary) experience the constraint as a genuine coordination mechanism that preserves the rule of law and constrains judicial activism. The payer seats (modern rights claimants, progressive legislators) experience the same structure as an asymmetric extraction that freezes democratic development and denies constitutional recognition. The engine computes this divergence from the structural data: low directionality for beneficiaries with arbitrage-grade exit, high directionality for victims with constrained exit. The authored claim (tangled_rope) acknowledges both experiences as structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   The Federalist Society network and conservative judiciary are structural beneficiaries: they collect institutional prestige, political influence, and interpretive control (low d, subsidized by the constraint). Modern rights claimants and progressive legislators are structural targets: they pay in denied constitutional protections and overridden legislation (high d, amplified extraction). Living constitutionalist scholars are excluded from the conversation, which further suppresses their exit options. Constitutional historians are observers whose findings are selectively deployed; they do not collect or pay in the constraint's currency of power.
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism was arguably constructed as a scaffold to counter mid-20th-century judicial activism, but it carried no sunset clause and has persisted beyond its founding problem. The classification as tangled_rope (rather than rope or snare) prevents mislabeling: it captures the genuine coordination function (interpretive stability, judicial constraint) while insisting on the asymmetric extraction component (modern rights denial, democratic constraint) and the active enforcement required to maintain it. A rope classification would ignore the victims; a snare classification would deny the coordination function that sincere originalists genuinely value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_kernel_contest,
    'Does the originalist reading of the Constitution foreclose all alternative interpretive frameworks, or does it coexist with textual positivism while structurally suppressing living constitutionalism?',
    'Analysis of judicial behavior, legal briefs, and hiring patterns to determine whether originalist and positivist methods are deployed by the same actors without contradiction, and whether living constitutionalist arguments are ruled logically inadmissible or merely institutionally excluded.',
    'If originalism forecloses living constitutionalism logically but coexists with positivism, the constraint family is correctly partitioned; if originalism and living constitutionalism are merely in institutional tension, the forecloses relation should be downgraded to influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_kernel_contest, conceptual, 'Structural relationship between originalist reading and sibling readings in the constitutional kernel').

omega_variable(
    epistemic_recoverability,
    'Can the original public meaning of constitutional provisions be recovered with sufficient precision to genuinely constrain modern judicial discretion?',
    'Advances in historical methods, corpus linguistics, and founding-era archival research could resolve whether original meaning is determinate or irreducibly ambiguous.',
    'High recoverability supports the coordination claim (genuine constraint on judges); irreducible ambiguity suggests the extraction component dominates (judges select from indeterminate history to reach preferred outcomes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_recoverability, empirical, 'Whether original public meaning is epistemically recoverable enough to function as claimed').

omega_variable(
    coordination_extraction_boundary,
    'Is the constraint''s active enforcement directed primarily at preserving interpretive stability (coordination) or at securing modern political outcomes (extraction)?',
    'Comparative doctrinal analysis of originalist judicial outcomes across issue areas to test whether the method produces principled consistency or tracks conservative political preferences.',
    'If enforcement correlates strongly with political valence, the extraction fraction rises and the constraint edges toward snare; if outcomes are politically cross-cutting, the coordination fraction is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether originalist enforcement is principled or politically instrumental').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__originalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__originalist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__originalist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__originalist_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__originalist_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_1787__originalist_reading, theater_ratio, 45, 0.55).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__originalist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__originalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__originalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__originalist_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__originalist_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(us_c_be_t45, us_constitution_1787__originalist_reading, base_extractiveness, 45, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__originalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__originalist_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__originalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__originalist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__originalist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(us_c_su_t45, us_constitution_1787__originalist_reading, suppression_requirement, 45, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'U.S. Constitution' decomposes into structurally distinct interpretive commitments. This story isolates the originalist reading; sibling readings instantiate different constraints with different epsilon values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
