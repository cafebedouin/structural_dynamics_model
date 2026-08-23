% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Text â Copyleft Counterfactual Reading
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This is one reading of the permissive_license_text kernel. The standing
 *   arrangement is the widespread use of permissive software licenses (e.g.,
 *   MIT, BSD, Apache) that relax copyright without requiring reciprocity. The
 *   copyleft counterfactual reading treats this arrangement as a tangled
 *   rope: it performs genuine coordination by lowering legal friction for
 *   code reuse, but it simultaneously enables asymmetric extraction by
 *   allowing downstream builders â proprietary and copyleft alike â to
 *   absorb upstream labor without returning derivative works to the commons.
 *   The reading claims that viral reciprocity (GPL) is the necessary
 *   alternative to prevent this exploitation. Sibling readings include the
 *   commons_coordination_reading (framing permissive licensing as pure
 *   coordination maximizing freedom) and the corporate_moat_reading (framing
 *   it as enabling uncompensated proprietary extraction).
 *
 * KEY AGENTS:
 *   - copyleft_advocates: Primary beneficiary (organized/mobile) â gains code inflow and ideological vindication from the permissive regime's structural failures
 *   - proprietary_builders: Primary target (powerful/constrained) â bears the long-term costs of a non-reciprocal commons despite apparent zero-price access
 *   - upstream_contributors: Secondary target (moderate/constrained) â supplies labor without reciprocity, experiencing direct extraction of uncompensated improvements
 *   - permissive_license_authors: Agenda-setter (moderate/mobile) â authors and maintains the legal text that removes reciprocity requirements
 *   - oss_norm_institutions: Analytical observer (institutional/analytical) â tracks compliance and ecosystem health without direct extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.78).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.66).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Text â Copyleft Counterfactual Reading").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, '10ceb943-39da-47d2-819f-a3c8d63de6fa').
narrative_ontology:cs_kernel_codification('10ceb943-39da-47d2-819f-a3c8d63de6fa', fixed_text).
narrative_ontology:cs_authority_grounding('10ceb943-39da-47d2-819f-a3c8d63de6fa', distributed).
narrative_ontology:cs_reading_relation('10ceb943-39da-47d2-819f-a3c8d63de6fa', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('10ceb943-39da-47d2-819f-a3c8d63de6fa', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('10ceb943-39da-47d2-819f-a3c8d63de6fa', foundational, reciprocity_mandatory_for_sustainable_commons).
narrative_ontology:cs_axiom_status(reciprocity_mandatory_for_sustainable_commons, holdable).
narrative_ontology:cs_axiom_grounding('10ceb943-39da-47d2-819f-a3c8d63de6fa', reciprocity_mandatory_for_sustainable_commons, instrumental).
narrative_ontology:cs_axiom('10ceb943-39da-47d2-819f-a3c8d63de6fa', foundational, permissive_licensing_enables_asymmetric_extraction).
narrative_ontology:cs_axiom_status(permissive_licensing_enables_asymmetric_extraction, holdable).
narrative_ontology:cs_axiom_grounding('10ceb943-39da-47d2-819f-a3c8d63de6fa', permissive_licensing_enables_asymmetric_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('10ceb943-39da-47d2-819f-a3c8d63de6fa', reciprocal_commons_sustainability).
narrative_ontology:cs_drift_state('10ceb943-39da-47d2-819f-a3c8d63de6fa', permissive_hegemony_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('10ceb943-39da-47d2-819f-a3c8d63de6fa', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, proprietary_builders).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, upstream_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for reciprocal licensing norms and the GPL. They benefit from the permissive license regime because it supplies a pool of code that can be lawfully incorporated into copyleft projects without reciprocity flowing back to permissive upstreams, and because the perceived failures of non-reciprocal commons structurally vindicate their advocacy.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, beneficiary,
    organized, generational, mobile, global).

% Build proprietary products that incorporate permissively licensed code. In this reading they are structurally victimized by a low-reciprocity equilibrium that prevents sustainable shared investment, commoditizes their differentiation by granting identical zero-cost access to competitors, and forces reliance on undermaintained upstream commons.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_builders, payer,
    powerful, biographical, constrained, global).

% Write and maintain source code released under permissive terms. Their labor is extracted into downstream proprietary and copyleft derivatives that are not required to contribute improvements back. They bear the sustainability cost of a commons that consumes their effort without reciprocal replenishment.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, upstream_contributors, payer,
    moderate, biographical, constrained, global).

% Authored and propagated the standard permissive license texts that removed reciprocity requirements. They set the legal default that downstream use need not return source, and they actively defend the boundaries of those texts in legal and normative discourse.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, permissive_license_authors, agenda_setter,
    moderate, generational, mobile, global).

% Organizations that steward open-source definitions and license compliance standards. They observe the tension between permissive and reciprocal licensing camps, certify licenses, and track ecosystem health without directly extracting from or paying into the constraint.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, oss_norm_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, low-friction legal mechanism for sharing software source code across organizational and jurisdictional boundaries, eliminating bilateral negotiation over reuse rights.
% TRANSFER_FUNCTION: Moves intellectual labor and source code from upstream contributors to downstream builders â including proprietary firms and copyleft projects â without requiring disclosure of derivative source code or payment, enabling uncompensated extraction of derivative value.
% ABSENT_VOICES: Upstream maintainers experiencing burnout and sustainability collapse are structurally underrepresented in license governance; corporate procurement officers who would prefer a reciprocity requirement to prevent competitor free-riding are absent from the permissive-license design conversation.
% DISAPPEARANCE_RATIONALE: If the permissive license text and its non-reciprocity clause disappeared overnight, downstream builders would need to negotiate terms bilaterally or default to copyleft or proprietary licensing. The current pattern of uncompensated incorporation into proprietary products would stall, and the software commons would reorganize around explicit reciprocity or closed negotiation.
% FOUNDING_PROBLEM: Early software licensing created high friction for code sharing; default proprietary terms prevented reuse and required expensive bilateral negotiation for every derivative.
% FOUNDING_PROBLEM_CORROBORATION: Permissive license authors and open-source institutions attest the friction problem remains live. Copyleft advocates and independent software-commons researchers attest the founding problem has been solved and the arrangement now creates a second-order sustainability crisis; empirical studies on maintainer burnout and open-source funding gaps from outside the beneficiary set support the dead/contested reading.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the permissive text structurally permits downstream capture of derivative value without return obligation, and because corporate scale has amplified this extraction over decades. Suppression (0.66) reflects the active cultural and institutional marginalization of copyleft alternatives â corporate GPL bans, permissive-default tooling, and the framing of reciprocity as a commercial threat. Theater ratio (0.47) captures the performative 'freedom' rhetoric that obscures the extraction: the licenses are celebrated as maximizing liberty while systematically diverting value away from upstream sustainers. Accessibility collapse is moderate (0.45) because copyleft alternatives remain legally available but are culturally and institutionally disadvantaged. Resistance (0.6) reflects sustained copyleft advocacy and growing maintainer-pushback against uncompensated use.
 *
 * PERSPECTIVAL GAP:
 *   The copyleft advocate seat and the proprietary builder seat compute divergent classifications. From the copyleft advocate position, the permissive text is a flawed but usable feeder system that validates reciprocal licensing. From the upstream contributor position, it is direct extraction of unpaid labor. From the proprietary builder position, it is a coordination mechanism that has become a competitive trap. The engine computes these divergences from the structural data â the reading does not adjudicate a single seat as correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyleft advocates are declared beneficiaries because they receive code inflow from permissive upstreams and ideological validation from the regime's perceived failures; their mobile exit options push directionality toward the beneficiary pole. Proprietary builders are declared victims in this reading because the non-reciprocal equilibrium commoditizes their differentiation and erodes shared infrastructure; their constrained exit and high power produce high directionality. Upstream contributors are straightforward targets: constrained, moderate power, and direct cost-bearing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by preserving the genuine coordination function: permissive licenses do solve a real bilateral-friction problem and enable cross-organizational reuse that copyleft's overhead might impede. However, the coordination is asymmetric â some parties coordinate while others pay â which is why it is tangled rope rather than rope. It is not a pure snare because the extraction is not the sole function; the legal friction reduction is real and used by non-extractive actors. The measurement series shows extraction accumulating over time, which supports the tangled-rope diagnosis rather than a static coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one reading of the permissive_license_text kernel, and how would sibling readings reclassify the beneficiary structure?',
    'Compare authored epsilon and stakeholder roles with commons_coordination_reading and corporate_moat_reading; if sibling readings produce divergent epsilon values for the same legal text, the epsilon-invariance principle requires treating them as distinct constraints.',
    'If readings converge on extraction metrics, the kernel is a unified constraint with contested classification; if they diverge, decomposition is validated and the family link in network.affects_constraints is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame uncertainty about kernel decomposition').

omega_variable(
    copyleft_benefit_mechanism,
    'Do copyleft advocates structurally benefit from the permissive license regime itself, or only in the counterfactual where copyleft replaces it?',
    'Measure incorporation flow of permissive code into copyleft projects versus proprietary products; measure advocacy-resource allocation and ideological recruitment tied to permissive-license failure modes.',
    'If copyleft advocates only benefit counterfactually, they should be reclassified from beneficiaries to observers or excluded, and the extraction target shifts to upstream contributors alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_benefit_mechanism, empirical, 'Whether copyleft advocate benefit is real or counterfactual').

omega_variable(
    alternatives_suppression,
    'Does the permissive license text actively suppress copyleft alternatives, or merely coexist with them via network-effect dominance?',
    'Analyze corporate policy bans on GPL, permissive-default platform tooling, and cultural framing that treats reciprocity as commercially toxic.',
    'If suppression is structurally active, the constraint leans toward snare; if network-effect dominance without active suppression, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternatives_suppression, empirical, 'Mechanism of copyleft alternative marginalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_copyleft_cf_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perm_copyleft_cf_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(perm_copyleft_cf_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(perm_copyleft_cf_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(perm_copyleft_cf_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(perm_copyleft_cf_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(perm_copyleft_cf_tr_t30, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(perm_copyleft_cf_tr_t35, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 35, 0.47).

% Extraction over time
narrative_ontology:measurement(perm_copyleft_cf_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(perm_copyleft_cf_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(perm_copyleft_cf_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(perm_copyleft_cf_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(perm_copyleft_cf_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(perm_copyleft_cf_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.73).
narrative_ontology:measurement(perm_copyleft_cf_be_t30, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(perm_copyleft_cf_be_t35, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 35, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perm_copyleft_cf_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(perm_copyleft_cf_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(perm_copyleft_cf_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(perm_copyleft_cf_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(perm_copyleft_cf_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(perm_copyleft_cf_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(perm_copyleft_cf_su_t30, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(perm_copyleft_cf_su_t35, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 35, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This constraint is one decomposition of the permissive_license_text kernel per the epsilon-invariance principle. The commons_coordination_reading, copyleft_counterfactual_reading, and corporate_moat_reading share the same legal text but instantiate different structural claims with different epsilon values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
