% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UN Charter Article 27 P5 Veto Power (Oligopoly Reading)
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the oligopoly reading of the UN
 *   Charter Article 27 P5 veto kernel. Under this reading, the veto is not a
 *   coordination mechanism for collective security but a structural snare: it
 *   entrenches a mid-20th-century geopolitical oligopoly, extracts ongoing
 *   authority rents for the five permanent members, and actively suppresses
 *   institutional evolution (including Charter reform) that would
 *   redistribute power. The P5's combined veto over Charter amendments
 *   (Article 108) creates a self-locking mechanism whereby the authority the
 *   constraint distributes is used to prevent its revision. The referent is
 *   the standing arrangement under contestâthe veto's immutability and
 *   active useâassessed by this reading's lights.
 *
 * KEY AGENTS:
 *   - P5 Permanent Members (agenda_setter/beneficiary): Hold veto power and block reform
 *   - Non-P5 UN Members (payer): Majority membership excluded from veto and reform path
 *   - Reform Advocacy Coalitions (excluded): Advance democratization proposals blocked by P5
 *   - UN Secretariat (observer): Implements mandates but cannot alter power distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.82).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.88).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UN Charter Article 27 P5 Veto Power (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '28f114fa-eabe-4f11-b755-05d46b4832c1').
narrative_ontology:cs_kernel_codification('28f114fa-eabe-4f11-b755-05d46b4832c1', formalized).
narrative_ontology:cs_authority_grounding('28f114fa-eabe-4f11-b755-05d46b4832c1', extraction).
narrative_ontology:cs_interpretation_layer_present('28f114fa-eabe-4f11-b755-05d46b4832c1').
narrative_ontology:cs_reading_relation('28f114fa-eabe-4f11-b755-05d46b4832c1', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('28f114fa-eabe-4f11-b755-05d46b4832c1', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('28f114fa-eabe-4f11-b755-05d46b4832c1', foundational, veto_constitutes_authority_rent_extraction).
narrative_ontology:cs_axiom_status(veto_constitutes_authority_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('28f114fa-eabe-4f11-b755-05d46b4832c1', veto_constitutes_authority_rent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('28f114fa-eabe-4f11-b755-05d46b4832c1', foundational, charter_immutability_entrenches_oligopoly).
narrative_ontology:cs_axiom_status(charter_immutability_entrenches_oligopoly, holdable).
narrative_ontology:cs_axiom_grounding('28f114fa-eabe-4f11-b755-05d46b4832c1', charter_immutability_entrenches_oligopoly, conventional).
narrative_ontology:cs_reference_frame('28f114fa-eabe-4f11-b755-05d46b4832c1', postwar_great_power_concert).
narrative_ontology:cs_drift_state('28f114fa-eabe-4f11-b755-05d46b4832c1', contemporary_multipolar_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('28f114fa-eabe-4f11-b755-05d46b4832c1', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_un_members).
narrative_ontology:constraint_vindicates(article_27_veto_power__oligopoly_reading, great_power_concert_legitimacy).
narrative_ontology:constraint_vindicates(article_27_veto_power__oligopoly_reading, charter_immutability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent seats on the UN Security Council with unilateral veto power over substantive resolutions and Charter amendments. Derive structural authority, agenda control, and immunity from enforcement actions. Actively block any reform proposal that would dilute permanent membership or veto rights.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_permanent_members, beneficiary).

% Comprise the majority of UN member states. Bound by the UN Charter and Security Council decisions but lack veto power or permanent representation. Attempts to advance Charter reform through the General Assembly are structurally blocked by the P5 veto over amendments. Bear the cost of institutional exclusion and agenda subordination.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_un_members, payer,
    organized, generational, constrained, global).

% Campaign for expansion of permanent membership, limitation of veto use, or democratization of international security governance. Active in the General Assembly and intergovernmental negotiations, but their proposals require P5 approval under Article 108, which is never forthcoming.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, reform_advocacy_coalitions, excluded,
    moderate, generational, constrained, global).

% Administrative body that implements Security Council mandates and supports diplomatic processes. Derives procedural authority from the Charter but cannot advance institutional reform against P5 opposition. Its operational independence is bounded by the Council's political direction.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat, observer,
    institutional, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a procedural circuit-breaker ensuring that no Security Council enforcement resolution is adopted against the will of any permanent member.
% TRANSFER_FUNCTION: Moves institutional authority, agenda control, and enforcement immunity from the broader UN membership to the five permanent members, extracting geopolitical rents through structural blockage of Charter reform.
% ABSENT_VOICES: Non-P5 states and reform coalitions advocating for democratization of the Security Council are present in the General Assembly but excluded from effective Charter revision, which requires P5 concurrence.
% DISAPPEARANCE_RATIONALE: If the veto and its immutability disappeared, Security Council decision-making would shift toward majoritarian or weighted models, P5 structural privilege would collapse, and the UN security architecture would reorganize around contemporary geopolitical realities rather than 1945 power distributions.
% FOUNDING_PROBLEM: Prevention of great-power war and construction of a stable post-WWII security order by ensuring that no UN enforcement action could be imposed on a major Allied power against its will.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international relations scholars outside the P5 beneficiary set attest that the veto was designed for a specific 1945 geopolitical configuration; the Non-Aligned Movement and contemporary IR scholarship attest that the problem context has shifted fundamentally, while P5 states assert the problem remains live to justify continued privilege.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is authored high because the veto immutability extracts authority rents: P5 states capture permanent agenda control, immunity from enforcement, and structural leverage over international law. Suppression (0.88) is authored high because the constraint's persistence depends on actively blocking alternativesâCharter reform requires P5 consent, effectively zeroing the probability of amendment. Accessibility collapse (0.78) is high because once the veto structure is understood, the only alternatives (regional organizations, unilateralism, Charter replacement) are costly and fragmented. Resistance (0.55) is moderate: non-P5 states voice sustained reform demands, but resistance is institutionally contained by procedural rules. Theater ratio (0.55) is moderate-high: much Security Council activity maintains the performative frame of 'international peace and security' while operating as great-power management, and reform debates are ritualized without expectation of success.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat, the veto is a necessary prerogative of global responsibility and a bulwark of Charter stability. From the non-P5 majority seat, the same structure reads as an immovable oligopoly that freezes power distribution circa 1945. The engine computes this divergence from the structural data: identical constraint, opposite directionalities, producing opposed per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 permanent members are declared beneficiaries and agenda-setters; their structural relationship to the constraint is subsidy (low d) because the constraint channels authority and blocking power to them. Non-P5 UN members are declared payers; their structural relationship is target (high d) because the constraint extracts voice and institutional possibility from them. The UN Secretariat sits near symmetric but leans target: it is structurally dependent on P5 consensus and cannot advance reform, though it derives some procedural authority from the Charter framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing great-power war through concertâwas plausible in 1945 but is substantially dead as a justification for contemporary immutability. The arrangement persists not because the problem is live but because the P5 capture the authority rents. This is not a piton: there is a concentrated beneficiary set that actively profits from maintenance, which distinguishes it from inertial degradation. The classification as snare is secured by the presence of identifiable beneficiaries with incentives to defend the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oligopoly_vs_coordination_empirical_signature,
    'Does the empirical pattern of veto use correlate with collective security needs or with narrow P5 geopolitical interests?',
    'Systematic coding of veto justifications against independent security assessments, plus comparison of veto use with P5 national interest alignment.',
    'If veto use tracks narrow interests over collective security, the coordination reading loses empirical support and the snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_vs_coordination_empirical_signature, empirical, 'Empirical signature distinguishing extraction from coordination').

omega_variable(
    charter_reform_viability_without_p5,
    'Is there any procedural, interpretive, or extralegal path to Charter amendment that bypasses P5 veto under Article 108?',
    'Legal analysis of alternative amendment theories and observation of any successful reform initiatives that circumvent P5 gatekeeping.',
    'If viable bypasses exist, the suppression metric is overstated and the constraint''s closure is less absolute than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charter_reform_viability_without_p5, conceptual, 'Whether Charter reform can bypass P5 consent').

omega_variable(
    authority_rent_quantification,
    'Can the authority rents captured by P5 states through veto power be quantified in diplomatic, economic, or institutional terms?',
    'Comparative institutional analysis measuring P5 influence over UN agendas, peacekeeping mandates, and sanction regimes relative to non-P5 members.',
    'If rents are demonstrable and substantial, the extractiveness metric is validated; if negligible, the oligopoly reading overstates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_rent_quantification, empirical, 'Quantifiability of P5 authority rents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__oligopoly_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__oligopoly_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__oligopoly_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__oligopoly_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(arti_tr_t50, article_27_veto_power__oligopoly_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__oligopoly_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(arti_tr_t70, article_27_veto_power__oligopoly_reading, theater_ratio, 70, 0.52).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.55).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__oligopoly_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__oligopoly_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__oligopoly_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__oligopoly_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(arti_be_t50, article_27_veto_power__oligopoly_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__oligopoly_reading, base_extractiveness, 60, 0.74).
narrative_ontology:measurement(arti_be_t70, article_27_veto_power__oligopoly_reading, base_extractiveness, 70, 0.79).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(arti_su_t10, article_27_veto_power__oligopoly_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__oligopoly_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(arti_su_t30, article_27_veto_power__oligopoly_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__oligopoly_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(arti_su_t50, article_27_veto_power__oligopoly_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__oligopoly_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(arti_su_t70, article_27_veto_power__oligopoly_reading, suppression_requirement, 70, 0.84).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the article_27_veto_power kernel, decomposed from coordination and sovereignty readings per the epsilon-invariance principle. Each reading carries a distinct epsilon and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
