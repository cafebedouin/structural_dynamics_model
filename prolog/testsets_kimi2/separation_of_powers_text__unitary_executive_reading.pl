% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Reading â Absolute Presidential Removal Power Over Independent Agencies
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint instantiates the unitary_executive_reading of the
 *   separation_of_powers_text kernel. It reads Article II's Vesting Clause as
 *   allocating all executive power exclusively to the President, rendering
 *   for-cause removal protections for independent agency heads
 *   unconstitutional and consolidating control over the administrative state
 *   in the White House. Unlike the formalist readingâwhich empowers the
 *   judiciary to police strict inter-branch boundariesâor the functionalist
 *   readingâwhich permits overlapping authority under intelligible
 *   principlesâthis reading makes the President the absolute beneficiary of
 *   executive-power doctrine and treats independent agencies as the primary
 *   victims. The claim is tangled_rope because it wraps a genuine
 *   coordination rationale (democratic accountability through a single
 *   elected chain of command) around an asymmetric extraction of
 *   institutional independence from agencies and reviewing authority from the
 *   courts.
 *
 * KEY AGENTS:
 *   - President: Primary beneficiary/agenda-setter (institutional/constrained) â gains at-will removal power and unified executive command.
 *   - Executive branch legal apparatus: Secondary beneficiary (institutional/constrained) â advances the theory and accrues influence from presidential empowerment.
 *   - Independent agency leadership: Primary target (institutional/constrained) â loses statutory for-cause removal protections and policy insulation.
 *   - Federal judiciary: Secondary target (institutional/constrained) â loses separation-of-powers review domain over internal executive structure.
 *   - Congress: Tertiary target (institutional/constrained) â loses constitutional power to create independent agencies with removal protections.
 *   - Public administration scholars: Analytical observer (analytical/analytical) â sees the full structure from outside the benefiting parties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.75).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.78).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Reading â Absolute Presidential Removal Power Over Independent Agencies").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, 'd6fd58f4-1bc7-4e93-ae60-c9f2edc3d336').
narrative_ontology:cs_kernel_codification('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', fixed_text).
narrative_ontology:cs_authority_grounding('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', lineage).
narrative_ontology:cs_interpretation_layer_present('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336').
narrative_ontology:cs_reading_relation('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', foundational, all_executive_power_vests_in_president).
narrative_ontology:cs_axiom_status(all_executive_power_vests_in_president, holdable).
narrative_ontology:cs_axiom_grounding('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', all_executive_power_vests_in_president, empirically_contingent).
narrative_ontology:cs_axiom('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', foundational, independent_agency_heads_are_unconstitutionally_insulated).
narrative_ontology:cs_axiom_status(independent_agency_heads_are_unconstitutionally_insulated, holdable).
narrative_ontology:cs_axiom_grounding('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', independent_agency_heads_are_unconstitutionally_insulated, empirically_contingent).
narrative_ontology:cs_reference_frame('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', article_ii_original_meaning).
narrative_ontology:cs_drift_state('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', post_new_deal_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d6fd58f4-1bc7-4e93-ae60-c9f2edc3d336', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch_legal_apparatus).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agency_leadership).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, federal_judiciary).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts Article II vests all executive power personally and seeks at-will removal authority over all agency heads. Benefits from concentrated control over the administrative state and a unified chain of command responsive to electoral accountability. Exit is constrained by term limits, congressional opposition, and impeachment, but within the framework occupies the apex of executive authority.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, president, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, president, beneficiary).

% Includes the Department of Justice, Office of Legal Counsel, and White House counsel who formulate and advance the unitary executive theory. They benefit from expanded presidential power that increases their own influence over agency policy and personnel. Their institutional identity is fused with executive supremacy.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch_legal_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Lead multi-member commissions and independent regulatory agencies with statutory for-cause removal protections. Under the unitary executive reading, these protections are unconstitutional, rendering them removable at will by the President. They lose institutional independence, policy continuity, and insulation from political pressure.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agency_leadership, payer,
    institutional, biographical, constrained, national).

% Under the unitary executive reading, courts lose a major domain of separation-of-powers review over internal executive structure; presidential removal power is treated as a constitutional absolute that courts cannot police. This reduces the judiciary's role as the ultimate arbiter of inter-branch boundaries compared to the formalist reading.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_judiciary, payer,
    institutional, generational, constrained, national).

% Loses the constitutional authority to structure the executive branch by creating independent agencies with for-cause removal protections. Legislative delegations to such agencies become vulnerable to constitutional challenge. Cannot easily exit the constitutional framework but resists through legislation, appropriations, and confirmation leverage.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, payer,
    institutional, generational, constrained, national).

% Analyze the unitary executive theory from outside the benefiting parties. Many attest that the modern administrative state requires agency independence for technocratic competence and that the reading represents executive aggrandizement rather than faithful constitutional interpretation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, public_administration_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, president).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes accountability for executive action in a single elected official, eliminating fragmentation of authority across multiple agency heads and ensuring a unified chain of command responsive to electoral outcomes.
% TRANSFER_FUNCTION: Transfers removal authority and policy control from independent agency heads, Congress, and the judiciary to the President, consolidating executive power in the White House.
% ABSENT_VOICES: Career civil servants, regulated publics benefiting from technocratic insulation, and future administrations that might prefer agency independence are structurally underrepresented in the originalist constitutional debate; their dependence on for-cause removal is treated as a constitutional error rather than a policy choice.
% DISAPPEARANCE_RATIONALE: If the unitary executive reading vanished as a live constitutional principle, independent agencies would retain or regain for-cause removal protections, presidential control over the administrative state would recede, and the architecture of the modern regulatory state would rest on more secure legislative foundations. The distribution of power between branches would shift measurably.
% FOUNDING_PROBLEM: The post-Reconstruction and New Deal growth of the administrative state created a fragmented executive with multiple independent agency heads not directly accountable to the President, raising concerns about democratic accountability and chain-of-command.
% FOUNDING_PROBLEM_CORROBORATION: Unitary executive advocates attest that unchecked bureaucratic power is the live problem. Administrative law scholars and agency leadership outside the benefiting party attest that the problem was solved by existing political controls and that the reading now functions as executive aggrandizement; corroboration from congressional committees and independent agencies supports the contested status.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.75) because the constraint strips long-standing statutory protections and concentrates control over a vast administrative apparatus. Suppression is high (0.78) because the reading's persistence depends on active judicial enforcement (e.g., Seila Law, Collins v. Yellen) and presidential legal action to overcome congressional and agency resistance. Theater is moderate-high (0.50) because constitutional argumentation about original meaning increasingly performs as cover for raw power consolidation. Accessibility collapse is high (0.72): once the reading is entrenched, the alternative of independent agencies appears constitutionally illegitimate. Resistance is moderate-high (0.65) due to institutional pushback from Congress, agencies, and functionalist scholars. Measurements are aligned on a single time grid spanning the theory's rise from academic novelty to enforceable doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The President and DOJ experience this reading as constitutional restoration and democratic accountability; independent agencies and Congress experience it as executive aggrandizement that strips their constitutional powers; the judiciary experiences it as a loss of review authority. The engine computes this divergence from the structural dataâbeneficiary declarations for the executive seats and victim declarations for the agency, legislative, and judicial seatsârather than from any reconciled claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and executive legal apparatus are declared beneficiaries, deriving low directionality (subsidy/expanded authority). Independent agency heads, the federal judiciary, and Congress are declared victims, deriving high directionality (bearing the costs of removed independence and reduced constitutional authority). The analytical observer occupies a neutral analytical seat with no directional stake. No override is necessary because the structural derivation correctly captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by satisfying the Tangled Rope gate: it carries a genuine coordination function (unified democratic accountability) and an asymmetric extraction function (stripping agency independence and judicial review). Active enforcement through the courts and OLC is required to hold the structure, distinguishing it from a pure Rope. If the coordination story were taken at face value without the victim set, it would misclassify as benign coordination; if the extraction were taken without the accountability rationale, it would misclassify as a pure Snare. The structural data capture both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_or_political_construction,
    'Is the unitary executive reading a recovery of the Constitution''s original meaning, or a modern political construction leveraging selective historical evidence?',
    'Comprehensive historical-legal review of founding-era evidence on removal practice and executive control, conducted by scholars outside the unitary executive advocacy network.',
    'If the historical evidence is weak or ambiguous, the reading''s empirically_contingent grounding collapses and its classification shifts toward Snare (coordination story as cover for extraction). If the evidence is robust, the Tangled Rope classification holds with a stronger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_or_political_construction, empirical, 'Whether the reading rests on genuine historical fact or selective construction').

omega_variable(
    judiciary_role_erosion,
    'Does the unitary executive reading structurally erode the judiciary''s power to police separation of powers, or does it merely shift the locus of judicial review?',
    'Comparative doctrinal analysis of judicial review rates in removal-power cases before and after the reading''s ascendancy, measuring the judiciary''s actual review capacity.',
    'If the judiciary loses meaningful review authority, the victim set is correctly broadened; if review merely shifts to different doctrinal frames, the judiciary''s payer status is overstated and should be reclassified to observer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_role_erosion, conceptual, 'Whether the judiciary is a genuine victim of this reading or a displaced reviewer').

omega_variable(
    kernel_reading_boundary,
    'How does the structural classification change if the same constitutional text is read through formalist (strict boundary) or functionalist (flexible balance) lenses rather than the unitary executive lens?',
    'Cross-reading comparison of beneficiary/victim structures and epsilon values across the three sibling constraints in the separation_of_powers_text family.',
    'If the formalist or functionalist readings produce substantially lower extraction with different victim profiles, the kernel is confirmed as a genuine commitment system with multiple constraint instantiations; if all readings converge on high extraction, the kernel itself may be intrinsically extractive regardless of reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural sensitivity of classification to reading choice within the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__unitary_executive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sepa_tr_t8, separation_of_powers_text__unitary_executive_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(sepa_tr_t16, separation_of_powers_text__unitary_executive_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(sepa_tr_t24, separation_of_powers_text__unitary_executive_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(sepa_tr_t32, separation_of_powers_text__unitary_executive_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__unitary_executive_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(sepa_tr_t44, separation_of_powers_text__unitary_executive_reading, theater_ratio, 44, 0.5).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sepa_be_t8, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(sepa_be_t16, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(sepa_be_t24, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(sepa_be_t32, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(sepa_be_t44, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 44, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sepa_su_t8, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(sepa_su_t16, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(sepa_su_t24, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(sepa_su_t32, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(sepa_su_t44, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 44, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, functionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the separation_of_powers_text kernel. The formalist reading emphasizes strict inter-branch boundaries and nondelegation; the functionalist reading permits overlapping authority under intelligible principles; this unitary executive reading concentrates all executive power in the President and treats independent agencies as unconstitutional. Each reading has a distinct beneficiary/victim structure and epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
