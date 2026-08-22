% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS Strict EEZ Exclusivity Reading (Article 57, 200nm, No Overlay)
 *   domain: international_law/maritime_geopolitics
 *
 * SUMMARY:
 *   This constraint instantiates the strict UNCLOS EEZ reading of the
 *   unclos_sovereignty_boundary kernel: under Article 57, coastal states
 *   possess exclusive sovereign rights within 200 nautical miles, and no
 *   overlay claimsâwhether historical usage, occupation, or non-ratifier
 *   customary assertionsâare valid. The reading treats the EEZ boundary as
 *   a formalized, ratifier-exclusive legal fact administered by ITLOS and
 *   Annex VII arbitration. Coastal ratifier states benefit from clear
 *   exclusive resource control; overlapping maritime claimants and historical
 *   rights holders bear the cost of exclusion. The constraint is actively
 *   enforced through international tribunal rulings, coast guard
 *   interdiction, and diplomatic pressure that suppresses alternative
 *   sovereignty frameworks. It presents itself as a coordination mechanism
 *   (clear rules prevent maritime chaos) but structurally operates as tangled
 *   rope: genuine coordination for ratifiers with uncontested coasts,
 *   asymmetric extraction for those whose historical or overlapping claims
 *   are voided.
 *
 * KEY AGENTS:
 *   - unclos_ratifier_coastal_states: Primary beneficiary (institutional/constrained exit) â collects exclusive resource rights
 *   - overlapping_maritime_claimants: Primary target (powerful/constrained exit) â bears exclusion from contested waters
 *   - historical_rights_holders: Secondary target (moderate/constrained exit) â bears voiding of pre-treaty usage claims
 *   - unclos_tribunal_system: Agenda setter (institutional/analytical exit) â administers and enforces the 200nm standard
 *   - non_ratifier_maritime_powers: Excluded party (powerful/mobile exit) â operates parallel customary framework outside strict reading
 *   - international_maritime_law_scholars: Analytical observer (analytical/analytical exit) â documents divergence between text and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.72).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.82).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS Strict EEZ Exclusivity Reading (Article 57, 200nm, No Overlay)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_geopolitics").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '32cb4834-8571-410e-8683-41c65c2f4af9').
narrative_ontology:cs_kernel_codification('32cb4834-8571-410e-8683-41c65c2f4af9', formalized).
narrative_ontology:cs_authority_grounding('32cb4834-8571-410e-8683-41c65c2f4af9', lineage).
narrative_ontology:cs_interpretation_layer_present('32cb4834-8571-410e-8683-41c65c2f4af9').
narrative_ontology:cs_reading_relation('32cb4834-8571-410e-8683-41c65c2f4af9', unclos_sovereignty_boundary__historical_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('32cb4834-8571-410e-8683-41c65c2f4af9', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('32cb4834-8571-410e-8683-41c65c2f4af9', foundational, eez_entitlement_derives_exclusively_from_unclos).
narrative_ontology:cs_axiom_status(eez_entitlement_derives_exclusively_from_unclos, holdable).
narrative_ontology:cs_axiom_grounding('32cb4834-8571-410e-8683-41c65c2f4af9', eez_entitlement_derives_exclusively_from_unclos, conventional).
narrative_ontology:cs_axiom('32cb4834-8571-410e-8683-41c65c2f4af9', foundational, two_hundred_nm_limit_overrides_historical_usage).
narrative_ontology:cs_axiom_status(two_hundred_nm_limit_overrides_historical_usage, holdable).
narrative_ontology:cs_axiom_grounding('32cb4834-8571-410e-8683-41c65c2f4af9', two_hundred_nm_limit_overrides_historical_usage, conventional).
narrative_ontology:cs_reference_frame('32cb4834-8571-410e-8683-41c65c2f4af9', unclos_formal_ratifier_framework).
narrative_ontology:cs_drift_state('32cb4834-8571-410e-8683-41c65c2f4af9', post_south_china_sea_arbitration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32cb4834-8571-410e-8683-41c65c2f4af9', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifier_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_maritime_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, historical_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive sovereign rights to explore, exploit, conserve and manage natural resources within 200 nautical miles of their baselines under UNCLOS. They benefit from legal clarity and can invoke ITLOS or Annex VII arbitration against encroachment. Their exit from the framework is constrained by treaty ratification and the diplomatic cost of abandoning a rules-based order they helped construct.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifier_coastal_states, beneficiary,
    institutional, generational, constrained, global).

% States with maritime claims that overlap with neighboring EEZs or with waters they historically used. Under the strict 200nm reading, their overlay claims are legally invalid, forcing them to accept coastal state exclusivity, enter bilateral negotiation from a weaker legal position, or face interdiction. They lose access to fisheries, seabed resources, and strategic maritime space they contest.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_maritime_claimants, payer,
    powerful, biographical, constrained, regional).

% States or communities asserting sovereign rights or usage privileges based on historical occupation, traditional fishing, or pre-UNCLOS practice. The strict reading renders these claims legally void, stripping them of formal standing to access traditional fishing grounds or resource deposits within another state's EEZ, regardless of how long they have used the waters.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, historical_rights_holders, payer,
    moderate, biographical, constrained, regional).

% Administers interpretation of UNCLOS Article 57 and maritime delimitation disputes through ITLOS and Annex VII arbitration panels. It issues binding rulings that enforce the 200nm exclusivity standard and invalidate overlay claims, thereby setting the legal agenda for how coastal state boundaries are recognized and contested.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_tribunal_system, agenda_setter,
    institutional, civilizational, analytical, global).

% Major naval powers that have not ratified UNCLOS but conduct freedom of navigation operations and assert customary international law positions. Their alternative legal frameworks are structurally excluded from the strict reading's ratifier-beneficiary structure, though they continue to operate in parallel through naval presence and bilateral pressure.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_maritime_powers, excluded,
    powerful, generational, mobile, global).

% Analyze the divergence between UNCLOS textual exclusivity and actual state practice, documenting how the strict reading interacts with historical rights, non-ratifier assertions, and grey-zone maritime activities that the formal framework does not cleanly capture.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifier_coastal_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, measurable rule for maritime boundary delimitation, preventing ad hoc territorial conflicts and establishing clear sovereign rights over resources within 200 nautical miles for ratifier coastal states.
% TRANSFER_FUNCTION: Moves exclusive resource extraction rights and territorial sea management authority from overlapping claimants, historical rights holders, and the international commons to coastal ratifier states.
% ABSENT_VOICES: Historical fishing communities with pre-UNCLOS usage rights, indigenous maritime peoples whose traditional waters fall within another state's EEZ, and non-ratifier states asserting customary freedom of navigation or historical occupation are structurally excluded from the strict reading's beneficiary framework.
% DISAPPEARANCE_RATIONALE: Without the strict 200nm limit and its exclusivity, maritime boundaries would revert to contested ad hoc claims, historical rights assertions, and force-based occupation. Coastal states would lose guaranteed exclusive resource zones, and overlapping claimants would regain legal standing for their overlay claims.
% FOUNDING_PROBLEM: Pre-UNCLOS maritime territoriality was chaotic, with conflicting claims, naval force as the primary boundary-setter, and no clear limit to coastal state expansion leading to enclosure of the high seas.
% FOUNDING_PROBLEM_CORROBORATION: Independent maritime historians and pre-UNCLOS legal scholarship attest to the chaotic pre-treaty environment. However, non-ratifier states and indigenous rights organizations attest that the strict 200nm solution overcorrected and displaced legitimate non-state-based ordering mechanisms; their testimony comes from outside the benefiting coastal state set.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the strict reading transfers substantial resource and territorial rights from overlapping and historical claimants to coastal states with no compensatory mechanism. Suppression (0.82) is higher still because the constraint's persistence depends on actively invalidating overlay claims through tribunal rulings and naval interdiction, not on voluntary compliance. Theater ratio (0.40) reflects moderate performative maintenance: coast guard patrols and legal filings partly signal commitment to the framework, but the underlying enforcement is functional. Accessibility collapse (0.80) is high because once the UNCLOS framework is accepted, historical and ad hoc alternatives collapse as legally cognizable. Resistance (0.65) reflects active pushback through grey-zone operations, non-ratifier naval presence, and persistent historical claims.
 *
 * PERSPECTIVAL GAP:
 *   The coastal state seat experiences the constraint as a protective legal entitlement that secures resources and prevents encroachment. The overlapping claimant seat experiences the identical constraint as an arbitrary legalized dispossession that voids their own claims. The engine computes this divergence from the same structural data: beneficiary role plus constrained exit yields low effective extraction for ratifiers; payer role plus constrained exit yields high effective extraction for claimants.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal ratifier states are the structural beneficiaries: they receive exclusive rights, low directionality, and subsidized coordination. Overlapping claimants and historical rights holders are the structural targets: they pay through exclusion and loss of access, high directionality. The tribunal system sits near symmetric in its administrative role but is not a target. Non-ratifier powers are excluded rather than coordinated; their structural relationship is external and their directionality is undefined within the strict reading's ratifier framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve pre-UNCLOS maritime chaosâa genuine coordination failure. For ratifiers with uncontested coasts, that founding problem remains live and the constraint functions as coordination. But for overlapping claimants and historical rights holders, the coordination function has atrophied into extraction: their exclusion is not a side effect of solving maritime chaos but a necessary feature of the strict reading's zero-overlay architecture. Classifying it as tangled rope rather than snare prevents mislabeling the real coordination enjoyed by ratifiers as pure cover, while still registering the asymmetric extraction imposed on excluded parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_suppressed_validity,
    'Do historical usage and occupation rights constitute a legitimate parallel sovereignty framework suppressed by UNCLOS formalism, or were they always subordinate to state treaty law?',
    'Comparative analysis of pre-UNCLOS state practice and indigenous maritime usage records against ITLOS rulings in South China Sea and similar arbitrations.',
    'If historical rights are independently valid, the strict reading''s suppression is higher than measured and its coordination function is cover for dispossession; if subordinate, the extraction is the necessary cost of legal clarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_rights_suppressed_validity, conceptual, 'Ambiguity over whether historical rights are genuinely foreclosed or were never legally operative.').

omega_variable(
    non_ratifier_customary_status,
    'Does the strict EEZ reading structurally exclude non-ratifier states, or do they operate under a parallel customary framework that the strict reading cannot assimilate?',
    'Examination of non-ratifier state practice and whether ITLOS and coastal states treat non-ratifier naval presence as legally distinct from overlay claims.',
    'If non-ratifiers are genuinely outside the constraint, the victim set is narrower than authored; if their customary framework is actively suppressed by the strict reading''s dominance, suppression and extraction are higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_ratifier_customary_status, empirical, 'Whether non-ratifier maritime powers are excluded parties or parallel operators.').

omega_variable(
    kernel_reading_scope_delta,
    'If the strict EEZ reading were relaxed to accommodate historical rights or non-ratifier enforcement, would the constraint decompose into a lower-extraction coordination mechanism or maintain its asymmetric structure?',
    'Comparative structural analysis of the sibling readings as separate constraint stories.',
    'Would determine whether the extraction is intrinsic to the 200nm exclusivity rule or contingent on the strict reading''s refusal to accommodate overlays.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_scope_delta, conceptual, 'Whether the strict reading''s exclusionary axioms are the source of extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_strict_eez_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(unclos_strict_eez_tr_t8, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(unclos_strict_eez_tr_t16, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(unclos_strict_eez_tr_t24, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(unclos_strict_eez_tr_t32, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(unclos_strict_eez_tr_t40, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(unclos_strict_eez_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(unclos_strict_eez_be_t8, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(unclos_strict_eez_be_t16, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(unclos_strict_eez_be_t24, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(unclos_strict_eez_be_t32, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(unclos_strict_eez_be_t40, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(unclos_strict_eez_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unclos_strict_eez_su_t8, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(unclos_strict_eez_su_t16, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(unclos_strict_eez_su_t24, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(unclos_strict_eez_su_t32, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(unclos_strict_eez_su_t40, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is the strict UNCLOS EEZ reading of the unclos_sovereignty_boundary kernel. It decomposes from the colloquial 'EEZ boundary' label by fixing the referent to the treaty-exclusive, 200nm, no-overlay-claims interpretation. Sibling readings share the same maritime spatial domain but instantiate different epsilon, beneficiary/victim structures, and authority groundings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
