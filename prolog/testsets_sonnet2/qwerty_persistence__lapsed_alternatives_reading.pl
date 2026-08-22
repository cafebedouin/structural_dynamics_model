% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__lapsed_alternatives_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__lapsed_alternatives_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: qwerty_persistence__lapsed_alternatives_reading
 *   human_readable: QWERTY Keyboard Layout as Lapsed-Alternatives Coordination Standard
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout became and remains the dominant standard not,
 *   under this reading, because any party actively suppresses alternatives to
 *   protect sunk capital, but because a shared layout has genuine
 *   coordination value: skill transfer, training materials, and manufacturing
 *   all benefit from convergence on a single standard, and rival layouts
 *   (Dvorak and others) simply never reached the adoption threshold needed to
 *   displace it. This is the lapsed_alternatives_reading of the
 *   qwerty_persistence kernel — it is deliberately generated as a clean,
 *   self-contained constraint with its own epsilon, its own beneficiary
 *   structure (diffuse, symmetric, non-concentrated), and no victim set. The
 *   sibling reading (incumbent_preservation_reading) is a different
 *   constraint entirely, with a concentrated beneficiary class defending the
 *   standard; this reading does not describe, average with, or hedge against
 *   that sibling — see kernel_context and the omega variables for where the
 *   two readings diverge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__lapsed_alternatives_reading, 0.18).
domain_priors:suppression_score(qwerty_persistence__lapsed_alternatives_reading, 0.12).
domain_priors:theater_ratio(qwerty_persistence__lapsed_alternatives_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qwerty_persistence__lapsed_alternatives_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__lapsed_alternatives_reading, rope).
narrative_ontology:human_readable(qwerty_persistence__lapsed_alternatives_reading, "QWERTY Keyboard Layout as Lapsed-Alternatives Coordination Standard").
narrative_ontology:topic_domain(qwerty_persistence__lapsed_alternatives_reading, "technology_history/industrial_standards/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__lapsed_alternatives_reading, 'a8643a14-0abc-4f3c-97ee-e799371215c6').
narrative_ontology:cs_kernel_codification('a8643a14-0abc-4f3c-97ee-e799371215c6', distributed).
narrative_ontology:cs_authority_grounding('a8643a14-0abc-4f3c-97ee-e799371215c6', distributed).
narrative_ontology:cs_reading_relation('a8643a14-0abc-4f3c-97ee-e799371215c6', qwerty_persistence__incumbent_preservation_reading, coexists_with).
narrative_ontology:cs_axiom('a8643a14-0abc-4f3c-97ee-e799371215c6', foundational, persistence_explained_by_coordination_threshold_not_agency).
narrative_ontology:cs_axiom_status(persistence_explained_by_coordination_threshold_not_agency, holdable).
narrative_ontology:cs_axiom_grounding('a8643a14-0abc-4f3c-97ee-e799371215c6', persistence_explained_by_coordination_threshold_not_agency, empirically_contingent).
narrative_ontology:cs_axiom('a8643a14-0abc-4f3c-97ee-e799371215c6', secondary, switching_costs_are_symmetric_across_adopters).
narrative_ontology:cs_axiom_status(switching_costs_are_symmetric_across_adopters, holdable).
narrative_ontology:cs_axiom_grounding('a8643a14-0abc-4f3c-97ee-e799371215c6', switching_costs_are_symmetric_across_adopters, empirically_contingent).
narrative_ontology:cs_reference_frame('a8643a14-0abc-4f3c-97ee-e799371215c6', network_coordination_equilibrium).
narrative_ontology:cs_drift_state('a8643a14-0abc-4f3c-97ee-e799371215c6', contemporary_digital_keyboard_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a8643a14-0abc-4f3c-97ee-e799371215c6', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, typists_and_manufacturers_network).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, new_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__lapsed_alternatives_reading, keyboard_hardware_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence__lapsed_alternatives_reading, new_typists).
narrative_ontology:constraint_vindicates(qwerty_persistence__lapsed_alternatives_reading, network_coordination_value_of_shared_typing_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The entire population of typists, typewriter/keyboard manufacturers, training programs, and software input systems that jointly rely on one shared key layout so that skills, muscle memory, hardware, and instructional materials all transfer across devices and employers. No single member decided the layout is optimal; everyone benefits from everyone else using the same one, and the benefit exists only because adoption is nearly universal.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, typists_and_manufacturers_network, beneficiary,
    moderate, generational, constrained, global).

% Designers of competing layouts (Dvorak and others) built systems claiming ergonomic or speed advantages. Their proposals never reached the adoption threshold needed to create a rival coordination network, so the alternatives lapsed for lack of a critical mass of co-adopters, not because any party suppressed them. They have no current forum in which to press the case; the market for layout-switching cleared decades ago.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, alternative_layout_designers, excluded,
    powerless, biographical, trapped, national).

% Each new typist bears the one-time learning cost of the standard layout rather than a hypothetically more efficient one, but immediately gains the ability to use any keyboard, any job, any shared computer without retraining. The cost is symmetric across all new entrants — no group is singled out to pay it and no group is exempted.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, new_typists, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__lapsed_alternatives_reading, new_typists, beneficiary).

% Manufacture keyboards to the standard layout because doing otherwise would produce a product few could use without retraining; they could switch designs at will (physical layout is cheap to change), but coordination value, not sunk capital, keeps them aligned with the incumbent standard. Entry of a manufacturer offering an alternative layout is not blocked by anyone; it simply finds no buyer pool because no alternative achieved network critical mass.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, keyboard_hardware_manufacturers, beneficiary,
    organized, generational, mobile, global).

% Study why some standards persist and others lapse, evaluating switching-cost models against capture models to determine which historical account best fits the adoption-curve evidence for typewriter and keyboard layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence__lapsed_alternatives_reading, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__lapsed_alternatives_reading, diffuse).
narrative_ontology:fixing_cost_class(qwerty_persistence__lapsed_alternatives_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, near-universal key layout lets typing skill, training materials, muscle memory, and hardware transfer freely across employers, devices, and borders — solving the coordination problem of a fragmented input-skill market.
% TRANSFER_FUNCTION: No systematic transfer from a payer class to a beneficiary class; the arrangement moves nothing extractive — it moves compatibility. Learning cost is paid once by each new entrant and offset by universal interoperability; no party collects a rent from another party's payment of that cost.
% ABSENT_VOICES: Designers of superior alternative layouts had their case heard in the market for adoption and lost it to the coordination problem: even users persuaded of an alternative's individual superiority could not unilaterally benefit without a critical mass of co-adopters, employers, and manufacturers switching with them. Their absence from the current standard is a coordination-threshold outcome, not an exclusion by an incumbent defender.
% DISAPPEARANCE_RATIONALE: If the standard layout vanished overnight, existing typists' skills would be stranded and manufacturers would need to re-coordinate around a new default — real short-term disruption. But under this reading, a new equilibrium would re-form quickly around whichever layout reaches critical mass first, because nothing but coordination value is holding the current layout in place; there is no beneficiary class actively defending it against a superior alternative. Whether the disruption counts as 'world rearranges' or 'world briefly reshuffles into an equivalent arrangement' is exactly the fact this reading and its sibling dispute.
% FOUNDING_PROBLEM: Early typewriter mechanisms and typing instruction needed one shared key arrangement so operators trained on one machine could use any machine, and so manufacturers could produce interchangeable, teachable products instead of a fragmented market of incompatible devices.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians studying network-effect standards (independent of typewriter or keyboard manufacturers) attest that switching costs and coordination thresholds, not active defense by an incumbent beneficiary, best explain the layout's persistence in comparative studies of standards competitions across multiple industries; no manufacturer or typing-instruction body is required as a corroborating source under this reading because this reading holds there is no concentrated beneficiary needing to defend the arrangement.
narrative_ontology:disappearance_verdict(qwerty_persistence__lapsed_alternatives_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence__lapsed_alternatives_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__lapsed_alternatives_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__lapsed_alternatives_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__lapsed_alternatives_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__lapsed_alternatives_reading_tests).
:- end_tests(qwerty_persistence__lapsed_alternatives_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) and nearly flat over the century-scale interval because, under this reading, the cost anyone bears is the one-time switching/learning cost inherent to any coordination standard, not a rent captured by a concentrated party. Suppression is low (0.12): nothing prevents an alternative from displacing QWERTY except the practical difficulty of assembling a critical mass of co-adopters, which is a coordination problem, not coercion. Accessibility collapse is moderate-high (0.62) because once the network effect took hold, switching costs did functionally foreclose most individuals' practical ability to benefit from an alternative even absent enforcement — this is the honest coordination-standard signature, not evidence of suppression. Resistance is low (0.2): there is little active resistance to the standard because there is no extraction to resist under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   No concentrated beneficiary is declared under this reading; the beneficiary group named (typists_and_manufacturers_network) is the diffuse network itself, not an extracting party. New typists pay the one-time learning cost but are simultaneously beneficiaries of the same network they join — the cost and benefit accrue to the same population, which is the rope signature (net beneficiaries, no suppressed alternatives, minimal coercive overhead) rather than the tangled-rope or snare signature the sibling reading would produce.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists the temptation to reclassify a coordination-value standard as extraction just because switching became costly over time. Rising switching costs are the natural, expected signature of a genuine coordination good achieving network effects — they are not, by themselves, evidence of a beneficiary actively defending an inefficient arrangement. Under this reading, the founding problem (fragmented, non-interoperable typing skill and hardware) is still live in the sense that any candidate replacement layout would face exactly the same coordination threshold problem that QWERTY itself once solved and that alternatives failed to clear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_capture_explanation,
    'Is QWERTY''s persistence better explained by coordination value and lapsed critical mass among alternatives (this reading), or by active defense of the standard by capital-invested incumbents (the sibling incumbent_preservation_reading)?',
    'Historical evidence on whether manufacturers, typing schools, or other incumbents took identifiable actions (lobbying, exclusive contracts, disparagement campaigns) to block alternative layouts from reaching adoption thresholds, versus evidence that alternatives simply failed to attract sufficient independent co-adopters despite a level playing field.',
    'If capture evidence dominates, this reading is the wrong lens for the historical episode and the sibling incumbent_preservation_reading is the structurally accurate constraint; if coordination-failure evidence dominates, this reading holds and the sibling overstates beneficiary agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_capture_explanation, empirical, 'Whether QWERTY persistence is a coordination-lapse story or a capture story — the central kernel-level dispute.').

omega_variable(
    sibling_reading_delineation,
    'Where exactly does the disagreement between this reading and incumbent_preservation_reading live — in the beneficiary set, in the epsilon value, or in both?',
    'Compare the two stories'' base_properties.beneficiaries and base_properties.extractiveness directly: this reading declares a diffuse network beneficiary and epsilon ~0.18; the sibling would declare a concentrated incumbent beneficiary and a substantially higher epsilon reflecting active switching-cost imposition as an extraction mechanism.',
    'Clarifies that the two readings are not a measurement dispute over one constraint but two structurally distinct constraints sharing a kernel — consistent with the epsilon-invariance principle: measuring the same historical episode through a coordination lens versus a capture lens yields genuinely different epsilon values, which is the signal that decomposition into two stories was correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delineation, conceptual, 'Documents the structural disagreement location between the two kernel readings for cross-reference.').

omega_variable(
    empirical_dvorak_evidence_status,
    'Does controlled empirical evidence support Dvorak or other alternative layouts as meaningfully faster/more ergonomic, or is the claimed superiority itself contested/overstated?',
    'Review of controlled typing-speed studies (including contested Navy studies attributed to Dvorak''s own advocacy) versus independent replications.',
    'If alternative superiority is itself weak or unproven, the lapsed_alternatives_reading gains additional support (there was never a strong efficiency case that failed to displace QWERTY, just an ordinary coordination equilibrium); if superiority is robust, the puzzle of non-adoption becomes sharper and marginally favors investigation of capture explanations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_dvorak_evidence_status, empirical, 'Whether alternative layouts'' claimed superiority is empirically robust, bearing on how puzzling non-adoption actually is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__lapsed_alternatives_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 60, 0.07).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence__lapsed_alternatives_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence__lapsed_alternatives_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence__lapsed_alternatives_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__lapsed_alternatives_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__lapsed_alternatives_reading, 0.05).
narrative_ontology:affects_constraint(qwerty_persistence__lapsed_alternatives_reading, qwerty_persistence__incumbent_preservation_reading).

% DUAL FORMULATION NOTE:
% This story and qwerty_persistence__incumbent_preservation_reading are sibling readings of the qwerty_persistence kernel, decomposed per the epsilon-invariance principle: the two readings assign structurally different epsilon values (this reading ~0.18, reflecting symmetric switching costs with no concentrated beneficiary; the sibling reading is expected to assign a substantially higher epsilon reflecting active incumbent rent extraction) because they are answering different structural questions about the same historical episode, not measuring the same constraint from two angles. Each carries its own beneficiary/victim structure and its own claimed_type; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
