% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Bottom-Up Norm Diffusion Preceding State Mandate (Endogenous Climb Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the endogenous-climb reading of a contested
 *   historical episode: a new social norm spread through voluntary, bottom-up
 *   adoption across communities before the state issued any formal mandate.
 *   Under this reading, the state's role was to ratify an accomplished social
 *   fact rather than to impose an unfamiliar rule by force. Enforcement costs
 *   stayed low, the adoption timeline was comparatively rapid because it rode
 *   existing social networks rather than administrative rollout, and
 *   resistance was minimal because by the time of the mandate the practice
 *   was already the local default in most communities. This is a clean,
 *   single reading: it does not attempt to average against the
 *   exogenous_override_reading (state coercion preceding legitimacy) or the
 *   hybrid_legitimation_reading (symbolic authority transfer plus
 *   institutional incentive) — those are separate constraints with their own
 *   ε values, linked here only through the kernel relationship.
 *
 * KEY AGENTS:
 *   - early_adopter_communities: Primary beneficiary (moderate/mobile) — converted early conformity into status and trade advantage
 *   - norm_entrepreneurs: Agenda-setter (organized/mobile) — modeled and promoted the practice ahead of state involvement
 *   - state_administrators: Beneficiary and secondary agenda-setter (institutional/arbitrage) — ratified an already-won social consensus at low cost
 *   - holdout_traditionalist_communities: Payer (powerless/constrained) — bore mild friction from nonconformity to an already-established norm
 *   - historians_of_state_formation: Analytical observer (analytical/analytical) — adjudicates sequencing evidence between competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Bottom-Up Norm Diffusion Preceding State Mandate (Endogenous Climb Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, '74c2569d-2965-470e-b776-5bb7f8794fe7').
narrative_ontology:cs_kernel_codification('74c2569d-2965-470e-b776-5bb7f8794fe7', distributed).
narrative_ontology:cs_authority_grounding('74c2569d-2965-470e-b776-5bb7f8794fe7', practice).
narrative_ontology:cs_interpretation_layer_present('74c2569d-2965-470e-b776-5bb7f8794fe7').
narrative_ontology:cs_reading_relation('74c2569d-2965-470e-b776-5bb7f8794fe7', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('74c2569d-2965-470e-b776-5bb7f8794fe7', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('74c2569d-2965-470e-b776-5bb7f8794fe7', foundational, legitimacy_precedes_mandate).
narrative_ontology:cs_axiom_status(legitimacy_precedes_mandate, holdable).
narrative_ontology:cs_axiom_grounding('74c2569d-2965-470e-b776-5bb7f8794fe7', legitimacy_precedes_mandate, empirically_contingent).
narrative_ontology:cs_axiom('74c2569d-2965-470e-b776-5bb7f8794fe7', secondary, state_as_ratifying_coordinator).
narrative_ontology:cs_axiom_status(state_as_ratifying_coordinator, holdable).
narrative_ontology:cs_axiom_grounding('74c2569d-2965-470e-b776-5bb7f8794fe7', state_as_ratifying_coordinator, conventional).
narrative_ontology:cs_reference_frame('74c2569d-2965-470e-b776-5bb7f8794fe7', decentralized_customary_convergence).
narrative_ontology:cs_drift_state('74c2569d-2965-470e-b776-5bb7f8794fe7', state_ratification_point, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('74c2569d-2965-470e-b776-5bb7f8794fe7', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_communities).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, state_administrators).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, holdout_traditionalist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, later_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopted the new norm voluntarily before any state mandate existed, gaining social standing, trade advantages, or status within emerging networks that rewarded early conformity. Their adoption produced the observable behavior that the state later ratified.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_communities, beneficiary,
    moderate, generational, mobile, regional).

% Merchants, local notables, or itinerant preachers who modeled and promoted the new norm ahead of any state involvement, accumulating reputational capital as the practice spread. They set the practical template the state mandate later codified rather than invented.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_entrepreneurs, agenda_setter,
    organized, generational, mobile, regional).

% Observed the norm's spread reach a threshold of popular acceptance and then issued a mandate formalizing what was already widely practiced, gaining low-cost legitimacy and administrative uniformity by ratifying an accomplished social fact rather than imposing an unfamiliar one.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, state_administrators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, state_administrators, agenda_setter).

% Continued older practices after the new norm had become the de facto standard elsewhere. Once the state mandate followed popular acceptance, holdouts found themselves administratively out of step, facing mild social and bureaucratic friction even though the state applied little direct coercion — the cost fell on nonconformity to an already-won social consensus.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, holdout_traditionalist_communities, payer,
    powerless, biographical, constrained, local).

% Inherit the norm as unremarkable background practice, benefiting from the low-conflict transition without having participated in either the grassroots adoption or the later ratification.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, later_generations, beneficiary,
    moderate, civilizational, analytical, national).

% Study the sequencing of adoption versus mandate to distinguish genuine bottom-up legitimation from retrospectively-narrated consent that actually followed coercive imposition. Their evidentiary standards determine whether this reading or a rival reading of the same historical episode is sustained.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historians_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine coordination problem: once a critical mass of communities had independently adopted a norm, formal state ratification reduced remaining ambiguity about which standard applied nationally, lowering transaction costs for those already complying and providing a single reference point for administrators.
% TRANSFER_FUNCTION: Little is transferred in the strict sense — the mandate ratifies an existing distribution of practice. What moves is administrative recognition and residual friction: holdout communities absorb the small cost of being out of step with a norm they did not choose to adopt early, while early adopters and norm entrepreneurs convert their head start into durable status.
% ABSENT_VOICES: Holdout traditionalist communities had voices during the period of gradual diffusion but by the time of state mandate the practical decision was already made by weight of numbers; their objections register in the historical record mainly as complaint about being 'left behind' rather than resistance capable of altering the outcome.
% DISAPPEARANCE_RATIONALE: If the mandate were struck down, most practice would likely continue unchanged because the norm's authority rests on prior popular acceptance rather than on the state instrument itself — but administrative uniformity (recordkeeping, legal recognition, dispute resolution) would fragment along whatever residual regional variation the earlier bottom-up diffusion had not yet smoothed over.
% FOUNDING_PROBLEM: Divergent local practice created friction in trade, legal recognition, and social interaction across regions; a critical mass of communities had already converged on a new norm through independent adoption, but the absence of a formal standard left ambiguity for administrators and for communities at the margins of the emerging consensus.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians studying trade-ledger and parish-record adoption curves attest that documented behavioral convergence substantially predates the state instrument in the surviving record; this corroboration comes from outside both the state administrators and the norm entrepreneurs who benefit from the endogenous-climb narrative, though the record is incomplete for the earliest diffusion period.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18 at interval end) because under this reading no party is coerced into adopting the norm against its interest — the state's mandate follows rather than manufactures compliance. Suppression is low (0.12) for the same reason: there is little active machinery holding the arrangement together because the underlying behavior was already voluntary. Theater ratio stays low and rises only slightly (0.05 to 0.15) reflecting a modest amount of ceremonial state ratification (proclamations, registries) layered atop genuine prior practice, not substituting for it. Accessibility collapse is moderate (0.35) — holdout communities retained some room to resist but faced increasing social cost as the norm became near-universal. Resistance is authored very low (0.10), consistent with a reading where the coordination story is not cover for coercion but a reasonably accurate description of the mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the norm entrepreneur and early adopter seats, this looks like pure coordination: a practical standard emerged from decentralized experimentation and the state simply caught up. From the holdout seat, the experience is one of gradually narrowing social space rather than any single coercive act — a genuinely different phenomenology from what the exogenous_override_reading would assign to the same nominal historical event. The engine computes these divergently from the structural data; this reading's claim is that the divergence remains mild rather than sharp.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters and norm entrepreneurs sit near the full-beneficiary end: they bore the risk of early adoption and captured the reputational and network returns. State administrators are also beneficiaries under this reading — they gain administrative uniformity and legitimacy cheaply by ratifying rather than manufacturing consent. Holdout traditionalist communities are the sole named victims, and even their cost is diffuse and social rather than coercively imposed — this is what distinguishes the endogenous_climb_reading from the exogenous_override_reading, where the same holdouts would face direct state coercion rather than social friction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented local practice creating friction for trade and administration) is authored as dead — the practice had already converged before the mandate, so the mandate's job was largely done at issuance rather than an ongoing extractive function. This reading forecloses a mandatrophy narrative in which the state continues to extract compliance value long after the coordination problem resolved, because under this reading the state never held the coercive leverage that would make continued extraction possible — its authority derived from ratifying consensus, not commanding it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sequencing_evidence_ambiguity,
    'Does the surviving historical record actually establish that popular adoption preceded state mandate, or is the sequencing itself a retrospective narrative constructed after the fact by parties who benefit from an endogenous-legitimacy story?',
    'Fine-grained dating of adoption evidence (trade ledgers, parish records, correspondence) against the date of the formal state instrument, cross-checked by historians without a stake in either the state''s legitimacy claim or the affected communities'' self-narrative.',
    'If sequencing evidence is weak or reconstructed after the fact, this reading collapses toward the hybrid_legitimation_reading or even the exogenous_override_reading, since the appearance of bottom-up consent could itself be a legitimating narrative imposed after coercive adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sequencing_evidence_ambiguity, empirical, 'Whether the bottom-up-before-mandate sequence is empirically established or narratively constructed.').

omega_variable(
    holdout_coercion_boundary,
    'Is the friction experienced by holdout traditionalist communities purely social (peer pressure, market disadvantage) as this reading claims, or did informal local enforcement (guild exclusion, communal sanction) supply coercive force that the state mandate merely formalized rather than created?',
    'Local court and guild records documenting sanctions against holdouts prior to the state mandate; if substantial informal coercion predates the mandate, the ''low suppression'' claim of this reading weakens.',
    'Evidence of pre-mandate informal coercion would shift this reading''s suppression score upward and blur the boundary with the hybrid_legitimation_reading, which explicitly allows for institutional incentive alongside symbolic authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(holdout_coercion_boundary, empirical, 'Whether holdout costs were purely social or partly coercive prior to state involvement.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice to read this historical episode through the endogenous_climb frame (versus exogenous_override or hybrid_legitimation) itself a defensible empirical judgment, or does it reflect which sources survived and whose interests they served?',
    'Triangulation across independent source traditions (state archives, merchant records, religious institution records, oral tradition where available) to see whether the climb narrative holds up when state-authored sources are down-weighted.',
    'If state-authored sources dominate the evidentiary base for the climb narrative, the apparent low-coercion, low-resistance profile of this reading may partly reflect selection in what got recorded rather than what happened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the reading selection is source-artifact-driven or genuinely evidence-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(impo_tr_t8, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(impo_tr_t24, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(impo_tr_t32, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 32, 0.13).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(impo_be_t8, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement(impo_be_t24, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement(impo_be_t32, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(imposition_mechanism_kernel__endogenous_climb_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__endogenous_climb_reading, 0.1).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the imposition_mechanism_kernel, decomposed per the ε-invariance principle because the natural-language question 'how did this norm achieve legitimacy' covers structurally distinct claims with different ε values: this reading (endogenous_climb) authors ε ≈ 0.18 (low extraction, coordination-dominant); the exogenous_override_reading would author high ε (coercion-dominant, state as extractor); the hybrid_legitimation_reading sits between, mixing symbolic authority transfer with institutional incentive. All three share the same nominal historical subject matter but are authored as separate stories with separate stakeholder sets, separate victim declarations, and separate claimed types, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
