% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Apex-Initiated Commitment Cascade with Fringe Validation Gate
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   Across state-formation episodes — codification campaigns, religious
 *   settlements, national-language policies, civic ritual programs — new
 *   commitments are initiated at the apex and carried downward through
 *   administrative strata, but they stabilize only where fringe actors adapt
 *   the commitment to local idiom and lend it credibility the center cannot
 *   manufacture. The center harvests the resulting legitimacy and uniformity;
 *   peripheral validators supply the interpretive labor; subject populations
 *   absorb compliance costs for commitments they did not choose; holders of
 *   displaced commitments face suppression or absorption. This file
 *   instantiates ONLY the hybrid_cascade_reading of the kernel
 *   state_commitment_installation_mechanism: the contest with the sibling
 *   readings is routed to omega variables and cs_structure, never averaged
 *   into epsilon. Per DP-001, this reading is one clean constraint with one
 *   stable epsilon over the standing arrangement it describes. KEY AGENTS (by
 *   structural relationship): - apex_state_authority: Primary beneficiary and
 *   agenda-setter (institutional/arbitrage) — initiates commitments, collects
 *   stabilized legitimacy - central_bureaucratic_elites: Secondary
 *   beneficiary (powerful/constrained) — careers ride on the cascade -
 *   fringe_validators_local_elites: Dual-positioned stabilizer
 *   (organized/constrained) — pays interpretive labor, collects intermediary
 *   standing - subject_peripheral_populations: Primary target
 *   (moderate/trapped) — bears compliance costs - rival_commitment_holders:
 *   Excluded alternative-holders (moderate/trapped) — bear displacement -
 *   historical_sociologists: Analytical observer — sees the full cross-case
 *   structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.66).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Apex-Initiated Commitment Cascade with Fringe Validation Gate").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, '8ee91c8f-ffda-4130-97a3-6556a1c7fd96').
narrative_ontology:cs_kernel_codification('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', distributed).
narrative_ontology:cs_authority_grounding('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', distributed).
narrative_ontology:cs_reading_relation('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', state_commitment_installation_mechanism__exogenous_imposition_reading, forecloses).
narrative_ontology:cs_axiom('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', foundational, fringe_validation_necessary_for_stabilization).
narrative_ontology:cs_axiom_status(fringe_validation_necessary_for_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', fringe_validation_necessary_for_stabilization, empirically_contingent).
narrative_ontology:cs_axiom('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', foundational, apex_initiation_precedes_and_conditions_fringe_adoption).
narrative_ontology:cs_axiom_status(apex_initiation_precedes_and_conditions_fringe_adoption, holdable).
narrative_ontology:cs_axiom_grounding('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', apex_initiation_precedes_and_conditions_fringe_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', two_phase_apex_initiated_fringe_stabilized_cascade).
narrative_ontology:cs_drift_state('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', contemporary_comparative_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8ee91c8f-ffda-4130-97a3-6556a1c7fd96', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_state_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, central_bureaucratic_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, subject_peripheral_populations).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, rival_commitment_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_validators_local_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_validators_local_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the commitment at the center — a legal code, an official creed, a national language of record, a civic calendar — and staffs the administrative ladder that carries it outward. Judges success by whether distant provinces come to treat the commitment as ordinary. Can reframe or replace a commitment that fails to take, and can shift resources between regions, so its position rarely depends on any single locality.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Staff the intermediate offices through which the commitment travels: prefects, inspectors, examiners, editors of the official gazette. Careers, pensions, and rank ride on the cascade continuing to flow. Leaving the administration means forfeiting position and income accumulated inside it.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, central_bureaucratic_elites, beneficiary,
    powerful, biographical, constrained, national).

% Provincial magistrates, parish clergy, village schoolteachers, and local notables who translate the center's commitment into the idiom of their districts — adjusting ritual, softening penalties, blending it with custom so neighbors accept it. The interpretive work is largely unpaid and expected as a duty of office. In exchange they gain standing as indispensable intermediaries, occasional exemptions for their communities, and a voice in how the commitment reads locally.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_validators_local_elites, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_validators_local_elites, beneficiary).

% Live under commitments chosen far above them: new courts, new holidays, new languages of record, new obligations. Compliance arrives whether or not they consented; what varies locally is how harshly it bites. Moving away means leaving land, kin, and livelihood, so most households accommodate, and dissent surfaces mainly as recalcitrance, petition, or occasional revolt.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, subject_peripheral_populations, payer,
    moderate, generational, trapped, regional).

% Carry the commitments the cascade displaces — customary-law elders, minority congregations, regional-language writers, guilds with their own oaths. Their practices lose legal standing as the center's commitment consolidates; they may continue privately at rising cost or petition for recognition, but registration, schooling, and courts are arranged around the incoming commitment, not theirs.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, rival_commitment_holders, excluded,
    moderate, biographical, trapped, regional).

% Compare installation episodes across centuries and regions, coding which commitments took root, which collapsed, and what local conditions separated the two. Positioned outside every collecting and bearing seat; their comparative studies are the main public record of the mechanism's operating pattern.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, apex_state_authority).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of making one commitment operative across a heterogeneous territory in less than a generation: the apex supplies initiative and uniformity, the administrative cascade supplies reach, and fringe adaptation supplies local credibility that no central office could manufacture on its own.
% TRANSFER_FUNCTION: Moves the commitment downward from apex to province, and moves legitimacy and compliance upward from province to apex; it also moves interpretive labor downward onto local elites without commensurate payment, and moves the costs of displaced practices onto the groups that held them.
% ABSENT_VOICES: Holders of the displaced commitments would object that what the record calls 'validation' was often submission under administrative duress, and ordinary subjects would object that the 'local interpretation' recorded as consent was authored by their elites, not by them. Both sit largely outside the archives the mechanism generates, because the mechanism's paperwork records successful installation.
% DISAPPEARANCE_RATIONALE: Without the cascade-and-validate pattern, apex initiatives would die at the first administrative boundary unless enforced garrison-style, and fringe innovation would remain provincial for lack of a carrier. State formation would proceed either through slow endogenous accumulation or through open coercion, with correspondingly different maps of law, language, and allegiance.
% FOUNDING_PROBLEM: Early modern rulers found that edicts proclaimed at the center lost force at the village edge: commitments needed carriers who could make them credible to people with no independent reason to trust the capital.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting seats: comparative studies of failed legal transplants and missionary campaigns document installations that collapsed precisely where fringe validation was absent, and petitions and rebellion records from the periphery attest the costs from below. The apex's own chronicles alone would not settle the matter, and the corroborating sources predate or sit apart from the benefiting administrations.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial but bounded (0.62): the mechanism genuinely delivers territory-wide commitment coordination, yet it systematically transfers stabilization costs downward while the apex collects the legitimacy rent — the transfer is the arrangement's operating principle, not a side effect. Suppression (0.66) reflects that persistence depends on actively displacing rival commitments through courts, schooling, registration, and conscription; it is authored as a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater (0.27) is low-to-moderate: installation and adaptation are real work, but a growing ceremonial shell (allegiance rites, staged acclamations, anniversary liturgies) accumulates as commitments normalize. Accessibility_collapse (0.52) is mid-range: alternatives collapse locally once the cascade completes but persist during the process and survive in enclaves. Resistance (0.58) is real and documented — rebellion, recalcitrance, passive noncompliance — and is partially absorbed by design rather than eliminated. The claimed type (tangled_rope) and the metrics were authored independently: the claim rests on the joint presence of a genuine coordination function and asymmetric extraction; the metrics describe observed operation. The temporal series share one grid (T=0,10,20,30,40,50) so every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the apex seat the arrangement is statecraft it directs and profits from; from the bureaucratic seat it is a career structure; from the fringe-validator seat it is simultaneously burden and opportunity — unpaid labor exchanged for intermediary standing; from the subject-population seat it is unchosen obligation softened by local translation; from the rival-holder seat it is dispossession. Same structure, divergent experienced types; the engine computes this divergence from role, power, and exit data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The apex authority and bureaucratic elites sit near the beneficiary end (d approaching 0): the cascade subsidizes them with legitimacy and position, and the apex's arbitrage-grade exit lets it abandon any single failing commitment without systemic loss. Subject populations and rival-commitment holders sit near the full-target end (d approaching 1): they bear the transfer, and trapped exit amplifies their effective extraction. Fringe validators occupy the middle: primary payer declarations push d up, the secondary beneficiary declaration pulls it down, and constrained exit keeps them from the arbitrage pole. Scope is national at the apex and regional below, so verification difficulty — and hence the scope amplification of effective extraction — falls hardest on the regional seats that can least verify what the center actually requires.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the mechanism as pure coordination (because commitments demonstrably spread and stabilize) ignores who pays for stabilization — the interpretive labor and compliance costs flow downhill while legitimacy flows up. Reading it as pure imposition (because installation is unchosen) ignores that the fringe-validation phase is real co-production: the falling suppression_requirement series after the installation peak shows enforcement needs declining as local validation takes over, which a hardening snare would not exhibit. The founding problem — edicts dying at the village boundary — remains live wherever states install commitments, so no mandatrophy resolution is declared; the arrangement has not outlived its function, but its benefits and burdens remain asymmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    validation_voluntariness,
    'Was fringe validation a voluntary act of adaptation or a coerced performance extracted under administrative pressure?',
    'Compare validator behavior across installation episodes differing in enforcement intensity: where validators retained outside options (alternative patrons, emigration, tolerated parallel institutions), did validation persist at similar rates?',
    'If validation is largely coerced, the stabilization phase is extraction wearing cooperative dress and effective extraction rises sharply across the payer seats; if voluntary, part of the measured burden is the price of genuine co-production and the coordination half of the arrangement strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_voluntariness, empirical, 'Whether the fringe-validation phase reflects consent or duress.').

omega_variable(
    fringe_validator_net_position,
    'Do status rents and community exemptions compensate fringe validators for their unpaid interpretive labor, or do they net out as losers?',
    'Reconstruct validator careers at household level across several episodes: income, office-holding, and exemption flows weighed against labor hours and liability exposure.',
    'If validators net positive, the victim set narrows to subject populations and displaced holders; if negative, the mechanism draws its stability from the very agents it extracts from, raising epsilon and shifting the computed classification toward the extraction pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_validator_net_position, empirical, 'Net position of the dual-role stabilizer seat.').

omega_variable(
    reading_committer_structure,
    'This constraint is one reading (hybrid_cascade_reading) of the kernel state_commitment_installation_mechanism — how would adopting a sibling reading change the structural picture?',
    'Adopting endogenous_climb_reading removes the apex beneficiary seat and reassigns initiative to fringe demonstrators; adopting exogenous_imposition_reading deletes the fringe-validation gate and renders subject populations pure targets. The disagreement is located in causal primacy between apex installation and fringe validation.',
    'Sibling readings yield different beneficiary/victim sets and therefore different per-seat directionalities and classifications; epsilon over the standing arrangement shifts with whichever causal account the corpus adopts. This file''s values are valid only under the hybrid reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_structure, conceptual, 'Committer structure: kernel membership, sibling structural deltas, location of the disagreement.').

omega_variable(
    absorption_disposition,
    'Does absorption of partial resistance via local interpretation function as a safety valve that lowers total coercion, or as a laundering step that converts dissent into recorded consent?',
    'Trace post-absorption trajectories in districts whose objections were absorbed: did subsequent rebellion, litigation, or emigration fall because grievances were materially addressed, or recur because they were merely recorded?',
    'A safety-valve finding supports the coordination half of the arrangement and dampens measured suppression; a laundering finding raises suppression and shifts weight toward the extraction half, moving the computed type toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorption_disposition, conceptual, 'Disposition of the resistance-absorption mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(stat_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(stat_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement_basis(stat_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'how new state commitments gain legitimacy' covers three structurally distinct claims with different epsilon values, different beneficiary/victim structures, and different failure modes. This file instantiates the hybrid_cascade_reading only. The endogenous and exogenous readings are separate stories linked here; each cites historical cases that this reading reinterprets as two-phase hybrids, so the siblings function as upstream interpretive pressures on this reading's case selection, while this reading's two-phase evidence base feeds back into the terms in which the siblings must state their claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
