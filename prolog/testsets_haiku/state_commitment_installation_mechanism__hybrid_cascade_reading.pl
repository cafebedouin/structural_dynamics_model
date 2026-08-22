% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: State Commitment Installation via Hybrid Cascade (Apex-Down + Fringe-Validation)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint captures the mechanism by which a state center installs
 *   new commitments (legal reforms, doctrinal shifts, administrative
 *   procedures) uniformly across its jurisdiction while maintaining
 *   legitimacy. The mechanism is hybrid: the apex authority imposes the
 *   commitment downward through the institutional hierarchy, but the
 *   commitment only stabilizes when fringe actors—local officials,
 *   implementers, and populations—validate it by adopting it as locally
 *   legitimate. Until validation occurs, suppression is high because forced
 *   compliance dominates. Once validation occurs, suppression recedes but
 *   extraction persists as the reshaping of local authority and autonomy that
 *   the commitment entails. The hybrid cascade reading positions the apex as
 *   the initiator and the fringe as the legitimator, with interpretive
 *   intermediaries brokering the translation. This stands in contrast to an
 *   endogenous climb reading (legitimacy climbs from fringe success) and an
 *   exogenous imposition reading (legitimacy comes solely from apex authority
 *   without needing fringe validation).
 *
 * KEY AGENTS:
 *   - state_apex: institutional authority, agenda-setter, arbitrage-level exit, collects compliance resources
 *   - interpretive_intermediaries: organized brokers, constrained exit, mediate between apex and fringe, benefit from enhanced authority during cascade
 *   - fringe_implementers: moderate power, local scope, constrained exit, bear disruption cost, provide the validation that stabilizes the cascade
 *   - local_authority_holders: moderate power, identity-locked exit, experience displacement of autonomy, resist but are reframed by intermediaries
 *   - general_population: powerless, trapped exit, subject to the commitment, benefit from coordination but pay disruption cost
 *   - rival_institutional_orders: organized, excluded from installation process, their domains subordinated by the cascade
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "State Commitment Installation via Hybrid Cascade (Apex-Down + Fringe-Validation)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, 'b4e7eaf7-798a-4a56-a823-e8cbb955afd5').
narrative_ontology:cs_kernel_codification('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', distributed).
narrative_ontology:cs_authority_grounding('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', extraction).
narrative_ontology:cs_interpretation_layer_present('b4e7eaf7-798a-4a56-a823-e8cbb955afd5').
narrative_ontology:cs_reading_relation('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', state_commitment_installation_mechanism__exogenous_imposition_reading, influences).
narrative_ontology:cs_axiom('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', foundational, fringe_validation_structurally_necessary).
narrative_ontology:cs_axiom_status(fringe_validation_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', fringe_validation_structurally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', foundational, apex_initiation_precedes_fringe_adoption).
narrative_ontology:cs_axiom_status(apex_initiation_precedes_fringe_adoption, holdable).
narrative_ontology:cs_axiom_grounding('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', apex_initiation_precedes_fringe_adoption, conventional).
narrative_ontology:cs_reference_frame('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', apex_directed_institutional_hierarchy).
narrative_ontology:cs_drift_state('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', post_validation_stabilization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b4e7eaf7-798a-4a56-a823-e8cbb955afd5', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, interpretive_intermediaries).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_implementers).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, local_authority_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, general_population).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, interpretive_intermediaries).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, general_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates new commitments (legal reforms, doctrinal shifts, bureaucratic procedures) at the center and installs them downward through the institutional hierarchy. Claims the authority to set norms binding on subordinate actors. Benefits from the legitimacy and compliance the cascade produces if it stabilizes. Enforces initial compliance through administrative authority while waiting for fringe validation to naturalize the commitment.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex, agenda_setter,
    institutional, generational, arbitrage, national).

% Professional and institutional brokers (judges, clergy, scholars, educated administrators) who mediate between apex pronouncements and fringe implementation. They translate apex commitments into locally coherent interpretations, absorb resistance by reframing the new commitment as continuous with local practice. They benefit from the enhanced status and authority their interpretive role grants them during the cascade; they also bear the cognitive and political cost of managing the translation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, interpretive_intermediaries, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, interpretive_intermediaries, payer).

% Local officials, village magistrates, parish priests, customary leaders who must execute the apex commitment on the ground. They bear the cost of disruption to existing arrangements, local resistance, and the need to reinterpret the commitment to fit local conditions. Their validation—the adoption of the commitment as locally legitimate—is what stabilizes the cascade. Until validation occurs, they experience suppression (forced compliance with an externally imposed norm); after validation, suppression recedes but extraction (the reshaping of their authority and autonomy) persists.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_implementers, payer,
    moderate, biographical, constrained, local).

% Customary leaders, village elders, local nobles whose authority is rooted in pre-existing institutions and legitimacy claims. The apex commitment often displaces or subordinates their authority to state-installed norms. They experience the cascade as a loss of autonomous decision-making power. Their identity is fused with the local authority system, so exit means abandoning the social role itself. They resist the commitment; their resistance is absorbed by interpretive intermediaries who reframe the commitment as a refinement of, not replacement for, local authority.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, local_authority_holders, payer,
    moderate, biographical, identity_locked, local).

% Subject to the commitment as it is locally implemented. They benefit from whatever coordination the commitment provides (unified legal standards, expanded state protection, common normative framework); they pay through disruption of customary practice, loss of autonomous dispute resolution, and alignment with state norms that may conflict with local custom. Their exit options are territorial (migrate) or exit the jurisdiction entirely. Until the commitment is validated locally, they experience higher suppression and uncertainty.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, general_population, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, general_population, payer).

% Alternative legitimacy sources (church authority, merchant guild norms, customary law, tribal hierarchies) whose domains the apex commitment encroaches on or subsumes. They are excluded from the installation process itself—the apex does not negotiate with them, but enforces subordination. They would argue against the cascade if included; their exclusion is maintained by the apex's monopoly on enforcement authority during the cascade phase.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, rival_institutional_orders, excluded,
    organized, generational, trapped, national).

% Examines the cascade mechanism from outside the system: how new commitments propagate, where resistance forms, at what point local validation occurs, and whether the extraction persists after validation or is reabsorbed as legitimate coordination.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, historical_analyst, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the institutional change problem: new commitments must install uniformly to coordinate state-wide behavior while validating locally so they are perceived as legitimate rather than imposed. The cascade mechanism enables both via apex authority + fringe validation + intermediary translation.
% TRANSFER_FUNCTION: Transfers authority over local decision-making from customary/local sources to state-installed norms and to state-literate intermediaries who interpret those norms. Moves compliance resources (time, disruption, autonomy cost) from beneficiaries to fringe payers.
% ABSENT_VOICES: Rival institutional orders (religious, customary, guild-based authority) are structurally excluded from the installation process. Their resistance is managed by intermediaries but never directly heard. Local populations participate in adoption but rarely in design.
% DISAPPEARANCE_RATIONALE: Without the cascade mechanism, new commitments would install slowly (without apex authority to enforce uniformity) or would collapse (without fringe validation to stabilize them). State institutional change capacity would degrade to slow local evolution or conflict-driven rupture. The mechanism is the apparatus enabling rapid, relatively stable, state-wide institutional transformation.
% FOUNDING_PROBLEM: How can a centralizing state install uniform institutional commitments across jurisdictions with distinct local authorities and legitimacy sources without triggering resistance that collapses the reform or requires destructive enforcement?
% FOUNDING_PROBLEM_CORROBORATION: Historians of state formation (Eisenstadt, Tilly, Ertman) and historical sociologists studying institutional change document this as a persistent problem. Development economists and institutional reform practitioners encounter it in weak-state contexts where reform efforts fail because they lack either apex authority or fringe legitimacy. Scholars of comparative law and constitutional history (e.g., Mattei on civil law reception) trace the cascade mechanism in actual institutional change episodes.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness trajectory is non-monotonic: it rises from 0.45 to 0.62 over the first 15 time units (installation phase, high extraction as the commitment displaces local authority and disrupts custom), then plateaus (validation phase, 20–40, extraction stabilizes as the commitment is internalized but not eliminated). Suppression shows the reverse: it falls from 0.8 to 0.58 over the same period (forced compliance gives way to internalized legitimacy), then stabilizes. Theater ratio rises from 0.2 to 0.41 and plateaus: early in the cascade, the mechanism is mostly functional enforcement; later, a growing share is performative maintenance of the new commitment's legitimacy through ritual and narrative. The coercion grid shows the level-differentiated character of the cascade: structural suppression (apex authority) remains high and stable; organizational suppression (intermediary enforcement) declines sharply as interpretation succeeds; class and individual suppression follow, declining as the commitment is validated. Resistance shows the same pattern inverted: organizational and class resistance is highest in the early phase (local authorities and populations resisting) and declines as validation occurs; structural resistance (rival institutional orders) is low throughout because they are excluded from the mechanism entirely and cannot mount effective counter-pressure.
 *
 * PERSPECTIVAL GAP:
 *   The apex perceives the cascade as a rational, efficient solution to the institutional change problem: install uniformly, let intermediaries manage local translation, achieve state-wide coordination at moderate suppression cost. Fringe payers perceive it as an extractive imposition dressed up as coordination: their authority is displaced, their customs disrupted, and they are offered only the chance to interpret the displacement in terms that make it locally palatable. From the fringe payer seats, the coordination benefit accrues primarily to the apex and intermediaries; the extraction benefit is captured by the state and the professional classes, while the fringe bears the cost. The engine's per-seat classification computation will likely produce tangled_rope from the apex's seat (genuine coordination with asymmetric gains) and snare from fringe payer seats (the same structure experienced as extraction with suppression, mitigated only by local reframing). This divergence is the point: the cascade mechanism works by making the same arrangement look like different things to different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State apex: d near 0.0 (full beneficiary) — captures compliance resources and enhanced authority. Interpretive intermediaries: d near 0.3 (beneficiary, but with payer elements) — benefit from authority gain, but must bear political and cognitive cost of managing translation; some extraction passes through their hands. Fringe implementers: d near 0.7 (mostly target) — bear disruption cost, constrained exit (must implement or lose administrative position), spatial isolation (local-scope alternatives collapsed). Local authority holders: d near 0.8 (strong target) — identity-locked exit (cannot exit without abandoning social role), forced subordination of their authority system. General population: d near 0.55 (symmetric) — genuine coordination benefit (unified legal standards, state protection) but diffuse disruption cost and loss of local autonomy; trapped exit (migration is costly). Rival institutional orders: d near 0.9 (extreme target) — excluded from the mechanism, their domains subordinated, no meaningful exit (trapped institutional competitor). The beneficiary declaration (state_apex, interpretive_intermediaries) and victim declaration (fringe_implementers, local_authority_holders) reflect the asymmetric gains and losses that structure the cascade.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to install uniform institutional change without widespread resistance) is live and persistent across state formations. The cascade mechanism addresses it by decomposing the problem: apex imposes uniformity; fringe validates legitimacy. This is not mandatrophy in the classic sense (where the original function atrophies but the mechanism persists). Rather, it is a hybrid mechanism that performs a genuine coordination function (uniform institutional change) alongside an extractive function (reshaping local authority and autonomy in the apex's favor). The theater_ratio remains moderate (0.41) rather than high because the functional component (installing and stabilizing commitments) is real and necessary; the mechanism does not degrade into pure performance. The mechanism would not be classified as piton because all seats acknowledge that something functional is happening, even if they disagree sharply on whether the distribution of gains is fair. The classification as tangled_rope (from the apex's seat) is appropriate: there is a genuine coordination problem (institutional change), a genuine coordination solution (the cascade), and asymmetric distribution (fringe payers bear costs, apex and intermediaries gain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apex_vs_fringe_legitimacy_primacy,
    'Which seats'' legitimacy claims are STRUCTURALLY PRIMARY to the cascade''s stability: apex authority (the installation force) or fringe validation (the adoption force)?',
    'Historical record: analyze cases where apex installed a commitment without fringe validation (did it collapse? persist theatrically?); cases where fringe validated without apex backing (did it spread without enforcement? remain local?). Comparative state formation: do weak-legitimacy states (apex authority alone, no intermediary infrastructure) differ systematically in institutional stability from hybrid-cascade states?',
    'If apex legitimacy is primary, the constraint should be classified from the apex''s seat as rope (the fringe is just a coordination problem to manage, not a structural necessity). If fringe validation is primary, it should be tangled_rope or snare from the apex''s seat (the fringe is a structural bottleneck the apex must control). If both are equally primary, the tangled_rope classification holds across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apex_vs_fringe_legitimacy_primacy, conceptual, 'Whether apex authority or fringe validation is the structural bottleneck for institutional change stability.').

omega_variable(
    validation_vs_internalization_mechanism,
    'Is fringe validation (adoption of the commitment as locally legitimate) a mechanical process of bottom-up consensus, or a process of cultural reinterpretation wherein the commitment is absorbed into existing legitimacy narratives and thus transformed?',
    'Fine-grained historical analysis: track how the commitment''s meaning shifts as it passes through intermediary interpretation to fringe adoption; compare the apex-stated rationale with the fringe-adopted rationale; measure discontinuity (how much was the commitment transformed?). If transformation is substantial, the mechanism is reinterpretation; if the commitment is adopted as-stated, it is consensus.',
    'If the mechanism is mechanical consensus, the extracted authority loss is minimal (the fringe genuinely understands and agrees). If the mechanism is reinterpretation, the extraction is substantial (the commitment gains surface legitimacy but its actual operation differs from its stated form, a form of cover story). Theater_ratio would increase if reinterpretation is the actual mechanism, because the interpretive apparatus would be more performative than functional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(validation_vs_internalization_mechanism, empirical, 'Whether fringe validation involves genuine consensus or cultural reinterpretation that masks transformation.').

omega_variable(
    rivalry_with_endogenous_climb,
    'Does the hybrid_cascade reading FORECLOSE the endogenous_climb reading, or do they COEXIST as different possible mechanisms in the same kernel?',
    'Logical test: can a commitment simultaneously climb from local success AND cascade downward from apex installation within a single framework? If yes, they coexist. If the hybrid reading''s assumption (apex initiates, fringe validates) logically rules out local climb (requires local origin), then they foreclose. Historical test: in the same state system, do some commitments cascade hybrid-style while others climb endogenously? If yes, coexistence; if commitment pathways are mutually exclusive by institutional structure, foreclosure.',
    'Foreclosure would suggest that state systems are structured to prevent either path (apex wants installation control, fringe wants origination agency); coexistence would suggest the state system is sufficiently flexible to admit both pathways depending on the commitment type or historical moment. This affects how the engine routes the reading relations graph.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rivalry_with_endogenous_climb, conceptual, 'Whether hybrid_cascade and endogenous_climb are logically incompatible or structurally coexistent reading strategies.').

omega_variable(
    suppression_internalization_in_fringe,
    'Is the measured suppression reduction (from 0.80 at t=0 to 0.58 at t=40) a genuine internalization of the apex commitment into fringe norms, or a strategic accommodation where the fringe performs compliance while maintaining internal resistance?',
    'Post-validation observation: after the commitment is adopted, does fringe resistance re-emerge when apex enforcement relaxes (indicating strategic accommodation)? Does the commitment persist without active enforcement maintenance (indicating genuine internalization)? Ethnographic tracking: do fringe actors describe the commitment as ''now one of our traditions'' (internalization) or ''something we do because we must'' (accommodation)?',
    'If internalization, the constraint is genuinely stabilized—suppression can remain low and stable indefinitely. If accommodation, the constraint is brittle—suppression will spike if enforcement relaxes, or theater_ratio will rise further as performative compliance masks hidden resistance. A accommodation-heavy constraint would reclassify toward piton (functional only while performing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_fringe, empirical, 'Whether suppression reduction reflects genuine norm internalization or strategic accommodation.').

omega_variable(
    reading_contention_with_exogenous_imposition,
    'Does the hybrid_cascade reading FORECLOSE the exogenous_imposition reading (apex authority alone suffices), or do they COEXIST?',
    'Logical test: if fringe validation is structurally necessary for cascade stability (as this reading claims), then exogenous_imposition (which claims validation is unnecessary) is foreclosed. If fringe validation is sufficient but not necessary (apex can install and maintain without fringe validation, though it is more costly), then both readings coexist. Historical test: can a commitment installed exogenously (apex-only, no fringe validation process) remain stable long-term? If yes, coexistence; if exogenous commitments eventually collapse or require transformation into the hybrid_cascade form, foreclosure.',
    'Foreclosure would mean the kernel contest is resolved by the hybrid reading (apex needs fringe legitimacy). Coexistence would mean both paths are viable but have different stability/legitimacy profiles. This shapes how the engine treats the reading_relations graph and which readings are ruled out by the committed authority structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contention_with_exogenous_imposition, conceptual, 'Whether exogenous_imposition reading is logically incompatible with hybrid_cascade or structurally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t5, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(stat_tr_t15, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(stat_tr_t25, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stat_be_t5, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(stat_be_t15, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(stat_be_t25, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(stat_su_t5, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t15, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(stat_su_t25, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.58).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(stat_grid_01, state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(stat_grid_02, state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse(class), 40, 0.72).
narrative_ontology:measurement(stat_grid_03, state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse(individual), 0, 0.45).
narrative_ontology:measurement(stat_grid_04, state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse(individual), 40, 0.62).
narrative_ontology:measurement(stat_grid_05, state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(stat_grid_06, state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse(organizational), 40, 0.78).
narrative_ontology:measurement(stat_grid_07, state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse(structural), 0, 0.85).
narrative_ontology:measurement(stat_grid_08, state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse(structural), 40, 0.85).
narrative_ontology:measurement(stat_grid_09, state_commitment_installation_mechanism__hybrid_cascade_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(stat_grid_10, state_commitment_installation_mechanism__hybrid_cascade_reading, resistance(class), 40, 0.42).
narrative_ontology:measurement(stat_grid_11, state_commitment_installation_mechanism__hybrid_cascade_reading, resistance(individual), 0, 0.55).
narrative_ontology:measurement(stat_grid_12, state_commitment_installation_mechanism__hybrid_cascade_reading, resistance(individual), 40, 0.35).
narrative_ontology:measurement(stat_grid_13, state_commitment_installation_mechanism__hybrid_cascade_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(stat_grid_14, state_commitment_installation_mechanism__hybrid_cascade_reading, resistance(organizational), 40, 0.48).
narrative_ontology:measurement(stat_grid_15, state_commitment_installation_mechanism__hybrid_cascade_reading, resistance(structural), 0, 0.25).
narrative_ontology:measurement(stat_grid_16, state_commitment_installation_mechanism__hybrid_cascade_reading, resistance(structural), 40, 0.35).
narrative_ontology:measurement(stat_grid_17, state_commitment_installation_mechanism__hybrid_cascade_reading, stakes_inflation(class), 0, 0.65).
narrative_ontology:measurement(stat_grid_18, state_commitment_installation_mechanism__hybrid_cascade_reading, stakes_inflation(class), 40, 0.48).
narrative_ontology:measurement(stat_grid_19, state_commitment_installation_mechanism__hybrid_cascade_reading, stakes_inflation(individual), 0, 0.55).
narrative_ontology:measurement(stat_grid_20, state_commitment_installation_mechanism__hybrid_cascade_reading, stakes_inflation(individual), 40, 0.38).
narrative_ontology:measurement(stat_grid_21, state_commitment_installation_mechanism__hybrid_cascade_reading, stakes_inflation(organizational), 0, 0.78).
narrative_ontology:measurement(stat_grid_22, state_commitment_installation_mechanism__hybrid_cascade_reading, stakes_inflation(organizational), 40, 0.62).
narrative_ontology:measurement(stat_grid_23, state_commitment_installation_mechanism__hybrid_cascade_reading, stakes_inflation(structural), 0, 0.8).
narrative_ontology:measurement(stat_grid_24, state_commitment_installation_mechanism__hybrid_cascade_reading, stakes_inflation(structural), 40, 0.75).
narrative_ontology:measurement(stat_grid_25, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression(class), 0, 0.7).
narrative_ontology:measurement(stat_grid_26, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression(class), 40, 0.48).
narrative_ontology:measurement(stat_grid_27, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression(individual), 0, 0.58).
narrative_ontology:measurement(stat_grid_28, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression(individual), 40, 0.38).
narrative_ontology:measurement(stat_grid_29, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression(organizational), 0, 0.82).
narrative_ontology:measurement(stat_grid_30, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression(organizational), 40, 0.62).
narrative_ontology:measurement(stat_grid_31, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression(structural), 0, 0.85).
narrative_ontology:measurement(stat_grid_32, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression(structural), 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.18).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel about institutional commitment legitimacy. The kernel 'state_commitment_installation_mechanism' decomposes into three structurally distinct readings: (1) endogenous_climb_reading — legitimacy climbs from fringe success; (2) exogenous_imposition_reading — legitimacy from apex authority alone; (3) hybrid_cascade_reading (this file) — legitimacy from apex initiation + fringe validation. Each reading has its own ε value, stakeholder structure, and classification boundary. The readings are linked via network.affects_constraints because they are competing explanations of the same institutional phenomenon, not because one causes the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__hybrid_cascade_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
