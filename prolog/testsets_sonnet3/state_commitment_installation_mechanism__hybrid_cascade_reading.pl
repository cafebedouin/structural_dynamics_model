% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Hybrid Cascade Installation of State Commitments (Apex-Down, Fringe-Validated)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the hybrid-cascade reading of the state
 *   commitment installation kernel: a new order-wide commitment (legal code,
 *   orthodoxy, administrative standard) is proclaimed at the apex and
 *   cascades downward, but its stability depends on a second phase in which
 *   fringe intermediaries adapt, translate, and locally re-legitimate it.
 *   This is distinct from the exogenous_imposition reading (which holds the
 *   apex mandate alone is sufficient and treats the periphery as passive
 *   recipient) and the endogenous_climb reading (which holds legitimacy is
 *   generated bottom-up through demonstrated local superiority, with the apex
 *   adopting what has already proven itself). In the hybrid reading, neither
 *   direction alone stabilizes the commitment — installation requires both
 *   the apex proclamation AND fringe validation, making the intermediary
 *   elite layer structurally indispensable and structurally extractive at
 *   once.
 *
 * KEY AGENTS:
 *   - central_state_apparatus: apex authority (institutional/arbitrage) — proclaims but cannot enforce alone
 *   - loyal_intermediary_elites: brokers (powerful/constrained) — indispensable translators, primary beneficiaries of the two-phase structure
 *   - peripheral_communities: fringe subjects (powerless/trapped) — bear the compliance and disruption cost
 *   - local_customary_authorities: displaced local leaders (moderate/constrained) — subordinated or absorbed
 *   - commitment_resistant_factions: outright refusers (moderate/trapped) — structurally unrecognized, excluded
 *   - court_and_field_historians: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.52).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Hybrid Cascade Installation of State Commitments (Apex-Down, Fringe-Validated)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, '68e2237e-8c6c-4584-a003-21185d10a4ba').
narrative_ontology:cs_kernel_codification('68e2237e-8c6c-4584-a003-21185d10a4ba', distributed).
narrative_ontology:cs_authority_grounding('68e2237e-8c6c-4584-a003-21185d10a4ba', extraction).
narrative_ontology:cs_interpretation_layer_present('68e2237e-8c6c-4584-a003-21185d10a4ba').
narrative_ontology:cs_reading_relation('68e2237e-8c6c-4584-a003-21185d10a4ba', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('68e2237e-8c6c-4584-a003-21185d10a4ba', state_commitment_installation_mechanism__exogenous_imposition_reading, influences).
narrative_ontology:cs_axiom('68e2237e-8c6c-4584-a003-21185d10a4ba', foundational, legitimacy_requires_bidirectional_ratification).
narrative_ontology:cs_axiom_status(legitimacy_requires_bidirectional_ratification, holdable).
narrative_ontology:cs_axiom_grounding('68e2237e-8c6c-4584-a003-21185d10a4ba', legitimacy_requires_bidirectional_ratification, conventional).
narrative_ontology:cs_axiom('68e2237e-8c6c-4584-a003-21185d10a4ba', secondary, intermediary_translation_is_structurally_necessary).
narrative_ontology:cs_axiom_status(intermediary_translation_is_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('68e2237e-8c6c-4584-a003-21185d10a4ba', intermediary_translation_is_structurally_necessary, instrumental).
narrative_ontology:cs_reference_frame('68e2237e-8c6c-4584-a003-21185d10a4ba', unified_apex_mandate_with_fringe_ratification).
narrative_ontology:cs_drift_state('68e2237e-8c6c-4584-a003-21185d10a4ba', post_intermediary_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('68e2237e-8c6c-4584-a003-21185d10a4ba', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, loyal_intermediary_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, local_customary_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and proclaims the new commitment (a legal code, religious orthodoxy, administrative standard, or symbolic order) at the apex and dispatches it downward through officials, texts, and ritual. It cannot enforce compliance everywhere at once and depends on the commitment being locally adapted and re-legitimated rather than uniformly imposed; it collects the durable authority that results once fringe validation stabilizes the commitment.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Provincial governors, translated clergy, or local notables who broker the apex commitment to their constituencies, reframing it in locally intelligible terms. They gain standing, patronage, and office from being the necessary translators, and their cooperation is what allows the two-phase adoption to succeed; leaving the arrangement means losing the position that makes them indispensable.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, loyal_intermediary_elites, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, loyal_intermediary_elites, agenda_setter).

% Ordinary subjects at the fringe who absorb the new commitment through the intermediaries' reinterpretation. They bear the disruption cost of abandoning prior norms and the labor of performing conformity with the new order, while having no direct channel to negotiate its terms with the apex; their local acceptance is what the state needs to call the installation legitimate, but their partial resistance is managed rather than answered.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_communities, payer,
    powerless, generational, trapped, local).

% Pre-existing local leaders, elders, or ritual specialists whose authority the new commitment displaces or subordinates. Some are absorbed into the intermediary layer if they cooperate; those who do not are marginalized, their prior legitimacy delegitimized by the new order's framing of them as parochial or backward.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, local_customary_authorities, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, local_customary_authorities, excluded).

% Groups that reject the new commitment outright rather than adapting it locally. They are structurally excluded from the negotiation between apex and intermediary — the hybrid cascade model has no seat for refusal, only for adapted acceptance — and their resistance is absorbed, suppressed, or reframed as deviance rather than engaged as a counter-proposal.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, commitment_resistant_factions, excluded,
    moderate, generational, trapped, regional).

% Document how the commitment moved from apex to fringe and back, tracing which local adaptations were retroactively canonized as continuity with the founding proclamation and which were erased as failed resistance.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, court_and_field_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, loyal_intermediary_elites).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of installing a new order-wide commitment (legal, religious, administrative) across a territory too large and heterogeneous for the apex to administer directly, by using local intermediaries to translate and re-legitimate the commitment in terms the periphery will accept.
% TRANSFER_FUNCTION: Moves interpretive authority and enforcement labor from the center to intermediary elites, and moves compliance costs, disrupted local practice, and the burden of legitimation-performance onto peripheral communities and displaced customary authorities.
% ABSENT_VOICES: Commitment-resistant factions and displaced customary authorities who reject the new order outright have no structural seat in the cascade — the model only recognizes adapted acceptance, not refusal, so their objections surface only as suppressed unrest or are retroactively narrated as backwardness.
% DISAPPEARANCE_RATIONALE: If the apex-cascade-with-fringe-validation mechanism vanished, either the center would have to impose commitments by direct coercion alone (a structurally different, more costly arrangement) or peripheries would generate commitments endogenously from local practice upward — either way, the intermediary elite layer would lose its distinctive brokerage function and the installed commitments would face renewed legitimacy contests.
% FOUNDING_PROBLEM: A center possessing the capacity to declare a new order-wide commitment but lacking the administrative reach to install it uniformly needed a mechanism that could both project authority downward and generate local buy-in without direct enforcement at every point.
% FOUNDING_PROBLEM_CORROBORATION: Central state chroniclers attest the mechanism as evidence of successful unification and the persistence of the founding mandate. Independent historical-sociological analysis and oral traditions preserved among displaced customary authorities corroborate that the founding problem (administrative reach) has been largely solved in many cases, and that the cascade mechanism now functions substantially to entrench intermediary elite privilege and manage residual resistance rather than to solve a live coordination gap.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58) is moderate-high and rising over the interval: initial installation carries real coordination cost that declines in relative suppression need as intermediary elites consolidate their brokerage role, but the extractive share grows as the intermediary layer entrenches privilege beyond what fringe validation actually requires. Suppression starts higher (0.62) during the initial imposition phase and eases (settling near 0.52) once local adaptation absorbs most resistance — suppression is not scaled by power or scope; it reflects the raw coercive requirement of the installation process itself, front-loaded and then substituted by softer legitimation work. Theater ratio rises (0.20 to 0.40) as the performative apparatus of 'consultative' local validation increasingly substitutes for genuine renegotiation of the commitment's terms.
 *
 * PERSPECTIVAL GAP:
 *   From the apex's seat, the arrangement looks like successful, legitimate state-building — a genuine coordination problem (territorial administrative reach) solved efficiently. From the peripheral community's seat, the same structure operates as an extraction mechanism that launders coercive imposition through a veneer of local consent generated by intermediaries who have their own stake in appearing as legitimate translators rather than agents of the center. The engine should compute divergent seat classifications from these structurally different positions even though both parties are looking at the same installation event.
 *
 * DIRECTIONALITY LOGIC:
 *   The central state apparatus and loyal intermediary elites are the structural beneficiaries: the state gains durable, low-enforcement-cost authority, and intermediaries gain brokerage rents and elevated local standing — both get low d. Peripheral communities and local customary authorities are the targets: they bear disruption costs and displaced status with limited exit (trapped/constrained), giving them high d. Commitment-resistant factions get the highest effective extraction despite moderate nominal power because the mechanism has no accommodation channel for them at all — their resistance is either suppressed or reframed, never negotiated.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid-cascade classification prevents two mislabeling errors: (1) treating the installation as pure top-down extraction (which would erase the genuine coordination function the two-phase structure serves — administering a large heterogeneous territory without direct force everywhere), and (2) treating it as pure organic coordination (which would erase the asymmetric extraction that accrues to the state and intermediary elites at the fixed cost of peripheral disruption and customary-authority displacement). Tangled Rope captures both: a real coordination function (large-scale commitment installation without universal direct enforcement) bundled with asymmetric extraction (intermediary rent-seeking, peripheral cost-bearing) sustained by active enforcement (suppression of resistant factions, continued coercive backstop even as it declines over time).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cascade_vs_climb_attribution,
    'In any specific historical case, is the observed stabilization better explained by genuine apex-to-fringe cascade with local validation, or does it retrospectively look that way because successful endogenous local adaptations were later claimed and canonized by the center as if they had cascaded from it?',
    'Comparative documentary and archaeological evidence tracing whether local adaptation practices predate or postdate the apex proclamation date; oral history cross-referenced against court chronicles.',
    'If local practices predate the proclamation, the case belongs to the endogenous_climb_reading rather than this hybrid_cascade_reading, and this constraint''s beneficiary/extraction structure would be misattributed to the wrong constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascade_vs_climb_attribution, empirical, 'Whether cascade attribution in specific cases is genuine or retrospective narrative capture.').

omega_variable(
    intermediary_necessity_vs_entrenchment,
    'Is the intermediary elite layer''s ongoing extraction proportional to the genuine translation labor required for fringe validation, or has it grown into entrenched rent-seeking beyond what stabilization actually needs?',
    'Comparing intermediary compensation/privilege levels against measurable translation/legitimation workload across cases and over time; identifying whether resistance-absorption costs decline while intermediary privilege continues rising.',
    'If extraction has decoupled from translation necessity, the constraint drifts from tangled_rope toward snare at the intermediary layer specifically, even while the apex-periphery relationship remains genuinely coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermediary_necessity_vs_entrenchment, empirical, 'Whether intermediary extraction remains proportionate to genuine coordination labor or has become self-sustaining rent.').

omega_variable(
    resistant_faction_exclusion_permanence,
    'Is the structural exclusion of outright-refusal factions from the cascade model a permanent feature of hybrid installation, or a phase that resolves once enough time passes for either full absorption or full separation?',
    'Longitudinal tracking of specific resistant factions across multiple generations to see whether they are eventually absorbed into the intermediary/local-authority structure, permanently suppressed, or achieve autonomous exit.',
    'Permanent exclusion supports classifying the resistant-faction relationship as approaching snare; eventual absorption or accommodation would support a more scaffold-like transitional reading for that specific sub-relationship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistant_faction_exclusion_permanence, conceptual, 'Whether exclusion of resistant factions is a permanent or transitional feature of the hybrid mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(stat_tr_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(stat_be_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(stat_su_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 32, 0.51).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_cascade_reading member of the state_commitment_installation_mechanism kernel family (3 readings: endogenous_climb_reading, exogenous_imposition_reading, hybrid_cascade_reading). Each reading authors its own ε and beneficiary/victim structure over the same underlying phenomenon of new-commitment installation. This reading's distinguishing structural claim is the two-phase, both-directions-required stabilization process, which produces a moderate-high, TANGLED_ROPE-leaning ε distinct from the pure-coercion profile expected under exogenous_imposition_reading and the low-suppression, merit-climbing profile expected under endogenous_climb_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__hybrid_cascade_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
