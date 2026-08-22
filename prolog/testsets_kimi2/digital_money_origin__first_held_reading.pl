% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Origin: First Practical Holding
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint instantiates the 'first_held_reading' of the contested
 *   digital_money_origin kernel. It treats digital money as having emerged
 *   when individuals first held non-physical monetary instruments as
 *   practical stores of value â a materially later origin than conceptual
 *   conceivability, and prior to regulatory incorporation. The constraint set
 *   includes the implementation barriers (hardware, connectivity, literacy)
 *   and network effects that accompanied this first practical holding. Early
 *   adopters with access are the structural beneficiaries; those lacking
 *   infrastructure are the victims, excluded from the new monetary
 *   coordination mechanism and concentrated into higher-friction cash
 *   systems.
 *
 * KEY AGENTS:
 *   - early_adopters_with_access: Primary beneficiary (moderate power, mobile exit) â gains network-effect advantages and censorship-resistant stores of value.
 *   - excluded_without_infrastructure: Primary payer/target (powerless, trapped exit) â excluded by technical barriers, bears costs of informal cash economy.
 *   - protocol_operators: Agenda-setter (organized, arbitrage exit) â maintains and enforces the technical protocol enabling non-physical holding.
 *   - traditional_financial_institutions: Analytical observer (institutional, analytical exit) â watches from outside before regulatory incorporation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.45).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Origin: First Practical Holding").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, 'be4f9dbc-dd89-4056-92c5-78b3ded78fba').
narrative_ontology:cs_kernel_codification('be4f9dbc-dd89-4056-92c5-78b3ded78fba', distributed).
narrative_ontology:cs_authority_grounding('be4f9dbc-dd89-4056-92c5-78b3ded78fba', distributed).
narrative_ontology:cs_reading_relation('be4f9dbc-dd89-4056-92c5-78b3ded78fba', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('be4f9dbc-dd89-4056-92c5-78b3ded78fba', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('be4f9dbc-dd89-4056-92c5-78b3ded78fba', foundational, practical_holding_constitutes_money_origin).
narrative_ontology:cs_axiom_status(practical_holding_constitutes_money_origin, holdable).
narrative_ontology:cs_axiom_grounding('be4f9dbc-dd89-4056-92c5-78b3ded78fba', practical_holding_constitutes_money_origin, conventional).
narrative_ontology:cs_reference_frame('be4f9dbc-dd89-4056-92c5-78b3ded78fba', practical_holder_origin).
narrative_ontology:cs_drift_state('be4f9dbc-dd89-4056-92c5-78b3ded78fba', regulatory_recognition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be4f9dbc-dd89-4056-92c5-78b3ded78fba', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters_with_access).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, excluded_without_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who first held non-physical monetary instruments as practical stores of value. They gained early access to lower-friction exchange, censorship-resistant settlement, and network-effect advantages as the system grew. Their participation required hardware, connectivity, and technical literacy that was not universally available.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters_with_access, beneficiary,
    moderate, biographical, mobile, global).

% Individuals and communities lacking reliable internet, digital devices, or technical literacy necessary to hold and transact non-physical monetary instruments. They are excluded from the emerging digital monetary network and bear the costs of remaining in cash-based or informal financial systems with higher friction and less connectivity.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, excluded_without_infrastructure, payer,
    powerless, immediate, trapped, global).

% Early developers, node operators, and network maintainers who implemented and enforced the technical protocols enabling non-physical value storage and transfer. They set the rules of participation, validated transactions, and maintained the distributed infrastructure that made the first holdings possible.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, protocol_operators, agenda_setter,
    organized, generational, arbitrage, global).

% Banks and monetary authorities observing the emergence of privately maintained non-physical stores of value. At the time of first holding, they have not yet incorporated these instruments into regulatory frameworks and operate outside the constraint's immediate governance.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, traditional_financial_institutions, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables practical, non-physical storage and transfer of value without geographic dependency on physical coin or paper, solving the coordination problem of remote and scalable exchange.
% TRANSFER_FUNCTION: Moves purchasing power and network-advantage from latecomers and the infrastructure-poor to early holders and protocol maintainers, mediated by implementation barriers and network effects.
% ABSENT_VOICES: Populations without internet access, device ownership, or technical literacy are excluded from the arrangement and would demand inclusive onboarding if present; traditional monetary authorities are not yet incorporated in this reading's frame.
% DISAPPEARANCE_RATIONALE: If the first-held digital money arrangement vanished overnight, early adopters would lose their non-physical store of value and the network effects built around it, while the excluded would remain in their pre-existing cash-based systems â the monetary landscape for this cohort would revert to purely physical instruments.
% FOUNDING_PROBLEM: The friction, cost, and geographic limits of physical money for remote and large-scale transactions, and the desire for a practical non-physical store of value.
% FOUNDING_PROBLEM_CORROBORATION: Early adopters and cypherpunk literature attest the founding problem of physical cash friction. Economic historians and development economists outside the beneficiary set corroborate that infrastructure exclusion is a real barrier, though they may dispute whether the first-held arrangement adequately addressed the founding problem for all parties.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__first_held_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is driven by network effects that advantage early holders and implementation barriers that exclude the infrastructure-poor. Suppression (0.45) reflects passive but durable exclusion: lack of internet, devices, or literacy functions as a barrier that is not actively policed but is structurally enforced by the protocol's requirements. Theater ratio is low (0.15) because the coordination function â non-physical storage and transfer â is genuinely operational, though a small origin-mythologizing layer exists. Accessibility collapse (0.55) captures the moderate collapse of non-digital alternatives for digital-age commerce. Resistance (0.30) is present but limited because the excluded are diffuse and the early adopters are not yet institutionally dominant.
 *
 * PERSPECTIVAL GAP:
 *   The early adopter seat experiences the constraint as a liberating coordination tool that solves physical cash friction; the excluded seat experiences the same structure as an extraction mechanism that locks them out of the emerging monetary network. The engine computes this divergence from the structural data â beneficiary status, power, and exit options â without requiring an authored reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters are declared beneficiaries with mobile exit, placing their directionality near the beneficiary end (low d). The excluded are declared victims (payers) with trapped exit, placing their directionality near the target end (high d). Protocol operators, as agenda-setters with arbitrage-grade exit, sit near the beneficiary end though their gains are indirect (protocol maintenance rather than rent capture). Traditional financial institutions are observers with analytical exit, directionality near neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by preserving the genuine coordination function (remote, non-physical value storage) alongside the asymmetric extraction (network-effect rent and infrastructure exclusion). Neither pure-coordination (Rope) nor pure-extraction (Snare) captures the structure; the Tangled Rope classification is warranted because the same technical arrangement that coordinates exchange also stratifies access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the ''first held'' reading capture the true emergence of digital money, or does it conflate technical possibility with the later regulatory and conceptual shifts described by sibling readings?',
    'Comparative historical analysis of when non-physical instruments first functioned as stores of value versus when they were statistically and legally recognized as money.',
    'If the first-held moment is the true origin, the constraint''s victim set is limited to infrastructure exclusion; if regulatory recognition is required, the constraint''s enforcement and beneficiary structure shifts to state actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Uncertainty about whether first practical holding or later recognition constitutes the true origin.').

omega_variable(
    network_effect_extraction,
    'Are the network effects and implementation barriers inherent technical properties of digital money, or are they amplified by policy choices that could be altered?',
    'Analysis of alternative protocol designs and infrastructure policies that reduce access barriers without destroying the coordination function.',
    'If barriers are policy-amplified, the constraint''s extractiveness is partially a snare; if purely technical, it is closer to a mountain-like feature of the technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_extraction, empirical, 'Whether extraction through exclusion is technically necessary or politically constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__first_held_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t5, digital_money_origin__first_held_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(digi_tr_t10, digital_money_origin__first_held_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(digi_tr_t15, digital_money_origin__first_held_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(digi_tr_t20, digital_money_origin__first_held_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__first_held_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(digi_be_t5, digital_money_origin__first_held_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(digi_be_t10, digital_money_origin__first_held_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(digi_be_t15, digital_money_origin__first_held_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(digi_be_t20, digital_money_origin__first_held_reading, base_extractiveness, 20, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(digital_money_origin__first_held_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the digital_money_origin kernel, decomposed per the Îµ-invariance principle because the natural-language label 'origin of digital money' conflates structurally distinct claims: conceptual conceivability, practical holding, and regulatory recognition. Each reading carries a different Îµ, stakeholder set, and temporal placement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
