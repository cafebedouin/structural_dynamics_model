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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Norm Climb — Bottom-Up Legitimation Reading
 *   domain: historical/sociological/cultural
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous-climb reading of the
 *   contested kernel 'imposition_mechanism': new norms achieve legitimacy
 *   through bottom-up adoption, observation, and peer imitation before the
 *   state formalizes them. The reading asserts that the sequence is
 *   adoption-first, mandate-second, and that legitimacy derives from cultural
 *   acceptance rather than coercive enforcement. Extraction is low (0.18)
 *   because the constraint is primarily coordinative rather than extractive;
 *   the state acts as centralizer and guarantor rather than as a coercer
 *   imposing against resistance. Suppression is minimal (0.12) because active
 *   resistance is low when the norm demonstrates utility and spreads through
 *   social proof. Theater ratio is very low (0.08) because the norm's
 *   operation is functional rather than performative: state enforcement
 *   effort is directed at consistency and standardization, not at suppressing
 *   alternatives.
 *
 * KEY AGENTS:
 *   - norm_adopting_population — the mass of actors who adopt through observation and peer pressure; derive legitimacy from seeing others succeed with the new practice
 *   - coordinating_state_apparatus — formalizes and standardizes norms already gaining acceptance; benefits from low enforcement cost because legitimacy precedes mandates
 *   - early_adopter_elite — signal cultural innovation; their adoption lowers the perceived risk and raises the status premium for others
 *   - prior_norm_keepers — lose status and social position as the new norm climbs; experience soft social pressure but not direct state coercion
 *   - competing_norm_coalitions — excluded from the adoption process; once coordination locks onto one norm, alternatives become individually irrational
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.14).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Norm Climb — Bottom-Up Legitimation Reading").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical/sociological/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, '6c834890-7a31-4b93-a40b-ce47b403af99').
narrative_ontology:cs_kernel_codification('6c834890-7a31-4b93-a40b-ce47b403af99', distributed).
narrative_ontology:cs_authority_grounding('6c834890-7a31-4b93-a40b-ce47b403af99', practice).
narrative_ontology:cs_interpretation_layer_present('6c834890-7a31-4b93-a40b-ce47b403af99').
narrative_ontology:cs_reading_relation('6c834890-7a31-4b93-a40b-ce47b403af99', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c834890-7a31-4b93-a40b-ce47b403af99', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('6c834890-7a31-4b93-a40b-ce47b403af99', foundational, legitimacy_precedes_mandate).
narrative_ontology:cs_axiom_status(legitimacy_precedes_mandate, holdable).
narrative_ontology:cs_axiom_grounding('6c834890-7a31-4b93-a40b-ce47b403af99', legitimacy_precedes_mandate, empirically_contingent).
narrative_ontology:cs_axiom('6c834890-7a31-4b93-a40b-ce47b403af99', foundational, adoption_driven_by_peer_visibility).
narrative_ontology:cs_axiom_status(adoption_driven_by_peer_visibility, holdable).
narrative_ontology:cs_axiom_grounding('6c834890-7a31-4b93-a40b-ce47b403af99', adoption_driven_by_peer_visibility, empirically_contingent).
narrative_ontology:cs_reference_frame('6c834890-7a31-4b93-a40b-ce47b403af99', organic_norm_emergence).
narrative_ontology:cs_drift_state('6c834890-7a31-4b93-a40b-ce47b403af99', post_formalization_lock_in, gap(stable, minor, false)).
narrative_ontology:cs_created_at('6c834890-7a31-4b93-a40b-ce47b403af99', '2026-06-11T14:32:15Z').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, norm_adopting_population).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, coordinating_state_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_elite).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, prior_norm_keepers).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, legitimacy_through_organic_adoption).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, state_as_coordinator_not_enforcer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts new norms through observation, imitation, and perceived cultural superiority of the practice. Sees legitimacy deriving from peer adoption and social proof rather than state decree. Benefits from coordination: the norm solves a genuine collective-action problem (what counts as honorable, how disputes are settled, what kinship obligations entail). Can choose alternative norms or revert to prior practice; the widespread adoption makes exit costly but not impossible. Does not experience enforcement pressure because adoption aligns with perceived self-interest and cultural status.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_adopting_population, beneficiary,
    organized, generational, mobile, national).

% Formalizes and standardizes norms that have already gained popular acceptance. Acts as centralizer and guarantor of consistency rather than imposer. Benefits from reduced enforcement costs because legitimacy precedes rather than follows mandates. May ratify, codify, or selectively speed adoption of norms that have already demonstrated utility and acceptance. Does not maintain the constraint through coercion; withdrawal of state support would slow but not stop the norm's operation once it is embedded in social practice.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, coordinating_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Hold the prior norms and bear the social costs of non-adoption as the new norm climbs. Their status and authority erode as the new practice demonstrates success and spreads through peer networks. Exit from the old norm is constrained by age, investment in the prior practice, and loss of social standing if they switch late. Do not experience direct state enforcement pressure; instead experience soft social pressure as adoption accelerates.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, prior_norm_keepers, payer,
    moderate, biographical, constrained, local).

% First to recognize and adopt the new norm; derive status and influence from early adoption and from being seen as cultural exemplars. Operate at the innovation frontier where the norm is still contested. Can leverage early-adopter advantage to build reputation and material benefit before mass adoption commoditizes the signal.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, early_adopter_elite, beneficiary,
    powerful, biographical, arbitrage, national).

% Represent alternative norm systems that lose legitimacy as the new norm climbs. Would argue for plural or competing norms but are structurally excluded from the coordination process once one norm achieves dominance. Their exclusion is not enforced by state violence but by the dynamic of coordination itself: once a norm reaches critical mass, adopting an alternative is individually irrational.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, competing_norm_coalitions, excluded,
    moderate, generational, trapped, national).

% Studies the norm-adoption process and documents the sequence: emergence of variant practices, diffusion through elite networks, tipping point into mass adoption, state formalization. Occupies a seat outside the adoption dynamics and can measure whether mandate followed or preceded acceptance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, external_anthropological_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of which norm governs social behavior (dispute resolution, kinship, honor, property). A shared understanding of 'how we do things here' reduces transaction costs and enables cooperation at scale. The new norm provides a focal point that allows millions of strangers to coordinate without explicit negotiation.
% TRANSFER_FUNCTION: No direct transfer of material resources. The flow is of social status and authority: early adopters and the state apparatus that formalizes the norm accrue legitimacy and coordinating power; those invested in prior norms lose status and influence. The population gains the benefit of coordination without bearing a concentrated extraction cost.
% ABSENT_VOICES: Competing norm coalitions and practitioners of prior norms are structurally excluded from norm selection: once one norm achieves critical-mass adoption, the coordination dynamic renders alternatives irrational to adopt individually, even if some actors prefer them. They would advocate for norm pluralism or slower transition but are not at the table where the new norm is standardized.
% DISAPPEARANCE_RATIONALE: If the new norm were suddenly abolished by state decree and the state ceased to enforce or standardize it, the norm would persist in social practice because legitimacy precedes mandates in this reading. Social proof and peer adoption are the constraint's load-bearing mechanism. The norm would degrade slowly as new generations grew up without state reinforcement, but elimination would require sustained counter-norm promotion, not just legal revocation.
% FOUNDING_PROBLEM: Absence of a shared normative framework for social coordination. Prior norms were locally variable, sometimes contradictory, creating friction in inter-group interaction and uncertainty about behavioral expectations. Need for a unified, recognizable norm that enables strangers to cooperate without negotiating baseline expectations.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists studying norm transitions attest that founding problems of this type—fragmentation and uncertainty about expectations—are real and observable in the archaeological and documentary record before norm unification (sources: anthropological studies of norm emergence in pre-state societies, historical accounts of successful norm transitions such as the spread of tea ceremony or marriage-by-choice norms). The problem persists: any large society lacking shared normative frameworks experiences coordination friction.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Under the endogenous-climb reading, extraction is low because no party bears concentrated costs; everyone who adopts does so because they perceive benefit (coordination, status, reduced uncertainty). The state's role is to accelerate and standardize a process already underway, not to impose against resistance. Suppression requirement is low because the norm persists through social proof and peer imitation rather than through enforcement. Theater ratio is minimal because the constraint does its functional work: coordination actually happens, enforcement is not theatrical. Measurements show gentle rise then stabilization: as the norm reaches critical mass (t=10-15), extractiveness rises slightly (early-adopter premium, status-hierarchy effects) but plateaus (maturity: the premium commoditizes). The temporal profile is key to distinguishing this reading from exogenous override: no ratcheting enforcement, no rising suppression requirement, no acceleration of resistance. The profile matches voluntary adoption followed by state formalization.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (prior-norm keepers, competing coalitions) and the coordinating-state seat should compute differently. Prior-norm keepers experience the constraint as loss of status through social dynamics, not state coercion—they are targets of soft pressure (coordination by peer networks), not coercive targeting. From their seat, the constraint looks like a social avalanche they cannot resist, not an imposed rule. Competing coalitions experience it as foreclosure of their preferred norm by the dynamics of coordination lock-in, not by state prohibition. From their seat, the constraint looks like a tragedy of the commons: individually rational adoption of the dominant norm produces collectively suboptimal norm pluralism. The state apparatus, by contrast, experiences the constraint as successful coordination it helped accelerate—low enforcement cost, wide adoption, legitimacy already present. These divergent perceptions arise from the structure (organic adoption, state as coordinator) and should compute as different types across seats: beneficiary seats may compute as rope (genuine coordination with benefit), payer seats may compute as snare (soft exclusion of alternatives, loss of status), and the state may compute as rope (low-cost coordination). The claim/metric independence rule preserves the authored divergence: I claim rope (low extraction, genuine coordination), but the metrics represent the actual operation where some seats experience exclusion and status loss.
 *
 * DIRECTIONALITY LOGIC:
 *   The norm-adopting population sits near the beneficiary end (d ~0.2-0.3) because they gain coordination benefit without bearing direct extraction; they choose to adopt. Early-adopter elite sit at the far beneficiary end (d ~0.0-0.1) because they gain status premium from early adoption; they face no suppression because they are leading the trend. Prior-norm keepers sit near the payer end (d ~0.7-0.8) because they bear status loss as the new norm displaces their practice, but exit is available (they can adopt, though late); the loss is soft social pressure, not legal sanction. Competing-norm coalitions sit at the full-target end (d ~0.9) because they are structurally excluded from the coordination process; once one norm locks in, alternatives are irrational to adopt individually, even for actors who would prefer them. The state apparatus occupies a complex position near symmetric (d ~0.45-0.55): it coordinates a real problem but also centralizes authority and sets standardization rules that constrain future norm evolution.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (when a constraint's mandate outlives its function) is not present in this reading. The founding problem (lack of shared normative framework, coordination friction) remains live: any large society continues to need shared norms for coordination. The state mandate (standardization, formalization of the norm) continues to serve the founding problem because it prevents norm drift and provides consistent focal points. However, the reading contains an irreducible ambiguity about whether the mandate is carrying legitimacy or vice versa: if mandate truly follows adoption, then the state's formalization is redundant once the norm is embedded in social practice. If the mandate becomes necessary to preserve the norm against erosion, then mandatrophy has occurred—the founding problem has been solved, but the state constraint persists to prevent reversion. This ambiguity is routed to the cs_axiom_overriding omega and to the kernel-reading uncertainty omega, not treated as mandatrophy resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adoption_sequence_empirical_status,
    'In actual historical cases of norm transition, does adoption precede state mandate? Can we distinguish instances where states formalized already-spreading norms from instances where states imposed norms against initial resistance?',
    'Comparative analysis of historical norm transitions (tea ceremony, marriage-by-choice, legal codes, honor systems) using documentary evidence and temporal dating of adoption vs. mandate. Correlate with suppression-requirement metrics: if adoption precedes mandate, suppression should be low; if mandate precedes adoption, suppression should be high and rising.',
    'If adoption typically precedes mandate, this reading''s structural picture is supported and extraction values are justified. If mandate typically precedes adoption, the exogenous_override reading better fits the data and extraction should be higher. If both sequences occur, the hybrid_legitimation reading and the coexists_with relation are supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_sequence_empirical_status, empirical, 'Whether adoption temporally precedes mandate in documented norm transitions.').

omega_variable(
    legitimacy_source_attribution,
    'When actors adopt a new norm, do they attribute legitimacy to peer adoption and cultural visibility (endogenous climb) or to state authority and legal requirement (exogenous override)?',
    'Primary source analysis of adoption narratives, interviews with norm-adopters, examination of contemporaneous justifications for norm change. Distinguish between explicit attribution (actors state reasons for adoption) and revealed preference (their adoption timing correlates with peer adoption vs. state enforcement).',
    'If actors primarily attribute legitimacy to peer adoption and cultural demonstration, the endogenous-climb reading''s axiom holds. If attribution is primarily to state authority or legal requirement, exogenous_override is better supported. Mixed attribution patterns support the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_attribution, empirical, 'Whether actors attribute norm legitimacy to peer adoption or state authority.').

omega_variable(
    state_coordination_role_interpretability,
    'Is the distinction between ''state as coordinator of already-climbing norms'' and ''state as enforcer imposing norms'' empirically sharp, or is it a conceptual reading imposed on ambiguous evidence?',
    'Case studies where the state''s role is explicitly documented (court records, policy papers, enforcement reports). Examine whether the state''s actions are consistent with coordination (standardization, formalization, dispute resolution using the norm) or enforcement (punishment for violation, suppression of alternatives, coercion against resistance).',
    'If the state''s documented role is coordinative, extraction stays low and this reading holds. If the role is primarily enforcement, extraction rises and exogenous_override gains support. If both roles are present in different phases, hybrid_legitimation gains support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_coordination_role_interpretability, conceptual, 'Whether ''state as coordinator'' is a distinct role empirically identifiable from ''state as enforcer'', or a conceptual framing imposed on evidence.').

omega_variable(
    norm_persistence_counterfactual,
    'If the state withdrew formalization and enforcement of the norm, would it persist in social practice or would it erode?',
    'Natural experiments where state enforcement is weakened or removed: historical periods of state collapse, jurisdictions that formally abandoned norm enforcement, or comparative cases where similar norms persist with different levels of state support.',
    'If norms persist without state support, legitimacy is truly endogenous (adoption-based) and extraction is low because the state''s role is supplementary. If norms erode without state support, legitimacy is partially dependent on enforcement (exogenous or hybrid mechanisms), and extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_persistence_counterfactual, empirical, 'Whether norm persistence is independent of state enforcement.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of the imposition_mechanism kernel genuinely incommensurable (logically incompatible for a single party), or can a single framework incorporate elements of all three?',
    'Examine whether a state can coordinate norms through formalization while also employing selective enforcement and symbolic authority. If so, the readings coexist within a single framework (coexists_with relation). If not, determine which pairs foreclose each other.',
    'If readings are incommensurable, some pairs should be marked forecloses in reading_relations. If they can coexist, all relations should be coexists_with or influences. The relation types shape the engine''s handling of rival claims about the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three readings of norm-imposition mechanisms are logically incompatible or can coexist in a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(impo_tr_t0, projected).
narrative_ontology:measurement(impo_tr_t5, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement_basis(impo_tr_t5, projected).
narrative_ontology:measurement(impo_tr_t10, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t15, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 15, 0.07).
narrative_ontology:measurement_basis(impo_tr_t15, observed).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement_basis(impo_tr_t25, observed).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(impo_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(impo_be_t0, projected).
narrative_ontology:measurement(impo_be_t5, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement_basis(impo_be_t5, projected).
narrative_ontology:measurement(impo_be_t10, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t15, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(impo_be_t15, observed).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 25, 0.19).
narrative_ontology:measurement_basis(impo_be_t25, observed).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(impo_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(impo_su_t0, projected).
narrative_ontology:measurement(impo_su_t5, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement_basis(impo_su_t5, projected).
narrative_ontology:measurement(impo_su_t10, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t15, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement_basis(impo_su_t15, observed).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement_basis(impo_su_t25, observed).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(impo_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, information_standard).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__endogenous_climb_reading, 0.08).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the contested kernel 'imposition_mechanism_kernel' into three structurally distinct readings. The endogenous_climb_reading (this story) models norm-acquisition through bottom-up adoption; the exogenous_override_reading models norm-imposition through coercion; the hybrid_legitimation_reading models combined mechanisms (symbolic authority + institutional incentives). The readings are not measurements of one constraint—they differ in ε (the referent is the same: norm-governance systems, but read differently), beneficiary/victim structures, and persistence mechanisms. Each reading is linked to its siblings via network.affects_constraints; they form a constraint family where mutual influence is recognized but each story stands as a separate ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__endogenous_climb_reading, organized, 0.25).
constraint_indexing:directionality_override(imposition_mechanism_kernel__endogenous_climb_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
