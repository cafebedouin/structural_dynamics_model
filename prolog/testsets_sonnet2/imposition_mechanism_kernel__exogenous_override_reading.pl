% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: Imposed Norm Enforcement via State Monopoly on Violence (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the exogenous-override reading of the
 *   imposition_mechanism_kernel: a new norm — administrative, linguistic,
 *   sartorial, religious, or legal — is imposed on a population by a state
 *   that grounds its authority in the monopoly on violence rather than in any
 *   prior or emergent cultural acceptance. On this reading, the norm never
 *   climbs from below and is never legitimated through symbolic transfer; it
 *   is installed and held in place by enforcement, and compliance tracks the
 *   visible presence of the coercive apparatus rather than converging toward
 *   voluntary adoption. This is a distinct constraint from the
 *   endogenous_climb_reading (where popular adoption precedes and outruns
 *   state mandate) and the hybrid_legitimation_reading (where symbolic
 *   authority transfer and institutional incentive combine with coercion) —
 *   each reading has its own epsilon, its own beneficiary/victim structure,
 *   and its own type, linked through network.affects_constraints and
 *   cs_structure.reading_relations rather than folded into one story.
 *
 * KEY AGENTS:
 *   - central_state_apparatus: agenda_setter (institutional/arbitrage) — declares and enforces the norm, collects governability returns
 *   - loyalist_administrative_class: beneficiary (organized/constrained) — enforces on the state's behalf for preferment
 *   - traditional_local_elites: payer (powerful/trapped) — customary authority displaced by force
 *   - noncompliant_subject_populations: payer (powerless/trapped) — bear fines and punishment, comply only under monitoring
 *   - cultural_minority_communities: payer (powerless/trapped) — entire cultural repertoire targeted for replacement
 *   - religious_and_customary_authorities: excluded (organized/constrained) — delegitimized, not consulted
 *   - historians_of_state_formation: observer (analytical/analytical) — reads the enforcement record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.68).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.86).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "Imposed Norm Enforcement via State Monopoly on Violence (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, 'e2a41770-755e-4536-8dd2-4a3f0fad2636').
narrative_ontology:cs_kernel_codification('e2a41770-755e-4536-8dd2-4a3f0fad2636', distributed).
narrative_ontology:cs_authority_grounding('e2a41770-755e-4536-8dd2-4a3f0fad2636', extraction).
narrative_ontology:cs_interpretation_layer_present('e2a41770-755e-4536-8dd2-4a3f0fad2636').
narrative_ontology:cs_reading_relation('e2a41770-755e-4536-8dd2-4a3f0fad2636', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2a41770-755e-4536-8dd2-4a3f0fad2636', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('e2a41770-755e-4536-8dd2-4a3f0fad2636', foundational, legitimacy_derives_from_coercive_capacity_not_consent).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_coercive_capacity_not_consent, holdable).
narrative_ontology:cs_axiom_grounding('e2a41770-755e-4536-8dd2-4a3f0fad2636', legitimacy_derives_from_coercive_capacity_not_consent, empirically_contingent).
narrative_ontology:cs_axiom('e2a41770-755e-4536-8dd2-4a3f0fad2636', secondary, compliance_is_monitoring_conditional_not_internalized).
narrative_ontology:cs_axiom_status(compliance_is_monitoring_conditional_not_internalized, holdable).
narrative_ontology:cs_axiom_grounding('e2a41770-755e-4536-8dd2-4a3f0fad2636', compliance_is_monitoring_conditional_not_internalized, empirically_contingent).
narrative_ontology:cs_reference_frame('e2a41770-755e-4536-8dd2-4a3f0fad2636', coercive_installation_baseline).
narrative_ontology:cs_drift_state('e2a41770-755e-4536-8dd2-4a3f0fad2636', post_enforcement_capacity_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e2a41770-755e-4536-8dd2-4a3f0fad2636', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, loyalist_administrative_class).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_local_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, noncompliant_subject_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, cultural_minority_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares the new norm (a dress code, a language mandate, a religious observance, a legal practice) and backs it with the coercive machinery of the state — police, garrisons, tax inspectors, courts. Does not wait for or require popular buy-in; compliance is monitored and punished. Collects the political and often fiscal returns of standardization: easier taxation, conscription, administration across a previously heterogeneous population.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Local officials, informers, and enforcers who adopt the new norm early and administer its imposition on others. Gain preferment, office, and protection from the coercive apparatus in exchange for enforcing compliance among their own communities. Their exit option is bounded by dependence on continued state patronage.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, loyalist_administrative_class, beneficiary,
    organized, biographical, constrained, national).

% Held prior authority grounded in customary or religious legitimacy that the imposed norm displaces or delegitimizes. Resisting risks confiscation, imprisonment, or execution; publicly complying while privately maintaining old practice invites surveillance. Their local standing is structurally undermined by a norm they did not choose and do not accept as legitimate.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_local_elites, payer,
    powerful, biographical, trapped, regional).

% Ordinary subjects who continue old practices out of habit, belief, or poverty (unable to afford compliance costs — new dress, new documents, new rituals). Face fines, corporal punishment, or exclusion from state services. Compliance is conditional entirely on the visible presence of enforcement; practice reverts wherever monitoring lapses.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, noncompliant_subject_populations, payer,
    powerless, biographical, trapped, regional).

% Communities whose entire cultural repertoire — language, dress, calendar, kinship practice — is the target of the override. The norm does not compete with theirs on persuasive grounds; it replaces theirs by force, with continuation of old practice criminalized or taxed into extinction. No credible exit exists short of flight or clandestine practice.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, cultural_minority_communities, payer,
    powerless, generational, trapped, regional).

% Priests, elders, and guild heads whose authority derived from the displaced order. They are not consulted in the norm's design and their objections are treated as sedition rather than input. Some are co-opted into the loyalist class; those who are not lose their institutional platform entirely.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, religious_and_customary_authorities, excluded,
    organized, generational, constrained, regional).

% Study the archival record — enforcement logs, tax rolls, court records of prosecution for noncompliance, petitions of resistance — to assess whether the imposed norm ever achieved independent cultural acceptance or remained perpetually enforcement-dependent.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, historians_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state achieves administrative legibility and uniform governability across a heterogeneous population — a single tax code, a single legal language, a single set of registrable identities — which genuinely lowers the cost of ruling at scale.
% TRANSFER_FUNCTION: Moves practical autonomy, customary status, and often material resources (fines, confiscated property, forced labor for compliance infrastructure) from subject populations and displaced local elites to the central state and the loyalist officials who administer the norm on its behalf.
% ABSENT_VOICES: The populations whose prior practice is being overridden are not party to the decision to override it; religious and customary authorities who would contest the norm's legitimacy on its own terms are excluded from the process and treated as threats rather than interlocutors.
% DISAPPEARANCE_RATIONALE: Withdraw the coercive apparatus and, on this reading, compliance collapses rapidly in regions without independent cultural buy-in: old practices resume, displaced elites reassert customary authority, and the loyalist administrative class loses its basis for office. The norm was never carried by anything other than the threat behind it.
% FOUNDING_PROBLEM: The state needed uniform, monitorable behavior across a population it did not control culturally — for taxation, conscription, and administrative legibility — and could not wait for, or did not attempt, persuasion.
% FOUNDING_PROBLEM_CORROBORATION: The central state apparatus and loyalist officials attest the norm is now settled practice. Independent historians working from enforcement and prosecution records (rather than state proclamations) attest that compliance tracked monitoring intensity for generations rather than converging on voluntary acceptance — corroboration from outside the beneficiary set that the founding problem (governability without consent) remained live long after the norm was declared solved.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the real transfer of autonomy, status, and resources from subject populations to the state and its loyalist administrators, layered on top of a genuine coordination gain (administrative legibility). Suppression is high and rising (0.70 to 0.86) because, on this reading, the norm's persistence depends structurally on continuous enforcement — there is no cultural ratchet doing independent work. Theater ratio is moderate and slowly climbing (0.18 to 0.32) as some enforcement activity shifts from substantive compliance-checking to performative displays of state presence (parades, loyalty oaths, inspection tours) once initial resistance is broken but full internalization never arrives. Accessibility collapse is only moderate (0.48) — deliberately lower than a mountain profile — because alternatives (old practice, flight, clandestine continuation) are suppressed but not eliminated; resistance stays real and substantial (0.74) throughout the interval, which is precisely the signature this reading claims: legitimacy contested, not achieved, only overridden.
 *
 * PERSPECTIVAL GAP:
 *   From the central state apparatus's seat, the norm is settled, successful coordination — uniformity achieved, governance costs lowered. From the trapped payer seats, the same arrangement is experienced as an ongoing, unresolved imposition whose acceptance was never won, only enforced. The engine should compute these as structurally different seat-level classifications from the same authored data, which is the point of authoring per-seat power/exit rather than a single narrative verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The central state apparatus sits at the full-beneficiary end: it sets the norm, controls enforcement, and collects the governability and fiscal returns. The loyalist administrative class benefits derivatively and is directionally close to beneficiary but with constrained exit, since its position depends entirely on continued state backing. Traditional elites, noncompliant populations, and cultural minorities are targets with trapped exit — they bear the extraction directly and have no credible alternative given the coercive backing behind the norm. Religious and customary authorities are excluded rather than coordinated: their exclusion from voice is the specific mechanism this reading identifies as distinguishing override from either bottom-up climb or hybrid legitimation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure snare) holds because a genuine coordination function exists on this reading — administrative legibility and uniform governance are real state interests, not merely pretextual. The classification prevents both mislabeling this as pure extraction with no coordination logic at all, and mislabeling it as legitimate coordination simply because a coordination benefit exists somewhere in the structure. The R5 corroboration test does the real work: the founding problem (governability without consent) is attested as still-live decades in by observers outside the beneficiary set, which is exactly the pattern that should keep suppression-dependent norms out of the mountain or rope categories regardless of how long they persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_vs_climb_empirical_indeterminacy,
    'For any given historical case where a norm is attributed to state imposition, is the exogenous-override reading the historically correct one, or does the archival record actually support endogenous climb or hybrid legitimation instead?',
    'Comparative analysis of enforcement-intensity records against compliance persistence after enforcement lapses or state capacity weakens (wars, successions, fiscal crises) — if compliance holds independent of monitoring, the override reading is empirically wrong for that case; if compliance collapses with monitoring, it corroborates the override reading.',
    'Misclassifying an endogenous-climb case as exogenous-override would overstate suppression and understate genuine cultural adoption; the reverse error would understate the coercive dependency and misread a fragile imposition as settled legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_vs_climb_empirical_indeterminacy, empirical, 'Whether the override reading is the historically accurate account for any specific imposed-norm case, as opposed to climb or hybrid readings.').

omega_variable(
    reading_boundary_location,
    'Where exactly does the structural disagreement between the three kernel readings live — in the beneficiary/victim structure, in the enforcement-dependency of compliance, or in the legitimacy claim itself?',
    'Compare the three sibling stories'' base_properties and stakeholder structures directly: if suppression and resistance trajectories diverge sharply while beneficiary sets remain similar, the disagreement is located in enforcement-dependency; if beneficiary/victim sets themselves differ, the disagreement is located in who the arrangement is understood to serve.',
    'Locating the disagreement correctly clarifies which sibling reading a new historical case should be assigned to, and prevents treating the three readings as a single averaged constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_location, conceptual, 'Where the structural disagreement among the three kernel readings is actually located.').

omega_variable(
    override_permanence_question,
    'Does a norm installed purely by exogenous override ever transition into genuine cultural acceptance over sufficiently long civilizational timescales, effectively migrating into the hybrid or climb reading''s territory?',
    'Longitudinal study of imposed norms across multiple generations after the coercive apparatus that installed them has weakened or vanished — does the norm persist voluntarily, or does it revert?',
    'If override-imposed norms reliably transition to independent legitimacy over generations, this reading''s classification would need to be time-indexed rather than treated as a stable end-state; if they reliably revert once enforcement lapses, it corroborates treating override as structurally distinct and non-convergent with the other readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(override_permanence_question, empirical, 'Whether exogenous-override norms transition into hybrid or endogenous legitimacy over long timescales, or remain permanently enforcement-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(impo_tr_t8, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(impo_tr_t24, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(impo_tr_t32, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(impo_be_t8, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(impo_be_t24, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(impo_be_t32, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(impo_su_t8, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 8, 0.78).
narrative_ontology:measurement(impo_su_t16, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 16, 0.83).
narrative_ontology:measurement(impo_su_t24, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 24, 0.85).
narrative_ontology:measurement(impo_su_t32, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 32, 0.86).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of imposition_mechanism_kernel, each authored as a separate constraint per the ε-invariance principle: the exogenous_override_reading (this story; tangled_rope, high suppression, contested legitimacy), the endogenous_climb_reading (bottom-up adoption preceding mandate; expected rope-flavored, low enforcement dependency), and the hybrid_legitimation_reading (symbolic authority transfer plus institutional incentive; expected mixed profile between the other two). The three share a kernel but instantiate structurally distinct constraints with different epsilon values, different beneficiary/victim structures, and different classifications — they are linked here rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
