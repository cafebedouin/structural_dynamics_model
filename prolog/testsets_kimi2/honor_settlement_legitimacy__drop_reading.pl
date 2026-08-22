% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Residual Honor Dueling Persistence (Drop Reading)
 *   domain: historical_sociology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the drop reading of the
 *   honor_settlement_legitimacy kernel: dueling did not contract to cognitive
 *   unthinkability but persisted as a live, if fringe, practice within
 *   residual honor cultures. The constraint governs the legitimacy of violent
 *   honor settlement in geographic and social niches where state monopoly on
 *   violence remains symbolically incomplete. State criminalization actively
 *   suppresses the practice, while the honor community enforces participation
 *   through ostracism and status sanctions. The reading asserts tangled_rope
 *   because the constraint carries a genuine coordination function (regulated
 *   dispute settlement, status maintenance) alongside asymmetric extraction
 *   (physical and legal costs borne by participants, especially marginal
 *   members). The authored metrics describe high extractiveness and
 *   suppression; the engine will compute per-seat divergence from this
 *   structural data.
 *
 * KEY AGENTS:
 *   - residual_honor_elites (powerful/identity_locked) â primary beneficiaries of status maintenance and boundary policing
 *   - pressured_duelists (moderate/trapped) â bear physical and legal costs of the constraint
 *   - marginal_honor_members (powerless/trapped) â most vulnerable to extraction, least able to decline challenges
 *   - honor_code_gatekeepers (organized/identity_locked) â administer and enforce the dueling code
 *   - state_legal_apparatus (institutional/analytical) â external suppressor, criminalizes the practice
 *   - liberal_reformers (moderate/mobile) â excluded voice rejecting violent settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.72).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.78).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Residual Honor Dueling Persistence (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '24d816eb-bb0d-4528-a5e5-acf254041940').
narrative_ontology:cs_kernel_codification('24d816eb-bb0d-4528-a5e5-acf254041940', fixed_text).
narrative_ontology:cs_authority_grounding('24d816eb-bb0d-4528-a5e5-acf254041940', lineage).
narrative_ontology:cs_interpretation_layer_present('24d816eb-bb0d-4528-a5e5-acf254041940').
narrative_ontology:cs_reading_relation('24d816eb-bb0d-4528-a5e5-acf254041940', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('24d816eb-bb0d-4528-a5e5-acf254041940', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('24d816eb-bb0d-4528-a5e5-acf254041940', foundational, honor_violence_persistent_legitimate).
narrative_ontology:cs_axiom_status(honor_violence_persistent_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('24d816eb-bb0d-4528-a5e5-acf254041940', honor_violence_persistent_legitimate, conventional).
narrative_ontology:cs_axiom('24d816eb-bb0d-4528-a5e5-acf254041940', secondary, legal_suppression_cannot_erase_honor).
narrative_ontology:cs_axiom_status(legal_suppression_cannot_erase_honor, holdable).
narrative_ontology:cs_axiom_grounding('24d816eb-bb0d-4528-a5e5-acf254041940', legal_suppression_cannot_erase_honor, empirically_contingent).
narrative_ontology:cs_reference_frame('24d816eb-bb0d-4528-a5e5-acf254041940', codified_honor_practice).
narrative_ontology:cs_drift_state('24d816eb-bb0d-4528-a5e5-acf254041940', state_suppression_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24d816eb-bb0d-4528-a5e5-acf254041940', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, residual_honor_elites).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, pressured_duelists).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, marginal_honor_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their social standing and reputation are maintained by the credible threat of violent settlement; they benefit from the boundary maintenance of the honor group and the deterrence against insult that the dueling code provides.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_elites, beneficiary,
    powerful, generational, identity_locked, regional).

% Bear the physical risk, legal jeopardy, and financial cost of dueling; often drawn into conflicts not of their choosing or with inferior skill and resources; face severe social ostracism if they decline a challenge.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, pressured_duelists, payer,
    moderate, immediate, trapped, local).

% The most vulnerable to honor culture enforcementâyounger, poorer, or lower-status members who cannot absorb the cost of a duel nor the cost of refusing one; their compliance is extracted through hierarchical social pressure.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, marginal_honor_members, payer,
    powerless, immediate, trapped, local).

% Administer the rituals and rules of duelingâseconds, referees, code interpretation; enforce compliance through social sanction and narrative control; preserve the normative framework against state suppression and liberal reform.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_code_gatekeepers, agenda_setter,
    organized, generational, identity_locked, regional).

% Criminalizes dueling and prosecutes participants; its suppression creates the fringe, underground conditions under which residual dueling persists; observes the practice from outside the honor culture's normative logic.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_apparatus, observer,
    institutional, generational, analytical, national).

% Reject the legitimacy of violent honor settlement and advocate for legal/monopolistic state dispute resolution; structurally excluded from residual honor communities and their normative framework.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, liberal_reformers, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within residual honor cultures, provides a regulated, terminal mechanism for interpersonal dispute settlement that prevents unlimited feud escalation and makes reputation costs calculable and bounded, preserving social hierarchy.
% TRANSFER_FUNCTION: Moves physical risk, legal jeopardy, and financial burden from challenged parties and lesser-skilled combatants to the broader community's status maintenance; moves enforcement labor from the state to the honor community's internal sanctioning apparatus.
% ABSENT_VOICES: Liberal bourgeois reformers, state prosecutors, and non-combatant family members are structurally excluded from the normative framework; their rejection of violence is treated as cowardice or illegitimate interference.
% DISAPPEARANCE_RATIONALE: Without the constraint, disputes within residual honor communities would lose their terminal resolution mechanism; status hierarchies would destabilize as insults go unanswered; the boundary between honor culture and mainstream society would dissolve as the internal sanctioning system collapses.
% FOUNDING_PROBLEM: In the absence of effective state monopoly on violence, how to regulate interpersonal conflict so that insults and injuries do not escalate into blood feuds, while preserving a stratified status order.
% FOUNDING_PROBLEM_CORROBORATION: State legal historians and liberal reformers (outside the beneficiary set) attest the founding problem is solved by modern legal institutions; residual honor elites attest it persists. No neutral corroboration existsâthe dispute is precisely over whether the founding problem still obtains in the residual niche.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because participants bear combined physical, legal, and social costs that exceed the coordination benefit they individually receive. Suppression (0.78) is high because the constraint must actively suppress non-violent alternatives (courts, apology, refusal) through social ostracism and must persist despite state criminalization. Theater ratio (0.45) reflects the highly codified, ritualized character of duelingâperformative elements are substantial but do not eliminate real violence. Accessibility collapse (0.70) captures the near-total absence of acceptable alternatives once a challenge is issued within the culture. Resistance (0.60) reflects sustained state prosecution and sporadic individual refusal.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (pressured_duelists, marginal_honor_members) and the beneficiary seat (residual_honor_elites) should compute to different constraint types: from the elites' perspective the arrangement is a regrettable but necessary regulator of social order; from the trapped payers' perspective it is coerced extraction backed by social annihilation. The gatekeepers' identity-locked position yields a computed type closer to the elites', while the state's analytical seat sees a snare or tangled rope depending on whether it credits the coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Residual_honor_elites are declared beneficiaries: they capture the coordination surplus of stable hierarchy without bearing proportionate individual risk (they can often choose opponents or avoid challenges). Pressured_duelists and marginal_honor_members are declared victims/payers: they bear the physical and legal extraction. The gatekeepers are agenda_setters with identity_locked exitâtheir directionality sits near the beneficiary end because their authority depends on the constraint's persistence. State_legal_apparatus is an observer with analytical exit and near-zero directionality toward the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The drop reading prevents mandatrophy mislabeling by insisting that the founding problem (unregulated interpersonal violence in the absence of complete state coverage) is contested rather than dead. If the problem were dead everywhere, the constraint would be a piton or snare; because it is asserted as live in residual niches, the coordination function remains potentially valid, supporting tangled_rope over pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persistence_vs_performance,
    'Does the fringe persistence of dueling represent genuine live practice or theatrical memory-work performed for cultural boundary maintenance?',
    'Archival documentation of actual duels, police records, and injury/mortality data from residual communities versus purely discursive or nostalgic evidence.',
    'If purely theatrical, classification shifts toward piton; if genuine, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_vs_performance, empirical, 'Whether fringe dueling is genuine practice or performance').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state legal penalties, geographic isolation) or internalized (shame, cowardice, identity fusion with honor culture)?',
    'Post-exit suppression trajectory: if individuals who leave the honor culture continue to feel compelled to duel or experience identity collapse, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measure; targets carry the constraint with them after exit, amplifying computed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_scope,
    'Does the drop reading''s claim of niche persistence coexist with the contraction reading''s claim of mainstream elimination, or do they compete for the same empirical territory?',
    'Scope decomposition: establish whether the readings describe different social strata and geographies (non-competing) or the same population (competing).',
    'If non-competing, the kernel decomposes by scope rather than contradictory empirical claims; if competing, evidence may foreclose one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Scope relationship between drop and contraction readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t10, honor_settlement_legitimacy__drop_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__drop_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(hono_tr_t30, honor_settlement_legitimacy__drop_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__drop_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(hono_tr_t50, honor_settlement_legitimacy__drop_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__drop_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hono_be_t10, honor_settlement_legitimacy__drop_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__drop_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(hono_be_t30, honor_settlement_legitimacy__drop_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__drop_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(hono_be_t50, honor_settlement_legitimacy__drop_reading, base_extractiveness, 50, 0.69).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__drop_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(hono_su_t10, honor_settlement_legitimacy__drop_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__drop_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(hono_su_t30, honor_settlement_legitimacy__drop_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(hono_su_t40, honor_settlement_legitimacy__drop_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(hono_su_t50, honor_settlement_legitimacy__drop_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__drop_reading, suppression_requirement, 60, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the honor_settlement_legitimacy kernel. The kernel decomposes into structurally distinct claims because the epsilon values and stakeholder configurations differ across readings: contraction_reading treats the constraint as culturally dead (low extraction, no parties), composite_reading as overdetermined decline (multiple mechanisms, moderate extraction), and drop_reading as persistent fringe coordination-extraction hybrid (high extraction for residual participants).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
