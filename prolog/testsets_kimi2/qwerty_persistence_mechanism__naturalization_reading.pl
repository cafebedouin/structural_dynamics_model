% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Persistence as Natural Competitive Outcome
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint story instantiates the naturalization reading of the
 *   qwerty_persistence_mechanism kernel. Under this reading, QWERTY's
 *   dominance does not reflect lock-in or active incumbent extraction, but
 *   rather the competitive survival of an adequately performing standard.
 *   Alternatives such as Dvorak are held to have lost through fair market
 *   competition rather than suppression, and switching costs are interpreted
 *   as genuine human-capital investment rather than extraction. No agent
 *   systematically benefits from the arrangement's persistence; the mechanism
 *   is read as self-enforcing competitive equilibrium.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.05).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.86).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Persistence as Natural Competitive Outcome").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic/technological").

domain_priors:emerges_naturally(qwerty_persistence_mechanism__naturalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '47b763d1-5d2e-4f7d-9ddc-2147460c575f').
narrative_ontology:cs_kernel_codification('47b763d1-5d2e-4f7d-9ddc-2147460c575f', implicit).
narrative_ontology:cs_authority_grounding('47b763d1-5d2e-4f7d-9ddc-2147460c575f', self_enforcing).
narrative_ontology:cs_reading_relation('47b763d1-5d2e-4f7d-9ddc-2147460c575f', qwerty_persistence_mechanism__lock_in_reading, forecloses).
narrative_ontology:cs_reading_relation('47b763d1-5d2e-4f7d-9ddc-2147460c575f', qwerty_persistence_mechanism__beneficiary_extraction_reading, influences).
narrative_ontology:cs_axiom('47b763d1-5d2e-4f7d-9ddc-2147460c575f', foundational, competitive_selection_produces_adequate_standards).
narrative_ontology:cs_axiom_status(competitive_selection_produces_adequate_standards, holdable).
narrative_ontology:cs_axiom_grounding('47b763d1-5d2e-4f7d-9ddc-2147460c575f', competitive_selection_produces_adequate_standards, empirically_contingent).
narrative_ontology:cs_axiom('47b763d1-5d2e-4f7d-9ddc-2147460c575f', secondary, switching_costs_reflect_genuine_human_capital).
narrative_ontology:cs_axiom_status(switching_costs_reflect_genuine_human_capital, holdable).
narrative_ontology:cs_axiom_grounding('47b763d1-5d2e-4f7d-9ddc-2147460c575f', switching_costs_reflect_genuine_human_capital, instrumental).
narrative_ontology:cs_reference_frame('47b763d1-5d2e-4f7d-9ddc-2147460c575f', competitive_equilibrium).
narrative_ontology:cs_drift_state('47b763d1-5d2e-4f7d-9ddc-2147460c575f', contemporary_technology_studies, gap(stable, minor, true)).
narrative_ontology:cs_created_at('47b763d1-5d2e-4f7d-9ddc-2147460c575f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the arrangement does not solve an active coordination problem through enforcement or deliberate design; rather, it reflects the spontaneous convergence of market participants on an adequate standard. The interoperability benefit is a side effect of competitive selection, not an engineered coordination solution.
% TRANSFER_FUNCTION: No systematic transfer occurs. The arrangement moves nothing from any agent to any other; it is a self-enforcing equilibrium without extractive flow. Switching costs reflect genuine human-capital investment rather than rent extraction.
% ABSENT_VOICES: Advocates of alternative keyboard layoutsâDvorak, Colemak, Workmanâand critics of market-fundamentalist narratives in technology studies are present in academic discourse. Under this reading, their objections are interpreted as theoretically mistaken or empirically unsupported rather than as evidence of structural exclusion.
% DISAPPEARANCE_RATIONALE: If the QWERTY arrangement vanished overnight, the global installed base of typing hardware, software defaults, and embodied human capital would require immediate re-coordination. Training regimes, equipment manufacturing, and interface design would reorganize around a replacement standard; the rearrangement would be extensive because the standard is deeply embedded, even if not actively enforced.
% FOUNDING_PROBLEM: The ongoing need for a unified, transferable keyboard layout standard that allows typists to move between equipment and manufacturers to access a global market without relearning.
% FOUNDING_PROBLEM_CORROBORATION: Ergonomic researchers and technology historians attest that keyboard standardization continues to serve a live coordination function. The naturalization reading is corroborated by independent empirical studies showing QWERTY's performance remains within the competitive range of alternatives, supporting the claim that persistence reflects ongoing adequacy rather than zombie maintenance.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_mechanism__naturalization_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.05) and suppression (0.05) are authored at floor because the reading denies any active enforcement, rent collection, or alternative suppression. Theater ratio (0.05) is minimalâthere is no performative maintenance because no party needs to justify ongoing coercion. Accessibility collapse (0.86) is high: once the standard is understood as the natural competitive winner, alternatives lose practical standing as viable replacements. Resistance (0.10) is low because the arrangement requires no organized defense; it persists through perceived adequacy rather than through conflict.
 *
 * PERSPECTIVAL GAP:
 *   A typist who experiences QWERTY as an unchangeable background condition may perceive a mountain; a Dvorak advocate perceives suppressed alternatives and coordination failure. Under this reading, the perspectival gap is attributed to the objective adequacy of QWERTY and the empirical weakness of alternative claims, rather than to structural asymmetry between beneficiary and payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are necessary. The reading asserts no concentrated beneficiary and no trapped payer; all agents are positioned near the symmetric or lightly-beneficiary end of the directionality spectrum, with effective extraction damped to near-zero for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The naturalization reading resists mandatrophy by insisting that the arrangement persists because it continues to solve a live coordination problem through adequate performance, not because it has become a zombie mechanism. The firewall is the adequacy claim: if the standard were genuinely inferior, the reading would face severe mandatrophy pressure as a dead founding problem sustained by inertia. The reading instead reframes persistence as ongoing functional legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_adequacy_contest,
    'Does QWERTY offer typing efficiency comparable to alternatives such as Dvorak, or is its adequacy an artifact of familiarity and installed-base dominance?',
    'Large-scale randomized controlled trials of typing speed, accuracy, and ergonomic load across QWERTY and alternative layouts, controlling for practice time and prior exposure.',
    'If alternatives demonstrate substantial superior performance, the naturalization reading''s adequacy claim weakens and the mechanism shifts toward lock-in or extraction; if performance is comparable, the naturalization reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_adequacy_contest, empirical, 'Whether QWERTY''s persistence reflects genuine adequacy or familiarity bias.').

omega_variable(
    market_structure_naturalness,
    'Was the historical standardization of QWERTY the product of unconstrained competitive selection, or did manufacturer collusion, path-dependent early commitments, and institutional bias structurally shape the outcome?',
    'Archival historical research on typewriter industry standard-setting processes, patent pooling, and early manufacturer agreements.',
    'If structural bias or active incumbent maintenance is documented, the naturalization reading collapses into beneficiary-extraction or lock-in; if competition was open, naturalization is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_structure_naturalness, empirical, 'Whether competitive selection was structurally fair or historically constrained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(qwer_tr_t75, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(qwer_tr_t125, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 125, 0.05).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 150, 0.05).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(qwer_be_t75, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(qwer_be_t125, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 125, 0.05).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 150, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(qwer_su_t25, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(qwer_su_t75, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 100, 0.05).
narrative_ontology:measurement(qwer_su_t125, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 125, 0.05).
narrative_ontology:measurement(qwer_su_t150, qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 150, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence_mechanism kernel decomposes into three structurally distinct constraintsânaturalization (self-enforcing competitive equilibrium), lock-in (path-dependent coordination failure), and beneficiary-extraction (incumbent rent maintenance). Each reading assigns a different causal mechanism to the same observed phenomenon and carries a different epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
