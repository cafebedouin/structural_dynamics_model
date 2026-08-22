% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Endogenous Reinterpretation of Marriage Commitment via Prophetic Revelation (1890 Manifesto)
 *   domain: religious_institutional_history
 *
 * SUMMARY:
 *   In 1890, LDS Church President Wilford Woodruff issued the Manifesto
 *   suspending plural marriage, framing the reversal as a divine revelation
 *   received on September 23, 1890. This reading treats the reversal as an
 *   endogenous reinterpretation of God's will through prophetic authority,
 *   preserving institutional legitimacy while extracting from theological
 *   consistency and the social and familial arrangements of practicing
 *   polygamists. The constraint coordinates the church's survival under
 *   federal pressure by allowing practice to shift without formally
 *   repudiating the doctrinal kernel (Section 132).
 *
 * KEY AGENTS:
 *   - church_leadership: Agenda-setter (institutional/global) â maintains prophetic authority and interpretive control
 *   - practicing_polygamists: Primary target (moderate/regional) â bear the direct costs of abandoned practice
 *   - theological_traditionalists: Secondary target (moderate/national) â bear costs of doctrinal inconsistency
 *   - ordinary_members: Beneficiary (organized/national) â receives institutional continuity
 *   - splinter_fundamentalists: Excluded (moderate/regional) â objections removed from discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.62).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Endogenous Reinterpretation of Marriage Commitment via Prophetic Revelation (1890 Manifesto)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '0f60d76c-1ec5-43ad-b9cc-6725e6d44329').
narrative_ontology:cs_kernel_codification('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', fixed_text).
narrative_ontology:cs_authority_grounding('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', lineage).
narrative_ontology:cs_interpretation_layer_present('0f60d76c-1ec5-43ad-b9cc-6725e6d44329').
narrative_ontology:cs_reading_relation('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', foundational, prophet_can_supersede_prior_revelation).
narrative_ontology:cs_axiom_status(prophet_can_supersede_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', prophet_can_supersede_prior_revelation, theological).
narrative_ontology:cs_axiom('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', foundational, manifesto_was_divine_revelation).
narrative_ontology:cs_axiom_status(manifesto_was_divine_revelation, holdable).
narrative_ontology:cs_axiom_grounding('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', manifesto_was_divine_revelation, theological).
narrative_ontology:cs_reference_frame('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', living_prophetic_authority).
narrative_ontology:cs_drift_state('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', post_manifesto_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f60d76c-1ec5-43ad-b9cc-6725e6d44329', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, ordinary_members).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, practicing_polygamists).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_traditionalists).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the prophetic office and interprets the Manifesto as a divine suspension of plural marriage. Bears the institutional burden of maintaining unity while preserving authoritative legitimacy. Cannot exit without dissolving the office itself.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives institutional survival and continued temple access in exchange for accepting the prophetic narrative. Bears diffuse cognitive cost of reconciling the reversal with prior teachings.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, ordinary_members, beneficiary,
    organized, biographical, identity_locked, national).

% Had entered plural marriages under doctrinal command and were required to abandon new marriages or live in hiding. Bore direct social, economic, and familial costs of the practice reversal.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, practicing_polygamists, payer,
    moderate, biographical, identity_locked, regional).

% Held that Section 132 was an eternal principle and experienced the Manifesto as a breach of theological consistency. Required to suppress public dissent to remain in fellowship.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_traditionalists, payer,
    moderate, generational, identity_locked, national).

% Rejected the Manifesto as political accommodation and were excommunicated or left to form splinter groups. Their objections were removed from institutional discourse.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, splinter_fundamentalists, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of institutional survival under federal coercion by producing a theologically legible mechanism to suspend plural marriage without formally repudiating the canon.
% TRANSFER_FUNCTION: Moves authority to redefine marriage practice from the fixed text of Section 132 to the living prophetic office, and transfers the social and familial costs of abandonment from the institution to practicing polygamists and theological traditionalists.
% ABSENT_VOICES: Splinter fundamentalists who later formed separate denominations and theological traditionalists who could not reconcile the reversal were excluded from the 1890 canonical process; their objections were anticipated and neutralized by the revelation framing rather than incorporated.
% DISAPPEARANCE_RATIONALE: If the revelation framing vanished, the church would have had to openly admit political capitulation or face federal destruction, collapsing the theological justification for prophetic authority and likely producing an earlier, sharper schism.
% FOUNDING_PROBLEM: The church faced federal disincorporation, threatened seizure of temples, imprisonment of leadership, and denial of Utah statehood if plural marriage continued to be practiced publicly.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative history (Edmunds-Tucker Act, 1887) and executive pressure from outside the church attest to the external threat. Post-Manifesto historical scholarship and dissenting fundamentalist records corroborate that the internal problem was institutional survival rather than a theological need for new revelation.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate: the constraint genuinely coordinates institutional survival (a real collective-action problem) but asymmetrically extracts from those whose marriages and theological certainties were sacrificed to maintain leadership legitimacy. Suppression (0.62) reflects active enforcement through ecclesiastical discipline and social boundary maintenance. Theater_ratio (0.45) captures the performative aspect of maintaining that the change was purely divine while historical context suggests federal pressure was operative. Accessibility_collapse (0.72) is high because prophetic authority collapses alternative interpretations for identity-locked members. Resistance (0.40) reflects the splintering of fundamentalist movements but limited internal institutional resistance after the initial shock.
 *
 * PERSPECTIVAL GAP:
 *   The church_leadership seat computes the constraint as coordination (preserving the church against federal destruction), while the practicing_polygamist and theological_traditionalist seats compute it as extraction (their families and doctrinal certainties were the price). The ordinary_members seat is nearer symmetric, receiving survival benefit while paying cognitive dissonance costs. The engine should compute divergent types across these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (church_leadership, ordinary_members) derive low directionality: the constraint subsidizes institutional survival and communal continuity. Victims (practicing_polygamists, theological_traditionalists) derive high directionality: the constraint extracts directly from their family structures and doctrinal commitments. Identity_locked exit options amplify effective extraction for victims while keeping beneficiaries anchored to the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the Manifesto as pure extraction (it solved a genuine collective-action problem of institutional survival) and prevents mislabeling it as pure coordination (it actively suppressed dissent and obscured the doctrine-practice gap). The R5 genealogy shows the founding problem (federal destruction) is dead, but the arrangement persists as interpretive framework, signaling potential drift toward piton if enforcement becomes purely theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_sincerity,
    'Was the 1890 Manifesto experienced as a genuine divine revelation by Woodruff and the leadership, or was it a theologically dressed political decision responding to federal pressure?',
    'Historical analysis of private journals, correspondence, and first-hand accounts from the 1889-1890 period; psychological assessment of sincerity is indirect but pattern-based.',
    'If primarily instrumental, the constraint''s theater_ratio rises and it drifts toward snare; if sincerely revelatory, the coordination function is stronger and tangled_rope classification stabilizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_sincerity, empirical, 'Whether the Manifesto was sincere revelation or instrumental political theology.').

omega_variable(
    doctrine_practice_gap,
    'Does the continued canonical status of Section 132 alongside the suspended practice constitute a structural contradiction or a legitimate theological tension resolvable through the living prophet framework?',
    'Analysis of official church curriculum, apologetic literature, and fundamentalist counter-arguments regarding the status of D&C 132.',
    'If a structural contradiction, extraction from theological consistency is higher; if a legitimate tension, the endogenous reading is more coherent and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap, conceptual, 'Whether the doctrine-practice gap is a contradiction or a tension.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of dissent following the Manifesto primarily structural (excommunication, shunning) or internalized (members accepting that prophetic authority transcends personal doctrinal objections)?',
    'Post-Manifesto member memoirs, exit narratives, and rates of public dissent versus private doubt.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates more strongly as identity_coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_reading_indexing,
    'This constraint is the endogenous_reinterpretation_reading of kernel marriage_commitment_reversal. How would classification change if the exogenous_override_reading (federal coercion without doctrinal revision) were adopted instead?',
    'Comparative analysis of the two readings'' beneficiary/victim structures: the exogenous reading preserves theological consistency but collapses institutional legitimacy.',
    'The exogenous reading would likely reclassify with higher extraction from leadership (theater/piton dynamics) and lower extraction from traditionalists, potentially shifting the primary type profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexing, conceptual, 'Structural delta between this reading and its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the marriage_commitment_reversal kernel, decomposed per the epsilon-invariance principle because each reading produces a distinct beneficiary/victim structure and epsilon value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
