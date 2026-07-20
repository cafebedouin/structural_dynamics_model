% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Gelassenheit Artifact Separation Reading
 *   domain: religious_studies/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the artifact_reading of the contested
 *   gelassenheit_separation kernel in Old Order Amish and related Anabaptist
 *   communities. Under this reading, separation from English society is
 *   operationalized as visible material distinction: any technology that
 *   resembles worldly artifacts is prohibited regardless of functional
 *   isolation or practical effect. Solar panels, modern synthetic fabrics,
 *   and other off-grid technologies are banned because they look modern, even
 *   when they would not connect the community to worldly systems. The reading
 *   prioritizes external markers of difference over internal purity or
 *   functional consequence, producing a high-extraction constraint enforced
 *   by the bishop council through the Ordnung.
 *
 * KEY AGENTS:
 *   - bishop_council: Agenda-setter (organized/constrained) â interprets Gelassenheit and enforces visible-distinction rules
 *   - amish_households: Primary payer (moderate/identity_locked) â bear the material costs of forgoing functionally harmless technology
 *   - amish_youth: Intensified payer (powerless/identity_locked) â face maximum pressure to conform to visible standards during courtship and baptism decisions
 *   - off_grid_tech_vendors: Excluded (moderate/mobile) â structurally barred from supplying functionally appropriate technology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.9).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.9).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Gelassenheit Artifact Separation Reading").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious_studies/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, '9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09').
narrative_ontology:cs_kernel_codification('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', distributed).
narrative_ontology:cs_authority_grounding('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', lineage).
narrative_ontology:cs_interpretation_layer_present('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09').
narrative_ontology:cs_reading_relation('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', gelassenheit_separation__principle_reading, forecloses).
narrative_ontology:cs_reading_relation('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', gelassenheit_separation__consequence_reading, forecloses).
narrative_ontology:cs_axiom('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', foundational, visible_distinction_as_separation).
narrative_ontology:cs_axiom_status(visible_distinction_as_separation, holdable).
narrative_ontology:cs_axiom_grounding('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', visible_distinction_as_separation, theological).
narrative_ontology:cs_axiom('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', foundational, resemblance_as_sufficient_prohibition).
narrative_ontology:cs_axiom_status(resemblance_as_sufficient_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', resemblance_as_sufficient_prohibition, conventional).
narrative_ontology:cs_reference_frame('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', visible_distinctiveness_mandate).
narrative_ontology:cs_drift_state('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', contemporary_technological_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ba8e90c-54c1-45c1-b6eb-f7585ab7dd09', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, bishop_council).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, amish_households).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, amish_youth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Gelassenheit for the church district and enforces the Ordnung. Determines whether a technology is prohibited based on its visible resemblance to English artifacts, regardless of functional isolation or off-grid capability. Authority depends on maintaining visible community boundaries.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, bishop_council, agenda_setter,
    organized, generational, constrained, local).

% Must abstain from solar panels, modern synthetic fabrics, and other technologies that resemble English artifacts even when functionally isolated and off-grid. Bear higher labor and material costs for alternatives. Physical exit is possible but identity-locked: leaving triggers shunning and loss of family, language, and eschatological community.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, amish_households, payer,
    moderate, biographical, identity_locked, local).

% Face intensified scrutiny during rumspringa and courtship. Visible conformity to technological abstinence is a prerequisite for baptism and marriage within the community. Must demonstrate rejection of English-appearing artifacts regardless of practical benefit.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, amish_youth, payer,
    powerless, biographical, identity_locked, local).

% Would supply solar panels, efficient fabrics, and other off-grid technologies tailored to Amish needs if permitted. Structurally excluded from the market by the resemblance rule, which bars their products based on appearance rather than function.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__artifact_reading, off_grid_tech_vendors, excluded,
    moderate, biographical, mobile, regional).

narrative_ontology:fixing_cost_class(gelassenheit_separation__artifact_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a visible boundary between Amish community and English society, preventing assimilation and preserving collective Anabaptist religious identity through material distinction.
% TRANSFER_FUNCTION: Moves the material cost of technological abstinenceâhigher labor, energy expense, and inefficiencyâfrom individual households to the maintenance of communal boundaries. Moves deference and compliance from members to the Ordnung authority.
% ABSENT_VOICES: Pragmatic Amish members who would adopt off-grid solar and synthetic fabrics for purely domestic benefit; youth who regard visible markers as performative rather than spiritual; alternative technology vendors who could supply culturally neutral equipment.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, Amish households would rapidly adopt off-grid solar panels and modern fabrics where functionally useful, visible distinction would blur within a generation, and the bishop council's authority to enforce Gelassenheit would collapse. Community identity would reorganize around either stricter geographic isolation or accelerated assimilation.
% FOUNDING_PROBLEM: European Anabaptist communities under persecution needed to resist cultural absorption by state churches and maintain visible separation from worldly society.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Anabaptism from academic observer seats corroborate the historical persecution context but contest its contemporary liveness in North America. The bishop council asserts continuity from a beneficiary seat. No independent external corroboration exists that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(gelassenheit_separation__artifact_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__artifact_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__artifact_reading, 0.9, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.90) because the constraint forbids technologies that would reduce labor and cost without threatening community boundaries, imposing pure deadweight loss for visible conformity. Suppression is equally high (0.90) because persistence depends on active enforcement: bishops inspect homes, regulate businesses, and apply social sanctions including shunning. Theater_ratio is moderate-high (0.55) because a growing share of enforcement addresses appearance rather than functional entanglementâfamilies hiding solar panels or synthetic clothing inside homes suggest the boundary is becoming performative. Accessibility_collapse is high (0.72) because while physical exit is possible, identity lock makes cognitive exit nearly impossible: members who leave lose language, family, and eschatological community. Resistance is moderate-low (0.35) because overt opposition is rare; dissent typically manifests as hidden adoption or youth exit during rumspringa rather than organized challenge.
 *
 * PERSPECTIVAL GAP:
 *   The bishop council seat should compute as tangled_rope or rope: from their perspective, the constraint solves a genuine coordination problem (maintaining separateness in a hyper-modern environment) and their authority is legitimate. The household and youth seats should compute closer to snare: they experience the same structure as arbitrary prohibition that extracts labor and material comfort for no functional gain. The engine derives this divergence from beneficiary/victim declarations and exit modulationâhouseholds are identity_locked targets while the council has constrained but authority-protected exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Bishop_council is the declared beneficiary (low d): the constraint subsidizes their authority and the community boundaries they are charged with maintaining. Amish_households and amish_youth are declared victims (high d): they bear the costs of technological abstinence and identity-locked exit amplifies their effective extraction. Off_grid_tech_vendors are excluded with mobile exit (very low d, but they are not in the victim array because their exclusion is structural rather than extractiveâthey lose a market but are not governed by the constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpersecution and cultural absorption of Anabaptist communitiesâis no longer live in North America in its original form. The constraint persists through mandatrophy: the mandate has shifted from survival to boundary performance. The artifact reading intensifies this drift by eliminating functional-evaluation exceptions that would have kept the constraint adaptive. The R5 genealogy flags this: founding_problem_status is contested, disappearance_verdict is world_rearranges, and the theater ratio above 0.5 indicates proxy goals replacing real function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artifact_reading_contest,
    'Is the artifact reading the authentic Gelassenheit tradition, or a modern hardening that prioritizes boundary performance over spiritual yieldedness?',
    'Historical analysis of Ordnung evolution across Amish settlements, comparing 19th-century prohibitions (which focused on pride and functional entanglement) with 20th-century artifact-based rules.',
    'If the artifact reading is a modern hardening, its high extractiveness represents accumulated drift rather than original intent, supporting reclassification toward piton or mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artifact_reading_contest, conceptual, 'Whether the artifact reading is authentic tradition or modern boundary hardening').

omega_variable(
    visible_marker_necessity,
    'Does visible technological distinction actually preserve community identity, or does it merely perform separation for external observers while imposing unnecessary internal costs?',
    'Comparative study of Amish settlements with varying Ordnung strictness: do artifact-read communities show higher retention and well-being, or higher covert defection and youth exit?',
    'If visible markers do not improve retention, the coordination function is cover for pure boundary extraction, pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visible_marker_necessity, empirical, 'Whether visible markers serve genuine identity preservation or performative extraction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (shunning, Meidung, economic exclusion) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit trajectory study: if suppression symptoms persist after physical and economic exit, the mechanism is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâthe target carries the suppression with them after exit, amplifying victim-seat severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in religious identity lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(gela_tr_t6, gelassenheit_separation__artifact_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(gela_tr_t12, gelassenheit_separation__artifact_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement(gela_tr_t18, gelassenheit_separation__artifact_reading, theater_ratio, 18, 0.49).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__artifact_reading, theater_ratio, 24, 0.53).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__artifact_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.74).
narrative_ontology:measurement(gela_be_t6, gelassenheit_separation__artifact_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(gela_be_t12, gelassenheit_separation__artifact_reading, base_extractiveness, 12, 0.82).
narrative_ontology:measurement(gela_be_t18, gelassenheit_separation__artifact_reading, base_extractiveness, 18, 0.85).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__artifact_reading, base_extractiveness, 24, 0.88).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__artifact_reading, base_extractiveness, 30, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.76).
narrative_ontology:measurement(gela_su_t6, gelassenheit_separation__artifact_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(gela_su_t12, gelassenheit_separation__artifact_reading, suppression_requirement, 12, 0.84).
narrative_ontology:measurement(gela_su_t18, gelassenheit_separation__artifact_reading, suppression_requirement, 18, 0.87).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__artifact_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__artifact_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is the artifact_reading of the gelassenheit_separation kernel. It is one of three structurally distinct readings (artifact, principle, consequence) that share a religious vocabulary but instantiate different constraints with different epsilon values and victim profiles. Decomposition follows the epsilon-invariance principle: the same kernel interpreted through different decision procedures produces non-equivalent structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
