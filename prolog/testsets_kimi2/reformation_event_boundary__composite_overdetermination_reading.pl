% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Reformation Event Boundary â Composite Overdetermination Reading
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the composite_overdetermination_reading of
 *   the reformation_event_boundary kernel. Under this reading, the
 *   Reformation was simultaneously a theological climb (doctrinal
 *   innovation), an institutional drop (Catholic collapse), a political swap
 *   (territorial realignment), and an emergent denominational
 *   proliferationânone reducible to the others. The constraint is the
 *   disciplinary enforcement of this composite frame in academic
 *   historiography, which treats monocausal or cleanly periodized accounts as
 *   methodologically defective. The reading claims structural truth for
 *   overdetermination while the metrics independently describe the frame's
 *   increasingly extractive and enforcement-dependent operation.
 *
 * KEY AGENTS:
 *   - Interdisciplinary Reformation scholars: Primary beneficiary (organized/arbitrage) â collect disciplinary authority and resources under the composite frame.
 *   - Academic gatekeepers: Agenda setter (institutional/mobile) â enforce the frame through peer review and tenure.
 *   - Confessional church historians: Primary target (moderate/identity_locked) â bear marginalization because their theological monocausality violates the composite norm.
 *   - Political monocausal historians: Secondary target (powerful/constrained) â possess global scholarly power but face pressure to adopt complexity gestures.
 *   - Synthetic narrative pedagogues: Tertiary target (moderate/constrained) â bear the pedagogical costs of transmitting overdetermined complexity.
 *   - General reading public: Excluded seat (powerless/trapped) â excluded from the academic conversation and trapped in a marketplace bereft of synthetic narratives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.72).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.8).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation Event Boundary â Composite Overdetermination Reading").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, 'd0a4b5b9-e090-4d87-a984-7cf2c45daf1b').
narrative_ontology:cs_kernel_codification('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', distributed).
narrative_ontology:cs_authority_grounding('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', practice).
narrative_ontology:cs_interpretation_layer_present('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b').
narrative_ontology:cs_reading_relation('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_axiom('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', foundational, reformation_irreducibly_composite).
narrative_ontology:cs_axiom_status(reformation_irreducibly_composite, holdable).
narrative_ontology:cs_axiom_grounding('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', reformation_irreducibly_composite, empirically_contingent).
narrative_ontology:cs_axiom('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', foundational, historiographical_overdetermination_structural).
narrative_ontology:cs_axiom_status(historiographical_overdetermination_structural, holdable).
narrative_ontology:cs_axiom_grounding('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', historiographical_overdetermination_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', overdetermined_composite_event_structure).
narrative_ontology:cs_drift_state('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d0a4b5b9-e090-4d87-a984-7cf2c45daf1b', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, interdisciplinary_reformation_scholars).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, confessional_church_historians).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, political_monocausal_historians).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, synthetic_narrative_pedagogues).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy the center of contemporary Reformation studies by publishing monographs and edited volumes that integrate theological, political, social, and institutional factors without ranking them. Their careers, grant funding, and conference circuits depend on the composite frame remaining the disciplinary default. They can move between history, religious studies, and political theory departments.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, interdisciplinary_reformation_scholars, beneficiary,
    organized, generational, arbitrage, global).

% Control peer review at flagship early modern journals, tenure committees at research universities, and grant panels for humanities funding. They enforce the composite frame by rejecting manuscripts that advance monocausal theological or political explanations of the Reformation as methodologically naive. They can relocate to other institutions or fields but are constrained by disciplinary consensus.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, academic_gatekeepers, agenda_setter,
    institutional, generational, mobile, global).

% Write from within Catholic, Lutheran, or Reformed intellectual traditions and treat the Reformation as a living theological event with doctrinal determinacy. Their identity is fused with confessional commitments; exit would require abandoning the theological self-understanding that grounds their vocation. They are marginalized in mainstream academic hiring and publishing because their monocausal theological accounts violate the composite norm.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessional_church_historians, payer,
    moderate, biographical, identity_locked, global).

% Specialists in state formation and political institutions who argue that princely interest and imperial constitutional conflict drove the Reformation. Despite substantial methodological power and archival resources, they face peer pressure to acknowledge social and theological factors they regard as secondary. Their exit is constrained by the field's expectation of interdisciplinary gesture.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, political_monocausal_historians, payer,
    powerful, biographical, constrained, global).

% Write textbooks and teach survey courses that must convey the Reformation to non-specialists. The composite overdetermination frame forces them to sacrifice narrative clarity and chronological coherence for a multi-causal mosaic that students find confusing. Their market and curricular standards constrain them to follow the academic consensus even when pedagogical effectiveness suffers.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, synthetic_narrative_pedagogues, payer,
    moderate, biographical, constrained, national).

% Seek accessible narrative history of the Reformation but encounter only academically complex, overdetermined accounts that refuse to identify protagonists or causes. They are excluded from the peer-review conversation that enforces the composite frame and are trapped in a marketplace where synthetic alternatives have been suppressed by scholarly consensus.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, general_reading_public, excluded,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates theological, political, social, and institutional historiography into a shared non-reductionist framework that permits multiple causal factors to operate simultaneously without requiring ordinal ranking or monocausal sequencing.
% TRANSFER_FUNCTION: Moves epistemic authority, journal space, and tenure-track lines from monocausal and confessional historiographical traditions to interdisciplinary and complexity-oriented scholarship; moves pedagogical burden and narrative confusion from specialists to generalist educators and students.
% ABSENT_VOICES: General readers seeking accessible narrative history; confessional communities who experience the Reformation as living theological heritage; political historians outside the early modern specialty who would simplify for comparative state-formation purposes. They are excluded from peer review, curriculum committees, and grant panels where the composite frame is enforced.
% DISAPPEARANCE_RATIONALE: If the composite overdetermination frame disappeared overnight, the interdisciplinary field of Reformation studies would collapse into competing monocausal departments and confessional faculties; textbook narratives would simplify into accessible chronologies; political and theological historians would regain parity in the academy; and the general reading public would see a resurgence of synthetic narrative history.
% FOUNDING_PROBLEM: Nineteenth- and early twentieth-century Reformation historiography was fragmented into mutually hostile confessional, nationalist, and Marxist camps that offered incompatible monocausal narratives and could not account for the simultaneous transformation of religion, politics, and society across early modern Europe.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by practicing political historians outside the benefiting interdisciplinary set who attest that the fragmentation problem persists in the form of their own marginalization; by textbook publishers who document the pedagogical difficulty of the composite frame; and by confessional seminaries who record the exclusion of their graduates from mainstream academic posts.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.72 over the interval because the composite frame evolved from a pluralistic corrective into a hegemonic gatekeeping mechanism that extracts career opportunities from monocausal scholars. Suppression reaches 0.80 because the frame now requires active enforcementâpeer review, hiring discrimination, and funding allocationâto prevent relapse into theological or political monocausality. Theater_ratio at 0.48 indicates substantial performative maintenance: much scholarly production gestures at multidimensionality without genuine analytical integration. Accessibility_collapse at 0.75 reflects that once the composite frame is accepted, monocausal alternatives become nearly unpublishable. Resistance at 0.55 captures ongoing pushback from confessional and political historians.
 *
 * PERSPECTIVAL GAP:
 *   The interdisciplinary beneficiary seat experiences the constraint as genuine intellectual liberation from reductionism, with low directionality. The confessional and political payer seats experience the same structure as epistemic extraction that forces them to adopt foreign methodological vocabulary or accept marginalization. The gatekeeper seat experiences moderate directionality: they enforce the frame but are themselves constrained by disciplinary consensus. The engine computes these divergences from beneficiary declarations and exit modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Interdisciplinary scholars are declared beneficiaries (low d, subsidized by the constraint). Confessional church historians are declared victims with identity_locked exit (high d, amplified extraction). Political monocausal historians are declared victims with constrained exit (high d but slightly lower than confessional scholars because their power affords partial mitigation). Pedagogues are victims with constrained exit (moderate-high d). The public is excluded and trapped (very high d). Gatekeepers are agenda setters with mobile exit (low-moderate d). No overrides are needed because the structural derivation chain produces accurate directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite frame began as a scaffold-like solution to the fragmentation of Reformation historiography, but it never carried a sunset clause and ossified into a tangled rope. The mandatrophy risk is mislabeling it as either a pure rope (ignoring the active suppression of monocausal alternatives) or a pure snare (ignoring the genuine coordination function that once allowed theological, political, and social historians to share a journal or department). The temporal measurements show extraction accumulation and enforcement intensification, confirming that the coordination function has been progressively colonized by rent-seeking behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_composite_frame,
    'Is the Reformation''s composite overdetermination an irreducible feature of the historical record, or a constructed historiographical convention that benefits the interdisciplinary field?',
    'Archaeological and documentary source criticism: if archival evidence shows clear sequencing of causal factors (e.g., theological rupture preceding political realignment), the composite frame is a constructed convention.',
    'If constructed, the constraint is a tangled rope or snare of disciplinary politics rather than a descriptive historical mountain; classification shifts toward higher extraction and theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_composite_frame, conceptual, 'Whether composite overdetermination is historiographical construction or historical fact').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of monocausal alternatives structural (peer review, hiring gatekeeping, funding allocation) or internalized (historians believe complexity is intrinsically virtuous and self-censor)?',
    'Survey and interview data from historians about their beliefs versus their publishing constraints; comparison of manuscript rejection rates for monocausal versus composite submissions.',
    'If internalized, effective suppression exceeds the structural measure because targets carry the constraint with them after any institutional exit; if purely structural, exit would substantially reduce extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in historiographical enforcement').

omega_variable(
    cs_framing_underdetermination,
    'Does the composite reading''s authority derive from evidence alone, or from an institutional legitimacy claim layered above historiographical practice that treats complexity as a proxy for sophistication?',
    'Sociology of the discipline: track citation networks and hiring patterns to determine whether composite-framed work is rewarded for empirical merit or for disciplinary boundary maintenance.',
    'If the latter, the authority_grounding shifts from practice to extraction, and the constraint''s classification moves toward snare-like dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framing of the composite reading as institutional self-interest versus evidence-based practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1900, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(refo_tr_t1925, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1925, 0.15).
narrative_ontology:measurement(refo_tr_t1950, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(refo_tr_t1975, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(refo_tr_t2000, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(refo_tr_t2025, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(refo_be_t1900, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(refo_be_t1925, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1925, 0.22).
narrative_ontology:measurement(refo_be_t1950, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(refo_be_t1975, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(refo_be_t2000, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(refo_be_t2025, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1900, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(refo_su_t1925, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1925, 0.25).
narrative_ontology:measurement(refo_su_t1950, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(refo_su_t1975, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(refo_su_t2000, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(refo_su_t2025, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__composite_overdetermination_reading, 0.08).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, political_swap_reading).

% DUAL FORMULATION NOTE:
% The reformation_event_boundary kernel decomposes into at least three structurally distinct constraints: composite_overdetermination_reading (tangled_rope), theological_climb_reading, and political_swap_reading. Each reading has a different epsilon, beneficiary structure, and classification. The composite reading influences its siblings by creating disciplinary pressure to acknowledge multiple causality, but it does not logically resolve the kernel's contested boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
