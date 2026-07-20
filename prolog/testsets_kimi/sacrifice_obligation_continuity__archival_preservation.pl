% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Law Archival Preservation (Non-Normative Reading)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint instantiates the archival_preservation reading of the
 *   sacrifice_obligation_continuity kernel. The kernel concerns the status of
 *   sacrificial law after the destruction of the Temple. Sibling readings
 *   include study_as_performance (study fulfills the commandment),
 *   performance_only (obligation awaits physical restoration), and
 *   messianic_suspension (obligation is suspended pending the messianic era).
 *   This reading uniquely claims that obligation has exited constraint space
 *   entirely; study operates as non-normative cultural memory. The claim and
 *   metrics are independent: the constraint is claimed as rope (pure
 *   coordination) while the authored metrics describe negligible extraction
 *   and coercion.
 *
 * KEY AGENTS:
 *   - Textual stewards (agenda_setters): Scholars and archivists who curate and transmit sacrificial texts without normative authority.
 *   - Heritage community (beneficiaries): The broader community that receives identity and continuity from preserved memory.
 *   - Normative traditionalists (excluded): Factions holding competing readings who reject the denial of normative force.
 *   - Academic observers (analytical): External scholars documenting the phenomenon.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.02).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Law Archival Preservation (Non-Normative Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, 'adf1c049-3c86-4b4a-8ebc-2962a1c0a666').
narrative_ontology:cs_kernel_codification('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', fixed_text).
narrative_ontology:cs_authority_grounding('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', distributed).
narrative_ontology:cs_reading_relation('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', foundational, sacrificial_law_abrogated).
narrative_ontology:cs_axiom_status(sacrificial_law_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', sacrificial_law_abrogated, empirically_contingent).
narrative_ontology:cs_axiom('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', foundational, textual_study_non_normative).
narrative_ontology:cs_axiom_status(textual_study_non_normative, holdable).
narrative_ontology:cs_axiom_grounding('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', textual_study_non_normative, conventional).
narrative_ontology:cs_reference_frame('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', archived_legal_corpus).
narrative_ontology:cs_drift_state('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', contemporary_cultural_practice, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('adf1c049-3c86-4b4a-8ebc-2962a1c0a666', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, textual_stewards).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, heritage_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, cultural_memory_persistence).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, textual_transmission_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars, archivists, and educators who curate sacrificial-law texts, teach them in academic and communal settings, and design transmission curricula. They act without claiming normative religious authority; their motivation is cultural continuity and historical fidelity. They could redirect scholarly attention to other corpora with minimal personal cost.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, textual_stewards, agenda_setter,
    organized, generational, mobile, global).

% The broader ethnic and religious community that supports or participates in preservation activities as a source of identity and historical connection. They receive curated memory, educational programming, and symbolic continuity. They are not obligated to participate or fund; exit means simply disengaging from cultural programs.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, heritage_community, beneficiary,
    moderate, generational, mobile, global).

% Religious factions who hold that sacrificial law remains binding in some formâsuspended, performative, or fulfilled through study. They are excluded from the archival-preservation framework because that framework explicitly denies normative force to the law. They would object that reducing sacrifice to cultural memory evacuates its theological substance, but they are not structurally governed by this constraint.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, normative_traditionalists, excluded,
    organized, generational, mobile, global).

% External historians, anthropologists, and religious-studies scholars who analyze the archival-preservation community as a case of ritual displacement and textual canon maintenance. They neither benefit nor pay; they document the structure from outside.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving a complex textual and ritual legal corpus across generational turnover without relying on normative obligation to motivate transmission, solving the collective-action problem of who curates, funds, and teaches material that no longer governs behavior.
% TRANSFER_FUNCTION: Moves attention, institutional resources, and cultural capital from the broader community toward specialist scholars and archival institutions, in exchange for curated continuity of memory and identity.
% ABSENT_VOICES: Normative traditionalists who hold that sacrificial law remains binding are not seated in this framework because it explicitly denies normative force to the law. They would argue that reducing sacrifice to cultural memory evacuates its theological substance and misrepresents the tradition as a museum piece rather than a living commandment.
% DISAPPEARANCE_RATIONALE: If the coordinated practice of archival study vanished, the specific institutions of textual preservationâyeshivot, archives, communal lecture seriesâwould collapse or shift to other subjects. The broader community would lose a key identity-maintenance mechanism, though it might replace it with other cultural forms. The rearrangement would be significant for the seated agents.
% FOUNDING_PROBLEM: The destruction of the central sacrificial site made physical performance impossible; the community needed a way to maintain connection to its foundational legal texts and historical identity without the central ritual mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of religion and secular ethnographers attest that displaced ritual traditions commonly transition to textual preservation; academic Jewish studies departments outside the beneficiary community corroborate that the transition to non-normative preservation is structurally complete.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).
:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.02â0.03) because the reading explicitly denies normative force: no agent is compelled to study, fund, or preserve. Suppression is near zero (0.05) because alternatives (assimilation, abandonment, engagement with other traditions) are not structurally blocked. Theater ratio is low (0.10) because preservation activities are substantive curatorial and educational work. Accessibility collapse is low (0.20) because exit is open; resistance is negligible (0.05) because there is nothing to resist. The flat measurement series across the interval reflects the stability of this non-coercive arrangement.
 *
 * PERSPECTIVAL GAP:
 *   Because the constraint is nearly pure coordination with zero normative force, all participating seats experience it as benign. The only divergence is between those inside the coordination (beneficiaries) and those outside it who hold competing readings (excluded traditionalists), but the latter are not structurally governed by this constraintâthey simply reject it. The engine will compute low Ï across all seated indices.
 *
 * DIRECTIONALITY LOGIC:
 *   Textual_stewards and heritage_community both sit near the beneficiary end of the directionality spectrum: the constraint coordinates a shared good (preserved cultural memory) without extracting from either. Both have mobile exit options and bear no costs. Normative_traditionalists are excluded from this constraint's framework; they do not participate in its directionality calculus because the reading explicitly rejects their normative premise. Academic_observers are analytical. Effective extraction (Ï) is negligible for all seated agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The archival preservation reading avoids mandatrophy by explicitly disclaiming the normative mandate that originally founded the sacrificial regime. Because it does not claim to solve the original problem (how to perform sacrifice), it does not inherit the obsolescence risk of a suspended or transformed obligation. It is not a piton because it is not maintained by inertia against a dead function; rather, it has redefined its function as cultural memory, which remains live. The low theater ratio confirms that maintenance activity is functional (preservation) rather than performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preservation_naturalness,
    'Is non-normative textual preservation a spontaneous coordination, or does it depend on unstated social coercion or identity-political investment that constitutes soft obligation?',
    'Ethnographic observation of exit behavior: do individuals who cease participation in preservation study face social sanction, identity loss, or institutional pressure?',
    'If tacit coercion exists, effective extraction is higher than authored and the constraint shifts toward tangled rope; if absent, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_naturalness, empirical, 'Whether zero normative force is structurally realized or aspirational.').

omega_variable(
    kernel_reading_location,
    'This constraint is the archival_preservation reading of the sacrifice_obligation_continuity kernel. Would the non-normative classification remain stable if sibling readings gained institutional dominance within the same communal space?',
    'Track institutional affiliation and curricula of study houses: if archival-preservation institutions begin issuing normative rulings or performance-readiness protocols, the zero-normative-force claim has drifted.',
    'Institutional instability would indicate the constraint is a scaffold or tangled rope rather than a stable rope; if stable, it confirms the reading''s structural independence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Stability of the non-normative framing against sibling reading competition.').

omega_variable(
    sibling_boundary_porosity,
    'Does the archival_preservation reading''s claim that study lacks normative force foreclose the sibling study_as_performance reading in shared institutional space, or do the boundaries remain porous?',
    'Examine whether institutions holding the archival reading ever treat study attendance as religious fulfillment or merit; if never, the readings are separable; if sometimes, the boundary is porous.',
    'A porous boundary would indicate the zero-extraction claim is overstatedâextraction re-enters through obligation-creepâand the effective Îµ rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_boundary_porosity, conceptual, 'Boundary between non-normative preservation and obligation-fulfillment readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 50, 0.08).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 50, 0.02).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 100, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__archival_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is the archival_preservation reading of the sacrifice_obligation_continuity kernel. Sibling readings instantiate structurally distinct constraints from the same kernel due to divergent Îµ values, beneficiary/victim structures, and normative claims, per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
