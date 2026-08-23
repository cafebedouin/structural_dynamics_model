% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation — Physical Performance Only (Study as Preparation)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint story captures the 'performance_only' reading of the
 *   sacrifice_obligation_continuity kernel: the Torah's command to bring
 *   sacrifices in the Jerusalem Temple remains binding as a requirement for
 *   physical performance; study of sacrificial law (kodashim) is meritorious
 *   preparation for future restoration but does not fulfill the obligation.
 *   The current generation of observant Jews bears an unfillable obligation —
 *   guilt without remedy, demand without possibility. The Temple was
 *   destroyed in 70 CE; nearly two millennia later, the obligation persists
 *   in daily prayer (amidah references to sacrifices), festival liturgy
 *   (mussaf), halakhic curriculum (entire order of Kodashim), and communal
 *   identity (kohanic status). The reading claims the obligation is a
 *   Mountain (divine law, unchangeable), but the metrics describe a Snare:
 *   high extraction (guilt, life structured around absence), active
 *   enforcement (liturgical mandate, educational curriculum, communal
 *   pressure), suppressed alternatives (study_as_performance reading exists
 *   but is subordinated), and a victim set (current generation) that cannot
 *   exit without identity rupture.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.82).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.78).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation — Physical Performance Only (Study as Preparation)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '730e9f57-3256-4887-988c-166454c6054c').
narrative_ontology:cs_kernel_codification('730e9f57-3256-4887-988c-166454c6054c', fixed_text).
narrative_ontology:cs_authority_grounding('730e9f57-3256-4887-988c-166454c6054c', lineage).
narrative_ontology:cs_interpretation_layer_present('730e9f57-3256-4887-988c-166454c6054c').
narrative_ontology:cs_reading_relation('730e9f57-3256-4887-988c-166454c6054c', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('730e9f57-3256-4887-988c-166454c6054c', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('730e9f57-3256-4887-988c-166454c6054c', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('730e9f57-3256-4887-988c-166454c6054c', foundational, physical_performance_irreducible).
narrative_ontology:cs_axiom_status(physical_performance_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('730e9f57-3256-4887-988c-166454c6054c', physical_performance_irreducible, deontological).
narrative_ontology:cs_axiom('730e9f57-3256-4887-988c-166454c6054c', foundational, study_prepares_not_fulfills).
narrative_ontology:cs_axiom_status(study_prepares_not_fulfills, holdable).
narrative_ontology:cs_axiom_grounding('730e9f57-3256-4887-988c-166454c6054c', study_prepares_not_fulfills, deontological).
narrative_ontology:cs_axiom('730e9f57-3256-4887-988c-166454c6054c', secondary, obligation_persists_through_exile).
narrative_ontology:cs_axiom_status(obligation_persists_through_exile, holdable).
narrative_ontology:cs_axiom_grounding('730e9f57-3256-4887-988c-166454c6054c', obligation_persists_through_exile, conventional).
narrative_ontology:cs_reference_frame('730e9f57-3256-4887-988c-166454c6054c', temple_operative_law).
narrative_ontology:cs_drift_state('730e9f57-3256-4887-988c-166454c6054c', contemporary_post_temple_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('730e9f57-3256-4887-988c-166454c6054c', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, rabbinic_authority_structure).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, messianic_restoration_expectation).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_observant).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, temple_study_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, temple_study_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, torah_eternal_binding_force).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, temple_centrality_in_worship).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, physical_performance_irreducibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by an obligation to bring sacrifices in the Jerusalem Temple that has not existed for nearly two millennia. Daily prayers, liturgy, and halakhic consciousness structure life around an unfillable absence. Guilt accrues for non-performance; study of sacrificial law is mandated as preparation but explicitly does not satisfy the obligation. No exit without abandoning religious identity and communal belonging.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_observant, payer,
    powerless, biographical, identity_locked, global).

% Dedicate lives to mastering the intricate laws of sacrifices (kodashim tractates, Maimonides' Hilkhot Avodah). Gain scholarly prestige and communal honor within the observant world. But their study is institutionally defined as 'preparation for future restoration' — never as fulfillment. The more expert they become, the more vividly they inhabit the gap between obligation and possibility.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, temple_study_practitioners, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, temple_study_practitioners, beneficiary).

% Maintain and transmit the obligation's binding force through halakhic decision, liturgy, and education. Authorize the interpretive move that study prepares but does not replace performance. Benefit from the obligation's persistence as a structuring center of religious authority, communal cohesion, and messianic orientation. Could theoretically modify the obligation's terms but face massive legitimacy costs for doing so.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_authority_structure, agenda_setter,
    institutional, generational, arbitrage, global).

% The eschatological framework that makes the unfulfillable obligation meaningful rather than absurd. Collects the motivational and orientational energy of the obligation's persistence without bearing its costs. The expectation itself is sustained by the obligation's unfillability — if sacrifices could be performed now, the messianic horizon would collapse.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_restoration_expectation, beneficiary,
    organized, civilizational, analytical, universal).

% The kohanim and leviim who actually performed the sacrificial service before 70 CE. Their lineal descendants retain symbolic status (first aliyah, duchaning) but the operative priesthood is extinct. They would have been the primary bearers of the obligation's performance costs and its fulfillment benefits. Their absence is the structural hole the constraint rotates around.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, historical_temple_priesthood, excluded,
    powerless, generational, trapped, local).

% Study the sacrificial system historically, philologically, and comparatively. Their work illuminates the textual tradition but carries no halakhic authority and no obligation. They see the constraint from outside its normative force.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, academic_scholars_of_ancient_ritual, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orients the entire religious system toward a transcendent center (the Temple) that exceeds present reality, maintaining communal identity across exile and providing a fixed point for messianic hope. The obligation's unfillability is the coordination mechanism — it aligns practice, prayer, study, and expectation around a shared absence.
% TRANSFER_FUNCTION: Moves psychological burden (guilt, inadequacy, yearning) from the current generation to the messianic future; moves interpretive authority and communal leadership to the rabbinic structure that manages the obligation's terms; moves scholarly labor into the kodashim curriculum without granting it fulfillments status.
% ABSENT_VOICES: The historical priesthood who would have performed the service — their perspective on whether study substitutes for performance is unrecoverable. The hypothetical future generation that will actually restore the Temple — they cannot consent to bearing the obligation's accumulated weight. Early dissenting voices (e.g., some Second Temple sects, early Christian readings) that rejected the obligation's continuity were excluded from the rabbinic canon.
% DISAPPEARANCE_RATIONALE: If the performance-only obligation vanished overnight, the entire liturgical architecture (daily amidah, mussaf, Temple-focused prayers), the kodashim curriculum in yeshivot, the kohanic/levitical status system, and the messianic restoration narrative would lose their central organizing principle. The religious world would reorganize around a different center — likely Torah study as supreme value (as in study_as_performance reading) or ethical monotheism without ritual center.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the rabbinic movement faced: how to maintain the Torah's sacrificial system as binding law when its physical performance was impossible, without either abandoning the commandments or pretending the destruction didn't happen. The performance-only reading solves this by preserving the obligation's formal demand while deferring its fulfillment to messianic restoration.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the rabbinic sources themselves (Mishnah, Talmud, Maimonides) as a deliberate interpretive choice — not an inevitable reading. Competing readings (study_as_performance, messianic_suspension) emerge from within the same tradition and dispute whether the problem was correctly framed. Modern scholars (e.g., Klawans, Balberg, Rubenstein) corroborate that the performance-only reading was one of several available responses to the Temple's destruction, not the only one.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.82: the obligation extracts lifetime orientation, daily prayer focus, educational resources, and psychological burden (guilt for non-performance) from the current generation with zero possibility of fulfillment. Suppression 0.78: the constraint's persistence depends on actively maintaining the obligation's binding force — through liturgy, curriculum, halakhic ruling, and communal norm — while marginalizing the study_as_performance reading that would reduce extraction. Theater_ratio 0.65: study of sacrificial law is performed intensively (yeshiva curriculum, daf yomi cycles, scholarly commentaries) but is institutionally defined as non-fulfilling — the performance of study replaces the performance of sacrifice without satisfying the obligation. Accessibility_collapse 0.88: no physical Temple can exist under current geopolitical conditions; the obligation's terms (specific location, priesthood, altar) make alternatives structurally impossible. Resistance 0.15: the current generation overwhelmingly accepts the obligation's binding force; dissent is channeled into messianic intensity rather than rejection.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic_authority_structure seat (agenda_setter, institutional power, arbitrage exit), the constraint appears as Rope: a genuine coordination problem (maintaining Torah unity after catastrophe) solved with minimal coercion — the community voluntarily embraces the obligation. From the current_generation_observant seat (payer, powerless, identity_locked), it computes as Snare: extraction without remedy, guilt without exit. From the temple_study_practitioners seat (payer/beneficiary, moderate, constrained), it computes as Tangled Rope: genuine scholarly coordination function exists alongside asymmetric extraction (their expertise deepens the obligation's hold on them). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Current generation observant are full targets (d ~0.95): identity-locked, powerless, bear the full extraction (guilt, obligation, life structured around absence) with no exit. Temple study practitioners are high targets (d ~0.8): constrained exit (could leave yeshiva world but at high identity cost), bear extraction while gaining secondary benefits (prestige, communal role). Rabbinic authority structure is beneficiary (d ~0.15): collects authority, coherence, communal leadership from the obligation's persistence; arbitrage exit (could modify but won't). Messianic restoration expectation is full beneficiary (d ~0.05): collects orientational energy without bearing costs. Historical priesthood is excluded (trapped, powerless) — their absence is the constraint's enabling condition. Academic scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining Torah unity post-destruction) is contested — some readings (study_as_performance) claim it was solved differently; modern scholars confirm multiple responses were available. The obligation persists despite the founding problem being arguably resolved (Torah unity maintained, Jewish survival achieved). Mandatrophy is unresolved: the arrangement continues extracting from the current generation for a coordination function that may no longer require this specific form. The performance_only reading denies mandatrophy by claiming the obligation's divine origin makes it immune to functional obsolescence — but this claim is exactly what the other readings contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the sacrifice_obligation_continuity kernel best framed as a single commitment with rival readings, or as a family of distinct constraints with different referents?',
    'Compare the ε values across readings: if performance_only yields high ε (guilt without remedy) while study_as_performance yields low ε (study satisfies), they are different constraints under the ε-invariance principle. The kernel framing is heuristic; the engine classifies each reading independently.',
    'If the kernel framing is rejected, the performance_only constraint stands alone as a snare with no need for reading_relations. If accepted, the reading_relations and axioms in cs_structure become active for cross-reading analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel abstraction clarifies or obscures the structural differences between readings.').

omega_variable(
    guilt_as_extraction_mechanism,
    'Is the guilt/psychological burden borne by the current generation a genuine extraction (transfer to beneficiaries) or an internally generated cost with no recipient?',
    'Trace whether the guilt functions as a resource for the rabbinic authority structure (motivating compliance, study, communal cohesion) or merely as a cost with no beneficiary. If the latter, extractiveness may be overstated; if the former, the transfer_function is validated.',
    'If guilt has no recipient, the constraint may be a piton (inertial persistence) rather than a snare (active extraction). If guilt fuels authority, snare classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guilt_as_extraction_mechanism, empirical, 'Whether the primary extraction (psychological burden) has a discernible recipient or is diffuse.').

omega_variable(
    study_preparation_boundary,
    'Where exactly does the boundary lie between ''preparation for future restoration'' (this reading) and ''fulfillment through study'' (study_as_performance reading)?',
    'Examine halakhic sources: does Maimonides'' ruling that ''one who studies the laws of sacrifice is as if he offered a sacrifice'' (Hilkhot Avodah 1:1) create a fulfillments claim or a preparatory merit? The boundary determines whether study_as_performance is a genuine sibling reading or a misreading of the same sources.',
    'If the boundary is porous, the two readings may be variants of one constraint (tangled_rope with study as partial fulfillment). If sharp, they are distinct constraints with different ε values and victim sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_preparation_boundary, conceptual, 'The precise halakhic distinction between preparation and fulfillment in sacrificial study.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression maintaining this constraint structural (communal enforcement, educational monopoly) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit trajectories: if former observant Jews report persistent guilt about sacrifice non-performance after leaving the community, suppression is partially internalized. If guilt dissolves with communal exit, suppression is primarily structural.',
    'If internalized, effective suppression exceeds the structural measure — the constraint travels with the agent. This would increase χ for identity_locked agents beyond the engine''s current derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in identity-locked religious obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_tr_t390, sacrifice_obligation_continuity__performance_only, theater_ratio, 390, 0.35).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_tr_t780, sacrifice_obligation_continuity__performance_only, theater_ratio, 780, 0.45).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_tr_t1170, sacrifice_obligation_continuity__performance_only, theater_ratio, 1170, 0.55).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_tr_t1560, sacrifice_obligation_continuity__performance_only, theater_ratio, 1560, 0.62).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_tr_t1950, sacrifice_obligation_continuity__performance_only, theater_ratio, 1950, 0.65).

% Extraction over time
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_be_t390, sacrifice_obligation_continuity__performance_only, base_extractiveness, 390, 0.55).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_be_t780, sacrifice_obligation_continuity__performance_only, base_extractiveness, 780, 0.62).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_be_t1170, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1170, 0.71).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_be_t1560, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1560, 0.78).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_be_t1950, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1950, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_su_t390, sacrifice_obligation_continuity__performance_only, suppression_requirement, 390, 0.5).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_su_t780, sacrifice_obligation_continuity__performance_only, suppression_requirement, 780, 0.6).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_su_t1170, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1170, 0.68).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_su_t1560, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1560, 0.74).
narrative_ontology:measurement(sacrifice_obligation_continuity__performance_only_su_t1950, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1950, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial 'sacrifice obligation after the Temple's destruction' into four structurally distinct readings with different ε values, victim sets, and coordination functions. The performance_only reading has the highest extractiveness (guilt without remedy); study_as_performance reduces extraction by granting fulfillments status to study; messianic_suspension suspends extraction; archival_preservation eliminates it. They are linked as a family because each reading cites the same textual sources and historical catastrophe but draws different structural conclusions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity__performance_only, moderate, 0.8).
constraint_indexing:directionality_override(sacrifice_obligation_continuity__performance_only, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
