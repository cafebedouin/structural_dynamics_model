% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Direct Performance of Divine Command (study_as_exercise reading)
 *   domain: religious/legal/commitment_system
 *
 * SUMMARY:
 *   This story generates the study_as_exercise reading of the
 *   temple_sacrifice_commitment kernel: the claim that sustained textual
 *   study of sacrificial law (korbanot) is not a preparatory substitute for,
 *   or archival record of, a suspended commandment, but is itself the full
 *   and direct performance of that commandment under conditions where the
 *   Temple does not stand. Under this reading the commitment is never
 *   actually unoccupied — it simply changed its mode of occupation from
 *   material act to intellectual act, with no deficit remaining to be filled.
 *   This story authors ONLY this reading as a clean, ε-invariant constraint;
 *   the sibling readings (performance_only, hybrid_preparatory,
 *   symbolic_transformation) are separate constraints with their own ε, their
 *   own beneficiary/victim structure, and their own classification, linked
 *   here only via network edges and omega variables per Rule 2.
 *
 * KEY AGENTS:
 *   - studying_community: primary beneficiary (organized/identity_locked) — discharges the commandment through study, bears no extraction
 *   - yeshiva_scholars: agenda_setter and beneficiary (institutional/identity_locked) — sets what counts as adequate study-performance, gains institutional legitimacy from the reading
 *   - restorationist_communities: excluded voice (moderate/constrained) — holds the commitment materially suspended; not addressed within this reading's own terms
 *   - textual_tradition_itself: analytical observer (non-agent) — the corpus cited as authorizing the equivalence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.03).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study of Sacrifice Law as Direct Performance of Divine Command (study_as_exercise reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious/legal/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '9193305a-e5cc-4f25-b9c4-f6de3ce9b582').
narrative_ontology:cs_kernel_codification('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', fixed_text).
narrative_ontology:cs_authority_grounding('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', lineage).
narrative_ontology:cs_interpretation_layer_present('9193305a-e5cc-4f25-b9c4-f6de3ce9b582').
narrative_ontology:cs_reading_relation('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', foundational, study_constitutes_direct_commandment_performance).
narrative_ontology:cs_axiom_status(study_constitutes_direct_commandment_performance, holdable).
narrative_ontology:cs_axiom_grounding('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', study_constitutes_direct_commandment_performance, conventional).
narrative_ontology:cs_axiom('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', secondary, commandment_fulfillment_requires_no_material_instantiation).
narrative_ontology:cs_axiom_status(commandment_fulfillment_requires_no_material_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', commandment_fulfillment_requires_no_material_instantiation, conventional).
narrative_ontology:cs_reference_frame('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', second_temple_sacrificial_practice).
narrative_ontology:cs_drift_state('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', post_destruction_rabbinic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9193305a-e5cc-4f25-b9c4-f6de3ce9b582', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, yeshiva_scholars).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, torah_study_equals_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in sustained textual study of the sacrifice laws (korbanot) as a substantive religious practice in its own right. Understands this study to discharge the underlying commandment fully — not as a stopgap or placeholder, but as the commandment's actual mode of fulfillment when the Temple does not stand. Derives spiritual, communal, and identity continuity from this practice; the practice requires no altar, no priesthood in operation, and no material sacrificial economy to be complete.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, civilizational, identity_locked, global).

% Teach and transmit the tractates of sacrificial law (Zevachim, Menachot, and related material) as core curriculum, treating mastery of this material as equivalent in religious value to other areas of legal study, and in this specific reading, as equivalent to the sacrificial act itself. They set the pedagogical and interpretive agenda for what counts as fulfilling this area of the commandment and benefit from the vitality and legitimacy this reading confers on their institutional role.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, yeshiva_scholars, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, yeshiva_scholars, agenda_setter).

% Hold that the commandment remains materially suspended and actively work toward or long for literal Temple restoration and resumed sacrificial practice. Within the study_as_exercise reading their material project is treated as unnecessary to full commandment fulfillment, since study alone already occupies the commitment — they are not addressed by this reading's own terms because this reading does not concede any deficit for them to fix.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, restorationist_communities, excluded,
    moderate, generational, constrained, national).

% The corpus of halakhic literature (Mishnah, Talmud, later codifiers) that this reading cites as authorizing study-as-performance. Included for completeness as the interpretive substrate rather than as an acting party.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, textual_tradition_itself, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, textual_tradition_itself).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed community around a shared, portable mode of covenant fidelity that requires no territorial Temple, no functioning priesthood, and no live sacrificial economy — allowing the commandment's normative force to remain fully binding and fully dischargeable under diaspora conditions.
% TRANSFER_FUNCTION: Moves nothing extractive between parties: the practice transfers attention and intellectual labor from the individual scholar into communal transmission of the tradition, and returns communal standing, continuity of identity, and (by this reading's own terms) full commandment-discharge to the same studying population. No party pays a cost that another party collects.
% ABSENT_VOICES: Restorationist communities who hold the commandment materially suspended are not represented within this reading's own framework — the reading does not treat their position as a gap needing address, since it holds the commitment already fully occupied. Their objection ('you have declared solved what is actually deferred') is visible only from outside this reading, in the sibling readings.
% DISAPPEARANCE_RATIONALE: Within the reading's own terms, if the study practice vanished, the community would lose its principal mode of discharging this commandment and covenant fidelity would suffer a real, felt rupture — arrangements (curricula, ordination tracks, communal calendars of study) are genuinely organized around it. From outside the reading (the performance_only sibling), nothing material would change because nothing material is currently occupying the commitment anyway — hence contested rather than a clean world_rearranges.
% FOUNDING_PROBLEM: After the Temple's destruction, the commandments concerning sacrifice had no material site of performance; the tradition needed an account of continued commandment-fulfillment that did not require territorial restoration or resumed priestly function.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources (attributed within the tradition to Rav, at BT Menachot 110a and parallels) attest the study-as-sacrifice equivalence directly, but this attestation originates within the same interpretive lineage that benefits from the reading's legitimacy. Restorationist voices, from outside the studying community's own framework, attest that the founding problem (absence of material performance) remains fully live and is not resolved, only reinterpreted — no attestation exists from a party with no stake in either outcome, which this story states plainly.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because, by this reading's own lights, no party pays for another party's gain: the studying community and the scholars who teach them occupy the same commitment and receive the same discharge. Suppression is low (0.05) because nothing coercive is required to sustain the practice — no one is compelled to study under threat, and no rival practice is actively suppressed by this reading's operation (the exclusion of restorationist voices is a silence, not an enforcement act). Theater ratio is low and stable (0.08-0.10 across the full interval) because the study is authored as substantively occupying the commandment, not as performative gesture standing in for something else — that theatrical possibility is exactly what the performance_only sibling reading asserts and this reading denies. Accessibility collapse is low-moderate (0.15): alternative modes of engaging the commandment (restoration advocacy, symbolic reinterpretation) remain fully visible and practiced by other communities: this reading does not need to foreclose them to be internally coherent, it simply does not require them.
 *
 * PERSPECTIVAL GAP:
 *   From inside this reading, the studying community and the teaching institutions see a fully coordinated, fully discharged commitment: rope, not tangled rope, because no coordinated party is also a paying party. From outside — from the restorationist reading's vantage — the same practice looks like it quietly retired a material obligation by redefining its terms, which is precisely the sibling dispute this story routes to omega variables rather than resolving here.
 *
 * DIRECTIONALITY LOGIC:
 *   Both named beneficiary groups sit near the full-beneficiary end of directionality: the studying community receives spiritual/covenantal discharge, the yeshiva scholars receive institutional legitimacy and pedagogical authority, and neither pays a cost the other collects. No victim group is declared because this reading, taken on its own terms, has no structural target — the restorationist communities are excluded from the conversation rather than extracted from within it, which is why they appear as `excluded` rather than `payer`.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (material performance impossible without a Temple) is authored as contested rather than resolved, because whether study fully occupies the commandment or merely holds its place is exactly what separates this reading from its siblings. This story does not resolve that dispute; it states the reading's own answer (the problem is not merely managed but actually solved by study) while documenting, via omega and via the founding_problem_corroboration field, that no attestation exists from outside interested parties. This prevents the story from either mislabeling the sibling disputes as settled or smuggling extraction into a reading that, by its own structure, has none.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_direct_performance_vs_placeholder,
    'Is textual study of sacrificial law genuinely a direct, complete performance of the underlying commandment, or is it a placeholder that manages the absence of material performance without actually discharging it?',
    'No empirical resolution mechanism exists: this is a live doctrinal dispute within the halakhic tradition itself, adjudicated by interpretive authority and communal reception rather than by external evidence. Different communities settle it differently and have for centuries.',
    'If study is genuinely direct performance, this reading is correct and the constraint is a pure rope with no victims. If study is a placeholder, the correct classification shifts toward the hybrid_preparatory or performance_only sibling readings, which carry different beneficiary/victim structures and potentially non-zero extraction (e.g., a claim that the studying community''s confidence rests on an unwarranted equivalence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_direct_performance_vs_placeholder, conceptual, 'Core kernel dispute: whether study equals performance or merely occupies its absence.').

omega_variable(
    excluded_restorationist_voice_weight,
    'Should the restorationist communities'' dissent from this reading be treated as a live structural objection (affecting classification) or as a separate theological position outside this constraint''s scope entirely?',
    'Track whether restorationist advocacy produces material consequences (organized movements toward reconstruction, political action) that would constitute an observable rival claim on the same commitment, versus remaining purely doctrinal commentary with no material trace.',
    'If restorationist advocacy remains purely doctrinal, this reading''s zero-extraction, no-victim classification stands undisturbed as an internally coherent reading. If restorationist advocacy generates material consequences that this reading''s community must respond to or suppress, the excluded voice becomes structurally relevant and the classification would need revisiting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_restorationist_voice_weight, conceptual, 'Whether the excluded sibling-reading advocates constitute mere commentary-grade absence or a structurally relevant dissent.').

omega_variable(
    cs_framing_kernel_vs_institution,
    'Is the correct commitment-system kernel here the sacrifice law text itself (fixed_text framing) or the broader claim of unbroken covenant fidelity that the text''s study is said to serve (a legitimacy-narrative framing one level up)?',
    'Compare how the classification would shift if the kernel were reframed as the covenant-fidelity-continuity narrative rather than the sacrificial-law corpus: under the narrative framing, yeshiva institutions'' interpretive authority itself becomes the object under contest, which could surface extraction (institutional legitimacy capture) invisible under the text-framing.',
    'Under the text-framing (adopted here), authority_grounding is lineage/practice and the reading reads as a clean rope. Under the narrative framing, authority_grounding could shift toward extraction (institutions benefiting from being the sole legitimate interpreters of what counts as covenant fidelity), which would push the classification toward tangled_rope. This story adopts the text-framing because the sibling readings are themselves framed as readings of the sacrifice-law corpus, not of the covenant-fidelity narrative, and consistency with the kernel-family requires matching that framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_institution, conceptual, 'Alternative kernel framing (text vs. legitimacy-narrative) that would change the CS classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 300, 0.08).
narrative_ontology:measurement(temp_tr_t700, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 700, 0.09).
narrative_ontology:measurement(temp_tr_t1100, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1100, 0.09).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 300, 0.02).
narrative_ontology:measurement(temp_be_t700, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 700, 0.03).
narrative_ontology:measurement(temp_be_t1100, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1100, 0.03).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1900, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__study_as_exercise, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This story is one of four members of the temple_sacrifice_commitment kernel family, each instantiating a distinct reading with its own ε and structure. study_as_exercise (this story) authors near-zero extraction and no victims. performance_only is expected to author the same underlying arrangement as archival/inert (potentially higher accessibility_collapse on the equivalence claim it rejects). hybrid_preparatory is expected to carry a scaffold shape with an implicit eschatological sunset. symbolic_transformation is expected to ground its authority claim in a doctrine of authorized transformation rather than direct equivalence, which may alter its authority_grounding relative to this story's lineage grounding. All four should be read as siblings under one kernel, never merged into a single averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
