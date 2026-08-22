% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Study of Kodashim as Preparatory Preservation for Messianic Temple Restoration
 *   domain: religious/legal/textual
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple, rabbinic tradition held
 *   that the laws of sacrificial offerings (Kodashim) remained halakhically
 *   binding even though performance became physically impossible. This
 *   reading treats the ongoing obligation to study these laws as instrumental
 *   preparation: the technical knowledge must be preserved intact so that,
 *   upon messianic restoration of the Temple, priests and courts can resume
 *   correct performance without loss of accuracy. The beneficiary of this
 *   arrangement is structurally a future generation that does not yet exist;
 *   the cost is borne by the present generation, who invest study time in a
 *   system they cannot enact. This is one of three readings of the same
 *   kernel — the study_as_performance reading holds that study itself enacts
 *   the sacrificial function regardless of Temple absence, and the
 *   study_as_archive reading holds that the material is historical/identity
 *   preservation without live legal force. This story concerns only the
 *   preparation reading; the siblings are separate constraints with their own
 *   ε values.
 *
 * KEY AGENTS:
 *   - current_generation_practitioners: bear the study burden without present ritual payoff
 *   - rabbinic_academies: administer and perpetuate the obligation, gain institutional continuity
 *   - future_messianic_generation: structural beneficiary, not a present actor
 *   - study_as_performance_adherents and study_as_archive_adherents: excluded sibling-reading holders
 *   - textual_critics_and_historians: analytical observers of transmission history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.22).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.28).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.22).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, scaffold).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Study of Kodashim as Preparatory Preservation for Messianic Temple Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/legal/textual").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_preparation).
narrative_ontology:has_sunset_clause(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '058369cc-1cb6-4106-8b5d-17d9a748d0bc').
narrative_ontology:cs_kernel_codification('058369cc-1cb6-4106-8b5d-17d9a748d0bc', fixed_text).
narrative_ontology:cs_authority_grounding('058369cc-1cb6-4106-8b5d-17d9a748d0bc', lineage).
narrative_ontology:cs_interpretation_layer_present('058369cc-1cb6-4106-8b5d-17d9a748d0bc').
narrative_ontology:cs_reading_relation('058369cc-1cb6-4106-8b5d-17d9a748d0bc', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('058369cc-1cb6-4106-8b5d-17d9a748d0bc', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('058369cc-1cb6-4106-8b5d-17d9a748d0bc', foundational, law_remains_binding_but_currently_unperformable).
narrative_ontology:cs_axiom_status(law_remains_binding_but_currently_unperformable, holdable).
narrative_ontology:cs_axiom_grounding('058369cc-1cb6-4106-8b5d-17d9a748d0bc', law_remains_binding_but_currently_unperformable, conventional).
narrative_ontology:cs_axiom('058369cc-1cb6-4106-8b5d-17d9a748d0bc', foundational, study_is_instrumental_not_intrinsically_efficacious).
narrative_ontology:cs_axiom_status(study_is_instrumental_not_intrinsically_efficacious, holdable).
narrative_ontology:cs_axiom_grounding('058369cc-1cb6-4106-8b5d-17d9a748d0bc', study_is_instrumental_not_intrinsically_efficacious, conventional).
narrative_ontology:cs_axiom('058369cc-1cb6-4106-8b5d-17d9a748d0bc', secondary, restoration_is_a_real_future_condition_not_merely_symbolic).
narrative_ontology:cs_axiom_status(restoration_is_a_real_future_condition_not_merely_symbolic, holdable).
narrative_ontology:cs_axiom_grounding('058369cc-1cb6-4106-8b5d-17d9a748d0bc', restoration_is_a_real_future_condition_not_merely_symbolic, theological).
narrative_ontology:cs_reference_frame('058369cc-1cb6-4106-8b5d-17d9a748d0bc', temple_era_sacrificial_practice).
narrative_ontology:cs_drift_state('058369cc-1cb6-4106-8b5d-17d9a748d0bc', contemporary_diaspora_scholarship, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('058369cc-1cb6-4106-8b5d-17d9a748d0bc', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, future_messianic_generation).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, rabbinic_academies).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, halakhic_continuity_project).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the technical obligation to study sacrificial law in exhaustive detail — a body of knowledge they cannot perform, will almost certainly never perform in their lifetimes, and which yields no observable ritual outcome. They bear the time and cognitive cost of mastering an inoperative legal system as an act of preparation for a restoration whose timing is unknown and unknowable. Leaving the obligation behind means abandoning a core marker of observant identity and communal standing.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Administer the curriculum that designates Kodashim study as binding preparatory obligation, train instructors, and certify mastery. They gain institutional continuity, scholarly prestige, and a rationale for maintaining specialized expertise across generations, regardless of whether restoration ever occurs. Their authority to define the obligation is itself unaffected by Temple absence.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_academies, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, rabbinic_academies, beneficiary).

% A projected future population that would benefit from an unbroken chain of technical sacrificial knowledge at the moment performance resumes. Not a present actor; named for completeness because the reading's coordination logic depends on this beneficiary existing eventually, not on any currently observable transfer to them.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, future_messianic_generation, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, future_messianic_generation).

% Hold that studying the laws itself enacts the sacrificial function, making Temple absence irrelevant to spiritual efficacy in the present. This reading treats study as instrumental waiting rather than intrinsically efficacious performance, so their view of what the obligation accomplishes right now is not represented within this reading's framework.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, study_as_performance_adherents, excluded,
    moderate, biographical, constrained, global).

% Hold that Kodashim documents a defunct system and that study is historical/identity preservation rather than binding law with cosmic stakes. This reading's claim that the obligation remains legally binding and technically necessary for a real future performance directly excludes their deflationary account of what the study is for.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, study_as_archive_adherents, excluded,
    moderate, biographical, mobile, global).

% Examine transmission history, manuscript variants, and the historical development of the obligation-to-study doctrine without being bound by it. They can trace how the preparatory framing emerged and shifted relative to Temple destruction, offering an outside vantage on whether the founding rationale still functions as claimed.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, textual_critics_and_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a complete, technically precise body of sacrificial law across generations who cannot practice it, so that if and when a Temple is rebuilt, priests and courts have an unbroken, letter-perfect record from which to resume performance without reconstruction or guesswork.
% TRANSFER_FUNCTION: Moves years of study time, cognitive effort, and communal resources from the current generation of practitioners toward the maintenance of technical knowledge whose payoff (if any) accrues to a future, currently nonexistent generation at the moment of messianic restoration.
% ABSENT_VOICES: Study-as-performance adherents, who would say the present-tense spiritual payoff is being undersold by treating the obligation as merely preparatory; study-as-archive adherents, who would say the binding-law framing overstates what a defunct system's study can obligate. Neither position is represented within this reading's own structure of obligation.
% DISAPPEARANCE_RATIONALE: Practitioners and academies dispute what would happen if the obligation vanished: academies argue the chain of technical transmission would break, foreclosing accurate restoration should the Temple ever be rebuilt; skeptical observers argue that since restoration has not occurred in two millennia and shows no operational trajectory toward occurring, removing the study obligation would change devotional practice and curricula but would not alter any world-state that depends on functioning sacrificial infrastructure.
% FOUNDING_PROBLEM: After the Second Temple's destruction, the sacrificial system could no longer be performed, but rabbinic authorities held it was not permanently abolished — only suspended pending restoration. The founding problem was to prevent irrecoverable loss of the technical knowledge required to resume performance correctly whenever restoration became possible.
% FOUNDING_PROBLEM_CORROBORATION: Academies and their students attest the problem is live: restoration remains theologically expected and the knowledge must not lapse. Historians and comparative religion scholars outside the tradition's institutional structure note the destruction is now roughly two thousand years past with no infrastructural trajectory toward a rebuilt Temple, and argue the 'preparation' framing has functioned continuously as an identity and continuity mechanism independent of any approaching restoration — a reading the academies do not corroborate.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because, under this reading's own premises, the cost imposed on current practitioners is preparatory investment toward a real future benefit, not rent extraction with no coordination function — the coordination function (preserving technically exact knowledge against transmission loss) is genuine within the reading's frame. Suppression is moderate-low (0.28): the obligation is enforced through communal and institutional norms (curriculum requirements, communal expectation) rather than coercive apparatus, but exit carries real identity cost. Theater ratio is low (0.15) because within this reading the study is understood as substantively purposeful — not empty performance — though it rises slowly over the interval as restoration recedes further from historical plausibility without any change in study intensity or institutional urgency, which is itself worth tracking. Accessibility collapse is moderate (0.4): alternative readings (performance, archive) remain visible and held by others, so the preparation reading has not fully foreclosed its own alternatives even for insiders, unlike a genuine mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the academy's administering seat, the arrangement reads as genuine, well-functioning coordination: knowledge preservation against an uncertain but hoped-for future need. From the payer seat — practitioners investing biographical time against a civilizational-scale, non-guaranteed payoff — the same structure can read as extraction dressed as piety, particularly as the interval since destruction lengthens without any narrowing of the gap to restoration. The engine computes these divergent seat classifications from the structural power/exit/time_horizon data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Current-generation practitioners are declared victims because the reading assigns them the full cost (years of study) with the benefit deferred to a beneficiary that does not yet exist in real time — this is a structurally unusual directionality pattern where the payer class and beneficiary class cannot coexist in the same historical moment. Rabbinic academies are dual-positioned: they set and administer the obligation (agenda_setter) while also gaining institutional continuity and prestige from perpetuating it (beneficiary), independent of whether restoration ever occurs — this is the seam most likely to generate seat divergence from the payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification requires a sunset clause and a coordination-generating beneficiary; both are present in this reading by construction (restoration ends the scaffold; the beneficiary is the restored priesthood/populace). The unresolved question the mandatrophy apparatus should surface is duration: a scaffold whose sunset condition has not obtained in nineteen-plus centuries starts to resemble a piton wearing scaffold's justification. This reading does not resolve that question — it is deliberately left to the founding_problem_status field (authored 'contested') and to omega_restoration_horizon below, rather than smuggled into the metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_horizon_indefiniteness,
    'Does the indefinite deferral of the sunset condition (Temple restoration) convert this scaffold into a de facto piton — a scaffold whose declared transitional justification has, in practice, no forecast horizon?',
    'Compare institutional statements across centuries for any operative claim of proximate restoration versus purely eschatological, non-time-bound language; a consistent shift toward purely eschatological framing over the interval would support piton reclassification.',
    'If restoration is treated as structurally indefinite rather than merely uncertain, the coordination-preparation framing weakens and the arrangement more closely resembles inertial persistence maintained by institutional continuity rather than genuine transitional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_horizon_indefiniteness, conceptual, 'Whether an indefinitely deferred sunset condition still counts as a sunset clause for scaffold purposes.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is ''the obligation to study Kodashim'' genuinely one kernel with three defensible readings (preparation, performance, archive), or does the halakhic tradition itself adjudicate among them such that only one reading is doctrinally live at a given point in history?',
    'Survey normative rulings (poskim) across eras for explicit endorsement or rejection of competing rationales for the study obligation; convergence on one rationale in mainstream halakhic literature would indicate the kernel is less contested than the three-reading framing suggests.',
    'If one reading is doctrinally dominant, the other two readings should be treated as minority or historically superseded positions rather than co-equal live readings, which would affect how the reading_relations edges should be weighted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the three-reading kernel decomposition reflects genuine live contestation or an analytical overlay on a more settled tradition.').

omega_variable(
    beneficiary_nonexistence_problem,
    'Can a beneficiary that does not yet exist (the future messianic generation) meaningfully ground a coordination function in the present, or does treating a non-actual population as the beneficiary structurally convert what looks like coordination into a claim that cannot be falsified or checked by anyone alive?',
    'Philosophical/legal analysis of analogous intergenerational obligation structures (e.g., environmental stewardship for future generations) to determine whether non-existence of the beneficiary at authorship time is a recognized valid ground for present obligation elsewhere in normative reasoning.',
    'If non-existent-beneficiary grounding is accepted as valid elsewhere, the preparation reading''s coordination claim is on solid structural footing; if not, the reading''s low extractiveness score may be too generous, since the claimed beneficiary transfer can never be verified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_nonexistence_problem, conceptual, 'Whether an obligation grounded in a currently nonexistent beneficiary can support a genuine coordination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t300, kodashim_obligation__study_as_preparation, theater_ratio, 300, 0.07).
narrative_ontology:measurement_basis(koda_tr_t300, observed).
narrative_ontology:measurement(koda_tr_t700, kodashim_obligation__study_as_preparation, theater_ratio, 700, 0.09).
narrative_ontology:measurement_basis(koda_tr_t700, observed).
narrative_ontology:measurement(koda_tr_t1100, kodashim_obligation__study_as_preparation, theater_ratio, 1100, 0.11).
narrative_ontology:measurement_basis(koda_tr_t1100, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.13).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t1950, kodashim_obligation__study_as_preparation, theater_ratio, 1950, 0.15).
narrative_ontology:measurement_basis(koda_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t300, kodashim_obligation__study_as_preparation, base_extractiveness, 300, 0.14).
narrative_ontology:measurement_basis(koda_be_t300, observed).
narrative_ontology:measurement(koda_be_t700, kodashim_obligation__study_as_preparation, base_extractiveness, 700, 0.17).
narrative_ontology:measurement_basis(koda_be_t700, observed).
narrative_ontology:measurement(koda_be_t1100, kodashim_obligation__study_as_preparation, base_extractiveness, 1100, 0.19).
narrative_ontology:measurement_basis(koda_be_t1100, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.21).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t1950, kodashim_obligation__study_as_preparation, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement_basis(koda_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_preparation, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t300, kodashim_obligation__study_as_preparation, suppression_requirement, 300, 0.22).
narrative_ontology:measurement_basis(koda_su_t300, observed).
narrative_ontology:measurement(koda_su_t700, kodashim_obligation__study_as_preparation, suppression_requirement, 700, 0.24).
narrative_ontology:measurement_basis(koda_su_t700, observed).
narrative_ontology:measurement(koda_su_t1100, kodashim_obligation__study_as_preparation, suppression_requirement, 1100, 0.26).
narrative_ontology:measurement_basis(koda_su_t1100, observed).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.27).
narrative_ontology:measurement_basis(koda_su_t1500, observed).
narrative_ontology:measurement(koda_su_t1950, kodashim_obligation__study_as_preparation, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement_basis(koda_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_preparation, 0.08).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the obligation to study Kodashim' per the ε-invariance principle. study_as_performance treats study as intrinsically efficacious (no deferred beneficiary, no victim class generated by deferral — expected low ε, likely rope). study_as_archive treats the material as historical/identity content with no live legal force (expected minimal ε, likely rope or piton). This story (study_as_preparation) alone generates a temporally split beneficiary/victim structure because it alone treats the law as binding-but-suspended pending a real future performance. All three share the same underlying text corpus (fixed_text kernel) but instantiate structurally distinct constraints with distinct ε values; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
