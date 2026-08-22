% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Study as Mitzvah Performance (Study-as-Exercise Reading)
 *   domain: religious/rabbinic_judaism/commitment_system
 *
 * SUMMARY:
 *   In rabbinic Judaism after the Second Temple's destruction in 70 CE,
 *   physical sacrifice became impossible but remained a Torah obligation. The
 *   Jewish learning tradition developed an interpretation in which study of
 *   the Kodashim (the Mishnaic tractates on sacrifice law and Temple service)
 *   itself constitutes the performance of the mitzvah (commandment). The
 *   scholar or student engaging in rigorous, continuous study of these texts
 *   is understood to fulfill the obligation, maintain the covenant, and
 *   participate in cosmic order through intellectual-spiritual engagement.
 *   This is one reading of the contested kernel—the Kodashim corpus—that
 *   governs what 'occupying' the sacrificial obligation looks like in the
 *   post-Temple era. Competing readings (performance_only: study defers to
 *   messianic restoration; substitution_archive: prayer replaced sacrifice,
 *   study is memorial) offer different instantiations of the same kernel.
 *   This story generates ONLY the study-as-exercise reading as a clean
 *   constraint: zero extractiveness, rope-type coordination around shared
 *   interpretive practice, zero suppression, beneficiaries are engaged
 *   scholars and learning communities maintaining the tradition. The other
 *   readings are SIBLING CONSTRAINTS, not alternatives within this story.
 *
 * KEY AGENTS:
 *   - engaged_scholars: Talmudic students and scholars engaged in the rigorous study of Kodashim; they are the primary agents performing the mitzvah through intellectual engagement
 *   - jewish_learning_communities: Yeshivas, study circles, and educational institutions that organize the curriculum and transmit the tradition
 *   - post_temple_jewish_communities: The broader Jewish collective for whom this reading provides continuity with the covenant in the Temple's absence
 *   - textual_tradition: The Talmudic and rabbinic corpus on sacrifice (not an agent, but the analytical referent)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.0).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.0).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Study as Mitzvah Performance (Study-as-Exercise Reading)").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/rabbinic_judaism/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '7985dc20-cd8d-430d-a3f9-90e0391a3efe').
narrative_ontology:cs_kernel_codification('7985dc20-cd8d-430d-a3f9-90e0391a3efe', fixed_text).
narrative_ontology:cs_authority_grounding('7985dc20-cd8d-430d-a3f9-90e0391a3efe', lineage).
narrative_ontology:cs_interpretation_layer_present('7985dc20-cd8d-430d-a3f9-90e0391a3efe').
narrative_ontology:cs_reading_relation('7985dc20-cd8d-430d-a3f9-90e0391a3efe', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('7985dc20-cd8d-430d-a3f9-90e0391a3efe', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('7985dc20-cd8d-430d-a3f9-90e0391a3efe', foundational, study_performance_equivalence).
narrative_ontology:cs_axiom_status(study_performance_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('7985dc20-cd8d-430d-a3f9-90e0391a3efe', study_performance_equivalence, deontological).
narrative_ontology:cs_axiom('7985dc20-cd8d-430d-a3f9-90e0391a3efe', foundational, continuous_kernel_occupation).
narrative_ontology:cs_axiom_status(continuous_kernel_occupation, holdable).
narrative_ontology:cs_axiom_grounding('7985dc20-cd8d-430d-a3f9-90e0391a3efe', continuous_kernel_occupation, deontological).
narrative_ontology:cs_reference_frame('7985dc20-cd8d-430d-a3f9-90e0391a3efe', post_temple_study_occupation).
narrative_ontology:cs_drift_state('7985dc20-cd8d-430d-a3f9-90e0391a3efe', contemporary_pluralist_judaism, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('7985dc20-cd8d-430d-a3f9-90e0391a3efe', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, engaged_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_learning_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, post_temple_jewish_communities).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_study_as_spiritual_practice).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, intellectual_engagement_cosmic_participation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish scholars and students engage in intensive study of the Kodashim (laws of sacrifice). Through this study they perform the mitzvah (commandment) itself; the intellectual-spiritual engagement IS the fulfillment, not a substitute for something else. They constitute the community maintaining the covenant through continuous interpretive engagement with the sacrificial system's underlying principles and meanings.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, engaged_scholars, beneficiary,
    moderate, civilizational, mobile, global).

% Yeshivas, educational institutions, and study circles organize around the continuous engagement with Kodashim. The community coordinates itself through shared interpretive practice; study sessions are both individual spiritual performance and collective coordination of the learning tradition. The reading holds that this ongoing engagement sustains the cosmic order in the absence of the physical Temple.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, jewish_learning_communities, beneficiary,
    organized, civilizational, mobile, global).

% The Talmudic and later rabbinic corpus on sacrifice laws is the kernel being continuously occupied through study. The tradition is not a monument or museum piece but a living engagement site where meaning emerges through each generation's interpretive work.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, textual_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_corpus__study_as_exercise, textual_tradition).

% The broader Jewish community that can no longer perform physical sacrifice at the Temple finds its spiritual participation in the sacrificial system through engagement with the Kodashim curriculum. Study provides continuity with the covenant and the Temple's function, even in its physical absence.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, post_temple_jewish_communities, beneficiary,
    organized, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes continuous interpretive engagement with the sacrificial corpus as collective spiritual practice. The reading coordinates scholars and communities around shared study, establishing a common referent (the Kodashim texts) and a shared understanding that this engagement constitutes the performance of the mitzvah itself, binding each generation into the tradition of cosmic maintenance through intellectual participation.
% TRANSFER_FUNCTION: Transfers authority and interpretive responsibility from generation to generation of scholars. Each generation receives the textual tradition and the understanding that their study fulfills the commandment; in return, they commit to sustained, rigorous engagement and pass the living tradition forward to the next cohort.
% ABSENT_VOICES: This reading marginalizes or excludes the 'performance_only' reading (those who hold that study is deferral, not fulfillment, and that the kernel awaits messianic restoration when physical sacrifice can resume). It also competes with the 'substitution_archive' reading (those who hold that prayer replaced sacrifice and Kodashim is memorial, not occupied).
% DISAPPEARANCE_RATIONALE: If this reading's institutional and spiritual infrastructure disappeared—if the commitment that 'study of Kodashim IS the performance' evaporated—Jewish communities would lose a primary channel through which they understand themselves to maintain cosmic order in the Temple's absence. The post-Temple Jewish identity that has structured itself around this engagement would require wholesale reconstitution; either the 'performance_only' reading (deferral to messianic times) or the 'substitution_archive' reading (prayer replaced sacrifice; study is memorial) would have to fill the void, and communities would rearrange their spiritual practice and curricula accordingly.
% FOUNDING_PROBLEM: After the Second Temple's destruction, the Jewish people could no longer perform physical sacrifice but remained bound by the covenant that included sacrificial obligations. The commandment to engage in sacrifice could not be abandoned but could not be executed in its literal form. What form does covenantal participation take? How do communities maintain their relationship to the sacrificial corpus and to God?
% FOUNDING_PROBLEM_CORROBORATION: This interpretation is attested by centuries of rabbinic tradition and contemporary Jewish learning communities. Talmudic statements treating study of sacrifice law as equivalent to performance (e.g., Talmud Bavli, Menachot 110a: 'Whoever studies the laws of the burnt offering is credited with having offered the burnt offering') ground this reading in authoritative textual sources. Living yeshiva communities, organized educational curricula in mainstream Jewish life, and contemporary Jewish learning materials all treat the study-as-performance understanding as normative. Academic scholars of Rabbinic Judaism (e.g., Shamma Friedman, Yaakov Elman) document this as the dominant rabbinic hermeneutic stance.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the reading holds that study IS the complete fulfillment—there is no remainder, no uncollected value, no asymmetric capture. The scholar who studies performs the mitzvah fully; no one is deprived. Suppression is zero: participation in study is voluntary, alternative learning paths exist (prayer, other commandments, other textual study), and no coercive apparatus enforces Kodashim study specifically. Theater is zero: the functional content of study-as-performance is the intellectual engagement itself, not performance of performance. Accessibility_collapse is very high (0.95) because within this reading's frame, the logic is internally coherent and alternatives (performance_only, substitution_archive) collapse when one commits to this interpretation—the reading occupies the kernel fully from its own vantage point. Resistance is very low (0.05) because there is no extraction to resist against; such resistance as exists comes from those who hold competing readings (performance_only advocates who argue deferral is the true posture, substitution_archive advocates who argue study is memorial). The claim/metric alignment is deliberate: the constraint is CLAIMED as rope (coordination around shared interpretive practice) and the metrics describe a pure coordination structure with no extraction, no suppression, no theater—full alignment because the reading's structural claim is that there is nothing but the coordination itself.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces NO perspectival divergence between seats because all seats in the constraint are beneficiaries or observers. A performance_only adherent (who believes study defers to messianic restoration) would experience this constraint differently—they would see extractiveness and suppression in the performance-language claiming equivalence when deferral is supposedly the truth. But performance_only adherents are EXCLUDED from this reading's stakeholder set (they are an excluded voice, named in six_questions.absent_voices). From within this reading, every seat sees coordination, not extraction. The engine will compute the same type for every seat because the structural data supports it uniformly.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are beneficiaries in this reading's frame: scholars benefit (they perform the mitzvah, maintain cosmic order, participate in the covenant), learning communities benefit (they sustain themselves through transmission and engagement), post-Temple communities benefit (they maintain continuity with the sacrificial covenant). There are no victims because no one bears costs asymmetrically; the 'cost' of study is the voluntary commitment to engagement, which the beneficiary reading frames as itself the benefit. The textual_tradition is an observer/referent, not an agent. Directionality derivation: all beneficiaries sit at d ≈ 0.0 (full beneficiary, no extraction). No overrides are needed because the structural data (beneficiaries only, no victims, zero extractiveness, zero suppression) produces the right d values automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly avoids mandatrophy by rejecting the premise that generates it: if study were deferral (performance_only reading), the founding problem (fulfilling the sacrificial mitzvah post-Temple) would be dead (can't be fulfilled until the Temple is rebuilt), but the arrangement (study curricula) would persist, triggering mandatrophy reclassification to piton. But this reading denies that premise: study IS fulfillment, so the founding problem is live (continuously lived in each act of study), the arrangement persists as the solution, and mandatrophy does not trigger. The reading's internal coherence depends on rejecting performance_only's framing; that rejection is where the mandatrophy avoidance lives. If this reading is challenged and performance_only is shown to be the actual community understanding, mandatrophy would activate and reclassify the constraint as piton (performance-language maintaining an archived structure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_occupation_status,
    'Is the Kodashim corpus a ''live'' kernel continuously occupied through study-as-performance, or is it an archived blueprint awaiting future physical restoration? Does study genuinely fulfill the mitzvah or deferentially hold the space until messianic times?',
    'Cross-reading comparative analysis: (1) textual evidence from rabbinic sources treating study equivalently to physical performance vs. sources treating physical performance as the only true form; (2) institutional behavior: do communities prioritize Kodashim study as primary spiritual practice (supporting occupation) or as secondary/deferential (supporting archive)? (3) phenomenological interview with scholars and community leaders about their subjective understanding of what study accomplishes.',
    'If the kernel is ''truly occupied'' (study is genuine performance), this reading''s type stabilizes as rope and extractiveness stays near zero. If the kernel is ''deferred occupation'' (study is placeholder), the constraint type would drift toward piton (theater-maintained, performative, waiting for true performance) and omegas around performance_only would activate. If the kernel is ''archived'' (prayer replaced sacrifice, study is memorial), this reading''s entire legitimacy frame collapses and substitution_archive would become the structurally accurate reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_occupation_status, conceptual, 'Whether this reading instantiates genuine kernel occupation or deferential placeholder performance.').

omega_variable(
    committer_frame_stability,
    'Does this reading instantiate a stable, lived commitment of Jewish communities to study-as-performance, or does it represent an idealization that competes against lived ambiguity where multiple readings coexist without clear winner?',
    'Ethnographic study of actual study practices: (1) Do practitioners (scholars, yeshiva students, community members) describe their Kodashim study in performance language or in deferential/memorial language? (2) Do institutional curricula treat Kodashim as equivalent to other mitzvot or as a special category? (3) Do practices vary by community, movement (Orthodox, Conservative, Reconstructionist), or geography? (4) Historical narrative: has the study-as-performance reading been the dominant rabbinic interpretation across all periods, or has it competing with the other readings across history?',
    'If study-as-performance is the stable, historically dominant reading held by the majority of communities, this story''s instantiation is well-grounded. If the reading is contested and communities navigate it ambiguously (some treating it as performance, others as deferral, still others as memorial), the constraint''s type would shift toward tangled_rope (coordination + hidden extraction of deferral ideology) or snare (performance-language masking memorial reality). The cs_structure.reference_frame and drift_state would need recalibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_stability, empirical, 'Whether study-as-performance is a stable, lived community commitment or an idealization competing against ambiguity.').

omega_variable(
    reading_foreclosure_necessity,
    'Do the axioms of this reading logically foreclose the performance_only or substitution_archive readings in any single framework, or do they coexist as live, non-contradictory positions that different parties hold?',
    'Logical analysis: Does the claim ''study IS performance'' entail that ''study is NOT performance'' is false? Or can a coherent framework hold (a) study IS performance in post-Temple times AND (b) physical sacrifice WOULD BE the true form if the Temple existed? Can a framework hold that prayer/study replaced sacrifice AND that the study itself constitutes a new form of occupying the kernel? If yes to any, readings coexist; if no, foreclosure is real.',
    'If readings do NOT logically foreclose each other, this story''s cs_structure.reading_relations should use ''coexists_with'' for both siblings; if they do foreclose, use ''forecloses''. This affects the narrative and the omega documentation of the committer frame''s logical coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_necessity, conceptual, 'Whether the study-as-exercise reading logically forecloses sibling readings or coexists with them.').

omega_variable(
    performance_language_capture,
    'Is the deployment of ''performance'' language (study IS the performance of the mitzvah) a genuine reframing of what fulfillment looks like, or does it risk masking a substitution as a continuation—using performance rhetoric to paper over the fact that physical sacrifice has been replaced?',
    'Rhetorical and historical analysis: (1) How does rabbinic literature explicitly handle the transition from physical sacrifice to study-based engagement? Does it frame study as genuinely equivalent or as ''as if'' equivalent? (2) Do Jewish legal and mystical texts treat study-performance as generating the same cosmic effects as physical sacrifice? (3) Contemporary philosophical examination: is there a coherent theory of how intellectual engagement accomplishes what physical sacrifice accomplishes, or is ''performance'' language doing ideological work to make replacement palatable?',
    'If performance language is genuine (study generates equivalent cosmic effects by design), this reading''s extractiveness stays at 0.0. If performance language is rhetorical capture masking substitution, there is a hidden extraction dynamic where the substitution_archive reading''s view (prayer replaced sacrifice) is true but obscured by performance framing. This would reframe the constraint as tangled_rope (apparent coordination, hidden substitution) or snare (performance-language concealing victim status of those who believe study truly fulfills vs. those who recognize substitution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_language_capture, conceptual, 'Whether performance-language represents genuine reframing or rhetorical masking of substitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__study_as_exercise, 0.05).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The kodashim_corpus kernel is instantiated via three structurally distinct readings. Each reading produces a different constraint (different ε, different beneficiary/victim structure, different type). The study-as-exercise reading (this story) asserts zero extractiveness and pure coordination (rope). The performance_only reading asserts that study is deferral (likely piton or scaffold with sunset). The substitution_archive reading asserts that study is memorial (likely piton or tangled_rope depending on whether prayer-replacement is extraction). All three share the same kernel but derive different constraints from different readings of what 'occupying' the kernel entails. Links via network.affects_constraints form the constraint family and enable cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
