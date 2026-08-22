% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Substitution Archive: Prayer and Study Replace Sacrifice
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   The Kodashim (sacrificial sections of the Mishnah and Talmud) preserve
 *   detailed laws and theology of Temple sacrifice despite its abolition
 *   after 70 CE. Rabbinic Judaism framed the textual preservation as a
 *   memorial and intellectual continuation—prayer and study substituted for
 *   physical sacrifice. This reading claims the Kodashim functions as an
 *   archive documenting what was superseded, NOT as an occupied kernel.
 *   Sacrificial restoration movements (Karaites, certain mystics, some
 *   contemporary groups) read the same corpus as a blueprint for future
 *   restoration, arguing the constraint is not simply memorial but rather a
 *   temporarily suspended practice awaiting messianic return. This story
 *   instantiates the 'substitution archive' reading: the constraint operates
 *   by claiming continuity with sacrifice while institutionally denying its
 *   restoration possibility. Extractiveness is moderate-to-high because the
 *   substitution obscures a deliberate replacement, imposing a single
 *   interpretive frame on those who might otherwise seek or advocate for
 *   living sacrifice.
 *
 * KEY AGENTS:
 *   - Rabbinic text-study institutions: agenda-setters, set the frame that Kodashim study substitutes for sacrifice and is the sole legitimate engagement with sacrificial tradition.
 *   - Sacrificial restoration seekers: victims, locked into the textual tradition by identity but excluded from mainstream interpretive authority; told their readings are heretical or premature.
 *   - General Jewish practitioners: beneficiaries and diffuse payers; gain coherence through substitution but forfeit embodied and experiential relationship to sacrifice law.
 *   - Competing interpretive movements (Karaite, mystical, heterodox): excluded; their readings of the archive as blueprint are structurally delegitimized.
 *   - Historical scholarship: observer seat, documents the deliberate theological move and its costs to those foreclosed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.68).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.72).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.68).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Substitution Archive: Prayer and Study Replace Sacrifice").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, 'db4b6fde-91f8-4864-a627-787b9cbb3ad4').
narrative_ontology:cs_kernel_codification('db4b6fde-91f8-4864-a627-787b9cbb3ad4', fixed_text).
narrative_ontology:cs_authority_grounding('db4b6fde-91f8-4864-a627-787b9cbb3ad4', lineage).
narrative_ontology:cs_interpretation_layer_present('db4b6fde-91f8-4864-a627-787b9cbb3ad4').
narrative_ontology:cs_reading_relation('db4b6fde-91f8-4864-a627-787b9cbb3ad4', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('db4b6fde-91f8-4864-a627-787b9cbb3ad4', kodashim_corpus__study_as_exercise, influences).
narrative_ontology:cs_axiom('db4b6fde-91f8-4864-a627-787b9cbb3ad4', foundational, sacrifice_permanently_superseded).
narrative_ontology:cs_axiom_status(sacrifice_permanently_superseded, holdable).
narrative_ontology:cs_axiom_grounding('db4b6fde-91f8-4864-a627-787b9cbb3ad4', sacrifice_permanently_superseded, deontological).
narrative_ontology:cs_axiom('db4b6fde-91f8-4864-a627-787b9cbb3ad4', foundational, study_substitutes_not_preserves).
narrative_ontology:cs_axiom_status(study_substitutes_not_preserves, holdable).
narrative_ontology:cs_axiom_grounding('db4b6fde-91f8-4864-a627-787b9cbb3ad4', study_substitutes_not_preserves, conventional).
narrative_ontology:cs_reference_frame('db4b6fde-91f8-4864-a627-787b9cbb3ad4', post_destruction_substitution_settlement).
narrative_ontology:cs_drift_state('db4b6fde-91f8-4864-a627-787b9cbb3ad4', contemporary_institutional_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('db4b6fde-91f8-4864-a627-787b9cbb3ad4', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, sacrificial_restoration_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, general_jewish_practitioners).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, general_jewish_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets the Kodashim (sacrificial) corpus as authoritative textual archive. Claims the detailed preservation and continuous study of sacrifice law serves redemptive and educational purposes while the actual sacrificial practice is obsolete. Controls what counts as legitimate engagement with the sacrificial tradition: study replaces ritual. Derives institutional authority and continuity from possessing the sole legitimate interpretive tradition.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Seek to restore physical sacrifice as a living practice, interpreting the textual archive as a blueprint awaiting messianic actualization. Face institutional denial that restoration is legitimate: told their reading is heretical, premature, or anti-halakhic. Remain bound to the textual tradition (cannot leave Judaism without leaving their identity) but are locked out of the primary interpretive authority. Pay the cost of institutional marginalization and exclusion from mainstream religious authority.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, sacrificial_restoration_seekers, payer,
    moderate, civilizational, identity_locked, global).

% Benefit from an intellectually coherent substitute for sacrifice: prayer, study, and ethical action. Accept the institutional framing that sacrifice is obsolete. Also bear a diffuse cost: the arrangement forecloses experiential, embodied engagement with sacrifice law and locks the tradition into one interpretive frame, constraining theological innovation within Jewish practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, general_jewish_practitioners, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, general_jewish_practitioners, payer).

% The abstract proposition that rabbinic Judaism maintains continuity with the Second Temple sacrificial tradition through textual preservation and intellectual engagement. This is not an actor but a doctrine whose vindication depends on institutional enforcement of the substitution narrative.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, historical_continuity_claim, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_corpus__substitution_archive, historical_continuity_claim).

% Karaite, mystical, and heterodox readings that interpret the sacrificial archive differently (as requiring restoration, as mystical symbol, as performance through study without denying restoration possibility) are structurally excluded from mainstream interpretive authority. Their exclusion is maintained by the same institutional enforcement machinery that sustains the substitution narrative.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, competing_interpretive_movements, excluded,
    powerful, civilizational, trapped, global).

% Documents the historical record: that sacrifice was abolished (by Roman destruction and rabbinic choice), that substitution was a deliberate theological move, and that the substitution narrative itself became enforced doctrine. Takes no position on theological truth but notes the structural dynamics of replacement and the cost borne by those who dispute it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, historical_scholarship, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits the detailed law and theology of sacrifice despite its abolition, maintaining Jewish textual civilization and continuity with pre-Rabbinic tradition through continuous intellectual engagement. Converts practice into study, embodied ritual into hermeneutical discipline.
% TRANSFER_FUNCTION: Moves interpretive authority from would-be practitioners seeking restoration to institutionalized text-study centers. Those seeking to restore sacrifice are told their desire is illegitimate; those accepting the substitution gain membership in the mainstream religious community. The constraint transfers permission to interpret from competing movements to rabbinic institutional hierarchy.
% ABSENT_VOICES: Sacrificial restoration movements, Karaite interpreters, and mystical readings that hold the archive as a blueprint for future restoration are structurally excluded from primary interpretive authority. They would argue for openness to multiple readings and preservation of the restoration possibility; their exclusion is enforced by the same framing that sustains the substitution narrative.
% DISAPPEARANCE_RATIONALE: If the constraint vanished and the Kodashim were reframed as open to multiple interpretations (blueprint vs. archive vs. intellectual exercise), the movement seeking sacrificial restoration would immediately claim institutional legitimacy and resources. The arrangement's disappearance would reorganize the field of permissible Jewish theology and practice.
% FOUNDING_PROBLEM: After the destruction of the Second Temple and cessation of sacrificial practice by Roman occupation and rabbinic decision, how could a Torah-centered Judaism preserve its textual heritage and claim continuity with the sacrificial tradition while accommodating the practical impossibility of sacrifice?
% FOUNDING_PROBLEM_CORROBORATION: Mainstream rabbinic sources and historical scholarship agree that sacrifice was abolished and substitution narratives were developed as a theological response. Sacrificial restoration movements and heterodox interpreters contest whether the founding problem was 'solved' or merely 'covered'—they argue the textual archive remains a charter for future restoration, not a closed memorial. Historical scholars outside the rabbinic authority structure document both the deliberate theological move and the cost to those whose interpretations were foreclosed.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the degree to which the constraint extracts from those who might want to practice sacrifice or hold alternative interpretations. At the reading's foundation (t=0, post-destruction), the substitution was a creative theological response to a practical impossibility; extractiveness was lower because the constraint solved a genuine coordination problem (how to preserve a textual civilization after the Temple was destroyed). Over 2000 years, extractiveness rose as the substitution hardened into enforced doctrine: by late medieval and modern periods, alternative readings are increasingly marginalized, and the extraction consists of denying interpretive legitimacy to those who hold the archive as a blueprint. Theater ratio rises over the same interval: the performative function (studying Kodashim as 'performing' the mitzvah or maintaining messianic hope) grows while the stated function (memorial, intellectual engagement) becomes more emphasized in institutional rhetoric. Suppression_requirement climbs because maintaining the single-reading frame requires actively excluding competing interpretations and those who hold them. The measurements reflect the transition from substitution-as-solution to substitution-as-enforcement: the constraint begins as a genuine alternative to a lost practice and evolves into a mechanism for crystallizing one reading and excluding others.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic institutional seat, the Kodashim corpus represents a living engagement with sacrificial law through study—continuity, not replacement. From the sacrificial restoration seeker's seat, the same corpus is a blueprint for future practice that is being institutionally buried under denial and marginalization. The engine computes these seats differently because they hold opposing roles (agenda-setter vs. victim-payer) and different exit structures (arbitrage vs. identity_locked). The payer seat experiences the constraint as suppressive and exclusionary; the agenda-setter seat experiences it as coordination and continuity. This divergence is exactly what the directed metrics capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic institutions are the structural beneficiary (d near 0.0): they control interpretive authority, define legitimate engagement, and institutionalize a narrative that vindicates their authority. Sacrificial restoration seekers are the structural target (d near 1.0): they are locked into the tradition by identity (cannot leave Judaism without leaving themselves), experience suppression (told their readings are illegitimate), have minimal exit (constrained), and bear the cost of institutional marginalization. General practitioners sit at moderate directionality (~0.5): they benefit from the coherence and membership the substitution provides but pay a diffuse cost in foreclosed theological possibility and embodied practice. The identity_lock on sacrificial seekers is the critical constraint-specific datum: they cannot arbitrage away from the tradition (unlike a constrained actor who can leave the institutional field entirely). This locks them into bearing the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure extraction (snare) or pure coordination (rope) by the tangled_rope frame: it does solve a genuine coordination problem (how to preserve textual civilization after sacrifice ends) AND it asymmetrically extracts (from those who might hold alternative readings). The constraint is tangled because the same mechanism—institutionalizing the substitution narrative—does both work (preserves the tradition) and extraction work (forecloses restoration readings). The theater_ratio measurement is critical here: as it climbs above 0.5, the performative function (studying Kodashim as 'performing' the mitzvah) grows relative to the stated functional claim (memorial, intellectual continuity). This suggests the constraint is beginning to operate more as theater (maintaining the appearance of living engagement) than as genuine coordination. Were theater to reach 0.7+, the constraint might reclassify toward piton (an atrophied function maintained theatrically); currently at 0.55 it remains tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_status_ambiguity,
    'Is the Kodashim corpus genuinely a ''memorial archive'' (as substitution_archive claims) or is it a ''blueprint awaiting restoration'' (as performance_only claims) or an ''occupied kernel through study'' (as study_as_exercise claims)?',
    'Historical and textual analysis of institutional rhetoric across periods: does rabbinic literature frame the Kodashim as permanently replaced, temporarily suspended, or continuously performed through study? Comparative analysis of how institutions handle restoration movements: are they delegitimized or engaged with on intellectual grounds?',
    'If the kernel is genuinely memorialized, the constraint is primarily a coordination mechanism with moderate extraction (tangled_rope). If it is genuinely a blueprint, the constraint operates primarily as suppression of legitimate interpretations (snare). If it is genuinely occupied through study, the constraint is nearly pure coordination (rope with high theater ratio).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_status_ambiguity, conceptual, 'Ambiguity about what interpretive status the Kodashim corpus has in the rabbinic framework itself—memorial, blueprint, or occupied kernel.').

omega_variable(
    suppression_internalization,
    'Is the suppression of restoration seekers primarily structural (institutional delegitimization) or has it become partially internalized (seekers internalize the narrative that restoration is illegitimate)?',
    'Post-institutional suppression trajectory: if restoration seekers who leave the rabbinic institutional framework continue to experience the suppression or reject the restoration possibility, the suppression is internalized; if they immediately embrace restoration possibility, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and the extraction persists even after institutional enforcement weakens. If structural, suppression would decay if institutional enforcement were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of restoration seekers is maintained structurally through institutional authority or partially through internalized beliefs.').

omega_variable(
    theater_ratio_trajectory,
    'Is the rising theater_ratio over the interval a sign of the constraint becoming a piton (atrophied function maintained theatrically), or is performative engagement itself the genuine function of the constraint?',
    'Ethnographic study of institutional practices: does time spent on Kodashim study correlate with other institutional goals (membership maintenance, professional authority, theological coherence) or with actual engagement with the sacrificial law itself? Do institutions invest in deepening understanding of Kodashim practice or in maintaining its cultural prestige?',
    'If theater is rising because the coordination function is decaying, the constraint may be transitioning toward piton. If theater is rising because performative engagement is the genuine function (maintaining connection to the past, intellectual discipline, identity maintenance), the constraint remains tangled_rope with valid coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_trajectory, empirical, 'Whether the rising theater ratio indicates atrophy of genuine function or represents the actual function of the constraint.').

omega_variable(
    founding_problem_status_ambiguity,
    'Was the founding problem (preserving Jewish civilization after Temple destruction) genuinely solved by the substitution, or merely deferred?',
    'Comparison of how the constraint operates to solve the original problem (coordination of a textless civilization) versus its current function (maintenance of institutional authority and exclusion of competing interpretations). If current extractiveness is primarily for institutional authority rather than civilization preservation, the founding problem may be dead.',
    'If the founding problem is dead but the constraint persists, this is mandatrophy: a constraint maintained by inertia and institutional capture rather than functional necessity. This would suggest reclassification toward piton or prolonged tangled_rope with mandatory review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_ambiguity, conceptual, 'Whether the founding problem (preserving civilization after Temple loss) is still live or has been superseded by institutional preservation.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the axioms of substitution_archive logically foreclose the axioms of performance_only and study_as_exercise, or do these readings genuinely coexist as live options within Jewish theology?',
    'Doctrinal analysis: can a mainstream rabbinic authority simultaneously teach that the Kodashim is (1) a memorial archive (substitution claim), (2) awaiting restoration (performance_only claim), and (3) occupied through study (study_as_exercise claim) without internal contradiction? Or are these mutually exclusive commitments?',
    'If genuinely foreclosed: reading_relations use forecloses. If genuinely coexisting as live options: reading_relations use coexists_with. If one reading creates structural pressure on others but does not foreclose: reading_relations use influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between the axioms of the three readings of the kodashim_corpus kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.35).
narrative_ontology:measurement(koda_tr_t250, kodashim_corpus__substitution_archive, theater_ratio, 250, 0.4).
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__substitution_archive, theater_ratio, 500, 0.45).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.52).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.54).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__substitution_archive, theater_ratio, 2000, 0.55).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(koda_be_t250, kodashim_corpus__substitution_archive, base_extractiveness, 250, 0.52).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__substitution_archive, base_extractiveness, 500, 0.58).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.64).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.66).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__substitution_archive, base_extractiveness, 2000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(koda_su_t250, kodashim_corpus__substitution_archive, suppression_requirement, 250, 0.55).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__substitution_archive, suppression_requirement, 500, 0.62).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.69).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.71).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__substitution_archive, suppression_requirement, 2000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__substitution_archive, 0.12).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).

% DUAL FORMULATION NOTE:
% The kodashim_corpus kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of the same textual tradition. substitution_archive (this story) claims the archive is memorialized; performance_only claims it is a blueprint awaiting restoration; study_as_exercise claims study itself performs the mitzvah. The three readings have different ε values, different victim/beneficiary structures, and different types because they assign radically different interpretive statuses to the kernel. They are linked by network.affects_constraints because each reading's institutional success creates pressure on the others: as substitution_archive hardens its claim to legitimacy, it forecloses or constrains the other readings' institutional space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
