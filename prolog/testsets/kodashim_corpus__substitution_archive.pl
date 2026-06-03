% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim as Substitution Archive: Prayer and Study Replace Sacrifice
 *   domain: religious_studies/rabbinic_judaism/commitment_system
 *
 * SUMMARY:
 *   The Kodashim corpus (six of the sixty-three tractates of the Mishnah,
 *   codified ~200 CE) preserves the complete legal system of animal sacrifice
 *   in the Jerusalem Temple. Following the Temple's destruction in 70 CE,
 *   this legal corpus became a memorial archive documenting a practice that
 *   was superseded by prayer and Torah study as the primary forms of divine
 *   service. This constraint embodies a substitution claim: prayer and study
 *   have replaced sacrifice; Kodashim is not an occupied kernel but an
 *   archive documenting what was lost. The constraint exhibits characteristic
 *   Tangled Rope structure—it claims continuity with the replaced practice
 *   while denying any possibility of restoration outside messianic time. This
 *   creates an asymmetry: rabbinic institutions benefit from the substitution
 *   (consolidating authority in text interpreters and prayer leaders), while
 *   those seeking living sacrificial practice are told their desire is
 *   obsolete. The constraint's extractiveness arises from this mismatch: the
 *   archive preserves the knowledge of how to sacrifice while the
 *   institutional framing prevents its practice. The theater ratio increases
 *   over time as Kodashim becomes purely performative—studied because it is
 *   obligatory but without practical application. This constraint is ONE
 *   READING of the Kodashim kernel; sibling readings include
 *   'study_as_exercise' (claiming continuous performance through intellectual
 *   engagement) and 'performance_only' (claiming the archive is an awaiting
 *   blueprint for messianic restoration).
 *
 * KEY AGENTS:
 *   - Those Seeking Living Sacrificial Practice (victims): powerless/identity_locked agents who internalize the belief that physical sacrifice is the true worship, yet are told restoration is forbidden except by the Messiah
 *   - Rabbinic Text-Study Institutions (primary beneficiaries): institutional/arbitrage actors who consolidate authority in interpretation and gain control over the meaning of sacrifice through textual mastery
 *   - Committed Jewish Community (secondary agents): moderate/constrained community experiencing genuine coordination through text study alongside suppression of restoration possibilities
 *   - Kodashim Corpus as Institutional Artifact: the archive itself, maintained through inertia as performative study obligation
 *   - Analytical Observer: risks naturalizing a contingent institutional choice (prayer for sacrifice) as a universal law of religious development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.42).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.58).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim as Substitution Archive: Prayer and Study Replace Sacrifice").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious_studies/rabbinic_judaism/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '0b0ef324-561b-4ed6-9ba2-e233ef1e704e').
narrative_ontology:cs_kernel_codification('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', fixed_text).
narrative_ontology:cs_authority_grounding('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', lineage).
narrative_ontology:cs_interpretation_layer_present('0b0ef324-561b-4ed6-9ba2-e233ef1e704e').
narrative_ontology:cs_reading_relation('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', kodashim_corpus__performance_only, influences).
narrative_ontology:cs_axiom('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', foundational, prayer_permanently_replaces_sacrifice).
narrative_ontology:cs_axiom_status(prayer_permanently_replaces_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', prayer_permanently_replaces_sacrifice, conventional).
narrative_ontology:cs_axiom('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', foundational, restoration_requires_messianic_authority).
narrative_ontology:cs_axiom_status(restoration_requires_messianic_authority, holdable).
narrative_ontology:cs_axiom_grounding('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', restoration_requires_messianic_authority, theological).
narrative_ontology:cs_reference_frame('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', substitution_of_prayer_for_sacrifice).
narrative_ontology:cs_drift_state('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', contemporary_diaspora_normalized, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b0ef324-561b-4ed6-9ba2-e233ef1e704e', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, those_seeking_living_sacrificial_practice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SACRIFICIAL PRACTITIONER (SNARE) — Structurally mobile (could study text instead, could leave the tradition) but identity-locked into the belief that living sacrifice is the true service of God. The substitution archive (Kodashim) claims sacrifice is 'obsolete' yet preserves it as a perpetual absence. The practitioner is told: 'This is what you want, but it is no longer permitted.' Extraction lies in the mismatch between internalized identity (sacrificial priest/Levite) and available practice (text study only). The suppression is internalized: the tradition teaches that restoration requires messianic conditions beyond the agent's control.
constraint_indexing:constraint_classification(kodashim_corpus__substitution_archive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMITTED JEWISH COMMUNITY (TANGLED ROPE) — Experiences genuine coordination through Kodashim study: the text embodies continuity with Temple service, enables spiritual engagement with sacrificial law, and maintains collective memory. But also experiences extraction: the substitution is presented as permanent ('never to be restored except by the Messiah'), which suppresses active movements toward restored practice. The community benefits from the coordinating function (text study as meaningful engagement) while being constrained by the framing that restoration is forbidden.
constraint_indexing:constraint_classification(kodashim_corpus__substitution_archive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC TEXT-STUDY INSTITUTIONS (ROPE) — Primary beneficiary. The substitution of prayer and study for sacrifice consolidates institutional authority in the hands of rabbis and text interpreters. Kodashim becomes the master archive: it is not a living law but a memorial that legitimizes current rabbinic institutional practice. These institutions benefit from arbitrage—they can interpret Kodashim flexibly, adjust practice as needed, and claim fidelity to the 'real' service (prayer/study) while maintaining exclusive control over the meaning of sacrifice. Rope classification reflects that study-based service is a genuine coordination mechanism, not pure extraction.
constraint_indexing:constraint_classification(kodashim_corpus__substitution_archive, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: KODASHIM AS INSTITUTIONAL ARTIFACT (PITON) — The continued preservation, commentary, and intensive study of Kodashim despite the absence of living sacrifice is substantially performative. The theater ratio (0.65) reflects that Kodashim study has high symbolic and spiritual content but limited practical application. The constraint persists through institutional inertia: the text must be studied because it is part of the canonical obligation (Talmud study), but its functional relationship to actual worship has atrophied. The performance is maintained by framing it as 'continuity with what was,' not as 'preparation for what could be restored.'
constraint_indexing:constraint_classification(kodashim_corpus__substitution_archive, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical position, one could argue that the substitution of prayer for sacrifice is a universal principle: all religions eventually replace visceral ritual with cognitive-spiritual practice as societies mature. The barrier to restored sacrifice appears immutable—sociological law, not institutional choice. However, the structural data contradicts this: the beneficiary institutions have clear incentives to preserve the substitution indefinitely, and the victims have not ceased seeking restoration. This perspective risks naturalizing a contingent institutional choice as inevitable human development. Engine false-summit detection will flag this as a false summit.
constraint_indexing:constraint_classification(kodashim_corpus__substitution_archive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ORGANIZED RESTORATION MOVEMENT (TANGLED ROPE) — If a sufficiently organized movement emerged claiming the authority to restore sacrifice, they would experience this constraint as tangled rope: the Kodashim corpus would simultaneously enable their practice (it is the legal blueprint) and restrict it (rabbinic institutions claim it is not operative, and only prayer/study may be performed). The archive would be simultaneously a resource (the knowledge of how to perform) and a suppressive mechanism (the framing that says restoration is forbidden). This perspective is counterfactual but diagnostically reveals the structural hybrid nature of the constraint.
constraint_indexing:constraint_classification(kodashim_corpus__substitution_archive, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kodashim_corpus__substitution_archive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kodashim_corpus__substitution_archive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kodashim_corpus__substitution_archive, TR),
    TR >= 0.70.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42): Moderate, reflecting the hybrid nature of tangled rope. The substitution does coordinate religious practice through prayer and study—genuine collective action problems are solved. But it also extracts: those whose identity is constituted through sacrificial practice are told their desire is forbidden; the rabbinic institutions monopolize the authority to define what 'service of God' now means. The extractiveness is obscured by the claim that substitution is permanent and unquestionable. Suppression (0.58): Moderate-high. Multiple suppressive mechanisms: (1) institutional claim that restoration requires messianic conditions beyond any group's authority; (2) identity-lock preventing those who internalize sacrificial identity from conceptualizing alternatives; (3) publication bias and institutional gatekeeping preventing restoration-movement voices from reaching mainstream rabbinic discourse. The suppression is internalized as much as external—the tradition teaches that the desire for restoration is a category error, not a legitimate claim. Theater ratio (0.65): Moderate-high. Kodashim study has high spiritual and symbolic content, but its practical function has atrophied. The theater increases over time (from 0.42 at 70 CE when the Temple was fresh loss, to 0.65 at present when Kodashim is studied as history rather than living law). The performance is maintained by framing study as 'continuation of sacrifice in another form,' which obscures the replacement beneath a veneer of continuity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence across the observation site. The sacrificial practitioner sees the archive as suppression—the knowledge of how to serve is preserved only to be forbidden. The rabbinic institution sees coordination—prayer and study are authentic, improved forms of service. The organized restoration movement (counterfactual perspective) would see tangled rope: the archive enables practice but the framing suppresses it. The institutional artifact view sees piton—the constraint persists through obligation, not function. The natural-law view sees immutable development, but the structural beneficiary/victim data contradicts this naturalization. The perspectival divergence reveals that 'substitution' is not a neutral claim but a reading contested by different positions within and outside the tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from the agent's structural position relative to the substitution. Victims with identity-lock (those seeking restored sacrifice) derive high d (0.85+): they experience full extraction because their internalized identity makes them unable to exercise the structural mobility they possess. Beneficiaries with arbitrage options (rabbinic institutions) derive low d (0.10-0.20): they control the interpretation and can adjust practice flexibly. The community deriving moderate d (0.50-0.60) experiences the constraint symmetrically—real benefits from coordinated prayer/study, real costs from suppressed restoration. The piton perspective experiences moderate-to-high d because the institutional actor maintains a performative constraint that increasingly works against its stated purpose (coordination through authentic engagement). The analytical observer's d (0.72) reflects that external analysis reveals the extractive structure the insiders' framing obscures.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED VIA READING SPECIFICITY: This constraint avoids mandatrophy by being ONE reading of the contested kernel, not a claim to THE correct reading. The substitution_archive reading asserts: 'Prayer and study replaced sacrifice; restoration is forbidden (except messianic).' This reading is genuinely tangled rope—it coordinates practice while suppressing alternatives. The sibling reading study_as_exercise asserts: 'Study IS the performance of sacrifice; the kernel is occupied.' This reading would be tangled rope or even rope from its own frame. The sibling reading performance_only asserts: 'The archive is a blueprint awaiting restoration; substitution is temporary.' This reading would be snare from the rabbinical institution's perspective. All three readings are defensible within the tradition; they coexist without logical foreclosure because they compete in authority but do not in formal logic. The mandatrophy dissolves when we recognize we are classifying a reading, not the kernel itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_vs_supersession,
    'Is Kodashim a substitution archive (claiming the old practice is permanently obsolete) or a supersession archive (claiming the old practice continues in transformed form)?',
    'Historical-textual analysis of Talmudic and post-Talmudic rabbinic language: does the corpus treat sacrifice as ''never to return except by Messiah'' (substitution) or as ''incorporated into prayer and study'' (supersession)? Analysis of actual restoration movements and rabbinic responses to them.',
    'If pure substitution: classification confirms snare from victim perspective (told it is forbidden). If supersession: victim classification might shift to constrained or mobile (the practice is said to continue, just in different form). Extractiveness would remain moderate but suppression mechanism would change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_supersession, empirical, 'Whether Kodashim claims substitution (replacement) or supersession (continuation in transformed form)').

omega_variable(
    messianic_restoration_credibility,
    'Within the tradition''s own epistemic frame, how credible is the claim that messianic conditions would permit restored sacrifice? Is restoration actively expected or relegated to abstract theological possibility?',
    'Content analysis of Jewish liturgical texts, medieval and modern Jewish philosophy, and contemporary movement statements. Measurement of doctrinal certainty vs. rhetorical affirmation.',
    'If restoration is actively expected: victim identity-lock is weakened (there is a perceived path to exit). If restoration is purely abstract: victim identity-lock is strengthened (the exit is permanently deferred). Extractiveness would remain similar, but the temporal horizon for experiencing the constraint changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_credibility, empirical, 'Credibility and temporal location of messianic restoration within the tradition').

omega_variable(
    reading_competition_in_authority,
    'Do all three readings (substitution_archive, study_as_exercise, performance_only) compete for authority within a single institutional framework, or are they held by separate communities/denominations?',
    'Institutional mapping: which rabbinic authorities, denominations, and community sectors hold each reading? Do they claim compatibility or mutual exclusivity?',
    'If competing within single framework: the constraint is better modeled as distributed authority (CS authority_grounding). If separated by denomination: the constraint is better modeled as inter-institutional power dynamics. Classification type remains tangled_rope, but the beneficiary/victim structure shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_competition_in_authority, empirical, 'Whether the three readings compete within one authority structure or are separated by institutional boundaries').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint one reading of the Kodashim kernel, or is it a reading of a deeper kernel about the relationship between text and practice?',
    'Meta-structural analysis: the Kodashim corpus is the manifest kernel here. But the deeper reading choice is about whether sacred texts preserve what they replace (archive function) or continue what they name (performance function). Does the constraint belong to ''sacrifice law'' or to ''the role of canonical texts in religious practice''?',
    'If Kodashim-specific: the three readings are local to Jewish law. If meta-structural: the constraint is a case study in how canons function across religious traditions (Christianity, Islam, Buddhism preserve texts of practices they no longer perform). Classification type remains tangled_rope, but the sibling reading structure may generalize.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this is a reading of the Kodashim kernel or a reading of a meta-kernel about canonical text function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_subst_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.42).
narrative_ontology:measurement(kodashim_subst_tr_t500, kodashim_corpus__substitution_archive, theater_ratio, 500, 0.58).
narrative_ontology:measurement(kodashim_subst_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.65).

% Extraction over time
narrative_ontology:measurement(kodashim_subst_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(kodashim_subst_be_t500, kodashim_corpus__substitution_archive, base_extractiveness, 500, 0.35).
narrative_ontology:measurement(kodashim_subst_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, enforcement_mechanism).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).

% DUAL FORMULATION NOTE:
% The Kodashim corpus is one constraint decomposed into three readings by the kernel contest structure. Each reading has its own extractiveness, its own beneficiary/victim structure, and its own classification type. They share a single source text (the Kodashim corpus) but assign it different functions and different temporal relationships (permanent replacement vs. occupied performance vs. awaiting restoration). All three readings link through network.affects_constraints because they are sibling readings of the same kernel, and because the dominance of the substitution_archive reading suppresses the authority claims of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__substitution_archive, powerless, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
