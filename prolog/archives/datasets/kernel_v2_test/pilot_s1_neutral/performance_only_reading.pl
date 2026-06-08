% ============================================================================
% CONSTRAINT STORY: performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_only_reading, []).

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
 *   constraint_id: performance_only_reading
 *   human_readable: Sacrifice Obligation (Performance-Only Reading) — Unfulfilled Commandment as Structural Impossibility
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   The performance-only reading of the sacrifice obligation kernel maintains
 *   that the commandment to perform animal sacrifice at the Temple (Exodus
 *   29:42, Leviticus 6:2, Numbers 28-29) is a binding mitzvat aseh (positive
 *   obligation) whose fulfillment cannot be waived, substituted, or
 *   reinterpreted through study or symbolic practice. After the Temple's
 *   destruction in 70 CE, this reading locks the Jewish people into a
 *   structural impossibility: the obligation persists as fully binding while
 *   its performance is materially impossible for 1,900+ years. No legitimate
 *   authority can dissolve the command; no individual can escape the
 *   contradiction. This reading instantiates one position in a contested
 *   kernel about how to relate divine commandment to post-destruction Jewish
 *   practice. The competing readings (study-as-exercise,
 *   messianic-suspension, symbolic-archive) each propose different
 *   resolutions: that rigorous study of sacrificial procedure fulfills the
 *   obligation vicariously; that the obligation is suspended pending Temple
 *   restoration; that sacrifice has been symbolically transformed into prayer
 *   and ethical action. The performance-only reading forecloses the first two
 *   alternatives and influences the third by insisting on the
 *   irreplaceability of physical sacrifice.
 *
 * KEY AGENTS:
 *   - Jewish people (post-destruction): Powerless/trapped — commanded but unable to perform; forced to maintain an obligation they cannot fulfill. Isolated from Temple service, from the means of atonement, from direct service to God as commanded.
 *   - Halakhic authority (Rabbinical courts, Rishonim, Acharonim): Organized institutional agents who maintain the binding force of the performance-only reading while coordinating practice around non-performance through substitution (prayer), reinterpretation (study as preparatory), and acceptance of practical suspension. Benefit from interpretive monopoly and institutional continuity; constrained by the need to hold together a coherent legal system under crisis conditions.
 *   - The textual tradition (Talmud, Codes, responsa): Institutional preservation of detailed sacrificial knowledge (Temple architecture, priestly procedures, offering protocols) maintained theatrically rather than functionally. Persists through institutional inertia and theological hope (Messianic restoration).
 *   - Competing readings: Study-as-exercise reading (treats study as fulfilling the obligation), Messianic-suspension reading (obligation suspended pending restoration), Symbolic-archive reading (sacrifice transformed into prayer and ethical action). Each proposes an exit from the structural impossibility; the performance-only reading constrains their legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_only_reading, 0.92).
domain_priors:suppression_score(performance_only_reading, 0.88).
domain_priors:theater_ratio(performance_only_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_only_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(performance_only_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(performance_only_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_only_reading, snare).
narrative_ontology:human_readable(performance_only_reading, "Sacrifice Obligation (Performance-Only Reading) — Unfulfilled Commandment as Structural Impossibility").
narrative_ontology:topic_domain(performance_only_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:requires_active_enforcement(performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_only_reading, '6c2df7e9-529b-43a8-af2f-15b6651c355f').
narrative_ontology:cs_kernel_codification('6c2df7e9-529b-43a8-af2f-15b6651c355f', fixed_text).
narrative_ontology:cs_authority_grounding('6c2df7e9-529b-43a8-af2f-15b6651c355f', lineage).
narrative_ontology:cs_interpretation_layer_present('6c2df7e9-529b-43a8-af2f-15b6651c355f').
narrative_ontology:cs_reading_relation('6c2df7e9-529b-43a8-af2f-15b6651c355f', performance_only_reading__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('6c2df7e9-529b-43a8-af2f-15b6651c355f', performance_only_reading__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c2df7e9-529b-43a8-af2f-15b6651c355f', performance_only_reading__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('6c2df7e9-529b-43a8-af2f-15b6651c355f', foundational, physical_performance_irreplaceable).
narrative_ontology:cs_axiom_status(physical_performance_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('6c2df7e9-529b-43a8-af2f-15b6651c355f', physical_performance_irreplaceable, deontological).
narrative_ontology:cs_axiom('6c2df7e9-529b-43a8-af2f-15b6651c355f', foundational, obligation_binding_despite_impossibility).
narrative_ontology:cs_axiom_status(obligation_binding_despite_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('6c2df7e9-529b-43a8-af2f-15b6651c355f', obligation_binding_despite_impossibility, deontological).
narrative_ontology:cs_reference_frame('6c2df7e9-529b-43a8-af2f-15b6651c355f', eternal_binding_obligation_at_covenant).
narrative_ontology:cs_drift_state('6c2df7e9-529b-43a8-af2f-15b6651c355f', post_destruction_1900_years, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6c2df7e9-529b-43a8-af2f-15b6651c355f', '2026-02-27T14:32:18Z').
narrative_ontology:cs_kernel_id(performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(performance_only_reading, jewish_people_post_destruction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE JEWISH PEOPLE POST-DESTRUCTION (SNARE) — Commanded to perform animal sacrifice at the Temple; Temple destroyed 70 CE and never rebuilt; obligation persists in halakhic literature as binding (Maimonides Mishneh Torah, Hilkhot Bi'at ha-Mikdash 1:1). No exit option: cannot perform the mitzvah, cannot escape the obligation. Trapped between literal command and material impossibility. Full extraction from this reading — the commandment persists, the people bear the guilt of non-performance, no beneficiary agent collects from this gap (it is structural impossibility, not rent extraction, but the cost is real). Generations experience this as an unsolvable contradiction embedded in the legal system itself.
constraint_indexing:constraint_classification(performance_only_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IMMUTABLE DIVINE COMMAND (MOUNTAIN) — From the standpoint of performance-only reading: the commandment to perform animal sacrifice is a divine command (mitzvat aseh, positive obligation) whose fulfillment cannot be waived, substituted, or reinterpreted. The obligation is inherent to the covenant itself (Numbers 29:39: 'These things you shall do to the Lord on your appointed times'). No human interpretive authority can dissolve the command — only God (through a new revelation) or the reappearance of the Temple could. This perspective perceives the constraint as an immutable feature of the legal-theological landscape, not as a constructed system. However, the existence of alternative readings (study-as-exercise, symbolic archive) and the engine's false summit detection will reveal this as a contested boundary between natural law and constructed impossibility.
constraint_indexing:constraint_classification(performance_only_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: HALAKHIC AUTHORITY (TANGLED ROPE) — The institutional authority that maintains the performance-only reading within the legal corpus (Talmud, Codes, contemporary pesak) while simultaneously coordinating Jewish legal practice around non-performance (prayer substitution, study as preparatory, theoretical discussion of Temple procedures, acceptance of practical suspension). The authority is coordinating a real problem: how to maintain the integrity of a legal corpus while acknowledging material impossibility. The authority also extracts: the interpretation is fixed by a specific lineage, alternative readings are delegitimized, and the authority's role as arbiter of what is 'authentic' depends on maintaining the performance-only reading as formally binding. This is both genuine coordination (holding together a living legal tradition under crisis conditions) and asymmetric extraction (the powerless masses cannot escape the framework, the authority maintains interpretive monopoly).
constraint_indexing:constraint_classification(performance_only_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TEXTUAL TRADITION (PITON) — The performance-only reading persists in the canon (Shulchan Arukh, Mishneh Torah) as formally binding law while being theatrically maintained rather than functionally performed. Extensive discussions of sacrifice procedures, Temple architecture, priestly garments, and offering protocols fill the legal literature — detailed, authoritative, and completely non-functional for 1,900 years. The maintenance is not cynical but genuinely committed: the tradition preserves the precise knowledge necessary to perform the mitzvah the moment conditions change (Messianic restoration, divine intervention, Temple reconstruction). The theater ratio is high because the entire discussion is preparatory, not performative — the constraint persists through institutional inertia and theological hope, not through any actual coordination or extraction occurring. The piton classification reflects that the function (actual sacrifice) has atrophied, but the institutional structure (the law, the learning, the authority) persists theatrically.
constraint_indexing:constraint_classification(performance_only_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From the universal analytical perspective, this constraint represents a structural impossibility elevated to the status of binding law. The analytical view risks naturalizing the impossibility as inevitable or immutable: 'Divine commands are inherently non-negotiable; therefore the constraint persists regardless of material conditions.' This naturalization obscures the contingent historical fact that alternative readings exist and that the performance-only reading was a deliberate choice among competing interpretive frameworks. The engine's false summit detection will flag this as a false natural law — the reading's claim to immutability depends on rejecting competing readings, which is a constructed rather than natural action.
constraint_indexing:constraint_classification(performance_only_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_only_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performance_only_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performance_only_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performance_only_reading, TR),
    TR >= 0.70.

:- end_tests(performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.92): Extremely high. The constraint extracts from the Jewish people in the form of an unsolvable obligation: they are commanded to perform a mitzvah they cannot perform, they experience guilt for non-fulfillment, and no legitimate authority can absolve them. The obligation is treated as binding across all 1,900+ years post-destruction with only minimal diminution in formal status. The extractiveness increases slightly over time (0.88 → 0.92) as the impossibility becomes more firmly entrenched in the tradition — the early centuries allowed some fluidity in interpretation; by the Medieval period and thereafter, the performance-only reading was highly standardized. Suppression (0.88): Extremely high and rising. The suppression operates through theological authority: the obligation's binding force is enforced through the authority of Talmud, the Codes, and the halakhic chain. Alternative readings exist but are suppressed through delegitimization or minority status. The suppression increases over time (0.75 → 0.88) as the tradition solidifies: early post-destruction Judaism (Tannaitic/Amoraic period) had more fluidity; by the Geonic period and especially the Medieval period (Maimonides, Shulchan Arukh), the performance-only reading became increasingly crystallized as the authoritative position. Theater ratio (0.30 → 0.65): Increasing. Early post-destruction Judaism had lower theater because the constraint was newer and the gap between obligation and possibility was still shocking. Over centuries, the tradition developed elaborate substitutes (prayer, study, theoretical discussion) that maintain the appearance of addressing the obligation while acknowledging its practical impossibility. By the Medieval period, extensive discussions of Temple procedures, sacrificial offerings, and priestly qualifications filled the legal literature — highly learned, theologically committed, and completely non-functional. The theater ratio levels off around 0.65 because the tradition reaches a stable equilibrium: the obligation is formally binding, the people accept practical non-performance, and the textual corpus preserves detailed knowledge against a potential future restoration.
 *
 * PERSPECTIVAL GAP:
 *   The five perspectives on this constraint diverge dramatically. The powerless Jewish people experience the constraint as an unescapable snare: commanded and condemned to permanent non-fulfillment. The immutable divine command perspective (Mountain) sees the obligation as natural law — inherent to the covenant, not subject to reinterpretation. The halakhic authority perspective (Tangled Rope) holds both the obligation and the impossibility in tension, coordinating practice while maintaining formal binding force. The textual tradition perspective (Piton) preserves detailed knowledge theatrically, maintaining institutional continuity through hope for restoration. The analytical observer (Mountain) risks naturalizing the constraint as inevitable feature of post-destruction Judaism, obscuring the contested nature of the reading choice. The perspectival gaps reveal how the same constraint appears as: inevitable law (Mountain), impossible obligation (Snare), managed coordination (Tangled Rope), preserved tradition (Piton), and naturalized impossibility (false Mountain). These gaps expose the contested status of the reading itself — whether the performance-only reading is the binding truth or one choice among alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   The performance-only reading creates a directing force (d → 1.0, full victimhood) toward the Jewish people by placing them in an impossible obligation. The derivation is straightforward: the Jewish people are trapped (no exit), have no alternative reading to escape to (the authority suppresses alternatives), and bear the full cost of non-performance (guilt, ontological incompleteness, expulsion from direct service to God). The directed force is unidirectional because there is no beneficiary agent — no one collects rents from the Jewish people's inability to perform. This is extraction without an extractor: a structural impossibility maintained as binding law, not a mechanism for transferring resources. The halakhic authority constrains this calculation: they benefit from maintaining interpretive authority over the reading, but they do not 'extract' in the usual sense. They coordinate a genuine problem (holding together a legal tradition under crisis conditions) while also exercising monopoly power over interpretation. The directionality for the authority is moderate (d ≈ 0.4-0.5): they are neither full targets nor full beneficiaries, but agents with constrained agency managing an impossible situation. The Messianic observer might assign d ≈ 0.0 (the obligation awaits restoration; when the Temple stands, d → 0.0 and the constraint dissolves). The analytical observer assigns d → 1.0 but risks naturalizing the impossibility as immutable rather than constructed.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate — to perform animal sacrifice at the Temple — is dead (the Temple has not been rebuilt in 1,900 years) but the obligation persists as binding law. This is mandate obsolescence with obligation persistence, which is the hallmark of mandatrophy-resolved status. The halakhic tradition acknowledges the mandatrophy through elaborate substitutes (prayer in place of sacrifice, study in place of performance, theoretical discussion of procedure in place of actual implementation), but the performance-only reading refuses to declare the obligation itself obsolete — only its practical performance is suspended. The tradition has not resolved the mandatrophy by rescinding the obligation; instead, it has attempted to manage it through: (1) theological reframing of the obligation as eternally binding even if temporarily impossible (Maimonides: the obligation is binding and will be performed when conditions permit); (2) substitutive practices that maintain formal adherence while acknowledging practical suspension (prayer, study, annual discussions of Temple procedure); (3) eschatological hope (the Messianic age will restore the Temple and enable performance). The mandatrophy is partially resolved through psychological acceptance (the Jewish people have integrated the impossibility into their theology) but not resolved through institutional restructuring (the obligation remains formally binding, not formally suspended or rescinded). The performance-only reading contributes to this partial resolution by insisting on the irreplaceability of physical sacrifice — if study or prayer fulfilled the obligation, the mandatrophy would be resolved. By maintaining the performance-only reading, the tradition keeps alive the expectation and hope of future restoration, which psychologically sustains the unsolved mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_performance_dichotomy,
    'Is the dichotomy between ''reading/study about sacrifice'' and ''performing sacrifice'' a fundamental theological distinction or a constructed interpretive choice?',
    'Textual-genealogical analysis: tracing when the performance-only reading solidified in the tradition (Talmudic period, Geonic period, Medieval period); identification of earlier sources that blur the boundary or treat study as functionally equivalent; comparison with sister traditions (Karaite, Samaritan) that made different choices.',
    'If fundamental: the reading''s claim to necessity is strengthened, and the snare classification holds. If constructed: the reading becomes one reading among legitimate alternatives, and the snare is revealed as a interpretive choice that constrains, not a law of nature. The false summit detector will fire if beneficiaries are identified for maintaining this constructed boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_performance_dichotomy, conceptual, 'Whether the performance-only reading represents a fundamental theological truth or an interpretive choice').

omega_variable(
    impossibility_as_binding_obligation,
    'Can a halakhic obligation remain fully binding when its performance is materially and permanently impossible?',
    'Survey of halakhic authorities across periods: do contemporary posskim rule that the obligation persists as binding with guilt/violation status, or has the obligation been reclassified (conditional, suspended, transformed)? Identification of the specific moment when authority ruled the performance impossible vs. obligation superseded.',
    'If obligation persists as binding despite impossibility: the snare classification is correct and the extractiveness reflects legitimate enforcement of a constraint the agent cannot fulfill. If obligation has been reclassified: the performance-only reading has already been abandoned in practice (even if textually preserved), and the piton classification gains strength — what persists is theater, not enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impossibility_as_binding_obligation, empirical, 'Whether halakhic obligation remains fully binding under permanent material impossibility').

omega_variable(
    temple_restoration_eschatology,
    'Is the hope/expectation of Temple restoration integral to the binding force of the performance-only reading, or is the reading binding independently of eschatological belief?',
    'Textual analysis: are the performance-only reading''s foundational sources grounded in eschatological belief (Maimonides: mitzvot will be observed again in Messianic age), or are they grounded in pure obligation logic (divine command is inherently binding regardless of performability)? Survey of contemporary authorities: do they ground the obligation''s binding force in restoration hope or in principle?',
    'If integral: the reading''s durability depends on eschatological belief persisting; if that belief erodes, the reading loses its psychological anchor and may become abandoned despite textual preservation. If independent: the reading is more theoretically robust but also more clearly a snare — the obligation persists indefinitely regardless of future possibilities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_restoration_eschatology, preference, 'Whether Temple restoration eschatology is integral to the obligation''s binding force').

omega_variable(
    kernel_reading_contest,
    'Is the performance-only reading merely one reading of a contested kernel (sacrifice obligation), or is it the authoritative reading and the alternatives are heresies/departures?',
    'Institutional history of Jewish legal authority: what is the status of the performance-only reading in the mainstream tradition (Ashkenazi, Sephardi, Mizrahi, Modern Orthodox)? How are competing readings (study-as-exercise, symbolic archive) treated institutionally (legitimate disagreement, minority position, heretical reinterpretation)?',
    'If merely one reading: the constraint story should be reframed from ''snare imposed by authority'' to ''reading chosen and maintained by authority,'' which affects the victim analysis. If authoritative and binding: the other readings are departures or minority positions, not co-equal siblings. The reading_relations edges in cs_structure are determined by this answer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Status of the performance-only reading in mainstream halakhic authority').

omega_variable(
    beneficiary_concealment,
    'Does the maintenance of the performance-only reading benefit any identifiable agent (the tradition, the authority, the textual corpus) even though the command cannot be performed?',
    'Institutional analysis: what authority, status, or institutional continuity would be lost if the performance-only reading were abandoned in favor of study-as-exercise or symbolic archive? Whose interpretive monopoly depends on the performance-only reading remaining binding?',
    'If beneficiaries exist: the snare classification is correct and the constraint may be an example of false natural law (beneficiaries maintaining the binding nature of an impossible obligation to preserve institutional authority). If no beneficiaries: the constraint is genuinely a structural impossibility without exploitative mechanism, and the classification should shift toward ''mountain of circumstance'' (immutable fact about post-destruction Judaism, not extracted from by any agent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concealment, empirical, 'Whether the performance-only reading''s maintenance benefits identifiable institutional actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_only_reading, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_theater_t0_70ce, performance_only_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(perf_theater_t500_500ce, performance_only_reading, theater_ratio, 500, 0.5).
narrative_ontology:measurement(perf_theater_t1000_1000ce, performance_only_reading, theater_ratio, 1000, 0.6).
narrative_ontology:measurement(perf_theater_t1500_1500ce, performance_only_reading, theater_ratio, 1500, 0.65).
narrative_ontology:measurement(perf_theater_t1900_1970ce, performance_only_reading, theater_ratio, 1900, 0.65).

% Extraction over time
narrative_ontology:measurement(perf_extractiveness_t0_70ce, performance_only_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(perf_extractiveness_t500_500ce, performance_only_reading, base_extractiveness, 500, 0.9).
narrative_ontology:measurement(perf_extractiveness_t1000_1000ce, performance_only_reading, base_extractiveness, 1000, 0.92).
narrative_ontology:measurement(perf_extractiveness_t1500_1500ce, performance_only_reading, base_extractiveness, 1500, 0.92).
narrative_ontology:measurement(perf_extractiveness_t1900_1970ce, performance_only_reading, base_extractiveness, 1900, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(perf_suppression_t0_70ce, performance_only_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(perf_suppression_t500_500ce, performance_only_reading, suppression_requirement, 500, 0.82).
narrative_ontology:measurement(perf_suppression_t1000_1000ce, performance_only_reading, suppression_requirement, 1000, 0.85).
narrative_ontology:measurement(perf_suppression_t1500_1500ce, performance_only_reading, suppression_requirement, 1500, 0.88).
narrative_ontology:measurement(perf_suppression_t1900_1970ce, performance_only_reading, suppression_requirement, 1900, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_only_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(performance_only_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(performance_only_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(performance_only_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice obligation kernel decomposes into four distinct constraints, each with its own ε value and structural dynamics. The performance-only reading (this story) has the highest extractiveness (0.92) because it maintains the obligation's binding force while denying all substitutive readings. The study-as-exercise reading has lower extractiveness (estimated ~0.30) because it permits fulfillment through study. The messianic-suspension reading has moderate extractiveness (estimated ~0.50) because it acknowledges the impossibility but maintains formal binding force pending restoration. The symbolic-archive reading has the lowest extractiveness (estimated ~0.15) because it permits fulfillment through prayer and ethical action. Each reading produces a different constraint with a different victim set and beneficiary structure. The performance-only reading is upstream of all others — it forecloses the study-and-symbolic readings and coexists with the messianic-suspension reading. The performance-only reading's persistence constrains the legitimacy of the other readings, which is why it appears in the affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_only_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
