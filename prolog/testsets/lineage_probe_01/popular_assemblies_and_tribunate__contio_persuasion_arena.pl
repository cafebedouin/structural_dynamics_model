% ============================================================================
% CONSTRAINT STORY: popular_assemblies_and_tribunate__contio_persuasion_arena
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_popular_assemblies_and_tribunate__contio_persuasion_arena, []).

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
 *   constraint_id: popular_assemblies_and_tribunate__contio_persuasion_arena
 *   human_readable: The Contio: Persuasion Assembly and the Republic's Public Sphere
 *   domain: legal/doctrinal/political_structure
 *
 * SUMMARY:
 *   The contio was Rome's formal persuasion assembly — the occasion when
 *   magistrates presented their proposals to the crowd in the Field of Mars
 *   before voting in the comitia. This constraint models one specific reading
 *   of contested Roman political authority: that the contio represented the
 *   Republic's actual public sphere, where consent was negotiated through
 *   hearing and oratorical persuasion before voting could claim legitimacy.
 *   However, this reading is one of several competing claims about where
 *   Roman popular power actually resided. The contio was either the forum
 *   where decisions were genuinely deliberated and shaped by popular voice
 *   (contio_persuasion_arena reading), or it was secondary to the timocratic
 *   structure of voting by property class in the centuriate assembly
 *   (comitia_centuriata_timocracy), or it was a ritual maintained to mask
 *   elite decisions already made in the Senate and distributed through patron
 *   networks (piton reading), or it was gradually superseded by the
 *   plebiscite's direct force of law bypassing comitia machinery
 *   (plebiscite_force_of_law), or it was always circumvented by the tribune's
 *   inviolable body blocking decisions without requiring hearing
 *   (tribunician_sacrosanctity). This story instantiates the reading that
 *   privileges hearing, persuasion, and the audible crowd as the locus of
 *   legitimate decision-making — and models the suppression built into that
 *   reading's own mechanism.
 *
 * KEY AGENTS:
 *   - Magistrate and Orators: Primary beneficiaries (institutional/arbitrage) — monopoly on audible voice and oratorical persuasion apparatus; extract consent manufacture while legitimacy from coordination function
 *   - Audible Crowd Faction: Secondary beneficiary (organized/mobile) — those positioned, vocal, and allied enough to be heard; benefit from orator networks and patron access
 *   - Silent Majority: Primary victims (powerless/trapped) — physically present in Field of Mars but acoustically unable to participate; suppressed by architectural design and mandatory attendance requirement
 *   - Senatorial Oligarchy: Powerful secondary actor (powerful/mobile) — benefits from contio as coordination mechanism but constrained by its own logic requiring public justification
 *   - Organized Plebeian Coalition: Organized secondary victim (moderate/constrained) — leverage through numbers and coalition threat, but constrained by orator monopoly on persuasive voice
 *   - Tribunician Authority: Potential competing reader (powerful/mobile) — the tribune's veto and sacrosanct body represent alternative legitimacy claim (inviolability) that may foreclose or coexist with contio-based legitimacy
 *   - Republican Institutional System: Performer of the ritual (institutional/arbitrage) — maintains the contio's theater while its deliberative function attenuates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(popular_assemblies_and_tribunate__contio_persuasion_arena, 0.52).
domain_priors:suppression_score(popular_assemblies_and_tribunate__contio_persuasion_arena, 0.68).
domain_priors:theater_ratio(popular_assemblies_and_tribunate__contio_persuasion_arena, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__contio_persuasion_arena, extractiveness, 0.52).
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__contio_persuasion_arena, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__contio_persuasion_arena, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(popular_assemblies_and_tribunate__contio_persuasion_arena, tangled_rope).
narrative_ontology:human_readable(popular_assemblies_and_tribunate__contio_persuasion_arena, "The Contio: Persuasion Assembly and the Republic's Public Sphere").
narrative_ontology:topic_domain(popular_assemblies_and_tribunate__contio_persuasion_arena, "legal/doctrinal/political_structure").

domain_priors:requires_active_enforcement(popular_assemblies_and_tribunate__contio_persuasion_arena).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(popular_assemblies_and_tribunate__contio_persuasion_arena, '139ff806-23ab-4110-8b36-66754d7c6ea4').
narrative_ontology:cs_kernel_codification('139ff806-23ab-4110-8b36-66754d7c6ea4', formalized).
narrative_ontology:cs_authority_grounding('139ff806-23ab-4110-8b36-66754d7c6ea4', practice).
narrative_ontology:cs_interpretation_layer_present('139ff806-23ab-4110-8b36-66754d7c6ea4').
narrative_ontology:cs_reading_relation('139ff806-23ab-4110-8b36-66754d7c6ea4', popular_assemblies_and_tribunate__comitia_centuriata_timocracy, coexists_with).
narrative_ontology:cs_reading_relation('139ff806-23ab-4110-8b36-66754d7c6ea4', popular_assemblies_and_tribunate__plebiscite_force_of_law, influences).
narrative_ontology:cs_reading_relation('139ff806-23ab-4110-8b36-66754d7c6ea4', popular_assemblies_and_tribunate__tribunician_sacrosanctity, forecloses).
narrative_ontology:cs_axiom('139ff806-23ab-4110-8b36-66754d7c6ea4', foundational, hearing_precedes_binding_vote).
narrative_ontology:cs_axiom_status(hearing_precedes_binding_vote, holdable).
narrative_ontology:cs_axiom_grounding('139ff806-23ab-4110-8b36-66754d7c6ea4', hearing_precedes_binding_vote, deontological).
narrative_ontology:cs_axiom('139ff806-23ab-4110-8b36-66754d7c6ea4', foundational, persuasion_over_coercion).
narrative_ontology:cs_axiom_status(persuasion_over_coercion, holdable).
narrative_ontology:cs_axiom_grounding('139ff806-23ab-4110-8b36-66754d7c6ea4', persuasion_over_coercion, instrumental).
narrative_ontology:cs_reference_frame('139ff806-23ab-4110-8b36-66754d7c6ea4', republican_deliberative_hearing).
narrative_ontology:cs_drift_state('139ff806-23ab-4110-8b36-66754d7c6ea4', late_republic_institutional_degradation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('139ff806-23ab-4110-8b36-66754d7c6ea4', '').
narrative_ontology:cs_kernel_id(popular_assemblies_and_tribunate__contio_persuasion_arena, popular_assemblies_and_tribunate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(popular_assemblies_and_tribunate__contio_persuasion_arena, magistrates_and_orators).
narrative_ontology:constraint_beneficiary(popular_assemblies_and_tribunate__contio_persuasion_arena, audible_crowd_faction).
narrative_ontology:constraint_victim(popular_assemblies_and_tribunate__contio_persuasion_arena, silent_majority).
narrative_ontology:constraint_victim(popular_assemblies_and_tribunate__contio_persuasion_arena, decisions_requiring_consensus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SILENT MAJORITY (SNARE) — Citizens physically present but spatially unable to hear, contest orators, or influence the persuasion. The contio suppresses exit: attendance is mandatory for voting legitimacy, but the acoustic arrangement ensures powerless citizens cannot participate in the actual persuasion. Extraction is maximal because the mechanism captures the appearance of consent while preventing actual deliberation.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__contio_persuasion_arena, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZED PLEBEIAN BLOC (TANGLED ROPE) — Moderate power through coalition and numbers but constrained by the contio's structure. The persuasion assembly both enables and exploits them: they benefit from the right to hear magistrates' justifications before voting (coordination function), but the orators' monopoly on the microphone extracts consent manufacture. Exit is costlier than exit from a simple snare because the plebs have some institutional leverage (the threat of united voting), but the contio's acoustic design suppresses their collective voice.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__contio_persuasion_arena, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: MAGISTRATE AND ORATORS (ROPE) — The contio is the magistrate's coordination mechanism: persuasion before the vote creates legitimate mandate and distributes the burden of justification onto deliberative discussion rather than fiat. The magistrate benefits from the coordination function (consent appears earned rather than coerced) and from the extraction of audible influence (those who can reach the crowd via oratorical skill shape the outcome). The mechanism's existence benefits the magistrate-orator nexus structurally — arbitrage exit means the magistrate can withdraw the contio and vote in the comitia without hearing, but doing so sacrifices the legitimacy gain.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__contio_persuasion_arena, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: SENATORIAL OLIGARCHY (TANGLED ROPE) — The Senate benefits from the contio as a coordination mechanism that distributes elite persuasion across magistrate networks and shapes plebeian expectations before voting. But the Senate's power is constrained by the contio's own logic: once a hearing is granted, decisions that cannot survive public articulation lose legitimacy. The oligarchy experiences the contio as both enabling (control of magistrate oratory) and limiting (the need to construct publicly defensible justifications). Exit is mobile but costly — the Senate could abolish the contio, but doing so would signal fear of public hearing and undermine Senate legitimacy.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__contio_persuasion_arena, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN LEGITIMACY RITUAL (PITON) — The contio is increasingly performative as the Republic ages: the ritual of hearing is maintained but its actual deliberative function degrades. Magistrates deliver prepared speeches; crowds are steered by organizers; the appearance of persuasion masks manufacture of consent. Theater ratio rises because the mechanism's primary function (generating consensus through public justification) is attenuated while the theatrical element (the ritual of the hearing itself) is preserved. The institutional system continues to require the contio because abandoning it would expose the oligarchy's actual unilateral power — inertia and theater sustain what coordination function has eroded.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__contio_persuasion_arena, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MOUNTAIN VIEW (MOUNTAIN) — From a civilizational perspective, the contio represents an immutable structural necessity of rule legitimation: any government that claims consent must hold at least the ritual of hearing. This perspective risks naturalizing the specific Roman design as a universal requirement. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that the contio's specific suppressive architecture (acoustic design, orator monopoly, mandatory attendance) is a contingent institutional choice, not an inherent requirement of public assembly.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__contio_persuasion_arena, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(popular_assemblies_and_tribunate__contio_persuasion_arena_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__contio_persuasion_arena, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__contio_persuasion_arena, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(popular_assemblies_and_tribunate__contio_persuasion_arena, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(popular_assemblies_and_tribunate__contio_persuasion_arena, TR),
    TR >= 0.70.

:- end_tests(popular_assemblies_and_tribunate__contio_persuasion_arena_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The contio extracts consent manufacture through orator monopoly on the audible channel and through mandatory attendance that makes voting legitimacy depend on prior hearing. But extraction is not maximal (not 0.66+) because the mechanism genuinely coordinates: magistrates must articulate defensible rationales, the orators must construct persuasive cases, and the plebs' threat to reject unpersuasive arguments creates real constraint on elite arbitrariness. The extraction accumulates over time (0.38 → 0.58) as the mechanism's deliberative function attenuates and the ritual's purely theatrical element grows. Suppression (0.68): High. The Field of Mars's acoustic design ensures the silent majority cannot hear or respond; crowd organizers control who speaks; magistrate monopoly prevents counter-oratory; mandatory attendance prevents opting out of the hearing without losing voting legitimacy. Suppression is not near-maximal (not 0.80+) because organized plebeian blocs and tribune threats provide partial exits and leverage points. Theater ratio (0.55): Moderate-high and rising. Early in the constraint's development, the contio genuinely coordinates: magistrates articulate novel proposals and plead their cases; crowds engage in genuine deliberation audibly. By the late Republic, the theater ratio rises to 0.62+ as speeches become formulaic, crowds are pre-marshaled by patron networks, and the comitia voting pattern follows senatorial decisions regardless of contio persuasion. The measurements track this degradation across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The contio produces sharp perspectival gaps precisely because it claims to democratize decision-making while engineering suppression. The silent majority sees snare (trapped by mandatory attendance, suppressed by architecture, extraction of the appearance of participation). The organized plebeian bloc sees tangled rope (genuine coordination function enabling hearing before vote, but orator monopoly extracts asymmetric influence). The magistrate sees rope (legitimate coordination mechanism generating consent). The Senate sees tangled rope (both enabling coordination of elite persuasion and limiting the Senate's unilateral power through the requirement to construct defensible justifications). The institutional system sees piton (the ritual persists through inertia and legitimacy theater while its deliberative function erodes). The civilizational analyst risks mountain (seeing democratic hearing as an immutable necessity) but the structural data reveals this as false summit — the contio's suppressive specific design is contingent, not inherent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by position in the acoustic and political structure. Magistrates and skilled orators occupy beneficiary + arbitrage positions → low d → negative effective extraction (they experience the constraint as enabling their voice). The silent majority occupies victim + trapped position → high d (0.90+) → maximum experienced extraction. The organized plebeian bloc occupies victim + constrained position → moderate d (0.55-0.65) → moderate chi (extraction exists but with some leverage for exit). The Senate occupies both beneficiary (via magistrate control) and victim (via requirement to justify) → moderate-high d (0.50-0.60) → moderate chi mixing coordination and extraction benefits. Directionality increases over the interval as extraction accumulates (suppression hardens, theater grows, deliberative function atrophies) while the mechanism retains its coordination claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    auditory_architecture_determinism,
    'Is the suppression of the majority''s voice a technical necessity of the contio''s size, or a deliberate architectural choice?',
    'Archaeological and literary analysis of Field of Mars acoustics; comparison with alternative assembly designs (Athenian Pnyx, later Roman theater acoustics); reconstruction of actual hearing ranges by position',
    'If technical: suppression is inherent to the scale and becomes a structural floor for the constraint''s ε. If deliberate: the suppression is engineered extraction, and the constraint reclassifies toward snare from perspectives that lack acoustic advantage. The difference separates ''immutable assembly limit'' from ''designed inequality.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(auditory_architecture_determinism, empirical, 'Whether acoustic suppression is architectural necessity or deliberate design choice').

omega_variable(
    contio_versus_comitia_actual_power,
    'Does the contio''s persuasion actually determine the vote outcome, or is it ceremonial cover for decisions made in the Senate and distributed via patron networks?',
    'Quantitative analysis of voting patterns: do propositions that fail in the contio''s stated reasoning ever pass the subsequent vote? Do votes correlate more strongly with oratorical content or with prior senatorial position and client-patron binding? Documentary evidence of magistrate instructions to crowd organizers.',
    'If contio persuasion determines outcomes: the constraint is genuinely mixed (coordination + extraction). If comitia vote follows senatorial decision regardless of contio: the contio is pure theater (piton reclassification from all perspectives), and suppression is irrelevant because nothing is actually being decided in the persuasion assembly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contio_versus_comitia_actual_power, empirical, 'Whether contio persuasion determines voting outcomes or is ceremonial cover for prior decisions').

omega_variable(
    tribunician_veto_as_anti_contio,
    'Does the tribunician veto function as a counter-contio: the tribune''s body-veto as a competing persuasion mechanism that circumvents the magistrate''s oratorical monopoly?',
    'Analysis of tribune vetoes and their stated justifications; comparison of effective influence between oratorical persuasion (magistrate in contio) and body-threat (tribune in street); frequency of vetoes against magnate-proposed measures after successful contio persuasion',
    'If veto is genuinely counter-persuasion: it represents a structural dual reading (tribunician_sacrosanctity constraint) with forecloses relation — the tribune''s alternate legitimacy claim (inviolable body standing for the oppressed) directly contradicts the contio''s (magistrate''s oratory standing for the collective). If veto is merely obstruction: the two readings coexist but represent different leverage mechanisms rather than competing legitimacy grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunician_veto_as_anti_contio, conceptual, 'Whether tribunician veto functions as competing persuasion or mere obstruction').

omega_variable(
    reading_versus_natural_law_interpretation,
    'Is this constraint a reading of contested Roman law (the contio''s specific design and its suppressive architecture), or does it naturalize a particular interpretation as an immutable feature of public assembly?',
    'Textual analysis of sources claiming the contio as ''natural'' necessity vs. those treating it as designed institutional choice; comparison with alternatives the Republic explicitly rejected or later modified; examination of whether later constitutions (Principate, Byzantine) treat hearing/persuasion as required in the same form',
    'If reading of designed institution: the ''natural law'' mountain perspective is correctly identified as false summit. If naturalized interpretation: the constraint''s formulation itself instantiates the cover story it models. This omega documents the reflexive problem: the constraint story is itself a reading of contested authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_natural_law_interpretation, conceptual, 'Whether this constraint instantiates a reading or naturalizes one interpretation as immutable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(popular_assemblies_and_tribunate__contio_persuasion_arena, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(contio_theater_t0_foundation, popular_assemblies_and_tribunate__contio_persuasion_arena, theater_ratio, 0, 0.35).
narrative_ontology:measurement(contio_theater_t100_degradation, popular_assemblies_and_tribunate__contio_persuasion_arena, theater_ratio, 100, 0.48).
narrative_ontology:measurement(contio_theater_t200_attenuation, popular_assemblies_and_tribunate__contio_persuasion_arena, theater_ratio, 200, 0.62).

% Extraction over time
narrative_ontology:measurement(contio_extract_t0_foundation, popular_assemblies_and_tribunate__contio_persuasion_arena, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(contio_extract_t100_accumulation, popular_assemblies_and_tribunate__contio_persuasion_arena, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(contio_extract_t200_consolidation, popular_assemblies_and_tribunate__contio_persuasion_arena, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(contio_suppress_t0_baseline, popular_assemblies_and_tribunate__contio_persuasion_arena, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(contio_suppress_t100_entrenchment, popular_assemblies_and_tribunate__contio_persuasion_arena, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(contio_suppress_t200_hardening, popular_assemblies_and_tribunate__contio_persuasion_arena, suppression_requirement, 200, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(popular_assemblies_and_tribunate__contio_persuasion_arena, enforcement_mechanism).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__contio_persuasion_arena, comitia_centuriata_timocracy).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__contio_persuasion_arena, plebiscite_force_of_law).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__contio_persuasion_arena, tribunician_sacrosanctity).

% DUAL FORMULATION NOTE:
% The contio_persuasion_arena reading is one of four competing instantiations of the popular_assemblies_and_tribunate kernel. All four stories share the same underlying Roman political structure but decompose different claims about legitimate power and its mechanisms. The contio reading emphasizes hearing and oratorical persuasion; the centuriate reading emphasizes voting structure and property class; the plebiscite reading emphasizes direct plebeian force bypassing magistrate mediation; the tribunician reading emphasizes the tribune's inviolable body as the mechanism of veto. Each has distinct ε: contio (0.52, tangled rope mix of coordination and suppression), centuriate (lower extractiveness if property voting is treated as coordination, higher if as ossified inequality), plebiscite (higher extractiveness if seen as breaking elite monopoly, or tangled rope if plebeian council reproduces oligarchy), tribunician (moderate extraction via monopoly on veto voice). Network edges show that changes in one reading's structural condition affect all siblings: e.g., if the tribunician veto's frequency rises, the contio's actual deliberative function falls (theta-link to siblin: the tribune's exit option preempts contio persuasion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
