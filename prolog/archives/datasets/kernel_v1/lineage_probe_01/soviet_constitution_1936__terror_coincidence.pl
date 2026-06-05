% ============================================================================
% CONSTRAINT STORY: soviet_constitution_1936__terror_coincidence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soviet_constitution_1936__terror_coincidence, []).

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
 *   constraint_id: soviet_constitution_1936__terror_coincidence
 *   human_readable: Soviet Constitution of 1936: Terror Coincidence Reading
 *   domain: political/historical
 *
 * SUMMARY:
 *   This reading instantiates the 'terror coincidence' interpretation of the
 *   Soviet Constitution of 1936: the most rights-rich constitutional text and
 *   the most rightless year arrived simultaneously. In December 1936, the
 *   USSR adopted by acclamation a constitution that enumerated universal
 *   rights — work, rest, education, speech, assembly — with unprecedented
 *   generosity on paper, while the Great Purge of 1937-1938 was already
 *   lengthening the execution lists and building the Gulag apparatus. The
 *   constraint here is not the constitution as formal text, but the spectacle
 *   created by the coincidence: the state guaranteed rights precisely as it
 *   systematized their denial, creating a extraction mechanism that operated
 *   through the simultaneous enactment and negation of legal protections. The
 *   beneficiary is the Party apparatus and the spectacle of socialist
 *   legality itself — legitimacy gained through the appearance of
 *   constitutionalism, while actual power flowed through the security
 *   apparatus operating entirely outside the constitution's formal structure.
 *   The victims are the purge targets of 1937, the Soviet population subject
 *   to arrest and terror, and the constitutional guarantees themselves
 *   rendered hollow by their simultaneous proclamation and violation. This
 *   reading is one of four interpretations of the contested 1936 kernel (the
 *   others being federal_fiction, party_state_duality, and
 *   rights_catalog_facade). Each reading emphasizes a different structural
 *   dimension — federalism, party-state relationship, rights catalog, or
 *   terror timing — but all four are live positions that different analysts
 *   maintain.
 *
 * KEY AGENTS:
 *   - Party Apparatus (Politburo, NKVD leadership): Primary beneficiary (organized/constrained) — controls the constitution's deployment as a legitimacy tool while maintaining unconstrained power through the terror apparatus; benefits from the democratic spectacle while operating outside its formal constraints.
 *   - Purge Targets (1937-1938): Primary victims (powerless/trapped) — subjected to arrest and execution under laws the constitution nominally protected them from; maximum extraction under the new legal regime; no exit available.
 *   - Soviet Population (Citizens acclaiming the constitution): Secondary victims (moderate/identity_locked) — participated in the constitutional acclamation while subject to systematic terror; structurally mobile but identity-locked to the Soviet state project; could not escape without abandoning Soviet identity.
 *   - Spectacle of Socialist Legality: Institutional beneficiary (institutional/arbitrage) — the performance of democratic constitutionalism as a propaganda tool; gains legitimacy value while constraining nothing; pure institutional theater.
 *   - International Observers (Western governments, Comintern allies): Secondary beneficiary (analytical/analytical) — the constitution's text provided evidence of Soviet socialism's commitment to universal rights; the spectacle obscured the simultaneous terror apparatus from distant observers; international credibility was extracted at the cost of the Soviet population's actual rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soviet_constitution_1936__terror_coincidence, 0.89).
domain_priors:suppression_score(soviet_constitution_1936__terror_coincidence, 0.92).
domain_priors:theater_ratio(soviet_constitution_1936__terror_coincidence, 0.96).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soviet_constitution_1936__terror_coincidence, extractiveness, 0.89).
narrative_ontology:constraint_metric(soviet_constitution_1936__terror_coincidence, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(soviet_constitution_1936__terror_coincidence, theater_ratio, 0.96).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soviet_constitution_1936__terror_coincidence, snare).
narrative_ontology:human_readable(soviet_constitution_1936__terror_coincidence, "Soviet Constitution of 1936: Terror Coincidence Reading").
narrative_ontology:topic_domain(soviet_constitution_1936__terror_coincidence, "political/historical").

domain_priors:requires_active_enforcement(soviet_constitution_1936__terror_coincidence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(soviet_constitution_1936__terror_coincidence, '96416579-e3d0-43b1-9365-0e1040893243').
narrative_ontology:cs_kernel_codification('96416579-e3d0-43b1-9365-0e1040893243', formalized).
narrative_ontology:cs_authority_grounding('96416579-e3d0-43b1-9365-0e1040893243', extraction).
narrative_ontology:cs_interpretation_layer_present('96416579-e3d0-43b1-9365-0e1040893243').
narrative_ontology:cs_reading_relation('96416579-e3d0-43b1-9365-0e1040893243', soviet_constitution_1936__federal_fiction, coexists_with).
narrative_ontology:cs_reading_relation('96416579-e3d0-43b1-9365-0e1040893243', soviet_constitution_1936__party_state_duality, coexists_with).
narrative_ontology:cs_reading_relation('96416579-e3d0-43b1-9365-0e1040893243', soviet_constitution_1936__rights_catalog_facade, coexists_with).
narrative_ontology:cs_axiom('96416579-e3d0-43b1-9365-0e1040893243', foundational, rights_guarantee_simultaneous_negation).
narrative_ontology:cs_axiom_status(rights_guarantee_simultaneous_negation, holdable).
narrative_ontology:cs_axiom_grounding('96416579-e3d0-43b1-9365-0e1040893243', rights_guarantee_simultaneous_negation, empirically_contingent).
narrative_ontology:cs_axiom('96416579-e3d0-43b1-9365-0e1040893243', foundational, legitimacy_extraction_via_spectacle).
narrative_ontology:cs_axiom_status(legitimacy_extraction_via_spectacle, holdable).
narrative_ontology:cs_axiom_grounding('96416579-e3d0-43b1-9365-0e1040893243', legitimacy_extraction_via_spectacle, instrumental).
narrative_ontology:cs_reference_frame('96416579-e3d0-43b1-9365-0e1040893243', socialist_legality_through_constitutionalism).
narrative_ontology:cs_drift_state('96416579-e3d0-43b1-9365-0e1040893243', post_1937_purge_peak, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('96416579-e3d0-43b1-9365-0e1040893243', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(soviet_constitution_1936__terror_coincidence, soviet_constitution_1936).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soviet_constitution_1936__terror_coincidence, spectacle_of_socialist_legality).
narrative_ontology:constraint_beneficiary(soviet_constitution_1936__terror_coincidence, party_apparatus_discretion).
narrative_ontology:constraint_victim(soviet_constitution_1936__terror_coincidence, purge_targets_1937).
narrative_ontology:constraint_victim(soviet_constitution_1936__terror_coincidence, soviet_population_subject_to_terror).
narrative_ontology:constraint_victim(soviet_constitution_1936__terror_coincidence, constitutional_guarantees_as_lived_reality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONDEMNED (SNARE) — Those on the purge lists in 1937 experienced maximum extraction under the new constitution. The document guaranteed them speech, assembly, work, rest, and education precisely as these were systematized denied. No exit available: arrested under laws the constitution nominally protected them from; tried under codes the constitution claimed to guarantee. Maximum experienced extractiveness — extraction is the constraint's entire function from this position.
constraint_indexing:constraint_classification(soviet_constitution_1936__terror_coincidence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOVIET POPULATION / ACCLAMATION PARTICIPANTS (SNARE) — The population voted by acclamation for the rights-rich text in December 1936 while the purge lists lengthened. They were simultaneously guaranteed rights and subject to systematic denial of those rights. The identity_locked exit reflects that Soviet identity was constituted through participation in the socialist state project — rejecting the constitution would require abandoning identification as a Soviet citizen, not merely changing policy. Suppression is maximal: structural barriers (arrest, execution, internal exile) combined with internalized commitment to the state they were acclaiming.
constraint_indexing:constraint_classification(soviet_constitution_1936__terror_coincidence, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: PARTY APPARATUS / DISCRETIONARY AUTHORITY (SNARE) — The apparatus that drafted and controlled the constitution saw the text as a tool for extracting legitimacy while preserving unconstrained power. The constitution guaranteed rights from without; the apparatus operated from outside the text's formal structure, unconstrained by the guarantees it proclaimed. Organized power with constrained exit — the apparatus was bound to the Soviet project but could restructure the constitution at will. Extraction flows toward this perspective: the apparatus benefits from the democratic spectacle while maintaining terror as the actual enforcement mechanism.
constraint_indexing:constraint_classification(soviet_constitution_1936__terror_coincidence, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SPECTACLE OF SOCIALIST LEGALITY (PITON) — The constitution itself functions as pure institutional theater at the civilizational scale. It performed socialism's commitment to universal rights for the world audience (the Comintern, Western sympathizers, international observers) while the actual Soviet state operated by decree, arrest, and execution. The text has degraded into performative ritual — maintained because the socialist state's legitimacy narrative requires the appearance of legality, but the actual mechanism of control is the terror apparatus operating entirely outside the constitution's formal structure. Theater ratio at maximum (0.96): the constitution is almost entirely performative content with zero functional constraint on state power.
constraint_indexing:constraint_classification(soviet_constitution_1936__terror_coincidence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the terror coincidence might be understood as following a natural law of totalitarianism: when absolute power requires legitimacy through universal rights language, the simultaneous guarantee and denial of those rights becomes structurally necessary. The state cannot operate without the legal fiction; the legal fiction cannot constrain the state. This creates an immutable paradox — a natural law of totalitarian structure. However, the base properties reveal this as a false summit: the 1936 constitution is not a law of totalitarianism but a contingent historical choice by a specific state to create this spectacle at this moment.
constraint_indexing:constraint_classification(soviet_constitution_1936__terror_coincidence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soviet_constitution_1936__terror_coincidence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soviet_constitution_1936__terror_coincidence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soviet_constitution_1936__terror_coincidence, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soviet_constitution_1936__terror_coincidence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soviet_constitution_1936__terror_coincidence, TR),
    TR >= 0.70.

:- end_tests(soviet_constitution_1936__terror_coincidence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.89): Very high, approaching total. The constraint extracts legitimacy from the state and terror-based control from the population. The constitution's rights catalog is simultaneously proclaimed and entirely negated in practice — the extraction consists of the state claiming credit for guarantees it has no intention of honoring. The high value reflects that the primary function of the constraint is to extract, not to coordinate or govern. The trajectory shows extractiveness rising sharply at constitutional adoption (0.78 → 0.89) and continuing to rise through the purge peak (0.91), indicating that as the terror intensified, the spectacle's extraction function became more pronounced. Suppression (0.92): Maximum. This reflects the total denial of alternatives — there is no legal redress for those persecuted under the new constitution, no mechanism to appeal to the rights it proclaims, no exit from the state's jurisdiction. The population is trapped by the state's borders and by their own identity fusion with the Soviet project. The suppression rises slightly from pre-constitutional baseline (0.85 → 0.92), indicating that the constitution itself created new suppressive mechanisms by establishing a legal framework that could be invoked against its own proclamations. Theater ratio (0.96): Nearly total. The constitution functions almost entirely as spectacle — the text's function is to perform socialism's commitment to universal rights for the world audience, not to constrain state power or protect citizens. The content of the text is almost entirely theatrical: rights proclaimed with precision as they were systematically negated; federal structure proclaimed with titles and borders while actual power remained centralized in the Party apparatus. The rise from pre-constitutional baseline (0.82 → 0.96) indicates that the adoption of the constitution transformed the Soviet state into a theater state — the regime's legitimacy narrative shifted from revolutionary necessity to democratic socialism, requiring the constitutional performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The powerless victim (condemned person) experiences it as pure snare — extraction with no coordination function, maximum suppression, maximum experienced χ. The organized beneficiary (Party apparatus) experiences it as rope — a coordination mechanism solving the problem of how to maintain absolute power while retaining international legitimacy; they see the constitution as functional to their governance goals, not extractive to themselves. The population sees it through identity_locked exit — they participated in the acclamation, constituting themselves as Soviet citizens through the act of affirming the constitution, while simultaneously subject to the terror it nominally prohibited; their perspective is snare (trapped, maximum extraction) but experienced as identity-affirming (they were constituting themselves as Soviets through the constitutional performance). The institutional theater perspective (piton) sees the constitution as degraded ritual — it persists because the socialist legitimacy narrative requires it, but the actual governance happens elsewhere. The analytical observer risks seeing a natural law (mountain) — that totalitarian states necessarily operate through simultaneous guarantee and denial of rights — but this is a false summit: the 1936 constitution represents a specific historical choice, not an inevitable law. The perspectival gaps reflect that the constraint's entire function is to create divergent realities for different observers: the international observer sees a rights-guaranteeing constitution; the victim sees its simultaneous negation; the beneficiary sees a tool that advances their power; the population sees their own identity reaffirmed while their actual rights are destroyed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures each agent's structural position in the constraint's extraction flow. The condemned (d ≈ 0.98) experience maximum targeting — they are the constraint's entire purpose, the victims upon whom the spectacle-extraction depends. The apparatus (d ≈ 0.08) experiences maximum beneficiary position — the constraint flows toward them; they capture all legitimacy value while bearing zero suppressive cost. The population (d ≈ 0.72) experiences mid-to-high targeting, derived from their victim status (purged, arrested) combined with identity_locked exit (they cannot leave without ceasing to be Soviet); the identity lock prevents them from exercising whatever structural mobility they might otherwise have, making them functionally equivalent to trapped agents despite theoretical mobility. The international observer (d ≈ 0.65) experiences moderate targeting — they bear the epistemic cost of being deceived by the spectacle; their understanding of Soviet governance is captured by the constitutional performance, making them unable to perceive the actual terror mechanism. The apparatus's beneficiary position is crucial: as the constraint's author and sole beneficiary, they derive d from the combination of benefits received (legitimacy extraction) and absence of constraints (they operate outside the constitution entirely). The schema's derivation chain computes these d values from beneficiary/victim declarations plus power level plus exit options; the algebra produces the observed differences without requiring explicit override.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint at ε=0.89 requires mandatrophy resolution — confirmation that the classification does not conflate extraction and coordination. This reading resolves mandatrophy by confirming that the constraint is pure snare with zero coordination function. The state did not solve a legitimate coordination problem by creating the constitution; it created a pure-extraction mechanism disguised as coordination. The 'coordination' that might exist (managing totalitarian control while maintaining international credibility) is a secondary effect — the constraint's primary function is to extract legitimacy while denying rights. The resolution confirms snare (pure extraction, minimal coordination) over tangled_rope (mixed extraction and coordination). The key evidence: the constitution constrains nothing; it coordinates nothing within the state apparatus itself (the Party apparatus operated entirely outside it); it serves only to legitimize the apparatus's absolute power. This is consistent with snare classification at maximum extractiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_coincidence,
    'Was the adoption of the rights-rich constitution in December 1936 deliberately timed to coincide with and obscure the escalating purges, or is the temporal coincidence functionally independent?',
    'Historical archival analysis of Politburo decisions: dating of constitution finalization vs. dating of purge list expansion; comparative analysis of international publicity campaigns for the constitution vs. timing of arrest waves; examination of internal Party communications discussing the strategic relationship between the constitutional text and the terror apparatus.',
    'If deliberate timing: the constraint is intentional extraction under cover of legality (snare confirmed, ε remains high). If independent: the constraint may reflect structural inevitability of totalitarian governance rather than strategic choice (mountain reading gains plausibility, though false-summit mechanism still applies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_coincidence, empirical, 'Whether the constitutional timing was deliberately coordinated with purge escalation').

omega_variable(
    rights_catalog_as_cover_story,
    'Did the enumeration of rights in the 1936 text function as a transparent cover story that all actors recognized, or did the spectacle successfully convince some portion of the Soviet population or international observers of genuine constitutional constraint?',
    'Analysis of contemporaneous Western responses to the constitution: did international observers treat it as genuine or performative? Survey of Soviet diaries, letters, and internal Party communications for evidence of belief or cynicism regarding the rights catalog. Comparative analysis of rights invocation in appeals, petitions, or defenses during the 1937-1938 purges: did defendants cite constitutional guarantees expecting enforcement, or only as rhetorical gesture?',
    'If transparent cover: extraction is pure snare with full knowledge (χ remains maximal). If partially successful spectacle: some population segments were captured by the legitimacy performance, creating a hybrid extraction mechanism (tangled_rope from some perspectives, snare from others). If widely believed: the constraint''s extraction mechanism depends on the epistemic capture of the audience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_catalog_as_cover_story, empirical, 'Whether the rights catalog functioned as transparent theater or captured audience belief').

omega_variable(
    alternative_reading_foreclosure,
    'Do the base properties of this reading (terror coincidence: maximum suppression, maximum theater, maximum extractiveness at the constitution''s debut) logically foreclose the federal_fiction and party_state_duality readings, or can all three coexist as different interpretations of the same structural fact?',
    'Logical analysis: the federal_fiction reading holds that the text''s federalism was genuine; the terror_coincidence reading holds that the text''s guarantees were simultaneous denial. Can a state genuinely guarantee federal structure while totally denying rights? Yes, structurally — federalism and terror are independent dimensions. The party_state_duality reading holds that the party operated outside the state; terror_coincidence holds that the text described a state while terror operated outside it — these describe the same structure. Conclusion: readings coexist rather than foreclose.',
    'If readings foreclose each other: only one reading is valid in any framework (rare). If readings coexist: all four readings of the 1936 kernel are simultaneously defensible, each describing a different structural dimension of the same text (most likely). The coexistence model confirms that this is a genuinely contested kernel, not a factual dispute resolvable by evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether this reading logically forecloses or coexists with sibling readings').

omega_variable(
    mandatrophy_extraction_totality,
    'At extractiveness 0.89, is the constraint''s function purely extractive (snare), or does the legitimacy spectacle itself constitute a genuine coordination service that the Soviet state was solving — i.e., the problem of how to maintain totalitarian control while retaining global credibility as a socialist state?',
    'Structural decomposition: if the constraint is snare (pure extraction, zero coordination function), the beneficiary (party apparatus) gains control while the victims (purge targets, population) lose rights. If the constraint is tangled_rope (mixed extraction + coordination), the beneficiary also solves the coordination problem of managing a totalitarian state while maintaining international socialist legitimacy. Evidence: did the constitution''s existence measurably improve the apparatus''s ability to govern? Did it prevent or delay Western intervention? Did it facilitate international alliances the USSR otherwise could not have secured? If yes to any: some coordination function exists beneath the extraction.',
    'If pure snare confirmed: the constraint''s sole function is to extract legitimacy while denying rights. If tangled_rope: the constraint solves a genuine (if evil) coordination problem while extracting from the victims. The difference affects classification, mandatrophy resolution, and the structure of omega variables needed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_extraction_totality, conceptual, 'Whether the constraint has any genuine coordination function or is pure extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soviet_constitution_1936__terror_coincidence, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov1936_theater_t0_pre_constitution, soviet_constitution_1936__terror_coincidence, theater_ratio, 0, 0.82).
narrative_ontology:measurement(sov1936_theater_t1_constitution_adopted, soviet_constitution_1936__terror_coincidence, theater_ratio, 1, 0.96).
narrative_ontology:measurement(sov1936_theater_t2_purge_peak_1937, soviet_constitution_1936__terror_coincidence, theater_ratio, 2, 0.97).

% Extraction over time
narrative_ontology:measurement(sov1936_extractiveness_t0_pre_constitution, soviet_constitution_1936__terror_coincidence, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(sov1936_extractiveness_t1_constitution_adopted, soviet_constitution_1936__terror_coincidence, base_extractiveness, 1, 0.89).
narrative_ontology:measurement(sov1936_extractiveness_t2_purge_peak_1937, soviet_constitution_1936__terror_coincidence, base_extractiveness, 2, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(sov1936_suppression_t0_pre_constitution, soviet_constitution_1936__terror_coincidence, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(sov1936_suppression_t1_constitution_adopted, soviet_constitution_1936__terror_coincidence, suppression_requirement, 1, 0.92).
narrative_ontology:measurement(sov1936_suppression_t2_purge_peak_1937, soviet_constitution_1936__terror_coincidence, suppression_requirement, 2, 0.94).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soviet_constitution_1936__terror_coincidence, identity_coordination).
narrative_ontology:affects_constraint(soviet_constitution_1936__terror_coincidence, soviet_constitution_1936__federal_fiction).
narrative_ontology:affects_constraint(soviet_constitution_1936__terror_coincidence, soviet_constitution_1936__party_state_duality).
narrative_ontology:affects_constraint(soviet_constitution_1936__terror_coincidence, soviet_constitution_1936__rights_catalog_facade).

% DUAL FORMULATION NOTE:
% The 1936 Soviet Constitution is a contested kernel with four structurally distinct readings. This file instantiates the terror_coincidence reading, emphasizing the temporal and functional coincidence of rights guarantee and rights negation. The federal_fiction, party_state_duality, and rights_catalog_facade readings describe alternative structural dimensions of the same constitutional text. All four readings coexist as live interpretations. They are linked via network.affects_constraints to indicate the constraint family. Each reading has its own ε value, its own beneficiary/victim structure, and its own perspectives reflecting the different structural dimensions emphasized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(soviet_constitution_1936__terror_coincidence, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
