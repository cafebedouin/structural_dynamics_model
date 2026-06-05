% ============================================================================
% CONSTRAINT STORY: soviet_constitution_1936__rights_catalog_facade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_soviet_constitution_1936__rights_catalog_facade, []).

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
 *   constraint_id: soviet_constitution_1936__rights_catalog_facade
 *   human_readable: Stalin Constitution Rights Catalog Facade (1936)
 *   domain: political/historical
 *
 * SUMMARY:
 *   The 1936 Stalin Constitution presented the world's most comprehensive
 *   rights catalog at the precise historical moment when their suppression
 *   was most complete and most systematic. The Constitution enumerated rights
 *   to work, rest, education, healthcare, housing, political participation,
 *   and expression with extraordinary specificity and guarantee — even as the
 *   Great Purge accelerated and the NKVD terror apparatus operated entirely
 *   outside constitutional constraint. This constraint instantiates a
 *   readings-based kernel contest: the rights-catalog-facade reading
 *   interprets the enumeration as a deliberate legitimacy mechanism whose
 *   primary function is to conceal suppression machinery from international
 *   observers and from Soviet citizens who wished to believe the text. The
 *   enumeration is not a failed attempt to protect rights; it is a successful
 *   extraction mechanism that captures legitimacy while suppressing any
 *   invocation of the enumerated rights. The facade operates through
 *   precision: the more detailed the enumeration, the more credible the
 *   appearance of protection, and the more obvious the citizens'
 *   powerlessness when they discover that naming a right in the text does not
 *   constrain the apparatus operating outside the text.
 *
 * KEY AGENTS:
 *   - Soviet Regime (Institutional/Arbitrage): Primary beneficiary — captures international legitimacy and manages internal compliance through promise-theater
 *   - Soviet Citizens (Powerless/Trapped): Primary victims — read the text as genuine protection; discover suppression has no constitutional constraint
 *   - Enumerated Rights Claimants (Powerless/Trapped): Victim subset — attempt to invoke listed rights and face NKVD consequences
 *   - Party Functionaries (Moderate/Constrained): Secondary victims and apparatus operators — navigate between the text they must cite and the apparatus logic that governs real action
 *   - International Observers (Moderate/Mobile): Secondary beneficiaries — the rights catalog influences Western perception of Soviet legitimacy and regime recognition
 *   - Analytical Observer (Analytical/Analytical): Detects the false-summit risk — risks naturalizing the contradiction as immutable totalitarian law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(soviet_constitution_1936__rights_catalog_facade, 0.68).
domain_priors:suppression_score(soviet_constitution_1936__rights_catalog_facade, 0.82).
domain_priors:theater_ratio(soviet_constitution_1936__rights_catalog_facade, 0.91).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(soviet_constitution_1936__rights_catalog_facade, extractiveness, 0.68).
narrative_ontology:constraint_metric(soviet_constitution_1936__rights_catalog_facade, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(soviet_constitution_1936__rights_catalog_facade, theater_ratio, 0.91).

% --- Constraint claim ---
narrative_ontology:constraint_claim(soviet_constitution_1936__rights_catalog_facade, snare).
narrative_ontology:human_readable(soviet_constitution_1936__rights_catalog_facade, "Stalin Constitution Rights Catalog Facade (1936)").
narrative_ontology:topic_domain(soviet_constitution_1936__rights_catalog_facade, "political/historical").

domain_priors:requires_active_enforcement(soviet_constitution_1936__rights_catalog_facade).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(soviet_constitution_1936__rights_catalog_facade, '1cb91149-63ba-4568-9dfd-3e4de30dd921').
narrative_ontology:cs_kernel_codification('1cb91149-63ba-4568-9dfd-3e4de30dd921', fixed_text).
narrative_ontology:cs_authority_grounding('1cb91149-63ba-4568-9dfd-3e4de30dd921', extraction).
narrative_ontology:cs_reading_relation('1cb91149-63ba-4568-9dfd-3e4de30dd921', soviet_constitution_1936__federal_fiction, coexists_with).
narrative_ontology:cs_reading_relation('1cb91149-63ba-4568-9dfd-3e4de30dd921', soviet_constitution_1936__party_state_duality, coexists_with).
narrative_ontology:cs_reading_relation('1cb91149-63ba-4568-9dfd-3e4de30dd921', soviet_constitution_1936__terror_coincidence, coexists_with).
narrative_ontology:cs_axiom('1cb91149-63ba-4568-9dfd-3e4de30dd921', foundational, enumerated_rights_constrain_state_apparatus).
narrative_ontology:cs_axiom_status(enumerated_rights_constrain_state_apparatus, holdable).
narrative_ontology:cs_axiom_grounding('1cb91149-63ba-4568-9dfd-3e4de30dd921', enumerated_rights_constrain_state_apparatus, deontological).
narrative_ontology:cs_axiom('1cb91149-63ba-4568-9dfd-3e4de30dd921', foundational, legitimacy_extraction_through_textual_promise).
narrative_ontology:cs_axiom_status(legitimacy_extraction_through_textual_promise, holdable).
narrative_ontology:cs_axiom_grounding('1cb91149-63ba-4568-9dfd-3e4de30dd921', legitimacy_extraction_through_textual_promise, empirically_contingent).
narrative_ontology:cs_reference_frame('1cb91149-63ba-4568-9dfd-3e4de30dd921', constitutional_rights_as_genuine_constraint).
narrative_ontology:cs_drift_state('1cb91149-63ba-4568-9dfd-3e4de30dd921', terror_intensification_1936_1938, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('1cb91149-63ba-4568-9dfd-3e4de30dd921', '').
narrative_ontology:cs_kernel_id(soviet_constitution_1936__rights_catalog_facade, soviet_constitution_1936).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(soviet_constitution_1936__rights_catalog_facade, soviet_regime_legitimacy).
narrative_ontology:constraint_beneficiary(soviet_constitution_1936__rights_catalog_facade, party_leadership).
narrative_ontology:constraint_victim(soviet_constitution_1936__rights_catalog_facade, soviet_citizens).
narrative_ontology:constraint_victim(soviet_constitution_1936__rights_catalog_facade, enumerated_rights_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOVIET CITIZEN (SNARE) — Powerless, trapped, reads enumerated rights as genuine protections. The facade offers no exit: the text cannot be invoked without fatal consequences. Maximally targeted for extraction. The regime captures the benefit of apparent rights-protection while suppressing any mechanism to enforce it. No coordination function — pure extraction hidden behind textual generosity.
constraint_indexing:constraint_classification(soviet_constitution_1936__rights_catalog_facade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PARTY FUNCTIONARY (SNARE) — Moderate power within the apparatus, constrained exit (purge risk, disloyalty consequences). Experiences the rights catalog as theater masking the real operation (party discipline, cadre selection, terror quotas). The enumerated rights establish no internal constraint on party action. Extraction: loss of genuine agency, subordination to apparatus logic. High suppression of alternative interpretations.
constraint_indexing:constraint_classification(soviet_constitution_1936__rights_catalog_facade, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVIET REGIME / INTERNATIONAL LEGITIMACY (ROPE) — Institutional beneficiary with arbitrage exit (can discard the text if needed). The rights catalog solves a coordination problem: it presents Soviet socialism as rights-protective to a world audience skeptical of revolutionary claims. The enumeration is precise and generous, generating international credibility. The regime coordinates with international skepticism by textual appearance. From this perspective, the constraint is nearly pure coordination (rope) — the text performs legitimation work that benefits the regime.
constraint_indexing:constraint_classification(soviet_constitution_1936__rights_catalog_facade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL MEMORY / LONG DURATION (PITON) — Over decades, the 1936 Constitution persisted as formal law despite its entire operative logic being externalized into party machinery and secret police. The text maintained performative force: schoolchildren memorized it, officials cited it, international observers cited its guarantees. The apparatus operated outside and against it. Theater ratio extremely high (0.91): the constraint persists through institutional inertia and ceremonial force, not functional enforcement. The regime continues to benefit from legitimacy while suppression machinery operates from outside the text.
constraint_indexing:constraint_classification(soviet_constitution_1936__rights_catalog_facade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the contradiction between enumerated rights and suppression machinery might appear as an immutable feature of totalitarian systems: whenever rights are declared in a totalitarian context, their negation is inevitable (the suppression apparatus must prevent them from being invoked). This perspective sees the rights-catalog facade as a natural law of authoritarian governance. However, the base properties contradict the mountain classification — structural data shows a regime deliberately constructing the contradiction for external legitimacy purposes, not a natural inevitability. This is a false-summit candidate.
constraint_indexing:constraint_classification(soviet_constitution_1936__rights_catalog_facade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soviet_constitution_1936__rights_catalog_facade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(soviet_constitution_1936__rights_catalog_facade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soviet_constitution_1936__rights_catalog_facade, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(soviet_constitution_1936__rights_catalog_facade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(soviet_constitution_1936__rights_catalog_facade, TR),
    TR >= 0.70.

:- end_tests(soviet_constitution_1936__rights_catalog_facade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The regime extracts legitimacy benefit while suppressing the mechanism by which rights could be enforced. The extraction is not direct economic coercion (citizens are not paying a material price for rights they fail to exercise) but rather legitimacy theft: the regime captures the credibility of rights-protective governance while retaining absolute power to suppress invocation. The theater ratio (0.91) indicates that the constraint's operation is nearly entirely performative — the rights enumeration is text designed to perform legitimacy. The extractiveness rises over the interval as the suppression apparatus intensifies while the text remains unchanged, widening the gap between enumerated protection and actual suppression. Suppression (0.82): Very high. The NKVD terror apparatus is the enforcement mechanism, operating outside constitutional constraint. Any citizen attempting to invoke enumerated rights faces arrest, torture, execution. Suppression is not merely social stigma or economic penalty — it is total criminalization of rights-exercise. The suppression rises as the purges intensify, creating the contradiction between guarantee and negation at maximum intensity.
 *
 * PERSPECTIVAL GAP:
 *   The regime (institutional/arbitrage) sees the rights catalog as coordination: textual generosity solves the legitimacy problem with hostile international observers and restless internal audiences. The text is a successful solution — it presents socialism as rights-protective and sustains regime credibility. Citizens (powerless/trapped) see the same text as false promise: the enumeration establishes rights they cannot invoke without fatal consequences. The text is deception — it announces protection while the apparatus forbids its use. These perspectives are not differences in interpretation; they are differences in structural position. The regime benefits; citizens are harmed. The constraint looks like pure coordination from one angle (rope legitimacy solution) and pure extraction from another (snare suppression under false promise). The analytical observer risks collapsing this perspectival gap by treating the contradiction as a natural law of totalitarian systems (mountain) rather than a deliberately engineered extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (Soviet regime) experiences the constraint as coordination with international legitimacy-seeking. The regime's exit option is arbitrage — they can discard or modify the text if it no longer serves legitimacy purposes. This produces low directionality (d ≈ 0.15-0.20) and negative effective extractiveness from the regime's perspective. The victims (Soviet citizens) experience the constraint as trapped powerlessness — they read the text as offering protection that the apparatus forbids them to invoke. This produces high directionality (d ≈ 0.90-0.95) and maximum effective extractiveness from the citizen perspective. The perspectival gap is extreme: the same constraint appears as Rope (beneficiary solving legitimacy problem) and Snare (victim experiencing suppression under false promise). The piton perspective captures the institutional durability: the text persists through ceremonial force despite its operative logic being externalized into the NKVD apparatus, generating high theater ratio and inertial classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the regime's experience (rope: successful legitimacy coordination) from the citizen's experience (snare: suppression under false promise) and recognizing that BOTH are structurally correct from their respective positions. The constraint does NOT reduce to a single true classification; it exhibits structural properties of both Rope and Snare depending on who experiences it. The piton classification captures an additional dynamic: the text's persistence through institutional inertia despite operational logic being externalized. The false-summit risk is that an observer naturalizes the contradiction as inherent to totalitarian systems, erasing the regime's agency in designing the facade. The resolution is to maintain the perspectival structure: this constraint IS a snare to citizens, a rope to the regime, and a piton to the institutional archive that preserves the text long after its legitimacy function has degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_inevitable_contradiction,
    'Is the contradiction between the rights catalog and suppression machinery a deliberate regime strategy, or an inevitable structural feature of totalitarian systems?',
    'Comparative analysis: how totalitarian regimes differently presented or hid rights claims (Nazi Weimar legacy, Chinese constitutionalism, North Korean legalism). If the contradiction is regime-engineered, identify decision points where alternatives were available.',
    'If intentional strategy: the constraint is a Snare engineered for legitimacy extraction. If inevitable: the mountain perspective holds, and the regime was operating under structural necessity rather than choice. Classification outcome changes from Snare (extraction with deception) to Mountain (immutable property).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentional_vs_inevitable_contradiction, empirical, 'Whether rights-suppression contradiction is deliberate strategy or inevitable').

omega_variable(
    international_credibility_necessity,
    'Did the 1936 rights catalog generate measurable international credibility that directly sustained Soviet foreign policy objectives or trade relationships?',
    'Historical analysis of Western diplomatic responses, recognition decisions, trade agreements, and ideological recruitment to Soviet communism between 1936 and 1939. Correlation between rights-catalog prominence in Soviet external messaging and diplomatic gains.',
    'If necessary: the rope perspective (regime legitimation as genuine coordination solving international skepticism) is empirically grounded. If unnecessary: the text was surplus to regime needs, suggesting internal audience (Soviet citizens) was the primary target, shifting analysis toward extraction from trapped citizens rather than coordination with international observers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_credibility_necessity, empirical, 'Whether rights catalog sustained Soviet international credibility').

omega_variable(
    suppression_concealment_mechanism,
    'Did Soviet regime deliberately design the rights enumeration to conceal suppression machinery, or did the enumeration precede and necessitate the suppression apparatus?',
    'Chronological analysis: when was the rights text drafted relative to when NKVD operations were intensified? Did the text create the perceived need to suppress invocation? Or did the apparatus preexist and the text was designed post-hoc to conceal it?',
    'If text-first: the rights catalog was the regime advertising to itself and the world that it was rights-protective, requiring suppression apparatus to prevent testing those claims. If apparatus-first: suppression machinery already existed and the text was added as cover. The causal direction determines whether the constraint is primarily about internal legitimation or external deception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_concealment_mechanism, empirical, 'Causal order between rights enumeration and suppression apparatus').

omega_variable(
    citizen_comprehension_of_facade,
    'To what extent did Soviet citizens recognize the rights catalog as facade rather than genuine protection?',
    'Analysis of Soviet memoirs, samizdat, private correspondence, and later oral histories. Did citizens cite the rights text as legitimate basis for claims? Did they understand enumeration as theater?',
    'If recognized as facade: citizens experienced extraction directly (false promise, cynicism). If not recognized: extraction operated through false hope, and the citizen perspective experiences betrayal rather than transparent suppression. The victim set''s understanding of the constraint changes its operational mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_comprehension_of_facade, empirical, 'Soviet citizen awareness of rights-suppression contradiction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(soviet_constitution_1936__rights_catalog_facade, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soviet_rights_tr_t0, soviet_constitution_1936__rights_catalog_facade, theater_ratio, 0, 0.85).
narrative_ontology:measurement(soviet_rights_tr_t2, soviet_constitution_1936__rights_catalog_facade, theater_ratio, 2, 0.89).
narrative_ontology:measurement(soviet_rights_tr_t5, soviet_constitution_1936__rights_catalog_facade, theater_ratio, 5, 0.91).

% Extraction over time
narrative_ontology:measurement(soviet_rights_be_t0, soviet_constitution_1936__rights_catalog_facade, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(soviet_rights_be_t2, soviet_constitution_1936__rights_catalog_facade, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(soviet_rights_be_t5, soviet_constitution_1936__rights_catalog_facade, base_extractiveness, 5, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(soviet_rights_su_t0, soviet_constitution_1936__rights_catalog_facade, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(soviet_rights_su_t2, soviet_constitution_1936__rights_catalog_facade, suppression_requirement, 2, 0.78).
narrative_ontology:measurement(soviet_rights_su_t5, soviet_constitution_1936__rights_catalog_facade, suppression_requirement, 5, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(soviet_constitution_1936__rights_catalog_facade, information_standard).
narrative_ontology:affects_constraint(soviet_constitution_1936__rights_catalog_facade, soviet_constitution_1936__federal_fiction).
narrative_ontology:affects_constraint(soviet_constitution_1936__rights_catalog_facade, soviet_constitution_1936__party_state_duality).
narrative_ontology:affects_constraint(soviet_constitution_1936__rights_catalog_facade, soviet_constitution_1936__terror_coincidence).

% DUAL FORMULATION NOTE:
% The 1936 Constitution kernel decomposes into four constraint stories, each examining a distinct structural reading of the same text. rights_catalog_facade focuses on the rights enumeration as legitimacy extraction and suppression concealment (ε=0.68, Snare). federal_fiction focuses on federalism as institutional facade (separate ε analysis). party_state_duality focuses on state/apparatus distinction in the text structure (separate ε analysis). terror_coincidence focuses on temporal coincidence of rights-richness and suppression-intensity (separate ε analysis). All four readings describe the same kernel but instantiate different constraints with different beneficiary/victim structures. They are linked by network affiliation rather than causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(soviet_constitution_1936__rights_catalog_facade, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
