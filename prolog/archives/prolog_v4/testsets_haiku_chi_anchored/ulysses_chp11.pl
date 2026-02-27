% ============================================================================
% CONSTRAINT STORY: ulysses_chp11
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp11, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp11
 *   human_readable: The Auditory Lure (Ormond Hotel)
 *   domain: social/artistic/biological
 *
 * SUMMARY:
 *   Chapter 11 of James Joyce's Ulysses (the Sirens episode) structures the
 *   Ormond Hotel bar as a complex auditory system in which music,
 *   conversation, laughter, clinking glasses, and fragmented narrative all
 *   merge into a unified acoustic field that entraps patrons in sensory
 *   stimulation. Leopold Bloom and other characters are trapped in the bar
 *   not by coercion but by the irresistible pull of sound itself — a modern
 *   reimagining of the classical Sirens myth. The constraint operates at
 *   multiple levels simultaneously: as a biological inevitability (human
 *   auditory attention cannot escape novel stimuli), as a commercial
 *   mechanism (proprietors benefit from patrons who linger intoxicated), as
 *   an artistic technique (Joyce uses acoustic overload to model
 *   consciousness itself), and as a social norm (leaving the bar requires
 *   social friction). The auditory lure exhibits asymmetric extraction: the
 *   bar proprietors and musicians benefit from the constraint, while patrons
 *   and linguistic meaning (the possibility of coherent communication) bear
 *   the cost. Yet the constraint also functions as coordination: the shared
 *   acoustic field creates community, enables commerce, and generates the
 *   artistic material that Joyce transforms into modernist narrative.
 *
 * KEY AGENTS:
 *   - Entranced Patrons: Primary victims (powerless/trapped) — experience auditory capture leading to continued consumption and suppression of rational exit
 *   - Bar Proprietors: Primary beneficiaries (institutional/arbitrage) — benefit from extended patronage and increased consumption driven by acoustic entrapment
 *   - Musician Performers: Secondary actors (moderate/constrained) — constrained by venue economics but also benefit from the coordinated acoustic space
 *   - Linguistic Meaning: Abstract victim (powerless/trapped) — communication itself is suppressed as language dissolves into pure sound
 *   - Modernist Artistic Consciousness: Organized beneficiary (organized/constrained) — uses the auditory lure as a tool for representing consciousness in literary form
 *   - Dublin Cultural Institution: Institutional curator (powerful/mobile) — maintains the Ormond Hotel mythology through literary and cultural memory
 *   - Analytical Observer: Universal observer (analytical/analytical) — risks naturalizing commercial extraction as biological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp11, 0.38).
domain_priors:suppression_score(ulysses_chp11, 0.52).
domain_priors:theater_ratio(ulysses_chp11, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp11, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp11, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ulysses_chp11, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp11, tangled_rope).
narrative_ontology:human_readable(ulysses_chp11, "The Auditory Lure (Ormond Hotel)").
narrative_ontology:topic_domain(ulysses_chp11, "social/artistic/biological").

domain_priors:requires_active_enforcement(ulysses_chp11).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp11, musical_performers).
narrative_ontology:constraint_beneficiary(ulysses_chp11, bar_proprietors).
narrative_ontology:constraint_beneficiary(ulysses_chp11, artistic_consciousness).
narrative_ontology:constraint_victim(ulysses_chp11, patron_sobriety).
narrative_ontology:constraint_victim(ulysses_chp11, rational_agency).
narrative_ontology:constraint_victim(ulysses_chp11, linguistic_meaning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENTRANCED PATRON (SNARE) — Trapped in the bar's acoustic field. Music and noise synergistically suppress rational decision-making; exits are constrained by social norms and intoxication. Cannot leave without social friction. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.41.
constraint_indexing:constraint_classification(ulysses_chp11, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BAR PROPRIETOR (ROPE) — Benefits from coordinated sound design that keeps patrons present and consuming. Experiences auditory lure as a coordination mechanism: music and noise solve the problem of sustained patronage. d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(ulysses_chp11, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE MUSICIAN PERFORMER (TANGLED ROPE) — Constrained by the need for audience and venue. Benefits from the bar's coordination mechanism (venue, amplification, social setting) but also forced to participate in extraction logic: music becomes product, meaning becomes consumable. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.19.
constraint_indexing:constraint_classification(ulysses_chp11, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: THE CULTURAL INSTITUTION (PITON) — The Ormond Hotel's auditory culture is maintained as a vestigial cultural artifact: it persists through literary reference (Joyce's chapter) and tourist mythology rather than genuine structural necessity. The actual function (keeping patrons present) has been replaced by mass media. theater_ratio=0.68 (≥0.70 threshold marginal). Institutional inertia preserves the performance of 'authentic Dublin bar experience' despite declining functional extraction.
constraint_indexing:constraint_classification(ulysses_chp11, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE MODERNIST ARTISTIC MOVEMENT (SCAFFOLD) — Views the auditory lure as a temporary pedagogical constraint: the subjective experience of being trapped in sensory stimulation is a tool for artistic consciousness-raising. The constraint has a sunset: once modernist techniques (stream-of-consciousness narrative, radical juxtaposition) mature, the need for the constraint diminishes. d≈0.38, f(d)≈0.36, σ=1.1 → χ≈0.14.
constraint_indexing:constraint_classification(ulysses_chp11, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / BIOLOGICAL VIEW (MOUNTAIN) — From a universal/civilizational perspective, auditory capture is an immutable property of human neurobiology: the human auditory system cannot selectively filter complex acoustic environments; attention is involuntarily drawn to novel stimuli (novelty bias, cocktail party effect). However, the structural data (ε=0.38, suppression=0.52, theater=0.68) reveals this as a false summit: the 'inevitable' framing naturalizes what is actually a contingent design choice (bar proprietors deliberately engineer soundscapes).
constraint_indexing:constraint_classification(ulysses_chp11, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp11_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp11, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp11, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp11, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp11_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from patrons in the form of extended consumption and suppressed agency. But the extraction is not total because patrons are also receiving genuine goods (music, company, intoxication) — the constraint is hybrid, not purely extractive. The value reflects that the bar proprietors are capturing some legitimate economic benefit from coordination, but also layering additional extraction through acoustic design. Suppression (0.52): Moderate-high. Auditory attention is involuntarily captured (neurobiological), but patrons retain some agency — they can leave (with social cost), refuse to drink further, or shift attention. The suppression is substantial but not complete. Theater ratio (0.68): High. The Ormond Hotel's auditory culture is partly functional (keeping patrons present) and partly performative (enacting 'authentic Dublin bar experience'). The performative content has increased over the interval as the venue has become a literary tourist destination rather than a working bar. The theater ratio just misses the piton threshold (0.70), but the piton perspective is included because the institutional/cultural view sees the auditory lure as substantially degraded — maintained by inertia rather than necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival variation. The entranced patron sees snare (trapped in sensory stimulation). The bar proprietor sees rope (coordinated acoustic space enabling commerce). The musician sees tangled rope (benefiting from venue but also constrained by market logic). The cultural institution sees piton (the auditory lure persists through literary reference and tourist mythology). The modernist artistic consciousness sees scaffold (the constraint is a temporary pedagogical tool with a sunset when modernist techniques mature). The analytical observer risks seeing mountain (biological inevitability of auditory capture) but the structural data reveals this as a false summit. The perspectival gaps reveal that the constraint's type is not intrinsic to its structure but dependent on the observer's exit options and beneficiary status.
 *
 * DIRECTIONALITY LOGIC:
 *   Entranced Patrons: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no exit options. Bar Proprietors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit at will. Musician Performers: Mixed + constrained → d≈0.50, f(d)≈0.65. Moderate position; benefit from venue coordination but constrained by market. Modernist Consciousness: Organized + constrained → d≈0.38, f(d)≈0.36. Low-to-moderate extraction; organization provides agency. Dublin Institution: Powerful + mobile → d≈0.15, f(d)≈-0.01. Beneficiary from cultural capital; mobile in decision-making. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Observer position that risks naturalizing constraint as inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   MODERNIST RESOLUTION: The auditory lure resolves the mandatrophy through the insight that the constraint is simultaneously a coordination mechanism (sharing acoustic space) and an extraction mechanism (sublimating patron agency into consumption). The modernist artistic consciousness (scaffold perspective) explicitly frames the constraint as a temporary pedagogical tool: the auditory overload teaches consciousness how to navigate complexity, fragmentation, and multiplicity. Once modernist literary techniques mature and readers internalize the modernist mode of attention, the constraint's sunset arrives — the novel no longer needs to physically entrap patrons in bars to model consciousness; the narrative itself becomes the auditory field. The mandatrophy is resolved by recognizing that the constraint serves genuine coordination (community in the bar, venue for artists) while also enabling genuine extraction (proprietor profit, patron intoxication suppressing agency). The classification as tangled_rope is precise: it has beneficiaries (proprietors, artists, literary consciousness) and victims (patrons, linguistic meaning), requires active enforcement (acoustic design), and exhibits both coordination function and asymmetric extraction. No single type captures the structure better than this hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_emergent_soundscape,
    'Is the auditory lure in the Ormond Hotel a deliberate design by proprietors or an emergent property of acoustic architecture?',
    'Historical records of bar design choices, proprietor interviews (contemporary or archival), comparison with Irish pubs of the same period',
    'If deliberate: snare/tangled_rope classification confirmed (intentional extraction). If emergent: rope classification strengthened (accidental coordination mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_vs_emergent_soundscape, empirical, 'Whether soundscape design is intentional or emergent').

omega_variable(
    aesthetic_vs_intoxicant_suppression,
    'Does music suppress rational agency primarily through aesthetic enchantment or through interaction with ethanol''s neurochemical effects?',
    'Neuropharmacological analysis; comparison of auditory susceptibility in sober vs intoxicated subjects; analysis of Joyce''s narrative technique (does he distinguish aesthetic from pharmacological compulsion?)',
    'If primarily aesthetic: the lure is an artistic/social constraint. If primarily pharmacological: the lure is a biological/chemical constraint with human agency as secondary actor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aesthetic_vs_intoxicant_suppression, empirical, 'Whether suppression is aesthetic or pharmacological').

omega_variable(
    modernist_pedagogy_authenticity,
    'Is the auditory lure in Ulysses a genuine modernist technique for consciousness expansion or a naturalized rationalization of commercial extraction?',
    'Literary analysis of Joyce''s textual intent vs readerly effects; comparison with other modernist use of sensory overwhelm; analysis of Joyce''s relationship to commercial publishing and literary markets',
    'If authentic pedagogy: scaffold/tangled_rope classification confirmed (temporary tool with artistic purpose). If naturalized extraction: snare classification strengthened (artistic language masks patron exploitation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_pedagogy_authenticity, conceptual, 'Whether modernist use is genuine pedagogy or naturalized extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp11, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulysses_tr_t0, ulysses_chp11, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ulysses_tr_t10, ulysses_chp11, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ulysses_tr_t20, ulysses_chp11, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(ulysses_be_t0, ulysses_chp11, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ulysses_be_t10, ulysses_chp11, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(ulysses_be_t20, ulysses_chp11, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp11, information_standard).
narrative_ontology:affects_constraint(ulysses_chp11, homeric_myth_modernism).
narrative_ontology:affects_constraint(ulysses_chp11, intoxication_agency_suppression).

% DUAL FORMULATION NOTE:
% The auditory lure is structurally distinct from the Sirens myth it references (constraint: homeric_myth_modernism, ε≈0.15, rope in most perspectives) and from the pharmacological effects of alcohol (constraint: intoxication_agency_suppression, ε≈0.60, snare in most perspectives). The auditory lure at ε≈0.38 represents the hybrid zone where artistic technique, commercial design, and biological constraint intersect. It is downstream of both the mythic reference and the pharmacological reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp11, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
