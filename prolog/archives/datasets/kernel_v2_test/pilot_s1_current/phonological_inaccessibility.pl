% ============================================================================
% CONSTRAINT STORY: phonological_inaccessibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_phonological_inaccessibility, []).

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
 *   constraint_id: phonological_inaccessibility
 *   human_readable: Phonological Inaccessibility and Renaissance Latin Reconstruction
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   The phonological inaccessibility constraint emerges at the intersection
 *   of linguistic drift and institutional authority during the Renaissance
 *   reconstruction of Classical Latin. Medieval Latin, as a living liturgical
 *   and administrative language, had undergone centuries of phonological
 *   evolution (palatalization before front vowels, lenition of intervocalic
 *   consonants, vowel syncope and merger) from the Classical form. By the
 *   15th century, medieval speakers could not produce Classical Latin
 *   phonology even with instruction — the phonetic distance was too great for
 *   adult acquisition. Renaissance humanists, beginning with Petrarch and
 *   intensifying through the 15th–16th centuries, undertook to recover and
 *   reconstruct 'authentic' Classical pronunciation based on comparative
 *   analysis of Romance forms, classical texts, and Roman grammarians'
 *   descriptions. This reconstruction effort created a structural paradox:
 *   the recovered pronunciation was more inaccessible than medieval Latin
 *   because it required abandoning speakers' native phonological intuitions
 *   and following textual authority. The constraint operates at multiple
 *   levels simultaneously: it enforces a unified standard across
 *   ecclesiastical and educational institutions (coordination function), it
 *   concentrates interpretive authority in the learned humanist elite
 *   (extraction function), it suppresses the legitimacy of medieval latinity
 *   as a living language alternative (suppression), and eventually it becomes
 *   a performative marker of erudition with minimal functional communication
 *   role (theater). The constraint exhibits all six DR types from different
 *   perspectives, making it a diagnostic case for how institutional power,
 *   epistemic authority, and linguistic naturalness interact.
 *
 * KEY AGENTS:
 *   - Medieval Clergyman: Primary victim (powerless/trapped) — speaker of living medieval Latin; faces institutional pressure to perform a phonologically inaccessible reconstruction; trapped by clerical identity and institutional dependence
 *   - Grammar Master: Secondary beneficiary (moderate/constrained) — teachers of Latin grammar; benefit from the constraint's difficulty and the need for constant institutional enforcement; constrained by pedagogical burden and professional reputation
 *   - Humanist Scholar: Primary beneficiary (institutional/arbitrage) — Renaissance scholars undertaking reconstruction; capture interpretive authority and intellectual prestige from 'authentic' recovery; high exit options (can adopt alternative reconstructions or shift to orthographic standardization)
 *   - Ecclesiastical Authority: Institutional beneficiary (institutional/arbitrage) — Church hierarchy benefits from standardized Latin across dioceses; uses reconstruction authority to consolidate institutional control; arbitrage options enable selective enforcement
 *   - Print Culture Coalition: Organized actor (organized/constrained) — printers, grammarians, publishers building standardized orthographic representation; constrained by market and institutional resistance but have collective agency; see sunset mechanism in orthographic stabilization
 *   - Medieval Latinity: Victim (powerless/trapped) — living language tradition suppressed by the classical reconstruction; cannot exit or defend its legitimacy once classical supremacy doctrine is institutionalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(phonological_inaccessibility, 0.62).
domain_priors:suppression_score(phonological_inaccessibility, 0.48).
domain_priors:theater_ratio(phonological_inaccessibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(phonological_inaccessibility, extractiveness, 0.62).
narrative_ontology:constraint_metric(phonological_inaccessibility, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(phonological_inaccessibility, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(phonological_inaccessibility, tangled_rope).
narrative_ontology:human_readable(phonological_inaccessibility, "Phonological Inaccessibility and Renaissance Latin Reconstruction").
narrative_ontology:topic_domain(phonological_inaccessibility, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(phonological_inaccessibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(phonological_inaccessibility, humanist_scholars).
narrative_ontology:constraint_beneficiary(phonological_inaccessibility, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(phonological_inaccessibility, classical_erudition_ideal).
narrative_ontology:constraint_victim(phonological_inaccessibility, medieval_latinity).
narrative_ontology:constraint_victim(phonological_inaccessibility, living_linguistic_communities).
narrative_ontology:constraint_victim(phonological_inaccessibility, vernacular_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL CLERGYMAN (SNARE) — Trapped in a living Latin vernacular (Classical Latin pronunciation is phonologically inaccessible from medieval spoken forms). Career and institutional identity depend on Latin competence, but the 'correct' form is unreachable by ear and requires constant textual enforcement. Experiences the constraint as coercive standardization of an inaccessible target, with no escape except loss of clerical status. Suppression is structural: alternative pronunciations and usages are marked as errors regardless of communicative function.
constraint_indexing:constraint_classification(phonological_inaccessibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GRAMMAR MASTER (TANGLED ROPE) — Experiences the constraint as both coordination and extraction. Coordination function: teaching a shared Latin standard enables communication across dioceses and time (real problem solved). Extraction function: the grammar master's authority and career prestige depend on maintaining the standard's difficulty and opacity. Exit is constrained: abandoning the standard risks professional status, but enforcing it requires constant pedagogical labor. Moderate experienced extraction — some benefit from standardization, significant cost from enforcement burden.
constraint_indexing:constraint_classification(phonological_inaccessibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: HUMANIST SCHOLAR (ROPE) — Benefits from the inaccessibility of Classical phonology. The reconstructed pronunciation requires constant reference to texts and precedent, which concentrates interpretive authority in the learned few. Exit options are strong (arbitrage): humanists can choose to emphasize different canonical texts, adopt alternative reconstructions, or retreat to orthographic standardization. Experiences the constraint as coordination: solving the problem of how to read the classical corpus correctly and authentically. Benefits from first-mover advantage in reconstruction authority.
constraint_indexing:constraint_classification(phonological_inaccessibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PRINT CULTURE COALITION (SCAFFOLD) — Organized network of printers, grammarians, and authors sees the constraint as a temporary coordination problem solvable through standardized orthographic representation (spelling regularization, diacritical marks, grammatical rules in print). Print enables distributed standardization that replaces performative oral enforcement. Has sunset logic: as printed grammars proliferate and literacy expands, the phonological inaccessibility becomes less binding — the standard is preserved through orthographic/textual transmission rather than phonological performance. Sunset estimated at 150–200 years (16th–18th centuries) as printing normalizes orthography and silent reading replaces oral performance as the standard interaction with Latin texts.
constraint_indexing:constraint_classification(phonological_inaccessibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CLASSICAL AUTHORITY RITUAL (PITON) — By the 18th–19th centuries, Classical Latin pronunciation has become a mostly performative marker of erudition. No living community depends on it for actual communication; it persists as a theatrical demonstration of scholarly status. The constraint's original function (enforcing a reconstructed standard via phonological difficulty) has atrophied — what remains is the ritual invocation of classical authenticity in academic settings. Theater ratio is high because the pronunciation's functional role is minimal but its signaling role is maximal. The piton classification derives from the degradation of the functional coordination (uniting communicative communities) into pure status performance (marking the educated from the uneducated).
constraint_indexing:constraint_classification(phonological_inaccessibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, phonological inaccessibility is presented as a natural consequence of sound change and historical drift: Classical Latin pronunciation is simply unreachable because intervening centuries of phonological evolution (palatalization, lenition, vowel shifts) have transformed the medieval speakers' available phonetic inventory. The constraint appears as a natural linguistic law. However, the structural data reveals this as a false summit: the constraint's persistence depends on active institutional enforcement (beneficiaries defending the classical standard, suppression of alternatives), not on phonological law alone. A genuinely natural phonological shift would be passive; this constraint is actively maintained.
constraint_indexing:constraint_classification(phonological_inaccessibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(phonological_inaccessibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(phonological_inaccessibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(phonological_inaccessibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(phonological_inaccessibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(phonological_inaccessibility, TR),
    TR >= 0.70.

:- end_tests(phonological_inaccessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): The constraint extracts substantial value asymmetrically. Humanist scholars and ecclesiastical authorities benefit from the authority position (interpretive prestige, institutional control). Medieval speakers and grammar masters bear the cost of enforcing an inaccessible standard. The extractiveness rises from medieval period (0.35) through humanist consolidation (0.68) as institutional enforcement intensifies, then stabilizes (0.62) as printing and literacy normalization reduce the burden of oral performance while preserving textual authority. Suppression (0.48): Moderate. Medieval alternatives are suppressed through institutional pressure (ecclesiastical authority), pedagogical practice (constant correction), and doctrinal authority (classical supremacy doctrine). However, suppression is not total — medieval usage persists in specialized domains (theological debate, legal Latin) and in actual speech communities. The suppression increases during humanist consolidation (0.58) as the reconstruction becomes canonical, then decreases (0.42) as the constraint transitions to performative status where medieval alternatives cease to be threats (they are simply 'incorrect' in a way that does not need active suppression once the outcome is settled). Theater ratio (0.58): Moderate-high. The constraint's function shifts from coordination (medieval period: solving the problem of unified standard) through increasing theater (humanist period: the reconstruction becomes more about demonstrating erudition than enabling communication) to predominantly performative (early modern period: Classical pronunciation is a marker of scholarly status, not a communicative necessity). The theater ratio rises from 0.28 to 0.65 as the functional coordination is replaced by status signaling, then slightly decreases (0.58) as printing and silent reading make the constraint less salient overall.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap of 4–5 classification types (from Snare to Rope to Mountain) reflects the profound structural difference between positions within the constraint. Those trapped in the enforcement mechanism (medieval clerics) experience maximal extraction with no alternatives. Those with institutional power (humanists, church authorities) experience coordination benefits and interpretive authority. Those attempting to build alternatives (print coalition) see the constraint as transitional. Those outside the functional system (civilizational analytical observer) risk naturalizing the contingent institutional arrangement as a law of nature. The gap reveals that the constraint's classification is not observer-independent — it depends entirely on the observer's structural position relative to the enforcement mechanism, the beneficiary group, and the availability of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Medieval clerics (trapped, no alternatives) have d = 0.95 (full target for extraction). Humanist scholars (institutional power, arbitrage exit) have d = 0.15 (beneficiaries). Grammar masters (moderate power, constrained exit) have d = 0.55 (moderate targets — bears teaching burden but benefits from career prestige). The temporal trajectory shows stable high d for powerless agents (medieval clerics have no escape throughout the interval) and stable moderate d for institutional actors (humanists can always shift textual authorities or methodologies, so their extraction vulnerability stays low). The directionality structure explains the tangled_rope classification at the moderate level: the grammar master simultaneously coordinates (teaches a shared standard) and bears extraction (unpaid pedagogical labor, professional vulnerability to critique of methods). The beneficiary declaration (humanist_scholars, ecclesiastical_authority) creates negative d for those groups, producing Rope classification. The victim declaration (medieval_latinity, living_linguistic_communities) creates high d and Snare classification from the powerless perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (recovering and transmitting Classical Latin pronunciation authentically) does not outlive its original function in the way mandatrophy typically involves — the mandate remains explicitly alive through classical education. However, the underlying functional reason for the mandate's urgency decays: by the 18th century, Latin is no longer a living communicative medium even in ecclesiastical contexts, so the pressure to maintain phonological authenticity becomes progressively more performative. The classical supremacy doctrine that justifies the constraint persists even as its practical necessity diminishes. This is not mandatrophy in the sense of a mandate outliving its function, but rather a constraint that persists through institutional inertia and status signaling (Piton classification) even as its functional coordination problem has been replaced by orthographic standardization (Scaffold sunset). The resolution of the perspectival analysis prevents misclassification: by tracking the tangled_rope (coordinate + extract) at institutional perspectives and the snare at powerless perspectives, the framework prevents the humanist's 'authentic recovery' narrative from naturalizing what is actually an extraction mechanism. The constraint is a false summit if classified as Mountain (natural phonological law) — the structural data reveals it is an enforced institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonological_recovery_feasibility,
    'Could medieval speakers have accessed Classical Latin pronunciation through explicit training and rehearsal, or was it genuinely inaccessible due to neurolinguistic constraints on adult phonological acquisition?',
    'Comparative analysis of adult L2 phonological acquisition rates in historical contexts; examination of manuscripts showing progressive ''correction'' of pronunciation across pedagogical texts; analysis of individual learners'' phonological trajectories',
    'If recoverable: the constraint is extraction mechanism (suppressing alternatives that speakers could manage). If genuinely inaccessible: the constraint is a coordination mechanism (solving the problem of how to transmit an unreachable form). Classification shifts from Snare to Tangled Rope at the powerless perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonological_recovery_feasibility, empirical, 'Whether Classical Latin pronunciation was phonologically recoverable by adult medieval speakers').

omega_variable(
    reconstruction_authenticity_contested,
    'Is the Renaissance reconstructed ''Classical'' pronunciation an accurate recovery of historical Roman pronunciation, or a constructed ideal that never existed and benefited from the inaccessibility that justified constant textual reference?',
    'Cross-linguistic phonological reconstruction methods (comparative Romance philology, phonetic principles); examination of which reconstructed features were contested among humanists; analysis of whether reconstructions changed when textual evidence was revised. Testing whether inaccessibility served the reconstruction process itself.',
    'If authentic: the constraint is a legitimate coordination mechanism (recovering lost knowledge). If constructed: the constraint is tangled rope with stronger extraction component (inaccessibility justified by appeal to authenticity, but authenticity was never independently established). Mountain claim would be false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconstruction_authenticity_contested, empirical, 'Whether reconstructed Classical pronunciation represents authentic recovery or constructed ideal').

omega_variable(
    medieval_latin_vitality_suppression,
    'Did institutional enforcement of Classical standards actively suppress the legitimacy of medieval Latin as a living language, or did medieval Latin''s functional decline precede the humanist reformation?',
    'Chronological analysis of medieval Latin usage across domains (liturgy, law, administration); measurement of functional domain reduction before vs. after humanist institutional pressure; examination of whether medieval latinity continued in specialized domains (theological debate, legal terminology) after humanist reforms or was directly suppressed',
    'If suppression: the constraint is Snare with victim group = medieval_latinity (legitimate alternative was actively erased). If decline preceded: the constraint is Tangled Rope or Scaffold (coordination mechanism responding to real decline). Suppression metric interpretation changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_latin_vitality_suppression, empirical, 'Whether Classical enforcement actively suppressed medieval Latin vitality').

omega_variable(
    kernel_authenticity_vs_reconstruction,
    'Is ''Classical Latin'' the kernel (a contested but stable reference corpus) or is the kernel ''correct Latin'' whose reading oscillates between medieval-derived and reconstructed-classical forms?',
    'Textual history of authoritative Latin grammars; examination of which texts (medieval rules vs. humanist reconstructions) were cited as authoritative across different institutions; chronological analysis of when the classical reading became canonical',
    'If Classical is the kernel: the constraint is a reading-relation story (medieval vs. classical readings coexist_with or foreclose each other). If ''correct Latin'' is the kernel: the constraint is ordinary (non-kernel) tangled_rope with a shifting definition of beneficiary and victim. This affects whether to use cs_structure fields.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authenticity_vs_reconstruction, conceptual, 'Kernel ambiguity: Classical corpus vs. abstract ''correct Latin'' standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(phonological_inaccessibility, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phon_theater_medieval_1400, phonological_inaccessibility, theater_ratio, 0, 0.28).
narrative_ontology:measurement(phon_theater_humanist_1450, phonological_inaccessibility, theater_ratio, 50, 0.48).
narrative_ontology:measurement(phon_theater_consolidation_1500, phonological_inaccessibility, theater_ratio, 100, 0.65).
narrative_ontology:measurement(phon_theater_stabilization_1600, phonological_inaccessibility, theater_ratio, 200, 0.58).

% Extraction over time
narrative_ontology:measurement(phon_extractiveness_medieval_1400, phonological_inaccessibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(phon_extractiveness_humanist_1450, phonological_inaccessibility, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(phon_extractiveness_consolidation_1500, phonological_inaccessibility, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(phon_extractiveness_stabilization_1600, phonological_inaccessibility, base_extractiveness, 200, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(phon_suppression_medieval_1400, phonological_inaccessibility, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(phon_suppression_humanist_1450, phonological_inaccessibility, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(phon_suppression_consolidation_1500, phonological_inaccessibility, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(phon_suppression_stabilization_1600, phonological_inaccessibility, suppression_requirement, 200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(phonological_inaccessibility, information_standard).
narrative_ontology:affects_constraint(phonological_inaccessibility, ecclesiastical_latinity_decline).
narrative_ontology:affects_constraint(phonological_inaccessibility, vernacular_language_legitimacy).
narrative_ontology:affects_constraint(phonological_inaccessibility, humanist_textual_authority).

% DUAL FORMULATION NOTE:
% Phonological inaccessibility is part of a constraint family centered on the classical language reconstruction project. The family includes: (1) classical_pronunciation_reconstruction (empirical): the pure technical problem of recovering lost phonology (Mountain-leaning); (2) phonological_inaccessibility (this story): the institutional constraint arising when reconstruction makes the language inaccessible to speakers (Tangled Rope); (3) ecclesiastical_latinity_decline (downstream): the suppression of living medieval Latin use once classical standards are institutionalized (Snare). Each story has distinct epsilon and benefits/victims. Inaccessibility affects the decline of ecclesiastical latinity by providing institutional cover for suppressing alternatives ('medieval Latin is simply not authentic'). The reconstruction project affects inaccessibility by justifying the phonological distance as authentic rather than as institutional burden.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(phonological_inaccessibility, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
