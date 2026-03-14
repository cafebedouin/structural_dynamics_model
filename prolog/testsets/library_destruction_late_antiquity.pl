% ============================================================================
% CONSTRAINT STORY: library_destruction_late_antiquity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_library_destruction_late_antiquity, []).

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
 *   constraint_id: library_destruction_late_antiquity
 *   human_readable: Library Destruction in Late Antiquity (4th-8th centuries CE)
 *   domain: historical/cultural/epistemic
 *
 * SUMMARY:
 *   Library destruction in Late Antiquity (4th-8th centuries CE) represents a
 *   massive epistemic extraction mechanism operating through physical
 *   destruction and institutional monopolization of knowledge. The constraint
 *   emerges from the collision between institutional Christianity's
 *   consolidation as the empire's dominant religion and the existence of
 *   competing intellectual traditions (pagan philosophy, Zoroastrianism,
 *   gnostic Christianity, Jewish rabbinic scholarship) preserved in libraries
 *   throughout the Mediterranean and Near East. The extraction mechanism
 *   operates on multiple levels: direct destruction of physical collections
 *   (libraries burned during invasions or intentionally confiscated),
 *   systematic suppression of non-Christian texts through imperial edicts and
 *   ecclesiastical prohibitions, institutional monopolization through
 *   monastic libraries that controlled which knowledge survived and how it
 *   was transmitted, and epistemic suppression through the narrative framing
 *   that knowledge loss was inevitable rather than chosen. The theater ratio
 *   reveals that by the 6th-7th centuries, the destruction had become
 *   institutionalized and performative — monastic scriptoria performed the
 *   role of preservation while selective copying ensured only
 *   Christian-compatible knowledge survived. The constraint exhibits high
 *   suppression (0.82) because alternatives to the emerging
 *   Christian-institutional monopoly were systematically eliminated through
 *   law, violence, and institutional control. The extractiveness trajectory
 *   (rising from 0.15 in 300 CE to 0.72 by 600 CE, then partially declining
 *   as Islamization stabilizes institutional roles by 800 CE) shows
 *   accumulation during the period of maximum institutional conflict and then
 *   partial stabilization as new institutional arrangements consolidate.
 *
 * KEY AGENTS:
 *   - Local Reading Communities: Primary victims (powerless/trapped) — communities dependent on libraries for education and intellectual continuity; face total knowledge loss with no alternatives
 *   - Pagan Philosophical Traditions: Primary victims (powerless/trapped) — Neoplatonic, Stoic, and other non-Christian lineages systematically suppressed through destruction and institutional prohibition
 *   - Christian Religious Authorities: Primary extractors (organized/constrained) — pursues both genuine knowledge preservation AND suppression of competing traditions; operates through institutional enforcement
 *   - Monastic Networks: Secondary beneficiary (institutional/arbitrage) — gains institutional legitimacy and resource allocation through becoming primary knowledge repository; functions as both preservationist and gatekeeper
 *   - Imperial Administration: Tertiary actor (institutional/arbitrage) — delegates enforcement authority to religious institutions while maintaining theater of state control over knowledge; progressively cedes actual power
 *   - Jewish and Zoroastrian Communities: Secondary victims (powerless/constrained) — lose institutional access to libraries but some textual preservation occurs through parallel institutional networks not directly targeted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(library_destruction_late_antiquity, 0.68).
domain_priors:suppression_score(library_destruction_late_antiquity, 0.82).
domain_priors:theater_ratio(library_destruction_late_antiquity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(library_destruction_late_antiquity, extractiveness, 0.68).
narrative_ontology:constraint_metric(library_destruction_late_antiquity, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(library_destruction_late_antiquity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(library_destruction_late_antiquity, snare).
narrative_ontology:human_readable(library_destruction_late_antiquity, "Library Destruction in Late Antiquity (4th-8th centuries CE)").
narrative_ontology:topic_domain(library_destruction_late_antiquity, "historical/cultural/epistemic").

domain_priors:requires_active_enforcement(library_destruction_late_antiquity).

% --- Structural relationships ---
narrative_ontology:constraint_victim(library_destruction_late_antiquity, textual_knowledge_base).
narrative_ontology:constraint_victim(library_destruction_late_antiquity, intellectual_continuity).
narrative_ontology:constraint_victim(library_destruction_late_antiquity, local_reading_communities).
narrative_ontology:constraint_victim(library_destruction_late_antiquity, pagan_philosophical_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL READING COMMUNITY (SNARE) — Communities dependent on libraries for education, religious study, and intellectual continuity face total extraction: collections destroyed, knowledge transmission severed, no alternative preservation mechanisms available. Exit is impossible — the physical destruction of collections leaves no choice.
constraint_indexing:constraint_classification(library_destruction_late_antiquity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PAGAN PHILOSOPHICAL TRADITIONS (SNARE) — Neoplatonic, Stoic, and other non-Christian intellectual lineages are systematically suppressed through library destruction, text confiscation, and institutional closure. No exit path exists — the constraint operates through physical destruction and institutional prohibition. Knowledge loss is permanent across generational timescales.
constraint_indexing:constraint_classification(library_destruction_late_antiquity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CHRISTIAN RELIGIOUS AUTHORITIES (TANGLED ROPE) — Organized institutional agents pursue both genuine coordination (preservation of Christian texts, establishment of scriptoria, creation of monastic libraries as knowledge repositories) AND asymmetric extraction (suppression of competing traditions, monopolization of literacy, control over what knowledge is preserved vs destroyed). Active enforcement required to maintain both functions simultaneously.
constraint_indexing:constraint_classification(library_destruction_late_antiquity, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: IMPERIAL ADMINISTRATIVE STRUCTURE (PITON) — Late Roman empire's institutional mechanisms for library management and knowledge control persist through inertia despite functional decay. Theater ratio reflects performative maintenance of state libraries while actual control has devolved to religious authorities. The constraint operates as delegated enforcement theater rather than direct state action.
constraint_indexing:constraint_classification(library_destruction_late_antiquity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: MONASTIC PRESERVATION NETWORKS (ROPE) — From a civilizational perspective, monastic scriptoria and libraries coordinate textual preservation, education, and knowledge transmission across dispersed communities. While Christian-text-focused, the mechanism solves a genuine coordination problem: how to preserve knowledge across political instability and population disruption. This is the beneficiary perspective — monasteries gain institutional legitimacy, resource allocation, and cultural authority through becoming the primary knowledge repositories.
constraint_indexing:constraint_classification(library_destruction_late_antiquity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From maximal analytical distance, knowledge loss in civilization transitions is structurally inevitable: institutional collapse during barbarian invasions necessarily creates conditions where physical texts decay and knowledge transmission breaks. The destruction appears as an immutable law of history rather than a contingent political choice. However, the structural data reveals this as false naturalization — the suppression of competing traditions is a distinct choice, not an inevitable consequence of institutional disruption.
constraint_indexing:constraint_classification(library_destruction_late_antiquity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(library_destruction_late_antiquity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(library_destruction_late_antiquity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(library_destruction_late_antiquity, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(library_destruction_late_antiquity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(library_destruction_late_antiquity, TR),
    TR >= 0.70.

:- end_tests(library_destruction_late_antiquity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint operates through suppression of competing knowledge traditions and institutional monopolization of preserved texts. Christian authorities benefit from exclusive control over remaining knowledge repositories, eliminating intellectual competition. The value is below 0.72 (maximum Snare) because some genuine knowledge preservation occurs through monastic scriptoria — the extraction is embedded within a real coordination mechanism rather than operating through pure destruction. Suppression (0.82): Very high. The constraint systematically eliminates alternatives to Christian institutional control through law (imperial edicts against paganism), violence (destruction of temples and libraries), and institutional prohibition (monastery rules restricting access to non-Christian texts). Targets facing the constraint have no exit options — knowledge is destroyed, libraries are closed, reading communities scattered. Theater ratio (0.55): Moderate-high. By the 6th-7th centuries, selective copying by monastic scriptoria creates the appearance of neutral preservation while performing the function of institutional gatekeeping. The theater increases over time as destruction becomes institutionalized and rationalized as 'preservation' rather than acknowledged as suppression. Claimed type (Snare): The structural signature matches Snare — high extractiveness, high suppression, asymmetric distribution of costs (victims lose knowledge, beneficiaries monopolize what remains), and systematic elimination of alternatives. The mandatrophy is resolved through the omega variables addressing whether destruction was intentional vs incidental and whether alternatives existed.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between the victim perspectives (Snare: trapped, powerless) and the institutional perspectives (Tangled Rope/Rope: organized/institutional with constrained or arbitrage exit). Victims experience the constraint as immutable physical destruction with no alternatives. Beneficiaries experience the constraint as functional institutional coordination that happens to suppress alternatives. The analytical observer experiences the constraint as either natural law (false mountain) or as contingent choice (true snare/tangled rope), depending on whether they naturalize institutional suppression as historical necessity. The perspectival gap reveals that 'library destruction' is simultaneously an epistemic tragedy (from victim perspective), an institutional coordination solution (from beneficiary perspective), and a contingent choice to eliminate intellectual competition (from analytical perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to the extraction flow. Reading communities and non-Christian traditions are pure targets (d approaching 1.0) — they bear costs of knowledge loss with zero beneficiary status. Christian religious authorities hold mixed position (d ≈ 0.45-0.55) — they benefit from monopoly control but also bear costs of maintaining the suppression mechanism and managing institutional enforcement. Monastic networks (d ≈ 0.20-0.30) — they are positioned as beneficiaries through institutional legitimacy and resource allocation, experiencing low extraction and high benefit. The imperial administration (d ≈ 0.35-0.40) — positioned as intermediary between enforcement authority and actual extractive benefit; progressively cedes power to religious authorities. The analytical observer (d ≈ 0.72) — positioned as distant observer measuring the extraction mechanism rather than participating in it.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint exhibits extractiveness > 0.70 (peaks at 0.72 in 600 CE) and suppression > 0.60 throughout. The mandatrophy risk is whether the constraint is pure extraction (Snare) or mixed coordination-extraction (Tangled Rope). The resolution operates through the omega variables: (1) Intentionality: Historical evidence shows that destruction of pagan libraries was deliberate institutional policy, not incidental loss — this supports Snare classification. (2) Preservation functionality: Monastic libraries did preserve some knowledge, including classical texts later rediscovered in the Renaissance — this creates the mixed coordination-extraction signature of Tangled Rope. The resolution: the constraint is structurally Snare (pure extraction with suppression as primary mechanism) from the perspective of victims and non-Christian traditions. It is Tangled Rope (coordination + extraction) from the perspective of Christian institutions that simultaneously preserve knowledge and consolidate doctrinal monopoly. The two classifications are not in contradiction — they reflect genuinely different structural positions relative to the constraint. The constraint resolves toward Snare when evaluated from the target/victim perspective (no exit options, pure cost), and toward Tangled Rope when evaluated from the institutional perspective (mixed coordination and extraction). This perspectival resolution of the mandatrophy is the analytical signal that the constraint involves both genuine institutional functions (preservation) and genuine extraction (suppression of alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_incidental_destruction,
    'How much library destruction was intentional suppression of pagan knowledge versus incidental consequence of political/military disruption?',
    'Textual analysis of primary sources distinguishing explicit orders to destroy pagan texts from accounts of accidental loss during invasions; comparison of destruction patterns (targeted vs random); examination of monastic scriptoria decision-making on which texts to preserve',
    'If primarily intentional: constraint is a pure Snare with deliberate extraction mechanism. If primarily incidental: constraint is Tangled Rope where suppression is a side effect of coordination rather than a primary extraction function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_vs_incidental_destruction, empirical, 'Proportion of intentional suppression vs incidental loss in library destruction').

omega_variable(
    preservation_mechanism_functionality,
    'To what extent did monastic libraries functionally replace destroyed collections, providing continuity of knowledge, versus create a new extractive monopoly over preserved knowledge?',
    'Comparative analysis of pre-destruction libraries vs monastic holdings; textual transmission analysis showing which works survived (Christian theology vs philosophy vs science vs classical literature); examination of access restrictions and copying patterns',
    'If monastic preservation genuinely maintained intellectual continuity: constraint transitions toward Rope/Scaffold and loses snare character. If monastic libraries bottlenecked knowledge access: constraint remains Snare with extracted benefits flowing to institutional controllers of remaining texts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preservation_mechanism_functionality, empirical, 'Whether monastic preservation functionally replaced destroyed collections').

omega_variable(
    suppression_internalization_depth,
    'Did reading communities internalize suppression of non-Christian knowledge as natural/inevitable, or maintain awareness of lost intellectual traditions as externally imposed extraction?',
    'Analysis of post-5th-century manuscripts for references to lost works; examination of whether scribal notes distinguish ''unavailable'' (lost) from ''heretical'' (suppressed); study of later Renaissance rediscovery reactions (surprise at recovered texts vs recognition of familiar lost knowledge)',
    'If internalized: communities experienced the constraint as immutable (mountain-like) even from the perspective of those bearing costs. If maintained awareness: constraint retained snare character and resistance mechanisms were possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Degree to which suppression was internalized versus recognized as external extraction').

omega_variable(
    alternative_preservation_plausibility,
    'Given institutional constraints of the period, were there structurally viable alternatives to library destruction for Christian communities to achieve doctrinal consolidation?',
    'Comparative historical analysis of other religious-institutional transitions (Islamic expansion, Buddhist transmission across Asia) examining whether knowledge consolidation required destruction vs could have coexisted with preservation of competing traditions; analysis of monastic scriptoria capacity constraints during this period',
    'If alternatives existed: destruction represents choice, not necessity — strengthens Snare classification. If alternatives were not viable given period constraints: destruction may be Tangled Rope (coordination + extraction as necessary side effect) rather than pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_preservation_plausibility, conceptual, 'Whether structurally viable alternatives to destruction existed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(library_destruction_late_antiquity, 300, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(libr_tr_t300, library_destruction_late_antiquity, theater_ratio, 300, 0.1).
narrative_ontology:measurement(libr_tr_t400, library_destruction_late_antiquity, theater_ratio, 400, 0.25).
narrative_ontology:measurement(libr_tr_t500, library_destruction_late_antiquity, theater_ratio, 500, 0.45).
narrative_ontology:measurement(libr_tr_t600, library_destruction_late_antiquity, theater_ratio, 600, 0.55).
narrative_ontology:measurement(libr_tr_t700, library_destruction_late_antiquity, theater_ratio, 700, 0.62).
narrative_ontology:measurement(libr_tr_t800, library_destruction_late_antiquity, theater_ratio, 800, 0.58).

% Extraction over time
narrative_ontology:measurement(libr_be_t300, library_destruction_late_antiquity, base_extractiveness, 300, 0.15).
narrative_ontology:measurement(libr_be_t400, library_destruction_late_antiquity, base_extractiveness, 400, 0.35).
narrative_ontology:measurement(libr_be_t500, library_destruction_late_antiquity, base_extractiveness, 500, 0.68).
narrative_ontology:measurement(libr_be_t600, library_destruction_late_antiquity, base_extractiveness, 600, 0.72).
narrative_ontology:measurement(libr_be_t700, library_destruction_late_antiquity, base_extractiveness, 700, 0.68).
narrative_ontology:measurement(libr_be_t800, library_destruction_late_antiquity, base_extractiveness, 800, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(library_destruction_late_antiquity, identity_coordination).
narrative_ontology:affects_constraint(library_destruction_late_antiquity, monastic_scriptoria_gatekeeping).
narrative_ontology:affects_constraint(library_destruction_late_antiquity, islamic_knowledge_transmission_networks).
narrative_ontology:affects_constraint(library_destruction_late_antiquity, jewish_textual_preservation_parallel_institutions).

% DUAL FORMULATION NOTE:
% Library destruction decomposes into three structurally distinct constraints: (1) library_destruction_late_antiquity (this story) — epistemic extraction through suppression of competing traditions, ε=0.68, Snare from victim perspective, Tangled Rope from institutional perspective; (2) monastic_scriptoria_gatekeeping — institutional control over knowledge reproduction, ε=0.45, Tangled Rope (genuine preservation + selective copying); (3) manuscript_transmission_bias — differential survival of Christian vs non-Christian texts through selective copying practices, ε=0.55, Tangled Rope (functional reproduction + selection bias). Each constraint has distinct ε and distinct mechanisms; they are linked through network dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(library_destruction_late_antiquity, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
