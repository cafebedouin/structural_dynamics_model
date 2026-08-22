% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Domain-Allocated Latin Correctness Regime (Hybrid Reading)
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   From the fourteenth to the seventeenth century, learned Europe ran Latin
 *   under a bifurcated normative regime: literary and rhetorical composition
 *   was measured against the classical canon, while technical and practical
 *   writing continued in the evolved medieval register without classical
 *   correction. This file instantiates the HYBRID READING of the
 *   latin_correctness kernel: normative authority allocated by domain. The
 *   epsilon referent is the standing bifurcated arrangement itself, assessed
 *   by this reading's own lights — not the continuity reading's universal
 *   license nor the rupture reading's universal correction, which are
 *   separate constraint files linked in the network section. The arrangement
 *   solves a real coordination problem (each register stays fit to its
 *   function) while transferring status, patronage, and credential leverage
 *   toward classically certified literati and the faculties that certify
 *   them; technical writers pay in correction labor, editorial deference, and
 *   discounted standing, and students pay in years of composition drill
 *   unrelated to their eventual technical work. KEY AGENTS (by structural
 *   relationship): humanist_literati — primary beneficiary
 *   (powerful/constrained); aristocratic_patrons_of_letters — secondary
 *   beneficiary (institutional/arbitrage); university_arts_faculties — agenda
 *   setter (institutional/constrained); scholastic_technical_authors —
 *   primary target (organized/trapped); non_elite_latin_students — secondary
 *   target (powerless/trapped); vernacular_writers_and_readers — excluded
 *   voice (moderate/mobile); historical_linguists — analytical observer.
 *   Claimed type and metrics are authored independently: the claim states
 *   what this reading takes the structure to be; the metrics describe its
 *   observed operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.62).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.58).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Domain-Allocated Latin Correctness Regime (Hybrid Reading)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, '4d8c4619-03dc-492c-b725-2f87ed8979d1').
narrative_ontology:cs_kernel_codification('4d8c4619-03dc-492c-b725-2f87ed8979d1', formalized).
narrative_ontology:cs_authority_grounding('4d8c4619-03dc-492c-b725-2f87ed8979d1', lineage).
narrative_ontology:cs_interpretation_layer_present('4d8c4619-03dc-492c-b725-2f87ed8979d1').
narrative_ontology:cs_reading_relation('4d8c4619-03dc-492c-b725-2f87ed8979d1', latin_correctness__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('4d8c4619-03dc-492c-b725-2f87ed8979d1', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_axiom('4d8c4619-03dc-492c-b725-2f87ed8979d1', foundational, domain_indexed_normative_authority).
narrative_ontology:cs_axiom_status(domain_indexed_normative_authority, holdable).
narrative_ontology:cs_axiom_grounding('4d8c4619-03dc-492c-b725-2f87ed8979d1', domain_indexed_normative_authority, instrumental).
narrative_ontology:cs_axiom('4d8c4619-03dc-492c-b725-2f87ed8979d1', secondary, technical_register_precision_immunity).
narrative_ontology:cs_axiom_status(technical_register_precision_immunity, holdable).
narrative_ontology:cs_axiom_grounding('4d8c4619-03dc-492c-b725-2f87ed8979d1', technical_register_precision_immunity, empirically_contingent).
narrative_ontology:cs_reference_frame('4d8c4619-03dc-492c-b725-2f87ed8979d1', bifurcated_domain_allocated_norms).
narrative_ontology:cs_drift_state('4d8c4619-03dc-492c-b725-2f87ed8979d1', post_vernacular_turn, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d8c4619-03dc-492c-b725-2f87ed8979d1', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, humanist_literati).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, aristocratic_patrons_of_letters).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, university_arts_faculties).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, scholastic_technical_authors).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, non_elite_latin_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose orations, poems, histories, and letters on classical models and police stylistic purity in salons, dedications, and print prefaces. Their market value depends on the scarcity that the classical standard maintains: few can certify Ciceronian finish, so those who can command patronage, professorships, and editorial authority. Leaving for vernacular composition was possible and some took it, but it forfeits the Latin prestige economy they dominate.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, humanist_literati, beneficiary,
    powerful, biographical, constrained, continental).

% Princes, cardinals, and civic oligarchies fund classicizing writers because a polished Latin court culture buys distinction at lower cost than military display. They collect the cultural return without bearing the compositional labor, and they can redirect patronage to vernacular writers or foreign courts if returns fall, which disciplines everyone else in the arrangement.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, aristocratic_patrons_of_letters, beneficiary,
    institutional, generational, arbitrage, continental).

% Set the grammar curriculum around classical auctores, examine candidates in classical composition, credential teachers, notaries, and clergy, and adjudicate in practice which texts count as literary (correctable against the ancients) and which count as technical (licensed in the evolved register). Tuition, fees, and institutional authority flow through keeping the gateway necessary; dismantling it would undercut the faculties' own mandate.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, university_arts_faculties, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, university_arts_faculties, beneficiary).

% Jurists, physicians, dialecticians, and natural philosophers writing consilia, quaestiones, commentaries, and practica in the evolved technical register. Their terminology predates and outruns the classical vocabulary, and their genres prize precision over periodic elegance. Humanist editors correct their works in reprint, patrons discount their style, and retraining in classical composition would cost years without improving the accuracy their professional writing requires. They are embedded in faculties, gloss traditions, and licensing structures they cannot leave without abandoning their profession.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, scholastic_technical_authors, payer,
    organized, biographical, trapped, continental).

% Boys from modest backgrounds enter grammar schools and arts faculties because Latin credentials are the only route to clerkships, benefices, notariates, and the professions. They must master classical composition drills regardless of the technical careers awaiting them, and failure marks them as barbari in the examiner's ledger. No alternative credentialing path exists inside the system.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, non_elite_latin_students, payer,
    powerless, immediate, trapped, regional).

% Poets, chroniclers, merchants, and artisan-authors working in Italian, French, Occitan, German, and English. They stand outside the Latin allocation debate entirely; they would object that the contest entrenches a learned monopoly and that their media already handle communication, including technical communication, without a bifurcated standard. They hold no seat in university disputation or Latin patronage networks.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, vernacular_writers_and_readers, excluded,
    moderate, biographical, mobile, national).

% Later analysts, from the descriptive turn in linguistics onward, reconstruct the history of both registers without a normative stake in either. They treat the bifurcated standard as data about language ideology: who got to define correctness, for which domains, and at whose expense. Their seat is outside the enforcement economy altogether.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, humanist_literati).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains differentiated written registers so each domain's communication stays predictable: holding literary and rhetorical composition to classical models gives evaluators, patrons, and readers shared criteria of excellence, while letting technical and practical writing retain its evolved forms keeps legal, medical, and philosophical content precise and transmissible across jurisdictions and generations.
% TRANSFER_FUNCTION: Moves prestige, patronage income, credential leverage, and editorial authority from technical writers and students toward classically certified literati and the faculties that certify them; moves correction labor (revising technical works toward classical norms) onto technical authors and their printers.
% ABSENT_VOICES: Vernacular writers and readers, and the artisan and surgical practitioners whose knowledge circulated in vernaculars, would object that the allocation debate presupposes a learned monopoly they were never invited to contest. They are outside the universities, the patronage networks, and the print gatekeeping that constitute the conversation.
% DISAPPEARANCE_RATIONALE: If the bifurcated allocation vanished overnight, credentialing and curricula would lose their organizing principle, the status premium of classically finished Latin would evaporate, technical writers would stop paying correction and editorial-deference costs, and patronage would reroute toward whatever excellence standard replaced the canon. The learned economy of western Europe was arranged around this allocation; it would reorganize.
% FOUNDING_PROBLEM: After antiquity, Latin fragmented into regional and professional varieties, and pan-European learned communication needed shared norms to stay intelligible across space and time. The founding problem was deciding what the ancient corpus was: a living parent whose changes could be licensed, or a dead standard to be restored. The hybrid answer allocated norms by domain, preserving technical continuity while keeping a classical bar for eloquence.
% FOUNDING_PROBLEM_CORROBORATION: No single external attestation settles it. Historical linguists assessing from outside the beneficiary set corroborate that the original integrative problem largely dissolved as vernaculars absorbed scholarly and literary functions after 1600; contemporaneous complaint records from law and medicine faculties corroborate that the cost side stayed live for those paying it; Neo-Latin defenders inside the system attested the problem as live. Corroboration is partial and split along the same lines as the interest structure.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62 at interval end) because a genuine register-coordination function offsets, without erasing, an asymmetric transfer of status and correction costs. Suppression (0.58) runs through curriculum mandates, examination gates, patronage discipline, and print-house style policing rather than physical coercion. Theater_ratio (0.42) reflects a substantial performative layer — Ciceronian display, purity policing beyond functional need, dedicatory classicizing — atop real register-maintenance work. Accessibility_collapse (0.48) is moderate because alternatives persist: vernacular media grew throughout the interval, and technical registers kept de facto license, so understanding the standard does not close every exit. Resistance (0.55) is real: scholastic faculties defended their terminology as precision rather than barbarism, and university inertia slowed humanist curricular reform for generations. The temporal series share one seven-point grid at fifty-year steps. Base extractiveness climbs with the humanist enforcement build-up (print, patronage, curricular reform), peaks around the Ciceronianist high tide circa 1500-1550, then partially recedes as vernaculars absorb literary production and technical Latin consolidates as a recognized specialty. Theater follows the same arc with a lag. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: machinery built up through 1550, then relaxed slightly as norms internalized and credible vernacular exit reduced the need for active policing.
 *
 * PERSPECTIVAL GAP:
 *   From the faculty and literati seats the arrangement presents as a well-run bilingual norm system: two registers, each judged by its proper criterion, administered by people qualified to tell them apart. From the technical authors' seat the same structure operates as a standing tax — correction labor, editorial humiliation, patronage discounts — levied on precisely the writing whose function the arrangement claims to protect. Students experience it as a gate: years of classical drill priced against credential access. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for humanist_literati (collect prestige rents, constrained exit keeps them invested), aristocratic_patrons_of_letters (pure collection with arbitrage exit — nearest the beneficiary pole), and university_arts_faculties (administer and collect through fees and credentialing authority, offset partly by the cost of running the boundary). Victim declarations drive high directionality for scholastic_technical_authors, amplified by trapped exit: their professional embedding means the full weight of the standard lands on them. Non_elite_latin_students sit near the target pole as well — powerless, short-horizon, no alternative credential path. Vernacular writers are excluded rather than coordinated; their mobility is exactly what keeps them outside the arrangement's reach. No directionality overrides are needed: the beneficiary/victim declarations plus exit options already differentiate every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents two opposite misreadings. Reading the arrangement as pure coordination (rope) would erase the identifiable payers — the technical writers pressured toward standards their genres cannot meet and the students drilled for gates they only need to pass. Reading it as pure extraction (snare) would erase the real coordination function: register differentiation genuinely kept legal argument precise and literary evaluation shared, and the technical license was a real concession, not cover. The mandatrophy risk sits past the interval edge: as vernaculars absorbed the literary function, the founding problem of pan-European learned coherence weakened, and the arrangement's persistence would increasingly rest on institutional momentum and theatrical classicizing rather than function — the contested founding_problem_status and the world_rearranges disappearance verdict together flag that drift for downstream lifecycle detection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_counterfactual,
    'How would classification shift if the continuity_reading or the rupture_reading were instantiated instead of this hybrid reading of the latin_correctness kernel?',
    'Compile and classify the two sibling constraint files against the same interval and compare victim sets, epsilon, and computed types across the three readings.',
    'The continuity reading would likely show negligible extraction (no status hierarchy, no corrected set); the rupture reading would likely show high extraction with a universalized victim set (every medieval writer corrected). The hybrid''s moderate profile and partial victim set are the structural delta of the domain-indexed allocation specifically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_counterfactual, conceptual, 'Committer-frame counterfactual across sibling readings of the latin_correctness kernel.').

omega_variable(
    domain_boundary_indeterminacy,
    'Where does the literary/rhetorical category end and the technical/practical begin — do dialogues, historiography, didactic verse, prefaces, and dedications fall under classical correction or technical license?',
    'Classify boundary cases by contemporaneous treatment (curricular placement, patronage expectations, editorial practice) rather than modern genre labels.',
    'Each expansion of the literary category moves additional technical writers into the corrected set and raises effective extraction on them; contraction relieves them. The enforcement seat''s discretion over the boundary is itself a lever of the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_indeterminacy, conceptual, 'Indeterminacy of the domain boundary that allocates normative authority.').

omega_variable(
    internalized_register_deference,
    'Is the pressure bearing on technical writers structural (examinations, patronage discipline, editorial correction) or internalized (self-description as barbarous, reluctance to publish, preemptive apology for style)?',
    'Track the publication behavior and self-presentation of technical authors who leave university jurisdictions or shift to vernacular composition; deference persisting after the barriers are removed indicates an internalized component.',
    'An internalized component raises effective pressure above the structural measure and makes it outlast institutional reform; a purely structural profile means removing the gates removes the burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_register_deference, empirical, 'Structural versus internalized mechanism of the pressure on technical writers.').

omega_variable(
    founding_problem_liveness,
    'At interval end, does the founding problem (supranational learned coherence with domain-fit norms) still demand this arrangement, or does the arrangement persist by institutional momentum?',
    'Compare the growth of vernacular scholarly publication against the persistence of Latin credentialing requirements after 1600; credentialing persisting where the communicative need has migrated indicates momentum dominance.',
    'A dead founding problem under a persisting arrangement signals drift toward inertial-theatrical maintenance beyond the interval and reframes late-period extraction as legacy cost rather than functional price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether the arrangement''s founding problem remains live at the end of the interval.').

omega_variable(
    vernacular_exit_damping,
    'Does the growing vernacular option dampen the arrangement''s pressure (credible exit disciplining the gatekeepers) or merely relocate the status hierarchy onto the new languages?',
    'Compare status premiums for Latin versus vernacular composition within the same courts and faculties across 1550-1650.',
    'Damping supports the moderate-extraction profile (live alternatives cap the asymmetry); relocation suggests the hierarchy is portable and the Latin-specific measure understates the underlying structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vernacular_exit_damping, empirical, 'Whether vernacular exit caps extraction or exports the hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 1350, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_hybrid_tr_t1350, latin_correctness__hybrid_reading, theater_ratio, 1350, 0.25).
narrative_ontology:measurement(latin_hybrid_tr_t1400, latin_correctness__hybrid_reading, theater_ratio, 1400, 0.3).
narrative_ontology:measurement(latin_hybrid_tr_t1450, latin_correctness__hybrid_reading, theater_ratio, 1450, 0.36).
narrative_ontology:measurement(latin_hybrid_tr_t1500, latin_correctness__hybrid_reading, theater_ratio, 1500, 0.45).
narrative_ontology:measurement(latin_hybrid_tr_t1550, latin_correctness__hybrid_reading, theater_ratio, 1550, 0.47).
narrative_ontology:measurement(latin_hybrid_tr_t1600, latin_correctness__hybrid_reading, theater_ratio, 1600, 0.45).
narrative_ontology:measurement(latin_hybrid_tr_t1650, latin_correctness__hybrid_reading, theater_ratio, 1650, 0.42).

% Extraction over time
narrative_ontology:measurement(latin_hybrid_be_t1350, latin_correctness__hybrid_reading, base_extractiveness, 1350, 0.45).
narrative_ontology:measurement(latin_hybrid_be_t1400, latin_correctness__hybrid_reading, base_extractiveness, 1400, 0.5).
narrative_ontology:measurement(latin_hybrid_be_t1450, latin_correctness__hybrid_reading, base_extractiveness, 1450, 0.57).
narrative_ontology:measurement(latin_hybrid_be_t1500, latin_correctness__hybrid_reading, base_extractiveness, 1500, 0.66).
narrative_ontology:measurement(latin_hybrid_be_t1550, latin_correctness__hybrid_reading, base_extractiveness, 1550, 0.68).
narrative_ontology:measurement(latin_hybrid_be_t1600, latin_correctness__hybrid_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(latin_hybrid_be_t1650, latin_correctness__hybrid_reading, base_extractiveness, 1650, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(latin_hybrid_su_t1350, latin_correctness__hybrid_reading, suppression_requirement, 1350, 0.4).
narrative_ontology:measurement(latin_hybrid_su_t1400, latin_correctness__hybrid_reading, suppression_requirement, 1400, 0.46).
narrative_ontology:measurement(latin_hybrid_su_t1450, latin_correctness__hybrid_reading, suppression_requirement, 1450, 0.52).
narrative_ontology:measurement(latin_hybrid_su_t1500, latin_correctness__hybrid_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(latin_hybrid_su_t1550, latin_correctness__hybrid_reading, suppression_requirement, 1550, 0.62).
narrative_ontology:measurement(latin_hybrid_su_t1600, latin_correctness__hybrid_reading, suppression_requirement, 1600, 0.6).
narrative_ontology:measurement(latin_hybrid_su_t1650, latin_correctness__hybrid_reading, suppression_requirement, 1650, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Latin correctness' decomposes under the epsilon-invariance principle into three readings of one kernel, each a separate story with its own epsilon, beneficiary/victim structure, and classification. This file is the hybrid reading (domain-indexed allocation; moderate extraction; partial victim set). The continuity reading (universal license for inherited forms) and the rupture reading (universal correction toward reconstructed classical norms) differ from it pointwise: the hybrid grants technical-domain legitimacy the rupture reading denies and withholds literary-domain license the continuity reading grants. Upstream/downstream: the continuity reading supplies the descriptive genealogy the hybrid partially adopts; the rupture reading supplies the corrective ideal the hybrid partially resists. Each sibling links back to this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
