% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Classical Purity Standard — Rupture Reading (Post-Classical Usage as Corruption)
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   From the fourteenth-century revival of ancient letters through the school
 *   reforms of the nineteenth century, the learned world ran on a single
 *   answer to the question of correct Latin: the usage of the canonical
 *   ancient authors, reconstructed from the manuscripts, is the measure, and
 *   everything written between antiquity and the revival is a falling-away
 *   from it. The arrangement rebuilt European schooling around imitation of
 *   Cicero and Virgil, refounded editing on manuscript comparison against
 *   ancient idiom, and reclassified a millennium of learned writing —
 *   theology, law, medicine, natural philosophy — as barbarous. It
 *   coordinated real work: texts were recovered, emended, and read across
 *   borders against a shared benchmark. It also moved standing: appointments,
 *   printing, and fees flowed to those who could pay the long apprenticeship
 *   the standard demanded, while the post-classical tradition and everyone
 *   formed in it absorbed the discredit. KEY AGENTS (by structural
 *   relationship): - humanist_academy_network: agenda-setter and collector
 *   (institutional/arbitrage) — sets the models, distributes the rewards -
 *   classical_philologists: primary beneficiary (organized/identity_locked) —
 *   collects authority and livelihood from the reconstruction enterprise -
 *   elite_classical_schools: beneficiary (institutional/constrained) — sells
 *   mastery of the models - medieval_scholars: primary target
 *   (moderate/identity_locked) — their life's work is the condemned object -
 *   practical_latin_users: target with partial exit (moderate/constrained) —
 *   bear the charge of barbarism, leak to the vernaculars -
 *   non_elite_students: target (powerless/trapped) — pay the apprenticeship
 *   that gates advancement - vernacular_authors: excluded voice
 *   (moderate/mobile) - historical_linguists: analytical observer
 *   (institutional/analytical) — sees the whole attested history
 *
 * KEY AGENTS:
 *   - humanist_academy_network: agenda-setter and collector (institutional/arbitrage) — sets the models, distributes the rewards
 *   - classical_philologists: primary beneficiary (organized/identity_locked) — collects authority and livelihood from the reconstruction enterprise
 *   - elite_classical_schools: beneficiary (institutional/constrained) — sells mastery of the models
 *   - medieval_scholars: primary target (moderate/identity_locked) — their life's work is the condemned object
 *   - practical_latin_users: target with partial exit (moderate/constrained) — bear the charge of barbarism, leak to the vernaculars
 *   - non_elite_students: target (powerless/trapped) — pay the apprenticeship that gates advancement
 *   - vernacular_authors: excluded voice (moderate/mobile)
 *   - historical_linguists: analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.66).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.68).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Purity Standard — Rupture Reading (Post-Classical Usage as Corruption)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, '8ab9232f-5655-4a65-af98-026154a45f82').
narrative_ontology:cs_kernel_codification('8ab9232f-5655-4a65-af98-026154a45f82', fixed_text).
narrative_ontology:cs_authority_grounding('8ab9232f-5655-4a65-af98-026154a45f82', lineage).
narrative_ontology:cs_interpretation_layer_present('8ab9232f-5655-4a65-af98-026154a45f82').
narrative_ontology:cs_reading_relation('8ab9232f-5655-4a65-af98-026154a45f82', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('8ab9232f-5655-4a65-af98-026154a45f82', latin_correctness__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('8ab9232f-5655-4a65-af98-026154a45f82', foundational, classical_corpus_binding_norm).
narrative_ontology:cs_axiom_status(classical_corpus_binding_norm, holdable).
narrative_ontology:cs_axiom_grounding('8ab9232f-5655-4a65-af98-026154a45f82', classical_corpus_binding_norm, deontological).
narrative_ontology:cs_axiom('8ab9232f-5655-4a65-af98-026154a45f82', secondary, post_classical_usage_is_corruption).
narrative_ontology:cs_axiom_status(post_classical_usage_is_corruption, holdable).
narrative_ontology:cs_axiom_grounding('8ab9232f-5655-4a65-af98-026154a45f82', post_classical_usage_is_corruption, conventional).
narrative_ontology:cs_reference_frame('8ab9232f-5655-4a65-af98-026154a45f82', golden_age_canonical_standard).
narrative_ontology:cs_drift_state('8ab9232f-5655-4a65-af98-026154a45f82', post_historical_linguistics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ab9232f-5655-4a65-af98-026154a45f82', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, elite_classical_schools).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_academy_network).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, practical_latin_users).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, non_elite_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learned academies, papal and princely chanceries, and later the school-and-university establishment that decide which ancient authors count as models, train the teachers who transmit them, and refuse advancement, prizes, or publication to writing that departs from them. Patronage, printing contracts, and professorships pass through its hands, and it answers to no body outside the republic of letters it staffs.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, humanist_academy_network, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, humanist_academy_network, beneficiary).

% Scholars whose livelihood, rank, and subject matter consist in recovering, emending, and expounding the ancient texts. The stricter the measure of correctness, the scarcer and more valuable their skills; their journals, chairs, and edition series presuppose that the ancient canon remains the measure of all Latin. A lifetime of accumulated authority is bound up with the canon's primacy, and retraining outside it would mean starting again at the bottom of a different field.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classical_philologists, beneficiary,
    organized, biographical, identity_locked, continental).

% Grammar schools, gymnasia, and colleges whose fees and reputation rest on promising mastery of the ancient models. Entrance examinations and prize systems keyed to classical proficiency keep demand for their product high; relaxing the requirement would undercut the credential they exist to sell, so they defend the curriculum even as other subjects crowd it.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, elite_classical_schools, beneficiary,
    institutional, generational, constrained, national).

% University masters, canon lawyers, physicians, and commentators who read, teach, and write in the post-classical learned language their formation gave them. Under the ancient-sources-only ruling their libraries become evidence of decline, their technical vocabulary becomes barbarism, and their students, printers, and appointments migrate to classically formed rivals. Leaving the tradition would mean abandoning the questions they spent their lives on; staying means absorbing the discredit attached to it.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_scholars, payer,
    moderate, generational, identity_locked, continental).

% Working writers of Latin — physicians compiling pharmacopoeias, jurists, administrators, missionaries — who need a serviceable learned register more than stylistic perfection. Every new thing they must name (gunpowder, syphilis, magnetic variation) exposes them to the charge of barbarism, and the arbiters offer no dispensation for necessity. Many respond by moving their work into vernacular print, a door the standard's keepers cannot close behind them.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, practical_latin_users, payer,
    moderate, biographical, constrained, continental).

% Children of families without leisure or books who must absorb an inflected classical idiom through years of rote imitation of ancient models before any advanced study, clergy, or profession opens to them. Failure closes the path entirely; success marks them as exceptional rather than equal, and the price is paid in the years childhood and adolescence might otherwise have spent on other preparation.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, non_elite_students, payer,
    powerless, biographical, trapped, national).

% Writers in Italian, French, English, and German who demonstrate daily that demanding thought can be carried without the ancient models. They are not invited to the councils where correctness is defined; their flourishing is treated as a separate and lesser phenomenon rather than as evidence bearing on the standard itself.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_authors, excluded,
    moderate, biographical, mobile, national).

% Scholars of language change who compare Latin's whole attested history, including the classical phase itself, which they treat as one moment of a moving language rather than a terminus. From that seat the label 'corruption' names no process any language obeys; they publish the comparison and the critique, but sit outside the examination boards and curriculum committees that administer the standard.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, historical_linguists, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single fixed textual norm gives the learned world one learnable target: ancient texts can be recovered, emended, and interpreted against a shared benchmark; a common written register works across regions and generations; editors, teachers, and correspondents know what they are each aiming at.
% TRANSFER_FUNCTION: Moves linguistic legitimacy, scholarly standing, curricular hours, and patronage away from the post-classical tradition and the people formed in it, toward those who complete the long classical apprenticeship; moves students' years toward ancient-model imitation; moves publishing credibility to houses and journals that enforce the ancient idiom.
% ABSENT_VOICES: Vernacular authors and working users of Latin were never seated where correctness was defined; the medieval authors themselves could not answer the charge brought against their language; the children ground through the grammar schools bore its costs without any voice in setting them.
% DISAPPEARANCE_RATIONALE: If the ruling vanished overnight, curricula would shed the imitation exercises, a millennium of scholastic and technical writing would re-enter the canon as legitimate Latin, editorial authority would diffuse from the classical seminars, and the credential premium attached to classical mastery would collapse — the whole architecture of European learned advancement would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Late-medieval learned Latin had fragmented into regional and professional varieties, and ancient texts were being misread through centuries of accumulated gloss. The reforming generation answered with a return to the sources: rebuild the ancient norm from the manuscripts and treat everything in between as debris. The founding problem was reliable access to antiquity and a unified learned register.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists and the medieval-editing tradition (from the Monumenta Germaniae Historica onward) corroborate from outside the benefiting parties that the drift problem was real but that the return-to-the-sources remedy overshot into blanket condemnation; Neo-Latin scholarship documents that the unified-register half of the founding problem died with vernacular ascendancy. The classical establishment itself attests that the interpretive half remains live. No party outside the dispute is indifferent, which is itself the signal that the status is contested rather than settled.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.66 after peaking near 0.73: the rate charged for learned standing stayed decoupled from the communicative service rendered, because the arbiters controlled both the currency and the exchange. Suppression (0.68) reflects the enforcement machinery — examination boards, curriculum mandates, editorial refusal, and open ridicule of 'barbarous' style — rather than participant preference; the arrangement could not survive on admiration alone, since the post-classical tradition kept offering a cheaper alternative. Theater (0.40) captures the growing share of activity devoted to purity display — Ciceronian one-upmanship, barbarism-hunting in colleagues' prose — relative to the textual recovery that justified the enterprise; the ratio climbs late as the function shrinks faster than the performance. Accessibility_collapse (0.58) is moderate: the vernacular door and the hybrid registers remained open throughout, at rising career cost. Resistance (0.55) records the scholastic counterattacks, the universities that held out against the new curriculum, and the nineteenth-century rehabilitation of medieval philology. Claim and metrics are independent authored facts: I claim tangled_rope because a genuine coordination function (source-based recovery of ancient texts, a stable cross-regional register) demonstrably coexists with asymmetric sanctioning of identifiable seats; the metric values describe the arrangement's actual operation, and the engine computes per-seat types from the structural data. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point. The trajectory is not cyclical: rise to a mid-interval plateau, partial decline as vernacular exit drained the paying population, then renewed hardening inside the shrinking enclave — enforcement intensifying as jurisdiction contracted.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as vocation and stewardship: it built the schools, recovered the texts, and sees only the coordination. The paying seats experience the same structure as a tribunal they never consented to: the medieval scholar watches his life's work reclassified as decay, the working physician discovers that naming a new disease is a fault, the student learns that the price of a future is a decade of imitation exercises. The excluded vernacular seat sees irrelevance posing as necessity. The analytical observer sees a category error administered with great skill. The engine computes these divergent classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The academy network and the schools sit near the beneficiary end: rewards flow to them by design, and their exit options (arbitrage for the network, franchise value for the schools) insulate them from the costs they impose. The philologist seat is nominally a pure collector but carries identity_locked exit — a career constituted by the canon cannot be cashed out elsewhere — which is why the arrangement's defenders defend it hardest precisely where its jurisdiction has collapsed. Medieval scholars sit nearest the full-target end: the condemned object is their entire inheritance, and neither their training nor their questions convert to the winning side. Practical users bear heavy assessment but their d is tempered by the vernacular exit that widened across the interval — which is exactly why aggregate extractiveness dips mid-late series before the enclave hardens. Students are assessed near-full with no exit at all; their powerlessness is structural, and their only historical recourse was the coalition that never formed: scholastic masters defended their own turf, working users defected individually to the vernaculars, and the two target populations never combined.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetrical errors. Reading the whole arrangement as pure predation erases the real achievement — the recovery, emendation, and cross-border readability of ancient texts, work that no rival program accomplished as well. Reading it as pure service erases the tribunal — the millennium of writing condemned, the careers redirected, the gate erected at the schoolhouse door. On mandate: the founding problem splits. Reliable access to antiquity remains live inside philology and will not expire; the unified learned register the arrangement was also built to supply is dead, killed by the vernaculars it inadvertently armed. Hence founding_problem_status is contested rather than resolved, and mandatrophy_resolved is deliberately not declared: half the mandate outlived its function while the other half still performs. Rising theater late in the series signals the atrophy risk — a growing share of activity defends the standard's dignity rather than doing its work — but a concentrated collector seat remains, so the profile has not reached the no-beneficiary condition that would mark a different type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the rupture_reading instantiation of the latin_correctness kernel: the ancient corpus is a binding norm for all Latin use and post-classical usage is corruption. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Not resolvable by data alone. Adopting the continuity reading removes the victim set entirely (post-classical usage legitimate, the burden collapses toward coordination cost); adopting the hybrid reading partitions the norm by domain (victims shrink to literary-register actors). The disagreement sits in one structural element: the normative status of post-classical usage — binding standard versus historical object.',
    'Classification is reading-indexed: tangled_rope under this reading; approximately rope under the continuity reading; low-burden rope or mild tangled_rope under the hybrid reading. Cross-reading comparison must run through the linked sibling stories, never by averaging their values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the latin_correctness kernel; sibling readings relocate the norm''s binding force and thereby change the victim set and the load-bearing classification.').

omega_variable(
    corruption_vs_language_change,
    'Is post-classical Latin ''corruption'' — decay measurable against a fixed norm — or ordinary language change of the kind that produced classical Latin itself out of archaic Latin?',
    'Apply the corruption test uniformly across periods: if every attested phase fails it equally, including the classical phase judged against archaic usage, the verdict describes a preference rather than a process. Comparative-historical method supplies the uniform application.',
    'If change, the condemnation clause loses its factual footing; the coordination function (stable reference texts) separates cleanly from the sanctioning function, and the measured burden shifts from necessary coordination cost to imposed sanction — strengthening the case for the hybrid partition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corruption_vs_language_change, empirical, 'Whether the corruption verdict names a real linguistic process or a status preference dressed as one.').

omega_variable(
    norm_naturalness_ambiguity,
    'Is the classical standard a self-evident benchmark — the ancients'' practice speaking for itself — or a constructed arrangement whose enforcement benefits identifiable seats: the trained interpreters, the schools selling mastery, the networks distributing patronage?',
    'Counterfactual and historical test: observe what the norm does where enforcement lapses (late-antique practical registers show usage continuing without the arbiters); ask whether the standard would persist if no seat collected standing, fees, or patronage from administering it.',
    'If constructed-with-beneficiaries, the arrangement belongs with enforced hybrids rather than natural benchmarks, and its costs are attributable to choices rather than to the nature of language; the natural-law framing that sometimes shields it from scrutiny fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_naturalness_ambiguity, conceptual, 'Self-evident benchmark versus enforced construction with identifiable beneficiaries.').

omega_variable(
    guardian_identity_fusion,
    'For the professionally trained scholarly seat, is defense of the ancient standard career calculation or identity fusion — has the scholar''s self-concept become constituted by mastery of the canon?',
    'Post-exit trajectory: track scholars who moved to vernacular subjects or medievalist work; if they report loss of standing in their own eyes rather than merely loss of income, fusion rather than interest drives maintenance.',
    'If fused, enforcement persists where material incentives have faded, predicting the enclave-hardening visible late in the measurement series; the guardian seat''s computed relationship to the arrangement shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardian_identity_fusion, empirical, 'Identity-lock dynamics among the standard''s professional guardians.').

omega_variable(
    access_gate_component,
    'How much of the standard''s social work is gating — converting family resources into credentials through a long apprenticeship — rather than communicating?',
    'Compare cohorts educated before and after classical requirements were dropped from advanced study: if learned precision and cross-lingual competence did not measurably decline, the gate component dominated the pedagogic justification.',
    'If gating dominates, the arrangement''s persistence tracks class reproduction rather than textual fidelity, and remedies aimed at teaching quality miss the operative mechanism entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_gate_component, empirical, 'Credential-gate share of the standard''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lati_tr_t10, latin_correctness__rupture_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(lati_tr_t20, latin_correctness__rupture_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(lati_tr_t30, latin_correctness__rupture_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__rupture_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(lati_tr_t50, latin_correctness__rupture_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(lati_tr_t60, latin_correctness__rupture_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__rupture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lati_be_t10, latin_correctness__rupture_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lati_be_t20, latin_correctness__rupture_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(lati_be_t30, latin_correctness__rupture_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(lati_be_t40, latin_correctness__rupture_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(lati_be_t50, latin_correctness__rupture_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(lati_be_t60, latin_correctness__rupture_reading, base_extractiveness, 60, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__rupture_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lati_su_t10, latin_correctness__rupture_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(lati_su_t20, latin_correctness__rupture_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(lati_su_t30, latin_correctness__rupture_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(lati_su_t40, latin_correctness__rupture_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(lati_su_t50, latin_correctness__rupture_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(lati_su_t60, latin_correctness__rupture_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'correct Latin' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that cannot share one story — measuring the arrangement under the rupture reading yields high burden with a large victim set; under the continuity reading the victim set vanishes and the burden approaches coordination cost; under the hybrid reading the victims shrink to literary-register actors. This story is the rupture member. Historical dependency runs from this reading outward: the rupture program constructed the fixed canon and the editorial apparatus that the continuity and hybrid readings subsequently react to and operate on, so this story sits upstream of both siblings in the family graph. Each member links the others through affects_constraints; no member's values may be averaged into another's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
