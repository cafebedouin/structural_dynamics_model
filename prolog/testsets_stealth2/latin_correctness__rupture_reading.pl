% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Classical Latin as Fixed Textual Standard — Rupture Reading
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   From Petrarch's generation onward, Italian and then European humanists
 *   rebuilt Latin from the ancient sources: manuscripts collated,
 *   inscriptions and coins read, Cicero's syntax codified, and a norm
 *   pronounced — correct Latin is the Latin of the classical corpus,
 *   recovered by specialist reconstruction, and the Latin that grew up after
 *   antiquity (scholastic theology, medieval law and medicine, chancery
 *   practice) is corruption. This file instantiates the RUPTURE READING of
 *   the latin_correctness kernel only: the claim that the standard is fixed
 *   in the ancient texts and that post-classical usage is deficient. Per the
 *   epsilon-invariance principle, the sibling readings (continuity_reading:
 *   medieval Latin as legitimate organic continuation; hybrid_reading:
 *   classical norms for literary domains, medieval forms legitimate for
 *   technical domains) are separate constraint files with their own epsilon,
 *   beneficiaries, and victims, linked through network.affects_constraints.
 *   The rupture reading is the most extractive of the three because its
 *   verdict is universal: it denies legitimacy to post-classical practice
 *   everywhere, converting linguistic difference into deficiency and routing
 *   authority, publication, and curricular resources to the reconstruction
 *   complex. Assumptions stated: the interval runs from the early humanist
 *   polemic (c. 1350) to the mid-20th century contraction of compulsory
 *   classical schooling (1950); the standing arrangement under contest is the
 *   historical regime in which the ancient corpus functioned as the binding
 *   norm, and epsilon is authored for that arrangement as the rupture seat
 *   itself encounters it.
 *
 * KEY AGENTS:
 *   - classical_philologists: agenda setter and principal beneficiary (institutional/identity_locked) — reconstruct the standard from ancient sources, edit the canon, and collect the credential premium their monopoly on reconstruction confers
 *   - humanist_educational_establishment: enforcement arm (institutional/constrained) — runs the schools and examinations through which the standard reaches every educated career
 *   - neo_latin_literary_elite: secondary beneficiary (powerful/arbitrage) — converts classical mastery into patronage and fame, with vernacular markets as an alternative code
 *   - medieval_manuscript_scholars: primary target (moderate/trapped) — their sources and skills are marked corrupt; retraining means abandoning the archives only they can read
 *   - scholastic_technical_writers: organized target (organized/constrained) — theologians, jurists, physicians whose necessary terminology has no classical precedent and whose forms are corrected by classical-trained editors
 *   - vernacular_technical_clerks: diffuse target (powerless/constrained) — chancery and notarial writers whose working Latin is judged debased whenever it surfaces before learned readers
 *   - practical_latin_users: excluded voice (moderate/mobile) — diplomats, merchants, manual-writers who were never consulted and mostly answered by leaving Latin altogether
 *   - modern_historical_linguists: analytical observer (analytical/analytical) — sees classical and medieval Latin as phases of one language's history and can view the whole structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.72).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.58).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin as Fixed Textual Standard — Rupture Reading").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, '02db44f1-a65c-432f-8e04-09ed994d24ec').
narrative_ontology:cs_kernel_codification('02db44f1-a65c-432f-8e04-09ed994d24ec', fixed_text).
narrative_ontology:cs_authority_grounding('02db44f1-a65c-432f-8e04-09ed994d24ec', lineage).
narrative_ontology:cs_interpretation_layer_present('02db44f1-a65c-432f-8e04-09ed994d24ec').
narrative_ontology:cs_reading_relation('02db44f1-a65c-432f-8e04-09ed994d24ec', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('02db44f1-a65c-432f-8e04-09ed994d24ec', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('02db44f1-a65c-432f-8e04-09ed994d24ec', foundational, classical_corpus_fixes_correctness).
narrative_ontology:cs_axiom_status(classical_corpus_fixes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('02db44f1-a65c-432f-8e04-09ed994d24ec', classical_corpus_fixes_correctness, conventional).
narrative_ontology:cs_axiom('02db44f1-a65c-432f-8e04-09ed994d24ec', foundational, medieval_divergence_is_deficiency).
narrative_ontology:cs_axiom_status(medieval_divergence_is_deficiency, holdable).
narrative_ontology:cs_axiom_grounding('02db44f1-a65c-432f-8e04-09ed994d24ec', medieval_divergence_is_deficiency, empirically_contingent).
narrative_ontology:cs_axiom('02db44f1-a65c-432f-8e04-09ed994d24ec', secondary, correctness_requires_reconstruction_expertise).
narrative_ontology:cs_axiom_status(correctness_requires_reconstruction_expertise, holdable).
narrative_ontology:cs_axiom_grounding('02db44f1-a65c-432f-8e04-09ed994d24ec', correctness_requires_reconstruction_expertise, instrumental).
narrative_ontology:cs_reference_frame('02db44f1-a65c-432f-8e04-09ed994d24ec', classical_canon_fixed_standard).
narrative_ontology:cs_drift_state('02db44f1-a65c-432f-8e04-09ed994d24ec', post_medieval_latin_rehabilitation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('02db44f1-a65c-432f-8e04-09ed994d24ec', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_educational_establishment).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, neo_latin_literary_elite).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_manuscript_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, scholastic_technical_writers).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_technical_clerks).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, philological_reconstruction_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, ciceronian_normativity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reconstruct classical usage from manuscripts, inscriptions, and early prints; edit the ancient texts; write the grammars and lexica that define correct form; sit on the editorial boards and examination boards that reject non-classical usage. Their rare skill — knowing what Cicero would have said — commands a premium only so long as ancient usage remains the required measure. Their entire training is organized around the ancient corpus, so leaving the arrangement would mean abandoning the object of a lifetime's formation.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classical_philologists, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, classical_philologists, beneficiary).

% Runs the schools and university arts faculties that teach the classical authors and drill composition to the classical measure, from Renaissance gymnasia through the state lycee and gymnasium curricula of the nineteenth century. Collects enrollment, fees, and state support by certifying classical competence, and administers the examinations through which the standard reaches every educated career. Its staff are themselves products of the curriculum they enforce, and pivoting to another pedagogical foundation would strand that formation.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, humanist_educational_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, humanist_educational_establishment, beneficiary).

% Composes Latin poetry, orations, and correspondence in classical style for courtly and republic-of-letters audiences; wins patronage, office, and lasting fame by demonstrated mastery. Because the same rhetorical training sells in the vernacular markets too, its members can move between codes and take prestige in either — Petrarch's Italian verse beside his Latin epic is the type case.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, neo_latin_literary_elite, beneficiary,
    powerful, biographical, arbitrage, continental).

% Studies charters, scholastic summae, chronicles, and correspondence written in post-classical Latin. Their sources fall outside the sanctioned canon, and their philological skill is rated as imperfect command of the language rather than command of a different historical layer. Retraining into classical specialization would mean abandoning the archives that only they can read, and the reputational discount follows their work wherever it is published.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_manuscript_scholars, payer,
    moderate, biographical, trapped, continental).

% University theologians, jurists, and physicians composing in the technical Latin of their traditions — terminology for transubstantiation, legal obligation, compound medicines — none of which antiquity ever needed. Classical-trained editors correct or ridicule their forms, and reaching prestige venues requires recasting their working language into classical dress that distorts it. They hold chairs and faculties, so unlike the clerks below they can and do mount organized institutional resistance.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, scholastic_technical_writers, payer,
    organized, biographical, constrained, continental).

% Chancery scribes, notaries, and municipal record-keepers whose working Latin mixes post-classical forms with vernacular calques because the business at hand — land transfers, tolls, guild rules — has no classical precedent. They have no seat in the correctness debate, and their products are marked as debased whenever they surface before learned readers. Their horizon is the next filing deadline, not the fate of a language.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_technical_clerks, payer,
    powerless, immediate, constrained, regional).

% Diplomats, merchants, and writers of artisans' manuals who use simplified working Latin for contracts, letters, and instructions. Nobody consulted them when the standard was set; their needs — speed, clarity, formulaic safety — never entered the norm-setting conversation. Most answered by drifting into the emerging vernacular languages, which is why they figure here as an absent voice rather than as a paying party.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, practical_latin_users, excluded,
    moderate, immediate, mobile, continental).

% Analyzes classical and medieval Latin as successive phases of one language's history, with change driven by ordinary mechanisms rather than decay. Holds no stake in either the classical credential or the medieval rehabilitation beyond disciplinary standing, and can see the whole structure — including what each camp's framing of the dispute conceals from itself.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, modern_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single fixed reference standard for written Latin across centuries and polities: one common corpus against which usage, texts, and editions can be checked, enabling reliable long-distance scholarly communication and stable critical editing of ancient works.
% TRANSFER_FUNCTION: Moves scholarly authority, publication access, curricular time, and prestige from practitioners of post-classical Latin — medievalists, scholastic technicians, chancery clerks — toward specialists in classical reconstruction; and moves the linguistic authority of the ancient past into the hands of the present-day gatekeepers who control its recovery.
% ABSENT_VOICES: Practical users of working Latin (diplomats, merchants, manual-writers) had no seat in the correctness debate; medieval authors themselves cannot answer the charge of corruption, and their texts were emended and marginalized without their consent; advocates of the vernaculars stood wholly outside the Latin conversation while the standard was being fixed. Their absence made the standard's unanimity cheaper than it looks.
% DISAPPEARANCE_RATIONALE: If the rupture standard vanished overnight, medieval texts would regain unqualified legitimacy and the medievalist discount would lift; technical Latin would stop being corrected into classical dress; the classical-specialist credential would deflate toward ordinary textual craftsmanship; and school curricula built around classical composition would reorganize. The arrangements of every named seat depend on the standard's standing.
% FOUNDING_PROBLEM: After the collapse of Roman institutions, written Latin drifted regionally and innovated without check; by the late Middle Ages usage varied widely across chanceries, universities, and religious orders, and the connection to the classical corpus was fraying. The rupture reading was built to solve this: recover the classical standard from the ancient sources and hold learned usage to it, restoring a stable, authoritative international language of scholarship.
% FOUNDING_PROBLEM_CORROBORATION: Historians of education corroborate the founding drift problem and the humanist response from outside the benefiting parties, as do early printed grammars and school statutes that record the restorative aim independently. Adversarial testimony agrees on the problem while disputing the remedy: medieval Latinists concede the classical corpus's unmatched normative richness while rejecting the corruption verdict, and historical linguists attest that the living-drift problem the standard targeted died with living Latin composition itself. No party outside the classical establishment attests that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is 0.72: the standard transfers scholarly authority, publication access, and curricular resources from every post-classical practice to the reconstruction complex, and the transfer is large because the verdict is universal — all divergence from the ancient corpus counts as fault, so every technical domain that antiquity did not furnish vocabulary for is structurally condemned. The value is authored for the standing arrangement from the rupture seat: the reading grants the standard's legitimacy while its own descriptive concessions (the training burden of an extinct language, the distortion of technical registers forced into classical dress, the dispossessed medieval scholarship) register the magnitude of the movement. It is high but not maximal because part of the standard's operation is genuine service — a stable reference corpus, editorial reliability, four centuries of mutually intelligible learned writing — which the reading itself counts as benefit. Suppression (0.58) is a raw structural property, unscaled by power or scope: it records the enforcement machinery (editorial gatekeeping, curricular requirement, corrective ridicule) as it stands at interval end, decayed from its 19th-century peak but still operative wherever classical norms govern editing and examination. Theater_ratio (0.30): a substantial minority of activity is purity display — Ciceronian competition, denunciation of barbarisms, ceremonial Latinity — while reconstruction and editing remain functional. Accessibility_collapse (0.60): within prestige writing, alternatives collapse once the standard is understood, but exits existed (the vernaculars, the ecclesiastical track), so collapse is partial. Resistance (0.60): sustained — the humanist-scholastic polemics, the universities' institutional counterweight, and the 20th-century rehabilitation of medieval Latin studies. The temporal series run on one shared seven-point grid (1350–1950 at century steps) for all three tracked metrics; the arc is rise-and-decay, not cyclical: extraction and enforcement ratchet up with print, the Jesuit colleges, and the state examination regimes, peak around 1850, then contract as Latin composition dies and medieval studies rehabilitate its object.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat the arrangement is restoration: the standard was always there in the texts, and enforcement merely strips away accretion — nothing is taken from anyone that was rightfully theirs. From the trapped medievalist seat the same arrangement is dispossession: a tradition's legitimacy confiscated by the people who control the definition of legitimacy, with the confiscation described as scholarship. From the arbitrage neo-Latin elite seat it is a market being won. The organized scholastic seats additionally show that coalition matters: because theologians and jurists held chairs and faculties, they mounted real institutional resistance (part of the authored 0.60 resistance), unlike the powerless chancery clerks who absorbed the verdict silently. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: classical_philologists and the educational establishment are subsidized by the standard (their scarce skill is valuable only because the standard demands it), and the neo-Latin elite's arbitrage exit pulls its derived d toward the beneficiary extreme — it can take its prestige elsewhere at low cost. Victims derive high directionality, amplified by exit structure: medieval_manuscript_scholars are trapped (material and training lock them to the devalued layer), scholastic_technical_writers are constrained (they need Latin for international discourse but cannot meet the classical measure without distorting their subject matter), and vernacular_technical_clerks are powerless with constrained exits. Practical_latin_users illustrate the exit boundary of the arrangement: their mobility meant they did not stay to pay — they left the jurisdiction of the standard for the vernaculars, which is why they are authored as excluded voices rather than payers. Continental spatial scope makes compliance verification harder across polities and presses, so the engine scales effective extraction modestly upward from the base epsilon; suppression is passed through unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: after Roman institutions collapsed, written Latin drifted regionally, and by the late Middle Ages the link to the classical corpus was fraying enough to threaten learned communication. The rupture standard solved that problem — and then overshot into a universal corruption verdict whose original object (living drift) has largely vanished now that nobody composes Latin as a working language. Classifying as tangled_rope keeps both facts visible: decomposing to pure rope would hide the dispossessed medieval and technical seats; decomposing to pure snare would erase the genuine coordination the standard performed for four centuries of scholarship. The mandatrophy watch is live: the measurement series is built to catch the turn from enforced standard to theatrically maintained relic — falling suppression_requirement after 1850 alongside sticky theater_ratio is the signature. If enforcement decays to pure ceremony while the corruption verdict persists rhetorically, the structure drifts toward piton; if the verdict is abandoned and only the editorial core remains, it resolves toward rope confined to textual criticism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This file authors one reading (rupture_reading) of the latin_correctness kernel; would instantiating continuity_reading or hybrid_reading instead yield a structurally different constraint with different epsilon, beneficiary sets, and victim sets?',
    'Generate the sibling files and compare computed classifications across the family; all three are linked via network.affects_constraints so contamination and coupling analyses can run across them.',
    'Under continuity_reading, epsilon drops sharply (no corruption verdict, no dispossessed medieval seats) and the structure approaches rope; under hybrid_reading, extraction concentrates in literary gatekeeping and the victim set shrinks to technical writers denied prestige venues.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this constraint is the rupture_reading of the latin_correctness kernel, one of three sibling readings.').

omega_variable(
    normative_criterion_disagreement_location,
    'Where exactly do the three readings disagree? The dispute is located in the criterion that fixes correctness: the ancient textual corpus (rupture), living transmission practice (continuity), or a domain partition assigning different criteria to literary versus technical registers (hybrid).',
    'Analysis of what each reading treats as defeater evidence: rupture is defeated by demonstration that reconstruction is indeterminate or that divergence is adaptive; continuity is defeated by demonstration that transmission lost determinacy; hybrid is defeated by demonstration that the domain boundary cannot be drawn.',
    'The located criterion determines which practices count as violations at all; mislocating it would attribute the extraction to the wrong structural element.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_criterion_disagreement_location, conceptual, 'Location of the inter-reading disagreement within the latin_correctness kernel.').

omega_variable(
    reconstruction_determinacy,
    'Is classical usage determinately recoverable from the surviving sources, or does ''the classical standard'' embed the reconstructors'' own choices at contested points (word choice, syntax, orthography)?',
    'Compare independent reconstruction efforts for convergence or divergence: the Ciceronian imitatio controversy, competing editorial practices for the same texts, grammatical disputes resolved by appeal to authority rather than evidence.',
    'If reconstruction is substantially indeterminate, the fixed standard is partly manufactured by its administrators, strengthening the capture reading of the classical credential premium; if determinate, the fixity claim stands and the standard''s authority rests on firmer ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_determinacy, empirical, 'Whether the ''fixed'' standard is determinate in the sources or partly constructed by its maintainers.').

omega_variable(
    corruption_or_adaptation,
    'Is medieval divergence from classical usage degradation (loss of classical precision) or adaptation (gain of precision for domains antiquity never addressed — theology, jurisprudence, medicine, administration)?',
    'Domain-level expressive adequacy comparison: measure ambiguity rates and term-formation success of scholastic theological and legal Latin against classical Latin attempting the same concepts.',
    'If adaptation, the corruption verdict loses its factual warrant; the reading''s own empirically-contingent foundational axiom is challenged, and the enforcement apparatus trends toward snare or piton assessment rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_or_adaptation, empirical, 'Whether the corruption verdict tracks a real deficit or misreads ordinary language change as decay.').

omega_variable(
    enforcement_decay_endpoint,
    'Will enforcement continue decaying toward purely editorial and ceremonial maintenance, or stabilize at a residual functional core (textual criticism of ancient texts, selective examination regimes)?',
    'Track Latin-requirement statistics, classics enrollment, and editorial norm adoption over coming decades; compare against the 1850 enforcement peak in the measurement series.',
    'Continued decay with rising theater_ratio dates a tangled_rope-to-piton transition; stabilization at a functional core supports the tangled_rope reading indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_decay_endpoint, empirical, 'Future trajectory of the standard''s enforcement machinery after the 20th-century contraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 1350, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1350, latin_correctness__rupture_reading, theater_ratio, 1350, 0.15).
narrative_ontology:measurement(lati_tr_t1450, latin_correctness__rupture_reading, theater_ratio, 1450, 0.22).
narrative_ontology:measurement(lati_tr_t1550, latin_correctness__rupture_reading, theater_ratio, 1550, 0.3).
narrative_ontology:measurement(lati_tr_t1650, latin_correctness__rupture_reading, theater_ratio, 1650, 0.34).
narrative_ontology:measurement(lati_tr_t1750, latin_correctness__rupture_reading, theater_ratio, 1750, 0.36).
narrative_ontology:measurement(lati_tr_t1850, latin_correctness__rupture_reading, theater_ratio, 1850, 0.33).
narrative_ontology:measurement(lati_tr_t1950, latin_correctness__rupture_reading, theater_ratio, 1950, 0.3).

% Extraction over time
narrative_ontology:measurement(lati_be_t1350, latin_correctness__rupture_reading, base_extractiveness, 1350, 0.45).
narrative_ontology:measurement(lati_be_t1450, latin_correctness__rupture_reading, base_extractiveness, 1450, 0.55).
narrative_ontology:measurement(lati_be_t1550, latin_correctness__rupture_reading, base_extractiveness, 1550, 0.63).
narrative_ontology:measurement(lati_be_t1650, latin_correctness__rupture_reading, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement(lati_be_t1750, latin_correctness__rupture_reading, base_extractiveness, 1750, 0.72).
narrative_ontology:measurement(lati_be_t1850, latin_correctness__rupture_reading, base_extractiveness, 1850, 0.76).
narrative_ontology:measurement(lati_be_t1950, latin_correctness__rupture_reading, base_extractiveness, 1950, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t1350, latin_correctness__rupture_reading, suppression_requirement, 1350, 0.35).
narrative_ontology:measurement(lati_su_t1450, latin_correctness__rupture_reading, suppression_requirement, 1450, 0.48).
narrative_ontology:measurement(lati_su_t1550, latin_correctness__rupture_reading, suppression_requirement, 1550, 0.62).
narrative_ontology:measurement(lati_su_t1650, latin_correctness__rupture_reading, suppression_requirement, 1650, 0.7).
narrative_ontology:measurement(lati_su_t1750, latin_correctness__rupture_reading, suppression_requirement, 1750, 0.74).
narrative_ontology:measurement(lati_su_t1850, latin_correctness__rupture_reading, suppression_requirement, 1850, 0.78).
narrative_ontology:measurement(lati_su_t1950, latin_correctness__rupture_reading, suppression_requirement, 1950, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, information_standard).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' conflates three structurally distinct claims about what fixes correctness, with materially different epsilon values. Per the epsilon-invariance principle the family decomposes into three stories: rupture_reading (this file — fixed ancient corpus, universal corruption verdict, high epsilon, broad victim set), continuity_reading (organic transmission, negligible extraction, no corruption victims), hybrid_reading (domain-partitioned norms, intermediate extraction concentrated in literary gatekeeping). Structural ordering: the rupture reading achieved institutional dominance first and created the conditions — expulsion of technical registers from prestige venues — under which the hybrid settlement formed; the continuity reading gained ground as historical linguistics matured and supplied the adaptation-over-decay account. Each file links the others via affects_constraints so family-level coupling and contamination analyses can run.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
