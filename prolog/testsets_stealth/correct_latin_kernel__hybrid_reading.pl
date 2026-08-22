% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Layered Reconstruction Standard for Correct Latin (Hybrid Reading)
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   Between Valla's Elegantiae (completed 1441) and the consolidated
 *   grammar-school standard circa 1600, a normative regime governed correct
 *   Latin built on a stratified diagnosis: inflectional morphology reached
 *   the Renaissance essentially intact through continuous transmission, while
 *   syntax and lexicon had drifted and required documentary recovery from the
 *   rediscovered ancient texts. Reconstruction was therefore layered —
 *   partial reoccupation. The regime performed real coordination (a single
 *   supranational learned register making texts composed in one country
 *   readable, citable, and cumulative in all others) while simultaneously
 *   redistributing authority from holders of living medieval practice to
 *   holders of recovered classical competence, and imposing a doubled
 *   curriculum on the young. This file instantiates ONLY the hybrid_reading
 *   of the correct_latin_kernel; the continuity_reading and
 *   discontinuity_reading are separate constraints in separate files, linked
 *   through network edges, and are neither averaged over nor described inside
 *   this classification (epsilon-invariance: one reading, one constraint, one
 *   stable epsilon). The epsilon referent is the standing arrangement under
 *   contest — the layered-reconstruction regime itself — assessed by this
 *   reading's own lights, which certify half the system as sound and half as
 *   requiring recovery. Claim/metric independence is preserved: claimed_type
 *   tangled_rope is authored from structure (genuine coordination function
 *   plus asymmetric costs plus active enforcement); the metrics are authored
 *   descriptively from the historical record, and the engine computes
 *   per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - humanist_editorial_network: agenda-setting beneficiary (institutional/arbitrage) — sets the standard through editions, grammars, and curricula; collects authority, patronage, and curricular control
 *   - classical_edition_printers: commercial beneficiary (powerful/mobile) — collects revenue from the corrected-text market without ruling on correctness
 *   - scholastic_university_masters: primary payer (organized/constrained) — working idiom publicly reclassified as defective; careers and chairs at stake
 *   - curial_chancery_scribes: payer (moderate/trapped) — formulaic administrative prose targeted for restyling while legal validity must be preserved
 *   - roman_curia: dual-positioned (institutional/constrained) — protected by morphological continuity in liturgy, targeted by stylistic reform in administration
 *   - grammar_school_students: payer (powerless/trapped) — bear the doubled curriculum: inherited forms plus corrective retraining
 *   - vernacular_women_writers: excluded (powerless/constrained) — barred from the schooling that confers standing, subject to its prestige ranking, no seat among standard-setters
 *   - modern_historical_linguists: analytical observer (analytical/analytical) — sees the full stratified structure from outside the quarrel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.56).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.44).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Layered Reconstruction Standard for Correct Latin (Hybrid Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, '0cd99d3a-5306-46dd-8a4a-d71f4a179e5d').
narrative_ontology:cs_kernel_codification('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', fixed_text).
narrative_ontology:cs_authority_grounding('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', expertise).
narrative_ontology:cs_interpretation_layer_present('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d').
narrative_ontology:cs_reading_relation('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', correct_latin_kernel__discontinuity_reading, forecloses).
narrative_ontology:cs_axiom('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', foundational, transmission_is_stratified).
narrative_ontology:cs_axiom_status(transmission_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', transmission_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', secondary, recovery_bounded_to_ruptured_strata).
narrative_ontology:cs_axiom_status(recovery_bounded_to_ruptured_strata, holdable).
narrative_ontology:cs_axiom_grounding('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', recovery_bounded_to_ruptured_strata, instrumental).
narrative_ontology:cs_reference_frame('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', stratified_transmission_baseline).
narrative_ontology:cs_drift_state('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', contemporary_medieval_latin_studies, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('0cd99d3a-5306-46dd-8a4a-d71f4a179e5d', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, humanist_editorial_network).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classical_edition_printers).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, scholastic_university_masters).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, curial_chancery_scribes).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, grammar_school_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, roman_curia).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, roman_curia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A dispersed republic of philologists, editors, and professors of eloquence — the Valla-Poliziano line and the correctors around the great presses — who establish which forms count as correct by publishing critical editions, grammars, and school statutes. They decide which ancient authors are canonical models, certify printed corrections, and staff the examinerships that gate advancement. Income and reputation flow from princes, cardinals, and city councils competing to host famous teachers; when a court's favor cools they relocate to another.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, humanist_editorial_network, agenda_setter,
    institutional, generational, arbitrage, continental).

% Printing houses such as the Aldine press invest in corrected classical texts and sell them across Europe. They profit from the demand the recovery program creates without themselves ruling on correctness; their capital can shift to other genres or markets if demand moves.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_edition_printers, beneficiary,
    powerful, biographical, mobile, continental).

% Arts and theology masters formed in medieval logico-scholastic prose watch their working idiom publicly reclassified as barbarous: disputations mocked, textbooks displaced from curricula, students drawn off to humanist courses. Retraining in classical style is possible but costly and late in a career; defending the old ways risks chairs, students, and printing contracts. Leaving the university means abandoning the vocation they were formed for, and their professional self-concept is bound up with the method under attack.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, scholastic_university_masters, payer,
    organized, biographical, constrained, continental).

% Draftsmen in papal, royal, and municipal chanceries produce formulaic administrative Latin optimized for speed and legal continuity. Stylistic reform campaigns single out their register for ridicule and demand rewritten formulas and restyled drafts — added labor on top of production quotas, while the underlying instruments must remain legally valid. Their employment ties them to offices that cannot simply stop issuing documents.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, curial_chancery_scribes, payer,
    moderate, biographical, trapped, continental).

% The papacy's liturgical and doctrinal Latin rests on the uninterrupted transmission of inflectional forms; the certification that this stratum arrived sound protects the immutability of the liturgy. At the same time curial administrative prose is a standing target of classicizing reform, forcing the Curia to sponsor revisions of its own working documents. Abandoning Latin entirely would dissolve its transnational character, so it cannot exit the arrangement — it can only fight over which strata the standard touches.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, roman_curia, beneficiary,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, roman_curia, payer).

% Children in Latin schools memorize the inherited declensions and conjugations — material already continuous with the ancient language — and then submit to a second corrective layer: imitation exercises, phrase banks, and correction of the syntactic and lexical habits absorbed at home. School days lengthen; failure closes the paths to university, law, and office. They choose neither the curriculum nor the standard it serves.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, grammar_school_students, payer,
    powerless, immediate, trapped, national).

% Educated women were largely shut out of the Latin schooling that confers standing in this arrangement, yet lived inside its prestige hierarchy, which ranked vernacular composition beneath Latinity. Figures like Christine de Pizan turned the exclusion into a vernacular literary culture and argued that the gate, not their capacity, was the defect. They held no seat among the standard-setters whose rankings governed their reception.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, vernacular_women_writers, excluded,
    powerless, biographical, constrained, national).

% Comparative and documentary linguists working centuries later can see the whole structure: which strata were transmitted without rupture, which innovated, and how the recovery program redistributed authority. They take no side in the Renaissance quarrel and bear none of its costs; their testimony is the main outside check on every party's self-description.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, modern_historical_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, humanist_editorial_network).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single supranational learned register: certifies the inflectional forms every literate European already shares, and restores a common syntactic and lexical repertoire from the ancient corpus so that texts composed in one country are readable, citable, and cumulative in all others, across generations.
% TRANSFER_FUNCTION: Moves scholarly authority, chairs, printing contracts, and patronage from holders of living medieval practice (scholastic masters, chancery clerks) to holders of recovered classical competence (philologists, their patrons, and the corrected-text trade); moves student years into extended Latinity training; moves legitimacy from institutional custom to textual evidence.
% ABSENT_VOICES: Women barred from Latin schooling had no seat in setting the standard though subject to its prestige ranking; practitioner-writers outside the universities (surgeons, apothecaries, municipal clerks) whose functional medieval technical vocabulary was classified as barbarous were never consulted; Byzantine emigre scholars contributed expertise but the curricular decisions sat with Italian and northern university authorities. They are located in vernacular print, workshop practice, and household education — outside the academies where the standard was fixed.
% DISAPPEARANCE_RATIONALE: If the layered standard vanished overnight, the pan-European learned register fragments into regional and functional varieties; the Republic of Letters loses its common medium and with it the cumulativity that made cross-border citation and dispute possible; careers reorganize around local vernaculars and customary chancery practice; the corrected-text market collapses; and the schools drop their corrective second layer, shortening training but severing the young from direct access to the ancient corpus.
% FOUNDING_PROBLEM: After the breakdown of Roman schooling, Latin diverged regionally and functionally; by the late Middle Ages the learned register differed markedly from the ancient corpus that remained authoritative for law, theology, and prestige. Scholars lacked both a reliable way to read the ancient texts accurately and a written register commanding trans-European authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: modern medieval Latinists (the Norberg-Lofstedt line) independently attest that the syntactic and lexical rupture was real and documentable; historians of education attest the training-burden side of the ledger from enrollment and curriculum records; Reformation-era critics hostile to humanist gatekeeping nonetheless conceded the textual-access problem was genuine. No corroborating source attests that the problem REMAINED unsolved past the interval — the dispute over status is between the access-half (widely attested as solved) and the medium-half (attested as migrating rather than dying).
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.56 at interval end: the regime's costs are real but bounded — deprecation of scholastic prose, restyling labor in chanceries, a lengthened curriculum — offset by recovery goods even critics consumed. Suppression 0.44 is authored as a raw structural property (unscaled by power or scope; only extractiveness is scaled in the engine): enforcement existed (curriculum statutes, print correctors, patronage preference, ridicule) but never approached monopoly coercion, and the vernacular exit stayed materially open. Accessibility_collapse 0.48 reflects that partial openness: understanding the standard does not close the vernacular or functional-administrative alternatives. Resistance 0.55 is high for a coordination arrangement because the payers were ORGANIZED — university nations, faculty colleges, and chancery offices mounted sustained counterattack (defense of the via antiqua, anti-Ciceronian polemic, curial inertia), which is why the regime needed active enforcement at all. Theater_ratio peaks mid-interval with the Ciceronian purism controversy (phrase-bank imitation as performance of purity) and declines as Poliziano-style eclecticism and scientific accommodation reabsorb medieval technical lexicon, restoring the share of substantive philological work. The suppression_requirement series is authored because enforcement-capacity change IS the traced dynamic: informal mockery (1441) hardened into statutes and print policing (peak 1540), then relaxed as compliance normalized (1600). All three series share one six-point grid (1441/1480/1510/1540/1570/1600) so no metric row is backfilled from another's endpoints. The extractiveness curve is hump-shaped, not monotonic: accumulation during the contest, partial release through accommodation — the regime survived its own victory by softening.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the editorial-network seat the arrangement is the rescue of a common inheritance: morphology needed only keeping, and the recovered texts restored what drift had buried — coordination experienced as gift. From the scholastic master's seat the same partition is dispossession: his morphology was never in doubt, but the layers declared ruptured are precisely the layers carrying his professional voice, so the recovery program functions as the confiscation of a working competence. From the student's seat it is doubled labor with no authorship. The curia experiences both faces at once — continuity certifies its liturgy, reform campaigns indict its chancery. The engine derives these divergences from role, power, and exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: the editorial network (agenda-setter and collector, arbitrage-grade mobility across patronage markets) nearest zero; printers (pure collectors, mobile capital) likewise low. Declared payers sit near the target end, amplified by exit structure: trapped scribes and students near the full-target end, constrained masters somewhat below them because retraining was possible at cost. The curia is the genuinely dual-positioned agent — beneficiary of the certified-continuous stratum, payer in the targeted strata — netting a mid-low directionality that no single role declaration captures; the derivation reads its beneficiary declaration and constrained exit. No directionality_overrides are authored: the override mechanism keys on power atoms, and the two institutional seats (editorial network, curia) require DIFFERENT d values, so a single institutional-level override would misfire one of them; the role-plus-exit derivation is left to resolve them, and this limitation is recorded here rather than papered over with a coarse override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves: reliable access to the ancient corpus, and a stable transnational learned medium. By interval end the access half was progressively solved — the very success of textual recovery — while the medium half persisted and later migrated toward the vernaculars. Authoring founding_problem_status as contested (rather than dead) keeps the mismatch consumer from firing a spurious zombie flag, but the risk is real and stated plainly: a regime whose diagnostic work is done can persist as ceremony. The theater_ratio decline after 1540 is the observable that guards against that drift — performative purism burned off while functional coordination (cumulative scholarship, corpus access) persisted, and the reabsorption of medieval technical vocabulary into scientific Latin shows the standard adapting rather than ossifying. Classification discipline cuts both ways: a pure-coordination reading would erase the documented deprecation costs borne by organized payers; a pure-extraction reading would erase the recovery goods that even the payers' own institutions consumed. The tangled_rope claim holds both facts in one structure, and the per-seat computation lets the engine price the asymmetry instead of the author asserting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition_location,
    'This story instantiates the hybrid_reading of the correct_latin_kernel: the point where the three readings part is exactly which strata of Latin transmission count as continuous versus ruptured. The continuity_reading places no stratum behind a recovery barrier; the discontinuity_reading places the whole system there; this reading draws the line between morphology and syntax/lexicon. What evidence fixes the partition where this reading draws it?',
    'Manuscript transmission studies and diachronic corpora comparing inflectional paradigm stability against syntactic and lexical innovation rates across the sixth through fifteenth centuries.',
    'If morphology shows significant ruptures, the partition slides toward the discontinuity_reading and this reading''s exempted layer shrinks; if medieval syntax proves internally continuous with late antique usage, the partition slides toward the continuity_reading and the recovery mandate contracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_partition_location, conceptual, 'Location of the continuity/rupture partition distinguishing this kernel reading from its siblings.').

omega_variable(
    corruption_versus_innovation_status,
    'Are the medieval syntactic and lexical forms this reading schedules for recovery corruptions of a prior norm, or rule-governed innovations of a parallel system that the normative label misdescribes?',
    'Systematic grammars of medieval Latin testing whether the targeted forms follow describable rules of their own rather than random degradation.',
    'If systematic, the arrangement''s costs were borne to displace a rival competent system rather than to repair errors, raising the arrangement''s effective extractiveness; if genuinely degraded, recovery reads closer to repair than displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_versus_innovation_status, empirical, 'Whether ''corruption'' accurately describes the forms scheduled for recovery.').

omega_variable(
    morphology_continuity_degree,
    'How complete was morphological continuity in fact — do losses such as the productive neuter, regional case-system erosion, and analogical reshaping leave the certified-continuous stratum less intact than this reading declares?',
    'Quantitative paradigm-transmission statistics across scriptoria and regional chanceries, 600-1400.',
    'Material morphological rupture would move the partition toward the discontinuity_reading, revising this reading''s foundational axiom and shrinking the layer exempt from recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphology_continuity_degree, empirical, 'Degree of continuity in the stratum this reading certifies as unbroken.').

omega_variable(
    vernacular_exit_elasticity,
    'Was the vernacular a genuinely available exit from the Latin standard, or an option whose attractiveness was itself produced by the standard''s training burdens and exclusions?',
    'Biographical and publication-pattern analysis of writers who switched registers mid-career, separating the pull of vernacular markets from the push of Latin gatekeeping.',
    'If exit was largely pushed by the standard''s costs, accessibility_collapse is effectively higher than authored and the arrangement holds its targets more tightly than surface options suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vernacular_exit_elasticity, conceptual, 'Elasticity of the vernacular alternative to the Latin standard.').

omega_variable(
    enforcement_decay_cause,
    'Does the falling suppression_requirement after mid-century reflect victory (compliance internalized, less force needed) or erosion (enforcement capacity declining while dissent persists)?',
    'Late-century school statutes, print-license records, and polemic intensity compared against measured compliance in surviving prose.',
    'Victory-normalization supports the coordination reading of the arrangement; erosion with persistent dissent would indicate unresolved contestation and possible resurgence of open conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_cause, empirical, 'Cause of the late-interval decline in required enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 1441, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clk_hybrid_tr_t1441, correct_latin_kernel__hybrid_reading, theater_ratio, 1441, 0.14).
narrative_ontology:measurement(clk_hybrid_tr_t1480, correct_latin_kernel__hybrid_reading, theater_ratio, 1480, 0.2).
narrative_ontology:measurement(clk_hybrid_tr_t1510, correct_latin_kernel__hybrid_reading, theater_ratio, 1510, 0.3).
narrative_ontology:measurement(clk_hybrid_tr_t1540, correct_latin_kernel__hybrid_reading, theater_ratio, 1540, 0.36).
narrative_ontology:measurement(clk_hybrid_tr_t1570, correct_latin_kernel__hybrid_reading, theater_ratio, 1570, 0.31).
narrative_ontology:measurement(clk_hybrid_tr_t1600, correct_latin_kernel__hybrid_reading, theater_ratio, 1600, 0.26).

% Extraction over time
narrative_ontology:measurement(clk_hybrid_be_t1441, correct_latin_kernel__hybrid_reading, base_extractiveness, 1441, 0.42).
narrative_ontology:measurement(clk_hybrid_be_t1480, correct_latin_kernel__hybrid_reading, base_extractiveness, 1480, 0.5).
narrative_ontology:measurement(clk_hybrid_be_t1510, correct_latin_kernel__hybrid_reading, base_extractiveness, 1510, 0.58).
narrative_ontology:measurement(clk_hybrid_be_t1540, correct_latin_kernel__hybrid_reading, base_extractiveness, 1540, 0.63).
narrative_ontology:measurement(clk_hybrid_be_t1570, correct_latin_kernel__hybrid_reading, base_extractiveness, 1570, 0.6).
narrative_ontology:measurement(clk_hybrid_be_t1600, correct_latin_kernel__hybrid_reading, base_extractiveness, 1600, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(clk_hybrid_su_t1441, correct_latin_kernel__hybrid_reading, suppression_requirement, 1441, 0.32).
narrative_ontology:measurement(clk_hybrid_su_t1480, correct_latin_kernel__hybrid_reading, suppression_requirement, 1480, 0.44).
narrative_ontology:measurement(clk_hybrid_su_t1510, correct_latin_kernel__hybrid_reading, suppression_requirement, 1510, 0.58).
narrative_ontology:measurement(clk_hybrid_su_t1540, correct_latin_kernel__hybrid_reading, suppression_requirement, 1540, 0.66).
narrative_ontology:measurement(clk_hybrid_su_t1570, correct_latin_kernel__hybrid_reading, suppression_requirement, 1570, 0.58).
narrative_ontology:measurement(clk_hybrid_su_t1600, correct_latin_kernel__hybrid_reading, suppression_requirement, 1600, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, discontinuity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of correct_latin_kernel per epsilon-invariance: the colloquial label 'restoration of correct Latin' conflates three structurally distinct claims about transmission — uniform continuity, uniform rupture, and stratified transmission. Each reading yields a different epsilon, a different victim set, and a different enforcement profile, so each is a separate story. This file carries the hybrid_reading's epsilon only. Edge structure: the continuity_reading's morphological-continuity result is cited as evidence INSIDE this reading's partition (upstream support), while the discontinuity_reading's textual-recovery machinery is the apparatus this reading borrows for half the system (downstream application); both siblings are listed in affects_constraints accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
