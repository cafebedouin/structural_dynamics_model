% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__reconstruction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__reconstruction_reading, []).

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
 *   constraint_id: classical_latin_standard__reconstruction_reading
 *   human_readable: Reconstruction Reading of Correct Latin — Philological Archaeology Standard
 *   domain: historical linguistics/philology/commitment systems
 *
 * SUMMARY:
 *   From Petrarch's generation onward, a network of Italian and then
 *   northern-European scholars declared that correct Latin subsists
 *   exclusively in the Classical form attested by surviving ancient authors,
 *   recoverable only through philological archaeology — collation of
 *   manuscripts, conjectural emendation, grammatical reconstruction — and
 *   that the Latin transmitted through continuous medieval use was
 *   accumulated corruption ('Gothic' barbarism) to be rejected, not
 *   developed. This reading of the classical_latin_standard kernel was
 *   implemented through new curricula (studia humanitatis), printed grammars
 *   and corrected editions, and patronage networks running through princely
 *   chanceries and the papal curia. Its operation systematically devalued the
 *   formed competence of scholastic masters, notaries, and clergy,
 *   transferring linguistic authority, employability, and institutional
 *   control to a new credentialed class of philologists. The ε referent for
 *   this story is the standing arrangement under contest: the reconstruction
 *   standard as actually administered in schools, presses, and chanceries,
 *   1400–1560, assessed honestly from its observable operation — not the
 *   restored antiquity the reading advertised. The claim/metric gap is
 *   deliberate: this reading presents itself as recovery and restoration,
 *   while the authored metrics describe enforced displacement and gatekeeping
 *   rent; the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - humanist_philological_elite: agenda-setting beneficiary (organized/identity_locked) — administers the restored standard through schools, presses, and chanceries while collecting the status, chairs, and editorial income it confers
 *   - scholastic_university_faculty: primary target (organized/trapped) — bears public delegitimization of its formed dialect, shrinking graduate demand, and the impossibility of mid-career retraining against younger competitors
 *   - chancery_notarial_draftsmen: secondary target (moderate/constrained) — bears the cost of restyling legal-formula Latin or losing clients to classically trained rivals
 *   - latin_students_generations: burden-bearing entrants (powerless/constrained) — pay lengthened, expensive training in exchange for canon access and the pan-European learned lingua franca
 *   - commercial_printers_publishers: incidental beneficiary (organized/arbitrage) — monetizes corrected editions and grammars, with capital free to leave the genre
 *   - parish_clergy_common_users: excluded voice (organized/trapped) — working liturgical Latin stigmatized by criteria set without their participation
 *   - historical_linguistics_analysts: analytical observer (analytical/analytical) — documents what transmitted Latin actually was and weighs textual yields against social costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, 0.68).
domain_priors:suppression_score(classical_latin_standard__reconstruction_reading, 0.68).
domain_priors:theater_ratio(classical_latin_standard__reconstruction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(classical_latin_standard__reconstruction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__reconstruction_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__reconstruction_reading, "Reconstruction Reading of Correct Latin — Philological Archaeology Standard").
narrative_ontology:topic_domain(classical_latin_standard__reconstruction_reading, "historical linguistics/philology/commitment systems").

domain_priors:requires_active_enforcement(classical_latin_standard__reconstruction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__reconstruction_reading, '4e392884-8034-4899-8a24-11cd5a7f7e51').
narrative_ontology:cs_kernel_codification('4e392884-8034-4899-8a24-11cd5a7f7e51', formalized).
narrative_ontology:cs_authority_grounding('4e392884-8034-4899-8a24-11cd5a7f7e51', expertise).
narrative_ontology:cs_interpretation_layer_present('4e392884-8034-4899-8a24-11cd5a7f7e51').
narrative_ontology:cs_reading_relation('4e392884-8034-4899-8a24-11cd5a7f7e51', classical_latin_standard__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('4e392884-8034-4899-8a24-11cd5a7f7e51', classical_latin_standard__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('4e392884-8034-4899-8a24-11cd5a7f7e51', foundational, correctness_recoverable_only_through_sources).
narrative_ontology:cs_axiom_status(correctness_recoverable_only_through_sources, holdable).
narrative_ontology:cs_axiom_grounding('4e392884-8034-4899-8a24-11cd5a7f7e51', correctness_recoverable_only_through_sources, empirically_contingent).
narrative_ontology:cs_axiom('4e392884-8034-4899-8a24-11cd5a7f7e51', secondary, post_classical_usage_is_drift_not_development).
narrative_ontology:cs_axiom_status(post_classical_usage_is_drift_not_development, holdable).
narrative_ontology:cs_axiom_grounding('4e392884-8034-4899-8a24-11cd5a7f7e51', post_classical_usage_is_drift_not_development, conventional).
narrative_ontology:cs_reference_frame('4e392884-8034-4899-8a24-11cd5a7f7e51', recovered_classical_corpus_norm).
narrative_ontology:cs_drift_state('4e392884-8034-4899-8a24-11cd5a7f7e51', high_humanist_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4e392884-8034-4899-8a24-11cd5a7f7e51', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__reconstruction_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, humanist_philological_elite).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, commercial_printers_publishers).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, scholastic_university_faculty).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, chancery_notarial_draftsmen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__reconstruction_reading, latin_students_generations).
narrative_ontology:constraint_victim(classical_latin_standard__reconstruction_reading, latin_students_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach in the new grammar schools and arts faculties, edit and emend classical texts for the presses, write the grammars and dictionaries that define correct usage, and staff the chanceries of Italian princes and the papal curia. Their authority rests on demonstrated command of recovered classical idiom; their income, chairs, and commissions flow from demand for the standard they administer. They define themselves against scholastic barbarism — leaving philology would forfeit the status and livelihood their training alone confers, so exit is not a live option they weigh.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, humanist_philological_elite, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, humanist_philological_elite, beneficiary).

% Hold regent master and theology, law, and medicine chairs earned under the old curriculum; lecture through terminist logic and disputed questions in the Latin they were formed in. As arts faculties adopt humanist letters and princes hire humanist secretaries, their dialect is publicly labeled barbarous, their graduates find fewer posts, and their learning is redescribed as scholastic muddle. Retraining in Ciceronian style mid-career pits them against men twenty years younger raised on the new drills; their formed capital has no second market.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, scholastic_university_faculty, payer,
    organized, biographical, trapped, continental).

% Draft instruments, petitions, and correspondence in formulaic medieval Latin optimized for legal effect. Patrons increasingly expect classical polish, formularies are rewritten on classical models, and a draftsman whose style reads archaic loses business to classically trained rivals. Switching trades means abandoning craft knowledge accumulated over decades, so they absorb restyling costs instead.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, chancery_notarial_draftsmen, payer,
    moderate, biographical, constrained, national).

% Children and adolescents routed into grammar schools built on imitation of Cicero, Livy, and Terence, spending years on composition drills before touching philosophy, law, or medicine. They bear the lengthened training and its fees, and inherit careers gated by fluency in the restored standard; in exchange they gain direct access to the classical canon and a learned lingua franca usable across Europe. Schooling choice sits with fathers and patrons, not with them.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, latin_students_generations, payer,
    powerless, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__reconstruction_reading, latin_students_generations, beneficiary).

% Print and sell corrected editions of classical authors, newly composed humanist grammars, and school texts; sponsor famous emendators because an authoritative text commands premium prices and blocks rival reprintings. Their capital moves freely among genres, cities, and languages — vernacular books, devotional works, news sheets — so nothing binds a press to the classical trade except current profit.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, commercial_printers_publishers, beneficiary,
    organized, biographical, arbitrage, continental).

% Recite the daily offices and administer sacraments in liturgical Latin learned by rote from local tradition. Breviary and preaching-manual reforms gradually apply classical criteria to texts they must recite verbatim; they are rarely consulted, their working competence acquires a stigma of rusticity, and vows and assignment leave no practical exit from the rite as revised.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, parish_clergy_common_users, excluded,
    organized, biographical, trapped, continental).

% Retrospective analytical seat: document what medieval Latin actually was as a functioning internal variety, reconstruct how the restored standard was taught, marketed, and enforced, and weigh the program's textual and scholarly yields against its displacement costs. Hold no stake in either the displaced or the installed practice.
narrative_ontology:constraint_stakeholder(classical_latin_standard__reconstruction_reading, historical_linguistics_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__reconstruction_reading, humanist_philological_elite).
narrative_ontology:fixing_cost_class(classical_latin_standard__reconstruction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative reference form of Latin for textual criticism, teaching, and cross-border scholarly communication, solving the problem of unrecoverable textual corruption and mutually unintelligible regional scholastic usage.
% TRANSFER_FUNCTION: Moves linguistic authority, status, employability, and institutional control from practitioners of transmitted medieval Latin to holders of philological training; moves curriculum years and schooling fees from dialectic and disputation to grammar and imitation; moves book-buying demand toward corrected classical editions.
% ABSENT_VOICES: Practitioners of working Latin outside the lettered elite — notaries, parish clergy, physicians trained in arabized terminology, and women, who were barred from Latinate schooling altogether — never sat in the conversation that declared their usage corrupt. Even the formal debate occurred entirely among parties who accepted that some single standard ought to govern; no seat argued for registered plurality as the legitimate settlement.
% DISAPPEARANCE_RATIONALE: If the reconstruction standard and its enforcement vanished overnight, arts curricula would revert to practice-transmitted Latin, corrected editions would lose their premium and their authority, philological chairs and grammar-school posts would evaporate, and chancery style would drift back to local formulae — the learned world would reorganize around whichever transmission practice regained institutional sponsorship.
% FOUNDING_PROBLEM: Centuries of manuscript transmission had corrupted the classical texts and let scholarly usage drift far from its ancient models; scholars could no longer establish what Cicero wrote, nor write a Latin that commanded respect in every European chancery and faculty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: displaced scholastic masters recorded their objections to curriculum replacement in university records (Paris, Leuven); histories of scholarship and printing show the text-recovery goals substantially achieved by the mid-sixteenth century; and modern historical linguistics documents medieval Latin as an internally consistent, fully functional working variety, undercutting the corruption framing that justified continued gatekeeping. No voice outside the benefiting set attests that the usage-policing half of the mandate remained necessary after recovery succeeded.
narrative_ontology:disappearance_verdict(classical_latin_standard__reconstruction_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__reconstruction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__reconstruction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__reconstruction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__reconstruction_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__reconstruction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__reconstruction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__reconstruction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the standard decoupled linguistic authority from demonstrated communicative success and re-coupled it to possession of scarce philological training, converting an installed base of competent practice into 'error' and collecting status and employment rents on the conversion. Suppression 0.68 is the raw structural level of the mature enforcement regime (curriculum replacement, polemics branding medieval usage corruption, patronage steering); it is authored unscaled — the engine owns directionality and scope amplification. The suppression_requirement series shows the enforcement arc honestly: build-up through the fifteenth century, peak around the displacement battles of 1500–1520, then decay as normalization made active coercion redundant — the scalar represents the mature phase, not the endpoint. Theater_ratio 0.40: the philology was largely functional (real collations, real emendations, real textual recovery — Valla's demonstrations changed what Europe could read), but as the recovery goals were met, a growing share of activity shifted to purity performance — ritualized Ciceronianism, display contempt for 'barbarisms', boundary policing that reproduced the distinction rather than recovering anything. Accessibility_collapse 0.40: alternatives did not collapse; continuity practice survived in liturgy and technical registers, hybrid positions stayed articulable, and the exclusion was enforced socially rather than rendered unthinkable. Resistance 0.60: sustained scholastic counterattack, the Ciceronian controversy, and decades of regional refusal before curricula flipped. Claimed type tangled_rope, stated independently of the metrics: the arrangement genuinely coordinated (a stable referent for textual criticism, teaching, and scholarly communication) while asymmetrically extracting through the same structure, held together by active enforcement — beneficiaries and victims are both named, and neither fact erases the other.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the agenda-setter seat the arrangement is a scholarly achievement its holders built, defend, and personally embody — coordination with themselves as rightful administrators. From the trapped payer seats the same structure operates as dispossession: their competence relabeled as defect by people who then sell the remedy. Students straddle: they bear the lengthened training yet receive real goods (canon access, mobility), so their computed position should sit nearer symmetric than either established seat. Coalition check: the trapped scholastic faculty retained organizational power (they still ran the universities in 1450), yet coalition failed — enforcement rode on patronage flows, print economics, and student preference rather than on the faculty's own institutions, so organizational capacity without demand-side leverage did not convert into effective exit or reversal.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive derivation: humanist_philological_elite is declared beneficiary-plus-administrator with identity_locked exit — identity fusion with the philological mission makes their investment total, placing them nearest the full-beneficiary end (d near 0.0). commercial_printers_publishers benefit incidentally but hold arbitrage exit (capital moves among genres and cities), damping their d toward the beneficiary side without locking them in. scholastic_university_faculty is declared victim with trapped exit (formed capital, age, no parallel market for their dialect) — nearest the full-target end (d approaching 1.0). chancery_notarial_draftsmen are victims with constrained exit — high d, slightly below the faculty. latin_students_generations carry a dual declaration (pay the training, receive the access), pulling their d toward the middle. parish_clergy_common_users hold no beneficiary or victim declaration — an excluded seat whose harms are commentary-grade — and would otherwise fall to the canonical fallback, which understates their position; the omega layer records this rather than forcing an override keyed to a shared power atom. historical_linguistics_analysts take the analytical seat with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — recover authentic classical texts and restore command of classical idiom — was substantially accomplished by the mid-sixteenth century: texts were established, methods codified, and the recovery problem that justified discontinuous return was solved. What persisted past that point is boundary maintenance: credential gating, purity policing, and the reproduction of a training pipeline whose length is itself the moat. Hence mandatrophy_resolved is declared, and the R5 pairing (founding_problem_status dead x disappearance_verdict world_rearranges) flags the capture/zombie signature the mismatch consumer cross-checks against this theater and extraction profile. The classification prevents mislabeling in both directions: reading the arrangement as pure snare erases the genuine textual science it performed and the real goods it delivered to entrants; reading it as pure rope hides that its persistence after mandate fulfillment is sustained by rent and stigma rather than by any outstanding recovery problem. Tangled_rope preserves both facts, and the temporal series marks where coordination yield flattened while gatekeeping continued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the reconstruction reading of the classical_latin_standard kernel; how would the sibling readings restructure the same subject matter?',
    'Author classical_latin_standard__continuity_reading and classical_latin_standard__hybrid_reading as separate stories; compare victim sets, epsilon, and per-seat classifications across the family.',
    'Under continuity_reading the victim set inverts (philologists become the rupturing party; practice communities hold beneficiary position and epsilon drops); under hybrid_reading the victim set shrinks to strict purists and measured extraction declines. Cross-reading comparison isolates how much of the measured extraction belongs to the reconstruction criterion itself versus to standardization as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one kernel, three readings, disjoint constraint structures.').

omega_variable(
    recovery_warrant_vs_fashion,
    'How much of the ''restored'' classical norm was philologically warranted by surviving evidence, and how much was conjecture crystallized into doctrine by the first generations of grammarians and editors?',
    'Compare humanist emendations and prescriptive grammatical rulings against subsequent critical editions and modern historical-linguistic reconstruction; quantify prescription survival rates across the sixteenth to twentieth centuries.',
    'If a large share of the standard was conjectural fashion, the extraction rode on manufactured authenticity and the snare-side reading strengthens; if mostly warranted, the coordination function dominates and the tangled_rope claim is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_warrant_vs_fashion, empirical, 'Whether the standard''s content was evidence-driven or authority-driven.').

omega_variable(
    displacement_welfare_accounting,
    'Did practitioners of transmitted medieval Latin suffer net welfare loss, or were they largely absorbed through retraining as demand shifted toward classically formed staff?',
    'Prosopography of regent masters, chancery staff, and medical/law faculties across the transition: career lengths, income trajectories, and the schooling choices of their children.',
    'Documented net loss confirms the victim declarations as written; broad absorption weakens them and pushes the classification toward coordination with transitional friction rather than systematic extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_welfare_accounting, empirical, 'Severity and reality of harm to the displaced user population.').

omega_variable(
    unitary_standard_necessity,
    'Was some single normative Latin standard structurally necessary for scholarly life, or were plural registers (classical forms for literary work, transmitted forms for technical, legal, and liturgical use) a stable equilibrium?',
    'Compare periods and regions with sustained plural-register practice (late antique technical Latin, Byzantine diglossia, pre-humanist Europe itself) for coordination failures attributable to non-unitary norms.',
    'If plural registers were stable, a portion of the suppression served boundary maintenance rather than any coordination problem, raising excess extraction above the Boltzmann floor; if unitarity was load-bearing, part of the suppression prices genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unitary_standard_necessity, conceptual, 'Whether a single standard was a coordination necessity or a constructed monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__reconstruction_reading, 1400, 1560).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t1400, classical_latin_standard__reconstruction_reading, theater_ratio, 1400, 0.12).
narrative_ontology:measurement(clas_tr_t1420, classical_latin_standard__reconstruction_reading, theater_ratio, 1420, 0.15).
narrative_ontology:measurement(clas_tr_t1440, classical_latin_standard__reconstruction_reading, theater_ratio, 1440, 0.2).
narrative_ontology:measurement(clas_tr_t1460, classical_latin_standard__reconstruction_reading, theater_ratio, 1460, 0.26).
narrative_ontology:measurement(clas_tr_t1480, classical_latin_standard__reconstruction_reading, theater_ratio, 1480, 0.3).
narrative_ontology:measurement(clas_tr_t1500, classical_latin_standard__reconstruction_reading, theater_ratio, 1500, 0.33).
narrative_ontology:measurement(clas_tr_t1520, classical_latin_standard__reconstruction_reading, theater_ratio, 1520, 0.36).
narrative_ontology:measurement(clas_tr_t1540, classical_latin_standard__reconstruction_reading, theater_ratio, 1540, 0.38).
narrative_ontology:measurement(clas_tr_t1560, classical_latin_standard__reconstruction_reading, theater_ratio, 1560, 0.4).

% Extraction over time
narrative_ontology:measurement(clas_be_t1400, classical_latin_standard__reconstruction_reading, base_extractiveness, 1400, 0.35).
narrative_ontology:measurement(clas_be_t1420, classical_latin_standard__reconstruction_reading, base_extractiveness, 1420, 0.42).
narrative_ontology:measurement(clas_be_t1440, classical_latin_standard__reconstruction_reading, base_extractiveness, 1440, 0.5).
narrative_ontology:measurement(clas_be_t1460, classical_latin_standard__reconstruction_reading, base_extractiveness, 1460, 0.58).
narrative_ontology:measurement(clas_be_t1480, classical_latin_standard__reconstruction_reading, base_extractiveness, 1480, 0.65).
narrative_ontology:measurement(clas_be_t1500, classical_latin_standard__reconstruction_reading, base_extractiveness, 1500, 0.7).
narrative_ontology:measurement(clas_be_t1520, classical_latin_standard__reconstruction_reading, base_extractiveness, 1520, 0.72).
narrative_ontology:measurement(clas_be_t1540, classical_latin_standard__reconstruction_reading, base_extractiveness, 1540, 0.7).
narrative_ontology:measurement(clas_be_t1560, classical_latin_standard__reconstruction_reading, base_extractiveness, 1560, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t1400, classical_latin_standard__reconstruction_reading, suppression_requirement, 1400, 0.25).
narrative_ontology:measurement(clas_su_t1420, classical_latin_standard__reconstruction_reading, suppression_requirement, 1420, 0.32).
narrative_ontology:measurement(clas_su_t1440, classical_latin_standard__reconstruction_reading, suppression_requirement, 1440, 0.45).
narrative_ontology:measurement(clas_su_t1460, classical_latin_standard__reconstruction_reading, suppression_requirement, 1460, 0.55).
narrative_ontology:measurement(clas_su_t1480, classical_latin_standard__reconstruction_reading, suppression_requirement, 1480, 0.65).
narrative_ontology:measurement(clas_su_t1500, classical_latin_standard__reconstruction_reading, suppression_requirement, 1500, 0.72).
narrative_ontology:measurement(clas_su_t1520, classical_latin_standard__reconstruction_reading, suppression_requirement, 1520, 0.74).
narrative_ontology:measurement(clas_su_t1540, classical_latin_standard__reconstruction_reading, suppression_requirement, 1540, 0.62).
narrative_ontology:measurement(clas_su_t1560, classical_latin_standard__reconstruction_reading, suppression_requirement, 1560, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__reconstruction_reading, information_standard).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__reconstruction_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' covers three structurally distinct claims differing on the source of normativity: unbroken practice (continuity_reading), dual textual-plus-practice authority (hybrid_reading), and exclusive archaeological recovery (this file). Measured against the continuity arrangement, reconstruction's epsilon reflects attack on an incumbent; measured against its own administered arrangement (this story's referent), it reflects displacement and gatekeeping rent. The stories form a constraint family: continuity_reading is the upstream incumbent whose legitimacy reconstruction attacks, and hybrid_reading is the downstream synthesis that inherits pressure from both. Each member carries its own stable epsilon; no story hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
