% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin as Classical Form Transmitted and Textually Correctable (Hybrid Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the contested kernel
 *   'correct Latin': the standard under which correct Latin is the Classical
 *   form as transmitted through medieval practice, correctable where
 *   transmitted forms diverge from ancient textual evidence — partial
 *   continuity with targeted reform. The standing arrangement under contest,
 *   and therefore the epsilon referent, is the operating apparatus that
 *   enforces this standard across scholarship, publishing, pedagogy, and
 *   liturgy: critical-edition emendation practice, the reference grammars and
 *   lexica, examination and curriculum norms, and the peer-review culture
 *   that adjudicates form in Latin publication. Assessed by the hybrid
 *   reading's own lights, the arrangement coordinates genuinely — one norm
 *   makes a two-millennium corpus editable, teachable, and citable — while
 *   concentrating corrective authority in the classical philological
 *   establishment and billing conformity costs to medievalist editors,
 *   students, and non-classical users. Epsilon is therefore authored
 *   moderate: not the near-zero a continuity reading would claim for its own
 *   arrangement, nor the high value a discontinuity reading would author for
 *   its corrective mandate. The sibling readings are separate constraint
 *   files in this kernel family, linked through network.affects_constraints;
 *   their differing epsilon values reflect different instantiations of the
 *   standard, not hedging within this one. Claim and metrics are independent
 *   authored facts: claimed_type tangled_rope states the reading's own
 *   structural assessment — coordination plus asymmetric extraction under
 *   active enforcement — while the metric values describe the arrangement's
 *   actual operation as the reading descriptively observes it.
 *
 * KEY AGENTS:
 *   - classical_philologists: agenda-setter and principal beneficiary (institutional / identity_locked) — edit the texts, write the grammars and dictionaries, adjudicate correctness, and collect the authority and editorial economy the standard generates
 *   - medievalist_scholars: primary payer (organized / constrained) — work on texts whose forms are corrected toward classical norms and whose period's usage is framed as decline
 *   - latin_students: payer (powerless / mobile) — bear the pedagogical cost of internalizing a partly reconstructed norm; principal exit is leaving Latin
 *   - ecclesiastical_latin_offices: beneficiary (institutional / identity_locked, civilizational horizon) — maintain transmitted liturgical Latin legitimated as carrier, correctable by periodic reform
 *   - academic_publishers_and_academies: beneficiary (institutional / constrained) — produce and sell the standard's artifacts and hold custodial standing
 *   - living_latin_practitioners: excluded payer (moderate / mobile) — fluent-usage norms with no seat in standard-setting bodies
 *   - historical_linguists: analytical observer (institutional / analytical) — document the language's actual evolution and see the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.48).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.5).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin as Classical Form Transmitted and Textually Correctable (Hybrid Reading)").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, 'f6aa333f-d432-4722-9d58-b6310b74c6d1').
narrative_ontology:cs_kernel_codification('f6aa333f-d432-4722-9d58-b6310b74c6d1', formalized).
narrative_ontology:cs_authority_grounding('f6aa333f-d432-4722-9d58-b6310b74c6d1', expertise).
narrative_ontology:cs_interpretation_layer_present('f6aa333f-d432-4722-9d58-b6310b74c6d1').
narrative_ontology:cs_reading_relation('f6aa333f-d432-4722-9d58-b6310b74c6d1', correct_latin__continuity_reading, influences).
narrative_ontology:cs_reading_relation('f6aa333f-d432-4722-9d58-b6310b74c6d1', correct_latin__discontinuity_reading, influences).
narrative_ontology:cs_axiom('f6aa333f-d432-4722-9d58-b6310b74c6d1', foundational, transmitted_practice_partially_normative).
narrative_ontology:cs_axiom_status(transmitted_practice_partially_normative, holdable).
narrative_ontology:cs_axiom_grounding('f6aa333f-d432-4722-9d58-b6310b74c6d1', transmitted_practice_partially_normative, conventional).
narrative_ontology:cs_axiom('f6aa333f-d432-4722-9d58-b6310b74c6d1', foundational, textual_evidence_corrective_authority).
narrative_ontology:cs_axiom_status(textual_evidence_corrective_authority, holdable).
narrative_ontology:cs_axiom_grounding('f6aa333f-d432-4722-9d58-b6310b74c6d1', textual_evidence_corrective_authority, empirically_contingent).
narrative_ontology:cs_axiom('f6aa333f-d432-4722-9d58-b6310b74c6d1', secondary, correction_bounded_not_reoccupation).
narrative_ontology:cs_axiom_status(correction_bounded_not_reoccupation, holdable).
narrative_ontology:cs_axiom_grounding('f6aa333f-d432-4722-9d58-b6310b74c6d1', correction_bounded_not_reoccupation, conventional).
narrative_ontology:cs_reference_frame('f6aa333f-d432-4722-9d58-b6310b74c6d1', classical_norm_via_transmitted_practice).
narrative_ontology:cs_drift_state('f6aa333f-d432-4722-9d58-b6310b74c6d1', contemporary_digital_corpus_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6aa333f-d432-4722-9d58-b6310b74c6d1', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, ecclesiastical_latin_offices).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, academic_publishers_and_academies).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medievalist_scholars).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, latin_students).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, living_latin_practitioners).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, philological_method_doctrine).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, transmission_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University-based scholars who edit the classical texts, author the reference grammars and dictionaries, and train the next generation of editors. When a question of correct form arises anywhere in Latin studies, the field consults their methods: manuscript collation, stemmatic reasoning, lexical documentation. Their careers, journals, congresses, and institutes are organized around maintaining and applying the norm, and the deference the norm commands accrues to them. Leaving the norm would mean leaving the discipline their expertise consists in.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_philologists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, classical_philologists, beneficiary).

% Scholars of medieval Latin texts who work under a norm set elsewhere. Their editions are expected to justify retaining medieval spellings and usages rather than the reverse; their period's written practice is routinely described in the vocabulary of decline and corruption; and the reference works and major journals their careers require run on classical norms. They have carved out diplomatic-edition conventions within their own subfield, but publishing, promotion, and cross-field citation still route through the classical apparatus.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medievalist_scholars, payer,
    organized, generational, constrained, global).

% School and university pupils who must internalize a norm of correctness that is partly the reconstructed usage of ancient authors and partly inherited convention. Every exercise and examination corrects them against it; the reward for compliance is access to the texts and to the credential. Their main way out is to stop studying Latin, which most eventually do; those who stay inherit the costs of conformity for the length of a career.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, latin_students, payer,
    powerless, biographical, mobile, global).

% The Church's liturgical and curial offices maintain a continuous Latin written practice reaching back through the medieval centuries. The standard's hybrid shape suits them: their transmitted forms are recognized as legitimate carriers of the language, while accumulated errors remain correctable by periodic reform. Latin is constitutive of their liturgical self-understanding; abandoning it would dissolve a practice they understand themselves to be, so they remain inside it across institutional generations.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, ecclesiastical_latin_offices, beneficiary,
    institutional, civilizational, identity_locked, global).

% University presses, edition series, lexicographical projects, and learned academies produce the standard's artifacts: critical editions, dictionaries, grammars, examination syllabuses. The norm's authority sustains demand for these products and the custodial standing of their makers; the same bodies are bound by the norm, since their output is judged against it and revisions must be argued from its premises.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, academic_publishers_and_academies, beneficiary,
    institutional, generational, constrained, global).

% Speakers and writers who use Latin as a living language in conversation, composition, teaching, and online communities. Their working criterion of correctness is fluent contemporary usage. They hold no seat on editorial boards, academy committees, or examination bodies, and when their usage diverges from classical forms it is marked as error in the venues that matter for credentials. Practicing outside institutional channels is possible but costs them publications, students, and standing.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, living_latin_practitioners, excluded,
    moderate, biographical, mobile, global).

% Researchers who study how Latin actually changed — phonology, morphology, syntax, register, sociolinguistics — across every period, without endorsing any normative standard. Their work documents that the language was always variable and that the correctness norm is a scholarly construction layered on that variation, which lets them see the whole structure: what the norm coordinates, whom it costs, and where its authority comes from.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, historical_linguists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single norm of Latin form under which a textual corpus spanning two millennia can be edited, taught, examined, and cited unambiguously; editorial emendation, reference lexicography, and pedagogical correction are solved once, centrally, rather than separately by every community of users.
% TRANSFER_FUNCTION: Moves corrective authority and editorial deference from all users and editors of Latin toward the classical philological establishment; moves compliance labor — learning and applying the classical norm, emending transmitted forms, justifying retained medieval usages — from medievalist editors, students, and contemporary writers to the standard's periphery.
% ABSENT_VOICES: Living Latin practitioners would argue correctness should track fluent contemporary usage; they sit outside every editorial board, academy committee, and examination body that defines the standard. The historical community of medieval scribes whose practice the standard adjudicates is unconsulted by construction — its voice survives only as manuscript evidence to be ruled on. Medievalist editors hold partial voice within their subfield but not in the classical reference apparatus.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, critical editions would lose their emendation criterion, curricula and examinations would lose their norm, the reference grammars and lexica would lose their adjudicating role, and the Church's liturgical Latin would lose the frame that legitimates its transmitted forms while permitting correction. Edition practice would reorganize around a pure diplomatic-versus-normalized split, and the authority economy of classical philology would collapse into ordinary historical linguistics.
% FOUNDING_PROBLEM: After Latin ceased to be acquired as a living vernacular and its written practice diversified across regions and centuries, scholars, teachers, and the Church needed a stable answer to what form of the language counts as correct — without one, each generation's usage drifts, editions diverge, and older strata of the corpus become progressively harder to teach and cite.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: medievalist scholars — who pay the standard's costs — attest the transmission problem is real even while disputing the classical norm's reach (diplomatic-edition debates presuppose the problem of divergent transmitted forms); historical linguists attest the drift problem is real as a descriptive matter; and the standard's independent adoption by mutually suspicious institutions (Church curia, secular academies, national school systems) over more than a century corroborates that some shared norm was demanded by the situation rather than imposed by a single beneficiary.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.48: by the hybrid reading's own lights the arrangement coordinates genuinely (one norm makes a two-millennium corpus editable, teachable, and citable) while its corrective apparatus concentrates authority in the philological establishment and bills costs to medievalist editors, students, and non-classical users — real extraction, bounded by the reading's own doctrine that correction is adjustment rather than reoccupation. Suppression 0.50 is authored as a raw structural property, unscaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine's computation): enforcement is real inside the standard's domains — editorial emendation norms, examination standards, peer review — while alternatives persist at the periphery (diplomatic editions, ecclesiastical usage, living-Latin communities), so neither collapse nor tolerance describes it. Theater 0.32: the great corrective work of scientific philology was functional, but as the obvious corruptions were fixed, an increasing share of correction activity normalizes indifferent variants and re-performs settled judgments — hence the rising series. Accessibility_collapse 0.55: within the classical core (critical editions, examinations, reference works) alternatives collapse almost completely; at the periphery they persist. Resistance 0.45: the diplomatic-edition movement, orthography debates, and living-Latin advocacy are real but have won only peripheral concessions without threatening the core standard. The three measurement series share one time grid (t 0-60, mapping roughly to the 1890s consolidation of scientific philology through the 2020s digital-corpus era), with every metric authored at every point; suppression_requirement is authored because this story specifically tracks enforcement-capacity change — the machinery hardened with scientific philology, then relaxed as medievalist practice gained standing and orthographic policy pluralized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute different types from the same structure. From the philologist's position the standard is the discipline's own achievement: correction is a service rendered to the texts and to the field, and the deference it commands is earned expertise. From the medievalist's position the same apparatus is a norm set elsewhere that recasts their period's practice as decline and taxes their editions with justification burdens. From the student's position it is an arbitrary-seeming gatekeeping norm whose rationale is historical rather than practical. From the living-Latinist's position it is an academy norm that ignores the only criterion that matters for a language people actually speak. The engine computes this per-seat divergence from power, exit, and directionality; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: classical_philologists collect the corrective authority and the editorial economy (d near the beneficiary end, stabilized further by identity lock — their expertise is the standard); ecclesiastical_latin_offices receive legitimation of transmitted forms plus a licensed correction channel (low d, with a small upward pull from the corrections their own texts periodically undergo); academic_publishers_and_academies collect the artifact economy (low d, damped slightly by their own subjection to the norm). Targets: medievalist_scholars bear justification burdens and stigmatization with constrained exit (high d); latin_students bear the conformity costs with mobile exit, which damps effective extraction per capita but not the structure (high d, moderated); living_latin_practitioners bear stigmatization of fluent usage with real but costly exit (high d). The historical_linguist seat is analytical and collects nothing. The declarations map cleanly onto the derivation chain, so no directionality overrides are authored.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a stable norm of form for a corpus written across many centuries and regions under divergent conventions — is still live, so this is not a mandatrophy case: no sunset is declared and none is due. The drift signal to watch is theater: the corrective mandate was sized for the great restorations of the nineteenth and early twentieth centuries, and the rising theater_ratio series records the apparatus continuing at scale after the restorations were done. The R5 mismatch check reads founding_problem_status=live against disappearance_verdict=world_rearranges — no dead-mandate flag fires. The classification earns its keep against mislabeling in both directions: a pure coordination reading would miss the extraction billed to the medievalist and student seats; a pure extraction reading would miss the coordination that makes the corpus teachable at all and would wrongly predict that the standard could be abolished without cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the hybrid_reading instantiation of the correct_latin kernel — how would the classification and its extraction topology shift if the kernel were read by its siblings instead?',
    'Generate the sibling stories (continuity_reading, discontinuity_reading) as separate constraint files and compare computed per-seat classifications and epsilon across the family.',
    'Under the continuity reading, transmitted practice is fully normative, so the corrective apparatus itself is the extraction: epsilon rises for the philological establishment while medievalist costs largely vanish. Under the discontinuity reading, all transmitted deviation is corruption: the corrective mandate is total, suppression rises, and medieval practice loses legitimacy entirely. This story''s moderate epsilon is specific to the hybrid settlement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of the correct_latin kernel; siblings would restructure beneficiaries, victims, and epsilon.').

omega_variable(
    normative_source_location,
    'Where exactly do the three readings disagree — which structural element of the standard carries the disagreement?',
    'Analyze what authorities are actually cited when correctness disputes arise in editorial prefaces, examination boards, and lexicographical revisions: precedent of continuous practice, ancient textual evidence, or the two in combination.',
    'The normative source determines which seat holds agenda-setting power: practice-sovereignty moves it to transmitters and living users; text-sovereignty concentrates it in textual critics; the hybrid split divides it — which is why the hybrid reading produces the family''s intermediate extraction topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_source_location, conceptual, 'The disagreement''s location: the normative source of correctness (practice vs. texts vs. practice-as-corrected-carrier).').

omega_variable(
    correction_mandate_scope,
    'Is the corrective mandate actually ''targeted adjustment'' as the hybrid frame claims, or has it expanded into a standing correction economy that no longer tracks the frame?',
    'Audit emendation rates and their stated justifications across edition generations: count corrections that restore sense versus corrections that normalize indifferent variants, and track the ratio over the interval.',
    'A mandate that has outgrown ''targeted'' pushes the arrangement from coordination-with-correction toward enforcement-with-coordination-cover, raising effective extraction on the payer seats and dating a type transition; a genuinely targeted mandate keeps the coordination component dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correction_mandate_scope, empirical, 'Whether the corrective apparatus remains bounded as the reading''s own frame asserts.').

omega_variable(
    medieval_rehabilitation_trajectory,
    'Is the rehabilitation of medieval practice — diplomatic editions, tolerant orthographic policy, respect for scribal habits — a stable pluralization or a transient concession the classical core will reabsorb?',
    'Track orthographic and emendation policy across major edition series and medieval Latin dictionary projects over successive decades; test whether concessions survive personnel and publisher turnover.',
    'If reabsorbed, suppression climbs back and the payer seats'' exit narrows; if stable, the arrangement pluralizes and the extraction component shrinks toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_rehabilitation_trajectory, empirical, 'Direction of the medieval-practice legitimacy trend within the standard''s domains.').

omega_variable(
    correctness_naturalness_ambiguity,
    'Do the standard''s practitioners experience correct Latin as a natural property of the language waiting to be discovered, or as a scholarly convention they maintain?',
    'Analysis of disciplinary rhetoric and pedagogy: whether deviations are framed as violations of the language''s true form or as departures from an agreed convention, and whether reform proposals are argued as discoveries or as policy.',
    'A natural-law framing launders the apparatus''s authority and raises tolerated suppression; a conventional framing exposes the standard as maintained and makes its costs visible and negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correctness_naturalness_ambiguity, conceptual, 'Natural vs. constructed framing of the correctness norm among its enforcers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(correct_latin_hybrid_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(correct_latin_hybrid_tr_t0, observed).
narrative_ontology:measurement(correct_latin_hybrid_tr_t10, correct_latin__hybrid_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(correct_latin_hybrid_tr_t10, observed).
narrative_ontology:measurement(correct_latin_hybrid_tr_t20, correct_latin__hybrid_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(correct_latin_hybrid_tr_t20, observed).
narrative_ontology:measurement(correct_latin_hybrid_tr_t30, correct_latin__hybrid_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(correct_latin_hybrid_tr_t30, observed).
narrative_ontology:measurement(correct_latin_hybrid_tr_t40, correct_latin__hybrid_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(correct_latin_hybrid_tr_t40, observed).
narrative_ontology:measurement(correct_latin_hybrid_tr_t50, correct_latin__hybrid_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(correct_latin_hybrid_tr_t50, observed).
narrative_ontology:measurement(correct_latin_hybrid_tr_t60, correct_latin__hybrid_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(correct_latin_hybrid_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(correct_latin_hybrid_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(correct_latin_hybrid_be_t0, observed).
narrative_ontology:measurement(correct_latin_hybrid_be_t10, correct_latin__hybrid_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(correct_latin_hybrid_be_t10, observed).
narrative_ontology:measurement(correct_latin_hybrid_be_t20, correct_latin__hybrid_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(correct_latin_hybrid_be_t20, observed).
narrative_ontology:measurement(correct_latin_hybrid_be_t30, correct_latin__hybrid_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(correct_latin_hybrid_be_t30, observed).
narrative_ontology:measurement(correct_latin_hybrid_be_t40, correct_latin__hybrid_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(correct_latin_hybrid_be_t40, observed).
narrative_ontology:measurement(correct_latin_hybrid_be_t50, correct_latin__hybrid_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement_basis(correct_latin_hybrid_be_t50, observed).
narrative_ontology:measurement(correct_latin_hybrid_be_t60, correct_latin__hybrid_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement_basis(correct_latin_hybrid_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(correct_latin_hybrid_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(correct_latin_hybrid_su_t0, observed).
narrative_ontology:measurement(correct_latin_hybrid_su_t10, correct_latin__hybrid_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(correct_latin_hybrid_su_t10, observed).
narrative_ontology:measurement(correct_latin_hybrid_su_t20, correct_latin__hybrid_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(correct_latin_hybrid_su_t20, observed).
narrative_ontology:measurement(correct_latin_hybrid_su_t30, correct_latin__hybrid_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(correct_latin_hybrid_su_t30, observed).
narrative_ontology:measurement(correct_latin_hybrid_su_t40, correct_latin__hybrid_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement_basis(correct_latin_hybrid_su_t40, observed).
narrative_ontology:measurement(correct_latin_hybrid_su_t50, correct_latin__hybrid_reading, suppression_requirement, 50, 0.51).
narrative_ontology:measurement_basis(correct_latin_hybrid_su_t50, observed).
narrative_ontology:measurement(correct_latin_hybrid_su_t60, correct_latin__hybrid_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(correct_latin_hybrid_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial notion 'correct Latin' decomposes into three structurally distinct normative claims (continuity_reading, discontinuity_reading, hybrid_reading), each with its own epsilon, beneficiaries, and victims, linked through affects_constraints per the epsilon-invariance principle. This file is the hybrid settlement. The continuity sibling is upstream in the sense that transmission is the primary fact the hybrid reading presupposes; the discontinuity sibling supplies the corrective instrument the hybrid reading borrows. The hybrid reading structurally influences both: each accepted emendation pressures the continuity reading's practice-sovereignty claim, and each legitimated medieval form pressures the discontinuity reading's corruption framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
