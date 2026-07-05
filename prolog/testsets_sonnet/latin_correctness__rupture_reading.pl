% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Classical Latin Correctness Standard — Rupture Reading (Renaissance Humanist Restoration)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This story instantiates the rupture reading of the latin_correctness
 *   kernel: the Renaissance humanist claim that Classical Latin,
 *   reconstructed philologically from ancient manuscript sources (chiefly
 *   Cicero and the Augustan authors), is the sole legitimate standard of
 *   correctness, and that the thousand years of medieval Latin usage —
 *   scholastic philosophy, legal notarial practice, technical and medical
 *   writing — constitutes corruption to be purged rather than a legitimate
 *   continuation or a domain-appropriate register. The story models only this
 *   reading as a clean, ε-invariant constraint: it does not average across
 *   the continuity or hybrid readings, and it does not describe the contest
 *   itself inside the classification. Two sibling constraints
 *   (continuity_reading, hybrid_reading) hold structurally different
 *   beneficiary/victim sets and different ε values under the same kernel;
 *   they are separate files linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.78).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.71).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin Correctness Standard — Rupture Reading (Renaissance Humanist Restoration)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, 'cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee').
narrative_ontology:cs_kernel_codification('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', fixed_text).
narrative_ontology:cs_authority_grounding('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', lineage).
narrative_ontology:cs_interpretation_layer_present('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee').
narrative_ontology:cs_reading_relation('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', foundational, classical_corpus_is_sole_legitimate_standard).
narrative_ontology:cs_axiom_status(classical_corpus_is_sole_legitimate_standard, holdable).
narrative_ontology:cs_axiom_grounding('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', classical_corpus_is_sole_legitimate_standard, conventional).
narrative_ontology:cs_axiom('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', foundational, post_classical_lexical_innovation_is_corruption_not_growth).
narrative_ontology:cs_axiom_status(post_classical_lexical_innovation_is_corruption_not_growth, holdable).
narrative_ontology:cs_axiom_grounding('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', post_classical_lexical_innovation_is_corruption_not_growth, conventional).
narrative_ontology:cs_reference_frame('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', ciceronian_augustan_textual_corpus).
narrative_ontology:cs_drift_state('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', high_medieval_scholastic_flourishing, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cdd03d36-1ec9-4bc3-b9a0-10efc6ef41ee', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_grammarians).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, ciceronian_stylists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, print_house_editors_of_classical_texts).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, elite_latin_academies).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_scholastic_philosophers).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_notarial_and_legal_latinists).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_adjacent_technical_writers).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, provincial_clergy_educated_in_medieval_latin).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, non_elite_students_without_access_to_classical_corpora).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, textual_authenticity_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, ciceronian_style_as_correctness_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compile grammars and philological commentaries that define correct Latin exclusively by reconstructed classical (mainly Ciceronian and Augustan) usage, verified against manuscript sources. They administer the standard through teaching posts, court patronage, and control of print editions, and they personally accrue prestige and employment from certifying what counts as correct.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, humanist_grammarians, agenda_setter,
    institutional, generational, arbitrage, continental).

% Writers and orators who have trained extensively in classical models gain immediate competitive advantage once the rupture standard is enforced in courts, universities, and diplomatic correspondence — their existing skill becomes the new gatekeeping currency without needing to relearn anything.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, ciceronian_stylists, beneficiary,
    powerful, biographical, mobile, continental).

% Produce and sell critical editions of classical authors, grammars, and glossaries purging medieval accretions. Demand for these products is created directly by the rupture standard's insistence that only reconstructed classical usage is legitimate; their revenue depends on the standard's continued authority.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, print_house_editors_of_classical_texts, beneficiary,
    organized, biographical, arbitrage, continental).

% Academies and university faculties built around classical philology certify students and scholars against the rupture standard, controlling admission to prestigious intellectual and clerical careers through Latin competency examinations keyed to classical norms.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, elite_latin_academies, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, elite_latin_academies, agenda_setter).

% Wrote a millennium of philosophy, theology, and logic in a technical Latin vocabulary (quidditas, haecceitas, esse) that has no classical precedent because it names concepts classical Latin never needed. Under the rupture standard their entire corpus is relabeled barbarous and their vocabulary is unsalvageable — there is no reconstruction path back to correctness because the concepts themselves are medieval inventions.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_scholastic_philosophers, payer,
    moderate, generational, trapped, continental).

% Draft charters, contracts, and legal instruments in a functional administrative Latin evolved to handle feudal and canonical concepts absent from the classical world. They cannot simply adopt classical diction because classical Latin has no words for their subject matter; the rupture standard delegitimizes their working language without offering a substitute.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_notarial_and_legal_latinists, payer,
    moderate, generational, trapped, regional).

% Physicians, artisans, and early natural philosophers writing in technical Latin borrowed heavily from vernacular and Arabic-derived terminology to describe instruments, procedures, and materia medica unknown to antiquity. Classical purism has no vocabulary for their domains; they are penalized for using the only Latin capable of expressing their work.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_adjacent_technical_writers, payer,
    powerless, biographical, trapped, regional).

% Trained in the Latin taught by their local schools and monasteries, which is medieval in vocabulary and syntax. When appointments, publication, and clerical advancement come to require classical fluency, they face retraining costs many cannot meet, or permanent exclusion from advancement despite genuine competence in the Latin they were taught.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, provincial_clergy_educated_in_medieval_latin, payer,
    powerless, biographical, constrained, regional).

% Lack access to the manuscript libraries, tutors, and print editions needed to acquire genuine classical fluency, which requires years of specialized instruction and expensive texts. They are measured against a standard whose material preconditions they structurally cannot meet, regardless of effort.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, non_elite_students_without_access_to_classical_corpora, payer,
    powerless, biographical, trapped, local).

% Study the humanist rupture as a historical episode, reconstructing how a reading of Latin correctness was built, whom it credentialed, and whom it erased — able to see the full structure without a stake in either the humanist or scholastic camp.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, modern_classical_philologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__rupture_reading, humanist_grammarians).
narrative_ontology:fixing_cost_class(latin_correctness__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, verifiable textual standard (reconstructed classical usage, checked against manuscript philology) so that Latin literacy across a fragmented continent can be certified and compared on common terms, rather than left to a thousand incompatible regional and institutional dialects.
% TRANSFER_FUNCTION: Moves prestige, teaching posts, court and clerical appointments, and print-market revenue from users of medieval technical and administrative Latin to practitioners and certifiers of reconstructed classical style — a millennium of accumulated technical vocabulary is devalued in the same act that valorizes classical fluency.
% ABSENT_VOICES: Medieval scholastic philosophers and legal notaries, most of them centuries dead, cannot testify to the functional adequacy of their own Latin; living provincial clergy and technical writers who depend on medieval vocabulary have no seat in the humanist academies that set the standard and no counter-institution of comparable prestige.
% DISAPPEARANCE_RATIONALE: If the rupture standard vanished, classical fluency exams would lose their gatekeeping force, humanist print editions would lose their exclusive claim to correctness, and scholastic and technical Latin vocabularies would be free to stand on functional merit rather than classical pedigree — university admissions, clerical advancement criteria, and the market for classical editions would all reorganize.
% FOUNDING_PROBLEM: Latin as actually used across medieval Europe had drifted into regionally divergent, philosophically dense, and administratively specialized forms that humanist scholars perceived as incoherent, ugly, and disconnected from the admired literary achievement of Rome's golden age; the rupture standard was built to recover a single, stable, prestigious model against which correctness could be measured.
% FOUNDING_PROBLEM_CORROBORATION: Humanist grammarians and their institutional heirs attest the problem was real linguistic decay requiring restoration. Modern historical linguists, working outside the humanist tradition's own self-justification, largely reject 'corruption' as a description of medieval Latin's development and instead describe ordinary diachronic change plus functional specialization — this outside corroboration undermines rather than supports the founding narrative, which is why status is marked contested rather than live.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply over the interval (0.35 to 0.78) as humanist grammars, print editions, and academy credentialing systems consolidate around classical-only correctness, converting a stylistic preference into a career gatekeeping mechanism with no available path to compliance for scholastic and technical vocabularies that describe post-classical concepts. Suppression climbs even faster (0.30 to 0.71) because enforcement moves from private taste to institutional examination, court patronage, and print-market dominance — medieval Latin becomes actively unpublishable and uncredentialed rather than merely unfashionable. Theater ratio is moderate and rising (0.42 at T=200): a genuine philological achievement (accurate manuscript reconstruction) is real, but an increasing share of humanist activity is performative purism — denouncing scholastic vocabulary has social value independent of any functional linguistic argument.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist agenda-setter seat, the rupture standard is a restoration of correctness and a genuine coordination achievement (a single verifiable norm across a fragmented linguistic landscape). From the scholastic philosopher or notarial-Latin seat, the identical structure operates as exclusionary extraction: their working language is delegitimized wholesale with no route back to correctness, because the vocabulary gap is conceptual, not stylistic. The engine computes this divergence from the structural beneficiary/victim and exit-option data; the claimed_type (tangled_rope) reflects that both a genuine coordination function and asymmetric extraction are present and structurally inseparable in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanist grammarians and elite academies are structural beneficiaries and agenda-setters: they define the standard, administer certification, and collect prestige and employment from it — d sits near the beneficiary end. Ciceronian stylists and classical print editors are secondary beneficiaries whose existing skills or products are revalued upward by fiat. Medieval scholastic philosophers, legal notarial writers, and technical writers are structural victims: their vocabulary describes concepts classical Latin never had words for, so there is no reconstruction path to compliance — d sits at the full-target end, and their exit is trapped rather than merely constrained, because the concepts (quidditas, notarial formulae, materia medica terms) have no classical equivalent to retreat to.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading risks mislabeling a millennium of functional linguistic evolution as pure decay. The founding problem (perceived incoherence and low prestige of medieval Latin relative to classical literary achievement) is contested rather than dead or clearly live: outside corroboration from modern historical linguistics undermines the corruption narrative, supporting the reading that this coordination function (a shared, verifiable standard) has curdled into an extraction mechanism that disproportionately benefits those already trained in classical models while permanently excluding those whose Latin developed to serve genuinely new conceptual needs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    corruption_vs_evolution_framing,
    'Is medieval Latin''s divergence from classical norms best modeled as corruption of a fixed standard, or as ordinary diachronic language change plus legitimate functional specialization?',
    'Comparative historical linguistics: compare the mechanisms of medieval Latin change (analogical leveling, syntactic simplification, borrowed technical vocabulary) against attested mechanisms of language change in other well-documented traditions without a rupture ideology attached, to test whether ''corruption'' names a distinct process or simply an evaluative label applied post hoc by a rival institutional faction.',
    'If medieval Latin''s development matches ordinary language-change mechanisms, the rupture reading''s core premise (classical is fixed, medieval is decay) is an institutional framing rather than a linguistic fact, strengthening the reading of this constraint as extraction dressed as restoration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_evolution_framing, conceptual, 'Whether ''corruption'' describes a real linguistic process or a value judgment imposed by a competing standard-setting faction.').

omega_variable(
    kernel_framing_alternative_readings,
    'Given that continuity_reading and hybrid_reading are equally coherent framings of the same underlying kernel (what counts as correct Latin), what specific evidence or institutional context justifies treating rupture_reading as the operative constraint for a given historical actor, rather than treating it as one contested claim among three live alternatives?',
    'Trace which reading a given historical institution (a specific university faculty, print house, or court chancery) actually enforced through hiring, publication, and credentialing decisions at a specific date; the enforced reading, not the philosophically ''best'' one, is the operative constraint for that seat.',
    'If an institution''s actual enforcement pattern matches hybrid_reading rather than rupture_reading (e.g., permitting scholastic Latin in canon law faculties while requiring classical style in rhetoric chairs), this story''s claimed extractiveness overstates that institution''s actual practice, and the correct story to cite for that seat is hybrid_reading, not rupture_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_readings, conceptual, 'Framing under-determination among the three sibling readings of the latin_correctness kernel, and what evidence disambiguates which reading a given historical actor actually enforced.').

omega_variable(
    recoverability_of_scholastic_vocabulary,
    'Could scholastic technical vocabulary (quidditas, haecceitas, esse subsistens) in principle be reconstructed or re-derived from classical roots to satisfy purist demands, or is the conceptual gap genuinely unbridgeable within classical Latin''s resources?',
    'Philological analysis of whether humanist-era attempts at Latin coinage for scholastic concepts (and later, Enlightenment or modern classicizing coinages) succeeded in producing classically-derivable equivalents without loss of precision.',
    'If bridgeable, the victim status of scholastic philosophers is a transitional cost rather than a permanent structural exclusion, weakening the trapped exit_options assignment; if unbridgeable, the trapped classification and high extractiveness are structurally justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recoverability_of_scholastic_vocabulary, empirical, 'Whether scholastic Latin''s exclusion under classical purism is a permanent structural bar or a solvable translation problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__rupture_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(lati_tr_t80, latin_correctness__rupture_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement(lati_tr_t120, latin_correctness__rupture_reading, theater_ratio, 120, 0.38).
narrative_ontology:measurement(lati_tr_t160, latin_correctness__rupture_reading, theater_ratio, 160, 0.41).
narrative_ontology:measurement(lati_tr_t200, latin_correctness__rupture_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lati_be_t40, latin_correctness__rupture_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(lati_be_t80, latin_correctness__rupture_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(lati_be_t120, latin_correctness__rupture_reading, base_extractiveness, 120, 0.74).
narrative_ontology:measurement(lati_be_t160, latin_correctness__rupture_reading, base_extractiveness, 160, 0.77).
narrative_ontology:measurement(lati_be_t200, latin_correctness__rupture_reading, base_extractiveness, 200, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__rupture_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lati_su_t40, latin_correctness__rupture_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(lati_su_t80, latin_correctness__rupture_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(lati_su_t120, latin_correctness__rupture_reading, suppression_requirement, 120, 0.68).
narrative_ontology:measurement(lati_su_t160, latin_correctness__rupture_reading, suppression_requirement, 160, 0.7).
narrative_ontology:measurement(lati_su_t200, latin_correctness__rupture_reading, suppression_requirement, 200, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__rupture_reading, 0.08).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is the rupture_reading member of the latin_correctness kernel family (3 stories: rupture_reading, continuity_reading, hybrid_reading). Each reading instantiates a structurally distinct constraint with its own ε, beneficiary/victim set, and claimed_type, per the ε-invariance principle — they are not the same constraint viewed from different angles. rupture_reading carries the highest ε and the broadest victim set because it delegitimizes medieval Latin wholesale across literary, technical, and administrative domains alike; continuity_reading and hybrid_reading partition or reject that delegitimization and should show correspondingly lower extraction and narrower or absent victim sets. All three link to each other via affects_constraints as required for kernel families.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
