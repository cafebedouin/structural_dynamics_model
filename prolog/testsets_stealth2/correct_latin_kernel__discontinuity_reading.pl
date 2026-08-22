% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Classical-Latin Correctness Standard (Discontinuity Reading)
 *   domain: historical linguistics/philology/intellectual history
 *
 * SUMMARY:
 *   From Petrarch's fourteenth-century sense of standing amid ruins, the
 *   humanist movement built a regime in which correct Latin meant classical
 *   Latin: the post-classical written tradition of the Middle Ages was
 *   reclassified as barbarous corruption, and legitimate command of the
 *   language had to be rebuilt by occupying the symbolic space the ancients
 *   left behind — grammars, rhetorical handbooks, and above all the recovered
 *   texts themselves. This story instantiates the discontinuity_reading of
 *   the correct_latin_kernel: Classical and Medieval Latin are distinct
 *   systems, the living transmission chain broke, and reconstruction
 *   necessarily proceeded by symbolic reoccupation from texts. The reading is
 *   not neutral description; it licensed two centuries of institutional
 *   construction — humanist chanceries, printed school grammars, Ciceronian
 *   examination cultures — that moved careers, patronage, and legitimacy to
 *   whoever mastered the recovered corpus while devaluing the competence of
 *   an entire existing class of medieval-trained writers. Sibling readings
 *   (continuity_reading, hybrid_reading) instantiate different constraints
 *   from the same kernel and are linked in network.affects_constraints; per
 *   the epsilon-invariance principle each carries its own epsilon,
 *   beneficiaries, and victims. Claim/metric independence holds: the
 *   constraint is CLAIMED as tangled_rope from this seat, while the metrics
 *   are authored as descriptive facts about the arrangement's operation.
 *
 * KEY AGENTS:
 *   - - humanist_literati: Primary beneficiary (organized/identity_locked) — collects careers, patronage, and authority from the restored standard; bound to it by formed identity
 *   - - classical_text_printers: Secondary beneficiary and agenda-setter (powerful/mobile) — profits from and shapes the textual canon
 *   - - elite_patrons: Beneficiary (powerful/arbitrage) — converts the standard into distinction; can defect to vernacular patronage
 *   - - medieval_trained_clerics: Primary target (organized/constrained) — bears the devaluation of existing competence
 *   - - scholastic_university_faculties: Institutional target (institutional/constrained) — loses standing and enrollment
 *   - - grammar_school_students: Primary target (powerless/trapped) — bears the training cost; no coalition seat available
 *   - - classics_professors: Contemporary agenda-setter (institutional/identity_locked) — administers the standard today
 *   - - vernacular_writers: Excluded voice (organized/mobile) — argues from outside the adjudication
 *   - - historiographical_observers: Analytical observer — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.63).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Classical-Latin Correctness Standard (Discontinuity Reading)").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical linguistics/philology/intellectual history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '96f5660b-1934-4f6f-b20f-3afe61bd34a7').
narrative_ontology:cs_kernel_codification('96f5660b-1934-4f6f-b20f-3afe61bd34a7', fixed_text).
narrative_ontology:cs_authority_grounding('96f5660b-1934-4f6f-b20f-3afe61bd34a7', lineage).
narrative_ontology:cs_interpretation_layer_present('96f5660b-1934-4f6f-b20f-3afe61bd34a7').
narrative_ontology:cs_reading_relation('96f5660b-1934-4f6f-b20f-3afe61bd34a7', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('96f5660b-1934-4f6f-b20f-3afe61bd34a7', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('96f5660b-1934-4f6f-b20f-3afe61bd34a7', foundational, transmission_chain_broken).
narrative_ontology:cs_axiom_status(transmission_chain_broken, holdable).
narrative_ontology:cs_axiom_grounding('96f5660b-1934-4f6f-b20f-3afe61bd34a7', transmission_chain_broken, empirically_contingent).
narrative_ontology:cs_axiom('96f5660b-1934-4f6f-b20f-3afe61bd34a7', foundational, correction_derives_from_ancient_texts_only).
narrative_ontology:cs_axiom_status(correction_derives_from_ancient_texts_only, holdable).
narrative_ontology:cs_axiom_grounding('96f5660b-1934-4f6f-b20f-3afe61bd34a7', correction_derives_from_ancient_texts_only, instrumental).
narrative_ontology:cs_reference_frame('96f5660b-1934-4f6f-b20f-3afe61bd34a7', ciceronian_canonical_norm).
narrative_ontology:cs_drift_state('96f5660b-1934-4f6f-b20f-3afe61bd34a7', contemporary_medieval_studies_rehabilitation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96f5660b-1934-4f6f-b20f-3afe61bd34a7', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, humanist_literati).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_text_printers).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, elite_patrons).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_trained_clerics).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, scholastic_university_faculties).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, grammar_school_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose, edit, and teach from the recovered ancient corpus; their careers, patronage, and self-conception were built on classical competence acquired through decades of textual immersion. They police one another's prose for post-classical lapses, and stepping off the standard would forfeit both livelihood and the identity the training formed.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, humanist_literati, beneficiary,
    organized, generational, identity_locked, continental).

% Print corrected editions of ancient authors together with the grammars and style manuals the schools require; revenue scales with demand for the classical standard, and decisions about which texts to issue and which readings to print shape what counts as correct. Capital can move to other genres if the market turns.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_text_printers, beneficiary,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, classical_text_printers, agenda_setter).

% Princes, cardinals, and city oligarchies fund humanists, collect manuscripts, and display the restored standard as a mark of cultivation; patronage can be redirected to vernacular letters whenever that yields more distinction.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, elite_patrons, beneficiary,
    powerful, biographical, arbitrage, continental).

% Chancery secretaries, notaries, and clergy formed in the post-classical written tradition watch their competence relabeled as barbarism; preferment increasingly goes to those retrained on ancient models. Older institutional niches still accept their usage, but advancement flows through the new standard.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_trained_clerics, payer,
    organized, biographical, constrained, continental).

% Theology and arts faculties lecture, dispute, and publish in the inherited scholastic register; enrollment, patronage, and public standing migrate toward classically styled rivals, and curricular defense buys time rather than reversal.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, scholastic_university_faculties, payer,
    institutional, generational, constrained, continental).

% Children in the new grammar schools spend six to ten years on precepts, imitation exercises, and Latin verse; they neither set nor choose the curriculum, and withdrawal forfeits the credential their families are paying for.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, grammar_school_students, payer,
    powerless, immediate, trapped, national).

% Contemporary custodians who examine, certify, and transmit the classical standard through universities and school systems; their professional authority rests on the corpus they administer, and the discipline's posts, journals, and societies reproduce it.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classics_professors, agenda_setter,
    institutional, generational, identity_locked, global).

% Poets and prose writers in Italian, French, English, and German argue that their languages can carry learning and glory; they hold no seat where Latinity is adjudicated, and resources flow to Latin schooling ahead of theirs.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, vernacular_writers, excluded,
    organized, generational, mobile, national).

% Historians of language and education reconstruct what the classicizing program did to the medieval inheritance; holding no stake in the standard's upkeep, they can set the discontinuity account against the documentary record of medieval usage.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, historiographical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, humanist_literati).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, drift-resistant written code for learned exchange across a multilingual continent: anchoring correctness to a closed classical corpus lets a document composed in Krakow be parsed in Salamanca a generation later, and lets authority attach to texts that outlive their authors' speech communities.
% TRANSFER_FUNCTION: Moves roughly a decade of each student's training labor into classical imitation; moves fees, posts, and patronage toward the classically credentialed; moves legitimacy from living regional usage traditions to a canonical textual corpus and its credentialed interpreters.
% ABSENT_VOICES: Medieval-trained practitioners judged their own usage orderly and sufficient but were characterized as barbarians in absentia — the humanist conversation assigned them a place only as cautionary examples. Women were excluded from Latin schooling altogether and thus from the entire adjudication. Vernacular writers argued for their languages' capacity but held no seat where Latinity was ruled on.
% DISAPPEARANCE_RATIONALE: If the standard and its enforcement vanished overnight, the learned world would reorganize within a generation: chanceries and faculties would revert to regional post-classical registers or accelerate the vernacular turn already underway, the school system's central product would disappear, and the textual-editing economy built on the classical corpus would collapse — arrangements across education, religion, diplomacy, and scholarship visibly depend on it.
% FOUNDING_PROBLEM: Restore a lost classical standard: learned Europe believed the language of law, church, and scholarship had decayed into regional barbarism after the fall of Rome, and that its classical integrity had to be recovered from the ancient texts for the respublica litteraria to speak with one authoritative voice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of education and Neo-Latin philology, working from school statutes, chancery records, and print runs rather than from classics-department self-description, attest that the restorative program declared itself complete — the campaign against barbarism was won by the mid-sixteenth century, after which the apparatus turned to transmission and examination. The arrangement's own celebratory histories confirm the completion; no source inside or outside disputes that the original restorative problem was solved.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects a real transfer: fees, posts, and deference moved to the classically credentialed while the previously competent were reclassified as defective. Suppression (0.63) is authored as a raw structural property — unscaled by power or scope — capturing the enforcement machinery (school statutes, chancery style rules, patronage gatekeeping, ecclesiastical preference) that made non-classical usage career-costly without ever outlawing it. Theater (0.30): the coordination product was real — centuries of stable cross-regional learned prose — but a growing share of activity served intra-elite stylistic competition rather than communication. Accessibility_collapse (0.60): within learned Latin writing the alternatives collapsed hard once the standard was institutionalized, but vernacular exit existed and widened after 1550, keeping the value below mountain-range. Resistance (0.50): scholastic mockery, defenses of medieval usage, and eventual vernacular defection met the standard throughout. The measurement series share one seven-point grid (1350-1650 at fifty-year steps) so every metric is authored at every examined time point; trajectories rise through the institutionalization phase (1450-1550) and plateau as the standard saturates its market. Suppression_requirement is tracked because the story's central dynamic is enforcement-capacity build-up: from scattered literary taste to statutory school systems, peaking around 1600 and beginning to relax as vernaculars absorb functions.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist and classics seats the arrangement computes as the coordination it built: a drift-proof common code, the rescue of a civilization's textual inheritance, a meritocracy of philological skill. From the payer seats the same structure computes as dispossession: a lifetime of acquired competence declared worthless, children conscripted into a decade of imitation exercises, faculties stripped of standing by a fashion they did not set. Grammar-school students are the clearest powerless seat — individually replaceable, geographically dispersed, and without a forum in which to refuse — so no coalition power ever materialized against the curriculum from below; resistance came only from organized seats (faculties, clerical corporations) and was ultimately absorbed. The engine computes these divergent per-seat classifications from the structural data (opposed roles at comparable power levels); nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   humanist_literati sit near the beneficiary pole (d low): the standard subsidizes them with careers and authority. Their identity_locked exit does not make them targets — the lock binds them to a structure that pays them; it raises the cost of exit without reversing the flow of benefit, so the structural derivation from their beneficiary declaration correctly keeps d low. classical_text_printers and elite_patrons likewise derive low d; the printers' editorial agenda-setting is recorded as a secondary role, not as extraction borne. medieval_trained_clerics, scholastic_university_faculties, and grammar_school_students derive high d: they bear the transfer with constrained or trapped exit, and trapped or identity-locked targets sit nearer the full-target end than mobile ones. classics_professors inherit the agenda-setter seat with an identity lock analogous to the humanists'. vernacular_writers are excluded rather than coordinated — keeping them outside the adjudication is part of what the enforcement maintains. Continental spatial scope applies modest amplification: verifying 'correctness' across a continent is harder than locally, so effective extraction scales up somewhat from the base value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — restore a lost classical standard to a learned Europe convinced its common tongue had rotted — was substantially accomplished by the late sixteenth century: the corpus was recovered and emended, grammars codified, the standard teachable at scale. The arrangement nonetheless persists, administered today by a discipline whose authority rests on the corpus it maintains. Authoring the R5 interview honestly (founding_problem_status: dead against disappearance_verdict: world_rearranges) surfaces the mismatch flag rather than hiding it: this is the signature of a mandate that outlived its function and now reproduces itself institutionally. The tangled_rope claim prevents both failure modes: a pure-extraction reading would erase the genuine centuries-long coordination achievement (stable pan-European learned communication, textual criticism as a discipline), while a pure-coordination reading would erase the manufactured victim class (devalued medieval competence, gated access, purist surplus training). The classification holds both truths in one structure, and the temporal data show which component is growing: theater rises, extraction plateaus, enforcement peaks and begins to relax — the drift profile of a coordination shell thickening around a completed mission.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the correct_latin_kernel — the discontinuity_reading. How would the sibling readings restructure the constraint''s beneficiary/victim surface?',
    'Author the sibling stories (continuity_reading, hybrid_reading) and compare computed classifications; the disagreement locates in whether the classical-to-medieval transmission constitutes a system break or an evolution.',
    'Under continuity_reading the devalued-competence victim class dissolves (no corruption to purge, no retraining levy) and epsilon drops sharply; under hybrid_reading the victim set partitions (morphology users unharmed, syntax/lexicon users harmed) and extraction concentrates differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story instantiates one of three readings of the correct-Latin kernel; siblings are separate constraints.').

omega_variable(
    corruption_vs_coherent_variety,
    'Is ''Medieval Latin as corruption'' a linguistic fact or an evaluative frame imposed by Renaissance classicizing taste on a coherent, rule-governed variety?',
    'Comparative grammatical description of medieval Latin''s internal regularity (its own word-order norms, lexicon growth, orthographic conventions) against the classical benchmark, independent of humanist rhetoric.',
    'If medieval Latin is a coherent variety, the discontinuity reading manufactured a victim class out of competent users and effective extraction rises; if genuine decay from the classical norm, the restorative claim strengthens and part of the measured extraction is the price of repair.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_coherent_variety, conceptual, 'Whether the corruption judgment is descriptive or evaluative dressing on a rival variety.').

omega_variable(
    anchor_necessity_vs_closure,
    'Did anchoring correctness to a fixed classical corpus deliver coordination value unavailable to a continuously evolving standard (drift-proofing across regions and generations), or was the discontinuity premise retroactive cover for closing a credential circle?',
    'Compare coordination outcomes in domains that kept an evolved working standard (canon-law and scholastic registers) against classicized domains, on cross-regional intelligibility, document longevity, and training cost per competent writer.',
    'If the anchor was necessary, a large share of measured extraction is genuine coordination cost and the balance shifts toward rope; if dispensable, the extraction is closer to pure gating and the snare component grows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anchor_necessity_vs_closure, empirical, 'Whether the fixed-textual-anchor design was functionally required or socially convenient.').

omega_variable(
    pedagogical_inflation,
    'Was multi-year Ciceronian training proportionate to the communicative needs of learned writing, or inflated beyond function by status competition (the purism spirals Erasmus satirized in the Ciceronianus)?',
    'Compare attainment-versus-training-time curves across periods and institutions; locate the threshold beyond which added classical polish yields no measurable gain in document function or audience reach.',
    'Training beyond the functional threshold is surplus borne by students; proportionate training is coordination cost. Resolves how much of the student-seat burden is rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_inflation, empirical, 'Whether the pedagogical price of the standard tracked its communicative yield.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 1350, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1350, correct_latin_kernel__discontinuity_reading, theater_ratio, 1350, 0.08).
narrative_ontology:measurement(corr_tr_t1400, correct_latin_kernel__discontinuity_reading, theater_ratio, 1400, 0.11).
narrative_ontology:measurement(corr_tr_t1450, correct_latin_kernel__discontinuity_reading, theater_ratio, 1450, 0.17).
narrative_ontology:measurement(corr_tr_t1500, correct_latin_kernel__discontinuity_reading, theater_ratio, 1500, 0.23).
narrative_ontology:measurement(corr_tr_t1550, correct_latin_kernel__discontinuity_reading, theater_ratio, 1550, 0.27).
narrative_ontology:measurement(corr_tr_t1600, correct_latin_kernel__discontinuity_reading, theater_ratio, 1600, 0.29).
narrative_ontology:measurement(corr_tr_t1650, correct_latin_kernel__discontinuity_reading, theater_ratio, 1650, 0.3).

% Extraction over time
narrative_ontology:measurement(corr_be_t1350, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1350, 0.34).
narrative_ontology:measurement(corr_be_t1400, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1400, 0.41).
narrative_ontology:measurement(corr_be_t1450, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1450, 0.52).
narrative_ontology:measurement(corr_be_t1500, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1500, 0.6).
narrative_ontology:measurement(corr_be_t1550, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1550, 0.63).
narrative_ontology:measurement(corr_be_t1600, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1600, 0.61).
narrative_ontology:measurement(corr_be_t1650, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1650, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1350, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1350, 0.22).
narrative_ontology:measurement(corr_su_t1400, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1400, 0.31).
narrative_ontology:measurement(corr_su_t1450, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1450, 0.44).
narrative_ontology:measurement(corr_su_t1500, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1500, 0.57).
narrative_ontology:measurement(corr_su_t1550, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1550, 0.63).
narrative_ontology:measurement(corr_su_t1600, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1600, 0.66).
narrative_ontology:measurement(corr_su_t1650, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1650, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of correct_latin_kernel: continuity_reading (one evolving system; internal correction; no manufactured victim class; lowest epsilon), this discontinuity_reading (distinct systems; textual reoccupation; wholesale devaluation of medieval competence; highest epsilon), and hybrid_reading (layered recovery; partitioned victim set; intermediate epsilon). The continuity account is upstream — cited as the descriptive baseline — while the discontinuity account drove the institutional enforcement whose costs the other two readings dispute. Each member links the others here; each carries its own epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
