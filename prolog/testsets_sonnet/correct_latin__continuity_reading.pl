% ============================================================================
% CONSTRAINT STORY: correct_latin__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: correct_latin__continuity_reading
 *   human_readable: Continuity Reading: Latin Correctness as Living Transmitted Practice
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the contested 'correct
 *   Latin' kernel: correctness is defined by unbroken transmission through
 *   living practice, so medieval Latin is not a corruption of Classical Latin
 *   but its legitimate evolved continuation, in the same sense that no living
 *   Romance language is a 'corruption' of the Latin it descends from. This
 *   reading is generated as a single, ε-invariant constraint — it does not
 *   describe or average over the discontinuity reading
 *   (Classical-form-as-fixed-standard, medieval-as-error) or the hybrid
 *   reading (partial continuity with targeted textual correction). Those are
 *   separate constraints in the same family, linked via
 *   network.affects_constraints. The continuity reading's coordination
 *   function is real (a stable descriptive standard for an enormous span of
 *   usage) but it also redistributes scholarly and pedagogical legitimacy
 *   away from the classicizing-humanist correction tradition, which is why
 *   beneficiaries and a victim group are both declared despite the underlying
 *   claim being, in a narrow sense, a fact-of-the-matter question about
 *   language change.
 *
 * KEY AGENTS:
 *   - medieval_and_ecclesiastical_latinists: beneficiary (moderate/mobile) — their object of study is validated as legitimate rather than corrupt
 *   - practicing_clergy_and_notaries: beneficiary (moderate/constrained) — historical working users of Latin whose usage is retroactively legitimized
 *   - vernacular_derived_romance_scholarship: beneficiary/observer (moderate/mobile) — methodological premise of unbroken transmission is vindicated
 *   - classicizing_humanist_purists: payer (moderate/constrained) — normative authority of the classical-standard correction apparatus is undercut
 *   - medieval_manuscript_scribes_historical: excluded (powerless/trapped) — the historical usage being adjudicated has no voice in the adjudication
 *   - comparative_historical_linguists: observer (analytical) — assesses the claim against general theories of language change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__continuity_reading, 0.38).
domain_priors:suppression_score(correct_latin__continuity_reading, 0.42).
domain_priors:theater_ratio(correct_latin__continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__continuity_reading, rope).
narrative_ontology:human_readable(correct_latin__continuity_reading, "Continuity Reading: Latin Correctness as Living Transmitted Practice").
narrative_ontology:topic_domain(correct_latin__continuity_reading, "historical_linguistics/philology/intellectual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__continuity_reading, 'de670533-18af-4402-8ef1-9edd2082b344').
narrative_ontology:cs_kernel_codification('de670533-18af-4402-8ef1-9edd2082b344', distributed).
narrative_ontology:cs_authority_grounding('de670533-18af-4402-8ef1-9edd2082b344', practice).
narrative_ontology:cs_interpretation_layer_present('de670533-18af-4402-8ef1-9edd2082b344').
narrative_ontology:cs_reading_relation('de670533-18af-4402-8ef1-9edd2082b344', correct_latin__discontinuity_reading, forecloses).
narrative_ontology:cs_reading_relation('de670533-18af-4402-8ef1-9edd2082b344', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('de670533-18af-4402-8ef1-9edd2082b344', foundational, living_use_constitutes_correctness).
narrative_ontology:cs_axiom_status(living_use_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('de670533-18af-4402-8ef1-9edd2082b344', living_use_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('de670533-18af-4402-8ef1-9edd2082b344', foundational, no_discrete_rupture_between_classical_and_medieval_latin).
narrative_ontology:cs_axiom_status(no_discrete_rupture_between_classical_and_medieval_latin, holdable).
narrative_ontology:cs_axiom_grounding('de670533-18af-4402-8ef1-9edd2082b344', no_discrete_rupture_between_classical_and_medieval_latin, empirically_contingent).
narrative_ontology:cs_reference_frame('de670533-18af-4402-8ef1-9edd2082b344', unbroken_vernacular_transmission_standard).
narrative_ontology:cs_drift_state('de670533-18af-4402-8ef1-9edd2082b344', post_comparative_linguistics_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('de670533-18af-4402-8ef1-9edd2082b344', '').
narrative_ontology:cs_kernel_id(correct_latin__continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, medieval_and_ecclesiastical_latinists).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, practicing_clergy_and_notaries).
narrative_ontology:constraint_beneficiary(correct_latin__continuity_reading, vernacular_derived_romance_scholarship).
narrative_ontology:constraint_victim(correct_latin__continuity_reading, classicizing_humanist_purists).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, language_change_is_not_corruption).
narrative_ontology:constraint_vindicates(correct_latin__continuity_reading, diachronic_continuity_of_linguistic_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars who study, teach, and edit medieval Latin texts on their own terms. Under the continuity reading, their object of study is fully legitimate Latin rather than a corrupted derivative, which validates their editorial choices, their reliance on medieval orthography and syntax, and the institutional standing of medieval studies as a field distinct from classical philology.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_and_ecclesiastical_latinists, beneficiary,
    moderate, generational, mobile, continental).

% Historically (and in surviving liturgical/legal contexts), users of Latin as a working administrative and liturgical language who wrote and spoke the language as it had evolved through daily use. The continuity reading retroactively legitimizes their usage as authentic Latin rather than debased imitation, removing the burden of measuring their competence against a frozen ancient standard they never learned to reproduce.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, practicing_clergy_and_notaries, beneficiary,
    moderate, biographical, constrained, regional).

% Historical linguists tracing the emergence of Romance vernaculars from Latin. The continuity reading supports their central methodological premise: that Romance languages descend through an unbroken chain of spoken Latin usage, with medieval Latin as one visible waypoint rather than a written artifice imposed over a dead standard.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, vernacular_derived_romance_scholarship, beneficiary,
    moderate, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__continuity_reading, vernacular_derived_romance_scholarship, observer).

% Scholars and pedagogical traditions (descending from Renaissance humanism) whose authority rests on Classical Latin as a fixed, recoverable, superior standard against which all later usage is measured and corrected. The continuity reading strips their reconstruction project of its normative force: if medieval forms are legitimate evolution rather than error, the humanist correction apparatus (grammars, style guides, 'purified' editions) loses its claim to be restoring something rather than merely preferring something.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, classicizing_humanist_purists, payer,
    moderate, civilizational, constrained, continental).

% The historical writers and copyists whose actual usage is being adjudicated centuries after the fact. They have no voice in the debate over whether their Latin was legitimate language or corrupted imitation; the kernel contest is conducted entirely by later scholars using their surviving texts as evidence for arguments they could not have anticipated or contested.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, medieval_manuscript_scribes_historical, excluded,
    powerless, civilizational, trapped, regional).

% Linguists applying general theories of language change (sound laws, grammaticalization, contact-induced change) to the Latin-to-Romance transition. They assess the continuity claim against cross-linguistic evidence of how living languages change without becoming 'incorrect,' independent of the normative stakes internal to classical philology.
narrative_ontology:constraint_stakeholder(correct_latin__continuity_reading, comparative_historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(correct_latin__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-adjudicated standard of correctness for describing and teaching Latin across a very long historical span, avoiding the need for a separate normative apparatus for every century of usage — practitioners of any period's Latin can treat their own usage as continuous with the language rather than as a permanent deviation from an unreachable ancient target.
% TRANSFER_FUNCTION: Moves scholarly and pedagogical legitimacy from the classicizing correction tradition (humanist grammars, purist editorial practice) toward medievalist and diachronic-linguistic scholarship: institutional prestige, curricular space, and the authority to declare a given text 'good Latin' shift toward those who treat medieval forms as legitimate rather than toward those who treat them as errors to be flagged and corrected.
% ABSENT_VOICES: The historical Latin-speaking and Latin-writing populations of the medieval period cannot speak to whether they understood themselves as using a legitimate evolving language or as failing to reproduce a lost standard; the entire contest is conducted by later observers on their behalf, using textual residue as the only available evidence.
% DISAPPEARANCE_RATIONALE: If the continuity reading were simply retracted, medievalist scholarship would not vanish, but its normative footing would shift: medieval Latin texts would need to be re-described either as corrupted deviation (discontinuity reading) or as partially correctable hybrid material (hybrid reading), altering how editions are prepared, how the field is taught, and which grammars are treated as authoritative. Humanist-purist institutions would regain some ground; medievalist institutions would need a different justificatory frame. Whether this counts as 'the world rearranging' or 'a relabeling with no operational change' is exactly what the three readings dispute among themselves.
% FOUNDING_PROBLEM: How to characterize the relationship between the Latin of classical antiquity and the Latin actually written and spoken across the following millennium, given that they differ systematically in phonology, morphology, and syntax, and given that some later scholars wanted to correct later usage back toward the earlier form.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical linguists, working from general theories of language change rather than from investment in either camp, broadly corroborate that living languages change continuously without discrete 'corruption events' — supporting the continuity reading's core premise from outside the philological dispute itself. Humanist-tradition classicists, who are the losing party under this reading, dispute the premise; their corroboration is absent by definition, which is itself informative about whose ox is gored.
narrative_ontology:disappearance_verdict(correct_latin__continuity_reading, contested).
narrative_ontology:founding_problem_status(correct_latin__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__continuity_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__continuity_reading_tests).
:- end_tests(correct_latin__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.38) and rises only slowly across the interval: the continuity reading does not extract resources through coercion so much as reallocate scholarly legitimacy and curricular authority over centuries, a slow institutional drift rather than an acute transfer. Suppression is moderate (0.42) — the reading does not forcibly silence the discontinuity position (both readings remain publishable, teachable, live positions), but it does structurally deprioritize purist correction as a legitimate editorial practice within institutions that adopt it, which is a real, if soft, form of suppression. Theater ratio is modest and rises gradually (0.10 to 0.28) reflecting increasing performative citation of 'living language' rhetoric in some pedagogical contexts without correspondingly deep methodological engagement with contact linguistics. Accessibility collapse is moderate (0.45): once a department or tradition adopts the continuity framing, the humanist correction apparatus becomes harder to justify internally, though it never fully disappears as an available alternative elsewhere. Resistance is real and substantial (0.55) because the classicizing tradition has centuries of institutional weight and does not concede the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Medievalist and diachronic-linguistic scholars are declared beneficiaries because the continuity reading is the frame that validates their central objects and methods without requiring them to treat their material as perpetually deficient. Historical clergy and notaries are beneficiaries in the sense that their actual usage — which they could not have altered even if a purist standard had been imposed — is retroactively described as correct rather than as failure. Classicizing humanist purists are the payer group: their entire correction project (grammars, purified editions, pedagogical drilling toward a classical target) depends on medieval usage being treated as deviation to be fixed, and the continuity reading removes that premise. This is a directionality relationship mediated through institutional and disciplinary standing, not through material coercion, which is why suppression is moderate rather than high.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to characterize a millennium of documented linguistic change without a false discrete rupture — remains genuinely live; it is not a mandate that has outlived its function, because the underlying empirical question (was there a break or a continuum?) is still actively debated with new manuscript and sociolinguistic evidence. This blocks a mandatrophy verdict: the continuity reading is not zombie machinery defending an obsolete purpose, it is one live position in an ongoing, well-evidenced dispute. The disappearance_verdict is marked contested rather than world_rearranges precisely because whether retracting this reading would meaningfully change scholarly practice, or merely relabel the same editorial choices under a different justificatory frame, is itself part of what the three sibling readings disagree about.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_reading_kernel_choice,
    'Is ''continuous living practice'' actually the correct standard for linguistic correctness, or is it one defensible reading among several of an underdetermined kernel (what makes a later form of a language ''correct'')?',
    'No purely empirical resolution exists — this is a definitional/normative choice about what ''correctness'' means for a natural language, not a fact that historical or comparative linguistic evidence alone settles; comparative linguistics can describe the mechanisms of change but cannot adjudicate whether change constitutes corruption or evolution.',
    'If the continuity framing is treated as itself contestable rather than settled, the beneficiary/victim structure declared here softens: humanist purists are not simply ''wrong,'' they are holding a coherent alternative normative commitment, which would argue for treating this constraint''s classification as more contested (tangled_rope-adjacent) than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_reading_kernel_choice, conceptual, 'Whether continuity-as-correctness is a fact or a framing choice among kernel readings.').

omega_variable(
    sibling_reading_structural_delta,
    'What specifically changes structurally (beneficiaries, victims, extraction level) between this continuity reading and the discontinuity and hybrid readings of the same kernel?',
    'Compare the three sibling constraint files directly: discontinuity_reading should show classicizing purists as beneficiaries and medievalists as payers (an inversion of this file''s structure); hybrid_reading should show a flatter, lower-extraction profile with both groups partially validated.',
    'Confirms the ε-invariance discipline was followed correctly — each reading should have a stable, distinct ε and beneficiary/victim set rather than a blended or hedged profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents the expected structural contrast across the three linked kernel readings.').

omega_variable(
    manuscript_evidence_asymmetry,
    'Does the surviving manuscript record itself introduce a bias toward one reading — e.g., because ecclesiastical and administrative Latin is overrepresented in what survives, is the continuity reading partly an artifact of which texts happened to be preserved?',
    'Systematic review of manuscript survival rates by genre and region, cross-checked against known scriptoria practices and loss patterns, to assess whether the corpus used to argue continuity is representative or skewed toward institutional (hence more ''living-practice'') Latin.',
    'If survival bias is substantial, the empirical support for continuity-as-observed is weaker than it appears, which would raise resistance and lower confidence in the beneficiary claims made here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(manuscript_evidence_asymmetry, empirical, 'Whether manuscript survival bias inflates apparent support for the continuity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__continuity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t100, correct_latin__continuity_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(corr_tr_t100, observed).
narrative_ontology:measurement(corr_tr_t200, correct_latin__continuity_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement_basis(corr_tr_t200, observed).
narrative_ontology:measurement(corr_tr_t300, correct_latin__continuity_reading, theater_ratio, 300, 0.19).
narrative_ontology:measurement_basis(corr_tr_t300, observed).
narrative_ontology:measurement(corr_tr_t400, correct_latin__continuity_reading, theater_ratio, 400, 0.22).
narrative_ontology:measurement_basis(corr_tr_t400, observed).
narrative_ontology:measurement(corr_tr_t500, correct_latin__continuity_reading, theater_ratio, 500, 0.25).
narrative_ontology:measurement_basis(corr_tr_t500, observed).
narrative_ontology:measurement(corr_tr_t600, correct_latin__continuity_reading, theater_ratio, 600, 0.28).
narrative_ontology:measurement_basis(corr_tr_t600, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t100, correct_latin__continuity_reading, base_extractiveness, 100, 0.24).
narrative_ontology:measurement_basis(corr_be_t100, observed).
narrative_ontology:measurement(corr_be_t200, correct_latin__continuity_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement_basis(corr_be_t200, observed).
narrative_ontology:measurement(corr_be_t300, correct_latin__continuity_reading, base_extractiveness, 300, 0.32).
narrative_ontology:measurement_basis(corr_be_t300, observed).
narrative_ontology:measurement(corr_be_t400, correct_latin__continuity_reading, base_extractiveness, 400, 0.34).
narrative_ontology:measurement_basis(corr_be_t400, observed).
narrative_ontology:measurement(corr_be_t500, correct_latin__continuity_reading, base_extractiveness, 500, 0.36).
narrative_ontology:measurement_basis(corr_be_t500, observed).
narrative_ontology:measurement(corr_be_t600, correct_latin__continuity_reading, base_extractiveness, 600, 0.38).
narrative_ontology:measurement_basis(corr_be_t600, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(correct_latin__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin__continuity_reading, correct_latin__hybrid_reading).

% DUAL FORMULATION NOTE:
% correct_latin__continuity_reading is one of three sibling readings of the kernel correct_latin (continuity_reading, discontinuity_reading, hybrid_reading). Each reading is authored as a separate ε-invariant constraint per the ε-invariance principle: the natural-language label 'correct Latin' conflates three structurally distinct normative claims about what determines linguistic correctness across a documented period of systematic change. This file (continuity_reading) declares medieval Latin fully legitimate via unbroken transmission, with medievalist/diachronic scholarship as beneficiaries and classicizing humanist purists as payers. discontinuity_reading inverts this beneficiary/victim structure. hybrid_reading splits the difference with a flatter extraction profile. All three link to each other via affects_constraints as members of one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
