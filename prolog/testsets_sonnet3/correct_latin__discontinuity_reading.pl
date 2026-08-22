% ============================================================================
% CONSTRAINT STORY: correct_latin__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__discontinuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin__discontinuity_reading
 *   human_readable: Discontinuity Reading of Correct Latin: Classical Text as Sole Legitimate Norm
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   From the Italian humanists of the 14th-15th centuries onward, a scholarly
 *   tradition insisted that 'real' Latin was fixed in the texts of Cicero,
 *   Virgil, and their contemporaries, and that everything the language became
 *   during the medieval period — administrative, ecclesiastical, scholastic
 *   Latin used continuously for roughly a thousand years — was corruption to
 *   be philologically stripped away and reconstructed against classical
 *   exemplars. This reading treats the classical corpus as an external anchor
 *   reoccupied by scholarship rather than a living form that evolved:
 *   correctness is defined by fidelity to dead texts, not by continuity of
 *   practice.
 *
 * KEY AGENTS:
 *   - classical_philologists: agenda_setter, institutional power, administer the reconstruction apparatus
 *   - renaissance_humanist_tradition: beneficiary, institutional power, built pedagogical identity on the rupture claim
 *   - textual_critics: beneficiary/agenda_setter, professional livelihood depends on treating medieval forms as correctable error
 *   - medieval_latin_scribes_and_scholars: payer, powerless, retroactively demoted corpus with no voice
 *   - vernacular_educated_clergy: payer, moderate power, devalued functional competence
 *   - non_elite_latin_learners: payer, powerless, foreclosed advancement under classical-purity gatekeeping
 *   - modern_diachronic_linguists: observer, analytical power, evaluates the discontinuity claim against comparative evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__discontinuity_reading, 0.62).
domain_priors:suppression_score(correct_latin__discontinuity_reading, 0.58).
domain_priors:theater_ratio(correct_latin__discontinuity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(correct_latin__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__discontinuity_reading, "Discontinuity Reading of Correct Latin: Classical Text as Sole Legitimate Norm").
narrative_ontology:topic_domain(correct_latin__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__discontinuity_reading, 'ccb258b2-5d4f-4595-8818-6779ea0ef81f').
narrative_ontology:cs_kernel_codification('ccb258b2-5d4f-4595-8818-6779ea0ef81f', fixed_text).
narrative_ontology:cs_authority_grounding('ccb258b2-5d4f-4595-8818-6779ea0ef81f', lineage).
narrative_ontology:cs_interpretation_layer_present('ccb258b2-5d4f-4595-8818-6779ea0ef81f').
narrative_ontology:cs_reading_relation('ccb258b2-5d4f-4595-8818-6779ea0ef81f', correct_latin__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('ccb258b2-5d4f-4595-8818-6779ea0ef81f', correct_latin__hybrid_reading, influences).
narrative_ontology:cs_axiom('ccb258b2-5d4f-4595-8818-6779ea0ef81f', foundational, classical_text_is_sole_legitimate_anchor).
narrative_ontology:cs_axiom_status(classical_text_is_sole_legitimate_anchor, holdable).
narrative_ontology:cs_axiom_grounding('ccb258b2-5d4f-4595-8818-6779ea0ef81f', classical_text_is_sole_legitimate_anchor, conventional).
narrative_ontology:cs_axiom('ccb258b2-5d4f-4595-8818-6779ea0ef81f', foundational, medieval_usage_constitutes_corruption_not_evolution).
narrative_ontology:cs_axiom_status(medieval_usage_constitutes_corruption_not_evolution, holdable).
narrative_ontology:cs_axiom_grounding('ccb258b2-5d4f-4595-8818-6779ea0ef81f', medieval_usage_constitutes_corruption_not_evolution, empirically_contingent).
narrative_ontology:cs_reference_frame('ccb258b2-5d4f-4595-8818-6779ea0ef81f', ciceronian_augustan_textual_corpus).
narrative_ontology:cs_drift_state('ccb258b2-5d4f-4595-8818-6779ea0ef81f', post_comparative_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ccb258b2-5d4f-4595-8818-6779ea0ef81f', '').
narrative_ontology:cs_kernel_id(correct_latin__discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, renaissance_humanist_tradition).
narrative_ontology:constraint_beneficiary(correct_latin__discontinuity_reading, textual_critics).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, medieval_latin_scribes_and_scholars).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, vernacular_educated_clergy).
narrative_ontology:constraint_victim(correct_latin__discontinuity_reading, non_elite_latin_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and police the norm that Ciceronian/Augustan usage recorded in surviving manuscripts is the only legitimate Latin, treating medieval forms as errors to be corrected against reconstructed classical exemplars. They administer the philological apparatus (critical editions, grammars, pedagogy) that enforces this standard and derive professional authority, publication prestige, and pedagogical control from being the arbiters of correctness.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, classical_philologists, beneficiary).

% Built its entire identity and educational program on the claim that medieval Latin was a degeneration to be swept aside and Classical purity restored. Benefits from the discontinuity frame because it legitimizes humanist pedagogy, curricula, and the humanist scholars' self-positioning as recoverers of lost civilization against a maligned 'Gothic' interval.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, renaissance_humanist_tradition, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Practice philological reconstruction as their core professional activity: emending manuscripts against a hypothesized classical original. Their livelihood and disciplinary status depend on medieval forms being treated as corruptions requiring correction rather than as valid independent evolution; the discontinuity frame supplies the corruption they exist to fix.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, textual_critics, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__discontinuity_reading, textual_critics, agenda_setter).

% Wrote and thought in a living, functioning Latin that served administration, liturgy, philosophy, and law for a thousand years across most of Europe. Under this reading, their entire corpus is retroactively demoted to error-riddled deviation; they have no voice in a debate conducted centuries after their deaths by scholars who never had to justify their usage to them.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_latin_scribes_and_scholars, payer,
    powerless, generational, trapped, continental).

% Learned functional ecclesiastical and administrative Latin through continuous institutional transmission rather than through classical texts. When the discontinuity standard is imposed on education and publication, their Latin competence is devalued relative to humanist-trained scholars, narrowing their access to prestigious scholarly and clerical advancement despite genuine fluency in a working register.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, vernacular_educated_clergy, payer,
    moderate, biographical, constrained, regional).

% Students and lower clergy who acquire Latin through available regional teaching traditions rather than elite humanist academies. Measured against the reconstructed classical standard, their usage is branded barbarous regardless of communicative competence, foreclosing advancement paths that depend on being certified as writing 'correct' Latin.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, non_elite_latin_learners, payer,
    powerless, biographical, trapped, national).

% The vast body of charters, chronicles, scholastic treatises, and liturgical texts produced across the medieval period. Under the discontinuity reading, this record is treated as a corpus of errors to be filtered out rather than as primary linguistic evidence, silencing what the actual historical continuity of the language would show if given equal evidentiary standing.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, medieval_latin_documentary_record, excluded,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(correct_latin__discontinuity_reading, medieval_latin_documentary_record).

% Study Latin's actual historical trajectory using comparative and corpus methods without a prior commitment to which stage is 'correct.' They can evaluate whether medieval forms show regular sound change and grammaticalization consistent with organic evolution, which bears directly on whether the discontinuity claim is linguistically defensible or an artifact of humanist ideology.
narrative_ontology:constraint_stakeholder(correct_latin__discontinuity_reading, modern_diachronic_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__discontinuity_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, texually anchored reference standard against which Latin usage across a fragmented, multi-century, multi-region tradition can be evaluated and taught, avoiding an otherwise unmanageable proliferation of regional and period norms.
% TRANSFER_FUNCTION: Moves scholarly authority, pedagogical legitimacy, and access to advancement from those trained in living medieval/ecclesiastical Latin traditions to those trained in reconstructed classical philology; moves interpretive authority over 'what Latin is' from continuous practice communities to textual specialists who mediate access to the classical corpus.
% ABSENT_VOICES: Medieval scribes, scholastic philosophers, and vernacular clergy who actually used and developed the language for a millennium have no seat in a standard set retrospectively by humanist and post-humanist philology; the documentary record they left behind is treated as noise to be corrected rather than as evidence with its own claim to legitimacy.
% DISAPPEARANCE_RATIONALE: If the discontinuity standard vanished, medieval Latin texts and their authors would be restored to full linguistic legitimacy, philological curricula built around 'purifying' medieval usage would lose their organizing rationale, and the humanist self-narrative of civilizational recovery from a corrupt interval would require substantial revision — the disciplinary hierarchy that currently privileges classical reconstruction over medieval attestation would flatten considerably.
% FOUNDING_PROBLEM: Renaissance humanists confronted a Latin literary and administrative culture saturated with medieval scholastic vocabulary, syntax, and orthography that diverged noticeably from the surviving classical corpus, and sought a stable, prestige-bearing standard for reviving classical learning and rhetoric.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Renaissance and modern historical linguists attest that the humanist rupture narrative was itself a polemical and pedagogical project rather than a linguistically neutral discovery — regular sound-change and grammaticalization patterns documented in medieval corpora by diachronic linguists outside the classicist tradition support continuity; corroboration from outside the beneficiary set (comparative philologists, medievalists) largely rejects the corruption framing even as classicist institutions continue to teach it as settled.
narrative_ontology:disappearance_verdict(correct_latin__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__discontinuity_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the discontinuity standard genuinely coordinates a stable pedagogical and scholarly reference point (a real function) while also transferring prestige, access, and legitimacy away from medieval-trained practitioners toward classicist gatekeepers — a tangled rope, not a pure rope. Suppression (0.58) is somewhat higher near the standard's founding (0.65 at t0, during the humanist polemical campaign against 'Gothic' Latin) and eases modestly as the standard becomes institutionalized and less actively contested (0.58 at t600), since entrenched norms require less active suppression once naturalized. Theater ratio rises over the interval (0.25 to 0.44) as the discipline's actual function — training people to read and use Latin — is increasingly overshadowed by performative philological purism (elaborate emendation apparatus, prestige competitions over classical fidelity) with diminishing practical stakes as fewer people write original Latin at all. Accessibility collapse is high (0.7): once the classical-purity framing is internalized within classicist institutions, alternative standards become nearly unthinkable within that tradition. Resistance (0.55) reflects ongoing pushback from medievalist and diachronic-linguistics communities.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists and the humanist tradition sit near the beneficiary end: they set the standard, administer the apparatus that enforces it, and derive institutional and professional standing from it (low d). Textual critics benefit similarly since their professional practice depends on treating medieval Latin as correctable error to be fixed. Medieval scribes and scholars, vernacular clergy, and non-elite learners sit near the target end: they bear the cost of retroactive delegitimization, devaluation of functional competence, and foreclosed advancement, with trapped or constrained exit since they cannot renegotiate a standard applied centuries after their deaths or against institutions they cannot bypass.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — humanists needing a stable reference standard amid a proliferating, regionally divergent Latin tradition — was arguably live in the 14th-15th centuries but is contested today: modern historical linguistics treats medieval Latin as a well-documented stage of organic language change, not a corruption requiring correction. The corpus of texts humanists needed to organize their own revival project persists as an institutionalized purity standard long after the original scholarly problem (making sense of a fragmented manuscript tradition) has been superseded by better comparative methods. This is exactly the case the discontinuity/continuity split exists to separate: without decomposition, a single 'correct Latin' constraint would either overstate the coordination value (ignoring the extraction from medieval practitioners) or overstate the extraction (ignoring the real reference-standard function humanism provided). Authoring this reading alone, with its own ε, keeps that separation clean.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discontinuity_reading_identity,
    'This constraint is one reading (discontinuity_reading) of the correct_latin kernel. The continuity_reading holds that medieval Latin is legitimate evolved Classical Latin transmitted through unbroken practice; the hybrid_reading holds partial continuity with targeted textual reform. Which reading, if any, best reflects the linguistic facts about Latin''s actual historical development?',
    'Comparative and corpus-linguistic analysis of medieval Latin texts for regular sound-change, morphological, and syntactic patterns consistent with organic evolution from Classical Latin, cross-checked against the historical record of how and why the classical-purity standard was constructed by humanist scholars.',
    'If medieval Latin shows regular, rule-governed evolution (supporting continuity_reading), the discontinuity_reading''s corruption framing is exposed as a polemical construction rather than a linguistic finding, which would substantially lower its defensible ε and reclassify it closer to a snare (pure status extraction dressed as philology) rather than tangled_rope. If medieval Latin instead shows genuine systemic breakdown of classical grammar without regular compensating structure, the discontinuity_reading''s coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_reading_identity, conceptual, 'Which kernel reading (discontinuity vs continuity vs hybrid) the linguistic evidence actually supports, and where disagreement is located structurally.').

omega_variable(
    sibling_reading_delta_location,
    'Where exactly is the structural disagreement located between this reading and its siblings — is it in the evaluation of the manuscript evidence, in the definition of ''legitimate usage,'' or in the institutional interests served by each framing?',
    'Trace the historiography of the humanist rupture claim against the counter-historiography developed by 19th-20th century medievalists (e.g. the ''Medieval Latin as living language'' scholarship) to identify whether the dispute is empirical (what happened to the language) or normative (what should count as correct).',
    'If the disagreement is primarily normative/institutional rather than empirical, the discontinuity_reading''s claimed_type should be read as more purely extractive (status/legitimacy transfer) since the coordination function (having *a* standard) does not require this *specific* standard''s rupture claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_location, conceptual, 'Locating whether the reading dispute is empirical or normative/institutional.').

omega_variable(
    reconstruction_fidelity_uncertainty,
    'How faithfully does philological ''reconstruction'' of Classical Latin actually recover the ancient spoken/written language, versus constructing a scholarly artifact that never existed as a single uniform register?',
    'Comparison of reconstructed classical norms against the internal variation documented within surviving classical-era texts themselves (regional, register, and period variation within antiquity).',
    'If classical Latin itself was highly variable and the ''pure'' reconstructed standard is itself a scholarly idealization, the discontinuity_reading''s foundational claim to represent a single authentic ancient form weakens further, supporting a higher extraction assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_fidelity_uncertainty, empirical, 'Whether the reconstructed classical standard is itself an artifact rather than a recovered historical reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__discontinuity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__discontinuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(corr_tr_t100, correct_latin__discontinuity_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(corr_tr_t200, correct_latin__discontinuity_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement(corr_tr_t300, correct_latin__discontinuity_reading, theater_ratio, 300, 0.38).
narrative_ontology:measurement(corr_tr_t400, correct_latin__discontinuity_reading, theater_ratio, 400, 0.41).
narrative_ontology:measurement(corr_tr_t500, correct_latin__discontinuity_reading, theater_ratio, 500, 0.43).
narrative_ontology:measurement(corr_tr_t600, correct_latin__discontinuity_reading, theater_ratio, 600, 0.44).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__discontinuity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(corr_be_t100, correct_latin__discontinuity_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(corr_be_t200, correct_latin__discontinuity_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(corr_be_t300, correct_latin__discontinuity_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(corr_be_t400, correct_latin__discontinuity_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement(corr_be_t500, correct_latin__discontinuity_reading, base_extractiveness, 500, 0.61).
narrative_ontology:measurement(corr_be_t600, correct_latin__discontinuity_reading, base_extractiveness, 600, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__discontinuity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(corr_su_t100, correct_latin__discontinuity_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement(corr_su_t200, correct_latin__discontinuity_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement(corr_su_t300, correct_latin__discontinuity_reading, suppression_requirement, 300, 0.6).
narrative_ontology:measurement(corr_su_t400, correct_latin__discontinuity_reading, suppression_requirement, 400, 0.59).
narrative_ontology:measurement(corr_su_t500, correct_latin__discontinuity_reading, suppression_requirement, 500, 0.58).
narrative_ontology:measurement(corr_su_t600, correct_latin__discontinuity_reading, suppression_requirement, 600, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__discontinuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, correct_latin__hybrid_reading).
narrative_ontology:affects_constraint(correct_latin__discontinuity_reading, renaissance_humanist_pedagogy_canon).

% DUAL FORMULATION NOTE:
% correct_latin__discontinuity_reading is one of three sibling constraints instantiating the correct_latin kernel (continuity_reading, discontinuity_reading, hybrid_reading). Each reading names a different legitimate-usage set and a different victim/beneficiary structure over the same underlying historical Latin corpus; they are linked here rather than merged because their ε values and classifications diverge (per the ε-invariance principle) despite sharing a label in ordinary discourse ('correct Latin').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
