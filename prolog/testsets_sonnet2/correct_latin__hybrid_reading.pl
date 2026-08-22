% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Hybrid Classical/Medieval Latin Correction Standard
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid reading of the correct_latin
 *   kernel: correct Latin is the classical form as transmitted through
 *   medieval practice, but that transmission is understood as containing
 *   accumulated errors (orthographic drift, vocabulary substitution)
 *   correctable against textual evidence, while the underlying grammatical
 *   structure inherited through medieval use is treated as legitimately
 *   continuous with Classical Latin. This is neither the continuity reading
 *   (which would accept medieval usage wholesale as evolved Classical Latin)
 *   nor the discontinuity reading (which would treat medieval Latin as
 *   corrupt deviation requiring full reconstruction from ancient texts). The
 *   hybrid reading authorizes targeted, textually-evidenced correction of
 *   specific features while preserving the grammatical core as inherited — a
 *   D0 (partial continuity) position with textual guidance rather than full
 *   reoccupation. Its beneficiaries are those positioned to perform and
 *   administer the correction (humanist scholars, reform educators,
 *   manuscript editors); its victims are those whose Latin practice is
 *   grammatically sound but lexically/orthographically 'provincial' by the
 *   corrected standard (vernacular-influenced clerics, provincial teachers,
 *   monastic scribal communities) — their fluency is accepted while their
 *   specific forms are targeted, which is precisely what makes this reading
 *   extractive in a way the pure continuity reading is not.
 *
 * KEY AGENTS:
 *   - humanist_textual_scholars: primary agenda-setter (institutional/arbitrage) — administers the correction standard
 *   - reform_minded_educators: beneficiary (organized/mobile) — gains prestige from compliance
 *   - manuscript_editors: beneficiary/agenda-setter (moderate/constrained) — produces the corrected texts that enforce the standard
 *   - vernacular_influenced_clerics: primary target (powerless/trapped) — grammatically legitimate but lexically corrected
 *   - provincial_latin_teachers: primary target (powerless/trapped) — cannot access the correcting apparatus
 *   - monastic_scribal_traditions: target/excluded (moderate/constrained) — continuity function honored rhetorically, authority diminished practically
 *   - vernacular_language_advocates: excluded (organized/mobile) — outside the frame of the Latin-correctness contest entirely
 *   - later_philologists: analytical observer — reconstructs the negotiation and its winners/losers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.42).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.38).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Hybrid Classical/Medieval Latin Correction Standard").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, 'a87d68ee-6224-4a23-b3d2-d89528e19cb9').
narrative_ontology:cs_kernel_codification('a87d68ee-6224-4a23-b3d2-d89528e19cb9', distributed).
narrative_ontology:cs_authority_grounding('a87d68ee-6224-4a23-b3d2-d89528e19cb9', expertise).
narrative_ontology:cs_interpretation_layer_present('a87d68ee-6224-4a23-b3d2-d89528e19cb9').
narrative_ontology:cs_reading_relation('a87d68ee-6224-4a23-b3d2-d89528e19cb9', correct_latin__continuity_reading, influences).
narrative_ontology:cs_reading_relation('a87d68ee-6224-4a23-b3d2-d89528e19cb9', correct_latin__discontinuity_reading, influences).
narrative_ontology:cs_axiom('a87d68ee-6224-4a23-b3d2-d89528e19cb9', foundational, grammatical_transmission_legitimate_lexical_evidence_correctable).
narrative_ontology:cs_axiom_status(grammatical_transmission_legitimate_lexical_evidence_correctable, holdable).
narrative_ontology:cs_axiom_grounding('a87d68ee-6224-4a23-b3d2-d89528e19cb9', grammatical_transmission_legitimate_lexical_evidence_correctable, conventional).
narrative_ontology:cs_axiom('a87d68ee-6224-4a23-b3d2-d89528e19cb9', secondary, correction_is_targeted_not_wholesale_reoccupation).
narrative_ontology:cs_axiom_status(correction_is_targeted_not_wholesale_reoccupation, holdable).
narrative_ontology:cs_axiom_grounding('a87d68ee-6224-4a23-b3d2-d89528e19cb9', correction_is_targeted_not_wholesale_reoccupation, instrumental).
narrative_ontology:cs_reference_frame('a87d68ee-6224-4a23-b3d2-d89528e19cb9', classical_grammar_with_medieval_transmission_layer).
narrative_ontology:cs_drift_state('a87d68ee-6224-4a23-b3d2-d89528e19cb9', high_renaissance_humanist_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a87d68ee-6224-4a23-b3d2-d89528e19cb9', '').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_textual_scholars).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, reform_minded_educators).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, manuscript_editors).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, vernacular_influenced_clerics).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, provincial_latin_teachers).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, monastic_scribal_traditions).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, textual_evidence_as_arbiter_doctrine).
narrative_ontology:constraint_vindicates(correct_latin__hybrid_reading, corrective_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collate manuscripts against recovered classical exemplars and issue corrected forms — restoring classical orthography and vocabulary while accepting the medieval grammatical core as a legitimate transmission layer. They set the standard of what counts as an error worth correcting versus an acceptable evolved form, and their editorial authority is the mechanism through which the hybrid standard is enforced in schools and chanceries.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_textual_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, humanist_textual_scholars, beneficiary).

% Adopt the corrected curriculum as a mark of prestige and rigor, gaining patronage and appointments by teaching the hybrid standard over inherited local Latin. Their exit option is real — they can move between institutions that adopt the reform faster or slower — but their livelihood increasingly depends on visible compliance with the correction program.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, reform_minded_educators, beneficiary,
    organized, biographical, mobile, regional).

% Produce the corrected editions that circulate as reference texts, earning reputation and commissions from patrons who want classicized documents. They depend on humanist scholars' judgments of correctness for their own authority, so their exit from the correction framework would mean losing the basis of their trade.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, manuscript_editors, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, manuscript_editors, agenda_setter).

% Write and speak the Latin they were trained in, shaped by centuries of regional pronunciation and usage. Under the hybrid standard their vocabulary and spelling are flagged as errors to be corrected against classical texts, even though their grammatical competence is accepted as continuous. They cannot simply relearn a different Latin without institutional retraining they mostly cannot access; their documents are increasingly read as marked, provincial, or embarrassing.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, vernacular_influenced_clerics, payer,
    powerless, biographical, trapped, local).

% Teach the Latin transmitted to them by their own teachers, without access to the manuscript collations and classical exemplars that circulate among urban humanist centers. Their students are now measured against a corrected standard they were never taught and often cannot afford the texts or training to teach. Exit would mean abandoning their profession or migrating to centers they have no means to reach.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, provincial_latin_teachers, payer,
    powerless, biographical, trapped, local).

% Maintain centuries of copying practice that preserved Latin literacy through periods when classical texts were scarce or lost, and whose own orthographic conventions are now treated as accumulated error requiring correction. Their continuity function — keeping Latin alive at all — is acknowledged in principle but their specific practices are targeted for correction, so their historical contribution is honored rhetorically while their present authority over the language is diminished.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, monastic_scribal_traditions, payer,
    moderate, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, monastic_scribal_traditions, excluded).

% Argue that living vernaculars, not any form of Latin, should carry administrative and literary weight, and are entirely outside the hybrid standard's frame of reference — the correction debate treats Latin's legitimacy as settled and argues only about which Latin, leaving the vernacular case unheard within this arrangement.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, vernacular_language_advocates, excluded,
    organized, generational, mobile, regional).

% Study the correction program's manuscripts, correspondence, and pedagogical output centuries later to reconstruct how the hybrid standard was negotiated, which corrections stuck, and whose Latin was suppressed or dignified by the process.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, later_philologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, humanist_textual_scholars).
narrative_ontology:fixing_cost_class(correct_latin__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single teachable, writable, textually-anchored Latin standard so that documents, scholarship, and instruction across regions remain mutually legible despite centuries of regional drift — solving the real problem that medieval Latin had diverged enough in vocabulary and orthography to impede cross-regional communication among the learned.
% TRANSFER_FUNCTION: Moves prestige, patronage, and institutional legitimacy toward those who can demonstrate command of the textually-corrected standard (humanist scholars, reform-aligned educators, manuscript editors) and away from those whose Latin competence was built entirely on regional and monastic transmission (vernacular-influenced clerics, provincial teachers, scribal communities), even though the latter's underlying grammatical fluency is accepted as legitimate.
% ABSENT_VOICES: Vernacular-language advocates are entirely outside this arrangement's frame — the correction debate presupposes Latin's continued authority and argues only about which version of Latin is correct. Provincial teachers and monastic scribes are nominally inside the conversation (their grammatical continuity is honored) but have no access to the manuscript apparatus that would let them contest specific corrections; their absence is practical, not principled.
% DISAPPEARANCE_RATIONALE: If the hybrid correction standard vanished overnight, regional Latin practices would re-diversify without a common textual anchor, humanist scholars would lose their gatekeeping function over what counts as correct usage, manuscript editors would lose their market for corrected editions, and provincial teachers and monastic scribes would regain unchallenged authority over their own inherited forms — the prestige economy built around textual correction would collapse along with it.
% FOUNDING_PROBLEM: Centuries of regional divergence in medieval Latin had produced mutually distant orthographic and lexical practices; meanwhile newly recovered or better-collated classical manuscripts revealed that much medieval usage diverged from ancient exemplars in ways that impeded both cross-regional communication and access to classical literary and legal heritage.
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars and reform educators attest the founding problem is fully live — that without ongoing correction, communicative and scholarly access to the classical tradition continues to erode. Provincial teachers and monastic communities, corroborated by later philologists studying comparative intelligibility across regional medieval Latin corpora, attest that mutual intelligibility among educated readers was largely intact before the reform and that the 'problem' was substantially a prestige contest dressed as a communication crisis — this is testimony from outside the beneficiary set, not merely self-report from those it burdens.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).
:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate, not high — because the hybrid reading's coordination function is real: a shared standard that resolves genuine cross-regional intelligibility problems has value, and the grammatical-core concession means the reading does not simply discard all medieval practice as the discontinuity reading would. But it is not negligible, because the correction apparatus concentrates prestige and gatekeeping authority in those with access to manuscript collation, while the costs of being 'corrected' land on populations with no practical path to compliance. Suppression is moderate (0.38) and rising over the interval as institutional adoption of corrected curricula hardens from suggestion into requirement for advancement. Theater ratio is moderate (0.3) and rising: a growing share of corrective activity is about demonstrating classical literacy for prestige rather than resolving actual communication breakdowns, though the coordination core has not fully atrophied.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist scholar and manuscript editor seats, this looks like principled restoration guided by evidence — a genuine improvement over unguided drift. From the provincial teacher and monastic scribe seats, the identical arrangement operates as a moving target: their grammatical competence is praised in the abstract while their concrete usage is continuously reclassified as error, with no realistic path to the resources that would let them meet the corrected standard on its own terms. The engine should compute these as structurally different experiences of the same constraint, not reconcile them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (humanist scholars, reform educators, manuscript editors) sit near the low-d end: they administer or profit from the correction apparatus and have mobile or arbitrage-grade exit — they can relocate to wherever the correction program is most institutionally rewarded. Victims (vernacular-influenced clerics, provincial teachers, monastic scribes) sit near the high-d end: trapped or constrained exit, no access to the manuscript apparatus that would let them contest or comply with corrections on favorable terms, and their local prestige is directly undercut by the correction standard's spread. Vernacular advocates are excluded rather than positioned on the beneficiary/victim axis at all — the correction contest doesn't touch their claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists mislabeling in both directions. Labeling it pure coordination (rope) would erase the real asymmetry — provincial and monastic Latin users bear correction costs they cannot offset, while humanist scholars capture the prestige of setting the standard. Labeling it pure extraction (snare) would erase the real coordination gain — cross-regional intelligibility and access to classical literary and legal heritage are genuine goods the correction program advances, and the grammatical-core concession is a real, non-trivial continuity claim, not mere cover. Tangled rope captures the structure precisely: a genuine coordination function (shared correctable standard) coexisting with asymmetric extraction (concentrated prestige capture at the expense of trapped provincial and monastic Latin users), sustained by active enforcement (curricular requirements, editorial gatekeeping) rather than by voluntary convergence alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grammatical_core_boundary_ambiguity,
    'Where exactly does the accepted ''grammatical core'' end and the correctable ''surface features'' (orthography, vocabulary) begin? The hybrid reading depends on this boundary being stable, but it is contested by the correcting scholars themselves case by case.',
    'Systematic comparison of which specific medieval features were retained versus corrected across multiple humanist correction programs (Italian, French, English) to see whether a consistent grammar/lexicon boundary was actually applied or whether the boundary shifted opportunistically to favor whichever correction increased scholarly prestige.',
    'If the boundary is applied consistently, the hybrid reading''s coordination claim is stronger (a principled, evidence-guided line). If the boundary shifts opportunistically toward whatever maximizes scholarly gatekeeping power, the extraction component is understated by the current ε and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grammatical_core_boundary_ambiguity, empirical, 'Whether the grammar/lexicon correction boundary is principled or opportunistic.').

omega_variable(
    textual_evidence_availability_asymmetry,
    'Is the correction program''s dependence on textual evidence itself an extraction mechanism, given that manuscript access was radically unequal across regions and institutions?',
    'Map manuscript library holdings and humanist center locations against the geographic distribution of provincial teachers and monastic communities flagged for correction, to assess whether ''correctability via textual evidence'' tracked actual textual access or was a formally neutral standard applied to an informally unequal evidentiary landscape.',
    'If evidentiary access closely tracked institutional power, the hybrid reading''s claim to be evidence-guided (rather than power-guided) correction is undermined, raising the effective extractiveness beyond the authored 0.42.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_evidence_availability_asymmetry, empirical, 'Whether textual-evidence correction tracked actual manuscript access or formal neutrality only.').

omega_variable(
    which_reading_is_the_true_kernel_state,
    'Among continuity_reading, discontinuity_reading, and this hybrid_reading, is one of them the historically accurate description of how medieval Latin actually related to Classical Latin, or are all three genuinely underdetermined by the evidence, making the choice among readings itself a normative/political act rather than a discovery?',
    'This is the committer-level question the kernel decomposition exists to hold open; it is not resolvable within any single reading''s own framework, since each reading''s evidentiary standards presuppose its own answer.',
    'If the hybrid reading is not privileged by the evidence over its siblings, then its selection by humanist correction programs was itself a contest for authority dressed as philological discovery — reinforcing rather than undermining the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_true_kernel_state, conceptual, 'Whether the hybrid reading is evidentially privileged among kernel readings or itself a contested political choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(corr_tr_t40, correct_latin__hybrid_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(corr_tr_t80, correct_latin__hybrid_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(corr_tr_t120, correct_latin__hybrid_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement(corr_tr_t160, correct_latin__hybrid_reading, theater_ratio, 160, 0.28).
narrative_ontology:measurement(corr_tr_t200, correct_latin__hybrid_reading, theater_ratio, 200, 0.3).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(corr_be_t40, correct_latin__hybrid_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(corr_be_t80, correct_latin__hybrid_reading, base_extractiveness, 80, 0.34).
narrative_ontology:measurement(corr_be_t120, correct_latin__hybrid_reading, base_extractiveness, 120, 0.38).
narrative_ontology:measurement(corr_be_t160, correct_latin__hybrid_reading, base_extractiveness, 160, 0.4).
narrative_ontology:measurement(corr_be_t200, correct_latin__hybrid_reading, base_extractiveness, 200, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin__hybrid_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(corr_su_t40, correct_latin__hybrid_reading, suppression_requirement, 40, 0.26).
narrative_ontology:measurement(corr_su_t80, correct_latin__hybrid_reading, suppression_requirement, 80, 0.31).
narrative_ontology:measurement(corr_su_t120, correct_latin__hybrid_reading, suppression_requirement, 120, 0.34).
narrative_ontology:measurement(corr_su_t160, correct_latin__hybrid_reading, suppression_requirement, 160, 0.37).
narrative_ontology:measurement(corr_su_t200, correct_latin__hybrid_reading, suppression_requirement, 200, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the correct_latin kernel. continuity_reading authors low ε for a coordination-heavy arrangement where medieval usage is fully legitimate; discontinuity_reading authors high ε for a heavily correctional arrangement that treats medieval usage as corrupt; this hybrid_reading sits structurally between them with moderate ε (0.42), reflecting partial grammatical continuity plus targeted lexical/orthographic correction. Each reading is ε-invariant on its own terms; the three are not averaged or reconciled but linked via affects_constraints to preserve the kernel-contest structure at the network level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
