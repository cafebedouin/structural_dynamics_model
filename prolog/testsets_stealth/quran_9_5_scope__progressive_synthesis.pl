% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Verse 9:5 Time-Bound Reading Settlement (Progressive Synthesis)
 *   domain: religious/jurisprudential/political-theological
 *
 * SUMMARY:
 *   Verse 9:5 commands slaying polytheists where found after the sacred
 *   months, in the context of Medinan treaty disputes. The kernel contest is
 *   over the verse's normative scope: eternal universal command
 *   (abrogating_universal), context-bound defensive provision
 *   (contextual_defensive), or time-bound political directive superseded by
 *   the Quran's ethical trajectory (progressive_synthesis — this story). This
 *   file instantiates ONLY the progressive reading as a clean,
 *   epsilon-invariant constraint: the operative settlement that the verse's
 *   directive is historically expired and cannot be deployed as a standing
 *   legal command. Under this settlement the verse exits active constraint
 *   space — neither polytheists nor Muslims are bound by its directive — and
 *   the live arrangement is the hermeneutical settlement itself, which
 *   coordinates the community's reading while stripping textualist authority
 *   structures of their strongest legal instrument. Constraint family: the
 *   colloquial label 'the legal status of the Sword Verse' decomposes into
 *   three structurally distinct constraints with different epsilon values,
 *   different beneficiary/victim sets, and different types — the
 *   abrogating_universal reading's constraint binds polytheists as targets,
 *   while this settlement's arrangement binds interpreters. The epsilon
 *   values differ because the readings instantiate different constraints, not
 *   because one constraint is measured differently; linkage is via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - modernist_reform_scholars: agenda-setter and principal beneficiary (organized/identity_locked) — authors and maintains the settlement, collects the transferred interpretive authority
 *   - textualist_jurisprudential_authorities: primary payer (institutional/identity_locked) — bears the loss of the verse as a legal instrument
 *   - abrogationist_militant_movements: payer (organized/identity_locked) — bears delegitimation of their central proof-text
 *   - muslim_minority_communities: beneficiary (organized/identity_locked) — civic belonging secured by the settlement
 *   - secular_pluralist_polities: beneficiary (institutional/constrained) — assurance against the universal-command reading
 *   - non_muslim_religious_minorities: beneficiary (powerless/trapped) — the class the verse's directive classically addressed
 *   - traditionalist_rank_and_file: excluded (powerless/identity_locked) — objects but holds no seat in the fora where the settlement is authored
 *   - academic_quran_studies: analytical observer (analytical/analytical) — sees the full structure, holds no seat in maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.22).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.45).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.22).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, rope).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Verse 9:5 Time-Bound Reading Settlement (Progressive Synthesis)").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "religious/jurisprudential/political-theological").

domain_priors:requires_active_enforcement(quran_9_5_scope__progressive_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, 'f6440ab3-1754-4039-802e-01c50f521ad8').
narrative_ontology:cs_kernel_codification('f6440ab3-1754-4039-802e-01c50f521ad8', fixed_text).
narrative_ontology:cs_authority_grounding('f6440ab3-1754-4039-802e-01c50f521ad8', expertise).
narrative_ontology:cs_interpretation_layer_present('f6440ab3-1754-4039-802e-01c50f521ad8').
narrative_ontology:cs_reading_relation('f6440ab3-1754-4039-802e-01c50f521ad8', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('f6440ab3-1754-4039-802e-01c50f521ad8', quran_9_5_scope__contextual_defensive, influences).
narrative_ontology:cs_axiom('f6440ab3-1754-4039-802e-01c50f521ad8', foundational, verse_9_5_directive_time_bound).
narrative_ontology:cs_axiom_status(verse_9_5_directive_time_bound, holdable).
narrative_ontology:cs_axiom_grounding('f6440ab3-1754-4039-802e-01c50f521ad8', verse_9_5_directive_time_bound, empirically_contingent).
narrative_ontology:cs_axiom('f6440ab3-1754-4039-802e-01c50f521ad8', foundational, ethical_trajectory_supersedes_literalism).
narrative_ontology:cs_axiom_status(ethical_trajectory_supersedes_literalism, holdable).
narrative_ontology:cs_axiom_grounding('f6440ab3-1754-4039-802e-01c50f521ad8', ethical_trajectory_supersedes_literalism, deontological).
narrative_ontology:cs_reference_frame('f6440ab3-1754-4039-802e-01c50f521ad8', ethical_trajectory_supremacy_framework).
narrative_ontology:cs_drift_state('f6440ab3-1754-4039-802e-01c50f521ad8', contemporary_post_9_11_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f6440ab3-1754-4039-802e-01c50f521ad8', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_polities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, muslim_minority_communities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, non_muslim_religious_minorities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, modernist_reform_scholars).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_jurisprudential_authorities).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, abrogationist_militant_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University-based scholars of Islam, reformist seminary faculties, and progressive juristic councils. They author and maintain the settlement: curricula, commentaries, and opinions that read verse 9:5 as a time-bound seventh-century political directive superseded by the Quran's ethical arc. They collect institutional standing, publication fields, and state and interfaith advisory roles as the settlement prevails, and they bear the continuous maintenance burden — answering literalist counter-argument, training the next cohort, refreshing the historical case. Their professional and confessional identity is fused with the hermeneutic; abandoning it would collapse career and self-concept together.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, modernist_reform_scholars, agenda_setter,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__progressive_synthesis, modernist_reform_scholars, beneficiary).

% Traditional fiqh councils, classical-commentary institutions, and jurists formed in the transmitted method, whose authority rests on the verse's continuing force as a legal command. Where the settlement prevails, their strongest instrument for the law of treaty-breakers and the abrogation doctrine is ruled out of order, and their students are recruited into rival institutions. Adopting the settlement would dissolve the method that constitutes their standing; they therefore contest it as innovation or capitulation, and their institutions shrink or harden as the settlement spreads.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, textualist_jurisprudential_authorities, payer,
    institutional, generational, identity_locked, global).

% Militant movements whose central legal proof-text is the verse read as an abrogating universal command. They sit outside the settlement's institutions and reject its authority, but its spread in mainstream scholarship and state doctrine strips their proof-text of scholarly legitimacy, raising recruitment and legitimation costs and exposing them to heresy charges from within the tradition. Their identity is fused with the proof-text; the settlement's prevalence is an existential rather than marginal cost to them.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, abrogationist_militant_movements, payer,
    organized, biographical, identity_locked, regional).

% Muslims living as minorities in secular pluralist states. The universal-command reading renders them suspect citizens; the settlement dissolves that suspicion by ruling the directive out as a standing command, securing their civic belonging and their institutions' acceptance. Their relationship to the Quran is constitutive, so exit from the interpretive community is not a live option; they bear intra-community costs when textualist co-religionists read their settlement-adherence as inauthenticity.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, muslim_minority_communities, beneficiary,
    organized, generational, identity_locked, global).

% Non-Muslim-majority states and their legal orders. The settlement removes the standing scriptural claim that their territories are legitimate targets of an expansionist directive, converting a theological threat into a manageable interpretive dispute. They are not governed by the hermeneutical rule, but their counter-extremism doctrine, interfaith frameworks, and integration policy all presuppose it; reverting to threat assessment against every reading of the verse is the only exit, and it is not a live one.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, secular_pluralist_polities, beneficiary,
    institutional, generational, constrained, global).

% Hindu, Christian, and other non-Muslim communities in Muslim-majority states — the class the verse's directive classically addressed. Under the universal-command reading they live under a standing scriptural threat mobilizable in any communal crisis; the settlement removes that threat from live law and grounds their protection in the pluralist reading instead. Their protection tracks which reading their host state's institutions adopt, and emigration is a partial, costly exit at best.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, non_muslim_religious_minorities, beneficiary,
    powerless, generational, trapped, regional).

% Ordinary believers formed in traditionalist curricula who find the settlement unpersuasive but hold no scholarly standing. They are absent from the academic and juristic fora where the settlement is authored and defended; their objection — that it is elite revision detached from the transmitted method — registers only as diffuse resistance, never as a seat in the conversation. Their religious identity is not separable from the community whose settlement this is, so exit is not available to them either.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, traditionalist_rank_and_file, excluded,
    powerless, biographical, identity_locked, global).

% Historical-critical and comparative scholars of Quranic exegesis. They document the verse's occasion of revelation, the classical abrogation debates, and the modern settlement's genealogy from the nineteenth-century modernists through the late twentieth-century reformers to the present. They see the full structure — the settlement's real civic payoff, its real hermeneutical costs, its dependence on institutions — and hold no seat in its maintenance.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__progressive_synthesis, academic_quran_studies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__progressive_synthesis, modernist_reform_scholars).
narrative_ontology:fixing_cost_class(quran_9_5_scope__progressive_synthesis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single authoritative scope for a canonical military directive so that a textual community can hold scriptural allegiance and pluralist civic allegiance simultaneously; the settlement removes the directive from the set of live commands without removing the verse from the canon.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimation capacity from textualist jurisprudential institutions to the modernist reform academy; moves security assurance to pluralist polities and to Muslim and non-Muslim minority communities; the payment is the textualist seat's loss of its strongest legal instrument and the militant seat's loss of its central proof-text.
% ABSENT_VOICES: Traditionalist rank-and-file believers and classical-method jurists outside reformist institutions would object that the settlement is elite revision detached from the transmitted method; they are absent from the academic and policy fora where the settlement is authored and defended. Also absent: the verse's original addressees — the treaty-breaking tribes of its occasion — whose perspective survives only through the tradition's own campaign reports.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the abrogationist reading would face no institutional counterweight in mainstream discourse; minority-Muslim civic belonging would destabilize as the universal-command claim re-entered live argument; pluralist polities would revert from assurance to threat assessment; and textualist authorities would recover their instrument. Many arrangements — curricula, juristic practice, interfaith frameworks, state counter-extremism doctrine — depend on the settlement's persistence.
% FOUNDING_PROBLEM: The late-nineteenth-century crisis of scriptural legitimacy under colonial rule: European polemic cited verse 9:5 as proof of inherent Islamic expansionism, Muslim modernists needed a reading that reconciled canon with citizenship in emerging nation-states, and reformist scholars constructed the time-bound reading to secure both.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the colonial-era polemical record itself — missionary and orientalist literature citing 9:5, which the settlement's own authors cite as their occasion — and by the textualist opposition, which attests the same crisis from the opposite direction, citing the colonial context as evidence that the settlement was capitulation rather than exegesis. Hostile testimony converges on the problem's content and continued liveness.
narrative_ontology:disappearance_verdict(quran_9_5_scope__progressive_synthesis, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__progressive_synthesis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__progressive_synthesis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_9_5_scope__progressive_synthesis, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__progressive_synthesis, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).
:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22 at interval end): the settlement imposes hermeneutical discipline but transfers little material value; its principal cost — the textualist seat's loss of the verse as legal instrument — is borne by a seat the progressive reading counts as losing an illegitimate claim, and the civic payoff is broad. It is not near-zero because the settlement concentrates interpretive authority in the reformist academy (see gain_flow): the authority transfer compounds as the settlement prevails, which the series records as extraction accumulation (0.08 to 0.22). Suppression is moderate (0.45): the settlement excludes abrogationist and literalist readings from the institutions it holds and stigmatizes literalism within reformist spaces, but runs no coercive apparatus of its own and the excluded readings persist outside its jurisdiction. Theater is low (0.18): the exegetical work is real, though official-orthodoxy repetition grows with state adoption. Accessibility collapse is low (0.30): the rival readings remain live, taught, and institutionally housed. Resistance is high (0.65): takfir-adjacent charges, modernism-as-Western-imposition rhetoric, and institutional refusal in classical-law bodies. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: the settlement began as an unenforced scholarly minority position and matured into an enforced orthodoxy requiring continuous hermeneutical and, in some jurisdictions, state maintenance. The series are cyclical as well as trending: settlement ascendancy through the early twentieth century, a contest trough during the 1970s-80s Islamist resurgence (visible as the dip at t=96-108 in extractiveness and theater, when settlement institutions contracted in several jurisdictions), then post-reinstitutionalization recovery. The oscillation is contest-driven — an external ideological cycle — not intermittent reinforcement by the settlement itself; the base_properties values are measured at the recovery/ascendancy phase (t=144). All three series share one time grid (0, 24, 48, 72, 96, 108, 132, 144).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute differently. From the textualist jurisprudential seat, the settlement operates as confiscation: it takes the verse out of the instrument set their authority was built to administer, and their identity lock — their method IS the transmitted textualism — makes the loss total rather than partial. From the reformist and minority seats, the same structure operates as a coordination achievement: it dissolves a standing threat and reconciles scriptural and civic allegiance. The excluded rank-and-file seat experiences a third structure: an elite settlement authored in fora they cannot enter, defended in a scholarly vocabulary they were never trained in. The engine computes these per-seat classifications from the structural data; the authored rope claim is the progressive seat's own framing and does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: secular_pluralist_polities (assurance against the universal-command claim; not governed by the interpretive rule but dependent on it), muslim_minority_communities (civic belonging secured; identity-locked to the community whose settlement this is), non_muslim_religious_minorities (the class the directive classically addressed; protection tracks which reading their host institutions adopt), and modernist_reform_scholars (institutional standing, publication fields, advisory roles — and, as gain_flow records, the seat the extracted interpretive authority accrues to). Victims: textualist_jurisprudential_authorities (primary paying seat — the verse is ruled out of their instrument set; identity-locked, so the loss is unhedgeable) and abrogationist_militant_movements (their central proof-text loses scholarly legitimacy, raising recruitment and legitimation costs). The derivation should place the textualist seat near the full-target end (declared victim plus identity-locked exit), the polity, minority, and non-Muslim seats near the beneficiary end, and the scholar seat low but not minimal — a dual agenda-setter/beneficiary position that bears the enforcement burden its own settlement requires. No directionality overrides are authored: the beneficiary/victim declarations plus exit options capture the seat structure, and the scholar seat's dual position is recorded through secondary_role and gain_flow rather than by overriding the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling an expansionist-looking canonical directive with pluralist civic order — is live, so no mandatrophy is declared. The classification discipline matters in three directions. First, it prevents mislabeling the settlement as pure extraction because a paying seat exists: the coordination function is genuine and dominant, and the paying seat is not coerced into participation. Second, it prevents mislabeling the verse's directive itself as natural law: the abrogationist reading treats the directive as eternal divine law — a natural-law frame — but under this reading that is a constructed claim sustained by hermeneutical enforcement, a false-summit pattern whose proper home is the sibling story, not this one. Third, it guards against premature drift toward theatrical maintenance: the theater series is rising as the settlement becomes official orthodoxy, and if exegetical work continues to be replaced by ritual repetition while the enforcement infrastructure persists, the settlement would drift toward performative upkeep — the shared measurement grid exists to catch that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This story instantiates only the progressive_synthesis reading of kernel quran_9_5_scope. How much of the classification — low extraction, coordination claim, beneficiary/victim sets — is a property of the underlying text and history, versus a property of the reading choice?',
    'Generate the sibling stories (quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive) and compare per-seat classifications across the family; the disagreement''s structural location is the verse''s temporal index (eternal command / occasion-bound provision / expired directive), and the victim set flips from polytheist populations (siblings) to textualist authority structures (this reading).',
    'Under abrogating_universal the verse itself is the operative constraint with polytheist populations as targets and militant authorities as beneficiaries; under contextual_defensive the verse is a treaty-law provision with treaty-breaking tribes as paying seats. This story''s low extraction and coordination claim hold only within the progressive reading''s frame and do not transfer to the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Reading-indexed classification: the constraint exists only under the progressive reading of the verse-9:5-scope kernel.').

omega_variable(
    supersession_mechanism_ambiguity,
    'Is the ethical trajectory''s supersession of the verse a claim about the text''s internal structure (the canon''s own later, universalizing material governs its earlier particular directives) or about moral history (the community''s developed ethics overrides the text)?',
    'Examine how settlement authors ground supersession: double-movement and occasion-of-revelation arguments are internal-structure claims; civic-utility and moral-development arguments are moral-history claims. Classify the dominant grounding in the institutions that actually maintain the settlement.',
    'If internal-structure, the settlement is a conventional interpretive standard with low extraction, as authored. If moral-history, the settlement licenses open-ended revision of the canon by present ethics, the textualist seat''s extraction claim strengthens, and the structure moves toward enforced revision rather than interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_mechanism_ambiguity, conceptual, 'Whether supersession is an intra-textual hermeneutic or a license for moral-historical revision.').

omega_variable(
    polity_benefit_genuineness,
    'Do secular-pluralist polities and minority communities benefit from the settlement as a genuine coordination good (a shared reading standard that reduces conflict), or as a legitimacy subsidy that pacifies dissent without changing material conditions?',
    'Compare minority-community security and belonging outcomes across jurisdictions where the settlement is institutionally dominant versus contested, controlling for material policy; if outcomes track settlement prevalence independent of policy, the benefit is substantive, and if not, it is subsidy.',
    'If subsidy, the polity and minority seats'' directionality shifts away from pure beneficiary, part of the settlement''s coordination function is cover, and extraction is understated by the authored 0.22.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(polity_benefit_genuineness, empirical, 'Whether the settlement''s civic payoff is coordination or legitimation subsidy.').

omega_variable(
    hermeneutical_enforcement_sufficiency,
    'Is the settlement''s enforcement purely hermeneutical (scholarly authority, curricula, juristic councils) or does its persistence depend on secular state power (state religious ministries, security designations of literalist and militant movements)?',
    'Compare settlement stability across jurisdictions with and without state enforcement of religious norms, and trace whether settlement institutions survive loss of state patronage.',
    'If state-dependent, the authored suppression understates the coercive infrastructure, the enforcement flag describes a heavier apparatus than hermeneutics alone, and the settlement''s character moves toward state-maintained orthodoxy rather than scholarly authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_enforcement_sufficiency, empirical, 'Whether the settlement''s enforcement is scholarly, state-backed, or both.').

omega_variable(
    militant_seat_victim_status,
    'Do abrogationist militant movements bear the settlement''s costs as victims, or are they better modeled as a rival arrangement whose delegitimation is a benefit to every other seat in this story?',
    'Assess whether the settlement imposes costs on the movements that they did not already impose on themselves through their own commitments, and whether their loss of proof-text legitimacy transfers to any seat in this story or simply dissipates.',
    'If rival-arrangement rather than victim, the victim declaration overstates asymmetric extraction, the settlement''s profile moves toward pure coordination, and the paying-seat structure reduces to the textualist authorities alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militant_seat_victim_status, conceptual, 'Whether the militant seat is a paying party or a rival arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 144).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.04).
narrative_ontology:measurement(qura_tr_t24, quran_9_5_scope__progressive_synthesis, theater_ratio, 24, 0.06).
narrative_ontology:measurement(qura_tr_t48, quran_9_5_scope__progressive_synthesis, theater_ratio, 48, 0.08).
narrative_ontology:measurement(qura_tr_t72, quran_9_5_scope__progressive_synthesis, theater_ratio, 72, 0.11).
narrative_ontology:measurement(qura_tr_t96, quran_9_5_scope__progressive_synthesis, theater_ratio, 96, 0.1).
narrative_ontology:measurement(qura_tr_t108, quran_9_5_scope__progressive_synthesis, theater_ratio, 108, 0.11).
narrative_ontology:measurement(qura_tr_t132, quran_9_5_scope__progressive_synthesis, theater_ratio, 132, 0.16).
narrative_ontology:measurement(qura_tr_t144, quran_9_5_scope__progressive_synthesis, theater_ratio, 144, 0.18).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(qura_be_t24, quran_9_5_scope__progressive_synthesis, base_extractiveness, 24, 0.1).
narrative_ontology:measurement(qura_be_t48, quran_9_5_scope__progressive_synthesis, base_extractiveness, 48, 0.13).
narrative_ontology:measurement(qura_be_t72, quran_9_5_scope__progressive_synthesis, base_extractiveness, 72, 0.17).
narrative_ontology:measurement(qura_be_t96, quran_9_5_scope__progressive_synthesis, base_extractiveness, 96, 0.15).
narrative_ontology:measurement(qura_be_t108, quran_9_5_scope__progressive_synthesis, base_extractiveness, 108, 0.16).
narrative_ontology:measurement(qura_be_t132, quran_9_5_scope__progressive_synthesis, base_extractiveness, 132, 0.21).
narrative_ontology:measurement(qura_be_t144, quran_9_5_scope__progressive_synthesis, base_extractiveness, 144, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__progressive_synthesis, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(qura_su_t24, quran_9_5_scope__progressive_synthesis, suppression_requirement, 24, 0.15).
narrative_ontology:measurement(qura_su_t48, quran_9_5_scope__progressive_synthesis, suppression_requirement, 48, 0.24).
narrative_ontology:measurement(qura_su_t72, quran_9_5_scope__progressive_synthesis, suppression_requirement, 72, 0.33).
narrative_ontology:measurement(qura_su_t96, quran_9_5_scope__progressive_synthesis, suppression_requirement, 96, 0.4).
narrative_ontology:measurement(qura_su_t108, quran_9_5_scope__progressive_synthesis, suppression_requirement, 108, 0.42).
narrative_ontology:measurement(qura_su_t132, quran_9_5_scope__progressive_synthesis, suppression_requirement, 132, 0.46).
narrative_ontology:measurement(qura_su_t144, quran_9_5_scope__progressive_synthesis, suppression_requirement, 144, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, information_standard).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the legal status of the Sword Verse' decomposes into three structurally distinct constraints — a standing universal command (abrogating_universal), a context-bound treaty-law provision (contextual_defensive), and a time-bound expired directive whose settlement is this story (progressive_synthesis). Each member carries its own epsilon, beneficiary/victim structure, and claimed type. Edges run from this settlement to both siblings because its institutional prevalence changes their operating environments: it occupies the moderating ground the contextual reading traditionally held (structural pressure without logical foreclosure) and directly contradicts the abrogationist reading's core premise. The contest is bidirectional in practice — the siblings' persistence is this settlement's measured resistance — but the authored edges record this reading's downstream structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
