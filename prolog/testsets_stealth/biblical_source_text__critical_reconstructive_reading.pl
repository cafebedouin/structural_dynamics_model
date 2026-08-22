% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical-Reconstructive Priority in Biblical Translation
 *   domain: religious/scholarly
 *
 * SUMMARY:
 *   Within the biblical_source_text kernel, this story instantiates the
 *   critical_reconstructive_reading: the commitment that translation and
 *   interpretation must defer to an evidentially reconstructed textual basis,
 *   and that neither formal structure nor communicative meaning may be
 *   privileged until that basis is established. The arrangement is
 *   administered by critical-edition committees, staffed by academic
 *   philologists, consumed by translation agencies, and borne — in lost
 *   certainty and displaced authority — by confessional communities whose
 *   received texts are progressively reclassified as late or composite.
 *   Stated assumptions: the interval 0-75 maps to circa 1950-2025, spanning
 *   the consolidation of the modern critical editions, the Qumran publication
 *   cycle, and the digital-apparatus era; epsilon's referent is the
 *   critical-reconstructive arrangement itself, assessed by this reading's
 *   own lights; the formal-equivalence and dynamic-equivalence readings are
 *   separate constraint files linked through the network block, not folded
 *   into this one.
 *
 * KEY AGENTS:
 *   - academic_biblical_scholars: primary beneficiary (organized/identity_locked) — collects professional authority, publication standing, and curricular indispensability from the reconstruction-first ordering
 *   - critical_edition_committees: agenda setter (institutional/constrained) — administers the standard and decides which readings enter the base text
 *   - bible_translation_agencies: dual-positioned beneficiary/payer (institutional/constrained) — receives the settled base text, absorbs recurring revision costs
 *   - confessional_denominations: primary payer (organized/identity_locked) — loses proof-texts and doctrinal certainty as received readings are reclassified
 *   - traditionalist_translators: payer (moderate/constrained) — craft subordinated to apparatus decisions they cannot appeal
 *   - lay_congregants: payer with incidental benefit (powerless/trapped) — encounters destabilization as marginal notes with no seat in the process
 *   - majority_text_advocates: excluded voice (organized/trapped) — contests the method from outside the editorial rooms
 *   - comparative_religion_historians: analytical observer (analytical/analytical) — documents the authority migration without a stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.6).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.48).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical-Reconstructive Priority in Biblical Translation").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/scholarly").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '3d8ea8d0-9628-4a88-a872-708efabaab17').
narrative_ontology:cs_kernel_codification('3d8ea8d0-9628-4a88-a872-708efabaab17', formalized).
narrative_ontology:cs_authority_grounding('3d8ea8d0-9628-4a88-a872-708efabaab17', expertise).
narrative_ontology:cs_interpretation_layer_present('3d8ea8d0-9628-4a88-a872-708efabaab17').
narrative_ontology:cs_reading_relation('3d8ea8d0-9628-4a88-a872-708efabaab17', biblical_source_text__formal_equivalence_reading, influences).
narrative_ontology:cs_reading_relation('3d8ea8d0-9628-4a88-a872-708efabaab17', biblical_source_text__dynamic_equivalence_reading, influences).
narrative_ontology:cs_axiom('3d8ea8d0-9628-4a88-a872-708efabaab17', foundational, textual_basis_precedes_interpretive_privilege).
narrative_ontology:cs_axiom_status(textual_basis_precedes_interpretive_privilege, holdable).
narrative_ontology:cs_axiom_grounding('3d8ea8d0-9628-4a88-a872-708efabaab17', textual_basis_precedes_interpretive_privilege, empirically_contingent).
narrative_ontology:cs_axiom('3d8ea8d0-9628-4a88-a872-708efabaab17', foundational, hypothetical_autograph_is_legitimate_recovery_target).
narrative_ontology:cs_axiom_status(hypothetical_autograph_is_legitimate_recovery_target, holdable).
narrative_ontology:cs_axiom_grounding('3d8ea8d0-9628-4a88-a872-708efabaab17', hypothetical_autograph_is_legitimate_recovery_target, empirically_contingent).
narrative_ontology:cs_axiom('3d8ea8d0-9628-4a88-a872-708efabaab17', secondary, ecclesial_certainty_yields_to_evidential_revision).
narrative_ontology:cs_axiom_status(ecclesial_certainty_yields_to_evidential_revision, holdable).
narrative_ontology:cs_axiom_grounding('3d8ea8d0-9628-4a88-a872-708efabaab17', ecclesial_certainty_yields_to_evidential_revision, instrumental).
narrative_ontology:cs_reference_frame('3d8ea8d0-9628-4a88-a872-708efabaab17', evidential_consensus_baseline).
narrative_ontology:cs_drift_state('3d8ea8d0-9628-4a88-a872-708efabaab17', contemporary_initial_text_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3d8ea8d0-9628-4a88-a872-708efabaab17', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, critical_edition_committees).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, bible_translation_agencies).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_denominations).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, traditionalist_translators).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, lay_congregants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, lay_congregants).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, bible_translation_agencies).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, earliest_witness_priority).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, reasoned_eclecticism).
narrative_ontology:constraint_vindicates(biblical_source_text__critical_reconstructive_reading, genealogical_method_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Philologists, textual critics, and professors whose daily work is weighing manuscript witnesses and publishing reconstructions. The priority of establishing the textual basis is what makes their expertise the necessary first step for every translation committee and seminary curriculum; their publications, appointments, and standing flow through guild venues that presuppose the method. Leaving the method would mean leaving the profession they trained for.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars, beneficiary,
    organized, generational, identity_locked, global).

% Small editorial boards of the kind behind the major Greek and Hebrew critical editions. They convene periodically, weigh variant readings against stated criteria, and publish the base text that modern translation projects license from. Their decisions determine which readings enter the running text and which familiar passages acquire footnotes marking them as late. Membership is collegial and term-bound; dissent happens inside the committee room, not by publishing a rival standard.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, critical_edition_committees, agenda_setter,
    institutional, generational, constrained, global).

% Missionary and denominational translation organizations producing Bibles in hundreds of languages. They receive a settled, adjudicated base text and build entire translation programs on it, but they also absorb recurring costs: revising published translations when a new edition overturns readings, retraining translator cohorts, and defending the changes to constituencies who preferred the older wording.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, bible_translation_agencies, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, bible_translation_agencies, payer).

% Church bodies whose doctrine, liturgy, and apologetics cite specific wordings of specific verses. Each critical decision that reclassifies a beloved passage as a later addition removes a proof-text they taught from, forces catechetical revision, and relocates final say over what Scripture says from bishops and confessions to academic committees. Their communal identity is bound to the transmitted text; they respond to revisions by resisting them rather than adopting them.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_denominations, payer,
    organized, generational, identity_locked, global).

% Translators and publishers working in the King James lineage and other received-text traditions. Their craft judgments about rendering the text as received are subordinated to apparatus decisions they did not make and cannot appeal; the standard editions publicly label their textual basis defective, which costs them scholarly credibility and market standing.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, traditionalist_translators, payer,
    moderate, biographical, constrained, global).

% Ordinary worshippers who encounter the entire apparatus only as marginal notes in their pew Bibles — notices that the earliest manuscripts do not include a familiar verse — losing certainty about passages they memorized, with no seat in any deliberative body and no realistic channel to contest a reading. They also receive the upside: the text they eventually read is more accurately established than the one their grandparents read.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, lay_congregants, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, lay_congregants, beneficiary).

% Scholars and lay movements arguing for Byzantine-priority or majority-text methods, who contend that the preference for the oldest witnesses is methodologically biased and that the hypothetical-original target imports unstated assumptions. They publish in parallel journals and presses; they sit outside the editorial rooms where the standard is set, and their objections register mainly as footnotes in the literature they critique.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, majority_text_advocates, excluded,
    organized, generational, trapped, global).

% Historians of religion and book culture who study how the biblical text was transmitted, disputed, and standardized, without a stake in which translation philosophy prevails. They document how authority migrated between academy and church across each revision cycle, and both camps cite their work when convenient.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, comparative_religion_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars).
narrative_ontology:fixing_cost_class(biblical_source_text__critical_reconstructive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single adjudicable textual basis for translation and interpretation: variant readings scattered across thousands of manuscripts are weighed once, under agreed evidentiary criteria, so that translators and interpreters work from a common reconstructed text instead of divergent received traditions.
% TRANSFER_FUNCTION: Moves interpretive authority — and the certainty and status attached to what the text says — from ecclesial bodies and received-text traditions to academic editorial committees and credentialed scholars; moves doctrinal certainty away from confessional communities whenever a critical decision overturns a familiar reading.
% ABSENT_VOICES: Majority-text advocates and confessional authorities who regard the hypothetical-original construct itself as theologically loaded are absent from editorial deliberation; pew-level voices affected by footnote destabilization have no seat anywhere in the process. Unanimity inside the editorial rooms therefore reflects who was admitted, not assent.
% DISAPPEARANCE_RATIONALE: If the reconstruction-first priority vanished overnight, translation projects would fragment across competing received texts with no adjudication layer, cross-confessional reference works and lectionary agreements would lose their common basis, and the academic discipline built on reconstruction would lose its mandate — the arrangements of every named party demonstrably depend on it.
% FOUNDING_PROBLEM: Divergent manuscript streams and received texts meant every translation decision rested on a contested base with no agreed adjudication.
% FOUNDING_PROBLEM_CORROBORATION: Confessional translation agencies, Catholic and Orthodox scholarly bodies, and majority-text advocates all attest the manuscript-divergence problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60 because the arrangement's costs concentrate asymmetrically: confessional seats surrender proof-texts, catechetical stability, and final say over their own scriptures, while the coordinating good (a common base text) is real and consumed by all sides. Suppression is authored at 0.48 as a raw structural property — unscaled by power or scope, per the framework's division of labor; the engine scales only extractiveness. Enforcement is real (peer review, accreditation expectations, edition licensing) but parallel traditions persist and no exit is legally barred, so suppression sits mid-range rather than high. Theater_ratio is 0.20: the philological work is overwhelmingly functional, but a visible share of activity is ritual maintenance of the standard — successive editions issued with marginal changes that sustain institutional momentum and subscription revenue more than they alter the text. Accessibility_collapse is 0.35: understanding the constraint does not close alternatives, since received-text translations, formal-equivalence projects, and dynamic-equivalence projects all remain live. Resistance is 0.55: sustained confessional pushback, majority-text movements, and periodic public controversies over revised wordings. The three temporal series run on one shared grid (t=0,15,30,45,60,75) so every metric is authored at every examined point; endpoints match the scalar base_properties. The rising suppression_requirement series is authored deliberately: enforcement capacity hardened over the interval (publication gatekeeping, accreditation norms, centralized digital apparatuses), which is the dynamic being traced. Coalition note: the payer seats are not uniformly powerless — confessional_denominations are organized and have repeatedly formed counter-coalitions around received-text standards, which caps how far effective extraction against them can ratchet.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats compute differently from the same structure. From the committee and scholar seats, the arrangement is the discipline they inhabit: the sequencing of textual basis before interpretive privilege is simply correct procedure, and destabilizing received readings is truth-tracking service. From the confessional and traditionalist seats, the same sequencing operates as dispossession — authority over the text migrates outward to people who do not pray with it. Identity-lock runs on both sides through different mechanisms: for academic_biblical_scholars it is professional identity fusion (career path dependence — the critical method constitutes their expertise, so entertaining received-text parity is professionally self-annihilating); for confessional_denominations it is relational-communal identity (the received wording is woven into liturgy and self-concept, so adopting the critical text feels like betraying the community's own inheritance). If either identity frame broke — scholars treating reconstruction as one tool among several, denominations treating textual plurality as ordinary — the extraction asymmetry would soften markedly without any rule changing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: academic_biblical_scholars and critical_edition_committees sit near the beneficiary end (the arrangement subsidizes their authority), with bible_translation_agencies nearby but pulled toward symmetric by their genuine revision costs. Victim declarations map to high directionality: confessional_denominations and traditionalist_translators sit near the full-target end, amplified by identity_locked exit; lay_congregants are trapped targets whose costs arrive without their participation. No directionality_overrides are authored: the override mechanism keys on power_atom, and the institutional atom is shared by critical_edition_committees (clean beneficiary, d near 0.05) and bible_translation_agencies (mixed, d nearer 0.3) — an override at that atom would correct one seat by corrupting the other. The agencies' dual position is carried instead through secondary_role, which the structural derivation reads directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — divergent witnesses with no adjudication layer — is live, so this is not a mandate outliving its function, and no mandatrophy_resolved flag is authored. The tangled_rope classification does real work in both directions: it prevents mislabeling the arrangement as pure extraction (the coordination function is genuine — without a common base text, translation collapses into warring text-traditions), and it prevents mislabeling it as pure coordination (the extraction is asymmetric and enforced, not a diffuse coordination cost — identifiable confessional seats pay in certainty and authority while identifiable academic seats collect in standing and mandate). The slow rise in theater_ratio alongside rising extractiveness is the drift signal worth watching: if the recovery target proves unreachable (see the recoverability omega), the arrangement's maintenance could decouple from its function and slide toward piton dynamics in a later cohort.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the biblical_source_text kernel (reading: critical_reconstructive_reading). What structurally changes under the sibling readings — formal_equivalence_reading and dynamic_equivalence_reading?',
    'Generate the sibling stories and compare per-seat classifications: the same seats (committees, agencies, denominations, laity) reclassified under each reading''s own beneficiary/victim declarations and exit structure.',
    'Under formal_equivalence_reading, extraction concentrates on translators and reader-communities facing untranslatable structures; under dynamic_equivalence_reading, it concentrates on communities needing liturgical or doctrinal precision. This reading''s distinct signature is extraction flowing from confessional certainty toward the academic guild.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which kernel, which reading, what the siblings would change.').

omega_variable(
    disagreement_location_axis,
    'Where exactly do the three readings disagree — on facts, on values, or on sequencing?',
    'Conceptual analysis testing whether the three priorities are sequentially compatible (text first, then structure-or-meaning choices) or competitively exclusive within a single translation act.',
    'If sequential, the kernel decomposes into stage-gates rather than rivals and each reading''s classification softens toward rope; if competitive, the readings are genuine rivals and per-seat extraction diverges sharply across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_axis, conceptual, 'Locates the kernel dispute: priority-ordering versus substantive rivalry.').

omega_variable(
    hypothetical_original_recoverability,
    'Is the hypothetical original text recoverable in principle, or is an ''initial text'' the practical ceiling of the method?',
    'Convergence behavior of genealogical methods as digitized witnesses accumulate; whether successive flagship editions stabilize toward agreement or proliferate competing readings.',
    'If unrecoverable, the reading''s second foundational axiom fails empirically and the arrangement drifts toward theatrical maintenance of an unreachable target — theater_ratio should then climb past 0.5 and the classification slides toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypothetical_original_recoverability, empirical, 'Empirical status of the recovery target anchoring the whole reading.').

omega_variable(
    extraction_valence_confessional,
    'Is the destabilization of received texts extraction from confessional communities, or the distributed price of a public good those communities also consume?',
    'Track whether confessional seats receive compensating access — open digital apparatuses, freely licensed critical texts, scholarly services — proportional to the certainty surrendered at each revision cycle.',
    'Real compensation pulls the confessional seats'' effective extraction down toward the coordination side of the hybrid; absent compensation, the asymmetry reads as one-directional transfer and the arrangement trends snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_valence_confessional, empirical, 'Whether confessional costs are uncompensated transfer or shared-cost public-good provision.').

omega_variable(
    original_text_priority_naturalness,
    'Is ''establish the textual basis first'' a universal epistemic necessity, or a historically particular construction that happens to serve guild interests?',
    'Comparative history of transmission traditions that never posited a single recoverable original (rabbinic pluriformity, early Syriac and Latin streams) and whether their translation practice collapsed without such a priority.',
    'If the priority is constructed rather than natural, claims of epistemic neutrality fail and the beneficiary structure becomes dispositive — the arrangement is a maintained standard serving identifiable collectors, not a discovered law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_text_priority_naturalness, conceptual, 'Naturalness of the reconstruction-first ordering versus constructed guild interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crit_recon_tr_t0, biblical_source_text__critical_reconstructive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(crit_recon_tr_t0, observed).
narrative_ontology:measurement(crit_recon_tr_t15, biblical_source_text__critical_reconstructive_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(crit_recon_tr_t15, observed).
narrative_ontology:measurement(crit_recon_tr_t30, biblical_source_text__critical_reconstructive_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(crit_recon_tr_t30, observed).
narrative_ontology:measurement(crit_recon_tr_t45, biblical_source_text__critical_reconstructive_reading, theater_ratio, 45, 0.16).
narrative_ontology:measurement_basis(crit_recon_tr_t45, observed).
narrative_ontology:measurement(crit_recon_tr_t60, biblical_source_text__critical_reconstructive_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(crit_recon_tr_t60, observed).
narrative_ontology:measurement(crit_recon_tr_t75, biblical_source_text__critical_reconstructive_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement_basis(crit_recon_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(crit_recon_be_t0, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(crit_recon_be_t0, observed).
narrative_ontology:measurement(crit_recon_be_t15, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(crit_recon_be_t15, observed).
narrative_ontology:measurement(crit_recon_be_t30, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(crit_recon_be_t30, observed).
narrative_ontology:measurement(crit_recon_be_t45, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 45, 0.56).
narrative_ontology:measurement_basis(crit_recon_be_t45, observed).
narrative_ontology:measurement(crit_recon_be_t60, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(crit_recon_be_t60, observed).
narrative_ontology:measurement(crit_recon_be_t75, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 75, 0.6).
narrative_ontology:measurement_basis(crit_recon_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(crit_recon_su_t0, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(crit_recon_su_t0, observed).
narrative_ontology:measurement(crit_recon_su_t15, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(crit_recon_su_t15, observed).
narrative_ontology:measurement(crit_recon_su_t30, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(crit_recon_su_t30, observed).
narrative_ontology:measurement(crit_recon_su_t45, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 45, 0.44).
narrative_ontology:measurement_basis(crit_recon_su_t45, observed).
narrative_ontology:measurement(crit_recon_su_t60, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement_basis(crit_recon_su_t60, observed).
narrative_ontology:measurement(crit_recon_su_t75, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 75, 0.48).
narrative_ontology:measurement_basis(crit_recon_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, information_standard).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'faithful Bible translation' decomposes into three structurally distinct commitments: what counts as the text (this reading — critical reconstruction), how source-language structure maps to target structure (formal_equivalence_reading), and how communicative effect maps (dynamic_equivalence_reading). Each has its own epsilon, beneficiary set, and failure modes; conflating them produces observable-dependent epsilon and violates invariance. This reading sits upstream because both siblings consume whatever textual basis it certifies; linkage runs through affects_constraints in all three family files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
