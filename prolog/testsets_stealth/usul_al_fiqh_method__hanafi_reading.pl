% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Expansive Juristic-Derivation Regime (Qiyas / Ra'y / Istihsan)
 *   domain: religious/legal/methodological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the usul_al_fiqh_method kernel:
 *   the Hanafi reading, under which analogical reasoning is expansively
 *   applicable where the texts are silent, considered judgment supplements
 *   analogy at its limits, and juristic preference may depart from strict
 *   analogy for public welfare. The standing arrangement under assessment is
 *   the operative Hanafi methodological regime itself — the allocation of
 *   law-derivation authority to inference-trained jurists — assessed by the
 *   reading's own lights; the reading's endorsed ideal of method-bound
 *   discovery is NOT the referent. Structurally the regime does two things at
 *   once: it solves a genuine coordination problem (finite texts, recurring
 *   novel cases, need for consistent cross-generational adjudication) and it
 *   transfers interpretive authority asymmetrically to the jurist class while
 *   overriding the limiting authority of textualist specialists. The claim
 *   and the metrics are independent authored facts: the claimed type is
 *   tangled_rope because both a coordination function and an enforced
 *   extraction asymmetry are present; the metrics describe the regime's
 *   actual operation and rise over the interval as the school consolidates.
 *   Sibling readings of the same kernel (Maliki, Shafi'i, Hanbali) are
 *   separate constraint stories with their own epsilon values, beneficiary
 *   sets, and classifications; they are linked, not averaged, per the
 *   epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - hanafi_jurist_class: Primary beneficiary and agenda-setter (institutional / identity_locked) — administers the derivation method and collects interpretive authority, appointments, and fees through it
 *   - imperial_legal_bureaucracy: Secondary beneficiary (institutional / mobile) — purchases governability and legal adaptability through patronage of the method
 *   - textualist_hadith_scholars: Primary payer (organized / constrained) — their text-binding limiting authority is overridden wherever inference departs from strict analogy
 *   - untrained_litigants: Payer (powerless / trapped) — governed by rules no founding text states, decided by officials they did not choose
 *   - rival_madhhab_jurists: Excluded (organized / mobile) — object in treatises but hold no seat in courts run on this method
 *   - comparative_law_historians: Analytical observer (analytical / analytical) — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.64).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.54).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Expansive Juristic-Derivation Regime (Qiyas / Ra'y / Istihsan)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "religious/legal/methodological").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, 'b806fb29-34ba-49da-8fd4-71e606d8027c').
narrative_ontology:cs_kernel_codification('b806fb29-34ba-49da-8fd4-71e606d8027c', formalized).
narrative_ontology:cs_authority_grounding('b806fb29-34ba-49da-8fd4-71e606d8027c', lineage).
narrative_ontology:cs_interpretation_layer_present('b806fb29-34ba-49da-8fd4-71e606d8027c').
narrative_ontology:cs_reading_relation('b806fb29-34ba-49da-8fd4-71e606d8027c', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('b806fb29-34ba-49da-8fd4-71e606d8027c', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_reading_relation('b806fb29-34ba-49da-8fd4-71e606d8027c', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('b806fb29-34ba-49da-8fd4-71e606d8027c', foundational, textual_silence_licenses_expansive_analogy).
narrative_ontology:cs_axiom_status(textual_silence_licenses_expansive_analogy, holdable).
narrative_ontology:cs_axiom_grounding('b806fb29-34ba-49da-8fd4-71e606d8027c', textual_silence_licenses_expansive_analogy, deontological).
narrative_ontology:cs_axiom('b806fb29-34ba-49da-8fd4-71e606d8027c', foundational, public_welfare_overrides_strict_analogy).
narrative_ontology:cs_axiom_status(public_welfare_overrides_strict_analogy, holdable).
narrative_ontology:cs_axiom_grounding('b806fb29-34ba-49da-8fd4-71e606d8027c', public_welfare_overrides_strict_analogy, instrumental).
narrative_ontology:cs_reference_frame('b806fb29-34ba-49da-8fd4-71e606d8027c', rationalist_expansive_source_hierarchy).
narrative_ontology:cs_drift_state('b806fb29-34ba-49da-8fd4-71e606d8027c', post_taqlid_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b806fb29-34ba-49da-8fd4-71e606d8027c', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, imperial_legal_bureaucracy).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_hadith_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, untrained_litigants).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, rational_extension_of_revelation).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, public_welfare_override_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanafi_reading, jurist_authority_over_silent_cases).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in the Kufan rationalist curriculum, members decide cases and issue opinions wherever the founding texts are silent, working by analogy from revealed ratios, considered judgment, and departures from strict analogy justified by public welfare. The method is theirs to apply and theirs to define: school councils, commentarial hierarchies, and appointment to judgeships all flow through demonstrated mastery of it. Leaving the method would mean surrendering the credentials, offices, and standing that mastery confers; remaining means submitting their own conclusions to the school's internal review disciplines.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, beneficiary).

% Dynastic administrations from the Abbasids to the Ottomans staff their courts with jurists trained in this method because it lets judges resolve novel commercial, fiscal, and administrative questions without waiting for textual warrant. The state collects governability: predictable rulings across provinces, law that tracks local conditions, and a learned class bound to the method it administers. Patronizing a different school remained available in principle and dynasties occasionally shifted allegiance, so the tie is one of convenience rather than necessity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, imperial_legal_bureaucracy, beneficiary,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, imperial_legal_bureaucracy, agenda_setter).

% Specialists in authenticating and transmitting the Prophet's reported words hold that binding law must trace to verified text and that human inference may extend it only narrowly. Wherever a court applies considered judgment or departs from strict analogy, their limiting function is overridden: their authentication expertise retains prestige, but the adjudicative authority they claim passes to jurists trained in inference instead. Their recourse is polemic, migration toward schools that weight text more heavily, or competition within the same discursive field; none of these removes the override in territory governed by this method.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_hadith_scholars, payer,
    organized, generational, constrained, continental).

% Merchants, tenants, heirs, and defendants bring disputes before judges whose rulings rest on inferential methods they have no training to evaluate. Outcomes in comparable cases vary with the individual jurist's judgment of public welfare, and the remedy for a disliked ruling is hiring another member of the same trained class. Most cannot relocate their dispute to another jurisdiction, and none can argue from the founding texts directly, since the method's authority presupposes exactly the training they lack.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, untrained_litigants, payer,
    powerless, biographical, trapped, regional).

% Jurists of the other Sunni schools operate under methodologies that weight transmitted text, Medinan practice, or hadith authentication more heavily than expansive inference. Several produced sustained written objections to treating considered judgment and welfare-based departures as law-creating. They publish, teach, and adjudicate in their own jurisdictions, but hold no seat in courts run on this method; their objections register as inter-school polemic rather than internal review.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rival_madhhab_jurists, excluded,
    organized, generational, mobile, continental).

% Academic analysts reconstruct how the method formed, spread, and hardened, comparing it with gap-filling doctrines in other legal traditions. They collect no fees from its operation and answer to no school; their accounts shape neither rulings nor appointments, though they supply the documentary record on which any outside assessment rests.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_law_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled, repeatable procedure for deciding cases the founding texts do not address, so that courts across regions and generations reach consistent outcomes without awaiting new revelation; keeps the claim that divine law is complete administrable by extending it through disciplined human inference.
% TRANSFER_FUNCTION: Moves interpretive authority, together with the appointments, fees, and social standing attached to it, from the fixed textual corpus and its textualist guardians to the trained jurist class; moves dispute outcomes away from litigants' own reading of the texts and toward jurist-derived rules.
% ABSENT_VOICES: Textualist hadith scholars objected historically in the ahl al-hadith / ahl al-ra'y controversy but held no seat in courts run on this method; litigants governed by welfare-based departures had no procedural voice in how the method was selected or bounded; rival-school jurists could object in treatises but not inside the courtroom.
% DISAPPEARANCE_RATIONALE: If the method vanished overnight, every Hanafi court would lack any procedure for the large class of cases the texts do not settle: litigation would stall, judges would improvise without shared discipline, or the field would reorganize around a rival methodology with a different beneficiary class. Appointments, curricula, fee structures, and the state's reliance on adaptable law all depend on the arrangement's continuing operation.
% FOUNDING_PROBLEM: Early Muslim communities confronted novel commercial, fiscal, and administrative situations that the Quran and the early hadith corpus did not explicitly address; Kufan jurists built systematic analogy, considered judgment, and welfare-based preference so that God's law could remain administrable across unforeseen cases.
% FOUNDING_PROBLEM_CORROBORATION: The existence of the silence problem is corroborated from outside the benefiting parties: every rival school also accepts some gap-filling instrument (even the most text-restrictive reading admits analogy for clear silence), so no party to the jurisprudential field denies that novel cases require handling; modern historians of Islamic law independently document the formative problem. What the parties dispute is the permissible scope of the instruments, not the reality of the problem.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64 at interval end) reflects a transfer of interpretive authority that is decoupled from textual warrant: each exercise of considered judgment or welfare-based preference converts jurist discretion into binding law, and the discretion is certified by the same class that exercises it. Suppression (0.54) is a raw, unscaled structural property: enforcement runs through court exclusivity, school review disciplines, and the taqlid norms that bind later jurists to settled positions — real coercive machinery, but short of a monopoly, since rival schools published, taught, and adjudicated openly elsewhere. Theater ratio (0.35) is low-to-moderate: the method performed live adjudicative work throughout, but a growing share of activity became scholastic display (commentary super-commentary, methodological treatises as identity markers) as the taqlid consolidation narrowed live derivation. Accessibility collapse (0.48): alternatives never vanished — three sibling methodologies persisted as live options and mixed jurisdictions existed — but within Hanafi-governed court systems the alternative collapsed almost entirely, since the bench recognized only this method. Resistance (0.55): sustained, literate, centuries-long opposition from hadith specialists and rival-school jurists, which the regime absorbed rather than silenced. The measurement series run on one shared time grid (points 0-50 at decade steps mapped onto the formative-through-taqlid arc, roughly 750-1350 CE) so every tracked metric is authored at every examined point; trajectories are monotonic rather than cyclical, driven by institutional consolidation rather than intermittent reinforcement. Base extractiveness rises monotonically (accumulation of jurist-class advantage layered onto a genuine coordination function), which the engine may read as an accumulation signal warranting investigation; suppression_requirement rises in step as school boundaries hardened and enforcement infrastructure matured. Coalition note: the least powerful payers (untrained litigants) are diffuse, geographically scattered, and procedurally voiceless, so coalition power among them is weak despite their numbers — their protection historically came from rival schools offering exit, not from collective action.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the jurist-class seat the arrangement is faithful extension of revelation: analogy discovers the ratio legislator intended, preference corrects analogy where it would defeat revealed purpose, and the discipline binding jurists is real. From the textualist seat the same structure is licensed discretion displacing textual limits — authority flowing to whoever controls the inference machinery. From the litigant seat it is dependence on unevaluable method. The engine computes these per-seat classifications from the structural data (roles, power, exit); the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The jurist class declares as beneficiary and agenda-setter: derivation places it near the beneficiary end of directionality, dampening effective extraction it experiences — though its identity_locked exit and its submission to school review disciplines exert mild upward pressure, since the method binds its holders as well as serving them. The imperial bureaucracy is the purest beneficiary: mobile exit (dynasties could and occasionally did patronize other schools) and a consumption-only relationship push it nearest the subsidy end. Textualist hadith scholars declare as payers with constrained exit: their limiting function is overridden wherever the method operates, placing them near the target end. Untrained litigants are the fullest targets: trapped exit, no methodological voice, outcomes varying with individual jurist judgment. Rival-school jurists sit outside the arrangement rather than under it — their exclusion is the enforcement object itself, experienced as a boundary maintained against them rather than extraction taken from them. No directionality overrides are needed: the beneficiary/victim declarations plus exit options already yield the correct ordering, and the one dual-positioned agent (jurist class) is captured by its secondary role rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Calling the regime pure coordination ignores the enforced asymmetry: a specific class captures the authority the method creates, textualist limitation is overridden rather than debated to resolution, and persistence depends on active enforcement (court exclusivity, taqlid discipline). Calling it pure extraction ignores the genuine achievement: every mature legal system facing finite founding texts develops gap-filling instruments, and this one administered vast territories for centuries through them. Tangled rope preserves both halves. On mandatrophy proper: the founding problem (textual silence in the face of novel cases) remains live — it recurs every generation — so no resolved-mandatrophy declaration is made, and the founding_problem_status x disappearance_verdict pair (live x world_rearranges) raises no zombie flag. The incipient risk is different: the taqlid-era drift visible in the rising theater_ratio suggests the apparatus may be transitioning from live derivation toward inertial maintenance, which is tracked in the atrophy omega rather than declared resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the hanafi_reading of the usul_al_fiqh_method kernel; what structural changes would instantiating a sibling reading instead produce?',
    'Generate and compare the sibling stories (usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading): diff their beneficiary sets, epsilon values, and computed types against this file.',
    'Under the hanbali_reading the beneficiary set shifts from rationalist jurists to hadith specialists, jurist discretion collapses, and epsilon falls sharply; under the maliki_reading Medinan practice and custom enter as parallel authority channels with their own beneficiary structure. The classification of THIS file is valid only for the hanafi_reading; cross-reading comparison requires the sibling files, not re-parameterization of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one of four readings of the shared source-hierarchy kernel, and its classification does not transfer across readings.').

omega_variable(
    istihsan_discretion_binding,
    'Is jurist latitude under welfare-based preference genuinely bounded by the requirement of a stated public-interest justification, or effectively self-certified by the ruling jurist?',
    'Code the classical and post-classical fatwa corpora: measure inter-mufti convergence on materially similar cases where preference departed from strict analogy, and audit whether the public-interest justifications are stated, reviewable, and consistently applied.',
    'High convergence supports the method''s internal account and stabilizes the tangled-rope reading; systematic divergence shows discretion is self-certified, raises effective extraction above the authored base, and drifts the classification snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_discretion_binding, empirical, 'Whether the preference instrument''s discretion is method-bound or self-certified.').

omega_variable(
    gap_filling_necessity_decomposition,
    'Is some gap-filling mechanism a structural necessity of any finite-text legal system, such that only the specific Hanafi allocation of gap-filling authority is constructed?',
    'Comparative-law survey: test whether every mature legal system facing finite founding texts develops analogy, equity, or custom doctrines. If universal, decompose the story into a necessity core and a constructed allocation layer.',
    'Decomposition would split the constraint: the necessity core (silent cases must be handled somehow) computes as a natural feature with negligible extraction, while the constructed allocation layer (who may fill gaps, with what latitude, enforced how) carries the measured extraction and the contested classification. The current single-story epsilon conflates the two layers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gap_filling_necessity_decomposition, conceptual, 'Natural necessity of gap-filling versus the constructed allocation of gap-filling authority.').

omega_variable(
    taqlid_era_functional_atrophy,
    'After the taqlid consolidation, did the methodological apparatus continue performing live derivations, or did it degrade toward scholastic reproduction of settled positions?',
    'Sample post-consolidation Hanafi legal opinions and madrasa curricula: classify each invocation of analogy, considered judgment, or preference as live first-order derivation versus citation-chain rehearsal.',
    'If rehearsal dominates, the late-period arrangement trends toward inertial maintenance of an atrophied function and the authored theater_ratio trajectory understates the terminal condition; if live derivation persists, the tangled-rope reading holds across the whole interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_era_functional_atrophy, empirical, 'Live derivation versus theatrical maintenance in the post-taqlid period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hanafi_usul_tr_t10, usul_al_fiqh_method__hanafi_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(hanafi_usul_tr_t20, usul_al_fiqh_method__hanafi_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(hanafi_usul_tr_t30, usul_al_fiqh_method__hanafi_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(hanafi_usul_tr_t40, usul_al_fiqh_method__hanafi_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(hanafi_usul_tr_t50, usul_al_fiqh_method__hanafi_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(hanafi_usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(hanafi_usul_be_t10, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(hanafi_usul_be_t20, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(hanafi_usul_be_t30, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(hanafi_usul_be_t40, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(hanafi_usul_be_t50, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 50, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(hanafi_usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hanafi_usul_su_t10, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(hanafi_usul_su_t20, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(hanafi_usul_su_t30, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(hanafi_usul_su_t40, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(hanafi_usul_su_t50, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 50, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'usul al-fiqh' covers four structurally distinct allocations of law-derivation authority, one per Sunni school. Per the epsilon-invariance principle these are four constraints, not one constraint viewed from four angles: each reading has its own epsilon (this reading's is the highest of the set, reflecting maximal jurist discretion), its own beneficiary/victim structure (here: rationalist jurist class benefits; textualist limitation is overridden), and its own classification. This upstream story links to all three siblings via affects_constraints; the Shafi'i edge additionally carries an influences relation (Hanafi expansive practice shaped the conditions under which the Shafi'i systematization emerged). No sibling averages into this file's metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
