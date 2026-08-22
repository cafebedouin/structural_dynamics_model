% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [TERMINATED — REVERSED BY AL-MUTAWAKKIL (848–851)]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: The Mihna: State-Enforced Createdness of the Qur'an (833–851)
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   In 833 the Abbasid caliph al-Ma'mun decreed that judges, scholars, and
 *   traditionists affirm the createdness of the Qur'an — the Mu'tazilite
 *   doctrinal position — and dispatched examiners to test them; refusal meant
 *   dismissal, imprisonment, and flogging. The mihna continued under
 *   al-Mu'tasim and al-Wathiq (Ahmad ibn Hanbal's public flogging; Baghdad
 *   scholars exiled to a desert prison) until al-Mutawakkil reversed it in
 *   848. Kernel framing: this story instantiates the
 *   state_enforced_creation_reading — one of three readings of the
 *   quran_ontological_status kernel. The pure doctrinal claim
 *   (created_reading) and the traditionalist counter-claim
 *   (uncreated_reading) are separate constraints with their own epsilon: the
 *   metaphysics alone carries low extraction; the state-enforcement operator
 *   is what generates this story's high extraction, suppression, and named
 *   victims. The epsilon referent is the standing enforcement arrangement as
 *   the scholarly class experienced it — not the Mu'tazilite doctrine as
 *   argued in a Baghdad madrasa.
 *
 * KEY AGENTS:
 *   - caliphal_authority (al-Ma'mun, al-Mu'tasim, al-Wathiq): agenda-setter and primary beneficiary (institutional/arbitrage) — authors the decrees, collects doctrinal control, repeals the arrangement at will
 *   - mutazilite_rationalists (court theologians, chief qadi Ibn Abi Du'ad): secondary beneficiary (organized/constrained) — collects appointments and standing; fortunes bound to patronage
 *   - mihna_examiners (governors and tribunal officials): enforcement administration (organized/mobile) — collects career advancement for administering the examinations
 *   - traditionalist_hadith_scholars (Ahmad ibn Hanbal and the ahl al-hadith): primary target (moderate/identity_locked) — bears the floggings, prisons, and exile; signing would destroy the tradition that constitutes them
 *   - independent_jurists (state-appointed qadis): compelled signatory (moderate/constrained) — trades signature for office under threat of dismissal
 *   - unaffiliated_scholars (reciters, teachers, transmitters): swept-in payer (moderate/constrained) — bears the examination regime without having taken a side
 *   - common_believers: excluded seat (powerless/constrained) — the creed is fixed over their heads; no seat in the tribunals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.82).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.88).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "The Mihna: State-Enforced Createdness of the Qur'an (833–851)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "religious/political/theological").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'b1188734-4a34-4cbd-babb-23a3493c40b2').
narrative_ontology:cs_kernel_codification('b1188734-4a34-4cbd-babb-23a3493c40b2', formalized).
narrative_ontology:cs_authority_grounding('b1188734-4a34-4cbd-babb-23a3493c40b2', extraction).
narrative_ontology:cs_interpretation_layer_present('b1188734-4a34-4cbd-babb-23a3493c40b2').
narrative_ontology:cs_reading_relation('b1188734-4a34-4cbd-babb-23a3493c40b2', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_reading_relation('b1188734-4a34-4cbd-babb-23a3493c40b2', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_axiom('b1188734-4a34-4cbd-babb-23a3493c40b2', foundational, quran_created_rationally_demonstrable).
narrative_ontology:cs_axiom_status(quran_created_rationally_demonstrable, holdable).
narrative_ontology:cs_axiom_grounding('b1188734-4a34-4cbd-babb-23a3493c40b2', quran_created_rationally_demonstrable, theological).
narrative_ontology:cs_axiom('b1188734-4a34-4cbd-babb-23a3493c40b2', foundational, settled_doctrine_state_compellable).
narrative_ontology:cs_axiom_status(settled_doctrine_state_compellable, overridden).
narrative_ontology:cs_axiom_grounding('b1188734-4a34-4cbd-babb-23a3493c40b2', settled_doctrine_state_compellable, conventional).
narrative_ontology:cs_reference_frame('b1188734-4a34-4cbd-babb-23a3493c40b2', caliphal_doctrinal_supremacy).
narrative_ontology:cs_drift_state('b1188734-4a34-4cbd-babb-23a3493c40b2', mutawakkilite_reversal_848, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b1188734-4a34-4cbd-babb-23a3493c40b2', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mutazilite_rationalists).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_hadith_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, independent_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, unaffiliated_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mihna_examiners).
narrative_ontology:constraint_vindicates(quran_ontological_status__state_enforced_creation_reading, mutazilite_createdness_doctrine).
narrative_ontology:constraint_vindicates(quran_ontological_status__state_enforced_creation_reading, caliphal_doctrinal_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Abbasid court under al-Ma'mun, al-Mu'tasim, and al-Wathiq issued the decrees requiring every judge, traditionist, and public teacher to affirm the Qur'an's createdness, appointed the examiners, and punished refusal with dismissal, imprisonment, and flogging. The doctrine cost the court nothing to hold and gave it a lever over an increasingly independent religious establishment: offices, stipends, and judicial authority flowed to those who signed. The same authority reversed the requirement by simple edict in 848 when a successor calculated differently — the court was never bound by the doctrine it imposed.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Court-aligned rationalist theologians whose position on the Qur'an became the state's required creed. They staffed the judiciary — the chief qadi Ibn Abi Du'ad directed examinations — gained stipends, appointments, and standing, and supplied the arguments the tribunals used. Their position depended entirely on caliphal patronage: when the policy reversed, they lost office, were barred from teaching, and fell from influence. Leaving the alliance mid-interval would have meant surrendering the positions it provided.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mutazilite_rationalists, beneficiary,
    organized, biographical, constrained, continental).

% Governors and appointed examiners such as Ishaq ibn Ibrahim in Baghdad summoned scholars, put the affirmation questions, recorded answers, and sent refusers to the caliph in chains. Enforcement duty was a career asset — advancement came from rigor and yield — and postings rotated, so an examiner could move on when assignments changed.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mihna_examiners, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, mihna_examiners, beneficiary).

% Hadith specialists and traditionists, Ahmad ibn Hanbal foremost among them, held that the Qur'an is God's own uncreated speech; signing the affirmation would repudiate the body of tradition they spent their lives transmitting and their standing within it. Refusal brought prison and the lash — Ahmad was publicly flogged and imprisoned, and other refusers were exiled to a desert prison fortress, where some died. The way out — signing — meant betraying the very thing the tribunals demanded they affirm, so most endured the punishment instead, and Baghdad crowds gathered in the streets in support.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_hadith_scholars, payer,
    moderate, biographical, identity_locked, regional).

% State-appointed judges and jurisprudents were required to sign the affirmation to hold or keep office; refusers were dismissed and replaced with candidates who signed. Many held no deep conviction on the metaphysical question either way, but their livelihoods and official standing rode on the signature: refusing meant losing position, complying meant lending their judicial authority to the court's line.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, independent_jurists, payer,
    moderate, biographical, constrained, regional).

% Qur'an reciters, teachers, and transmitters with no position in the dispute were summoned anyway, because the tribunals tested anyone holding religious office or teaching in public. They carried the costs of the examination regime — summons, oath, the risk of refusing — without having taken a side; some signed reluctantly, a few refused and suffered, others stayed silent and hoped not to be called.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, unaffiliated_scholars, payer,
    moderate, biographical, constrained, regional).

% The non-elite population prayed from the book whose status was being settled and rioted in the streets when Ahmad ibn Hanbal was beaten, but no believer outside the examined classes was consulted; the creed was fixed in tribunal rooms they could not enter, and leaving the community's religious life was not a real option for any of them.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, common_believers, excluded,
    powerless, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized the public creed of the caliphate's religious offices: one doctrinal line on revelation's ontological status, uniformly affirmed by every judge, teacher, and transmitter, resolving by administrative fiat a dispute that was producing rival rulings, competing patronage networks, and conflicting public teaching.
% TRANSFER_FUNCTION: Moves public doctrinal conformity — and the epistemic authority over revelation it represents — from the scholarly class to the caliphal court; moves offices, salaries, and physical safety from the court to scholars who affirm, and imprisonment and flogging to those who refuse.
% ABSENT_VOICES: Non-elite believers whose book and worship were being defined; provincial scholars beyond the tribunals' effective summons; and rationalists who accepted the createdness doctrine but opposed compelling it, on the argument that coerced assent is not conviction. All stood outside the tribunal rooms where the creed was settled.
% DISAPPEARANCE_RATIONALE: The arrangement did in fact vanish (848–851): dismissed jurists returned to office, the traditionalist position re-entered open teaching, the Hanbali school consolidated around Ahmad's ordeal as a badge of legitimacy, Mu'tazilite appointees were purged, and caliphal claims to settle doctrine collapsed — the religious economy of Baghdad reorganized around the reversal within a generation.
% FOUNDING_PROBLEM: Al-Ma'mun's decrees framed it as implementing an already-demonstrated truth: reason, he held, had settled that the Qur'an is created, and the remaining problem was obstinate traditionists and negligent predecessors who had failed to compel assent. The arrangement was built to finish that settlement by administrative force and to reclaim doctrinal authority from an increasingly independent scholarly class.
% FOUNDING_PROBLEM_CORROBORATION: The caliphal decrees (preserved in al-Tabari's history) attest the theological framing — that reason had settled the question and only obstinacy remained — but they issue from the arrangement's author and chief beneficiary. From outside the benefiting set: the interrogation and trial records of the victims (Ahmad ibn Hanbal's ordeal, preserved by his students) attest the enforcement mechanism while flatly rejecting that framing; the near-universal swing of compelled affirmers back to uncreated-teaching within weeks of the 848 reversal attests that the problem the affirmations solved was compliance, not conviction; later chroniclers (al-Tabari, al-Dhahabi) treat the episode as a contest over authority between court and scholarly class. No source outside the beneficiary set attests the founding problem as the decrees framed it.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82) because what the tribunals took — careers, bodily safety, and the scholarly class's independent authority over doctrine — was decoupled from any service the arrangement rendered its targets; the coordination product (a uniform public creed) was compelled and evaporated on release. Suppression is higher still (0.88) because persistence depended entirely on active machinery — summons, tribunals, prisons, the lash — and the arrangement collapsed within roughly three years of that machinery stopping, the sharpest available evidence that coercion rather than consent held it up. Theater is low-moderate (0.26): the tribunals performed real suppressive work; the affirmation ritual had a ceremonial shell but enforced genuine conformity while it lasted, with theater rising only as enforcement decayed after 845. Accessibility collapse (0.7): public space for the uncreated position closed almost completely in the state's centers while private dissimulation and provincial distance kept alternatives alive. Resistance (0.7): Ahmad's endurance became a mass symbol, Baghdad crowds risked riot, refusers accepted flogging — and the state itself ultimately repudiated the arrangement. The measurement series runs on one shared time grid (833/836/839/842/845/848/851) with every tracked metric authored at every point; the base scalars describe the arrangement at operative maturity (c. 836–847, the standing arrangement the story is about), while the series records the full lifecycle including the 848–851 collapse — the end-state values sit below the scalars because the arrangement was dismantled, not because it mellowed.
 *
 * PERSPECTIVAL GAP:
 *   From the caliphal seat the arrangement is legitimate administration: the imam settling, by the reason he is charged to uphold, a question the traditionists refuse to see settled — the tribunals are due process. From the traditionalist seat the same tribunals are persecution: an armed metaphysics demanding that scholars affirm what they believe false, on pain of the lash. From the unaffiliated scholar's seat it is an arbitrary toll — a signature with no doctrinal content extracted under threat of dismissal. From the rationalist beneficiary's seat it is overdue vindication shadowed by rising political risk. The engine computes these per-seat divergences from the structural data; the story does not adjudicate them — though the historical record shows which seats' experience the reversal vindicated.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal court authors the arrangement and collects its product (doctrinal control, a subordinated judiciary) — structurally nearest the beneficiary end (d near 0), and its arbitrage-grade exit (repeal by edict) confirms it was never bound by what it imposed. The rationalist court party collects appointments and standing (low d) while carrying reversal risk the plain beneficiary reading only partly captures. Examiners collect careers for administering (low-to-moderate d). The traditionalist scholars sit at the full-target end (d near 1): they bear the floggings and prisons, and their identity-locked exit — signing meant destroying the tradition that constituted them — places them deeper in the target position than mobile payers would be. Jurists and unaffiliated scholars are constrained targets: high d, softened only by the option of reluctant signature. The unexamined believer population holds no formal seat; its costs (a creed fixed over its heads) and benefits (whatever uniformity it valued) are diffuse and near-symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two misreadings. As rope: the coordination story (a single creed line for the caliphate's religious offices) is real but thin — it cannot account for floggings, prison deaths, and the exile of Baghdad scholars, and the uniformity it produced evaporated on release. As piton: the arrangement had concentrated beneficiaries and died the moment enforcement stopped — the opposite of inertial persistence; nothing about it outlived the will that held it up, and no party was too indifferent to kill it. The snare reading fits: a doctrinal cover over an authority grab, held up by coercion, with named victims. On mandatrophy: the founding mandate (compel assent to the rationally settled truth) was not allowed to atrophy — it was repudiated outright by the same institution within fifteen years. founding_problem_status is authored contested because the parties dispute whether the underlying problem was ever the theological one, with the authority contest between court and scholarly class the likelier candidate; the mismatch consumer should read this as a repudiated mandate, not a zombie one — the arrangement is terminally dead, not walking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the state_enforced_creation_reading of the quran_ontological_status kernel; how would the sibling readings (created_reading, uncreated_reading) restructure the constraint''s epsilon and victim set if instantiated instead?',
    'Author the sibling stories separately and compare: the created_reading without state enforcement should show far lower suppression (a doctrine argued in disputation, not compelled) and no prisoners; the uncreated_reading shows the mirror-image structure in later episodes where Sunni courts enforced orthodoxy against dissenters.',
    'If the metaphysical claim is decoupled from enforcement, epsilon collapses toward the pure doctrinal dispute''s level; the snare classification belongs to the enforcement arrangement, not to the createdness claim itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: which kernel this is, which reading is instantiated, and what the sibling readings would change structurally.').

omega_variable(
    doctrine_enforcement_separability,
    'Is the measured extraction attributable to the createdness doctrine as such, or to the caliphate''s decision to compel it — would a state-enforced uncreatedness doctrine produce the same epsilon?',
    'Compare with later state-enforced orthodoxy episodes (e.g., al-Qadir''s enforced anti-Mu''tazilite creed from 1017): if compelling the opposite doctrine yields similar extraction and victim structure, epsilon belongs to the enforcement operator rather than to either doctrine.',
    'If epsilon tracks enforcement rather than doctrine, the kernel''s metaphysical content is classification-irrelevant and the three readings share a common enforcement-layer profile whenever state-compelled — sharpening the reading_relations analysis (influences vs. forecloses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_enforcement_separability, empirical, 'Whether the constraint''s extractive content sits in the enforcement operator or in the doctrine enforced.').

omega_variable(
    compliance_vs_conviction,
    'Did the public affirmations extracted under the mihna represent genuine doctrinal conversion or coerced compliance masking continued uncreated-belief (dissimulation)?',
    'Post-mihna doctrinal behavior: scholars who affirmed under compulsion resumed uncreated-teaching almost immediately upon the 848 reversal — the speed and near-unanimity of the swing indicates the tribunals extracted conformity, not conviction.',
    'If affirmations were largely coerced compliance, the arrangement''s coordination product (a uniform public creed) was fictitious — the extraction produced no durable good, sharpening the snare reading; if some conversion was real, part of the arrangement''s output survives as genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_vs_conviction, empirical, 'Whether the arrangement''s coordination output was real belief or coerced performance.').

omega_variable(
    reversal_cause,
    'Did the 848 reversal reflect a shift in the court''s doctrinal conviction or a political realignment (al-Mutawakkil courting traditionalist legitimacy against the Turkish commanders and the fallen Ibn Abi Du''ad faction)?',
    'Chronicle analysis of the reversal''s sequence: the release of prisoners and the ban on Mu''tazilite discourse preceded any new doctrinal argument — the ordering suggests political realignment rather than persuasion.',
    'If political, the arrangement''s death confirms that its life depended entirely on the agenda-setter''s will — no independent support base held it up — reinforcing the snare classification and the caliph''s arbitrage-grade exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversal_cause, empirical, 'Whether the arrangement ended by persuasion, realignment, or repudiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 851).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t833, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 833, 0.18).
narrative_ontology:measurement_basis(qura_tr_t833, observed).
narrative_ontology:measurement(qura_tr_t836, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 836, 0.2).
narrative_ontology:measurement_basis(qura_tr_t836, observed).
narrative_ontology:measurement(qura_tr_t839, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 839, 0.24).
narrative_ontology:measurement_basis(qura_tr_t839, observed).
narrative_ontology:measurement(qura_tr_t842, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 842, 0.26).
narrative_ontology:measurement_basis(qura_tr_t842, observed).
narrative_ontology:measurement(qura_tr_t845, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 845, 0.3).
narrative_ontology:measurement_basis(qura_tr_t845, observed).
narrative_ontology:measurement(qura_tr_t848, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 848, 0.38).
narrative_ontology:measurement_basis(qura_tr_t848, observed).
narrative_ontology:measurement(qura_tr_t851, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 851, 0.45).
narrative_ontology:measurement_basis(qura_tr_t851, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.5).
narrative_ontology:measurement_basis(qura_be_t833, observed).
narrative_ontology:measurement(qura_be_t836, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 836, 0.68).
narrative_ontology:measurement_basis(qura_be_t836, observed).
narrative_ontology:measurement(qura_be_t839, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 839, 0.78).
narrative_ontology:measurement_basis(qura_be_t839, observed).
narrative_ontology:measurement(qura_be_t842, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 842, 0.82).
narrative_ontology:measurement_basis(qura_be_t842, observed).
narrative_ontology:measurement(qura_be_t845, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 845, 0.78).
narrative_ontology:measurement_basis(qura_be_t845, observed).
narrative_ontology:measurement(qura_be_t848, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 848, 0.45).
narrative_ontology:measurement_basis(qura_be_t848, observed).
narrative_ontology:measurement(qura_be_t851, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 851, 0.25).
narrative_ontology:measurement_basis(qura_be_t851, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.55).
narrative_ontology:measurement_basis(qura_su_t833, observed).
narrative_ontology:measurement(qura_su_t836, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 836, 0.75).
narrative_ontology:measurement_basis(qura_su_t836, observed).
narrative_ontology:measurement(qura_su_t839, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 839, 0.84).
narrative_ontology:measurement_basis(qura_su_t839, observed).
narrative_ontology:measurement(qura_su_t842, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 842, 0.88).
narrative_ontology:measurement_basis(qura_su_t842, observed).
narrative_ontology:measurement(qura_su_t845, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 845, 0.8).
narrative_ontology:measurement_basis(qura_su_t845, observed).
narrative_ontology:measurement(qura_su_t848, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 848, 0.45).
narrative_ontology:measurement_basis(qura_su_t848, observed).
narrative_ontology:measurement(qura_su_t851, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 851, 0.2).
narrative_ontology:measurement_basis(qura_su_t851, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, uncreated_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the mihna / the created-Qur'an controversy' conflates a metaphysical claim with a state-enforcement arrangement laid over that claim. Per the epsilon-invariance principle these are separate constraints: created_reading (the doctrine alone — low epsilon, no prisoners), this story (the doctrine plus caliphal enforcement — high epsilon, named victims), and uncreated_reading (the mirror doctrinal claim, later state-enforced in its own right by Sunni courts). This story links to both siblings: the enforcement reading upstream-shaped each sibling's operating environment, discrediting createdness by association after 848 and forging the uncreated position's political identity as the creed of the persecuted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
