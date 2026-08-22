% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE (historical interval)]
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
 *   human_readable: The Mihna: Caliphal Enforcement of the Created-Qur'an Doctrine
 *   domain: religious/political
 *
 * SUMMARY:
 *   In 833 CE the Abbasid caliph al-Ma'mun decreed that the Qur'an is created
 *   and ordered officials, judges, and traditionists examined on their
 *   affirmation; his successors al-Mu'tasim and al-Wathiq continued the
 *   inquisition (mihna), imprisoning and flogging refusers — Ahmad ibn Hanbal
 *   foremost — until al-Mutawakkil reversed the policy after 848. This story
 *   instantiates the state_enforced_creation_reading of the
 *   quran_ontological_status kernel: a metaphysical claim fused with coercive
 *   state enforcement. The epsilon referent is the standing arrangement under
 *   contest — the mihna enforcement regime itself — not the bare doctrine and
 *   not any alternative arrangement. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as snare (its coordination story — creedal
 *   uniformity — is thin cover for extraction), while the metrics
 *   independently describe heavily extractive, actively enforced operation.
 *   The interval closes at 848, the last point at which the apparatus
 *   operated at designed strength; the post-interval collapse under
 *   al-Mutawakkil is documented in commentary and omegas rather than in the
 *   series. KEY AGENTS (by structural relationship): - abbasid_caliphate:
 *   Primary beneficiary and agenda-setter (institutional/arbitrage) — runs
 *   the tribunals, collects submission - mutazilite_theologians: Secondary
 *   beneficiary (organized/constrained) — supplies doctrine and staffing,
 *   collects offices - traditionalist_scholars: Primary target
 *   (organized/identity_locked) — bears prison and flogging for refusal -
 *   provincial_hadith_communities: Diffuse target (moderate/identity_locked)
 *   — bear the test regionally without leverage - complying_scholars:
 *   Dual-positioned (moderate/constrained) — pay in surrendered independence,
 *   collect continued office - provincial_believers: Excluded seat
 *   (powerless/trapped) — the stakes are theirs, the conversation is not -
 *   islamic_historiographers: Analytical observer — sees the full structure
 *
 * KEY AGENTS:
 *   - abbasid_caliphate: agenda-setter and primary beneficiary (institutional/arbitrage) — decrees, examines, punishes, collects doctrinal submission
 *   - mutazilite_theologians: beneficiary with agenda-setting secondary role (organized/constrained) — supply the doctrine and staff the exams, collect preferment tied to the regime's fortunes
 *   - traditionalist_scholars: primary payer (organized/identity_locked) — refusal costs the body, affirmation costs the vocation; Ahmad ibn Hanbal the emblem
 *   - provincial_hadith_communities: payer (moderate/identity_locked) — regional transmission circles facing the same test without court leverage
 *   - complying_scholars: payer with beneficiary secondary role (moderate/constrained) — compelled affirmation traded for continued office
 *   - provincial_believers: excluded (powerless/trapped) — worshipers whose scripture's status is settled over their heads
 *   - islamic_historiographers: observer (analytical/analytical) — reconstruct the full structure from records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.8).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.88).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "The Mihna: Caliphal Enforcement of the Created-Qur'an Doctrine").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "religious/political").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'd2d50fc4-22a2-41d0-bf63-d0b15dd27977').
narrative_ontology:cs_kernel_codification('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', formalized).
narrative_ontology:cs_authority_grounding('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', extraction).
narrative_ontology:cs_interpretation_layer_present('d2d50fc4-22a2-41d0-bf63-d0b15dd27977').
narrative_ontology:cs_reading_relation('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', foundational, quran_created_not_eternal).
narrative_ontology:cs_axiom_status(quran_created_not_eternal, holdable).
narrative_ontology:cs_axiom_grounding('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', quran_created_not_eternal, theological).
narrative_ontology:cs_axiom('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', foundational, compelled_affirmation_of_created_quran).
narrative_ontology:cs_axiom_status(compelled_affirmation_of_created_quran, overridden).
narrative_ontology:cs_axiom_grounding('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', compelled_affirmation_of_created_quran, instrumental).
narrative_ontology:cs_reference_frame('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', caliphal_doctrinal_supremacy).
narrative_ontology:cs_drift_state('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', post_mutawakkil_reversal, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('d2d50fc4-22a2-41d0-bf63-d0b15dd27977', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mutazilite_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, provincial_hadith_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, complying_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, complying_scholars).
narrative_ontology:constraint_vindicates(quran_ontological_status__state_enforced_creation_reading, caliphal_doctrinal_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the decree requiring affirmation that the Qur'an is created, commissions the interrogation tribunals, appoints the examiners, and punishes refusers with dismissal, prison, and flogging. Uses doctrinal uniformity to subordinate a scholarly class with independent mass credibility after the civil war between al-Amin and al-Ma'mun. Collects submission, legitimation, and a religious establishment answerable to the center; can reverse the entire arrangement by a later decree, as its successor did.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate, agenda_setter,
    institutional, generational, arbitrage, continental).

% Supply the rationalist arguments for the created-Qur'an thesis and serve as examiners in the doctrinal tests. Receive judgeships, stipends, and official preferment while their school sets the state's creed. Their fortunes are bound to the regime that favors them: when the caliphate changes course, they lose office and standing with it.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mutazilite_theologians, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, mutazilite_theologians, agenda_setter).

% Transmit the hadith corpus and teach the received faith; summoned before tribunals to affirm that the Qur'an is created. Refusal costs prison and flogging — Ahmad ibn Hanbal is the emblematic case — but affirmation would repudiate the very tradition they exist to transmit. Their identity and their vocation are the same thing, so exit and betrayal coincide.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    organized, generational, identity_locked, continental).

% Local circles of traditionists and their students outside the capital who learn and transmit scripture as God's own speech. They face the same demand when the test reaches their towns; they hold no leverage at court, and their responses — concealment, quiet persistence, occasional martyrdom — preserve the practice at personal risk.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, provincial_hadith_communities, payer,
    moderate, generational, identity_locked, regional).

% Judges, preachers, and teachers who affirm the formula when examined and keep their posts. They surrender doctrinal independence and carry the knowledge that their affirmation was compelled; in exchange they retain salary, status, and immunity from the tribunal. Leaving means abandoning the career entirely.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, complying_scholars, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, complying_scholars, beneficiary).

% Worshippers in the towns and villages who pray from the recited Qur'an and hold it to be God's very word. The ontological status of their scripture is being settled by tribunals they will never enter; their attachment registers politically only as crowd sympathy when a famous refuser is paraded through Baghdad.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, provincial_believers, excluded,
    powerless, generational, trapped, regional).

% Contemporary annalists and later historians of doctrine who reconstruct the mihna's motives, mechanics, and aftermath from trial records, biographical dictionaries, and caliphal correspondence. They see the full structure — the doctrine, the enforcement machinery, and the interests each served — without sitting inside it.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, islamic_historiographers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the empire's religious offices on a single doctrinal line: by testing every judge, preacher, and hadith teacher, the arrangement solves the problem of official religion speaking with many voices, producing a uniform public creed answerable to the center.
% TRANSFER_FUNCTION: Moves doctrinal submission and professional security from the scholarly class to the caliphate; moves offices, stipends, and preferment to compliant scholars and Mu'tazilite clients; moves prison, flogging, and dismissal onto refusers.
% ABSENT_VOICES: Provincial believers and the non-elite pious had no seat in the tribunals; imprisoned refusers spoke only through students and sympathizers; the generations who would inherit the settled creed were unrepresented. Crowd sympathy in Baghdad registered lay attachment obliquely, but no mechanism translated it into the examination rooms.
% DISAPPEARANCE_RATIONALE: Tribunals close, dismissed judges are reinstated, traditionalist teaching resumes openly, and the caliphate loses its doctrinal lever over the ulema — precisely the reorganization that followed al-Mutawakkil's reversal, when the religious economy reverted to plural, traditionist-led instruction within a few years.
% FOUNDING_PROBLEM: After the civil war between al-Amin and al-Ma'mun, the caliphate confronted a scholarly class with independent mass credibility and a live dispute over the Qur'an's ontological status; the mihna was built to settle that dispute by imperial fiat and to break the ulema's autonomy by making office conditional on creed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the enforcing institution itself formally abandoned the project (al-Mutawakkil's reversal, including the public honoring of Ahmad ibn Hanbal), traditionalist biographical literature records the project's failure from the victims' side, and later Sunni legal consensus rejects compelled creed. No surviving heir of the benefiting parties attests the founding problem as live.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.80 at interval end) because the arrangement took public affirmation, careers, and bodily punishment from the scholarly class while the gains concentrated in one seat. Suppression is higher still (0.88) because nothing about the arrangement rests on participant preference — its entire persistence mechanism is the tribunal apparatus: summons, oath, prison, flogging, dismissal. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Theater ratio is moderate (0.35): the purging function was real while the arrangement lasted, but examination of the ever-larger compliant majority grew ritualized — a public ceremony of affirmation increasingly detached from discovery. Accessibility collapse is 0.62: flight, silence, and dissimulation remained available, and open refusal carried certain punishment, so alternatives narrowed sharply for career scholars without vanishing. Resistance is 0.68: a determined minority refused at real cost, popular sympathy surrounded the chief refuser, and the traditionist coalition's cohesion — its ability to absorb punishment without breaking — is the main reason the arrangement required escalating enforcement and ultimately failed. All three tracked series run on one shared time grid (833, 836, 839, 842, 845, 848) so every metric is authored at every examined point; suppression_requirement is tracked because this story specifically traces enforcement-capacity buildup, which rises monotonically to its 848 peak on the eve of reversal.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the caliphal seat the arrangement looks like legitimate settlement of a dangerous dispute — order imposed on fitna — and its arbitrage exit means the caliphate bears almost none of the arrangement's costs. From the Mu'tazilite seat it looks like truth defended and careers advanced, with the hidden liability that their position is mortgaged to a patron who can abandon them. From the traditionalist seat the identical structure is pure persecution: a demand that one's vocation be repudiated on pain of the body. Complying scholars occupy the middle — a tax paid in integrity, refunded in salary. Same nominal class, divergent constraint: traditionalist and complying scholars hold similar standing in the ulema class, and what differentiates them is exit structure (identity_locked versus constrained) and role acceptance, not global power. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphate sits nearest the beneficiary pole (collects the transfer, controls the rules, holds arbitrage-grade exit — it demonstrated the exit by reversing the whole arrangement). Mu'tazilite theologians sit near it: net collectors of office and preferment, with exit constrained by their dependence on the regime. Traditionalist scholars sit nearest the target pole: they bear the full coercive load and their identity lock removes the exit that would dampen effective extraction. Provincial hadith communities are similarly locked but lack even the elite scholars' networked visibility. Complying scholars derive a mid-high directionality — they pay submission and receive office, a genuinely dual position the secondary_role records. Provincial believers are excluded rather than coordinated: the arrangement decides for them, and their trapped position places them toward the target end despite never entering a tribunal room.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification separates the epistemic content from the enforcement arrangement, which prevents two symmetrical errors: blaming the created-Qur'an doctrine for the caliphate's extraction (the doctrine, held freely among peers, is the sibling created_reading with far lower epsilon), and excusing the enforcement by the doctrine's sincerity (the enforcement's victims were punished for a metaphysical disagreement, not for any harm done). On the genealogy interview the founding problem — settling creed by imperial fiat and breaking ulema autonomy — is dead: the enforcing institution itself formally abandoned the project, and the arrangement did not linger into theatrical maintenance, so the piton path is bounded; the dead-status x world-rearranges combination flags the capture pattern honestly rather than letting a flattering origin myth launder fifteen years of inquisition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the quran_ontological_status kernel (state_enforced_creation_reading); which structural facts belong to the doctrine itself and which to the enforcement fusion with its siblings created_reading and uncreated_reading?',
    'Compare the sibling files'' epsilon values, beneficiary/victim sets, and computed types: the bare created_reading should show low extraction and no victim class; the uncreated_reading should show an inverted or empty victim set relative to this file.',
    'If the enforcement layer is stripped analytically, the residual constraint is the created_reading with far lower epsilon; if the ontological premise flipped, the victim set inverts — the same state machinery enforcing the uncreated reading would punish Mu''tazilites. The snare classification attaches to the fusion, not to either premise alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story is the state-enforced reading of the Qur''an-ontology kernel; siblings are separate constraints.').

omega_variable(
    enforcement_legitimacy_seam,
    'Where is the disagreement located: does the created-Qur''an premise itself license political compulsion, or is compulsion an independent axiom grafted onto the doctrine by caliphal interest?',
    'Examine Mu''tazilite kalam treatises on the Qur''an''s creation apart from their writings on imamate and commanding right: if the compulsion warrant appears only in the political works, the seam is real and the enforced reading is a hybrid.',
    'Determines the weight of the influences edge to created_reading: a doctrine with latent enforcement tendencies differs structurally from one hijacked by a patron. If the seam is real, the plain created_reading carries no enforcement liability; if not, the sibling inherits part of this file''s extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_seam, conceptual, 'Whether compulsion is internal to the doctrine or an external political graft.').

omega_variable(
    contingency_of_coercion,
    'Was the mihna''s extractive form contingent on al-Ma''mun''s person and post-civil-war politics, or is it intrinsic to any state enforcement of this doctrine?',
    'Comparative institutional history of other creed-enforcement episodes under different rulers and dynasties: do comparable arrangements show the same victim structure and enforcement escalation regardless of the enforcing personality?',
    'If contingent, the snare classification binds to this instantiation and a differently-situated enforcement of the same doctrine might compute as tangled_rope; if intrinsic, any state enforcement of this doctrine computes as snare and the enforced reading type itself carries the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingency_of_coercion, empirical, 'Whether the snare character is personality-contingent or structural to enforced creed.').

omega_variable(
    compelled_affirmation_sincerity,
    'Did compelled affirmation change belief or only public words — did the tribunals extract doctrinal conversion or mere verbal conformity?',
    'Post-mihna biographical and pedagogical records: did complying scholars teach the created-Qur''an voluntarily after enforcement ended, or revert immediately to traditionist instruction?',
    'If mere words, the arrangement extracted performance (raising effective theater above the authored 0.35) and belief persisted underground, explaining the speed of the traditionalist resurgence after 848; if belief shifted, extraction reached deeper than the punishment record shows and the collapse trajectory understates the arrangement''s former reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelled_affirmation_sincerity, empirical, 'Whether the extraction product was belief or performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 848).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t833, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 833, 0.2).
narrative_ontology:measurement_basis(qura_tr_t833, observed).
narrative_ontology:measurement(qura_tr_t836, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 836, 0.24).
narrative_ontology:measurement_basis(qura_tr_t836, observed).
narrative_ontology:measurement(qura_tr_t839, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 839, 0.28).
narrative_ontology:measurement_basis(qura_tr_t839, observed).
narrative_ontology:measurement(qura_tr_t842, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 842, 0.3).
narrative_ontology:measurement_basis(qura_tr_t842, observed).
narrative_ontology:measurement(qura_tr_t845, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 845, 0.33).
narrative_ontology:measurement_basis(qura_tr_t845, observed).
narrative_ontology:measurement(qura_tr_t848, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 848, 0.35).
narrative_ontology:measurement_basis(qura_tr_t848, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.55).
narrative_ontology:measurement_basis(qura_be_t833, observed).
narrative_ontology:measurement(qura_be_t836, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 836, 0.7).
narrative_ontology:measurement_basis(qura_be_t836, observed).
narrative_ontology:measurement(qura_be_t839, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 839, 0.77).
narrative_ontology:measurement_basis(qura_be_t839, observed).
narrative_ontology:measurement(qura_be_t842, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 842, 0.81).
narrative_ontology:measurement_basis(qura_be_t842, observed).
narrative_ontology:measurement(qura_be_t845, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 845, 0.83).
narrative_ontology:measurement_basis(qura_be_t845, observed).
narrative_ontology:measurement(qura_be_t848, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 848, 0.8).
narrative_ontology:measurement_basis(qura_be_t848, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.6).
narrative_ontology:measurement_basis(qura_su_t833, observed).
narrative_ontology:measurement(qura_su_t836, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 836, 0.78).
narrative_ontology:measurement_basis(qura_su_t836, observed).
narrative_ontology:measurement(qura_su_t839, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 839, 0.84).
narrative_ontology:measurement_basis(qura_su_t839, observed).
narrative_ontology:measurement(qura_su_t842, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 842, 0.86).
narrative_ontology:measurement_basis(qura_su_t842, observed).
narrative_ontology:measurement(qura_su_t845, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 845, 0.87).
narrative_ontology:measurement_basis(qura_su_t845, observed).
narrative_ontology:measurement(qura_su_t848, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 848, 0.88).
narrative_ontology:measurement_basis(qura_su_t848, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, created_reading).

% DUAL FORMULATION NOTE:
% Constraint family for the quran_ontological_status kernel. The colloquial label 'the Mu'tazilite doctrine that the Qur'an is created' covers two structurally distinct constraints: the bare doctrinal claim (created_reading — a peer intellectual position with negligible extraction) and its fusion with caliphal enforcement (this file — the mihna regime, substantially extractive). The uncreated_reading is the ontological rival whose suppression is this constraint's enforcement object. Measuring the doctrine by its scholarly content yields low epsilon; measuring the enforcement arrangement yields high epsilon — per the epsilon-invariance principle these are two constraints, not one constraint viewed two ways. This upstream enforced reading structurally influenced the bare doctrine's fortunes (state backing, then stigma by association) without foreclosing it, and logically forecloses the uncreated reading within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
