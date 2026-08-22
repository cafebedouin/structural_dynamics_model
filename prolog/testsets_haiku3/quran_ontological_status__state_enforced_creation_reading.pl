% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: State-Enforced Mu'tazilite Creation Doctrine (Mihna Inquisition)
 *   domain: theological/political
 *
 * SUMMARY:
 *   The Abbasid caliphate, under al-Ma'mun (r. 813–833 CE / 198–218 AH) and
 *   al-Mu'tasim (r. 833–842 CE / 218–227 AH), institutionalizes the
 *   Mu'tazilite theological doctrine that the Qur'an is created (makhlūq)
 *   rather than uncreated and eternally existent. The state enforces this
 *   doctrine through the mihna—an inquisition apparatus that summons scholars
 *   and judges to public tribunals, demands their affirmation of the
 *   created-Qur'an position, and punishes refusal with imprisonment, torture,
 *   and dismissal from office. The canonical victim is Ahmad ibn Hanbal,
 *   imprisoned and tortured for his refusal to affirm the doctrine. This is a
 *   state transformation of a theological disagreement into a snare: the
 *   created-Qur'an position is presented as rational truth, but its
 *   persistence depends entirely on coercive enforcement and suppression of
 *   the competing uncreated-Qur'an tradition. This reading instantiates the
 *   created_reading theological claim WITHIN the political frame of state
 *   enforcement—it describes the constraint AS it was instituted by power,
 *   not as a free intellectual choice.
 *
 * KEY AGENTS:
 *   - Abbasid caliphate (al-Ma'mun, al-Mu'tasim): agenda-setter, institutional power, arbitrage exit (can reverse course), administers mihna tribunals
 *   - Traditionalist scholars (Ahmad ibn Hanbal, Dawud al-Zahiri): victims, moderate power individually but organized collectively, trapped exit (identity-locked to their theological commitments), face imprisonment and torture
 *   - Mu'tazilite school: beneficiary temporarily (state backing, institutional dominance), organized power, contingent on state favor
 *   - Literalist communities: victims, powerless individually, trapped exit (devotional practice anchored in uncreated-speech tradition), face coercive conformity pressure
 *   - Scholarly pluralism: excluded, would-be institutional actor, the precedent of open theological debate is itself suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.82).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.88).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Mu'tazilite Creation Doctrine (Mihna Inquisition)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "theological/political").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'f740d0dc-1bfe-4012-9d7e-71be546564cd').
narrative_ontology:cs_kernel_codification('f740d0dc-1bfe-4012-9d7e-71be546564cd', fixed_text).
narrative_ontology:cs_authority_grounding('f740d0dc-1bfe-4012-9d7e-71be546564cd', extraction).
narrative_ontology:cs_interpretation_layer_present('f740d0dc-1bfe-4012-9d7e-71be546564cd').
narrative_ontology:cs_reading_relation('f740d0dc-1bfe-4012-9d7e-71be546564cd', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_reading_relation('f740d0dc-1bfe-4012-9d7e-71be546564cd', quran_ontological_status__uncreated_reading, coexists_with).
narrative_ontology:cs_axiom('f740d0dc-1bfe-4012-9d7e-71be546564cd', foundational, quranic_creation_via_rational_dialectic).
narrative_ontology:cs_axiom_status(quranic_creation_via_rational_dialectic, holdable).
narrative_ontology:cs_axiom_grounding('f740d0dc-1bfe-4012-9d7e-71be546564cd', quranic_creation_via_rational_dialectic, deontological).
narrative_ontology:cs_axiom('f740d0dc-1bfe-4012-9d7e-71be546564cd', foundational, state_authority_over_theological_orthodoxy).
narrative_ontology:cs_axiom_status(state_authority_over_theological_orthodoxy, overridden).
narrative_ontology:cs_axiom_grounding('f740d0dc-1bfe-4012-9d7e-71be546564cd', state_authority_over_theological_orthodoxy, empirically_contingent).
narrative_ontology:cs_reference_frame('f740d0dc-1bfe-4012-9d7e-71be546564cd', quranic_rational_interpretation_framework).
narrative_ontology:cs_drift_state('f740d0dc-1bfe-4012-9d7e-71be546564cd', post_mihna_reversal, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('f740d0dc-1bfe-4012-9d7e-71be546564cd', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_school_temporarily).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes the mihna (inquisition) tribunals, demands public affirmation of the created-Qur'an doctrine from all scholars and judges, and uses theological conformity as the mechanism for political control over the scholarly establishment. Benefits from doctrinal uniformity imposed by state authority and from the precedent that theological claims can be enforced by sovereign decree. Administers the enforcement machinery directly through appointed inquisitors and judges.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate, agenda_setter,
    institutional, generational, arbitrage, continental).

% Gains state backing and institutional power during the mihna period (roughly 218–234 AH / 833–847 CE under al-Ma'mun and al-Mu'tasim). Their theological position—that the Qur'an is created and rationalism is the path to truth—becomes state dogma, backed by coercive power against dissenting scholars. They do not set the terms of enforcement; the state does. Their benefit is contingent on state favor and subject to reversal when political winds shift.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_school_temporarily, beneficiary,
    organized, biographical, constrained, continental).

% Face inquisition tribunals where they are pressured to publicly affirm that the Qur'an is created. Refusal brings imprisonment, torture (Ahmad ibn Hanbal is the canonical example—imprisoned and tortured for refusing to affirm the doctrine), dismissal from judicial office, and social stigma. Their theological position—that the Qur'an is uncreated and eternally existent—is declared heretical. They cannot exit the scholarly community without abandoning their identity and reputation; they cannot change their theological conviction without intellectual dishonesty. The constraint is identity-locked for these agents.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    moderate, biographical, trapped, continental).

% Ordinary believers whose lived practice of Qur'an recitation and interpretation is anchored in the uncreated-speech tradition (kalām Allāh qadīm). The state-enforced created-Qur'an doctrine contradicts their devotional understanding. They face pressure to conform in public discourse, in teaching their children, in communal religious practice. Dissent is costly but often covert. They lack institutional power to contest the state's theological position.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    powerless, biographical, trapped, continental).

% The precedent of reasoned debate between theological schools—where the created and uncreated positions coexist as live scholarly options—is itself suppressed by the mihna. Scholars who might ordinarily defend the right to pluralism find that defense criminalized. The exclusion of pluralism-as-such is the constraint's deepest mechanism: it transforms a theological disagreement into a binary test of loyalty to state-imposed orthodoxy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, excluded,
    institutional, civilizational, analytical, continental).

% Observe the Abbasid experiment in enforced theological conformity. Some adopt versions of state-enforced doctrine; others (notably the Umayyad successor states) reverse the mihna and restore traditionalist dominance after the Abbasid collapse of central authority. Their observations and reversals constitute the long-term test of whether the state-enforced creation doctrine persists by genuine conviction or inertial enforcement.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, competing_caliphates_and_regional_powers, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A coordination function exists but is subsidiary: rational theological discourse via the Mu'tazilite method (dialectical proof, rejection of literal anthropomorphism) could, in principle, unify the scholarly community around one defensible interpretation. The state frames the mihna as enforcing that rational consensus.
% TRANSFER_FUNCTION: Transfers scholarly authority and institutional power from traditionalist schools to Mu'tazilite-aligned jurists and theologians. Transfers the authority to define orthodoxy from scholarly consensus to sovereign decree. Extracts conformity and public affirmation of a specific metaphysical claim from scholars and communities who reject it, under threat of imprisonment and torture.
% ABSENT_VOICES: Popular preachers and village-level religious teachers are largely excluded from the inquisition tribunals (which target high-status judges and scholars); their dissent simmers in communities but is not formally recorded in the elite discourse that historians document. The voices of those who would defend scholarly pluralism as a positive good—as opposed to merely defending their own theological position—are structurally absent from the tribunals.
% DISAPPEARANCE_RATIONALE: If the mihna and its enforcement apparatus vanished overnight, the scholarly community would revert to open debate between uncreated and created positions (as it did after the mihna ended in 234 AH). The precedent that state power can impose theological orthodoxy would be broken, though the memory of its possibility would persist. The Mu'tazilite school would retain its intellectual position but lose state backing and institutional dominance.
% FOUNDING_PROBLEM: The Abbasid caliphate seeks to establish rational theological uniformity across the scholarly establishment to strengthen state authority and prevent theological disagreement from fragmenting political loyalty. The Mu'tazilite position (Qur'an created, reason as path to truth) is chosen as the state doctrine because it supports strong state authority: if the Qur'an is created (not eternally coexistent with God), then the Qur'an is subject to interpretation and reinterpretation by the community of believers and their sovereign.
% FOUNDING_PROBLEM_CORROBORATION: The Abbasid state itself frames the mihna as necessary for theological correctness and political unity. Later historians and jurists (Ibn Kathir, al-Khatib al-Baghdadi, writing centuries later) attest that the founding problem was political control masquerading as theological truth-seeking. Traditionalist scholars imprisoned during the mihna (Ahmad ibn Hanbal, Dawud al-Zahiri) attest that the state's stated problem was theological debate, but the actual mechanism was suppression of dissent. Independent scholarly analysis from outside the contemporary parties (modern historians of Islamic theology) confirms the political-authority framing outweighed the theological one.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high and rising through the interval (0.55 → 0.82 by peak) because the state's coercive apparatus scales up: initial tribunals target high-status judges; enforcement intensity increases as the caliphate consolidates the constraint; the measurement reflects intensifying suppression machinery. Theater ratio is moderate but rising (0.25 → 0.42) because the justification (rational theology, truth-seeking) becomes increasingly decoupled from the mechanism (purging dissenting scholars, enforcing public affirmation regardless of conviction). Suppression is the dominant driver: the constraint's persistence depends on imprisoning Ahmad ibn Hanbal, dismissing traditionalist judges, and preventing public dissent—not on voluntary adoption of the created-Qur'an doctrine by the scholarly community. The measurement series is authored on one shared time grid (interval 0–50, with points at 0, 8, 16, 24, 32, 40, 50) so every metric is valued at every examined moment. The slight decline in the final measurements (extractiveness drops from 0.82 to 0.78, suppression from 0.89 to 0.82) reflects the mihna's eventual loss of state backing as al-Mu'tasim's successors face political pressure and the apparatus begins to decay—but at interval end the constraint remains substantially extractive and suppressive.
 *
 * PERSPECTIVAL GAP:
 *   The caliphate's seat computes the constraint as rope or weak coordination (theological correctness achieved through state-backed rational discourse). The traditionalist-scholar seat computes it as snare or pure extraction (coercive suppression of dissent, masked as truth-seeking). The Mu'tazilite school's seat is internally divided: those who genuinely hold the created-Qur'an position see it as rope (their doctrine vindicated by rational argument), while those who hold it only to access state power see it as contingent rent-extraction. The engine computes per-seat types from the structural data (beneficiary/victim, power, exit, enforcement) independently of the authored claim; this story's claim (snare) matches what the payer and target seats would compute, but diverges from what the beneficiary seat computes. The divergence is exactly the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The agenda-setter seat (Abbasid caliphate) is the structural beneficiary: it extracts doctrinal conformity and political loyalty, collects the authority to define orthodoxy by decree. Directionality for this seat is near-full-beneficiary (d ≈ 0.1): the constraint subsidizes caliphal power, lowers its exit costs (unifying the scholarly establishment under state doctrine), and concentrates institutional authority. The traditionalist-scholar seats are near-full-target (d ≈ 0.9): the constraint extracts their public affirmation, imprisons them for refusal, and gives them no exit except intellectual capitulation or flight. Identity-lock is the mechanism: these scholars cannot exit their tradition without ceasing to be scholars; they cannot affirm the created-Qur'an doctrine without betraying their intellectual convictions; the constraint holds them in place by making every exit costlier than remaining and enduring. The Mu'tazilite school is asymmetrically positioned (d ≈ 0.35): they benefit from state backing and temporary institutional dominance, but they do not set the enforcement terms—the state does—and they are vulnerable to loss of favor. The literalist communities are targets (d ≈ 0.85) but distributed and powerless: they bear conformity pressure but lack institutional standing to resist or exit openly. Scholarly pluralism is excluded entirely (not in stakeholder array but noted in agents list): the constraint's structure forbids the kind of open debate that would ordinarily characterize scholarly life.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is contested: the Abbasid state claims the problem is ongoing theological confusion requiring state-enforced rational doctrine; traditionalist scholars and later historians claim the problem was solved when the inquisition ended (the Qur'an remains the object of scholarly debate, and no subsequent caliph attempted such comprehensive theological enforcement). The disappearance_verdict is world_rearranges: if the mihna vanished, the scholarly community would revert to open debate. The mismatch (contested status × world_rearranges verdict) is diagnostic: the constraint persists not because the founding problem demands it but because the political authority benefits from enforcing it. The theater_ratio rising over time (0.25 → 0.42) confirms degradation toward performance: the state's stated justification (rational theology) increasingly diverges from its actual mechanism (suppressing dissent). This is a live mandatrophy candidate: the founding problem has been solved by the scholarly community's own lights (the created vs. uncreated debate continues as live scholarship), but the enforcement apparatus persists because the authority that instituted it benefits from enforced orthodoxy. The constraint will eventually be reversed (historical fact: the mihna ends in 234 AH under al-Mutawakkil, and the uncreated-Qur'an position is restored to official orthodoxy), but the structural analysis at the moment of peak enforcement (time_point 24–32) shows pure extraction dressed as theological correctness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationalism_as_cover_or_genuine_motivation,
    'Was the state''s adoption of the Mu''tazilite created-Qur''an doctrine driven primarily by genuine commitment to rational theology, or was rationalism adopted as an intellectual justification for what was fundamentally a political goal (consolidating state authority over the scholarly establishment)?',
    'Examine al-Ma''mun''s private correspondence, administrative records of the inquisition, and the temporal correlation between state theological pronouncements and political challenges to central authority. If theology shifted when political threats declined, rationalism was instrumental; if theology was defended even when politically costly, genuine commitment is supported.',
    'If rationalism was instrumental cover, the constraint is a pure snare with a sophisticated cosmetic narrative. If rationalism was genuinely held, it may be better classified as a tangled_rope (real coordination benefit plus asymmetric extraction). The measurement of theater_ratio bears directly on this question—if rationalism was cover, theater should be substantial and rising.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalism_as_cover_or_genuine_motivation, empirical, 'Whether state rationalism was genuine philosophical commitment or political instrumentality.').

omega_variable(
    suppression_structural_vs_internalized,
    'Was the measured suppression (0.88 at peak) primarily structural (external barriers: imprisonment, dismissal, legal penalties) or internalized (the target scholars began to doubt their own position or accepted the state''s frame)?',
    'Post-mihna trajectory analysis: after al-Mutawakkil ends the inquisition (234 AH), traditionalist scholars continue to affirm the uncreated-Qur''an doctrine with unbroken conviction. If their commitment persisted after the external suppression was lifted, suppression was predominantly structural; if traditionalists had partially internalized doubt, some would have remained ambivalent or defected to the created-Qur''an position.',
    'If structural, the constraint can be reversed by removing the enforcement apparatus—as it was. If partially internalized, the constraint has longer-term effects on scholarly confidence even after reversal. Internalization would raise the effective suppression above the structural measure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression persists after the enforcement apparatus is removed.').

omega_variable(
    kernel_vs_reading_theological_claim,
    'Does the kernel reference the metaphysical claim (the Qur''an''s ontological status as an abstract theological question) or does it reference the state-contested version of that claim (the Qur''an''s status as it becomes the subject of state enforcement)?',
    'Philosophical analysis: if the kernel is ''Qur''an''s ontological status'' understood as a timeless theological question, then all three readings (created, uncreated, state-enforced-created) are readings of the same kernel at different institutional moments. If the kernel is ''the state''s theological position'' or ''the contested theological status during the Abbasid inquisition,'' then this reading is the kernel itself and the others are counterfactual alternatives.',
    'If the first interpretation is correct, all three readings are valid siblings, and the engine should compute per-seat types for each, then aggregate to a family-level classification. If the second is correct, this reading is the actual historical constraint, and the uncreated and pure-created readings are counterfactual—not siblings but alternatives the kernel itself closed off. The distinction matters for how the network edges and family structure are authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_theological_claim, conceptual, 'Whether the kernel is the abstract theological claim or the state-contested version of it.').

omega_variable(
    beneficiary_contingency_and_reversal,
    'Does the Mu''tazilite school''s benefit persist after state backing is withdrawn, or does the school''s intellectual credibility collapse once the state enforcer is removed?',
    'Trace Mu''tazilite influence in post-mihna scholarship. If Mu''tazilite theology continues to attract serious scholars after al-Mutawakkil restores traditionalism to official status, the school''s benefit was genuine (not purely extraction-contingent). If the school rapidly becomes marginal after state support ends, its benefit was entirely contingent on state enforcement and extraction machinery.',
    'Contingent benefit supports the classification as snare (the Mu''tazilites are rent-collectors during enforcement, not genuine innovators). Persistent intellectual influence would suggest tangled_rope (real coordination benefit in rational theology exists, but extraction occurs through state coercion). The historical record shows Mu''tazilite ideas persist in Islamic philosophy for centuries, but formal institutional dominance is lost; this is the mixed case.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_contingency_and_reversal, empirical, 'Whether Mu''tazilite benefit is contingent on state enforcement or persists after enforcement ends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qura_tr_t8, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(qura_tr_t16, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(qura_tr_t24, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(qura_tr_t32, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(qura_tr_t40, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 50, 0.39).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qura_be_t8, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(qura_be_t16, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(qura_be_t24, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 24, 0.81).
narrative_ontology:measurement(qura_be_t32, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 32, 0.82).
narrative_ontology:measurement(qura_be_t40, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(qura_su_t8, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(qura_su_t16, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 16, 0.84).
narrative_ontology:measurement(qura_su_t24, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 24, 0.88).
narrative_ontology:measurement(qura_su_t32, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 32, 0.89).
narrative_ontology:measurement(qura_su_t40, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(qura_su_t50, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__state_enforced_creation_reading, 0.15).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (state_enforced_creation_reading) of the contested kernel quran_ontological_status. The kernel family includes: (1) quran_ontological_status__created_reading (pure theological claim without state enforcement), (2) quran_ontological_status__uncreated_reading (traditionalist theological claim, later restored after mihna ends), (3) this reading (state_enforced_creation). Each reading instantiates the same kernel (ontological status of the Qur'an) but differs in what institutional power structure defends it and what beneficiary/victim asymmetries arise. The three are linked via affects_constraints to enable the engine to trace how the same theological question becomes transformed under different political regimes. This reading instantiates the constraint AS it was instituted by Abbasid coercive power, not as a free intellectual choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
