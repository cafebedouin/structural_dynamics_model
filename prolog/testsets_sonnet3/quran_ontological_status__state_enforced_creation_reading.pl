% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: State-Enforced Mu'tazilite Doctrine of the Created Qur'an (Mihna)
 *   domain: Islamic Theology / Philosophy of Language / Political Authority
 *
 * SUMMARY:
 *   Under Caliph al-Ma'mun (r. 813-833) and his two immediate successors, the
 *   Abbasid state adopted Mu'tazilite theology's doctrine that the Qur'an is
 *   created (makhluq) as official orthodoxy and enforced it through the
 *   mihna: an inquisition apparatus that interrogated judges, scholars, and
 *   officials, demanding public affirmation of createdness on pain of loss of
 *   office, imprisonment, or corporal punishment. This story is NOT about
 *   whether the Qur'an is created or uncreated as a theological matter (those
 *   are the sibling readings) — it is about the specific historical
 *   constraint formed when a state apparatus converted that metaphysical
 *   dispute into a loyalty test backed by coercive machinery. The doctrine
 *   itself supplied the content of the test; the mihna supplied the
 *   extraction mechanism.
 *
 * KEY AGENTS:
 *   - abbasid_caliphal_authority: agenda-setter, converts doctrine into political control mechanism
 *   - mutazilite_court_theologians: temporary beneficiary, gains state patronage contingent on caliphal favor
 *   - ahmad_ibn_hanbal: primary named victim, imprisoned and flogged for refusal
 *   - traditionalist_hadith_scholars and literalist_lay_communities: diffuse victim class bearing purges and surveillance
 *   - later_sunni_traditionalist_establishment: analytical observer writing the post-hoc historical record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.81).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.9).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Mu'tazilite Doctrine of the Created Qur'an (Mihna)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "Islamic Theology / Philosophy of Language / Political Authority").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '1409e6fe-9ab2-4428-8302-4ae783229705').
narrative_ontology:cs_kernel_codification('1409e6fe-9ab2-4428-8302-4ae783229705', distributed).
narrative_ontology:cs_authority_grounding('1409e6fe-9ab2-4428-8302-4ae783229705', extraction).
narrative_ontology:cs_interpretation_layer_present('1409e6fe-9ab2-4428-8302-4ae783229705').
narrative_ontology:cs_reading_relation('1409e6fe-9ab2-4428-8302-4ae783229705', quran_ontological_status__uncreated_reading, coexists_with).
narrative_ontology:cs_reading_relation('1409e6fe-9ab2-4428-8302-4ae783229705', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('1409e6fe-9ab2-4428-8302-4ae783229705', foundational, state_may_adjudicate_metaphysical_truth_by_coercion).
narrative_ontology:cs_axiom_status(state_may_adjudicate_metaphysical_truth_by_coercion, overridden).
narrative_ontology:cs_axiom_grounding('1409e6fe-9ab2-4428-8302-4ae783229705', state_may_adjudicate_metaphysical_truth_by_coercion, instrumental).
narrative_ontology:cs_axiom('1409e6fe-9ab2-4428-8302-4ae783229705', secondary, public_doctrinal_affirmation_is_legitimate_state_interest).
narrative_ontology:cs_axiom_status(public_doctrinal_affirmation_is_legitimate_state_interest, overridden).
narrative_ontology:cs_axiom_grounding('1409e6fe-9ab2-4428-8302-4ae783229705', public_doctrinal_affirmation_is_legitimate_state_interest, conventional).
narrative_ontology:cs_reference_frame('1409e6fe-9ab2-4428-8302-4ae783229705', pre_mihna_scholarly_pluralism).
narrative_ontology:cs_drift_state('1409e6fe-9ab2-4428-8302-4ae783229705', al_mamun_mihna_declaration, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1409e6fe-9ab2-4428-8302-4ae783229705', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mutazilite_court_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_hadith_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, ahmad_ibn_hanbal).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_lay_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, provincial_judges_and_examiners).
narrative_ontology:constraint_vindicates(quran_ontological_status__state_enforced_creation_reading, quran_makhluq_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The caliph (al-Ma'mun and successors) declares the createdness of the Qur'an official doctrine and orders provincial judges to interrogate scholars and officials, demanding public affirmation under threat of imprisonment, flogging, or loss of office. The doctrine functions as a loyalty test that consolidates the caliph's claim to religious as well as political authority, subordinating the scholarly class (ulama) to the state.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Rationalist theologians whose doctrine (that an eternal, uncreated Qur'an implies a second eternal being alongside God) gains state backing and institutional prestige as the empire's official theology. They serve as examiners in the mihna tribunals. Their gain is contingent on caliphal favor; when the policy is later reversed under al-Mutawakkil, their institutional position collapses along with it.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mutazilite_court_theologians, beneficiary,
    organized, biographical, constrained, continental).

% Scholars committed to the doctrine that the Qur'an is uncreated speech of God, refusing on theological grounds to affirm createdness even under duress. They are hauled before tribunals, stripped of judicial and teaching posts, imprisoned, and in the most severe cases tortured. Their only 'exit' is public recantation, which most refuse; there is no geographic or institutional escape from an empire-wide inquisition.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_hadith_scholars, payer,
    moderate, biographical, trapped, regional).

% The most prominent traditionalist scholar, repeatedly interrogated, imprisoned for roughly two years, and flogged for refusing to affirm the Qur'an's createdness. His endurance becomes a rallying point for traditionalist resistance, but at the time of the mihna he has no institutional leverage against the state apparatus arrayed against him.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, ahmad_ibn_hanbal, payer,
    powerless, biographical, trapped, regional).

% Ordinary believers and lower-tier religious functionaries who hold the traditional doctrine but have no scholarly standing to resist publicly. They experience the mihna as an atmosphere of surveillance and coerced conformity, losing local imams and teachers to purges, with no realistic avenue to contest imperial theological policy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_lay_communities, payer,
    powerless, biographical, trapped, regional).

% The practice of open theological dispute among competing schools (Mu'tazilite, traditionalist, and others) without state-imposed orthodoxy. Listed for completeness as a non-agent casualty: the precedent of using state coercion to settle a metaphysical dispute damages the space for future doctrinal disagreement regardless of which side eventually wins.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself, payer,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself).

% Local qadis tasked with administering the loyalty tests. They implement caliphal policy against scholars in their own communities, sometimes reluctantly, and themselves face removal from office if judged insufficiently rigorous or if the policy later reverses and they are seen as having been complicit persecutors.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, provincial_judges_and_examiners, agenda_setter,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, provincial_judges_and_examiners, payer).

% The scholarly consensus that crystallizes after the mihna's reversal, treating Ibn Hanbal's endurance as vindication and the episode as a cautionary precedent against state involvement in doctrinal adjudication. Writes the historical record from outside the original contest.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, later_sunni_traditionalist_establishment, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Superficially, the doctrine coordinates a unified imperial theology, resolving ambiguity about the Qur'an's ontological status so that legal and doctrinal rulings proceed from a single settled premise across a vast, religiously diverse empire.
% TRANSFER_FUNCTION: Moves institutional position, physical safety, and doctrinal authority from traditionalist scholars and lay literalist communities to the caliph (who gains a mechanism of political-religious control) and to Mu'tazilite theologians (who gain state patronage and examiner status), via tribunals that convert a metaphysical claim into a compliance test backed by imprisonment and corporal punishment.
% ABSENT_VOICES: Scholars who held nuanced or agnostic positions between strict createdness and strict eternality were given no room in a binary loyalty test; ordinary believers subject to local purges had no venue to be heard at all — the tribunal structure itself excluded any voice that was not caliph, examiner, or accused.
% DISAPPEARANCE_RATIONALE: If the state enforcement mechanism vanished, the theological dispute over the Qur'an's createdness would remain a live, unresolved question debated among schools as it had been before al-Ma'mun's decree and as it continued to be argued after the mihna's reversal — but the imprisonments, floggings, purges of judges and teachers, and the chilling effect on traditionalist scholarship would not have occurred. The coercive apparatus, not the theological question, is what rearranges the world.
% FOUNDING_PROBLEM: The stated problem was doctrinal incoherence at the top of an empire spanning many theological factions, and the caliph's asserted need to anchor legal and religious authority in a single, state-sanctioned metaphysical position rather than leaving it to competing schools.
% FOUNDING_PROBLEM_CORROBORATION: Later Sunni traditionalist historiography (outside the Mu'tazilite and caliphal parties that benefited) attests that the underlying problem was never genuinely one of empire-wide incoherence — competing schools had coexisted for generations without state coercion — and that the mihna's actual function was political consolidation under al-Ma'mun and his immediate successors; the policy's abrupt reversal under al-Mutawakkil, with no comparable social rupture, is cited by this outside tradition as evidence the 'problem' was never load-bearing to begin with.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness and suppression rise sharply from a low baseline (0.35/0.30 at T0, before the mihna was declared policy) to peaks around T8-T12 (0.72-0.81 extraction, 0.85-0.90 suppression) during the most active persecution years under al-Ma'mun and al-Mu'tasim, then collapse rapidly by T20 when al-Mutawakkil reverses the policy (0.40 extraction, 0.15 suppression) — the abruptness of that collapse is itself evidence the policy's true load-bearing function was political control rather than doctrinal necessity, since a genuine settlement of theological incoherence would not simply vanish by decree. Theater ratio climbs alongside suppression (0.20 to 0.62) because as the tribunals continue for years, an increasing share of the interrogation apparatus is directed at officials and minor scholars whose actual theological commitments were never in doubt — the affirmations extracted become ritual compliance rather than genuine doctrinal settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the caliphal seat, the arrangement reads as legitimate doctrinal consolidation backed by scholarly rationalist argument (the Mu'tazilite kalam case against an eternal, uncreated co-existent with God). From the traditionalist payer seats, the identical structure reads as coercive suppression of a theological position with equally serious scriptural and traditional grounding. The engine's per-seat computation is expected to diverge sharply here: agenda_setter and beneficiary seats likely compute toward rope/tangled_rope framing (doctrine-as-coordination), payer seats toward snare (doctrine-as-cover-for-extraction) — this divergence is the analytical point of the story, not an inconsistency to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal authority sits at the pure-beneficiary end: it authored the enforcement mechanism, controls its application, and gains a durable tool for asserting authority over the religious establishment — arbitrage-grade exit because the caliph can end the policy unilaterally, as al-Mutawakkil does. Mu'tazilite theologians sit closer to beneficiary but with constrained exit: their gain is real but wholly contingent on continued caliphal favor, which evaporates instantly at reversal. Traditionalist scholars and Ibn Hanbal specifically sit at the full-target end: trapped exit, no institutional lever, direct physical and professional cost. Lay literalist communities are also targets but diffuse and powerless, bearing the ambient cost of purges without individual notice. Provincial judges are a genuinely dual seat — agenda-setters at the point of enforcement but themselves payers under later reversal or caliphal displeasure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (asserted doctrinal incoherence requiring state resolution) is marked dead specifically because the mihna's abrupt reversal under al-Mutawakkil, without comparable social rupture, demonstrates the empire could and did function with theological plurality both before and after the inquisition. This prevents mislabeling the episode as necessary coordination that simply expired — the founding-problem/disappearance-verdict mismatch (status=dead, verdict=world_rearranges only with respect to the coercive apparatus, not the theology) is exactly the capture-flag signature the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_versus_apparatus_separability,
    'Is the extraction measured here intrinsic to the createdness doctrine itself, or entirely an artifact of the specific historical decision to enforce it via state tribunal — such that an identical doctrine adopted by voluntary scholarly consensus would carry near-zero ε?',
    'Compare this reading''s structure to periods or regions where Mu''tazilite theology held scholarly influence without state coercion (e.g., its earlier standing as one school among several prior to al-Ma''mun''s decree) — if extraction and suppression metrics there are near zero, the separability is confirmed and the high ε in this story is entirely attributable to the enforcement layer.',
    'Confirms the ε-invariance decomposition into three sibling stories is correct rather than artificial — the doctrine and the apparatus are genuinely different structural objects, not two measurements of one object.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_versus_apparatus_separability, conceptual, 'Whether extraction belongs to the doctrine or to its state enforcement.').

omega_variable(
    genuine_versus_pretextual_theological_motivation,
    'Did al-Ma''mun and the Mu''tazilite establishment genuinely believe the createdness doctrine was theologically necessary and enforcement merely an unfortunate but sincere means of establishing truth, or was the doctrine selected substantially because it was useful for asserting caliphal authority over an increasingly independent traditionalist scholarly class?',
    'Close reading of al-Ma''mun''s own letters ordering the mihna alongside independent (non-participant) chronicles from the period, weighing stated theological rationale against timing relative to caliphal succession disputes and the growing institutional independence of hadith-based scholars.',
    'If substantially pretextual, this strengthens the snare classification and the founding_problem_status of dead; if substantially sincere, the tangled_rope framing (genuine coordination attempt with asymmetric costs) becomes more defensible even though the human costs to traditionalist scholars remain unchanged either way.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_versus_pretextual_theological_motivation, empirical, 'Sincerity versus instrumentality of the caliph''s theological motivation.').

omega_variable(
    kernel_framing_alternative_locus,
    'Is the more defensible framing of this constraint ''the state''s enforcement of a theological doctrine'' (as authored here) or ''the caliphate''s general assertion of authority over religious interpretation, for which createdness was merely the vehicle at this moment''? Under the second framing, the relevant kernel might be caliphal-versus-scholarly religious authority rather than the Qur''an''s ontological status specifically.',
    'Compare against other Abbasid-era doctrinal impositions (if any) that used different theological content but the same coercive apparatus and beneficiary structure — if the apparatus recurs with interchangeable doctrinal content, the kernel is more properly located at the authority-contest level, not the Qur''an-ontology level.',
    'If the alternative framing is adopted, this story''s classification would likely remain snare (the mechanism doesn''t change), but its network linkage would shift toward a caliphal-authority kernel family rather than the quran_ontological_status kernel family, changing which sibling stories it should link to via affects_constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_locus, conceptual, 'Whether the kernel is best located at the doctrine level or the authority-contest level; documents the CS-framing under-determination per the framing-omega guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t4, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(qura_tr_t8, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 8, 0.58).
narrative_ontology:measurement(qura_tr_t12, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 12, 0.6).
narrative_ontology:measurement(qura_tr_t16, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 16, 0.62).
narrative_ontology:measurement(qura_tr_t20, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t4, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(qura_be_t8, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(qura_be_t12, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(qura_be_t16, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 16, 0.79).
narrative_ontology:measurement(qura_be_t20, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qura_su_t4, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(qura_su_t8, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(qura_su_t12, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(qura_su_t16, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(qura_su_t20, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, uncreated_reading).

% DUAL FORMULATION NOTE:
% Part of the quran_ontological_status kernel family (3 readings). created_reading and uncreated_reading are the theological positions considered on their own merits as live scholarly disputes (expected near-zero to low extraction, rope-like or mountain-adjacent depending on how naturalized each side's certainty is). This story, state_enforced_creation_reading, is the historically specific instantiation where one side of that dispute was fused with state coercive power via the mihna tribunals (high extraction, snare). The three stories share a kernel — what the Qur'an's ontological status is — but instantiate structurally distinct constraints with different ε, different beneficiary/victim structure, and different classification. Linked via affects_constraints in both directions; the state-enforced reading exerts downstream pressure on the legitimacy conditions of created_reading long after the mihna ends, since the doctrine remains associated with persecution in later Sunni memory regardless of its independent theological merits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
