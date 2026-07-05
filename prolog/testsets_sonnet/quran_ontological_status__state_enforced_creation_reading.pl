% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Mihna: State-Enforced Doctrine of the Created Qur'an
 *   domain: Islamic Theology / Philosophy of Language / Political Authority
 *
 * SUMMARY:
 *   This story instantiates one reading among three of a contested kernel
 *   about the ontological status of the Qur'an. The created-Qur'an position,
 *   held on purely theological grounds by Mu'tazilite scholars, is a separate
 *   constraint (created_reading, not authored here). What is authored here is
 *   the structurally distinct event in which the Abbasid caliphate (chiefly
 *   al-Ma'mun and his immediate successors) converted that theological claim
 *   into a compulsory loyalty test enforced through inquisitorial tribunals
 *   (mihna), with imprisonment, flogging, and career destruction for
 *   noncompliance. The doctrine's philosophical content is identical to the
 *   created_reading sibling; what differs entirely is the presence of state
 *   coercion, named victims, and an enforcement apparatus, which is why this
 *   reading computes as extractive where the pure theological claim does not.
 *
 * KEY AGENTS:
 *   - abbasid_caliphal_authority: agenda_setter (institutional/arbitrage) — administers and enforces the tribunal system
 *   - mutazilite_court_theologians: beneficiary (organized/mobile) — gains patronage and doctrinal power contingent on state backing
 *   - ahmad_ibn_hanbal: primary named victim (moderate/trapped) — imprisoned and flogged for refusal
 *   - traditionalist_hadith_scholars and literalist_communities: diffuse victims bearing the suppression
 *   - later_islamic_historiography: analytical observer recording the episode's eventual repudiation
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
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "Mihna: State-Enforced Doctrine of the Created Qur'an").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "Islamic Theology / Philosophy of Language / Political Authority").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'eb928998-6edb-4d72-9371-9e4e7abc7a4d').
narrative_ontology:cs_kernel_codification('eb928998-6edb-4d72-9371-9e4e7abc7a4d', distributed).
narrative_ontology:cs_authority_grounding('eb928998-6edb-4d72-9371-9e4e7abc7a4d', extraction).
narrative_ontology:cs_interpretation_layer_present('eb928998-6edb-4d72-9371-9e4e7abc7a4d').
narrative_ontology:cs_reading_relation('eb928998-6edb-4d72-9371-9e4e7abc7a4d', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('eb928998-6edb-4d72-9371-9e4e7abc7a4d', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('eb928998-6edb-4d72-9371-9e4e7abc7a4d', foundational, doctrinal_conformity_is_caliphal_prerogative).
narrative_ontology:cs_axiom_status(doctrinal_conformity_is_caliphal_prerogative, overridden).
narrative_ontology:cs_axiom_grounding('eb928998-6edb-4d72-9371-9e4e7abc7a4d', doctrinal_conformity_is_caliphal_prerogative, conventional).
narrative_ontology:cs_axiom('eb928998-6edb-4d72-9371-9e4e7abc7a4d', secondary, created_speech_theology_warrants_state_compulsion).
narrative_ontology:cs_axiom_status(created_speech_theology_warrants_state_compulsion, overridden).
narrative_ontology:cs_axiom_grounding('eb928998-6edb-4d72-9371-9e4e7abc7a4d', created_speech_theology_warrants_state_compulsion, instrumental).
narrative_ontology:cs_reference_frame('eb928998-6edb-4d72-9371-9e4e7abc7a4d', pre_mihna_scholarly_pluralism).
narrative_ontology:cs_drift_state('eb928998-6edb-4d72-9371-9e4e7abc7a4d', height_of_mihna_enforcement, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('eb928998-6edb-4d72-9371-9e4e7abc7a4d', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mutazilite_court_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_hadith_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, ahmad_ibn_hanbal).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, provincial_judges_and_officials).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, provincial_judges_and_officials).
narrative_ontology:constraint_vindicates(quran_ontological_status__state_enforced_creation_reading, mutazilite_rationalist_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Caliph (al-Ma'mun and successors) issues edicts requiring judges, scholars, and officials to publicly affirm that the Qur'an is created, then convenes tribunals (mihna) to test compliance. Non-compliant scholars are imprisoned, flogged, or removed from office. The doctrine gives the caliphate a theological lever over the scholarly class, who had previously been an independent check on political authority through control of religious interpretation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% Rationalist theologians whose doctrine of divine transcendence and created speech becomes state orthodoxy. They gain court patronage, official teaching positions, and the power to interrogate rivals in the tribunals. Their theological victory is real but entirely contingent on caliphal backing; when the state later reverses position under al-Mutawakkil, their institutional position collapses overnight.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mutazilite_court_theologians, beneficiary,
    organized, biographical, mobile, regional).

% Scholars committed to the doctrine that the Qur'an is uncreated face summons before tribunals, loss of judicial and teaching posts, imprisonment, and physical punishment for refusing to affirm the state doctrine. Their exit options are recantation (loss of standing and conscience), flight (loss of community and livelihood), or endurance of custodial abuse. Most lack the court connections to resist quietly the way some elite jurists do.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_hadith_scholars, payer,
    moderate, biographical, constrained, regional).

% The most prominent traditionalist jurist is imprisoned and flogged for repeatedly refusing to affirm the created-Qur'an doctrine before caliphal tribunals. He has no meaningful exit: recantation would destroy the authority his resistance itself generates, and flight is foreclosed by his prominence and the state's determination to make an example of him. His endurance becomes the founding legend of the doctrine's eventual reversal.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, ahmad_ibn_hanbal, payer,
    moderate, civilizational, trapped, regional).

% Ordinary believers and local prayer leaders who hold the uncreated-Qur'an position as basic piety, not sophisticated theology, are pressured through local officials and informal denunciation to conform outwardly. They have no access to the elite negotiating space available to prominent jurists and simply absorb the social and legal risk of nonconformity or dissemble their beliefs.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    powerless, generational, trapped, regional).

% The prior arrangement in which competing theological schools coexisted without state-enforced orthodoxy is displaced by a regime in which holding the wrong metaphysical position about the Qur'an is a prosecutable offense. This is not an actor but the casualty of converting a contested doctrinal question into a loyalty test administered by tribunal.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism_itself).

% Judges (qadis) required to affirm the doctrine as a condition of holding office face a forced choice between career and conviction; some comply readily and retain or gain position, others comply reluctantly under duress, and a minority resist and are removed. Their situation shows the mechanism's core function: converting theological assent into a filter for administrative loyalty.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, provincial_judges_and_officials, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, provincial_judges_and_officials, beneficiary).

% Subsequent generations of jurists and historians, largely from traditions that ultimately prevailed after the mihna's reversal, record and interpret the episode, generally as a cautionary account of state overreach into doctrine. Their retrospective framing shapes how the episode is remembered, but they were not present as adjudicators during the enforcement itself.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, later_islamic_historiography, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a residual coordination claim: a unified doctrinal position could, in principle, resolve theological disputes that were destabilizing court patronage networks and could align religious and political authority under a single interpretive standard. But this coordination function is thin — the disputed question does not require uniform resolution to function socially, unlike a genuine coordination problem such as a shared calendar or currency.
% TRANSFER_FUNCTION: Moves institutional standing, physical safety, and livelihood from traditionalist scholars, judges, and ordinary believers who hold or are suspected of holding the uncreated-Qur'an position, to the caliphal court (which gains a compliance-sorting mechanism over the scholarly class) and to Mu'tazilite theologians (who gain patronage, office, and the power to interrogate rivals).
% ABSENT_VOICES: The traditionalist scholarly consensus that would eventually reassert itself under al-Mutawakkil has no voice within the tribunal process itself — the mihna is designed precisely to exclude that position from legitimate expression, not merely to outvote it. Ordinary literalist believers have essentially no forum at all; their compliance or resistance is recorded only anecdotally.
% DISAPPEARANCE_RATIONALE: Had the mihna never been instituted, the theological dispute over the Qur'an's createdness would likely have remained a scholarly controversy without state enforcement machinery — judges would not have faced loyalty tests, Ibn Hanbal would not have become a martyr-figure whose endurance retroactively legitimated traditionalist theology, and the later consolidation of Sunni orthodoxy around the uncreated position (partly as a reaction against the mihna itself) would have taken a different shape or timeline.
% FOUNDING_PROBLEM: The caliphate sought a unifying doctrinal and political tool: aligning religious orthodoxy with rationalist theology that supported a transcendent, non-anthropomorphic conception of God consistent with philosophical and political centralization, while asserting caliphal authority over religious interpretation against an increasingly independent scholarly class.
% FOUNDING_PROBLEM_CORROBORATION: The mihna was formally abandoned by al-Mutawakkil within a generation, and mainstream Sunni theological and legal tradition — which was not among the doctrine's beneficiaries and in fact bore its costs — subsequently repudiated both the created-Qur'an doctrine and the inquisitorial mechanism, treating the episode as a cautionary precedent against state doctrinal coercion. No enduring institution outside the transient Mu'tazilite court faction attests the founding problem as ongoing or the mechanism as justified.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises sharply from a moderate baseline (0.35) to 0.81 as the tribunal apparatus is built out and applied systematically against named scholars — this is not a static theological dispute but an accelerating enforcement campaign. Suppression peaks even higher (0.90) at the height of the mihna under al-Mu'tasim/al-Wathiq before declining as the policy loses political sustainability and is finally abandoned under al-Mutawakkil (suppression falls to 0.60 by interval end as enforcement is being wound down but has not yet fully reversed). Theater ratio rises through the middle period (0.55-0.60) as public affirmation ceremonies become increasingly performative rituals of loyalty divorced from genuine theological persuasion — the tribunals are demonstrably not convincing anyone, they are sorting the compliant from the defiant.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal authority sits at the far beneficiary end: it acquires a durable instrument for subordinating an independent scholarly class to political control, with the metaphysical content of the doctrine almost incidental to its enforcement function. Mu'tazilite theologians benefit conditionally and temporarily — their d is low but not zero, since their gains are entirely dependent on caliphal favor and evaporate the moment political winds shift, which the mid-9th-century reversal confirms. Traditionalist scholars, and especially Ibn Hanbal, sit at the target end: trapped exit options, direct physical and institutional costs, and no meaningful alternative once summoned before a tribunal. Literalist communities and the abstract casualty of scholarly pluralism sit similarly at the target end but diffusely, since the costs are distributed across many powerless individuals rather than concentrated on named victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (aligning religious orthodoxy with a rationalist theology serving caliphal centralization) was real from the caliphate's perspective but was never a problem shared by the traditionalist scholarly class or ordinary believers — it was a political founding problem dressed as a theological one. Its status is dead: the mihna was abandoned within roughly two decades and subsequent orthodoxy repudiated both the doctrine and the coercive mechanism. Classifying this as snare rather than tangled_rope reflects that no durable coordination function survived independent of the coercion — once state backing was withdrawn, the arrangement collapsed entirely rather than persisting as degraded coordination, which also distinguishes it from a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_vs_instrument_separability,
    'Is the created-Qur''an doctrine itself extractive, or is the doctrine theologically neutral and the extraction wholly attributable to the state''s decision to enforce it coercively?',
    'Compare this reading''s metrics against the sibling created_reading story (same doctrinal content, no state enforcement). If created_reading measures as near-mountain/rope while this reading measures as snare, the extraction is attributable entirely to the enforcement layer, not the doctrine.',
    'If extraction is wholly attributable to enforcement, this confirms the ε-invariance decomposition was correct to split the doctrinal claim from the enforcement event into separate constraints rather than treating ''the created-Qur''an controversy'' as one constraint with a single ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_instrument_separability, conceptual, 'Whether extraction belongs to the theological claim or to its state enforcement.').

omega_variable(
    mutazilite_sincerity_vs_capture,
    'Did Mu''tazilite theologians who cooperated with the mihna genuinely believe state enforcement was theologically warranted, or did they instrumentally use caliphal power to win an argument they could not win through scholarly persuasion alone?',
    'Examine contemporaneous Mu''tazilite writings for explicit endorsement of coercive tribunal methods versus writings that focus purely on doctrinal argument; look for recorded discomfort among rationalist theologians with the tribunal''s methods.',
    'If sincere belief in coercion''s legitimacy, the beneficiary group is a more willing co-architect of the snare; if instrumental capture, the beneficiary role is closer to an opportunistic rider on a caliphal political project it did not design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mutazilite_sincerity_vs_capture, empirical, 'Whether rationalist theologians were architects or opportunistic beneficiaries of the coercive mechanism.').

omega_variable(
    kernel_framing_alternative,
    'Could this constraint alternatively be framed as a pure political-control mechanism that merely borrowed theological vocabulary, rather than as a reading of the Qur''an-ontology kernel at all?',
    'Assess whether the caliphate would have pursued equivalent loyalty-testing machinery absent any live theological controversy to weaponize — i.e., whether the doctrinal content was load-bearing or replaceable with any other available wedge issue.',
    'If the theological content was substantially replaceable, this story is better modeled primarily as a political-control constraint with a thin theological veneer, which would change how much weight the kernel linkage (versus an independent political-authority constraint) should carry in classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Alternative framing: political control mechanism using theology as pretext, versus a genuine reading of the ontology kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t4, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(qura_tr_t8, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(qura_tr_t12, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(qura_tr_t16, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 16, 0.6).
narrative_ontology:measurement(qura_tr_t20, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t4, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(qura_be_t8, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 8, 0.74).
narrative_ontology:measurement(qura_be_t12, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(qura_be_t16, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 16, 0.79).
narrative_ontology:measurement(qura_be_t20, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 20, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t4, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(qura_su_t8, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 8, 0.88).
narrative_ontology:measurement(qura_su_t12, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(qura_su_t16, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 16, 0.83).
narrative_ontology:measurement(qura_su_t20, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__state_enforced_creation_reading, 0.1).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, uncreated_reading).

% DUAL FORMULATION NOTE:
% This constraint is the state-enforcement instantiation of the quran_ontological_status kernel's created reading. It shares the created_reading sibling's metaphysical content but diverges completely in structural properties (enforcement, named victims, extractiveness) because it adds a coercive political apparatus absent from the pure theological claim. It stands in logical opposition to uncreated_reading, the doctrine the mihna's victims held and were punished for holding. All three readings are linked via network edges rather than merged into one constraint, per the ε-invariance principle: their extraction profiles differ by a wide margin and must not be averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
