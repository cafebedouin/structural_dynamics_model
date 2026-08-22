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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: State-Enforced Qur'anic Creationism via Mihna Inquisition
 *   domain: theological/political
 *
 * SUMMARY:
 *   During the Abbasid caliphate (especially under al-Ma'mun, r. 813–833 CE),
 *   the doctrine of the created Qur'an—the Mu'tazilite rationalist claim that
 *   the Qur'an is divine speech (kalām Allāh) but created/temporally bounded
 *   rather than eternal—was adopted as official state dogma and enforced
 *   through the mihna (inquisition). Traditionalist scholars, particularly
 *   Ahmad ibn Hanbal and his students, rejected this doctrine, holding that
 *   the Qur'an is uncreated (qadīm), coeternal with God's essence. The state
 *   employed tribunals, imprisonment, torture, and public interrogation to
 *   extract affirmation of the created-Qur'an doctrine from scholars. Those
 *   who resisted faced systematic suppression. The constraint described here
 *   is NOT the theological claim alone (that would be a separate
 *   rope/philosophy story) but the STATE-ENFORCED version: a snare converting
 *   metaphysical doctrine into a tool for political loyalty testing and
 *   scholarly purge. This reading instantiates one side of the contested
 *   kernel 'Qur'anic ontological status'—the reading where the state's
 *   enforcement machinery transforms theological dispute into extraction.
 *
 * KEY AGENTS:
 *   - caliphal_authority: Sets doctrine as policy, conducts mihna, extracts compliance affirmation. Power: institutional. Exit: none (the state apparatus that enforces policy).
 *   - mu_tazilite_rationalist_school: Intellectually aligned with created-Qur'an doctrine; temporarily benefits from state backing and institutional patronage. Power: organized. Exit: constrained (they can advocate alternative theology if state backing shifts, but they are institutionally dependent on caliphal favor).
 *   - traditionalist_scholars_ahl_hadith: Resist the created-Qur'an doctrine on textual and theological grounds; subjected to mihna tribunals, imprisonment, torture. Power: moderate (respected but politically outgunned). Exit: identity-locked (recanting violates doctrinal integrity and community identity) and trapped (exit = public apostasy + loss of scholarly authority).
 *   - literalist_communities: Ordinary believers committed to traditionalist interpretation; face pressure through suppression of their scholarly leaders and de facto prohibition on uncreated-Qur'an teaching. Power: powerless. Exit: constrained (cannot change community doctrine without losing membership; can emigrate but that destroys social position).
 *   - scholarly_pluralism_itself: The metasystem where multiple valid interpretations of the same textual tradition coexist without one being statally enforced. Power: analytical (not an agent but a structural principle). Exit: analytical (the constraint's persistence depends on suppressing this principle).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.82).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.91).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Qur'anic Creationism via Mihna Inquisition").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "theological/political").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '92164082-96d3-41a3-81c4-e94797a7a66e').
narrative_ontology:cs_kernel_codification('92164082-96d3-41a3-81c4-e94797a7a66e', formalized).
narrative_ontology:cs_authority_grounding('92164082-96d3-41a3-81c4-e94797a7a66e', extraction).
narrative_ontology:cs_interpretation_layer_present('92164082-96d3-41a3-81c4-e94797a7a66e').
narrative_ontology:cs_reading_relation('92164082-96d3-41a3-81c4-e94797a7a66e', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('92164082-96d3-41a3-81c4-e94797a7a66e', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_axiom('92164082-96d3-41a3-81c4-e94797a7a66e', foundational, quranic_ontology_statally_determined).
narrative_ontology:cs_axiom_status(quranic_ontology_statally_determined, holdable).
narrative_ontology:cs_axiom_grounding('92164082-96d3-41a3-81c4-e94797a7a66e', quranic_ontology_statally_determined, deontological).
narrative_ontology:cs_axiom('92164082-96d3-41a3-81c4-e94797a7a66e', foundational, rationalist_metaphysics_doctrinal_necessity).
narrative_ontology:cs_axiom_status(rationalist_metaphysics_doctrinal_necessity, overridden).
narrative_ontology:cs_axiom_grounding('92164082-96d3-41a3-81c4-e94797a7a66e', rationalist_metaphysics_doctrinal_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('92164082-96d3-41a3-81c4-e94797a7a66e', state_monopoly_on_doctrinal_authority).
narrative_ontology:cs_drift_state('92164082-96d3-41a3-81c4-e94797a7a66e', post_mutawakkil_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('92164082-96d3-41a3-81c4-e94797a7a66e', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, rationalist_school_mu_tazilites).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars_ahl_hadith).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_rationalist_school).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the mihna as official doctrine and enforces it through tribunals, imprisonment, and torture. Uses the created-Qur'an doctrine as a loyalty test—public affirmation of the doctrine signals political compliance with caliphal authority. Maintains enforcement machinery (judges, interrogators, prison apparatus) specifically to extract doctrinal compliance. Benefits directly from consolidated theological authority and scholarly suppression of competitors. Exit: none (the state is the enforcement apparatus itself).
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Intellectually aligned with the created-Qur'an doctrine and receives institutional patronage, court positions, and validation through state backing. Their doctrine becomes official; their scholars gain prestige and resources. Benefit from state suppression of traditionalist competitors. Exit: constrained—they can change doctrinal positions if state support reverses, but institutional identity and career advancement are locked into rationalist advocacy. If the caliphate shifts backing to traditionalism (as historically occurred under al-Mutawakkil), the school faces institutional collapse.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_rationalist_school, beneficiary,
    organized, biographical, constrained, national).

% Resist the created-Qur'an doctrine on theological and textual grounds, holding the Qur'an to be uncreated (qadīm). Subjected to mihna tribunals where they are interrogated, pressured to recant, and if they resist, imprisoned and tortured. Ahmad ibn Hanbal is imprisoned for years, whipped, and forced into hiding despite his age and prominence. The constraint forces a choice: public false affirmation (which destroys scholarly credibility and violates doctrinal integrity) or imprisonment/torture (which extracts suffering and eliminates their teaching authority). Exit: identity-locked—affirming the created-doctrine would constitute apostasy from their self-concept as Qur'an-preservers and literal-text scholars. Scholarly identity is constituted through the uncreated doctrine; recanting is categorically unacceptable.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars_ahl_hadith, payer,
    moderate, biographical, identity_locked, national).

% Ordinary believers and students committed to traditionalist, literalist interpretation of the Qur'an. Face suppression through: (1) prohibition on their teachers' public ministry (traditionalist scholars are imprisoned or hiding); (2) state-sponsored teaching of the created-doctrine in schools and mosques; (3) social pressure to affirm the state doctrine or risk being identified with suppressed traditionalists. Cannot freely study under their preferred teachers. Exit: constrained—they can theoretically adopt the rationalist doctrine if pressured, but doing so requires abandoning their community identity and their preferred interpretation. Emigration is theoretically available but practically costly (loss of family, livelihood, social standing).
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    powerless, biographical, constrained, national).

% The metaprinciple that multiple valid interpretive schools can coexist within a single tradition without one being statally enforced. Scholarly pluralism would hold that the created-doctrine and the uncreated doctrine are both legitimate theological positions, each supported by textual tradition and rational argumentation, and that scholars should be free to pursue either without state coercion. The mihna directly suppresses this principle by declaring one doctrine mandatory and using state power to eliminate the other. Not an agent (cannot negotiate or exit), but a structural principle whose elimination is the constraint's function.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, excluded,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

% Later caliphs (al-Mutawakkil, r. 847–861) who shifted state backing from the rationalist doctrine back to traditionalism, ending the mihna. They observed the constraint's operation under their predecessors and chose to reverse it, discovering that the traditionalist doctrine was sustainable without state enforcement (it had survived underground) while the rationalist doctrine collapsed when state patronage ended. Their reversal revealed that the mihna was not solving a stable coordination problem but imposing a contingent political choice.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, subsequent_caliphate, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stated function: establishing unified Islamic doctrinal orthodoxy by declaring the created-Qur'an doctrine as correct and eliminating competing interpretations through scholarly examination and loyalty testing. The actual function (post-reversal): leveraging doctrinal dispute as a pretext for consolidating caliphal authority over theological interpretation and suppressing scholarly independence.
% TRANSFER_FUNCTION: Transfers scholarly authority, institutional legitimacy, and doctrinal control from traditionalist schools to the caliphal authority (and secondarily to the Mu'tazilite rationalist school as the state's favored interpretive apparatus). Moves suffering, loss of livelihood, and forced recantations from the state apparatus to traditionalist scholars and literalist communities. Moves prestige and court position from traditionalists to rationalists.
% ABSENT_VOICES: Regionalist and local scholarly communities not centered in the caliphal court; non-elite believers whose theological views are never solicited; women scholars and traditionalists whose perspectives are systematized out of the formal mihna record; scholars in peripheral provinces who resist the mihna but whose resistance is not recorded in caliphal sources; future generations of traditionalists (post-al-Mutawakkil) who would affirm that the constraint was unjust and unsustainable.
% DISAPPEARANCE_RATIONALE: If the mihna and its enforcement apparatus disappeared overnight, traditionalist scholars would immediately resume teaching and preaching (as they did when state backing shifted under al-Mutawakkil); literalist communities would return to their preferred interpretations; the rationalist school would lose institutional patronage and institutional authority, though individual rationalist scholars would continue their intellectual work. The scholarly landscape would reorganize around the multiple schools as it had before the mihna—no single doctrine would be state-enforced. The caliphate would lose a tool of doctrinal control and scholarly suppression. The constraint's disappearance would trigger rapid structural rearrangement because it was not solving an underlying coordination problem; it was imposing a contingent political choice.
% FOUNDING_PROBLEM: Early Abbasid period faced genuine theological diversity across multiple scholarly schools (Mu'tazilite rationalists, Traditionalist literalists, early Ash'arite moderates). The caliphate sought unified doctrinal authority to prevent theological fragmentation, sectarian conflict, and challenges to caliphal legitimacy grounded in doctrinal interpretation. The problem: which doctrine should unify Islam, and who decides?
% FOUNDING_PROBLEM_CORROBORATION: Historians (al-Tabari, later Sunni historians) attest that after the mihna was abandoned under al-Mutawakkil, theological diversity persisted and was eventually resolved through institutional development of jurisprudential schools (Hanbali, Maliki, Shafi'i, Hanafi for Sunnis; Twelver Shi'ism for Shi'a), NOT through state doctrinal monopoly. The problem of theological unity was solved by allowing multiple schools to coexist with formal recognition. This constitutes external corroboration that the founding problem was addressed through means OTHER than the state-enforced creation doctrine, and that the founding problem had become moot by the time the mihna's institutional persistence ceased (9th–10th centuries). Non-benefiting historians confirm: the mihna did not achieve lasting doctrinal unification; it merely suppressed traditionalism temporarily. Once state backing reversed, traditionalism resurged and became the majority position, making the state's creation-doctrine enforcement not a solution but a historical interregnum.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness reaches 0.82 because the constraint transfers scholarly legitimacy, institutional patronage, and doctrinal authority from traditionalists to the rationalist school via state coercion—not through persuasion but through tribunals, torture, and public humiliation. The temporal series (0.65 → 0.82) models the gradual intensification of the mihna apparatus: early enforcement relies on voluntary affirmation and soft pressure; by the end of the interval, torture and imprisonment are routine. Suppression is extremely high (0.91) because the constraint's persistence depends entirely on active enforcement: without state machinery, the traditionalist reading would naturally survive (Ahmad ibn Hanbal's students preserved and transmitted his doctrine despite imprisonment). Theater is low-to-moderate (0.28): the state describes the mihna as doctrinal clarification and orthodoxy-protection, but this narrative covers straightforward political loyalty testing. The theater ratio rises over time as enforcement must resort to more elaborate justifications as resistance hardens. Accessibility of alternatives collapses substantially (0.76): once the mihna is instituted, traditionalist scholars cannot publicly teach or debate without triggering interrogation and torture; the only accessible 'alternative' is recantation. Resistance is substantial (0.73): Ahmad ibn Hanbal and his immediate circle actively resist across decades; their resistance is never fully suppressed, merely contained by state force.
 *
 * PERSPECTIVAL GAP:
 *   From the caliphal authority's seat, the constraint appears to be clarification of Islamic orthodoxy and protection of doctrinal integrity—a legitimate coordination function. From the traditionalist scholar's seat, the same structure is an inquisition designed to purge scholarly competition and extract false affirmations under torture. The engine computes these divergent classifications from the structural data: the caliphal seat (beneficiary, mobile exit) computes to snare-with-beneficiary-extraction; the traditionalist seat (victim, identity-locked exit) computes to pure snare. The perspectival gap IS the constraint—it is only a snare from the target seats' perspective; from the beneficiary seat it is a legitimate state function. This asymmetry is precisely what makes it a snare rather than a rope: ropes are experienced as mutually beneficial by participants; snares are experienced as extractive by victims and justified by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority sits at d ≈ 0.0 (full beneficiary): it extracts doctrinal control, political loyalty affirmation, and institutional power consolidation from the constraint. The rationalist school sits at d ≈ 0.15 (beneficiary with modest vulnerability): they gain institutional patronage but are structurally dependent on caliphal favor and could lose everything if the state shifts; their exit options are 'constrained' (they can theoretically change positions if state backing reverses). Traditionalist scholars sit at d ≈ 0.95 (full target): they bear imprisonment, torture, and public humiliation; their exit options are identity-locked (affirming the false doctrine is existentially unacceptable given their self-concept as literal-text preservers). Literalist communities sit at d ≈ 0.88 (target with diffuse pain): they experience suppression of their teachers, prohibition on their preferred interpretation, and social pressure to conform; exit is constrained-to-trapped (leaving means leaving the faith community or geographic region). Scholarly pluralism sits at d ≈ 1.0 (pure target, though it is not an agent): the constraint's entire function is to eliminate it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'Islamic theological unity and doctrinal clarity in an era of competing scholarly schools.' That problem is LIVE during the Abbasid period (multiple interpretations genuinely competed for institutional legitimacy), but the state-enforced creation doctrine does NOT solve it in a sustainable way. The mihna succeeded briefly in suppressing traditionalist schools, but after al-Ma'mun's successors withdrew state backing (under al-Mutawakkil, r. 847–861 CE), the traditionalist doctrine resurged and became the majority legal school (Hanbali jurisprudence) across Sunni Islam. The state's doctrinal monopoly collapsed because it was NOT solving the underlying coordination problem—it was imposing a particular school's victory through force. Thus: founding_problem_status = 'dead_but_constraint_persists'—the problem (theological uncertainty) was addressed through institutional development of jurisprudential schools, not through the mihna. The mandate for state-enforced orthodoxy outlived the problem it supposedly solved, becoming pure extraction. The constraint exhibits mandatrophy: its stated function (doctrinal clarity) ceased being its actual function (maintaining caliphal power through doctrinal control); after state backing was removed, the constraint simply ceased to operate, revealing that it was inertial, not functionally necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the constraint ''the created-Qur''an doctrine'' or ''the state''s enforcement machinery (mihna) converting that doctrine into a suppression mechanism''? Does the distinction mark a single constraint or two?',
    'Structural analysis: if removal of the state enforcement leaves the theological claim alive (held by rationalist scholars without coercion), they are separable constraints; if the theological claim cannot survive without enforcement, they are inseparable and the constraint is inherently political.',
    'If separable: the created-Qur''an doctrine is a rope (genuine philosophical coordination around a shared metaphysical claim); the mihna is a snare layered on top. If inseparable: the constraint IS the snare — the doctrinal claim is semantically identical to ''the state''s theological test for political loyalty.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether theological claim and enforcement machinery are separable constraints.').

omega_variable(
    identity_lock_mechanism,
    'For traditionalist scholars (Ahmad ibn Hanbal, his school), what mechanism binds them to resist the state-enforced doctrine? Is it: professional identity (the scholar''s reputation and career rest on doctrinal stance), relational identity (their self-concept and community membership constituted through literalist interpretation), or ideological identity (they believe the uncreated doctrine is true and affirming the false doctrine is categorically impermissible)?',
    'Post-enforcement trajectories: scholars who recant under torture/imprisonment then restore their original position after state pressure weakens signal ideological identity; those who maintain their schools without recanting signal relational and professional lock; those who shift doctrinal stance and remain recanted signal lower identity lock.',
    'If primarily ideological: the suppression is pure coercion against held truth-claims (extraction is maximal). If relational: the identity frame could break if community reconstituted outside the literal reading (exit becomes theoretically possible). If professional: career incentives could flip if state backing shifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'What type of identity-lock binds traditionalist resisters.').

omega_variable(
    reading_committer_underdetermination,
    'Does the framing of this constraint as ''state-enforced creation doctrine'' foreclose the uncreated reading''s core premise (Allah''s transcendence forbids temporal artifacts including revelation), or can both readings coexist as different theological traditions within Islam, with state enforcement being the NEW variable that distinguishes them?',
    'Textual and jurisprudential analysis: if the created-Qur''an claim logically entails that the uncreated reading is metaphysically impossible (not just politically suppressed), they foreclose each other; if the uncreated reading remains logically viable but politically expelled, they coexist-with state machinery as the differentiator.',
    'If foreclose: the readings represent incompatible metaphysical systems (one true, one false by internal logic). If coexist: the state''s role is to choose one framework and suppress its rival, not to settle a logical question. This affects whether we model the mihna as clarification of doctrine or as political conquest of theology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_underdetermination, conceptual, 'Whether reading-level axioms foreclose or coexist with sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qura_tr_t10, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(qura_tr_t20, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(qura_tr_t30, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(qura_be_t10, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(qura_be_t20, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(qura_be_t30, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(qura_su_t10, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(qura_su_t20, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(qura_su_t30, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 30, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__state_enforced_creation_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).

% DUAL FORMULATION NOTE:
% The kernel 'Qur'anic ontological status' decomposes into three constraint stories by the ε-invariance principle. The theological claim alone (the created-doctrine as rationalist philosophy) is structurally distinct from the state-enforced version (the same doctrine weaponized as an inquisition). The uncreated reading (traditionalist theology) is a third distinct constraint. All three share the kernel (the contested interpretation of Qur'anic ontology) but have different ε values, different beneficiary/victim structures, and different classifications. This story (state_enforced_creation_reading) describes the mihna—the conversion of theological dispute into extraction machinery. The created_reading describes the rationalist doctrine on its own terms. The uncreated_reading describes the traditionalist doctrine's structure. Each story declares its own ε; the network links them to enable cross-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, institutional, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
