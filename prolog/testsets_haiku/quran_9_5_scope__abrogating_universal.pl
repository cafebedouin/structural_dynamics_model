% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Qur'an 9:5 Abrogating-Universal Reading: Eternal Offensive Jihad Mandate
 *   domain: religious/political
 *
 * SUMMARY:
 *   The abrogating-universal reading interprets Qur'an 9:5 as nullifying
 *   (nasikh) all prior Qur'anic verses emphasizing peace, tolerance, and
 *   defensive warfare. Under this reading, the verse mandates permanent,
 *   universal offensive jihad against all non-Muslims until they submit to
 *   Islamic law or convert — a standing legal obligation (fard kifayah or
 *   fard 'ain depending on application) with no temporal or geographic
 *   limits. This reading is one of three major hermeneutical positions on
 *   9:5. The other readings (contextual-defensive and progressive-synthesis)
 *   reject the abrogation claim, interpret the verse as addressing a specific
 *   7th-century context, and prioritize treaty obligations and defensive
 *   doctrine. The abrogating-universal reading is claimed as the structurally
 *   true reading by expansionist movements and some classical and
 *   contemporary jihadi ideologues; it is rejected by mainstream Islamic
 *   scholarship and international Islamic organizations. This story
 *   instantiates ONLY the abrogating-universal reading as a constraint,
 *   analyzing its structural properties, beneficiary/victim alignment, and
 *   suppression mechanisms. The other readings are not described here — they
 *   are separate constraints (constraint_9_5_scope__contextual_defensive and
 *   constraint_9_5_scope__progressive_synthesis) linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Expansionist movements (Salafist jihadi networks, historical caliphates claiming abrogation doctrine): beneficiary and agenda-setter. Draw institutional legitimacy and operational mandate from the reading. Identity-locked to it.
 *   - Non-Muslim populations (Christians, Jews, polytheists, secularists, religious minorities): structural targets. Trapped with two exit options the constraint offers: submission or conversion. Vulnerable by definition under the reading.
 *   - Coexistence-framework advocates (mainstream Islamic scholars, reformists, historians): Suppressed by doctrinal claim that their reading is abrogated and invalid. Identity-locked — exiting means abandoning scholarly authority.
 *   - Jihadist ideological networks: Beneficiary and organized agenda-setter. Use the reading as recruitment foundation and operational justification. Identity fusion with the reading is high.
 *   - International humanitarian law: Observer seat. Absent from the reading's internal logic; enforcement limited to non-accepting jurisdictions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.88).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.91).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.88).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Qur'an 9:5 Abrogating-Universal Reading: Eternal Offensive Jihad Mandate").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "religious/political").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, 'be3cf9cd-88c7-4d11-b324-e2117e5bd40f').
narrative_ontology:cs_kernel_codification('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', fixed_text).
narrative_ontology:cs_authority_grounding('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', lineage).
narrative_ontology:cs_interpretation_layer_present('be3cf9cd-88c7-4d11-b324-e2117e5bd40f').
narrative_ontology:cs_reading_relation('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', quran_9_5_scope__progressive_synthesis, coexists_with).
narrative_ontology:cs_axiom('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', foundational, quranic_abrogation_valid_and_total).
narrative_ontology:cs_axiom_status(quranic_abrogation_valid_and_total, holdable).
narrative_ontology:cs_axiom_grounding('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', quranic_abrogation_valid_and_total, empirically_contingent).
narrative_ontology:cs_axiom('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', foundational, eternal_universal_jihad_obligation).
narrative_ontology:cs_axiom_status(eternal_universal_jihad_obligation, holdable).
narrative_ontology:cs_axiom_grounding('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', eternal_universal_jihad_obligation, deontological).
narrative_ontology:cs_reference_frame('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', eternal_divine_mandate_for_believer_supremacy).
narrative_ontology:cs_drift_state('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', contemporary_post_colonial_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('be3cf9cd-88c7-4d11-b324-e2117e5bd40f', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, jihadi_ideological_networks).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, coexistence_framework_advocates).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, reformist_islamic_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draw legitimacy from the abrogating-universal reading to justify offensive military campaigns against non-Muslim populations. The reading provides theological cover for conquest framed as divinely mandated obligation, not optional aggression. Their institutional identity fuses with this reading: rejecting it means renouncing the basis for their mobilization and the religious authority structure they have built. Examples include historical caliphates claiming abrogation doctrine and contemporary Salafist jihadi networks.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, expansionist_movements, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__abrogating_universal, expansionist_movements, agenda_setter).

% Under this reading's interpretation, are legitimate targets of offensive violence unless they formally submit to Islamic law or convert. No exit mechanism exists except the two options the constraint itself offers: submission or conversion. Resistance is suppressed by the constraint's explicit framing of non-submission as justifying continued military action. Their vulnerability is structural to the reading itself. This includes Christians, Jews, polytheists, secularists, and other religious minorities.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, non_muslim_populations, payer,
    powerless, immediate, trapped, global).

% Islamic scholars and religious leaders who interpret Qur'an 9:5 within historical context and read the tradition as permitting peaceful coexistence, treaty obligations, and defensive-only warfare. Under the abrogating-universal reading, their interpretive framework is declared void by abrogation; their theological authority is suppressed by the doctrinal claim that their reading has been textually overruled. Exit means abandoning professional identity and scholarly tradition. This includes mainstream Islamic scholars, reformist movements, and academic historians of Islam.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, coexistence_framework_advocates, payer,
    moderate, biographical, identity_locked, global).

% Scholars who read Qur'an 9:5 as a time-bound historical directive rather than eternal law, or who argue that ethical progression within the Quranic corpus places limits on literal application. The abrogating-universal reading explicitly rejects their hermeneutical methodology (context-sensitivity, progressive ethics) as illegitimate. Their suppression is doctrinal: the reading declares their interpretive tools invalid. Professional identity is constituted through scholarship; exiting means career and intellectual death.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, reformist_islamic_scholars, payer,
    moderate, biographical, identity_locked, global).

% Use the abrogating-universal reading as the theological foundation for recruitment, indoctrination, and operational justification. Adherents to this reading fuse their identity with the organizational mission; the reading provides the cognitive frame that makes violence appear not as choice but as obligation. Exit means cognitive and social defection from the entire interpretive community, which carries severe relational and existential costs.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, jihadi_ideological_networks, beneficiary,
    organized, generational, identity_locked, global).

% Islamic scholars working within contextual, defensive, or progressive-synthesis hermeneutics are excluded from the authority structure that treats the abrogating-universal reading as binding. Their exclusion is not accidental but structural: the reading explicitly rejects their interpretive methodologies as invalid. They exist but are not heard in the discourse spaces where the constraint is enforced. This includes Islamic academic institutions, reformist movements, and sufi traditions that emphasize coexistence.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__abrogating_universal, competing_hermeneutical_traditions, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__abrogating_universal, expansionist_movements).
narrative_ontology:fixing_cost_class(quran_9_5_scope__abrogating_universal, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None structurally present. The reading does not solve a collective-action problem among non-combatants; it mandates one-directional military action by believers against non-believers. Any coordination it achieves is the coordination of believers around offensive action, not cooperation between opposed populations.
% TRANSFER_FUNCTION: Transfers legitimacy and obligation for offensive violence from optional/contextual interpretation to mandatory divine command. Transfers authority from contextual-historical hermeneutics to abrogation-doctrine literalism. Transfers victims: under the reading, all non-Muslims outside formal submission networks become legitimate targets. Appropriates life, property, and sovereignty as lawful extraction from the non-Muslim category.
% ABSENT_VOICES: Scholars of the contextual-defensive and progressive-synthesis readings are excluded from authority structures built on the abrogating-universal reading. Representatives of non-Muslim populations have no seat in the jurisprudential discourse; their vulnerability is authored by the reading without their participation. Humanitarian-law frameworks are absent from the reading's internal logic, which treats divine command as overriding international norms. Representatives of coexistence movements are systematically excluded.
% DISAPPEARANCE_RATIONALE: If the abrogating-universal reading lost institutional legitimacy and enforcement (if scholarly consensus shifted to contextual-defensive or progressive-synthesis readings, or if Islamic jurisprudence formally rejected the abrogation doctrine as applied here), the theological mandate for offensive jihad would evaporate. Non-Muslim populations would cease to be defined as automatic targets. Expansionist movements would lose the textual foundation for first-strike campaigns. Coexistence frameworks would move from suppressed to legitimate. The world would not rearrange neutrally — it would rearrange toward treaty-based relations, defensive-only warfare doctrine, and institutional pluralism.
% FOUNDING_PROBLEM: 7th-century Medinan question: How should believers respond to treaty-breaking polytheist tribes near Medina after repeated violations of the constitution of Medina? The abrogating-universal reading answers by elevating this specific historical directive into an eternal, universal obligation binding all believers against all non-Muslims.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (hostile 7th-century tribal politics in the Hijaz) has no living instantiation. No contemporary party disputes that the original tribes and treaty contexts no longer exist. However, the abrogating-universal reading persists by claiming the problem was never the specific tribes but the universal category 'non-Muslim rejection of Islam' — a universal, trans-historical problem. This re-framing of the founding problem is NOT corroborated by sources outside the reading itself. Contextual-defensive and progressive-synthesis scholars explicitly reject the universalization, arguing the problem was specific and time-bound. The re-definition of the founding problem is internal to the abrogating-universal reading's own logic. Independent historians of 7th-century Arabia and modern Islamic scholars outside the abrogating-universal tradition do not corroborate the universalized reading of the founding problem.
narrative_ontology:disappearance_verdict(quran_9_5_scope__abrogating_universal, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__abrogating_universal, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__abrogating_universal, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__abrogating_universal, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__abrogating_universal, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is exceptionally high (0.88) because the reading authorizes unlimited appropriation of non-Muslim life, property, and sovereignty as legitimate targets absent submission. There is no proportionality gate, no temporal limit, no geographic boundary — the extraction is maximal. Suppression is equally high (0.91) because the constraint's persistence depends critically on suppressing alternative readings. The abrogation doctrine must be defended against historical-contextual and progressive-ethical challenges; coexistence frameworks must be actively excluded from authority structures; reformist scholars must be delegitimized as 'deviant' or 'apostate.' This suppression is not peripheral but central — without it, the reading collapses to one voice among many. Theater_ratio is moderate-low (0.22) because the reading is relatively straightforward in its application: it does not require elaborate secondary justification, it does not depend on theatrical maintenance. Where theater exists, it appears in the ex-post rationalization of specific military campaigns as justified under the reading, not in the reading's core operation. The measurement series shows accretion over the interval: as the reading institutionalizes (first in classical Islamic jurisprudence through abrogation doctrine, then in modern jihadist ideology from ~1950 onward), extractiveness rises and suppression requirement rises. Theater_ratio remains lower because the reading does not depend on performance to maintain itself — its extractiveness is direct and its suppression is structural.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and agenda-setter seats (expansionist movements, jihadi networks) experience the reading as coordinate liberation and sacred obligation. From their position, the reading solves the coordination problem of collective action among believers and justifies the prioritization of offensive action. From the payer seats (non-Muslim populations, coexistence advocates, reformist scholars), the same reading is pure extraction enforced by military and doctrinal suppression. The non-Muslim populations have no seat in the discourse that decides their status; coexistence advocates are excluded from authority structures; reformist scholars are delegitimized. The engine will compute radically different types at each seat: from the jihadi network's position, the reading may compute as coordination (though the suppression score should override toward snare); from the non-Muslim target's position, it computes unambiguously as snare with high extraction and suppression. From the reformist scholar's position, it is a snare deployed to suppress their intellectual and institutional authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for expansionist movements and jihadi networks (beneficiaries/agenda-setters): d approaches 0.0 (beneficiaries). They collect legitimacy, theological authority, and operational mandate. Their exit options are high (arbitrage-grade: they can adopt other readings, but identity-lock is deep, making exit cognitively costly). Power is organized/institutional. Directionality for non-Muslim populations (payers/victims): d approaches 1.0 (full targets). They have zero say in the reading's interpretation or application. Exit options are trapped (submit or convert, both existential losses). Power is powerless/moderate (depending on population). Directionality for coexistence advocates and reformist scholars (suppressed beneficiaries): d is mid-to-high (0.55–0.75, targets of doctrinal suppression). They have moderate power (scholarly authority, institutional position) but identity-lock is high (exiting means professional death). The reading suppresses their interpretive authority, not their physical survival, but the suppression is total within the discourse spaces that recognize the reading. No directionality override is needed; the structural derivation from beneficiary/victim + exit_options + identity_lock should produce the right directionality profiles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (7th-century hostile tribes, treaty violations, tribal polytheism near Medina) is unambiguously dead. No contemporary instance of that problem exists. The abrogating-universal reading avoids mandatrophy detection by redefining the founding problem: it claims the specific problem was never the point — the point was 'non-Muslim rejection of Islam' as a universal, trans-historical category. This move is internal to the reading's own logic and is not corroborated by independent sources. Contextual-defensive and progressive-synthesis readings explicitly reject this universalization. The reading thus exhibits mandatrophy: it was built to solve a problem that no longer exists, and it persists by redefining its own founding mandate to make that non-existence invisible. The constraint should signal mandatrophy_resolved=true in base_properties to flag this structural pattern. The founding_problem_status=dead + disappearance_verdict=world_rearranges mismatch is the engine signal for zombie-constraint detection: a constraint whose founding problem is dead but whose disappearance would rearrange the world indicates that what persists is extraction, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_doctrine_contingency,
    'Is the abrogation doctrine (nasikh) itself a valid hermeneutical principle, or is it a construct that enabled the abrogating-universal reading and would collapse if abrogation-as-doctrine were rejected?',
    'Historical textual analysis of how and when the abrogation doctrine emerged in Islamic jurisprudence; reconstruction of pre-abrogation hermeneutics; analysis of whether alternative hermeneutical frameworks (without abrogation) can accommodate the full Qur''anic corpus without contradiction.',
    'If abrogation-as-doctrine is itself a construct (not inevitable from the text), then the abrogating-universal reading rests on a contingent methodological choice, not a textual requirement. The reading would be exposed as one choice among alternatives. If abrogation is unavoidable, then some reading of 9:5 as abrogating must be true.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_doctrine_contingency, empirical, 'Whether the abrogation doctrine is textually necessary or methodologically contingent.').

omega_variable(
    identity_lock_mechanism,
    'To what extent is adherence to the abrogating-universal reading maintained by identity fusion (professional, ideological, relational), versus by conviction in its textual truth?',
    'Tracking changes in reading adoption when identity incentives shift (e.g., when reformist movements gain institutional power, when geographic displacement severs ideological networks, when scholarly consensus shifts). If identity-locked readers remain committed despite incentive shifts, lock is high; if they shift readings when institutional context changes, lock is weaker.',
    'If lock is high, the reading''s persistence depends more on institutional reproduction and identity investment than on the reading''s inherent plausibility. Suppression mechanisms (which suppress coexistence frameworks) matter more for persistence than the reading''s own logical force. If lock is low, the reading''s persistence might be more fragile and responsive to argumentative challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether the reading''s persistence relies on identity fusion or intellectual conviction.').

omega_variable(
    suppression_mechanism_internalization,
    'For coexistence advocates and reformist scholars who are suppressed by the reading''s claim that their hermeneutics are invalid, is the suppression primarily structural (institutional exclusion, professional cost) or internalized (the scholars have absorbed the abrogation doctrine and now self-suppress their own interpretive authority)?',
    'Comparative analysis of suppression trajectories: (1) Scholars who remain active in coexistence or reformist traditions despite institutional pressure (structural suppression, not internalized). (2) Scholars who abandon their readings and adopt abrogating-universal framing after institutional pressure (possible internalization). (3) Historical tracking of reformist movements that gained institutional power and suddenly re-emerged with confidence in their readings (suggesting suppression was structural, not internalized, and was removed by power shift).',
    'If suppression is primarily structural, it can be reversed by institutional change (policy shifts, scholarly consensus reversal). If it is internalized, suppressed scholars carry the constraint''s inhibition with them even after institutional pressure is removed; additional barriers to reformism emerge internally. The constraint''s effective suppression is higher if internalized, because it persists after external enforcement pressure is gone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Suppression mechanism: structural institutional pressure versus internalized cognitive inhibition.').

omega_variable(
    founding_problem_redefinition,
    'The abrogating-universal reading redefines its founding problem from ''specific 7th-century tribal conflicts'' to ''universal non-Muslim rejection of Islam.'' Is this redefinition a legitimate reinterpretation of the founding problem, or is it a cover story that obscures mandatrophy?',
    'Genealogical analysis of when the problem was redefined (early classical jurisprudence, medieval consolidation, modern jihadist ideology). Textual analysis of whether the original sources (hadith, early exegesis) support the universal reading or are being retrofitted. Assessment from scholars outside the abrogating-universal tradition of whether the redefinition is justified.',
    'If redefinition is legitimate, the reading is not maladjusted to its founding context — it has evolved to address a broader problem. If redefinition is a cover story, the reading exhibits mandatrophy: it persists because it serves extraction, not because the problem it was built to solve remains relevant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_redefinition, empirical, 'Whether the reading''s founding problem has been legitimately reinterpreted or illegitimately redefined to mask mandatrophy.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three readings of the 9:5 kernel (abrogating-universal, contextual-defensive, progressive-synthesis) genuinely incommensurable — unable to be held simultaneously within any single framework — or are they compatible under a higher-order interpretive scheme?',
    'Attempt to construct a coherent framework that accommodates all three readings: e.g., a reading that says ''the abrogation doctrine is valid AND the verse is context-specific AND ethical progression is real.'' If such a framework can be constructed without internal contradiction, the readings are compatible; if not, they are genuinely incommensurable.',
    'If incommensurable, each reading must suppress the others to persist, which explains the high suppression score. If compatible under a higher framework, the suppression is optional rather than structural, and the readings might coexist peacefully rather than competing for dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three kernel readings are logically incommensurable or reconcilable under a higher framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 632, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t632, quran_9_5_scope__abrogating_universal, theater_ratio, 632, 0.08).
narrative_ontology:measurement(qura_tr_t750, quran_9_5_scope__abrogating_universal, theater_ratio, 750, 0.12).
narrative_ontology:measurement(qura_tr_t1100, quran_9_5_scope__abrogating_universal, theater_ratio, 1100, 0.16).
narrative_ontology:measurement(qura_tr_t1450, quran_9_5_scope__abrogating_universal, theater_ratio, 1450, 0.19).
narrative_ontology:measurement(qura_tr_t1800, quran_9_5_scope__abrogating_universal, theater_ratio, 1800, 0.21).
narrative_ontology:measurement(qura_tr_t2026, quran_9_5_scope__abrogating_universal, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t632, quran_9_5_scope__abrogating_universal, base_extractiveness, 632, 0.62).
narrative_ontology:measurement(qura_be_t750, quran_9_5_scope__abrogating_universal, base_extractiveness, 750, 0.71).
narrative_ontology:measurement(qura_be_t1100, quran_9_5_scope__abrogating_universal, base_extractiveness, 1100, 0.78).
narrative_ontology:measurement(qura_be_t1450, quran_9_5_scope__abrogating_universal, base_extractiveness, 1450, 0.84).
narrative_ontology:measurement(qura_be_t1800, quran_9_5_scope__abrogating_universal, base_extractiveness, 1800, 0.87).
narrative_ontology:measurement(qura_be_t2026, quran_9_5_scope__abrogating_universal, base_extractiveness, 2026, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t632, quran_9_5_scope__abrogating_universal, suppression_requirement, 632, 0.58).
narrative_ontology:measurement(qura_su_t750, quran_9_5_scope__abrogating_universal, suppression_requirement, 750, 0.73).
narrative_ontology:measurement(qura_su_t1100, quran_9_5_scope__abrogating_universal, suppression_requirement, 1100, 0.82).
narrative_ontology:measurement(qura_su_t1450, quran_9_5_scope__abrogating_universal, suppression_requirement, 1450, 0.88).
narrative_ontology:measurement(qura_su_t1800, quran_9_5_scope__abrogating_universal, suppression_requirement, 1800, 0.9).
narrative_ontology:measurement(qura_su_t2026, quran_9_5_scope__abrogating_universal, suppression_requirement, 2026, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__abrogating_universal, 0.15).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The constraint 'quran_9_5_scope' is a contested kernel with three structurally distinct readings instantiated as separate constraint stories: (1) abrogating_universal: reads 9:5 as abrogating all prior peaceful verses and mandating eternal universal offensive jihad (this story); (2) contextual_defensive: reads 9:5 as addressing 7th-century tribal conflict, does not abrogate, prioritizes defensive warfare; (3) progressive_synthesis: reads 9:5 as time-bound political directive, rejects literalist abrogation. The three readings are held simultaneously by different Islamic communities and scholarly traditions. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different types. They are linked here as a constraint family via network.affects_constraints because each reading's persistence depends partly on suppression of the others. The abrogating-universal reading (this story) directly forecloses the validity of coexistence frameworks (affecting the contextual-defensive reading's institutional viability) and influences the progressive-synthesis reading (by claiming ethical progression is irrelevant if abrogation has occurred).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
