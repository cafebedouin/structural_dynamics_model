% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Expansionist-Legalist Jihad Reading: Governance Establishment Obligation
 *   domain: religious/political/jurisprudential
 *
 * SUMMARY:
 *   This constraint instantiates the expansionist-legalist reading of jihad
 *   found in classical Islamic jurisprudence, particularly the tradition of
 *   al-Shafi'i, al-Mawardi, and Ibn Qayyim al-Jawziyyah. The reading treats
 *   jihad as an obligation to establish Islamic governance (dar al-Islam)
 *   where it does not exist, subject to procedural conditions: an imam (state
 *   authority) must declare it, non-Muslims must first be invited to convert
 *   or submit, warfare must observe proportionality and avoid indiscriminate
 *   harm, and the result is a legal framework of dhimmi status for subjugated
 *   non-Muslims. The reading legitimates offensive campaigns as fulfilling a
 *   religious duty, not mere territorial conquest. It is one of three
 *   contested readings of the Qur'anic corpus on jihad; the
 *   defensive-spiritual reading and revolutionary-vanguard reading offer
 *   structurally different interpretations. This story describes the
 *   expansionist-legalist reading ONLY: its referent is the standing
 *   arrangement this reading instantiates, assessed by the reading's own
 *   jurisprudential lights. The reading's endorsed alternative
 *   (defensive-spiritual reading's framework of internal spiritual struggle
 *   and purely defensive response) is NOT the referent — the referent is the
 *   existing expansionist-legalist arrangement. Per OQ-26 and OQ-258, ε=0.68
 *   is reading-indexed over a fixed referent.
 *
 * KEY AGENTS:
 *   - State authority (imam, caliph, or Islamic state): agenda-setter; declares jihad and sets conditions; power=institutional, exit=analytical
 *   - Islamic juridical establishment: beneficiary; elaborates the reading and legitimates campaigns; power=institutional, exit=analytical
 *   - Non-Muslim populations: victim; subject to conquest campaigns; power=powerless, exit=trapped
 *   - Dissenting Islamic jurists (defensive-spiritual, revolutionary-vanguard): victim + excluded; face takfir and institutional marginalization; power=moderate, exit=identity_locked
 *   - Classical juridical corpus (Qur'an, hadith, fiqh): vindicated proposition, not a real actor
 *   - Conquered territory populations: victim; transformed from independent political agents to subjects/dhimmis; power=powerless, exit=trapped
 *   - International legal order: observer; cannot enforce its own norms; power=institutional, exit=constrained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.68).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.71).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Expansionist-Legalist Jihad Reading: Governance Establishment Obligation").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious/political/jurisprudential").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a').
narrative_ontology:cs_kernel_codification('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', fixed_text).
narrative_ontology:cs_authority_grounding('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', lineage).
narrative_ontology:cs_interpretation_layer_present('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a').
narrative_ontology:cs_reading_relation('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', foundational, islamic_state_supremacy).
narrative_ontology:cs_axiom_status(islamic_state_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', islamic_state_supremacy, conventional).
narrative_ontology:cs_axiom('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', foundational, caliphal_declaration_monopoly).
narrative_ontology:cs_axiom_status(caliphal_declaration_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', caliphal_declaration_monopoly, deontological).
narrative_ontology:cs_reference_frame('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', classical_legalist_jurisprudence).
narrative_ontology:cs_drift_state('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', contemporary_post_colonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2b6bcf2a-97df-4cd0-bc89-26ace4ca9b5a', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, state_authority).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_juridical_establishment).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dissenting_islamic_jurists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, conquered_territory_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims monopoly on declaring jihad and interpreting its conditions. Interprets classical jurisprudence to legitimate expansionist campaigns. Frames the reading as faithful to foundational texts. Faces no internal sanction for declaring offensive campaigns if procedural conditions are met (imam authority, invitation issued, proportionality claimed). Administers the expansion and the resulting governance structures.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, state_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Elaborates and transmits the legalist reading through fatwa, commentary, and pedagogical tradition. Benefits from interpretive authority affirmation, corpus binding precedent, and delegitimization of dissenting jurists. Serves as the authoritative voice on what the classical sources mean; their interpretive voice is monopolized and their institutional role is stabilized.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_juridical_establishment, beneficiary,
    institutional, civilizational, analytical, universal).

% Subject to military campaigns conducted under this reading's authorization. Face conversion (theoretically offered, varying in actual practice), subjection as dhimmi (subordinated but legally protected), or combat. Options for exit are limited: migration is often blocked by borders, resistance is met with military force, conversion requires religious change that may be rejected by the population. Legal position post-conquest is determined entirely by the state's implementation of the reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations, payer,
    powerless, biographical, trapped, universal).

% Hold alternative readings (defensive-spiritual or revolutionary-vanguard) that contradict the expansionist-legalist framework. Face pressure to recant through institutional marginalization, loss of teaching positions, takfir (excommunication), or legal consequences in theocratic contexts. Cannot exit Islam without abandoning their fundamental identity; cannot publicly advocate their reading without professional and legal risk in Islamic-majority states.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, dissenting_islamic_jurists, payer,
    moderate, generational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, dissenting_islamic_jurists, excluded).

% Populations in regions targeted for conquest. Experience the constraint as military occupation and forcible governance transition. May eventually receive dhimmi status (subordinated but legally protected under Islamic law, paying jizyah tax), become part of the Islamic polity through conversion, or continue as combatants. Their legal and political position is transformed entirely by the state's implementation of the expansionist-legalist reading without their prior consent.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, conquered_territory_populations, payer,
    powerless, biographical, trapped, universal).

% The corpus of classical Islamic jurisprudence (Qur'an, hadith, fiqh precedents). Treated as authoritative and binding by all three readings. The expansionist-legalist reading vindicates this corpus as containing coherent, rule-bound expansion jurisprudence; alternative readings dispute whether the corpus actually supports expansionism or has been misread by the establishment.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, classical_juridical_corpus, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jihad_quranic_corpus__expansionist_legalist_reading, classical_juridical_corpus).

% Non-Islamic states and Islamic states with competing readings of jihad (revolutionary-vanguard adherents, defensive-spiritual interpreters). The expansionist-legalist reading's operation constrains their territorial integrity and delegitimizes their resistance. Their voice in defining what jihad permissibly means is structurally excluded from the authoritative Islamic interpretation council.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, rival_state_authorities, excluded,
    institutional, civilizational, constrained, universal).

% The post-Westphalian system of nation-states, sovereignty norms, and treaties on warfare (Geneva Conventions, UN Charter prohibition on offensive war). Observes the expansionist-legalist reading as incompatible with its own prohibition on territorial conquest and offensive military campaigns. Cannot enforce its norms against theocratic actors operating under this reading without military confrontation or economic sanctions.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, international_legal_order, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, state_authority).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides procedural legitimacy for territorial expansion: specifies conditions (imam authority, invitation to Islam, proportionality, non-combatant immunity) that organize the pursuit of Islamic governance and distinguish legitimate from illegitimate military action. Establishes legal status of conquered populations (dhimmi) and their relationship to the Islamic state, creating predictable governance frameworks.
% TRANSFER_FUNCTION: Transfers sovereignty, legal authority, and territorial control from non-Islamic states to Islamic states; transfers religious and political identity of conquered populations from independent political agents to subjects of Islamic governance; transfers interpretive authority over the Qur'an and hadith from dissenting jurists to the state-backed juridical establishment; transfers resource flows (jizyah tax) from conquered populations to the Islamic state.
% ABSENT_VOICES: Conquered non-Muslim populations have no voice in the declaration or conditions of jihad; dissenting Islamic jurists (defensive-spiritual and revolutionary-vanguard readers) are structurally excluded from the authoritative interpretation council and face institutional marginalization; non-Islamic states and the post-Westphalian international legal order are barred from the framework. Revolutionary-vanguard readers especially are silenced through takfir and institutional marginalization in theocratic contexts.
% DISAPPEARANCE_RATIONALE: If this reading and its enforcement mechanisms disappeared, the Islamic juridical establishment's monopoly on governance legitimation would dissolve; competing readings (defensive-spiritual, revolutionary-vanguard) would resurface and claim equal validity; conquered territories would face reversion to independence movements or alternative governance frameworks; the state's claim to obligatory expansion would lack sacred legal grounding; international relations would revert to post-Westphalian norms without Islamic-law justifications for territorial conquest.
% FOUNDING_PROBLEM: How to reconcile the Qur'anic call to spread Islamic belief and establish Islamic governance with the necessity of just war conditions and legal order: the legalist reading solves this by establishing procedural conditions (imam authority, invitation to Islam, proportionality, non-combatant immunity, established governance framework post-conquest) that permit systematic expansion while maintaining the claim of restraint through legal procedure.
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic jurists (al-Shafi'i, al-Mawardi, Ibn Qayyim al-Jawziyyah) from the 9th–14th centuries are cited by proponents as establishing the foundational conditions for expansionist jihad. However, contemporary Islamic legal scholars and exegetes, including those trained in the classical jurisprudential tradition, dispute whether the founding problem is accurately characterized by the expansionist-legalist reading. Defensive-spiritual readers argue the founding problem was solving internal spiritual struggle and purely defensive response, not territorial expansion. Revolutionary-vanguard readers argue the founding problem was liberation from apostate rulers, not distant conquest under existing state authority. Contemporary Islamic reformers and modernists argue the founding problem is obsolete in the post-Westphalian international system or was never about territorial conquest. No major scholarly voice OUTSIDE the expansionist-legalist establishment and the states that benefit from it has corroborated that territorial expansion under state monopoly is the primary problem this reading solves. The characterization itself is disputed by all sibling readings and by contemporary Islamic intellectual communities in diaspora contexts.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the reading concentrates sovereignty transfer, legal status redefinition, and territorial control in the hands of the state and its juridical establishment, with no compensation or consent framework for conquered populations. The reading frames this extraction as obligation (fard kifayah or fard 'ayn, a collective or individual duty), not as self-interested power. Suppression is correspondingly high (0.71) because the reading's persistence depends on actively delegitimizing competing readings through takfir, institutional exclusion, and doctrinal monopoly — dissenting jurists cannot publicly advocate alternative readings without consequences in theocratic contexts. Theater ratio (0.42) is moderate: the procedural conditions (invitation, proportionality, imam authority) create genuine legal form and are not pure performance, but an increasing share of the constraint's enforcement machinery (from t=0 to t=100, rising from 0.28 to 0.42) consists of justifying the expansion itself rather than implementing the conditions. The measurement trajectory shows extractiveness rising sharply from t=0 (0.42, early classical period with stricter conditions) to t=60 (0.68, modern institutionalization and territorial consolidation), then plateauing and declining slightly (t=80–100), as the reading faces increasing international legal challenge and internal reform pressure. Suppression requirement also rises and plateaus, showing mounting need for doctrinal policing as alternative readings gain contemporary Islamic intellectual currency. Theater ratio rises monotonically as procedural justification becomes increasingly detached from actual combat conditions.
 *
 * PERSPECTIVAL GAP:
 *   The state authority seat and the juridical establishment seat compute this as coordination (solving the legitimacy problem of governance expansion within a legal framework) plus a beneficiary extraction (monopoly on interpretation). Conquered populations and dissenting jurists compute the same constraint as pure extraction with a thin legal coating. The expansionist-legalist reading's own axioms (islamic_state_supremacy, caliphal_religious_authority) require the state's interpretive monopoly to be legitimate, which means they foreclose alternative readings within the single-framework test — but the readings survive by being held by different parties (state, traditional establishment, revolutionary vanguard, defensive reformers). This perspectival gap is structural: the reading's beneficiaries experience it as justified procedure; the victims experience it as enforced reinterpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority: d ≈ 0.0 (full beneficiary — controls declaration, faces no internal sanction, collects sovereignty and legal authority). Juridical establishment: d ≈ 0.1 (beneficiary — interpretive monopoly affirmed, dissent delegitimized). Non-Muslim populations: d ≈ 0.95 (full target — face conquest, loss of sovereignty, forced legal reclassification, no exit option). Dissenting Islamic jurists: d ≈ 0.85 (target — face takfir, institutional marginalization, professional consequences, identity-locked so cannot exit Islam cleanly). Conquered territory populations (post-conquest): d ≈ 0.8 (target — transformed into subordinated subjects, legal options constrained to dhimmi status or conversion). International legal order: d ≈ 0.5 (symmetric, analytically positioned observer — faces constraint but cannot be conquered, competes normatively but cannot enforce, is outside the framework).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to reconcile belief-spread with just governance) is CONTESTED. The expansionist-legalist reading claims the problem is live and solved through its framework. Defensive-spiritual readers claim the founding problem is mischaracterized (the real problem is internal spiritual struggle, not territorial expansion) and that the legalist reading has substituted a different founding problem. Revolutionary-vanguard readers claim the founding problem is immediate liberation from apostate rulers, not distant conquest under state authority. This is Mandatrophy Type 2: the founding problem's status is disputed by the parties, and the reading's persistence depends on the state/establishment authority to exclude the alternative framings. If the founding problem were demonstrated to be genuinely satisfied (Islamic belief universally adopted, or all territories under Islamic governance), the expansion obligation would dissolve; if the problem is shown to be mischaracterized, the reading loses its narrative grounding. The reading avoids mandatrophy through institutional power, not through genuine problem resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_characterization_ambiguity,
    'Is the core problem this reading solves genuinely the establishment of Islamic governance globally, or has the reading mischaracterized its own founding problem to justify expansion the reading''s beneficiaries prefer?',
    'Historical genealogy of the reading''s development in classical jurisprudence, compared to empirical outcomes: if territorial expansion was the stated goal from inception and remains unsatisfied, the problem is live; if the stated goal was belief-spread or community protection and territorial expansion became instrumentalized later, the characterization has shifted.',
    'If the founding problem was mischaracterized, the reading is functionally a snare (extraction justified through displaced narrative) rather than a tangled rope (extraction plus coordination). If the problem was always territorial expansion and remains live (as the reading claims), it remains tangled rope with genuine founding problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_characterization_ambiguity, conceptual, 'Whether the expansionist-legalist reading correctly identifies its own founding problem or has instrumentalized expansion for other purposes.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the three readings (expansionist-legalist, defensive-spiritual, revolutionary-vanguard) COEXIST as live alternative positions held by different parties, or does one reading logically FORECLOSE the others within the same framework?',
    'Careful exegetical comparison of the Qur''anic text and hadith corpus, asking whether each reading can be grounded in the corpus without internal contradiction, and whether accepting one reading logically entails rejecting the core premise of the others within a single interpretive framework.',
    'If the readings coexist (no foreclosure), they remain in permanent contestation and none can claim definitive authority from the text alone. If one reading forecloses another, the foreclosed reading is logically indefensible from within the kernel (though may persist as a historical/political position). The classification of the constraint itself depends on this: if readings coexist, all three are live and their differences are inter-party conflict; if foreclosure obtains, the non-foreclosing reading claims doctrinal correctness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the classical Islamic jurisprudence corpus permits multiple internally consistent readings of jihad or whether one reading''s core premises logically exclude another.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of dissenting readings (defensive-spiritual and revolutionary-vanguard) primarily structural (legal consequences, institutional exclusion, takfir) or internalized (dissenting jurists accept the legalist reading as religiously correct)?',
    'Empirical observation of dissenting jurists in theocratic and non-theocratic Islamic contexts: in contexts where structural suppression is absent (diaspora communities, academic freedom), does the expansionist-legalist reading persist or do alternative readings resurface?',
    'If suppression is primarily structural, removing enforcement removes the constraint''s hold and alternatives resurface (the measured suppression=0.71 is context-dependent). If suppression is internalized, the constraint persists even without enforcement (the reading is genuinely held as correct, not coerced). The implications for post-constraint stability are opposite: structural suppression suggests rapid return to pluralism; internalized suppression suggests the reading is consensual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether dissenting Islamic jurists accept the expansionist-legalist reading as religiously correct or are suppressed by institutional power.').

omega_variable(
    classical_corpus_determinacy,
    'Is the classical Islamic jurisprudential corpus (Qur''an, hadith, fiqh) determinate enough to establish a single correct reading of jihad, or is it genuinely ambiguous and all three readings are textually defensible?',
    'Systematic exegetical analysis by scholars across the three readings, asking whether each reading selects hadith and verses fairly and whether alternative interpretations of the selected texts are plausible. Can one reading be shown to violate the corpus''s internal logic or is each internally consistent?',
    'Determinacy supports one reading''s claim to doctrinal authority and would suggest the others are errors (not coexisting alternatives). Ambiguity supports plural legitimacy and suggests the readings will persist as long as different parties control different institutions (the expansionist-legalist reading benefits from state control of interpretation, so benefits from ambiguity being resolved in its favor institutionally).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classical_corpus_determinacy, empirical, 'Whether the classical Islamic jurisprudential sources determine a single correct reading of jihad or permit multiple internally consistent readings.').

omega_variable(
    dhimmi_status_consent_ambiguity,
    'Is the dhimmi status offered to conquered non-Muslims under this reading''s legal framework a genuinely negotiated legal status, or is it extracted coercively through the prior military conquest?',
    'Examination of historical practice and contemporary implementation: did or do conquered populations have genuine alternatives (conversion, emigration, combat continuation), or are dhimmi terms imposed as the sole acceptable non-military option? Can dhimmis renegotiate their status or is it unilaterally determined by the Islamic state?',
    'If dhimmi status is genuinely negotiated with alternatives available, the constraint''s extractiveness is lower and the arrangement is closer to a coordination mechanism (conquered population gets legal protection and property rights, pays jizyah tax in exchange). If dhimmi status is imposed coercively as the sole non-military option, extractiveness remains high and the coordination function is a post-hoc legal formalization of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dhimmi_status_consent_ambiguity, empirical, 'Whether dhimmi status is a negotiated legal arrangement or coercively extracted through prior military dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(jiha_tr_t60, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(jiha_tr_t80, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 80, 0.43).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(jiha_be_t60, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(jiha_be_t80, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 80, 0.69).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(jiha_su_t60, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(jiha_su_t80, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__expansionist_legalist_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel jihad_quranic_corpus. The expansionist-legalist reading establishes state monopoly on declaration and permits offensive campaigns under procedural conditions. It INFLUENCES (but does not foreclose) the defensive-spiritual reading: if the state has authority to declare expansionist jihad, the defensive reading must operate within that authority structure or claim the state is misinterpreting the corpus (creating institutional tension). It COEXISTS_WITH the revolutionary-vanguard reading: both permit offensive action but differ on who has authority (state vs. individual vanguard) and on the target (territorial expansion vs. apostate rulers). The three readings are not equivalent decompositions per ε-invariance; rather, they are alternative interpretations of the same kernel text. The constraint's ε (0.68, high extraction) reflects the expansionist-legalist reading's actual operation under state authority. The defensive-spiritual reading's ε would be substantially lower (around 0.25-0.35, describing coordination with minimal extraction). The revolutionary-vanguard reading's ε would be moderate-to-high (0.55-0.65, describing extraction by vanguard actors against state and civilians). All three readings share the kernel (Qur'anic corpus on jihad) but have different structural consequences for sovereignty, victim sets, and authority monopoly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
