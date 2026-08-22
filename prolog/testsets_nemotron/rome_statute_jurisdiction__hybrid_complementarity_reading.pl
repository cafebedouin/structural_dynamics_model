% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction (Hybrid Complementarity Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute's complementarity mechanism (Article 17) creates a
 *   hybrid jurisdictional architecture: the ICC has residual universal
 *   jurisdiction over core international crimes, but only exercises it when
 *   national jurisdictions are 'unwilling or unable' to genuinely investigate
 *   and prosecute. This reading positions the Statute as a genuine but
 *   constrained coordination mechanism — it solves the collective action
 *   problem of impunity for atrocity crimes while preserving sovereign
 *   primacy as the default enforcement layer. The hybrid reading claims the
 *   Statute is neither a universal mandate transcending consent (universalist
 *   reading) nor a purely consent-based framework (sovereigntist reading),
 *   but a structural hybrid where universal aspiration and sovereign primacy
 *   are held in productive tension. The constraint's extraction comes from
 *   the ICC's claim to residual authority over situations in state parties
 *   and Security Council referrals, while its suppression manifests in the
 *   complementarity admissibility test and the dependence on state
 *   cooperation for arrest and evidence. Theater is significant: the
 *   complementarity assessment process and victim participation regime
 *   perform coordination while the Court's operational reality depends on
 *   political cooperation it cannot compel.
 *
 * KEY AGENTS:
 *   - icc_prosecutor_office: agenda_setter (institutional/generational/analytical/global) — initiates investigations, controls complementarity assessments, depends on state cooperation
 *   - state_parties_coalition: beneficiary (organized/generational/arbitrage/global) — gains legitimacy from membership, accepts ICC jurisdiction as cost of coalition membership
 *   - non_state_parties_targeted: victim (powerful/biographical/trapped/global) — subject to ICC jurisdiction via Security Council referral or territorial state party referral without consent
 *   - state_parties_under_preliminary_examination: payer (powerful/biographical/constrained/global) — bear reputational and political costs of ICC scrutiny, may face investigation
 *   - situation_country_governments: victim (moderate/biographical/identity_locked/national) — governments in situation countries face ICC jurisdiction while claiming complementarity compliance
 *   - civil_society_transnational_advocacy_networks: beneficiary (organized/biographical/mobile/global) — gain institutional platform for accountability advocacy, shape Court's agenda
 *   - victim_participants: beneficiary/payer (powerless/biographical/trapped/local) — gain procedural voice but bear participation costs without guaranteed reparations
 *   - legal_scholars_practitioners: observer (analytical/civilizational/analytical/universal) — interpret and contest the Statute's meaning across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.55).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction (Hybrid Complementarity Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '036169ba-0ae4-499d-bad2-d318356e6b53').
narrative_ontology:cs_kernel_codification('036169ba-0ae4-499d-bad2-d318356e6b53', formalized).
narrative_ontology:cs_authority_grounding('036169ba-0ae4-499d-bad2-d318356e6b53', extraction).
narrative_ontology:cs_interpretation_layer_present('036169ba-0ae4-499d-bad2-d318356e6b53').
narrative_ontology:cs_reading_relation('036169ba-0ae4-499d-bad2-d318356e6b53', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('036169ba-0ae4-499d-bad2-d318356e6b53', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('036169ba-0ae4-499d-bad2-d318356e6b53', foundational, complementarity_as_operational_hybrid).
narrative_ontology:cs_axiom_status(complementarity_as_operational_hybrid, holdable).
narrative_ontology:cs_axiom_grounding('036169ba-0ae4-499d-bad2-d318356e6b53', complementarity_as_operational_hybrid, conventional).
narrative_ontology:cs_axiom('036169ba-0ae4-499d-bad2-d318356e6b53', foundational, residual_universal_jurisdiction_via_treaty_consent).
narrative_ontology:cs_axiom_status(residual_universal_jurisdiction_via_treaty_consent, holdable).
narrative_ontology:cs_axiom_grounding('036169ba-0ae4-499d-bad2-d318356e6b53', residual_universal_jurisdiction_via_treaty_consent, conventional).
narrative_ontology:cs_reference_frame('036169ba-0ae4-499d-bad2-d318356e6b53', rome_conference_1998_consensus).
narrative_ontology:cs_drift_state('036169ba-0ae4-499d-bad2-d318356e6b53', contemporary_practice_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('036169ba-0ae4-499d-bad2-d318356e6b53', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutor_office).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_coalition).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, civil_society_transnational_advocacy_networks).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, victim_participants).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_state_parties_targeted).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_under_preliminary_examination).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, situation_country_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_coalition).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, victim_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates preliminary examinations and investigations, controls complementarity assessments under Article 17, requests arrest warrants and summonses. Gains institutional authority, budget, and legitimacy from each case. Depends entirely on state cooperation for arrests, evidence, witness protection — has no enforcement arm. Cannot exit the constraint; the Office IS the constraint's operational core.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutor_office, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutor_office, beneficiary).

% 124 state parties (2024) gain diplomatic legitimacy, burden-sharing for atrocity crimes, and a forum for victim participation. Pay assessed contributions (~€150M/year), accept ICC jurisdiction over their nationals and territory, and face complementarity scrutiny. Can withdraw (Philippines 2019, Burundi 2017) but face reputational costs; major powers (US, China, Russia) remain outside with arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_coalition, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_coalition, payer).

% States not party to the Statute (US, China, Russia, India, Israel, etc.) subjected to ICC jurisdiction via UN Security Council referral (Sudan 2005, Libya 2011) or territorial state party referral (Afghanistan situation). Bear investigation and prosecution risk without consent, without assessed contributions, without complementarity protection — the Court assesses their national proceedings' 'genuineness' from outside. No exit except Security Council veto (P5) or universal ratification (impossible).
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_state_parties_targeted, payer,
    powerful, biographical, trapped, global).

% State parties (Kenya, Georgia, Ukraine, Palestine, Venezuela, etc.) under preliminary examination or investigation. Bear reputational costs, diplomatic pressure, resource diversion to complementarity defense. Must demonstrate 'genuine' national proceedings to avoid ICC intervention — a performance requirement. Can withdraw (constrained exit: Article 127 requires 1-year notice, obligations persist) but withdrawal does not affect pending matters.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_under_preliminary_examination, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_under_preliminary_examination, payer).

% Governments in situation countries (DRC, Uganda, CAR, Mali, etc.) that referred situations or accepted jurisdiction. Face ICC jurisdiction over their territory/nationals while claiming complementarity compliance. Their sovereign identity is fused with the complementarity performance: demonstrating 'genuine' proceedings becomes a test of legitimacy. Cannot exit the performance without conceding 'unwillingness' — identity-locked by the complementarity logic itself.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, situation_country_governments, payer,
    moderate, biographical, identity_locked, national).

% NGO coalitions (Coalition for the ICC, FIDH, HRW, Amnesty, local partners) gain institutional platform: Article 15 communications, victim representation, Assembly of States Parties participation, direct Prosecutor access. Shape Court's agenda through situation referrals, amicus briefs, monitoring. Mobile exit: can shift advocacy to regional courts, UN mechanisms, national courts — not dependent on ICC for relevance.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, civil_society_transnational_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Victims granted participation status (Article 68(3)): legal representation, views/concerns presentation, reparations applications. Gain procedural voice in proceedings that determine their fate. Bear costs: reliving trauma, security risks, legal representation dependence, years of proceedings without guaranteed reparations (Trust Fund for Victims is voluntary, underfunded). Trapped: no alternative forum for international criminal justice; national courts often unavailable or compromised.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victim_participants, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, victim_participants, payer).

% Interpret and contest the Statute across the three readings. Produce the doctrinal architecture each reading inhabits: universalist (international law as cosmopolitan), sovereigntist (consent as foundation), hybrid (complementarity as structural synthesis). No material stake; analytical seat sees full structure.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, legal_scholars_practitioners, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of impunity for core international crimes (genocide, crimes against humanity, war crimes) by creating a permanent court with residual jurisdiction that activates only when national systems fail — a burden-sharing mechanism that preserves sovereign primacy as default.
% TRANSFER_FUNCTION: Transfers investigative and prosecutorial authority from sovereign states to the ICC in cases where states are 'unwilling or unable' to genuinely proceed; transfers legitimacy and diplomatic cover to state parties in exchange for assessed contributions and jurisdictional acceptance; transfers procedural voice to victims in exchange for participation costs.
% ABSENT_VOICES: Populations in non-state party territories not referred (Syria, Myanmar, Yemen, China/Xinjiang) — would object to impunity but have no ICC pathway. Future generations who inherit the precedent of complementarity as sovereignty-preserving — not in the room. The excluded stakeholder (non_state_parties_targeted) captures only the referred subset.
% DISAPPEARANCE_RATIONALE: If the Rome Statute vanished overnight: 124 state parties would lose the complementarity backstop and the Assembly of States Parties forum; the ICC Prosecutor's Office would dissolve; Security Council referrals would lose their ICC pathway (ad hoc tribunals would return); victim participation regime would disappear; national complementarity performances would cease; the universal jurisdiction customary law trajectory would lose its treaty anchor. The international criminal justice architecture would reorganize around ad hoc mechanisms and national prosecutions.
% FOUNDING_PROBLEM: Ending impunity for the most serious crimes of international concern through a permanent, independent court that respects sovereign primacy — the Rome Conference (1998) sought to solve the gap between Nuremberg/Tokyo ad hoc tribunals and the persistent reality of unpunished atrocities.
% FOUNDING_PROBLEM_CORROBORATION: The ICC and state parties attest the problem is live (ongoing atrocities, complementarity gaps). The African Union and non-state parties attest the problem is reframed: the Court targets Africa disproportionately (10/12 situations pre-2018 African) while powerful states evade — the founding problem has been captured by power asymmetry. Independent legal scholars (Cryer, Simpson, Nouwen) corroborate the contested status: the Court exists and operates but its authority is structurally compromised by enforcement dependency.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).
:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the ICC's residual jurisdiction claim over state parties and Security Council referrals — it extracts investigative and prosecutorial authority from sovereigns without their case-by-case consent. Suppression (0.55) captures the complementarity admissibility test's coercive structure: states must demonstrate 'genuine' proceedings to avoid ICC intervention, creating pressure to perform compliance. Theater (0.48) is high because the complementarity mechanism and victim participation regime perform coordination and inclusion while the Court's actual enforcement depends on political cooperation it cannot compel — the gap between legal authority and operational capacity is the theater. Accessibility collapse (0.35) is moderate: alternatives (national prosecution, regional courts, truth commissions) exist but are constrained by the Statute's gravitational pull. Resistance (0.45) reflects state pushback (African Union withdrawal threats, US ASPA, Philippine withdrawal) and procedural challenges to admissibility.
 *
 * PERSPECTIVAL GAP:
 *   The ICC Prosecutor's seat experiences the constraint as coordination (d ~0.2): the Court provides a backstop that enables the system to function. State parties' seats are near symmetric (d ~0.5): they gain legitimacy and burden-sharing but accept jurisdiction. Non-state parties targeted via Security Council referrals are full targets (d ~0.9): they bear ICC jurisdiction without consent. Situation country governments are identity-locked targets (d ~0.8): they claim complementarity compliance while the Court assesses 'genuineness' — their sovereign identity is fused with the complementarity performance. Victim participants are dual-positioned: beneficiaries of procedural voice (d ~0.3) but payers of participation costs without reparations certainty (d ~0.7). The engine computes these divergences from the structural data; the hybrid reading claims the constraint IS this divergence — the tension is the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: ICC Prosecutor (institutional authority, budget, legitimacy), state parties coalition (legitimacy, burden-sharing, diplomatic cover), civil society networks (institutional platform, agenda influence), victim participants (procedural voice). Victims: non-state parties targeted without consent (Sudan, Libya situations), state parties under examination (Kenya, Georgia, Palestine situations), situation country governments (complementarity performance burden). The directionality derives from who holds the complementarity trigger: the Prosecutor initiates, states respond. States with arbitrage-grade exit (non-parties not referred) sit at beneficiary end; states trapped by referral or membership sit at target end. Identity-locked situation governments cannot exit the complementarity performance without conceding 'unwillingness.'
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending impunity for atrocity crimes through a permanent court — remains live (contested status). The hybrid reading prevents mislabeling: the universalist reading would call this pure coordination (mountain/rope) and miss the extraction from non-consenting states; the sovereigntist reading would call it pure extraction (snare) and miss the genuine coordination of complementarity assessments and victim participation. The tangled_rope classification captures both: real coordination function (complementarity as burden-sharing, victim participation as inclusion) AND asymmetric extraction (jurisdiction over non-consenting states, enforcement dependency as leverage). The mandate has not atrophied — the Court operates and produces judgments — but the gap between aspiration and operation widens (rising theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid complementarity reading a distinct constraint from the universalist and sovereigntist readings, or merely an interpretive midpoint?',
    'Compare the ε values and beneficiary/victim structures across the three readings; if the hybrid reading''s ε and structural relationships are not reducible to a convex combination of the other two, it is a distinct constraint.',
    'If distinct, the three readings form a constraint family linked by network.affects_constraints; if not, the hybrid reading should be merged with whichever reading it structurally matches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the hybrid reading instantiates a structurally distinct constraint from its siblings').

omega_variable(
    complementarity_mechanism_function,
    'Does the complementarity mechanism function as genuine coordination (preserving sovereign judicial capacity) or as extraction cover (enabling ICC to claim jurisdiction while deferring enforcement)?',
    'Track preliminary examination outcomes: if states genuinely investigate and prosecute, complementarity coordinates; if states perform investigations that never reach trial while ICC defers, complementarity is extraction cover.',
    'If genuine coordination, the constraint leans toward rope/scaffold; if extraction cover, it leans toward tangled_rope/snare. Determines whether theater_ratio reflects functional deferral or performative deference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complementarity_mechanism_function, empirical, 'Whether complementarity is coordination function or extraction mechanism').

omega_variable(
    enforcement_dependency_on_cooperation,
    'Is the ICC''s dependence on state cooperation for enforcement a structural feature of treaty design or a contingent operational weakness?',
    'Analyze arrest warrant execution rates by state party vs. non-state party, and Security Council referral enforcement patterns.',
    'If structural, the constraint''s extraction is limited by design (lower χ for state parties); if contingent, extraction could rise if cooperation improves, making the constraint more extractive over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependency_on_cooperation, conceptual, 'Whether enforcement cooperation dependency is structural or contingent').

omega_variable(
    victim_participation_extraction,
    'Does the victim participation regime extract legitimacy from victims without delivering meaningful reparations or procedural influence?',
    'Compare victim participation grants vs. reparations awarded, and victim legal representative influence on prosecutorial/charging decisions.',
    'If extractive, victim participants should be reclassified from beneficiaries to victims or dual-positioned agents, altering the constraint''s beneficiary/victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_participation_extraction, empirical, 'Whether victim participation is genuine benefit or legitimacy extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.35).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2014, 0.42).
narrative_ontology:measurement(rome_tr_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2020, 0.46).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2008, 0.32).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2014, 0.38).
narrative_ontology:measurement(rome_be_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.45).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2014, 0.52).
narrative_ontology:measurement(rome_su_t2020, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_state_cooperation_regime).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_law_universal_jurisdiction).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of rome_statute_jurisdiction kernel: universalist_reading (ε~0.25, claimed rope/mountain) → hybrid_complementarity_reading (ε=0.42, claimed tangled_rope) → sovereigntist_reading (ε~0.65, claimed snare). Upstream universalist claim (universal mandate) is cited as evidence for hybrid's residual authority; hybrid's complementarity mechanism is the sovereigntist's primary evidence of conditional consent. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, institutional, 0.2).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, powerful, 0.85).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, moderate, 0.75).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, organized, 0.35).
constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, powerless, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
