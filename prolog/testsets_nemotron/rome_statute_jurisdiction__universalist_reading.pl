% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Universalist Reading of Rome Statute Jurisdiction
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute's Article 12 (preconditions to jurisdiction) and Article
 *   13 (exercise of jurisdiction) are the textual kernel. The universalist
 *   reading interprets Article 12(2)(a) — 'the State on the territory of
 *   which the conduct in question occurred' — as granting the ICC
 *   jurisdiction over crimes committed on state party territory regardless of
 *   the perpetrator's nationality. Combined with Article 13(a) (state party
 *   referral) and 13(b) (UNSC referral), this reading claims the Statute
 *   establishes a universal mandate transcending sovereign consent for core
 *   crimes. This constraint story captures THAT reading as a standalone
 *   ε-invariant constraint. The sibling readings (sovereigntist, hybrid
 *   complementarity) are separate constraints linked via
 *   network.affects_constraints and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - icc_prosecutor_office: Primary agenda setter (institutional/constrained) — expands jurisdictional reach through territorial and UNSC triggers
 *   - icc_judiciary: Secondary agenda setter / beneficiary (institutional/constrained) — issues rulings that entrench the reading
 *   - civil_society_ngos: Beneficiary (organized/mobile) — advocates for universalist interpretation, provides evidentiary and political support
 *   - victims_of_core_crimes: Beneficiary (powerless/trapped) — the constituency the reading claims to serve
 *   - non_party_states_nationals: Primary payer (moderate/constrained) — face prosecution without their state's consent
 *   - sovereigntist_governments: Primary payer (powerful/mobile) — bear diplomatic and political costs, actively resist
 *   - targeted_officials_non_parties: Payer (moderate/trapped) — specific individuals under investigation or charged
 *   - state_parties_governments: Dual beneficiary/payer (organized/constrained) — authored the Statute but did not all anticipate this reading's reach
 *   - un_security_council: Agenda setter (institutional/arbitrage) — can activate jurisdiction via referral, holds veto power
 *   - international_legal_scholars: Observer (analytical/analytical) — interpretive discourse shapes legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.68).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.42).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Universalist Reading of Rome Statute Jurisdiction").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '407ceb39-e520-4dce-a746-61e185bf6d53').
narrative_ontology:cs_kernel_codification('407ceb39-e520-4dce-a746-61e185bf6d53', fixed_text).
narrative_ontology:cs_authority_grounding('407ceb39-e520-4dce-a746-61e185bf6d53', lineage).
narrative_ontology:cs_interpretation_layer_present('407ceb39-e520-4dce-a746-61e185bf6d53').
narrative_ontology:cs_reading_relation('407ceb39-e520-4dce-a746-61e185bf6d53', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('407ceb39-e520-4dce-a746-61e185bf6d53', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('407ceb39-e520-4dce-a746-61e185bf6d53', foundational, territorial_jurisdiction_overrides_nationality_consent).
narrative_ontology:cs_axiom_status(territorial_jurisdiction_overrides_nationality_consent, holdable).
narrative_ontology:cs_axiom_grounding('407ceb39-e520-4dce-a746-61e185bf6d53', territorial_jurisdiction_overrides_nationality_consent, conventional).
narrative_ontology:cs_axiom('407ceb39-e520-4dce-a746-61e185bf6d53', foundational, statute_object_and_purpose_requires_universal_application).
narrative_ontology:cs_axiom_status(statute_object_and_purpose_requires_universal_application, holdable).
narrative_ontology:cs_axiom_grounding('407ceb39-e520-4dce-a746-61e185bf6d53', statute_object_and_purpose_requires_universal_application, deontological).
narrative_ontology:cs_axiom('407ceb39-e520-4dce-a746-61e185bf6d53', secondary, complementarity_is_not_a_sovereign_veto).
narrative_ontology:cs_axiom_status(complementarity_is_not_a_sovereign_veto, holdable).
narrative_ontology:cs_axiom_grounding('407ceb39-e520-4dce-a746-61e185bf6d53', complementarity_is_not_a_sovereign_veto, conventional).
narrative_ontology:cs_reference_frame('407ceb39-e520-4dce-a746-61e185bf6d53', rome_conference_consensus_text).
narrative_ontology:cs_drift_state('407ceb39-e520-4dce-a746-61e185bf6d53', contemporary_icc_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('407ceb39-e520-4dce-a746-61e185bf6d53', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_prosecutor_office).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_judiciary).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, civil_society_ngos).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, sovereigntist_governments).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, targeted_officials_non_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, state_parties_governments).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, state_parties_governments).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, international_criminal_law_universality).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, no_impunity_for_core_crimes).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, territorial_jurisdiction_override_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates investigations and prosecutions under the universalist reading, claiming jurisdiction over nationals of non-party states when crimes occur on state party territory or via UNSC referral. The Office treats the Statute as granting inherent authority to act without state consent, and its operational mandate expands with each territorial or referral trigger. Institutional survival and legitimacy depend on demonstrating that universal jurisdiction is workable.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_prosecutor_office, agenda_setter,
    institutional, generational, constrained, global).

% Adjudicates jurisdictional challenges and interprets Article 12 and 13 to affirm territorial jurisdiction over non-party nationals. The Pre-Trial and Appeals Chambers have issued rulings (e.g., Afghanistan, Palestine, Myanmar/Bangladesh) that structurally entrench the universalist reading. Judicial authority and institutional prestige grow with each successful assertion of jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_judiciary, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, icc_judiciary, beneficiary).

% Human rights organizations (Human Rights Watch, Amnesty International, FIDH, national NGOs) advocate for the universalist reading as the only interpretation that fulfills the Statute's anti-impunity object and purpose. They provide evidence, legal amicus briefs, and political pressure that sustain the Prosecutor's willingness to pursue non-party nationals. They gain moral authority and fundraising capacity from ICC action.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, civil_society_ngos, beneficiary,
    organized, biographical, mobile, global).

% Victims of genocide, war crimes, crimes against humanity, and aggression committed by nationals of non-party states on state party territory (or referred by UNSC). Under the universalist reading, they have a pathway to justice that would not exist under sovereign consent. Their participation in proceedings (reparations, testimony) is structurally enabled by this reading. Exit is not meaningful — they are the constituency the constraint claims to serve.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes, beneficiary,
    powerless, biographical, trapped, global).

% Nationals of states that have not ratified the Rome Statute (e.g., US, Russia, China, Israel, India) who commit alleged core crimes on the territory of states parties. They face investigation and potential prosecution without their state's consent. Their exit options are limited: they cannot renounce nationality to escape jurisdiction, and their home states may not protect them. They bear the full cost of the constraint's jurisdictional reach.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states_nationals, payer,
    moderate, biographical, constrained, global).

% Governments of non-party states (US, Russia, China, Israel, India, Turkey, etc.) that reject the universalist reading. They bear political and diplomatic costs: sanctions threats (US ASPA, ICC sanctions), bilateral immunity agreements (Article 98), diplomatic pressure, and the erosion of sovereign control over their nationals. They actively resist through non-cooperation, bilateral agreements, and political campaigns. Their exit is high — they can and do ignore the Court — but the constraint still extracts compliance costs from their allies and partners.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, sovereigntist_governments, payer,
    powerful, generational, mobile, global).

% Government officials, military commanders, and intelligence personnel of non-party states who are specifically investigated or charged (e.g., US personnel in Afghanistan, Israeli officials in Palestine, Russian officials in Georgia/Ukraine context). They face travel restrictions, asset freezes, reputational damage, and the permanent threat of arrest. Their exit is near-zero — they cannot change nationality, and their state's protection is political, not legal.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, targeted_officials_non_parties, payer,
    moderate, immediate, trapped, global).

% Governments that ratified the Statute. They benefit from the Court's deterrent effect and the outsourcing of difficult prosecutions, but they also bear costs: obligation to cooperate with arrests, surrender of their own nationals, diplomatic friction with non-party allies, and budget contributions. Their position is dual — they authored the constraint but did not all anticipate the universalist reading's reach.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, state_parties_governments, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, state_parties_governments, payer).

% The UNSC can refer situations in non-party states (Darfur, Libya) under Article 13(b), activating ICC jurisdiction without state consent. The P5 members (US, Russia, China, UK, France) hold veto power over referrals. The universalist reading makes the Court a tool the UNSC can selectively deploy — a structural power the Council benefits from but does not control.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Academics and jurists who debate the Statute's interpretive possibilities. The universalist reading is one of three live scholarly positions. They do not bear costs or collect rents from the constraint's operation, but their interpretations shape the legitimacy discourse that the Court's authority depends on.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the prosecution of core international crimes across borders by providing a single permanent court with jurisdiction that does not depend on ad hoc tribunals or the consent of the perpetrator's state — solving the impunity gap that exists when national systems are unable or unwilling to act.
% TRANSFER_FUNCTION: Transfers jurisdictional authority and enforcement costs from national systems (especially of non-party states) to the ICC and its state party supporters. The Prosecutor and Judiciary gain authority to investigate and prosecute; state parties bear cooperation obligations and budget costs; non-party nationals and their states bear the risk of prosecution without consent.
% ABSENT_VOICES: Populations in non-party states who might support ICC jurisdiction but have no voice in their government's rejection of the Statute; future generations who will inherit the precedent of universal jurisdiction without consent; states that would join the Statute but are deterred by the universalist reading's expansive reach (the 'chilled accession' problem).
% DISAPPEARANCE_RATIONALE: If the universalist reading vanished overnight, the ICC would revert to a consent-based court (sovereigntist or hybrid reading). The Prosecutor could not investigate non-party nationals on state party territory without UNSC referral. Ongoing investigations (Afghanistan, Palestine, Myanmar/Bangladesh, Ukraine via state party referrals) would collapse. The anti-impunity architecture would lose its only universal jurisdictional claim.
% FOUNDING_PROBLEM: The post-Cold War hope that a permanent international criminal court could end impunity for the worst crimes, overcoming the ad hoc limitations of the ICTY and ICTR and the Security Council's political veto over justice.
% FOUNDING_PROBLEM_CORROBORATION: The Rome Conference delegates (1998) attest the founding problem was impunity, but the negotiating record shows deep disagreement on whether universal jurisdiction without consent was the solution — the US, China, India, Israel, and others objected precisely on this point. The universalist reading's proponents (civil society, like-minded states) attest the problem remains live; the sovereigntist camp attests the problem was solved by complementarity and the universalist reading is mission creep. No neutral corroborator exists — the founding problem is itself the kernel contest.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the constraint transfers jurisdictional authority from sovereign states to an international institution without the consent of the affected states or their nationals. The transfer is not reciprocal — non-party states gain no corresponding authority. Suppression (0.42) is moderate: the constraint does not physically prevent exit (non-party states can and do ignore the Court), but it creates legal and diplomatic costs that suppress full non-cooperation by state parties and their allies. Theater ratio (0.31) reflects that the Court's complementarity proceedings, admissibility challenges, and diplomatic engagement perform a coordination function (legitimacy maintenance) that is partially genuine but increasingly covers the extraction of jurisdictional authority. The constraint is a tangled rope because it has a genuine coordination function (ending impunity for core crimes via a permanent court) AND asymmetric extraction (non-party nationals and states bear costs without consent). Active enforcement is required: the Prosecutor must seek arrest warrants, state parties must cooperate, and the Judiciary must confirm charges — none of this happens automatically.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (Prosecutor, Judiciary, UNSC), the constraint appears as a rope or even a scaffold — a coordination mechanism that solves the impunity problem and may eventually become unnecessary if national systems improve. From the payer seats (non-party nationals, targeted officials, sovereigntist governments), the same constraint appears as a snare — extraction without consent, enforced through legal and diplomatic pressure. From the beneficiary seats (victims, civil society), it appears as a mountain — a moral and legal necessity that should be universal. The engine computes these divergent per-seat types from the structural data; this commentary documents the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Prosecutor and Judiciary are structural beneficiaries: they collect jurisdictional authority, institutional legitimacy, and operational mandate (d near 0.0). Civil society NGOs are beneficiaries: they gain moral authority, access, and relevance (d ~ 0.15). Victims are beneficiaries but trapped — they gain a justice pathway they otherwise lack, but cannot exit the condition of victimhood (d ~ 0.1, identity_locked dynamics). State parties are dual: they benefit from deterrence and outsourcing but pay cooperation costs (d ~ 0.45). The UNSC is an agenda-setter with arbitrage exit — it can activate or block referrals at will (d ~ 0.2, but with high variance). Non-party nationals, targeted officials, and sovereigntist governments are payers: they bear the jurisdictional reach without consent. Targeted officials are identity_locked/trapped (d ~ 0.95). Non-party nationals are constrained (d ~ 0.75). Sovereigntist governments are powerful/mobile — they can resist, but the constraint still extracts costs from their allies and international operations (d ~ 0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist reading claims to solve the founding problem (impunity for core crimes) but the coordination function (ending impunity via a universal court) and the extraction function (asserting jurisdiction over non-consenting states/nationals) have become structurally entangled. The complementarity mechanism was designed as the safeguard against extraction — it requires the Court to defer to genuine national proceedings. In practice, complementarity has not prevented the universalist reading's expansion: the Prosecutor interprets 'unwilling or unable' broadly, and the Judiciary has affirmed territorial jurisdiction over non-party nationals. The mandate has not atrophied — the problem (impunity) persists — but the reading's method (universal jurisdiction without consent) extracts from those who never agreed to the bargain. This is the tangled rope signature: coordination and extraction are inseparable in the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_efficacy,
    'Does the complementarity mechanism (Article 17) genuinely constrain the universalist reading''s jurisdictional reach, or has it been interpreted to permit the Prosecutor to bypass national systems at will?',
    'Empirical analysis of all admissibility challenges and complementarity rulings: count how many national investigations were deemed ''genuine'' vs. ''unwilling or unable'' by the Chambers, and whether the pattern correlates with the nationality of the accused.',
    'If complementarity is a genuine brake, the universalist reading''s extraction is self-limiting (tangled_rope with coordination dominant). If complementarity is a rubber stamp, the reading is a snare wearing coordination''s clothes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_efficacy, empirical, 'Whether complementarity functions as a real constraint on the universalist reading''s expansion').

omega_variable(
    territorial_jurisdiction_naturalness,
    'Is territorial jurisdiction over non-party nationals a natural extension of the state party''s sovereign right to control its territory, or a constructed expansion of the Statute''s text beyond the negotiators'' intent?',
    'Travaux préparatoires analysis of the Rome Conference: what did delegates understand Article 12(2)(a) to mean? Correlate with the voting record and the statements of the like-minded group vs. the non-party objectors.',
    'If the universalist reading reflects the negotiators'' intent (the like-minded group''s design), its naturalness claim is stronger. If it is a later judicial construction, the reading is a constructed constraint with identifiable beneficiaries (the Court itself) — a false summit candidate if ever claimed as mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(territorial_jurisdiction_naturalness, conceptual, 'Whether the universalist reading''s textual basis is original intent or judicial construction').

omega_variable(
    universalist_reading_as_kernel_reading,
    'This constraint is one reading (universalist_reading) of the contested kernel rome_statute_jurisdiction. The sibling readings (sovereigntist_reading, hybrid_complementarity_reading) instantiate different constraints with different ε, beneficiaries, and victims. How does the kernel''s contestation affect the universalist reading''s structural classification?',
    'Generate all three readings as separate constraint stories. Compare their ε, beneficiary/victim structures, and computed per-seat types. The kernel''s structural reality is the set of readings, not any single one.',
    'If the sovereigntist reading computes as mountain (low ε, no extraction) and the universalist as tangled_rope, the kernel itself is a site of structural disagreement — the Statute''s text under-determines the constraint. This validates the kernel/reading decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universalist_reading_as_kernel_reading, conceptual, 'Commitment kernel framing: this constraint is one reading of rome_statute_jurisdiction; sibling readings are sovereigntist_reading and hybrid_complementarity_reading').

omega_variable(
    victim_seat_legitimacy,
    'Are the ''victims_of_core_crimes'' stakeholders genuine beneficiaries of the universalist reading, or does the reading instrumentalize their suffering to legitimize jurisdictional expansion that primarily benefits the Court''s institutional survival?',
    'Track victim participation outcomes: reparations actually delivered, victim satisfaction surveys, whether investigations prioritize victim-identified perpetrators vs. politically accessible ones. Compare with national or hybrid tribunal outcomes for the same victim populations.',
    'If victims are instrumentalized, the beneficiary declaration is a cover story — the true beneficiary is the Court institution. This would shift the constraint toward snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_seat_legitimacy, empirical, 'Whether victim beneficiary status is genuine or instrumentalized for institutional legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__universalist_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement(rome_tr_t2006, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(rome_tr_t2012, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(rome_tr_t2016, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement(rome_tr_t2018, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(rome_tr_t2021, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__universalist_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 1998, 0.25).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2002, 0.32).
narrative_ontology:measurement(rome_be_t2006, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2006, 0.38).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(rome_be_t2012, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2012, 0.52).
narrative_ontology:measurement(rome_be_t2016, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement(rome_be_t2018, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(rome_be_t2021, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2021, 0.66).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 1998, 0.15).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2002, 0.2).
narrative_ontology:measurement(rome_su_t2006, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2006, 0.25).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(rome_su_t2012, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(rome_su_t2018, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2018, 0.4).
narrative_ontology:measurement(rome_su_t2021, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2021, 0.41).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__universalist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, icc_state_cooperation_obligations).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, article_98_agreements_network).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, universal_jurisdiction_customary_law).

% DUAL FORMULATION NOTE:
% This constraint (universalist_reading) and its two siblings form the rome_statute_jurisdiction kernel family. The universalist reading claims the Statute's text establishes universal jurisdiction via territorial trigger; the sovereigntist reading claims the Statute requires sovereign consent; the hybrid reading claims complementarity balances both. They have different ε values (universalist highest extraction, sovereigntist lowest) because they instantiate different constraints from the same text. The decomposition follows the ε-invariance principle: the label 'Rome Statute jurisdiction' conflates three structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, institutional, 0.05).
constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, powerful, 0.65).
constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, moderate, 0.75).
constraint_indexing:directionality_override(rome_statute_jurisdiction__universalist_reading, powerless, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
