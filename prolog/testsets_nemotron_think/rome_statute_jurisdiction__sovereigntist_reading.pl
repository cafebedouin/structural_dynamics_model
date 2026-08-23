% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdictional Framework (Sovereigntist Reading)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute's jurisdictional framework is contested territory. The
 *   sovereigntist reading instantiates the constraint as a conditional,
 *   consent-based coordination mechanism: jurisdiction exists only where
 *   states have accepted it (Art 12) or the UNSC refers (Art 13(b)).
 *   Non-party nationals are immune; complementarity (Art 17) means the Court
 *   defers to genuine national proceedings, it does not override them. This
 *   reading claims the framework is a rope — genuine coordination among
 *   consenting states. The authored metrics capture the extraction
 *   experienced by victims in non-party states (denied access) and the
 *   growing suppression required to maintain the consent boundary against
 *   universalist pressure. The claim/metric independence is deliberate: the
 *   reading claims rope; the metrics reveal extractive dimensions the reading
 *   does not acknowledge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.45).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.4).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdictional Framework (Sovereigntist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, 'ee311363-6f3a-4fb6-ad5a-93067c7e6403').
narrative_ontology:cs_kernel_codification('ee311363-6f3a-4fb6-ad5a-93067c7e6403', formalized).
narrative_ontology:cs_authority_grounding('ee311363-6f3a-4fb6-ad5a-93067c7e6403', lineage).
narrative_ontology:cs_interpretation_layer_present('ee311363-6f3a-4fb6-ad5a-93067c7e6403').
narrative_ontology:cs_reading_relation('ee311363-6f3a-4fb6-ad5a-93067c7e6403', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee311363-6f3a-4fb6-ad5a-93067c7e6403', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('ee311363-6f3a-4fb6-ad5a-93067c7e6403', foundational, state_consent_sovereign_prerogative).
narrative_ontology:cs_axiom_status(state_consent_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('ee311363-6f3a-4fb6-ad5a-93067c7e6403', state_consent_sovereign_prerogative, conventional).
narrative_ontology:cs_axiom('ee311363-6f3a-4fb6-ad5a-93067c7e6403', foundational, complementarity_as_deference_not_override).
narrative_ontology:cs_axiom_status(complementarity_as_deference_not_override, holdable).
narrative_ontology:cs_axiom_grounding('ee311363-6f3a-4fb6-ad5a-93067c7e6403', complementarity_as_deference_not_override, conventional).
narrative_ontology:cs_reference_frame('ee311363-6f3a-4fb6-ad5a-93067c7e6403', consent_based_criminal_cooperation_framework).
narrative_ontology:cs_drift_state('ee311363-6f3a-4fb6-ad5a-93067c7e6403', contemporary_universalist_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ee311363-6f3a-4fb6-ad5a-93067c7e6403', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, states_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, icc_institution).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, icc_victims_non_party_states).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, sovereign_equality_principle).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, treaty_consent_foundation).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, complementarity_as_deference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that ratified the Rome Statute and participate in the Assembly of States Parties. They set the budget, elect judges, and define cooperation obligations. They gain a coordinated forum for international criminal justice but accept ICC jurisdiction over their nationals and territory. Exit is formal withdrawal (Art 127) which takes one year and does not discharge obligations already incurred.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, states_parties, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, states_parties, beneficiary).

% States that have not ratified (e.g., US, China, Russia, India, Israel). They are not bound by the Statute and their nationals are immune from ICC prosecution absent UNSC referral. They object to the Court's existence as infringing sovereignty but are structurally excluded from the ASP where rules are made. Their exit option is simply non-participation; they cannot be expelled because they never joined.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_party_states, excluded,
    institutional, generational, arbitrage, global).

% Victims of genocide, crimes against humanity, or war crimes committed in the territory of non-party states by nationals of non-party states. They have no access to the ICC unless the UNSC refers the situation. Their national courts may be unwilling or unable to prosecute. They bear the cost of the consent requirement: impunity for perpetrators in non-party states. No individual exit exists; they are trapped by the territorial/nationality jurisdictional limits.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_victims_non_party_states, payer,
    powerless, biographical, trapped, global).

% The Court itself — Presidency, Chambers, Registry, Office of the Prosecutor. It administers the jurisdictional framework, requests cooperation, and seeks to build legitimacy. It benefits from the Statute's existence (its institutional life depends on it) but is constrained by state consent: it cannot investigate proprio motu in non-party states without UNSC referral, and depends entirely on states for arrest and enforcement. Exit is institutional dissolution (which the ASP could theoretically trigger).
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_institution, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, icc_institution, beneficiary).

% The UNSC can refer situations in non-party states to the ICC (Art 13(b)), overriding the consent requirement. This power is exercised by the P5, three of which are non-party states (US, China, Russia). The UNSC does not fund the Court and bears no direct cost of referrals. Its exit option is vetoing referrals or ignoring the Court entirely — it controls the override valve.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, un_security_council, agenda_setter,
    institutional, immediate, arbitrage, global).

% NGOs, legal scholars, and states that read the Statute as establishing universal jurisdiction over core crimes regardless of consent. They lobby for expansive interpretations (e.g., territorial jurisdiction over non-party nationals, broad Art 12(3) declarations). They are excluded from the ASP's formal decision-making but influence the Court's practice through amicus briefs, advocacy, and funding. Their exit is shifting advocacy to other forums (national courts, UN human rights bodies).
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, universalist_advocates, excluded,
    organized, generational, mobile, global).

% Academic observers who analyze the Statute's interpretation, the Court's jurisprudence, and the political dynamics of consent. They neither collect from nor pay into the constraint. Their role is to map the interpretive field and document drift. Exit is irrelevant — they observe from outside the authority structure.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, legal_scholars_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a treaty-based forum where consenting states can cooperate on investigation and prosecution of core international crimes, sharing evidence, harmonizing procedures, and avoiding duplicative national efforts — a coordination mechanism for states that choose to join.
% TRANSFER_FUNCTION: Moves investigative and prosecutorial authority from national systems to the ICC for situations in states parties or referred by UNSC; moves the cost of proceedings from the territorial state to the ASP budget (funded by states parties); moves the prospect of justice from victims in non-party states to the realm of political discretion (UNSC referral).
% ABSENT_VOICES: Victims in non-party states (e.g., Syria, Myanmar pre-referral, Palestine pre-Art 12(3)) who would object to the consent barrier but have no seat in the ASP. Also, future generations in non-party states who inherit the impunity gap. Their absence is structural: the consent framework defines them out of the constituency.
% DISAPPEARANCE_RATIONALE: If the consent-based jurisdictional framework vanished overnight, the ICC would lose its legal basis; states parties would revert to ad hoc cooperation or universal jurisdiction; the UNSC referral pathway would lose its anchor; victims in non-party states would lose even the theoretical pathway to the Court. The international criminal justice architecture would reorganize around national prosecutions and universal jurisdiction claims.
% FOUNDING_PROBLEM: The post-Cold War impunity gap for atrocity crimes: national courts were unwilling or unable to prosecute their own leaders or foreign perpetrators; ad hoc tribunals (ICTY, ICTR) were temporary and Security Council-dependent. The Rome Statute was built to create a permanent, treaty-based court with jurisdiction triggered by state consent.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the 1998 Rome Conference negotiating record (A/CONF.183/9) and the Preamble's reference to 'putting an end to impunity.' States parties and the ICC institution attest the problem remains live (ongoing atrocities, complementarity gaps). Non-party states and sovereigntist scholars attest the problem was never universal — the Statute was a consent-based solution, not a universal mandate — and that the 'impunity gap' in non-party states is a feature, not a bug, of the sovereign consent design. No single corroborating source outside the beneficiary set resolves the contestation.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the structural denial of ICC access to victims in non-party states — a cost imposed by the consent rule, not by the victims' choice. Suppression (0.40) captures the political and legal effort to maintain the consent boundary: non-party states' diplomatic resistance, Art 98 agreements, US ASPA legislation, and the P5 veto dynamics that gate UNSC referrals. Theater ratio (0.22) is low but rising: the Court's early years had high functional activity; recent years show more performative compliance (preliminary examinations that close, cooperation requests ignored) as the consent constraint binds. Accessibility collapse (0.38) is moderate because alternatives persist (national courts, universal jurisdiction, ad hoc tribunals, truth commissions) but are structurally weaker. Resistance (0.52) is high because the consent boundary is actively contested by universalist advocates, the Prosecutor's proprio motu interpretations, and situations like Palestine/Ukraine that test the territorial jurisdiction of non-party nationals.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (states parties, ICC, UNSC) experience the constraint as coordination with manageable costs. The payer seat (victims in non-party states) experiences it as extraction with no exit. The excluded seats (non-party states, universalist advocates) experience it as a political boundary they cannot cross. The engine computes this divergence from the structural data; the sovereigntist reading's claim of 'rope' only reflects the agenda-setter perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   States parties are beneficiaries (d ≈ 0.15) — they gain a coordinated justice forum at shared cost. The ICC institution is a beneficiary (d ≈ 0.20) — it exists because of the framework. Non-party states are near-symmetric (d ≈ 0.50) — they avoid obligations but lose influence. Victims in non-party states are full targets (d ≈ 0.95) — they bear the impunity cost with trapped exit. The UNSC is a beneficiary of the override power (d ≈ 0.10). Universalist advocates are excluded (d ≈ 0.60) — they pay advocacy costs without structural access. The analytical seat sees the full structure (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (impunity gap) is contested: states parties say it persists; non-party states say the Statute was never meant to cover them. The arrangement persists because states parties benefit from the coordination, the ICC needs the Statute to exist, and the UNSC retains a useful referral tool. The consent constraint has not atrophied into a piton — it is actively defended by its beneficiaries and actively contested by its victims. Mandatrophy is not resolved; the consent boundary is the live fault line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_structure,
    'How does the structural classification change when the Rome Statute is read as a universalist vs. sovereigntist vs. hybrid commitment?',
    'Generate separate constraint stories for each reading with their own ε, beneficiaries/victims, and claimed_type; compare engine outputs across the kernel family.',
    'If the sovereigntist reading computes as rope but universalist computes as tangled_rope or snare, the kernel''s classification is reading-dependent — the framework must track this as a constraint family, not a single constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_structure, conceptual, 'Committer-frame decomposition of the Rome Statute jurisdiction kernel into three readings with distinct structural profiles.').

omega_variable(
    victim_status_non_party_nationals,
    'Are victims in non-party states properly classified as ''victims'' of this constraint, or are they excluded from the constraint''s scope entirely?',
    'Analyze whether the consent rule is a jurisdictional limit (defining the constraint''s domain) or an extraction mechanism (imposing costs on a defined population). The Rome Statute''s Preamble and Art 21(3) reference to victims'' rights are relevant.',
    'If victims are within scope, the constraint has asymmetric extraction (tangled_rope). If they are outside scope, the constraint is a pure coordination mechanism among consenting states (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_non_party_nationals, conceptual, 'Whether the consent boundary defines the constraint''s domain or extracts from a population the kernel claims to protect.').

omega_variable(
    unsc_referral_extraction,
    'Does the UNSC referral power (Art 13(b)) function as an extraction mechanism that imposes ICC jurisdiction on non-consenting states, or as a coordination override for threats to international peace?',
    'Examine UNSC referral practice (Darfur, Libya) — who bears the cost, who benefits, and whether referrals are driven by justice or geopolitics.',
    'If referrals are geopolitically selective extraction, the constraint has a hidden extraction channel. If they are genuine peace enforcement, they are a coordination override.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unsc_referral_extraction, empirical, 'Structural nature of the UNSC referral override in the sovereigntist reading''s framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(rome_tr_t22, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 22, 0.22).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(rome_be_t22, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 22, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(rome_su_t4, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(rome_su_t8, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(rome_su_t12, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(rome_su_t16, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(rome_su_t22, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 22, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__sovereigntist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, universal_jurisdiction_national_courts).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, ad_hoc_tribunal_legacy).

% DUAL FORMULATION NOTE:
% This constraint is the sovereigntist_reading of the rome_statute_jurisdiction kernel. The universalist_reading and hybrid_complementarity_reading are sibling constraints with different ε, beneficiary/victim structures, and claimed_types. All three form a constraint family linked by affects_constraints. The ε-invariance principle requires separate stories because the consent boundary (sovereigntist) vs. universal mandate (universalist) vs. functional complementarity (hybrid) produce different extractiveness values for the same treaty text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__sovereigntist_reading, institutional, 0.15).
constraint_indexing:directionality_override(rome_statute_jurisdiction__sovereigntist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
